// The page: relays the sidecar's event stream into a chat feed and the canvas.
(function () {
  const $ = id => document.getElementById(id);
  const feed = $('feed');
  const params = new URLSearchParams(location.search), traceUrl = params.get('trace'), speed = Math.max(0.25, +params.get('speed') || 1);
  const PC = window.PortolanCanvas;
  const root = document.documentElement;
  const keep = (k, v) => { try { localStorage.setItem('portolan.' + k, v); } catch (e) {} };
  const kept = k => { try { return localStorage.getItem('portolan.' + k); } catch (e) { return null; } };
  let project = '', live = false, seen = new Set(), tools = {}, liveEl = null, liveTool = null, toolJson = '', lastWho = '';
  let files = null; // file contents carried by a recorded trace
  let view = null, which = '';
  let latest = null; // the whole project's graph, as last received
  let shown = null, shownKey = ''; // what the canvas is drawing: one module of it, or all of it
  let moduleName = null; // '' = the whole project; null = not chosen yet

  // ---- one module at a time ----
  // A project of any size is too much to draw whole, so the canvas draws one file's module:
  // its types and functions, with the types it uses from elsewhere as faint outside nodes.
  const top = x => (x.m_path || [])[0] || '';
  // The project's modules, by folder then name, each with how many definitions it has.
  function modules(g) {
    const by = {};
    [...g.nodes.filter(n => n.kind === 'alias'), ...g.edges, ...(g.values || [])].forEach(x => {
      const m = top(x); if (!m) return;
      const file = (x.at && x.at.file) || '', dir = file.includes('/') ? file.slice(0, file.lastIndexOf('/')) : '';
      (by[m] = by[m] || { name: m, size: 0, dir }).size++;
    });
    return Object.values(by).sort((a, b) => a.dir.localeCompare(b.dir) || a.name.localeCompare(b.name));
  }
  function moduleOfFile(g, file) {
    const hit = g.edges.find(e => e.at && e.at.file === file) || g.nodes.find(n => n.at && n.at.file === file) || (g.values || []).find(v => v.at && v.at.file === file);
    return hit ? top(hit) : '';
  }
  // A project that is one module is drawn flat, as a Hazel program is: the file's own module is not a region.
  function flatten(g) {
    const items = [...g.nodes.filter(n => n.kind === 'alias'), ...g.edges, ...(g.values || [])];
    const tops = new Set(items.map(top).filter(Boolean));
    if (tops.size !== 1 || items.some(x => !(x.m_path || []).length)) return g;
    const cut = x => ({ ...x, m_path: (x.m_path || []).slice(1) });
    return { ...g, nodes: g.nodes.map(cut), edges: g.edges.map(cut), values: (g.values || []).map(cut) };
  }
  function moduleView(g, mod) {
    if (!mod) return flatten(g);
    const inMod = x => top(x) === mod, cut = x => ({ ...x, m_path: (x.m_path || []).slice(1) });
    const edges = g.edges.filter(inMod), values = (g.values || []).filter(inMod);
    const byKey = new Map(g.nodes.map(n => [n.key, n])), want = new Set();
    const need = k => { const n = byKey.get(k); if (!n || want.has(k)) return; want.add(k); (n.parts || []).forEach(need); };
    g.nodes.forEach(n => { if (n.kind === 'alias' && inMod(n)) need(n.key); });
    edges.forEach(e => { need(e.src); need(e.dst); });
    values.forEach(v => need(v.key));
    const nodes = g.nodes.filter(n => want.has(n.key)).map(n => (n.kind === 'alias' && !inMod(n) ? { ...n, kind: 'ghost', label: n.key, m_path: [] } : cut(n)));
    return { ...g, nodes, edges: edges.map(cut), values: values.map(cut), loose_tests: (g.loose_tests || []).filter(t => t.at && moduleOfFile(g, t.at.file) === mod) };
  }
  function drawPicker() {
    const ms = latest ? modules(latest) : [], sel = $('module');
    $('module-wrap').hidden = ms.length < 2;
    const sig = JSON.stringify(ms);
    if (sel.dataset.sig !== sig) {
      sel.dataset.sig = sig;
      const option = (v, t) => { const o = document.createElement('option'); o.value = v; o.textContent = t; return o; };
      const dirs = [...new Set(ms.map(m => m.dir))], kids = [option('', 'whole project')];
      dirs.forEach(d => {
        const os = ms.filter(m => m.dir === d).map(m => option(m.name, m.name + '  ·  ' + m.size));
        if (dirs.length < 2) return kids.push(...os);
        const group = document.createElement('optgroup'); group.label = d || '.'; group.append(...os); kids.push(group);
      });
      sel.replaceChildren(...kids);
    }
    sel.value = moduleName || '';
  }
  function choose(mod, cause) {
    if (mod !== moduleName) unpick();
    moduleName = mod;
    keep('module.' + project, mod);
    drawPicker();
    present(cause, true);
  }
  // Draw the current module of the latest graph: at once after a switch, as a paced beat otherwise.
  function present(cause, fresh) {
    if (!latest) return;
    if (moduleName === null) {
      const ms = modules(latest), saved = kept('module.' + project);
      moduleName = ms.length < 2 ? '' : ms.some(m => m.name === saved) ? saved : ms[0].name;
      drawPicker();
      fresh = fresh || moduleName !== ''; // a one-module project's first definitions arrive as a beat
    }
    if (moduleName && !modules(latest).some(m => m.name === moduleName)) moduleName = '';
    const g = inSyntax(moduleView(latest, moduleName)), key = JSON.stringify(g);
    if (key === shownKey && !fresh) return; // nothing in this module changed
    shown = g;
    shownKey = key;
    if (!live) return;
    if (fresh) { view.reset(); view.show(g); } else view.push(g, cause);
  }
  $('module').onchange = () => choose($('module').value);

  // A graph shown at once is framed before its layout has settled, and comes out cut off or far
  // too small. Watch the board until it stops changing, then frame it; a hidden tab draws late.
  let settling = 0;
  function fitWhenSettled() {
    clearInterval(settling);
    let last = '', same = 0, tries = 0;
    settling = setInterval(() => {
      const board = $('canvas').querySelector('.canvas-root'), n = $('canvas').querySelectorAll('.canvas-node').length;
      const now = n + '|' + (board ? board.style.width + board.style.height : '');
      same = n > 0 && now === last ? same + 1 : 0;
      last = now;
      if (same === 2 || same === 8) PC.fit(); // once when it stops moving, once more after its own camera has finished
      if (same >= 8 || ++tries > 60) clearInterval(settling);
    }, 250);
  }

  // ---- canvas: Hazel's, ported. The plain view is only a fallback when its bundle is missing. ----
  const views = {
    hazel: {
      mount: el => PC.mount(el),
      push: (g, c) => PC.push(g, { label: (c && c.label) || 'edit', ok: true }),
      show: g => { PC.show(g); fitWhenSettled(); },
      reset: () => PC.reset(),
      onSelect: cb => PC.onSelect((name, _kind, at) => {
        const g = shown;
        const item = g && (g.edges.find(e => e.name === name || e.label === name) || g.nodes.find(n => n.key === name || n.label === name) || (g.values || []).find(v => v.name === name));
        cb(item || { label: name, at }, item && g.edges.includes(item) ? 'edge' : 'node', at || (item && item.at));
      }),
    },
    plain: {
      mount: el => PlainView.mount(el),
      push: (g, c) => PlainView.push(g, c, true),
      show: g => PlainView.push(g, null, false),
      reset: () => PlainView.reset(),
      onSelect: cb => PlainView.onSelect((item, kind) => cb(item, kind, item.at)),
    },
  };
  function use(name) {
    which = name;
    $('canvas').innerHTML = '';
    view = views[name];
    view.mount($('canvas'));
    view.onSelect(showSource);
    view.reset();
    if (shown && live) view.show(shown);
    $('which').textContent = name === 'hazel' ? 'plain view' : 'canvas';
  }
  $('which').hidden = !PC;
  $('which').onclick = () => use(which === 'hazel' ? 'plain' : 'hazel');
  use(PC ? 'hazel' : 'plain');
  const toggle = (id, set) => { const b = $(id); b.onclick = () => { const on = !b.classList.contains('tool-active'); b.classList.toggle('tool-active', on); if (PC) set(on); }; };
  toggle('pace', on => PC.setPace(on));
  toggle('follow', on => PC.setFollow(on));
  $('fit').onclick = () => PC && PC.fit();
  const redraw = () => window.dispatchEvent(new Event('resize')); // the canvas sizes and paints its dot grid by script

  // ---- light / dark, and the corner variation ----
  function chrome() {
    $('theme').textContent = root.dataset.theme === 'dark' ? 'light' : 'dark';
    $('corners').textContent = root.dataset.corners === 'square' ? 'round' : 'square';
  }
  $('theme').onclick = () => {
    if (root.dataset.theme === 'dark') delete root.dataset.theme; else root.dataset.theme = 'dark';
    keep('theme', root.dataset.theme || 'light');
    chrome();
    redraw();
  };
  $('corners').onclick = () => {
    root.dataset.corners = root.dataset.corners === 'square' ? 'round' : 'square';
    keep('corners', root.dataset.corners);
    chrome();
  };
  chrome();

  // ---- the chat pane's width ----
  const main = $('main'), setSide = w => main.style.setProperty('--side-w', Math.round(Math.max(260, Math.min(w, main.clientWidth - 320))) + 'px');
  if (kept('side')) main.style.setProperty('--side-w', kept('side') + 'px');
  $('divider').addEventListener('pointerdown', ev => {
    ev.preventDefault();
    $('divider').setPointerCapture(ev.pointerId);
    const move = e => setSide(main.getBoundingClientRect().right - e.clientX);
    const up = () => {
      $('divider').removeEventListener('pointermove', move);
      keep('side', parseInt(main.style.getPropertyValue('--side-w'), 10));
      redraw();
    };
    $('divider').addEventListener('pointermove', move);
    $('divider').addEventListener('pointerup', up, { once: true });
  });
  $('divider').addEventListener('keydown', ev => {
    if (ev.key !== 'ArrowLeft' && ev.key !== 'ArrowRight') return;
    setSide($('sidebar').clientWidth + (ev.key === 'ArrowLeft' ? 24 : -24));
    keep('side', parseInt(main.style.getPropertyValue('--side-w'), 10));
    redraw();
  });

  // ---- OCaml or Reason: one choice for the code shown and for the types on the canvas ----
  // Until chosen, it is the syntax most of the project is written in. The graph record prints
  // types in OCaml syntax; the sidecar converts them, and what it cannot convert stays as it is.
  let syntaxPref = ['re', 'ml'].includes(kept('syntax')) ? kept('syntax') : null, typesOff = !!traceUrl;
  const native = file => (/\.rei?$/.test(file) ? 're' : 'ml'), reason = new Map();
  function projectSyntax(g) {
    let re = 0, ml = 0;
    g.edges.forEach(e => { if (e.at) native(e.at.file) === 're' ? re++ : ml++; });
    return re > ml ? 're' : 'ml';
  }
  const syntaxNow = () => syntaxPref || (latest ? projectSyntax(latest) : 'ml');
  const typed = n => n.kind === 'derived';
  async function learnTypes(g) {
    if (typesOff || syntaxNow() !== 're') return;
    const want = new Set();
    const add = t => { if (t && !reason.has(t)) want.add(t); };
    g.nodes.forEach(n => { add(n.ty); if (typed(n)) add(n.label); });
    g.edges.forEach(e => add(e.ty));
    (g.values || []).forEach(v => add(v.ty));
    if (!want.size) return;
    const types = [...want];
    try {
      const r = await fetch('types', { method: 'POST', body: JSON.stringify({ types }) });
      if (!r.ok) throw new Error(r.status);
      (await r.json()).types.forEach((t, i) => reason.set(types[i], t || types[i]));
    } catch (e) { typesOff = true; } // a replay has no sidecar to ask: types stay as printed
  }
  function inSyntax(g) {
    if (typesOff || syntaxNow() !== 're') return g;
    const re = t => (t && reason.get(t)) || t;
    return { ...g, nodes: g.nodes.map(n => ({ ...n, ty: re(n.ty), label: typed(n) ? re(n.label) : n.label })),
      edges: g.edges.map(e => ({ ...e, ty: re(e.ty) })), values: (g.values || []).map(v => ({ ...v, ty: re(v.ty) })) };
  }
  let picked = null;
  function drawSyntax() {
    const b = $('lang');
    b.hidden = !!traceUrl;
    b.textContent = syntaxNow() === 're' ? 'ocaml' : 'reason';
  }
  $('lang').onclick = async () => {
    syntaxPref = syntaxNow() === 're' ? 'ml' : 're';
    keep('syntax', syntaxPref);
    drawSyntax();
    if (latest) { await learnTypes(latest); present(null, true); }
    if (picked) showSource(picked.item, picked.kind, picked.at);
  };
  drawSyntax();
  function unpick() {
    picked = null;
    $('info-path').textContent = 'Pick a function or type on the canvas to see its definition.';
    $('info-type').textContent = $('info-code').textContent = '';
  }
  async function showSource(item, kind, at) {
    picked = { item, kind, at };
    $('info-path').textContent = at ? at.path : item.label || item.name || '';
    $('info-type').textContent = kind === 'edge' && item.ty ? ': ' + item.ty : item.ty ? '= ' + item.ty : '';
    $('info-code').textContent = '';
    drawSyntax();
    if (!at) return;
    try {
      if (files) { $('info-code').textContent = (files[at.file] || '').split('\n').slice(at.line - 1, at.end_line).join('\n'); return; }
      if (syntaxNow() !== native(at.file)) {
        const r = await fetch('refmt?file=' + encodeURIComponent(at.file) + '&line=' + at.line + '&end_line=' + at.end_line + '&to=' + syntaxNow());
        if (r.ok) { $('info-code').textContent = await r.text(); return; }
      }
      const text = await (await fetch('source?file=' + encodeURIComponent(at.file))).text();
      $('info-code').textContent = text.split('\n').slice(at.line - 1, at.end_line).join('\n');
    } catch (e) { $('info-code').textContent = '(could not read ' + at.file + ')'; }
  }

  // ---- formatted text: a small Markdown reader for the agent's replies ----
  const esc = s => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  function inline(s) {
    const code = [];
    s = esc(s).replace(/`([^`\n]+)`/g, (_, c) => { code.push(c); return '\u0000' + (code.length - 1) + '\u0000'; });
    s = s.replace(/\*\*([^*\n]+)\*\*/g, '<b>$1</b>').replace(/(^|[\s(])\*([^*\n]+)\*(?=[\s).,;:!?]|$)/g, '$1<i>$2</i>')
      .replace(/\[([^\]\n]+)\]\((https?:\/\/[^\s)]+)\)/g, '<a href="$2" target="_blank" rel="noopener">$1</a>');
    return s.replace(/\u0000(\d+)\u0000/g, (_, i) => '<code>' + code[+i] + '</code>');
  }
  function markdown(text, done) {
    const lines = text.replace(/\r/g, '').split('\n'), out = [];
    let i = 0, para = [], list = null;
    const flushP = () => { if (para.length) { out.push('<p>' + inline(para.join(' ')) + '</p>'); para = []; } };
    const flushL = () => { if (list) { out.push('<' + list.tag + '>' + list.items.map(x => '<li>' + inline(x) + '</li>').join('') + '</' + list.tag + '>'); list = null; } };
    while (i < lines.length) {
      const l = lines[i];
      const fence = l.match(/^\s*```(.*)$/);
      if (fence) {
        flushP(); flushL();
        const body = [];
        i++;
        while (i < lines.length && !/^\s*```\s*$/.test(lines[i])) body.push(lines[i++]);
        const closed = i < lines.length;
        i++;
        // A finished `html` block is shown as the page fragment it describes, with its source underneath.
        if (done && closed && fence[1].trim().toLowerCase() === 'html') out.push('<div class="html-block" data-html="' + esc(body.join('\n')) + '"></div>');
        else out.push('<pre>' + esc(body.join('\n')) + '</pre>');
        continue;
      }
      const h = l.match(/^(#{1,4})\s+(.*)$/), ul = l.match(/^\s*[-*]\s+(.*)$/), ol = l.match(/^\s*\d+[.)]\s+(.*)$/);
      if (h) { flushP(); flushL(); out.push('<h' + Math.min(4, h[1].length + 1) + '>' + inline(h[2]) + '</h' + Math.min(4, h[1].length + 1) + '>'); }
      else if (ul || ol) { flushP(); const tag = ul ? 'ul' : 'ol'; if (!list || list.tag !== tag) { flushL(); list = { tag, items: [] }; } list.items.push((ul || ol)[1]); }
      else if (/^\s*\|.*\|\s*$/.test(l)) {
        flushP(); flushL();
        const rows = [];
        while (i < lines.length && /^\s*\|.*\|\s*$/.test(lines[i])) rows.push(lines[i++].trim().replace(/^\||\|$/g, '').split('|').map(c => c.trim()));
        const ruled = rows.length > 1 && rows[1].every(c => /^:?-+:?$/.test(c));
        const tr = (cells, tag) => '<tr>' + cells.map(c => '<' + tag + '>' + inline(c) + '</' + tag + '>').join('') + '</tr>';
        out.push('<div class="table-scroll"><table>' + (ruled ? '<thead>' + tr(rows[0], 'th') + '</thead>' : '') + '<tbody>' + rows.slice(ruled ? 2 : 0).map(r => tr(r, 'td')).join('') + '</tbody></table></div>');
        continue;
      }
      else if (!l.trim()) { flushP(); flushL(); }
      else if (list && /^\s{2,}\S/.test(l)) list.items[list.items.length - 1] += ' ' + l.trim();
      else { flushL(); para.push(l.trim()); }
      i++;
    }
    flushP(); flushL();
    return out.join('');
  }

  // ---- chat feed ----
  const rel = p => { const k = '/' + project + '/'; const i = p ? p.lastIndexOf(k) : -1; return i >= 0 ? p.slice(i + k.length) : p || ''; };
  const pinned = () => feed.scrollHeight - feed.scrollTop - feed.clientHeight < 60;
  function append(d) { const stick = pinned(); feed.appendChild(d); if (stick) feed.scrollTop = feed.scrollHeight; return d; }
  // An `html` block from the agent is drawn in a frame that can run no script and load nothing
  // from outside: markup, styles and inline SVG only.
  function frames(el) {
    el.querySelectorAll('.html-block').forEach(block => {
      const css = getComputedStyle(document.body), ink = css.color;
      const frame = document.createElement('iframe');
      frame.setAttribute('sandbox', 'allow-same-origin');
      frame.srcdoc = '<!doctype html><meta charset="utf-8"><meta http-equiv="Content-Security-Policy" content="default-src \'none\'; style-src \'unsafe-inline\'; img-src data:; font-src data:">' +
        '<style>html{color-scheme:' + (root.dataset.theme === 'dark' ? 'dark' : 'light') + '}body{margin:0;padding:10px 12px;font:13px/1.5 system-ui,sans-serif;color:' + ink + ';background:transparent}svg{max-width:100%;height:auto}table{border-collapse:collapse}td,th{padding:3px 8px;border-bottom:1px solid rgba(128,128,128,.3);text-align:left}</style>' + block.dataset.html;
      frame.onload = () => { try { frame.style.height = Math.min(640, frame.contentDocument.documentElement.scrollHeight) + 'px'; } catch (e) {} };
      const source = document.createElement('details'), sum = document.createElement('summary'), pre = document.createElement('pre');
      sum.textContent = 'source';
      pre.textContent = block.dataset.html;
      source.append(sum, pre);
      block.removeAttribute('data-html');
      block.append(frame, source);
    });
  }
  function add(cls, text, who, rich) {
    if (who && who !== lastWho) { const w = document.createElement('div'); w.className = 'who' + (who === 'You' ? ' you' : ''); w.textContent = who; append(w); }
    if (who) lastWho = who;
    const d = document.createElement('div');
    d.className = cls;
    if (rich) { d.innerHTML = markdown(text, true); frames(d); } else d.textContent = text;
    return append(d);
  }
  // ---- an edit by the agent, as a diff ----
  // The lines of `was` and `now` in order, each marked ' ' (in both), '-' or '+'. Lines the two share at
  // the ends are matched first; the middle by longest common subsequence, unless it is too big to be worth it.
  function lineDiff(was, now) {
    const a = was.replace(/\n$/, '').split('\n'), b = now.replace(/\n$/, '').split('\n'), out = [];
    let head = 0, tail = 0;
    while (head < a.length && head < b.length && a[head] === b[head]) head++;
    while (tail < a.length - head && tail < b.length - head && a[a.length - 1 - tail] === b[b.length - 1 - tail]) tail++;
    const x = a.slice(head, a.length - tail), y = b.slice(head, b.length - tail);
    a.slice(0, head).forEach(l => out.push([' ', l]));
    if (x.length * y.length > 250000) { x.forEach(l => out.push(['-', l])); y.forEach(l => out.push(['+', l])); }
    else {
      const n = x.length, m = y.length, t = Array.from({ length: n + 1 }, () => new Uint16Array(m + 1));
      for (let i = n - 1; i >= 0; i--) for (let j = m - 1; j >= 0; j--) t[i][j] = x[i] === y[j] ? t[i + 1][j + 1] + 1 : Math.max(t[i + 1][j], t[i][j + 1]);
      let i = 0, j = 0;
      while (i < n || j < m) {
        if (i < n && j < m && x[i] === y[j]) { out.push([' ', x[i]]); i++; j++; }
        else if (i < n && (j === m || t[i + 1][j] >= t[i][j + 1])) out.push(['-', x[i++]]);
        else out.push(['+', y[j++]]);
      }
    }
    a.slice(a.length - tail).forEach(l => out.push([' ', l]));
    return out;
  }
  // What an Edit, MultiEdit or Write call changes, as marked lines; a gap between two edits is ['…', ''].
  function changes(name, input) {
    if (name === 'Write') return input.content == null ? [] : String(input.content).replace(/\n$/, '').split('\n').map(l => ['+', l]);
    const edits = name === 'MultiEdit' ? input.edits || [] : [input];
    return edits.flatMap((e, i) => (i ? [['…', '']] : []).concat(lineDiff(String(e.old_string || ''), String(e.new_string || ''))));
  }
  const LONG = 14, MOST = 400; // a diff longer than LONG lines starts folded; no more than MOST are drawn
  function diffBlock(lines) {
    const count = k => lines.filter(l => l[0] === k).length, d = document.createElement('details'), sum = document.createElement('summary'), pre = document.createElement('pre');
    d.className = 'diff';
    d.open = lines.length <= LONG;
    sum.innerHTML = '<span class="add"></span> <span class="del"></span>';
    sum.children[0].textContent = '+' + count('+');
    sum.children[1].textContent = count('-') ? '−' + count('-') : '';
    lines.slice(0, MOST).forEach(([k, text]) => {
      const row = document.createElement('span');
      row.className = k === '+' ? 'add' : k === '-' ? 'del' : k === '…' ? 'gap' : 'same';
      row.textContent = (k === '…' ? '⋯' : k + ' ' + text) + '\n';
      pre.appendChild(row);
    });
    if (lines.length > MOST) pre.appendChild(document.createTextNode('… ' + (lines.length - MOST) + ' more lines'));
    d.append(sum, pre);
    return d;
  }
  function toolRow(name, input) {
    let what = name;
    const edit = name === 'Edit' || name === 'MultiEdit' || name === 'Write';
    if (edit) what = (name === 'Write' ? 'Write ' : 'Edit ') + rel(input.file_path);
    else if (name === 'Bash') what = '$ ' + String(input.command || '');
    else if (name === 'Read') what = 'Read ' + rel(input.file_path);
    else if (input && (input.pattern || input.path)) what = name + ' ' + (input.pattern || rel(input.path));
    const d = document.createElement('div');
    d.className = 'tool';
    d.innerHTML = '<span class="dot">●</span><span class="what"></span><span class="more"></span>';
    d.children[1].textContent = what;
    d.children[1].title = what;
    const lines = edit ? changes(name, input) : [];
    if (lines.length) d.appendChild(diffBlock(lines));
    return append(d);
  }
  function dropLive() { if (liveEl) { liveEl.remove(); liveEl = null; } if (liveTool) { liveTool.remove(); liveTool = null; } }

  let liveText = '';
  function agent(ev) {
    if (ev.type === 'stream_event') {
      if (!live) return;
      const e = ev.event || {};
      if (e.type === 'content_block_start') {
        dropLive();
        const b = e.content_block || {};
        if (b.type === 'text') { liveText = ''; liveEl = add('msg say md', '', 'Claude'); }
        else if (b.type === 'tool_use') { toolJson = ''; liveTool = toolRow(b.name + ' …', {}); liveTool.dataset.name = b.name; }
      } else if (e.type === 'content_block_delta') {
        const d = e.delta || {}, stick = pinned();
        if (d.type === 'text_delta' && liveEl) { liveText += d.text; liveEl.innerHTML = markdown(liveText); if (PC) PC.setThought(liveText); }
        else if (d.type === 'input_json_delta' && liveTool) {
          toolJson += d.partial_json || '';
          const m = toolJson.match(/"file_path"\s*:\s*"([^"]+)"/);
          liveTool.children[1].textContent = liveTool.dataset.name + ' ' + (m ? rel(m[1]) : '') + ' … ' + toolJson.length + ' chars';
        }
        if (stick) feed.scrollTop = feed.scrollHeight;
      }
      return;
    }
    if (ev.type === 'assistant') {
      dropLive();
      const msg = ev.message || {};
      (msg.content || []).forEach((b, i) => {
        const key = (msg.id || '') + ':' + (b.id || b.type + ':' + i + ':' + String(b.text || b.thinking || '').length);
        if (seen.has(key)) return;
        seen.add(key);
        if (b.type === 'text' && b.text.trim()) add('msg say md', b.text, 'Claude', true);
        else if (b.type === 'thinking' && (b.thinking || '').trim()) add('msg think', b.thinking, 'Claude');
        else if (b.type === 'tool_use') { lastWho = 'Claude'; tools[b.id] = toolRow(b.name, b.input || {}); }
      });
    } else if (ev.type === 'user') {
      const c = (ev.message || {}).content;
      (Array.isArray(c) ? c : []).forEach(b => {
        if (b.type !== 'tool_result' || !tools[b.tool_use_id]) return;
        const row = tools[b.tool_use_id];
        row.classList.add(b.is_error ? 'bad' : 'ok');
        if (b.is_error) row.children[2].textContent = typeof b.content === 'string' ? b.content : (b.content || []).map(x => x.text || '').join('\n');
      });
    } else if (ev.type === 'result') {
      dropLive();
      add('turn', ev.is_error ? 'stopped' : 'turn done');
      lastWho = '';
    }
  }

  function cell(id, text, cls) { const c = $(id); c.textContent = text; c.className = 'cell' + (id === 'agent' ? ' tail' : '') + (cls ? ' ' + cls : ''); }
  let marked = false; // the graph on show is a partial one, with broken definitions marked
  function flux(ok, partial, output) {
    const f = $('flux');
    f.hidden = ok;
    if (ok) return;
    f.innerHTML = '<b></b>\n';
    f.firstChild.textContent = partial ? 'Build failing. Broken definitions are marked; the rest is current.' : 'Build failing. The picture is the last good state.';
    if (output) f.appendChild(document.createTextNode(output.split('\n').slice(0, 14).join('\n')));
  }

  // A graph event carries the whole graph or the change from the one before it (sidecar/delta.rei):
  // fields set or dropped, and each changed list as runs [i, n] of the old list and added items.
  function patched(old, d) {
    const g = { ...old, ...(d.set || {}) };
    (d.drop || []).forEach(k => delete g[k]);
    Object.entries(d.lists || {}).forEach(([k, ops]) => { g[k] = ops.flatMap(op => (Array.isArray(op) ? old[k].slice(op[0], op[0] + op[1]) : op.add)); });
    return g;
  }

  let buildOut = '';
  const handlers = {
    hello(e) { project = e.project; $('project').textContent = e.project; $('mode').textContent = e.agent && e.agent.startsWith('script') ? '· scripted agent' : ''; },
    async caught_up() { if (latest) await learnTypes(latest); live = true; present(null, true); },
    session(e) { if (e.restart && feed.children.length) { add('turn', 'earlier session above'); lastWho = ''; } },
    replay_end() { $('mode').textContent = '· replay finished'; },
    prompt(e) { add('msg you', e.text, 'You'); },
    agent(e) { agent(e.ev); },
    agent_raw(e) { add('tool bad', e.line); },
    agent_state(e) { cell('agent', 'agent: ' + e.state, e.state === 'running' ? 'live' : ''); if (PC && live) PC.setBusy(e.state === 'running'); },
    build(e) {
      cell('build', e.ok ? 'build ok · ' + e.ms + ' ms' : 'build failing', e.ok ? '' : 'bad');
      buildOut = e.ok ? '' : e.output;
      flux(e.ok, marked, e.output); // the marks from the last partial graph still stand until a build succeeds
      if (e.ok) marked = false;
    },
    tests(e) { cell('tests', e.failing.length ? e.failing.length + ' test' + (e.failing.length > 1 ? 's' : '') + ' failing' : e.ok ? 'tests pass' : 'tests did not run', e.failing.length || !e.ok ? 'bad' : ''); },
    async graph(e) {
      if (!e.graph && !(e.delta && latest)) return;
      latest = e.graph || patched(latest, e.delta);
      drawPicker();
      drawSyntax();
      marked = !!e.partial;
      if (e.partial) flux(false, true, buildOut);
      if (!live) return; // the backlog's graph is drawn once, when it has all arrived
      await learnTypes(latest);
      // Follow the work: when the agent changes another module, the canvas goes there.
      const file = e.cause && e.cause.file, m = file ? moduleOfFile(latest, file) : '';
      if (live && moduleName && m && m !== moduleName && $('follow').classList.contains('tool-active')) choose(m, e.cause);
      else present(e.cause, false);
    },
    error(e) { add('tool bad', e.where + ': ' + e.message); },
    files(e) { files = e.files; },
  };
  // One event at a time, in order: a graph waits for its types before it is drawn.
  let turn = Promise.resolve();
  const dispatch = e => { turn = turn.then(() => (handlers[e.type] || (() => {}))(e)).catch(err => console.error(err)); };

  if (traceUrl) {
    // A recorded run, played in the page with its own timing. No sidecar involved.
    $('ask').hidden = true;
    $('mode').textContent = '· recorded run, ' + speed + '×';
    fetch(traceUrl).then(r => r.text()).then(async text => {
      const evs = text.split('\n').filter(l => l.trim()).map(l => { try { return JSON.parse(l); } catch (x) { return null; } }).filter(Boolean);
      evs.filter(e => e.type === 'files').forEach(dispatch);
      live = true;
      // Schedule against the clock, so a throttled tab plays in bursts instead of slowing down.
      const run = evs.filter(e => e.type !== 'files');
      let prev = 0, at = 0;
      const due = run.map(e => { at += Math.min(2000, ((e.t || prev) - prev) / speed); prev = e.t || prev; return at; });
      const start = performance.now();
      for (let i = 0; i < run.length;) {
        const now = performance.now() - start;
        while (i < run.length && due[i] <= now) dispatch(run[i++]);
        if (i < run.length) await new Promise(r => setTimeout(r, Math.min(250, Math.max(15, due[i] - now))));
      }
      $('mode').textContent = '· recorded run, finished';
    }).catch(() => { $('mode').textContent = '· could not load the recording'; });
  } else {
    const es = new EventSource('events');
    es.onmessage = m => { let e; try { e = JSON.parse(m.data); } catch (x) { return; } dispatch(e); };
    es.onerror = () => cell('agent', 'disconnected', 'bad');
  }

  // ---- prompt box ----
  async function send() {
    const text = $('prompt').value.trim();
    if (!text) return;
    $('prompt').value = '';
    await fetch('prompt', { method: 'POST', headers: { 'Content-Type': 'application/json' }, body: JSON.stringify({ text }) });
  }
  $('ask').addEventListener('submit', ev => { ev.preventDefault(); send(); });
  $('prompt').addEventListener('keydown', ev => { if (ev.key === 'Enter' && !ev.shiftKey) { ev.preventDefault(); send(); } });
  $('stop').onclick = () => fetch('stop', { method: 'POST' });
})();
