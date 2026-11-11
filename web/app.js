// The page: relays the sidecar's event stream into a chat feed and the canvas.
(function () {
  const $ = id => document.getElementById(id);
  const feed = $('feed');
  const params = new URLSearchParams(location.search), traceUrl = params.get('trace'), speed = Math.max(0.25, +params.get('speed') || 1);
  const PC = window.PortolanCanvas;
  let project = '', live = false, graphs = [], seen = new Set(), tools = {}, liveEl = null, liveTool = null, toolJson = '', lastWho = '';
  let files = null; // file contents carried by a recorded trace
  let view = null, which = '';

  // A project that is one compilation unit is drawn flat, as a Hazel program is:
  // the file's own module is not a region. Several files keep theirs.
  function flatten(g) {
    const items = [...g.nodes.filter(n => n.kind === 'alias'), ...g.edges, ...(g.values || [])];
    const tops = new Set(items.map(x => (x.m_path || [])[0]).filter(Boolean));
    if (tops.size !== 1 || items.some(x => !(x.m_path || []).length)) return g;
    const cut = x => ({ ...x, m_path: (x.m_path || []).slice(1) });
    return { ...g, nodes: g.nodes.map(cut), edges: g.edges.map(cut), values: (g.values || []).map(cut) };
  }

  // ---- canvas: Hazel's, ported. The plain view is only a fallback when its bundle is missing. ----
  const views = {
    hazel: {
      mount: el => PC.mount(el),
      push: (g, c) => PC.push(g, { label: (c && c.label) || 'edit', ok: true }),
      show: g => PC.show(g),
      reset: () => PC.reset(),
      onSelect: cb => PC.onSelect((name, _kind, at) => {
        const g = graphs.length ? graphs[graphs.length - 1] : null;
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
    if (graphs.length) view.show(graphs[graphs.length - 1]);
    $('which').textContent = name === 'hazel' ? 'plain view' : 'canvas';
  }
  $('which').hidden = !PC;
  $('which').onclick = () => use(which === 'hazel' ? 'plain' : 'hazel');
  use(PC ? 'hazel' : 'plain');
  const toggle = (id, set) => { const b = $(id); b.onclick = () => { const on = !b.classList.contains('tool-active'); b.classList.toggle('tool-active', on); if (PC) set(on); }; };
  toggle('pace', on => PC.setPace(on));
  toggle('follow', on => PC.setFollow(on));
  $('fit').onclick = () => PC && PC.fit();

  // ---- light / dark, and the corner variation ----
  const root = document.documentElement, keep = (k, v) => { try { localStorage.setItem('portolan.' + k, v); } catch (e) {} };
  function chrome() {
    $('theme').textContent = root.dataset.theme === 'dark' ? 'light' : 'dark';
    $('corners').textContent = root.dataset.corners === 'square' ? 'round' : 'square';
  }
  $('theme').onclick = () => {
    if (root.dataset.theme === 'dark') delete root.dataset.theme; else root.dataset.theme = 'dark';
    keep('theme', root.dataset.theme || 'light');
    chrome();
    window.dispatchEvent(new Event('resize')); // the canvas draws its dot grid by script: have it redraw in the new colour
  };
  $('corners').onclick = () => {
    if (root.dataset.corners === 'square') delete root.dataset.corners; else root.dataset.corners = 'square';
    keep('corners', root.dataset.corners || 'round');
    chrome();
  };
  chrome();

  // ---- the selected definition's code ----
  async function showSource(item, kind, at) {
    $('info-path').textContent = at ? at.path : item.label || item.name || '';
    $('info-type').textContent = kind === 'edge' && item.ty ? ': ' + item.ty : item.ty ? '= ' + item.ty : '';
    $('info-code').textContent = '';
    if (!at) return;
    try {
      const text = files ? files[at.file] || '' : await (await fetch('source?file=' + encodeURIComponent(at.file))).text();
      $('info-code').textContent = text.split('\n').slice(at.line - 1, at.end_line).join('\n');
    } catch (e) { $('info-code').textContent = '(could not read ' + at.file + ')'; }
  }

  // ---- chat feed ----
  const rel = p => { const k = '/' + project + '/'; const i = p ? p.lastIndexOf(k) : -1; return i >= 0 ? p.slice(i + k.length) : p || ''; };
  const pinned = () => feed.scrollHeight - feed.scrollTop - feed.clientHeight < 60;
  function append(d) { const stick = pinned(); feed.appendChild(d); if (stick) feed.scrollTop = feed.scrollHeight; return d; }
  function add(cls, text, who) {
    if (who && who !== lastWho) { const w = document.createElement('div'); w.className = 'who' + (who === 'You' ? ' you' : ''); w.textContent = who; append(w); }
    if (who) lastWho = who;
    const d = document.createElement('div');
    d.className = cls;
    d.textContent = text;
    return append(d);
  }
  function toolRow(name, input) {
    let what = name, more = '';
    if (name === 'Write') what = 'Write ' + rel(input.file_path) + (input.content != null ? ' (' + String(input.content).split('\n').length + ' lines)' : '');
    else if (name === 'Edit' || name === 'MultiEdit') { what = 'Edit ' + rel(input.file_path); more = String(input.new_string || '').split('\n').slice(0, 6).join('\n'); }
    else if (name === 'Bash') what = '$ ' + String(input.command || '');
    else if (name === 'Read') what = 'Read ' + rel(input.file_path);
    else if (input && (input.pattern || input.path)) what = name + ' ' + (input.pattern || rel(input.path));
    const d = document.createElement('div');
    d.className = 'tool';
    d.innerHTML = '<span class="dot">●</span><span class="what"></span><span class="more"></span>';
    d.children[1].textContent = what;
    d.children[1].title = what;
    d.children[2].textContent = more;
    return append(d);
  }
  function dropLive() { if (liveEl) { liveEl.remove(); liveEl = null; } if (liveTool) { liveTool.remove(); liveTool = null; } }

  function agent(ev) {
    if (ev.type === 'stream_event') {
      if (!live) return;
      const e = ev.event || {};
      if (e.type === 'content_block_start') {
        dropLive();
        const b = e.content_block || {};
        if (b.type === 'text') liveEl = add('msg say', '', 'Claude');
        else if (b.type === 'tool_use') { toolJson = ''; liveTool = toolRow(b.name + ' …', {}); liveTool.dataset.name = b.name; }
      } else if (e.type === 'content_block_delta') {
        const d = e.delta || {}, stick = pinned();
        if (d.type === 'text_delta' && liveEl) { liveEl.textContent += d.text; if (PC) PC.setThought(liveEl.textContent); }
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
        if (b.type === 'text' && b.text.trim()) add('msg say', b.text, 'Claude');
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

  const handlers = {
    hello(e) { project = e.project; $('project').textContent = e.project; $('mode').textContent = e.agent && e.agent.startsWith('script') ? '· scripted agent' : ''; },
    caught_up() { live = true; if (graphs.length) view.show(graphs[graphs.length - 1]); },
    replay_end() { $('mode').textContent = '· replay finished'; },
    prompt(e) { add('msg you', e.text, 'You'); },
    agent(e) { agent(e.ev); },
    agent_raw(e) { add('tool bad', e.line); },
    agent_state(e) { cell('agent', 'agent: ' + e.state, e.state === 'running' ? 'live' : ''); if (PC && live) PC.setBusy(e.state === 'running'); },
    build(e) {
      cell('build', e.ok ? 'build ok · ' + e.ms + ' ms' : 'build failing', e.ok ? 'ok' : 'bad');
      const f = $('flux');
      f.hidden = e.ok;
      if (!e.ok) { f.innerHTML = '<b>Build failing. The picture is the last good state.</b>\n'; f.appendChild(document.createTextNode(e.output.split('\n').slice(0, 14).join('\n'))); }
    },
    tests(e) { cell('tests', e.failing.length ? e.failing.length + ' test' + (e.failing.length > 1 ? 's' : '') + ' failing' : e.ok ? 'tests pass' : 'tests did not run', e.failing.length || !e.ok ? 'bad' : 'ok'); },
    graph(e) { const g = flatten(e.graph); graphs.push(g); if (live) view.push(g, e.cause); },
    error(e) { add('tool bad', e.where + ': ' + e.message); },
    files(e) { files = e.files; },
  };
  const dispatch = e => (handlers[e.type] || (() => {}))(e);

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
