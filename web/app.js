// The page: relays the sidecar's event stream into a chat feed and a canvas.
(function () {
  const $ = id => document.getElementById(id);
  const feed = $('feed');
  let project = '', live = false, graphs = [], seen = new Set(), tools = {}, liveEl = null, liveTool = null, toolJson = '';
  let view = null, which = 'plain';

  // ---- canvas: the ported Hazel canvas when its bundle is present, else the plain view ----
  const views = {
    plain: { mount: el => PlainView.mount(el), push: (g, c, a) => PlainView.push(g, c, a), reset: () => PlainView.reset(), onSelect: cb => PlainView.onSelect(cb) },
    hazel: {
      mount: el => window.PortolanCanvas.mount(el),
      push: (g, c) => window.PortolanCanvas.push(JSON.stringify(g), JSON.stringify(c || {})),
      reset: () => window.PortolanCanvas.reset && window.PortolanCanvas.reset(),
      onSelect: cb => window.PortolanCanvas.onSelect && window.PortolanCanvas.onSelect((name, kind) => {
        const g = graphs.length ? graphs[graphs.length - 1].graph : null;
        const item = g && (kind === 'node' ? g.nodes.find(n => n.key === name || n.label === name) : g.edges.find(e => e.name === name || e.label === name));
        if (item) cb(item, kind);
      }),
    },
  };
  function use(name) {
    which = name;
    $('canvas').innerHTML = '';
    view = views[name];
    view.mount($('canvas'));
    view.onSelect(showSource);
    view.reset();
    graphs.forEach(e => view.push(e.graph, e.cause, false));
    $('which').textContent = 'view: ' + name;
  }
  $('which').onclick = () => { if (window.PortolanCanvas) use(which === 'plain' ? 'hazel' : 'plain'); };
  if (!window.PortolanCanvas) $('which').title = 'The ported Hazel canvas is not built yet; this is the plain view.';
  use(window.PortolanCanvas ? 'hazel' : 'plain');

  // ---- the selected definition's code ----
  async function showSource(item, kind) {
    const at = item.at;
    $('info-path').textContent = at ? at.path : item.label || item.name;
    $('info-type').textContent = kind === 'edge' ? ': ' + item.ty : item.ty ? '= ' + item.ty : '';
    $('info-code').textContent = '';
    if (!at) return;
    try {
      const text = await (await fetch('source?file=' + encodeURIComponent(at.file))).text();
      $('info-code').textContent = text.split('\n').slice(at.line - 1, at.end_line).join('\n');
    } catch (e) { $('info-code').textContent = '(could not read ' + at.file + ')'; }
  }

  // ---- chat feed ----
  const rel = p => { const k = '/' + project + '/'; const i = p ? p.lastIndexOf(k) : -1; return i >= 0 ? p.slice(i + k.length) : p || ''; };
  const pinned = () => feed.scrollHeight - feed.scrollTop - feed.clientHeight < 60;
  function add(cls, text) {
    const stick = pinned();
    const d = document.createElement('div');
    d.className = cls;
    d.textContent = text;
    feed.appendChild(d);
    if (stick) feed.scrollTop = feed.scrollHeight;
    return d;
  }
  function toolRow(name, input) {
    let what = name, more = '';
    if (name === 'Write') { what = 'Write ' + rel(input.file_path) + ' (' + String(input.content || '').split('\n').length + ' lines)'; }
    else if (name === 'Edit' || name === 'MultiEdit') { what = 'Edit ' + rel(input.file_path); more = String(input.new_string || '').split('\n').slice(0, 6).join('\n'); }
    else if (name === 'Bash') { what = '$ ' + String(input.command || ''); }
    else if (name === 'Read') { what = 'Read ' + rel(input.file_path); }
    else if (input && (input.pattern || input.path)) { what = name + ' ' + (input.pattern || rel(input.path)); }
    const stick = pinned();
    const d = document.createElement('div');
    d.className = 'tool';
    d.innerHTML = '<span class="dot">●</span><span class="what"></span><span class="more"></span>';
    d.children[1].textContent = what;
    d.children[1].title = what;
    d.children[2].textContent = more;
    feed.appendChild(d);
    if (stick) feed.scrollTop = feed.scrollHeight;
    return d;
  }
  function dropLive() { if (liveEl) { liveEl.remove(); liveEl = null; } if (liveTool) { liveTool.remove(); liveTool = null; } }

  function agent(ev) {
    if (ev.type === 'stream_event') {
      if (!live) return;
      const e = ev.event || {};
      if (e.type === 'content_block_start') {
        dropLive();
        const b = e.content_block || {};
        if (b.type === 'text') liveEl = add('msg say', '');
        else if (b.type === 'thinking') liveEl = add('msg think', '');
        else if (b.type === 'tool_use') { toolJson = ''; liveTool = toolRow(b.name + ' …', {}); liveTool.dataset.name = b.name; }
      } else if (e.type === 'content_block_delta') {
        const d = e.delta || {};
        const stick = pinned();
        if (d.type === 'text_delta' && liveEl) liveEl.textContent += d.text;
        else if (d.type === 'thinking_delta' && liveEl) liveEl.textContent += d.thinking || '';
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
        if (b.type === 'text' && b.text.trim()) add('msg say', b.text);
        else if (b.type === 'thinking' && (b.thinking || '').trim()) add('msg think', b.thinking);
        else if (b.type === 'tool_use') tools[b.id] = toolRow(b.name, b.input || {});
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
    }
  }

  function chip(id, text, cls) { const c = $(id); c.textContent = text; c.className = 'chip' + (cls ? ' ' + cls : ''); }

  const handlers = {
    hello(e) { project = e.project; $('project').textContent = '/ ' + e.project; chip('mode', e.agent && e.agent.startsWith('script') ? 'scripted agent' : ''); },
    caught_up() { live = true; },
    replay_end() { chip('mode', 'replay finished'); },
    prompt(e) { add('msg you', e.text); },
    agent(e) { agent(e.ev); },
    agent_raw(e) { add('tool bad', e.line); },
    agent_state(e) { chip('agent', 'agent: ' + e.state, e.state === 'running' ? 'live' : ''); },
    build(e) {
      chip('build', e.ok ? 'build ok · ' + e.ms + ' ms' : 'build failing', e.ok ? 'ok' : 'bad');
      const f = $('flux');
      f.hidden = e.ok;
      if (!e.ok) { f.innerHTML = '<b>Build failing. The picture is the last good state.</b>\n'; f.appendChild(document.createTextNode(e.output.split('\n').slice(0, 14).join('\n'))); }
    },
    tests(e) { chip('tests', e.failing.length ? e.failing.length + ' test' + (e.failing.length > 1 ? 's' : '') + ' failing' : e.ok ? 'tests pass' : 'tests did not run', e.failing.length || !e.ok ? 'bad' : 'ok'); },
    graph(e) { graphs.push(e); view.push(e.graph, e.cause, live); },
    extract(e) { if (!e.ok) chip('build', 'extractor failed', 'bad'); },
    error(e) { add('tool bad', e.where + ': ' + e.message); },
  };

  const es = new EventSource('events');
  es.onmessage = m => { let e; try { e = JSON.parse(m.data); } catch (x) { return; } (handlers[e.type] || (() => {}))(e); };
  es.onerror = () => chip('agent', 'disconnected', 'bad');

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
