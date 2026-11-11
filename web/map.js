// The map of modules: an experiment (logbook entry 7). The whole project as one small graph the
// canvas can draw: a node per top-level module, an arrow from a module to each module it uses,
// and a region per folder. It is made from the project's graph on the page; nothing new is extracted.
window.PortolanMap = (function () {
  const MAP = '\u0001map'; // the picker's value for this view; no module can have this name
  const top = x => (x.m_path || [])[0] || '';

  function graph(g) {
    const owner = new Map(), mods = new Map(), byKey = new Map(g.nodes.map(n => [n.key, n]));
    const defs = [...g.nodes.filter(n => n.kind === 'alias'), ...g.edges, ...(g.values || [])];
    defs.forEach(x => {
      const m = top(x); if (!m) return;
      const file = (x.at && x.at.file) || '';
      if (!mods.has(m)) mods.set(m, { name: m, dir: file.includes('/') ? file.slice(0, file.lastIndexOf('/')) : '', size: 0, at: x.at ? { ...x.at, path: m, line: 1, end_line: 1 } : null });
      mods.get(m).size++;
      owner.set(x.name || x.key, m);
    });
    // Every named type a type expression mentions, through lists, tuples and the like.
    const named = (key, seen = new Set()) => {
      const n = byKey.get(key); if (!n || seen.has(key)) return [];
      seen.add(key);
      return n.kind === 'alias' ? [key] : (n.parts || []).flatMap(p => named(p, seen));
    };
    const uses = new Map(); // "A B" -> the names in B that A mentions
    const note = (a, name) => {
      const b = owner.get(name); if (!a || !b || a === b) return;
      const k = a + ' ' + b;
      if (!uses.has(k)) uses.set(k, new Set());
      uses.get(k).add(name.slice(b.length + 1));
    };
    g.edges.forEach(e => { (e.deps || []).forEach(d => note(top(e), d)); [e.src, e.dst].forEach(k => named(k).forEach(t => note(top(e), t))); });
    (g.values || []).forEach(v => { (v.deps || []).forEach(d => note(top(v), d)); named(v.key).forEach(t => note(top(v), t)); });
    g.nodes.forEach(n => { if (n.kind === 'alias') (n.parts || []).forEach(p => named(p).forEach(t => note(top(n), t))); });

    const nodes = [...mods.values()].map(m => ({
      key: m.name, label: m.name, m_path: m.dir ? [m.dir] : [], kind: 'alias', ctors: [], ty: m.size + (m.size === 1 ? ' definition' : ' definitions'),
      doc: null, err: false, parts: [], former: null, abstract: false, exported: true, at: m.at,
    }));
    const edges = [...uses.entries()].map(([k, names]) => {
      const [a, b] = k.split(' '), list = [...names];
      return {
        name: a + ' uses ' + b, label: '', m_path: [], ty: a + ' uses ' + b + ': ' + list.slice(0, 12).join(', ') + (list.length > 12 ? ', …' : ''),
        src: a, dst: b, doc: null, err: false, hole: false, main: false, deps: [], exported: true, tests: [], at: mods.get(a).at,
      };
    });
    return { nodes, edges, values: [], loose_tests: [] };
  }

  return { MAP, graph };
})();
