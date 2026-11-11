// The plain view: a small stand-in for the ported Hazel canvas. It draws the same
// graph record (types as nodes, functions as arrows, modules as hulls, tests as pips)
// with a simple column layout. Positions never move once given, new things arrive one
// at a time behind a three-dot marker for the agent. It exists so the whole loop can
// be watched before the real canvas is in; it is not where layout work should go.
(function () {
  const GLYPH = { int: 'ℤ', bool: '𝔹', string: '𝕊', float: '𝔽', unit: '𝟙', char: 'c' };
  const R = { alias: 13, builtin: 9, derived: 10, ghost: 10, product: 5 };
  const STEP = 260;
  let host, pos = {}, used = {}, prevIds = new Set(), sel = null, onSel = () => {}, last = null;
  let queue = [], busy = false, hullColor = {}, trineAt = null;

  const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
  const short = (s, n) => (s.length > n ? s.slice(0, n - 1) + '…' : s);

  function place(g) {
    const rank = {};
    g.nodes.forEach(n => (rank[n.key] = 0));
    for (let i = 0; i < 8; i++) {
      g.nodes.forEach(n => { if (n.kind === 'product' || n.kind === 'derived') (n.parts || []).forEach(p => { if (p in rank) rank[n.key] = Math.min(6, Math.max(rank[n.key], rank[p] + 1)); }); });
      g.edges.forEach(e => { if (e.src !== e.dst && e.src in rank && e.dst in rank) rank[e.dst] = Math.min(6, Math.max(rank[e.dst], rank[e.src] + 1)); });
    }
    const near = {};
    const link = (a, b) => { (near[a] = near[a] || []).push(b); (near[b] = near[b] || []).push(a); };
    g.edges.forEach(e => link(e.src, e.dst));
    g.nodes.forEach(n => (n.parts || []).forEach(p => link(n.key, p)));
    g.nodes.forEach(n => {
      if (pos[n.key]) return;
      const r = rank[n.key], col = (used[r] = used[r] || new Set());
      const ys = (near[n.key] || []).filter(k => pos[k]).map(k => pos[k].y);
      let want = ys.length ? Math.round((ys.reduce((a, b) => a + b, 0) / ys.length - 70) / 92) : col.size;
      want = Math.max(0, want);
      let slot = want;
      for (let d = 0; d < 60; d++) { if (!col.has(want + d)) { slot = want + d; break; } if (want - d >= 0 && !col.has(want - d)) { slot = want - d; break; } }
      col.add(slot);
      pos[n.key] = { x: 90 + r * 200, y: 70 + slot * 92 };
    });
  }

  function geometry(g) {
    const groups = {}, out = {};
    g.edges.forEach(e => { const k = e.src + '>' + e.dst; (groups[k] = groups[k] || []).push(e.name); });
    g.edges.forEach(e => {
      const a = pos[e.src], b = pos[e.dst];
      if (!a || !b) return;
      const grp = groups[e.src + '>' + e.dst], i = grp.indexOf(e.name), n = grp.length;
      if (e.src === e.dst) {
        const s = 34 + i * 24;
        out[e.name] = { d: `M ${a.x - 9} ${a.y + 9} C ${a.x - s - 10} ${a.y + s + 16}, ${a.x + s + 10} ${a.y + s + 16}, ${a.x + 9} ${a.y + 10}`, mx: a.x, my: a.y + s * 0.75 + 12, tip: [a.x + 9, a.y + 10], dir: [-0.6, -0.8] };
        return;
      }
      const dx = b.x - a.x, dy = b.y - a.y, L = Math.hypot(dx, dy) || 1, ux = dx / L, uy = dy / L, px = -uy, py = ux;
      const off = (i - (n - 1) / 2) * 30 + (groups[e.dst + '>' + e.src] ? 14 : 0);
      const x1 = a.x + ux * 15, y1 = a.y + uy * 15, x2 = b.x - ux * 16, y2 = b.y - uy * 16;
      const cx = (x1 + x2) / 2 + px * off * 2, cy = (y1 + y2) / 2 + py * off * 2;
      const tx = x2 - cx, ty = y2 - cy, tl = Math.hypot(tx, ty) || 1;
      out[e.name] = { d: `M ${x1} ${y1} Q ${cx} ${cy} ${x2} ${y2}`, mx: (x1 + 2 * cx + x2) / 4, my: (y1 + 2 * cy + y2) / 4, tip: [x2, y2], dir: [tx / tl, ty / tl] };
    });
    return out;
  }

  function hulls(g, geo) {
    const members = {};
    const add = (path, x, y) => { if (path && path.length) (members[path.join('.')] = members[path.join('.')] || []).push([x, y]); };
    g.nodes.forEach(n => { if (n.kind === 'alias' && pos[n.key]) add(n.m_path, pos[n.key].x, pos[n.key].y); });
    g.edges.forEach(e => { if (geo[e.name]) add(e.m_path, geo[e.name].mx, geo[e.name].my); });
    let s = '', names = '';
    Object.keys(members).sort().forEach(m => {
      if (!(m in hullColor)) hullColor[m] = Object.keys(hullColor).length % 4;
      const pts = members[m], cx = pts.reduce((a, p) => a + p[0], 0) / pts.length, cy = pts.reduce((a, p) => a + p[1], 0) / pts.length;
      let c = '';
      pts.forEach(([x, y]) => {
        c += `<circle cx="${x}" cy="${y}" r="46"/>`;
        const L = Math.hypot(cx - x, cy - y), k = Math.floor(L / 34);
        for (let j = 1; j <= k; j++) c += `<circle cx="${x + (cx - x) * j / (k + 1)}" cy="${y + (cy - y) * j / (k + 1)}" r="26"/>`;
      });
      s += `<g class="hull" filter="url(#pv-goo)" fill="var(--hull-${hullColor[m]})">${c}</g>`;
      const top = pts.reduce((a, p) => (p[1] < a[1] ? p : a), pts[0]);
      names += `<text class="hname" x="${top[0] - 40}" y="${top[1] - 50}">${esc(m.toUpperCase())}</text>`;
    });
    return s + names;
  }

  function draw(g, fresh, animate) {
    place(g);
    const geo = geometry(g), order = {};
    fresh.forEach((id, i) => (order[id] = i));
    const cls = id => (animate && id in order ? ' arrive' : '');
    const delay = id => (animate && id in order ? ` style="animation-delay:${order[id] * STEP}ms"` : '');
    const xs = Object.values(pos).map(p => p.x), ys = Object.values(pos).map(p => p.y);
    const vb = xs.length ? [Math.min(...xs) - 130, Math.min(...ys) - 110, Math.max(...xs) - Math.min(...xs) + 280, Math.max(...ys) - Math.min(...ys) + 250] : [0, 0, 600, 400];
    let s = `<svg class="pv" viewBox="${vb.join(' ')}" preserveAspectRatio="xMidYMid meet"><defs><filter id="pv-goo" x="-30%" y="-30%" width="160%" height="160%"><feGaussianBlur in="SourceGraphic" stdDeviation="11"/><feColorMatrix values="1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 24 -10"/></filter></defs>`;
    s += hulls(g, geo);
    g.nodes.forEach(n => (n.parts || []).forEach(p => { const a = pos[p], b = pos[n.key]; if (a && b && (n.kind === 'product' || n.kind === 'derived')) s += `<path class="part" d="M ${a.x} ${a.y} L ${b.x} ${b.y}"/>`; }));
    g.edges.forEach(e => {
      const q = geo[e.name];
      if (!q) return;
      const w = short(e.label, 22).length * 6.4 + 14, [tx, ty] = q.tip, [ux, uy] = q.dir, px = -uy, py = ux;
      const id = 'e:' + e.name;
      s += `<g class="edge${e.main ? ' main' : ''}${e.hole ? ' hole' : ''}${sel === id ? ' sel' : ''}${cls(id)}" data-id="${esc(id)}"${delay(id)}>` +
        `<path class="wire" d="${q.d}"/><polygon points="${tx},${ty} ${tx - ux * 8 + px * 3.6},${ty - uy * 8 + py * 3.6} ${tx - ux * 8 - px * 3.6},${ty - uy * 8 - py * 3.6}"/>` +
        `<rect class="pill" x="${q.mx - w / 2}" y="${q.my - 9}" width="${w}" height="18" rx="9"/><text x="${q.mx}" y="${q.my + 3.6}">${esc(short(e.label, 22))}</text>` +
        (e.tests || []).map((t, i) => `<circle class="pip ${t.status}" cx="${q.mx - ((e.tests.length - 1) * 7) / 2 + i * 7}" cy="${q.my + 15}" r="2.7"><title>${esc(t.name)}</title></circle>`).join('') + `</g>`;
    });
    const vals = {};
    (g.values || []).forEach(v => (vals[v.key] = vals[v.key] || []).push(v.label));
    g.nodes.forEach(n => {
      const p = pos[n.key], r = R[n.kind] || 10, id = 'n:' + n.key;
      const glyph = n.kind === 'alias' ? 'τ' : n.kind === 'builtin' ? GLYPH[n.label] || '·' : n.kind === 'ghost' ? '?' : n.kind === 'derived' ? '◦' : '';
      s += `<g class="node ${n.kind}${sel === id ? ' sel' : ''}${cls(id)}" data-id="${esc(id)}"${delay(id)}><circle cx="${p.x}" cy="${p.y}" r="${r}"/>` +
        (glyph ? `<text class="glyph" x="${p.x}" y="${p.y + 3.6}">${glyph}</text>` : '') +
        (n.kind !== 'product' ? `<text class="name" x="${p.x}" y="${p.y + r + 13}">${esc(short(n.label, 20))}</text>` : '') +
        (vals[n.key] ? `<text class="vals" x="${p.x}" y="${p.y + r + 25}">· ${esc(short(vals[n.key].join(', '), 26))}</text>` : '') + `</g>`;
    });
    const at = trineAt || { x: vb[0] + 40, y: vb[1] + 40 };
    s += `<g class="trine" id="pv-trine" style="transform:translate(${at.x}px,${at.y}px)"><line x1="0" y1="0" x2="-15" y2="-9"/><line x1="-15" y1="-9" x2="-13" y2="10"/><line x1="-13" y1="10" x2="0" y2="0"/><circle r="3.4"/><circle cx="-15" cy="-9" r="3.4"/><circle cx="-13" cy="10" r="3.4"/><g class="say" id="pv-say"></g></g>`;
    host.innerHTML = s + '</svg>';
    return geo;
  }

  function play(item) {
    const { graph: g, cause, animate } = item;
    const ids = [...g.nodes.map(n => 'n:' + n.key), ...g.edges.map(e => 'e:' + e.name)];
    const fresh = ids.filter(id => !prevIds.has(id));
    const geo = draw(g, fresh, animate);
    prevIds = new Set(ids);
    last = g;
    if (!animate || !fresh.length) return 0;
    const label = cause && cause.label ? cause.label + (cause.file ? ' ' + cause.file : '') : '';
    fresh.forEach((id, i) => setTimeout(() => {
      const t = document.getElementById('pv-trine');
      if (!t) return;
      const p = id[0] === 'n' ? pos[id.slice(2)] : geo[id.slice(2)] && { x: geo[id.slice(2)].mx, y: geo[id.slice(2)].my };
      if (!p) return;
      trineAt = { x: p.x - 26, y: p.y - 22 };
      t.style.transform = `translate(${trineAt.x}px,${trineAt.y}px)`;
      const say = document.getElementById('pv-say');
      if (say && label && i === 0) { const w = label.length * 6.3 + 14; say.innerHTML = `<rect x="${-w - 22}" y="-34" width="${w}" height="19" rx="6"/><text x="${-w - 15}" y="-21">${esc(label)}</text>`; }
    }, i * STEP));
    return fresh.length * STEP + 500;
  }

  function pump() {
    if (busy || !queue.length) return;
    busy = true;
    const wait = play(queue.shift());
    setTimeout(() => { busy = false; pump(); }, wait);
  }

  window.PlainView = {
    mount(el) {
      host = el;
      host.addEventListener('click', ev => {
        const t = ev.target.closest('[data-id]');
        if (!t || !last) return;
        sel = t.dataset.id;
        host.querySelectorAll('.sel').forEach(x => x.classList.remove('sel'));
        t.classList.add('sel');
        const item = sel[0] === 'n' ? last.nodes.find(n => 'n:' + n.key === sel) : last.edges.find(e => 'e:' + e.name === sel);
        if (item) onSel(item, sel[0] === 'n' ? 'node' : 'edge');
      });
    },
    push(graph, cause, animate) { queue.push({ graph, cause, animate }); pump(); },
    reset() { pos = {}; used = {}; prevIds = new Set(); queue = []; busy = false; trineAt = null; last = null; if (host) host.innerHTML = ''; },
    onSelect(cb) { onSel = cb; },
  };
})();
