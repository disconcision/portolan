// The code panel under the canvas: the definition picked on the canvas in a read-only
// CodeMirror, coloured by syntax.js, with folding. Two views of it:
//   definition  the lines of the definition alone;
//   file        its whole file as an outline: every other definition folded to its first
//               line, the picked one open, marked, and scrolled to.
// The panel's top edge drags to resize it. Without the CodeMirror bundle it falls back to a
// highlighted <pre>.
(function () {
  const S = window.PortolanSyntax, CM = window.CM;
  const $ = id => document.getElementById(id);
  const info = $('info'), host = $('info-code'), note = $('info-note');
  const keep = (k, v) => { try { localStorage.setItem('portolan.' + k, v); } catch (e) {} };
  const kept = k => { try { return localStorage.getItem('portolan.' + k); } catch (e) { return null; } };
  const syntaxOf = file => (/\.rei?$/.test(file) ? 're' : /\.mli?$/.test(file) ? 'ml' : null);
  const NAMES = { re: 'Reason', ml: 'OCaml' };
  let mode = kept('codeview') === 'file' ? 'file' : 'definition';
  let wanted = null, serial = 0, view = null;

  // ---- what to show: the text, its syntax, and where the picked definition is in it ----
  async function load(want) {
    const { at, files, syntax } = want, native = syntaxOf(at.file), whole = mode === 'file';
    const lines = (text, from, to) => text.split('\n').slice(from - 1, to).join('\n');
    const natively = text => ({ text: whole ? text.replace(/\n$/, '') : lines(text, at.line, at.end_line), lang: native, first: whole ? 1 : at.line, converted: false });
    if (files) return natively(files[at.file] || '');
    const file = 'file=' + encodeURIComponent(at.file);
    if (native && syntax !== native) {
      const r = await fetch('refmt?' + file + (whole ? '' : '&line=' + at.line + '&end_line=' + at.end_line) + '&to=' + syntax);
      // a converted text has its own lines, so they are numbered from 1
      if (r.ok && r.headers.get('X-Portolan-Converted') !== 'no') return { text: (await r.text()).replace(/\n$/, ''), lang: syntax, first: 1, converted: true };
    }
    const r = await fetch('source?' + file);
    if (!r.ok) throw new Error(r.status);
    return natively(await r.text());
  }
  // The item of the outline that is the picked definition: by its lines, or by its name when
  // the text was converted and the lines no longer match. The first of that name wins.
  function pickedItem(items, line, want, converted) {
    if (!converted) {
      const inside = items.filter(it => line(it.a) <= want.at.line && line(it.b) >= want.at.line);
      return inside.sort((x, y) => y.depth - x.depth)[0] || null;
    }
    const path = String(want.at.path || '').split('/').slice(1), name = path.pop() || want.name;
    const chain = it => (it ? [...chain(it.up), it.name] : []);
    const named = items.filter(it => it.name === name);
    return named.find(it => chain(it.up).join('/') === path.join('/')) || named[0] || null;
  }

  // ---- the editor ----
  const marks = {};
  const mark = c => marks[c] || (marks[c] = CM.view.Decoration.mark({ class: c }));
  // The first token that ends after `pos`.
  function tokenAt(toks, pos) {
    let lo = 0, hi = toks.length;
    while (lo < hi) { const mid = (lo + hi) >> 1; if (toks[mid].b <= pos) lo = mid + 1; else hi = mid; }
    return lo;
  }
  // What folding a line hides. A bracket that opens on the line and closes on a later one is
  // folded to its inside; otherwise a definition that starts there is folded to its first line;
  // otherwise the more-indented lines under it.
  function foldAt(state, parsed, lineFrom, lineTo) {
    const { toks, items } = parsed, line = state.doc.lineAt(lineFrom), indent = l => l.text.search(/\S/);
    if (indent(line) < 0) return null;
    // a comment folds to its first line, and nothing folds inside one
    const lead = toks[tokenAt(toks, line.from + indent(line))];
    if (lead && lead.t === 'com' && lead.a <= line.from + indent(line)) return lead.a >= lineFrom && lead.b > lineTo ? { from: lineTo, to: lead.b } : null;
    let open = null;
    for (let i = tokenAt(toks, lineFrom); i < toks.length && toks[i].a < lineTo; i++) {
      const t = toks[i];
      if (t.mate && t.mate.a > lineTo && t.b < t.mate.a) open = t;
    }
    const item = items.find(it => it.a >= lineFrom && it.a <= lineTo && it.b > lineTo);
    // a definition whose body is not that bracket (its parameters run over lines, say) folds whole
    if (item && !(open && state.doc.lineAt(open.mate.a).number === state.doc.lineAt(item.b).number)) return { from: lineTo, to: item.b };
    if (open) return { from: open.b, to: open.mate.a };
    let last = null;
    for (let n = line.number + 1; n <= state.doc.lines; n++) {
      const l = state.doc.line(n);
      if (indent(l) < 0) continue;
      if (indent(l) <= indent(line)) break;
      last = l;
    }
    return last ? { from: lineTo, to: last.to } : null;
  }
  function chevron(open) {
    const el = document.createElement('span');
    el.className = 'hz-fold-mark' + (open ? ' open' : '');
    el.innerHTML = '<svg viewBox="0 0 8 8" width="8" height="8" aria-hidden="true"><path d="M2.5 1 L6 4 L2.5 7" fill="none" stroke="currentColor" stroke-width="1.3" stroke-linecap="round" stroke-linejoin="round"/></svg>';
    el.title = open ? 'Fold' : 'Unfold';
    return el;
  }
  function editor({ text, lang, first, converted }, want) {
    const { EditorState, StateField, RangeSetBuilder } = CM.state;
    const { EditorView, Decoration, ViewPlugin, lineNumbers, keymap, GutterMarker, gutterLineClass } = CM.view;
    const { codeFolding, foldGutter, foldService, foldEffect, foldKeymap } = CM.language;
    const parse = src => {
      if (!lang) return { toks: [], items: [] };
      const toks = S.tokens(src, lang);
      return { toks, items: S.outline(src, toks, lang) };
    };
    const now = parse(text);
    const parsed = StateField.define({ create: () => now, update: (v, tr) => (tr.docChanged ? parse(tr.state.doc.toString()) : v) });
    const paint = v => {
      const { toks } = v.state.field(parsed), b = new RangeSetBuilder();
      let i = 0;
      for (const { from, to } of v.visibleRanges) {
        for (i = Math.max(i, tokenAt(toks, from)); i < toks.length && toks[i].a < to; i++) {
          const c = S.cls(toks[i]);
          if (c) b.add(toks[i].a, toks[i].b, mark(c));
        }
      }
      return b.finish();
    };
    const colours = ViewPlugin.fromClass(class {
      constructor(v) { this.decorations = paint(v); }
      update(u) { if (u.docChanged || u.viewportChanged) this.decorations = paint(u.view); }
    }, { decorations: p => p.decorations });

    const base = [
      parsed, colours,
      EditorState.readOnly.of(true), EditorView.editable.of(false),
      EditorView.contentAttributes.of({ tabindex: '0', 'aria-label': 'Source of the picked definition' }),
      lineNumbers({ formatNumber: n => String(n + first - 1) }),
      codeFolding({
        preparePlaceholder: (state, range) => {
          const from = state.doc.lineAt(range.from), to = state.doc.lineAt(range.to);
          return to.number - from.number - (to.to > range.to ? 1 : 0);
        },
        placeholderDOM: (v, onclick, hidden) => {
          const chip = document.createElement('span');
          chip.className = 'hz-fold';
          chip.textContent = hidden > 0 ? '…+' + hidden : '…';
          chip.title = hidden > 0 ? hidden + ' more line' + (hidden > 1 ? 's' : '') + '. Click to unfold.' : 'Click to unfold.';
          chip.onclick = onclick;
          return chip;
        },
      }),
      foldService.of((state, from, to) => foldAt(state, state.field(parsed), from, to)),
      foldGutter({ markerDOM: chevron }),
      keymap.of(foldKeymap),
    ];
    const doc = CM.state.Text.of(text.split('\n')), effects = [], extra = [];
    const picked = mode === 'file' ? pickedItem(now.items, pos => doc.lineAt(pos).number, want, converted) : null;
    if (picked) {
      // mark the picked definition's lines, in the text and in the gutter
      const lines = new RangeSetBuilder(), gutter = new RangeSetBuilder();
      const tag = new (class extends GutterMarker { elementClass = 'hz-picked'; })();
      for (let n = doc.lineAt(picked.a).number; n <= doc.lineAt(picked.b).number; n++) {
        const at = doc.line(n).from;
        lines.add(at, at, Decoration.line({ class: 'hz-picked' }));
        gutter.add(at, at, tag);
      }
      extra.push(EditorView.decorations.of(lines.finish()), gutterLineClass.of(gutter.finish()));
      // scroll to it, or to the comment that sits directly on top of it
      let k = tokenAt(now.toks, picked.a), top = picked.a;
      while (k > 0 && (now.toks[k - 1].t === 'com' || now.toks[k - 1].t === 'attr') && !/\n\s*\n/.test(text.slice(now.toks[k - 1].b, top))) top = now.toks[--k].a;
      effects.push(EditorView.scrollIntoView(top, { y: 'start', yMargin: 4 }));
    }
    const state = EditorState.create({ doc, extensions: [base, extra] });
    if (mode === 'file') {
      // every definition but the picked one, what holds it and what it holds, folded to a line
      const within = (it, up) => !!it && (it === up || within(it.up, up));
      now.items.forEach(it => {
        if (picked && (within(it, picked) || within(picked, it))) return;
        const line = doc.lineAt(it.a), fold = it.b > line.to && foldAt(state, now, line.from, line.to);
        if (fold) effects.push(foldEffect.of(fold));
      });
    }
    if (!view) { host.textContent = ''; view = new EditorView({ state, parent: host }); } else view.setState(state);
    view.scrollDOM.scrollTop = 0;
    if (effects.length) view.dispatch({ effects });
  }

  async function draw() {
    const want = wanted, mine = ++serial;
    $('code-definition').classList.toggle('tool-active', mode === 'definition');
    $('code-file').classList.toggle('tool-active', mode === 'file');
    info.classList.toggle('has-code', !!(want && want.at));
    if (!want || !want.at) { note.textContent = ''; if (view) { view.destroy(); view = null; } host.textContent = ''; return; }
    let got;
    try { got = await load(want); } catch (e) { got = { text: '(could not read ' + want.at.file + ')', lang: null, first: 1, converted: false }; }
    if (mine !== serial) return; // something else was picked meanwhile
    note.textContent = got.converted ? 'converted from ' + NAMES[syntaxOf(want.at.file)] : '';
    note.title = got.converted ? 'The file is written in ' + NAMES[syntaxOf(want.at.file)] + '. The line numbers are those of the converted text, not of the file.' : '';
    if (CM) editor(got, want);
    else host.innerHTML = '<pre class="hz-plain">' + (got.lang ? S.html(got.text, got.lang) : got.text.replace(/&/g, '&amp;').replace(/</g, '&lt;')) + '</pre>';
  }
  function setMode(m) { mode = m; keep('codeview', m); draw(); }
  $('code-definition').onclick = () => setMode('definition');
  $('code-file').onclick = () => setMode('file');

  // ---- the panel's height: drag its top edge, or focus the edge and use the arrow keys ----
  const grip = $('info-grip'), pane = $('canvas-pane');
  const resized = () => window.dispatchEvent(new Event('resize')); // the canvas sizes itself by script
  function setHeight(h) {
    if (h == null) { info.style.removeProperty('--info-h'); delete info.dataset.sized; keep('info-h', ''); return; }
    h = Math.round(Math.max(72, Math.min(h, pane.clientHeight - 140)));
    info.style.setProperty('--info-h', h + 'px');
    info.dataset.sized = '';
    keep('info-h', h);
  }
  if (+kept('info-h')) setHeight(+kept('info-h'));
  grip.addEventListener('pointerdown', ev => {
    ev.preventDefault();
    grip.setPointerCapture(ev.pointerId);
    const move = e => setHeight(info.getBoundingClientRect().bottom - e.clientY);
    grip.addEventListener('pointermove', move);
    grip.addEventListener('pointerup', () => { grip.removeEventListener('pointermove', move); resized(); }, { once: true });
  });
  grip.addEventListener('dblclick', () => { setHeight(null); resized(); });
  grip.addEventListener('keydown', ev => {
    if (ev.key !== 'ArrowUp' && ev.key !== 'ArrowDown') return;
    ev.preventDefault();
    setHeight(info.clientHeight + (ev.key === 'ArrowUp' ? 24 : -24));
    resized();
  });

  window.PortolanCode = {
    // {at: {file, line, end_line, path}, name, files, syntax}: `files` is a recorded run's own
    // copy of the sources, `syntax` the one to show ('ml' or 're'). No `at` clears the panel.
    show(want) { wanted = want; return draw(); },
    clear() { wanted = null; return draw(); },
  };
})();
