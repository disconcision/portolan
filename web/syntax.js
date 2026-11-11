// Tokens for OCaml and Reason, coloured the way Hazel colours code: by sort. Every token is
// part of an expression, a pattern or a type, and a keyword or delimiter takes the sort of the
// form it belongs to. There is no parser here: a lexer, then one pass over the tokens that
// keeps track of which sort it is in. It is approximate; see `sorts` for the rules.
// Used by the code panel (code.js) and by code blocks in the chat. Needs nothing else.
(function () {
  const words = s => new Set(s.split(' '));
  const KEYWORDS = {
    ml: words('and as assert begin class constraint do done downto else end exception external for fun function functor if in include inherit initializer lazy let match method module mutable new nonrec object of open private rec sig struct then to try type val virtual when while with'),
    re: words('and as assert constraint downto else exception external for fun if in include lazy let module mutable nonrec of open pri pub rec switch to try type when while'),
  };
  const OPENERS = { '(': ')', '[': ']', '{': '}', '[|': '|]', begin: 'end', struct: 'end', sig: 'end', object: 'end', do: 'done' };
  const CLOSERS = new Set([')', ']', '}', '|]', 'end', 'done']);
  const BLOCK_WORDS = new Set(['begin', 'struct', 'sig', 'object', 'do', 'end', 'done']);
  const CHAR = /'(?:\\(?:[\\'"ntbr ]|\d{3}|x[0-9a-fA-F]{2}|o[0-7]{3})|[^\\'\n])'/y;
  const NUMBER = /0[xX][\da-fA-F_]+|0[oO][0-7_]+|0[bB][01_]+|\d[\d_]*(?:\.[\d_]*)?(?:[eE][+-]?\d+)?[lLn]?/y;
  const IDENT = /[A-Za-z_][A-Za-z0-9_']*/y;
  const OPERATOR = /[!$%&*+\-.\/:<=>?@^|~#]+/y;
  const QUOTED = /\{([a-z_]*)\|/y;
  const at = (re, src, i) => { re.lastIndex = i; const m = re.exec(src); return m ? m[0] : ''; };

  // ---- the lexer: [{t, a, b, s}] with t one of com str attr num id Id kw label tv op ----
  function lex(src, lang) {
    const ml = lang === 'ml', kws = KEYWORDS[lang], toks = [], n = src.length;
    const push = (t, a, b) => { const tok = { t, a, b, s: src.slice(a, b) }; toks.push(tok); return tok; };
    const [copen, cclose] = ml ? ['(*', '*)'] : ['/*', '*/'];
    let i = 0;
    while (i < n) {
      const c = src[i];
      if (c === ' ' || c === '\n' || c === '\t' || c === '\r') { i++; continue; }
      if (src.startsWith(copen, i) && !(ml && src[i + 2] === ')')) {
        let j = i + 2, depth = 1;
        while (j < n && depth) {
          if (src.startsWith(copen, j)) { depth++; j += 2; } else if (src.startsWith(cclose, j)) { depth--; j += 2; } else j++;
        }
        push('com', i, j); i = j; continue;
      }
      if (!ml && src.startsWith('//', i)) { let j = src.indexOf('\n', i); if (j < 0) j = n; push('com', i, j); i = j; continue; }
      if (c === '"') {
        let j = i + 1;
        while (j < n && src[j] !== '"') j += src[j] === '\\' ? 2 : 1;
        j = Math.min(n, j + 1);
        push('str', i, j); i = j; continue;
      }
      if (c === '{' && at(QUOTED, src, i)) {
        const open = at(QUOTED, src, i), close = '|' + open.slice(1, -1) + '}';
        let j = src.indexOf(close, i + open.length);
        j = j < 0 ? n : j + close.length;
        push('str', i, j); i = j; continue;
      }
      if (c === "'") {
        const ch = at(CHAR, src, i);
        if (ch) { push('str', i, i + ch.length); i += ch.length; continue; }
        const id = at(IDENT, src, i + 1);
        if (id) { push('tv', i, i + 1 + id.length); i += 1 + id.length; continue; }
        push('op', i, i + 1); i++; continue;
      }
      if (c === '[' && src[i + 1] === '@') {
        let j = i + 1, depth = 1;
        while (j < n && depth) {
          if (src[j] === '"') { j++; while (j < n && src[j] !== '"') j += src[j] === '\\' ? 2 : 1; }
          else if (src[j] === '[') depth++;
          else if (src[j] === ']') depth--;
          j++;
        }
        push('attr', i, j); i = j; continue;
      }
      if (c >= '0' && c <= '9') { const num = at(NUMBER, src, i); push('num', i, i + num.length); i += num.length; continue; }
      if (c === '`' || c === '~' || (ml && c === '?')) {
        const id = at(IDENT, src, i + 1);
        if (id) {
          let j = i + 1 + id.length;
          if (c !== '`' && ml && src[j] === ':' && src[j + 1] !== ':') j++; // OCaml's ~label: is one token
          push(c === '`' ? 'Id' : 'label', i, j); i = j; continue;
        }
      }
      const id = at(IDENT, src, i);
      if (id) {
        let j = i + id.length;
        if (kws.has(id)) {
          // let%ext and the binding operators let* and+ are part of the keyword
          const ext = /%[a-z_][\w.]*/y; ext.lastIndex = j;
          const m = ext.exec(src);
          if (m) j += m[0].length;
          else if ((id === 'let' || id === 'and') && /[*+]/.test(src[j] || '') && /\s/.test(src[j + 1] || '')) j++;
          push('kw', i, j).w = id;
        } else push(c >= 'A' && c <= 'Z' ? 'Id' : 'id', i, j);
        i = j; continue;
      }
      if (src.startsWith('[|', i) || src.startsWith('|]', i) || (ml && src.startsWith(';;', i))) { push('op', i, i + 2); i += 2; continue; }
      if ('()[]{},;'.includes(c)) { push('op', i, i + 1); i++; continue; }
      let op = at(OPERATOR, src, i) || c;
      if (!ml && op.length > 1 && op[0] === '=' && op[1] === '?') op = '='; // Reason's ~x=? is two tokens
      const bar = op.indexOf('|]');
      if (bar > 0) op = op.slice(0, bar);
      push('op', i, i + op.length); i += op.length;
    }
    return toks;
  }

  // ---- brackets: each opener and closer learns its partner (`mate`) ----
  function pair(code) {
    const stack = [];
    code.forEach(t => {
      const word = t.t === 'kw' ? t.w : t.t === 'op' ? t.s : '';
      if (OPENERS[word] && (t.t === 'op' || BLOCK_WORDS.has(word))) stack.push(t);
      else if (CLOSERS.has(word)) {
        let k = stack.length - 1;
        while (k >= 0 && OPENERS[stack[k].w || stack[k].s] !== word) k--;
        if (k < 0) return; // a closer with no opener: the text starts mid-way
        stack[k].mate = t; t.mate = stack[k];
        stack.length = k;
      }
    });
  }

  // ---- sorts: gives each token k = 'exp' | 'pat' | 'typ' ----
  // One frame per open bracket. A frame has the sort it was opened in (`base`) and the sort it is
  // in now (`mode`); 'mod' is a module expression, drawn as an expression, where `:` and `=` mean
  // nothing special. The rules, roughly:
  //   let / fun / a case bar  start a pattern, which ends at = or the arrow;
  //   a colon in a binder or in parentheses starts a type, which ends at = , ; or the bracket;
  //   type starts a name (a pattern), then a type up to ; (Reason) or the next definition (OCaml);
  //   in Reason, parentheses or a name directly before => are a function's parameters.
  function sorts(code, lang) {
    const re = lang === 're', arrow = re ? '=>' : '->';
    const frame = (kind, base, tok) => ({ kind, base, mode: base, tok, decl: null, tern: 0, guard: false, ret: false, match: 0 });
    const stack = [frame('top', 'exp')];
    let f = stack[0];
    const show = m => (m === 'mod' ? 'exp' : m);
    const bracket = () => f.kind === '(' || f.kind === '[' || f.kind === '{';
    const applied = p => p && (p.t === 'id' || p.t === 'Id' || p.s === ')' || p.s === ']');
    const index = new Map(code.map((t, i) => [t, i]));
    // In Reason: is the bracket at i a function's parameters? 1 = yes, 2 = yes with a return type.
    function params(i) {
      const open = code[i];
      if (!open.mate) return 0;
      const j = index.get(open.mate), next = code[j + 1];
      if (!next) return 0;
      if (next.s === '=>') return 1;
      if (next.s !== ':' || f.tern) return 0;
      for (let k = j + 2; k < code.length; k++) {
        const t = code[k];
        if (t.s === '=>') return 2;
        if (t.t === 'op' && (t.s === ';' || t.s === ',' || t.s === '=' || t.s === '{' || CLOSERS.has(t.s))) return 0;
        if (t.mate && OPENERS[t.s]) k = index.get(t.mate);
      }
      return 0;
    }
    for (let i = 0; i < code.length; i++) {
      const t = code[i], prev = code[i - 1], next = code[i + 1], s = t.s;
      const plain = () => { t.k = show(f.mode); };
      if (t.t === 'kw') {
        const w = t.w;
        if (!re && OPENERS[w] && BLOCK_WORDS.has(w)) { t.k = 'exp'; stack.push(f = frame('block', 'exp', t)); continue; }
        if (!re && (w === 'end' || w === 'done')) {
          t.k = 'exp';
          if (t.mate) { while (stack.length > 1 && stack.pop().tok !== t.mate); f = stack[stack.length - 1]; }
          continue;
        }
        const starts = () => { t.k = 'exp'; };
        if (w === 'let' || w === 'external' || w === 'val') { starts(); f.decl = 'let'; f.mode = 'pat'; }
        else if (w === 'for' && !re) { starts(); f.decl = 'let'; f.mode = 'pat'; }
        else if (w === 'and') { if (f.decl === 'let' || f.decl === 'type') { starts(); f.mode = 'pat'; } else plain(); }
        else if (w === 'type') {
          // not a definition: `module type`, `(type a)`, and `type a.` inside a binder's annotation
          if (f.mode === 'mod' || (prev && prev.s === '(') || (f.decl === 'let' && (f.mode === 'pat' || f.mode === 'typ'))) plain();
          else { starts(); f.decl = 'type'; f.mode = 'pat'; }
        }
        else if (w === 'module') { if (prev && prev.s === '(') plain(); else { starts(); f.decl = 'mod'; f.mode = 'mod'; } }
        else if (w === 'open' || w === 'include') { starts(); f.decl = 'mod'; f.mode = 'mod'; }
        else if (w === 'exception') { if (f.mode === 'pat') plain(); else { starts(); f.decl = 'type'; f.mode = 'typ'; } }
        else if (w === 'in') { starts(); f.mode = 'exp'; }
        else if (w === 'fun' || w === 'function') { starts(); f.mode = 'pat'; }
        else if (w === 'match' || w === 'try') { starts(); if (!re) f.match++; }
        else if (w === 'with') { if (!re && f.match > 0) { starts(); f.match--; f.mode = 'pat'; } else plain(); }
        else if (w === 'when') { starts(); f.guard = true; f.mode = 'exp'; }
        else if (w === 'rec' || w === 'nonrec') starts();
        else plain();
        continue;
      }
      if (t.t !== 'op') {
        if (t.t === 'tv') t.k = 'typ';
        else if (re && f.mode === 'exp' && !f.guard && t.t === 'id' && next && next.s === '=>') t.k = 'pat';
        else plain();
        continue;
      }
      if (s === '(' || s === '[' || s === '[|' || s === '{') {
        let base = f.mode, ret = false;
        if (s === '{' && base === 'mod') base = 'exp';
        if (s === '(' && re && f.mode === 'exp' && !f.guard && !applied(prev)) {
          const p = params(i);
          if (p) { base = 'pat'; ret = p === 2; }
        }
        stack.push(f = frame(s === '[|' ? '[' : s, base, t));
        f.ret = ret;
        t.k = show(base);
      } else if (CLOSERS.has(s)) {
        if (!t.mate) { plain(); continue; }
        let closed = f;
        while (stack.length > 1 && (closed = stack.pop()).tok !== t.mate);
        f = stack[stack.length - 1];
        t.k = show(closed.base);
        if (closed.ret) { f.mode = 'typ'; f.ret = true; }
      } else if (s === ',') {
        t.k = show(bracket() ? f.base : f.mode);
        if (bracket() && (f.mode === 'typ' || (f.mode === 'exp' && f.base === 'pat'))) f.mode = f.base;
      } else if (s === ';' || s === ';;') {
        t.k = show(f.base);
        if (re || s === ';;') Object.assign(f, { mode: f.base, decl: null, tern: 0, guard: false, ret: false });
        else if (f.mode === 'typ' || f.mode === 'mod' || (f.mode === 'pat' && f.base !== 'pat')) f.mode = f.base;
      } else if (s === '=') {
        if (f.mode === 'pat') {
          if (f.base === 'pat' && (f.kind === '{' || f.kind === '[')) t.k = 'pat'; // a field of an OCaml record pattern
          else if (f.decl === 'type') { t.k = 'exp'; f.mode = 'typ'; }
          else { t.k = f.base === 'pat' ? 'pat' : 'exp'; f.mode = 'exp'; }
        } else if (f.mode === 'typ' && f.base !== 'typ' && f.decl !== 'type') { t.k = f.base === 'pat' ? 'pat' : 'exp'; f.mode = 'exp'; }
        else plain();
      } else if (s === ':' || s === ':>') {
        if (f.mode === 'pat' && !(f.kind === '{' && f.base === 'pat')) { t.k = 'pat'; f.mode = 'typ'; }
        else if (f.mode === 'exp' && f.tern) { f.tern--; plain(); }
        else if (f.mode === 'exp' && f.kind === '(') { t.k = 'exp'; f.mode = 'typ'; }
        else plain();
      } else if (s === '?') {
        if (re && f.mode === 'exp' && prev && prev.s !== '=' && prev.t !== 'label') f.tern++;
        plain();
      } else if (s === arrow) {
        if (f.mode === 'pat') { t.k = 'exp'; f.mode = 'exp'; }
        else if (f.mode === 'typ' && f.ret) { t.k = 'exp'; f.mode = 'exp'; f.ret = false; }
        else plain();
        f.guard = false;
      } else if (s === '|') {
        if (f.mode === 'exp') { t.k = 'exp'; f.mode = 'pat'; f.guard = false; }
        else if (f.mode === 'pat' && prev && ((prev.t === 'kw' && (prev.w === 'with' || prev.w === 'fun' || prev.w === 'function')) || prev === f.tok)) t.k = 'exp';
        else plain();
      } else plain();
    }
  }

  // ---- definitions: the items of a file, nested through modules, for the outline ----
  // {name, word, a, b, depth, up}. Reason items end at their `;`. OCaml has no terminator, so an
  // item is a definition keyword that starts a line at the column its neighbours start at.
  const ITEM_WORDS = new Set(['let', 'type', 'module', 'open', 'include', 'exception', 'external', 'val', 'and', 'class']);
  function outline(src, toks, lang) {
    const code = toks.filter(t => t.t !== 'com' && t.t !== 'attr'), index = new Map(code.map((t, i) => [t, i])), items = [];
    const column = t => t.a - (src.lastIndexOf('\n', t.a - 1) + 1);
    const leads = t => !src.slice(src.lastIndexOf('\n', t.a - 1) + 1, t.a).trim();
    const skip = i => (code[i].mate && OPENERS[code[i].w || code[i].s] ? index.get(code[i].mate) : i);
    function name(i, hi) {
      const word = code[i].w;
      let j = i + 1;
      while (j < hi && code[j].t === 'kw' && (code[j].w === 'rec' || code[j].w === 'nonrec' || (word === 'module' && code[j].w === 'type'))) j++;
      if (word === 'type') while (j < hi && (code[j].t === 'tv' || code[j].s === '(')) j = skip(j) + 1; // OCaml's 'a t
      const t = code[j];
      return j < hi && t && (t.t === 'id' || t.t === 'Id') ? t.s : '';
    }
    function walk(lo, hi, depth, up) {
      let col = -1, start = -1;
      const close = end => {
        if (start < 0 || end <= start) return;
        const first = code[start], item = { name: first.t === 'kw' ? name(start, end) : '', word: first.w || '', a: first.a, b: code[end - 1].b, depth, up };
        items.push(item);
        if (item.word === 'module') {
          for (let k = start; k < end; k++) {
            const t = code[k], w = t.w || t.s;
            if (t.mate && (w === '{' || w === 'struct' || w === 'sig')) { walk(k + 1, index.get(t.mate), depth + 1, item); break; }
            k = skip(k);
          }
        }
        start = -1;
      };
      for (let i = lo; i < hi; i++) {
        const t = code[i];
        if (lang === 're') {
          if (start < 0) start = i;
          if (t.s === ';' && t.t === 'op') close(i + 1);
        } else if (t.t === 'kw' && ITEM_WORDS.has(t.w) && leads(t) && (col < 0 || column(t) === col)) {
          if (col < 0) col = column(t);
          close(i);
          start = i;
        } else if (start < 0) start = i;
        i = skip(i);
      }
      close(hi);
    }
    walk(0, code.length, 0, null);
    return items;
  }

  // The tokens of a text, with sorts. `lang` is 'ml' or 're'.
  function tokens(src, lang) {
    const toks = lex(src, lang), code = toks.filter(t => t.t !== 'com' && t.t !== 'attr');
    pair(code);
    sorts(code, lang);
    return toks;
  }
  // The class a token is drawn with, '' for a plain expression token.
  function cls(t) {
    if (t.t === 'com') return 'hz-com';
    if (t.t === 'str') return 'hz-str';
    if (t.t === 'attr') return 'hz-attr';
    const sort = t.k === 'pat' ? 'hz-pat' : t.k === 'typ' ? 'hz-typ' : '';
    return t.t === 'kw' ? (sort ? sort + ' hz-kw' : 'hz-kw') : sort;
  }

  // ---- the rest: strings and comments only, for the other languages the agent quotes ----
  const PLAIN = {
    sh: /(#.*)|("(?:[^"\\]|\\.)*"|'[^']*')/g,
    json: /((?!))|("(?:[^"\\]|\\.)*")/g,
    dune: /(;.*)|("(?:[^"\\]|\\.)*")/g,
    js: /(\/\/.*|\/\*[\s\S]*?\*\/)|("(?:[^"\\\n]|\\.)*"|'(?:[^'\\\n]|\\.)*'|`(?:[^`\\]|\\.)*`)/g,
  };
  const NAMES = {
    ocaml: 'ml', ml: 'ml', mli: 'ml', reason: 're', reasonml: 're', re: 're', rei: 're',
    sh: 'sh', bash: 'sh', shell: 'sh', zsh: 'sh', console: 'sh', json: 'json', jsonl: 'json', dune: 'dune', sexp: 'dune', lisp: 'dune', scheme: 'dune',
    js: 'js', javascript: 'js', ts: 'js', typescript: 'js', c: 'js', css: 'js', diff: 'diff', patch: 'diff',
  };
  const esc = s => s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  const span = (c, s) => (c ? '<span class="' + c + '">' + esc(s) + '</span>' : esc(s));

  // Highlighted HTML for a text in a language named as a Markdown fence names it; plain if unknown.
  function html(src, name) {
    const lang = NAMES[String(name || '').trim().toLowerCase().split(/\s/)[0]];
    if (lang === 'ml' || lang === 're') {
      let out = '', i = 0;
      tokens(src, lang).forEach(t => { out += esc(src.slice(i, t.a)) + span(cls(t), t.s); i = t.b; });
      return out + esc(src.slice(i));
    }
    if (lang === 'diff') return src.split('\n').map(l => span(/^\+(?!\+\+ )/.test(l) ? 'hz-add' : /^-(?!-- )/.test(l) ? 'hz-del' : /^@@/.test(l) ? 'hz-com' : '', l)).join('\n');
    if (!PLAIN[lang]) return esc(src);
    let out = '', i = 0;
    src.replace(PLAIN[lang], (m, com, str, k) => { out += esc(src.slice(i, k)) + span(com ? 'hz-com' : 'hz-str', m); i = k + m.length; return m; });
    return out + esc(src.slice(i));
  }

  window.PortolanSyntax = { tokens, cls, outline, html, known: name => !!NAMES[String(name || '').trim().toLowerCase().split(/\s/)[0]] };
})();
