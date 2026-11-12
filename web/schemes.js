// Portolan's colour schemes, shared by the page (the scheme menu beside light/dark) and by the
// colour-schemes study, where they are edited. A scheme gives a colour to each role in ROLES;
// vars() turns one into the CSS variables the page and the canvas read. What the editor changes
// is kept in localStorage under KEY, so every tab on the same origin sees it.
window.PortolanSchemes = (function () {
  const ROLES = [
    ['Page', [['ui', 'Background'], ['surface', 'Surfaces'], ['text', 'Text'], ['muted', 'Quiet text'], ['strong', 'Headings'], ['line', 'Lines'], ['highlight', 'Highlight']]],
    ['Top bar', [['topbar', 'Bar'], ['logoBg', 'Behind the logo'], ['logo', 'Logo'], ['brand', 'Brand tab'], ['brandInk', 'Brand tab text'], ['tab', 'Tabs'], ['tabInk', 'Tab text']]],
    ['Canvas header', [['header', 'Header'], ['title', 'Title'], ['tool', 'Buttons'], ['toolOn', 'Buttons, on']]],
    ['Panels', [['code', 'Code panel'], ['chat', 'Chat pane'], ['say', 'Your message'], ['composer', 'Message box'], ['send', 'Send button']]],
    ['Bottom bar', [['status', 'Bar'], ['run', 'Run cell'], ['agentCell', 'Agent cell']]],
    ['Canvas', [['board', 'Board'], ['dots', 'Dot grid'], ['type', 'Types'], ['arrow', 'Arrows'], ['label', 'Function names'], ['chip', 'Name chips'], ['guide', 'Guide lines']]],
    ['Modules', [['hull1', 'Region 1'], ['hull2', 'Region 2'], ['hull3', 'Region 3'], ['hull4', 'Region 4'], ['hullLabel', 'Region names']]],
    ['Tests and selection', [['pass', 'Passing'], ['fail', 'Failing'], ['indet', 'Unsettled'], ['select', 'Selection']]],
    ['Agent', [['agent', 'Trine'], ['agentInk', 'Trine outline'], ['bubble', 'Bubbles'], ['bubbleInk', 'Bubble ink']]],
  ];
  const S = (id, name, blurb, c) => ({ id, name, blurb, c });
  // The parts of the page follow the page's main colours until they are given their own.
  const derive = c => {
    const d = { topbar: c.ui, tab: mix(c.ui, c.text, 0.09), brandInk: on(c.brand, [c.strong, c.ui, c.surface]), header: c.ui, code: c.surface, chat: c.ui, status: c.ui, ...c };
    return { logoBg: d.topbar, logo: d.strong, tabInk: d.text, title: d.strong, tool: d.header, toolOn: d.highlight, say: d.surface, composer: d.surface, send: d.highlight,
      run: d.tab, agentCell: mix(d.status, d.highlight, 0.5), chip: d.surface, ...d };
  };
  const GRAPHITE = { ui: '#181818', surface: '#202020', text: '#D2D2D2', muted: '#8A8A8A', strong: '#F0F0F0', line: '#3F3F3F', highlight: '#3A3A3A', brand: '#3A3A3A', board: '#121212', dots: '#303030', type: '#EDEDED', arrow: '#CFCFCF', label: '#EDEDED', guide: '#8A8A8A', hull1: '#5A5A5A', hull2: '#777777', hull3: '#454545', hull4: '#8C8C8C', hullLabel: '#A0A0A0', pass: '#EDEDED', fail: '#FF5A4A', indet: '#7A7A7A', select: '#FFFFFF', agent: '#181818', agentInk: '#EDEDED', bubble: '#262626', bubbleInk: '#EDEDED' };
  const SCHEMES = [
    S('hazel', 'Hazel', 'What the build uses in light mode: Hazel\'s cream paper, purple types, slate arrows, blue names, and yellow where Hazel has green.',
      { ui: '#FCF5E3', surface: '#FFFCF3', text: '#566D75', muted: '#AF9D6C', strong: '#736131', line: '#AF9D6C', highlight: '#F8E18A', brand: '#F8E18A', board: '#FCF5E3', dots: '#DFCC99', type: '#955BE3', arrow: '#566D75', label: '#0093D8', guide: '#333333', hull1: '#6FBFC9', hull2: '#D493C6', hull3: '#D8B56A', hull4: '#97A5E0', hullLabel: '#8A744A', pass: '#4A9A4A', fail: '#D04030', indet: '#D0B040', select: '#317749', agent: '#F3C94B', agentInk: '#5A4118', bubble: '#F8DC7A', bubbleInk: '#3A2A12' }),
    S('ink', 'Ink', 'Minimal monochrome, light. One black ink on near-white; red is kept only for a failing test.',
      { ui: '#F4F4F1', surface: '#FFFFFF', text: '#1A1A1A', muted: '#8A8A85', strong: '#1A1A1A', line: '#C9C9C4', highlight: '#E4E4DF', brand: '#1A1A1A', board: '#FAFAF8', dots: '#DADAD5', type: '#1A1A1A', arrow: '#1A1A1A', label: '#1A1A1A', guide: '#8A8A85', hull1: '#D9D9D4', hull2: '#C6C6C0', hull3: '#E5E5E0', hull4: '#B6B6B0', hullLabel: '#6B6B66', pass: '#1A1A1A', fail: '#C8321E', indet: '#A8A8A3', select: '#1A1A1A', agent: '#FFFFFF', agentInk: '#1A1A1A', bubble: '#FFFFFF', bubbleInk: '#1A1A1A' }),
    S('console', 'Console', 'Mostly white with light warm greys, dark line work, and four accents that each have one job: blue types, green for passing, red for failing and the tab, a yellow agent. After the Super Famicom\'s buttons and the declarative dragging page.',
      { ui: '#F4F3EF', surface: '#FFFFFF', text: '#1D1D1F', muted: '#8F8B84', strong: '#1D1D1F', line: '#D6D2C6', highlight: '#E7E4DC', brand: '#DC3400', board: '#FFFFFF', dots: '#E4E1D9', type: '#2563EB', arrow: '#3A3A3D', label: '#1D1D1F', guide: '#A6A49E', hull1: '#7FA4F5', hull2: '#F0C24D', hull3: '#F08A66', hull4: '#6CC98D', hullLabel: '#6B6B70', pass: '#16A34A', fail: '#DC3400', indet: '#E2A400', select: '#1D1D1F', agent: '#E2A400', agentInk: '#715200', bubble: '#FFFFFF', bubbleInk: '#1D1D1F' }),
    S('graphite', 'Graphite', 'What the build uses in dark mode. Monochrome: light grey on near-black, red only for a failing test.',
      GRAPHITE),
    S('graphite-red-types', 'Graphite, red types', 'Graphite with the types in the red accent. Failing tests are the same red, as asked; judge whether that clashes.',
      { ...GRAPHITE, type: '#FF5A4A' }),
    S('graphite-red-names', 'Graphite, red names', 'Graphite with the function names in red and the types left grey.',
      { ...GRAPHITE, label: '#FF5A4A' }),
    S('graphite-red-tab', 'Graphite, red tab', 'Graphite with only the portolan tab and the headings in red.',
      { ...GRAPHITE, brand: '#FF5A4A', strong: '#FF7A6C' }),
    S('graphite-red', 'Graphite, red throughout', 'Red on the tab, headings, types, selection and Trine. Failing tests move to amber so they still stand apart.',
      { ...GRAPHITE, brand: '#FF5A4A', strong: '#FF7A6C', type: '#FF5A4A', select: '#FF5A4A', agent: '#FF5A4A', agentInk: '#181818', fail: '#FFC24B' }),
    S('graphite-amber', 'Graphite, amber', 'The same accenting in amber, which leaves red free for failures.',
      { ...GRAPHITE, brand: '#FFC24B', strong: '#FFD27A', type: '#FFC24B', select: '#FFC24B', agent: '#FFC24B', agentInk: '#181818' }),
    S('graphite-ice', 'Graphite, ice', 'The same accenting in a cold blue.',
      { ...GRAPHITE, brand: '#7FD6E8', strong: '#A8E4F0', type: '#7FD6E8', select: '#7FD6E8', agent: '#7FD6E8', agentInk: '#181818' }),
    S('sepia', 'Sepia', 'Hazel\'s paper with one brown ink. The closest monochrome to the original; Trine keeps his yellow.',
      { ui: '#F3EBD9', surface: '#FBF7EC', text: '#4A3A20', muted: '#A08A5A', strong: '#4A3A20', line: '#C9B88F', highlight: '#EADBB0', brand: '#4A3A20', board: '#F6EFE0', dots: '#DCCFB4', type: '#4A3A20', arrow: '#4A3A20', label: '#4A3A20', guide: '#8A744A', hull1: '#EADFC6', hull2: '#DFD0AE', hull3: '#F0E7D2', hull4: '#D2C097', hullLabel: '#8A744A', pass: '#4A3A20', fail: '#B0433A', indet: '#B09A6A', select: '#4A3A20', agent: '#F3C94B', agentInk: '#4A3A20', bubble: '#FBEEB8', bubbleInk: '#3A2A12' }),
    S('blueprint', 'Blueprint', 'Monochrome on a blue ground: white line work, the way a drawing office would print it.',
      { ui: '#0C2340', surface: '#12325A', text: '#D6E4F8', muted: '#7FA3D1', strong: '#FFFFFF', line: '#2B5486', highlight: '#24528C', brand: '#EAF2FF', board: '#0F2A4A', dots: '#23466F', type: '#EAF2FF', arrow: '#BFD6F5', label: '#EAF2FF', guide: '#6F93C2', hull1: '#1B4273', hull2: '#24528C', hull3: '#163760', hull4: '#2E64A8', hullLabel: '#A9C4E8', pass: '#EAF2FF', fail: '#FF8A7A', indet: '#7FA3D1', select: '#FFFFFF', agent: '#FFD666', agentInk: '#EAF2FF', bubble: '#12325A', bubbleInk: '#EAF2FF' }),
    S('tidepool', 'Tidepool', 'Cool grey-blue paper, indigo types, teal functions, sea-glass regions, a coral agent.',
      { ui: '#E4EBEE', surface: '#F8FBFC', text: '#16242E', muted: '#617380', strong: '#0E6B75', line: '#B7C5CD', highlight: '#CFE6E3', brand: '#0E6B75', board: '#EDF2F4', dots: '#C2CDD4', type: '#3A4DB5', arrow: '#0E6B75', label: '#0E6B75', guide: '#617380', hull1: '#8CC9C1', hull2: '#A8B7EA', hull3: '#E9C88F', hull4: '#D9A6CF', hullLabel: '#46606C', pass: '#2E8B57', fail: '#C8324B', indet: '#C9A227', select: '#1F6FEB', agent: '#E4572E', agentInk: '#16242E', bubble: '#FDEDE6', bubbleInk: '#16242E' }),
    S('observatory', 'Observatory', 'Blue-black ground, regions as dim nebulae, bright line work, an amber agent.',
      { ui: '#131825', surface: '#1A2030', text: '#D5DCE8', muted: '#8A96AB', strong: '#FFC24B', line: '#2C3548', highlight: '#2A3550', brand: '#FFC24B', board: '#0E121B', dots: '#283044', type: '#BBA8FF', arrow: '#7FD6E8', label: '#7FD6E8', guide: '#8A96AB', hull1: '#2F7D9C', hull2: '#7355B0', hull3: '#9A6230', hull4: '#8E4B7C', hullLabel: '#AEB9CC', pass: '#6FD08C', fail: '#FF6B81', indet: '#E0C060', select: '#5EA0FF', agent: '#FFC24B', agentInk: '#FFE9B0', bubble: '#1B2233', bubbleInk: '#E7ECF5' }),
    S('vellum', 'Vellum', 'Green-grey drafting paper, graphite arrows, regions in highlighter colours, a red-pencil agent.',
      { ui: '#DDE3D6', surface: '#F3F6EF', text: '#20261F', muted: '#667062', strong: '#20261F', line: '#B1BBAA', highlight: '#F0DF62', brand: '#20261F', board: '#E7EBE2', dots: '#BCC6B5', type: '#6A3FA0', arrow: '#20261F', label: '#1F5F3A', guide: '#667062', hull1: '#F0DF62', hull2: '#F2A3B6', hull3: '#98C9EA', hull4: '#B9D98C', hullLabel: '#4E5A4A', pass: '#2F7D4F', fail: '#B3261E', indet: '#B89B1E', select: '#2456C9', agent: '#D5382B', agentInk: '#20261F', bubble: '#FFF6C7', bubbleInk: '#20261F' }),
    S('camel', 'Camel', 'Bone white with charcoal line work; the OCaml orange is kept for names and the agent.',
      { ui: '#EFEDE8', surface: '#FFFFFF', text: '#1D1B19', muted: '#6C6861', strong: '#AD4C0C', line: '#CECAC2', highlight: '#FBE3CF', brand: '#EC6813', board: '#F6F5F2', dots: '#D2CFC8', type: '#5B4BB5', arrow: '#1D1B19', label: '#AD4C0C', guide: '#6C6861', hull1: '#F2B47E', hull2: '#B8B1D6', hull3: '#B6C7A2', hull4: '#E7C6D0', hullLabel: '#6C6861', pass: '#3C8A4E', fail: '#C22F3D', indet: '#C9A227', select: '#EC6813', agent: '#EC6813', agentInk: '#1D1B19', bubble: '#FFF1E3', bubbleInk: '#1D1B19' }),
  ];

  // ---- colours -> the page's and the canvas's variables ----
  const mix = (a, b, t) => { const p = h => [1, 3, 5].map(i => parseInt(h.slice(i, i + 2), 16)); const x = p(a), y = p(b); return '#' + x.map((v, i) => Math.round(v + (y[i] - v) * t).toString(16).padStart(2, '0')).join(''); };
  const lum = h => { const n = parseInt(h.slice(1), 16); return (0.299 * (n >> 16) + 0.587 * ((n >> 8) & 255) + 0.114 * (n & 255)) / 255; };
  // The ink that reads best on a fill, out of the scheme's own colours.
  const on = (bg, inks) => inks.reduce((a, b) => (Math.abs(lum(b) - lum(bg)) > Math.abs(lum(a) - lum(bg)) ? b : a));
  function vars(c) {
    c = derive(c);
    const soft = mix(c.ui, c.line, 0.5), strongLine = mix(c.line, c.text, 0.4);
    return {
      // the page
      '--ui-bkg': c.ui, '--df-bg': c.surface, '--surface-tab': c.tab, '--top-bkg': c.topbar, '--head-bkg': c.header, '--info-bkg': c.code, '--side-bkg': c.chat, '--bot-bkg': c.status,
      '--logo-bkg': c.logoBg, '--logo-ink': c.logo, '--tab-ink': c.tabInk, '--title-ink': c.title, '--tool-bkg': c.tool, '--tool-on': c.toolOn, '--say-bkg': c.say, '--composer-bkg': c.composer, '--send-bkg': c.send, '--run-bkg': c.run, '--agent-cell': c.agentCell, '--chip-bkg': c.chip, '--surface-highlight': c.highlight, '--surface-highlight-soft': mix(c.ui, c.highlight, 0.5),
      '--border-default': c.line, '--border-soft': soft, '--border-strong': strongLine,
      '--text-default': c.text, '--text-muted': c.muted, '--text-strong': c.strong, '--token-typ': c.type, '--token-pat': c.label,
      '--brand': c.brand, '--brand-ink': c.brandInk, '--ok': c.pass, '--err': c.fail, '--agent': c.agent, '--board': c.board, '--dots': c.dots,
      // only the app reads these: text between quiet and heading, a slightly raised surface, the test list's pips
      '--text-accent': mix(c.muted, c.strong, 0.5), '--surface-raised': mix(c.ui, c.text, 0.05), '--pc-surface-raised': mix(c.ui, c.text, 0.05), '--pip-pass': c.pass, '--pip-fail': c.fail,
      // the canvas (its stylesheet's own variables)
      '--pc-ui-bkg': c.ui, '--pc-surface-shard': c.dots, '--pc-tan-1': c.dots,
      '--pc-df-bg': c.surface, '--pc-paper-2': c.surface, '--pc-cell-active': c.surface,
      '--pc-border-default': c.line, '--pc-border-soft': soft, '--pc-border-strong': strongLine,
      '--pc-surface-highlight': c.highlight, '--pc-surface-highlight-soft': mix(c.ui, c.highlight, 0.5), '--pc-text-strong': c.strong, '--pc-text-default': c.text,
      '--pc-token-typ': c.type, '--pc-purple-1': c.type, '--pc-purple-2': c.type,
      '--pc-token-exp': c.arrow, '--pc-grey-7': c.arrow,
      '--pc-token-pat': c.label, '--pc-blue-2': c.label,
      '--pc-border-black': c.guide, '--pc-grey-8': c.guide, '--pc-CREASE': mix(c.surface, c.guide, 0.55),
      '--pc-hull-1': c.hull1, '--pc-hull-2': c.hull2, '--pc-hull-3': c.hull3, '--pc-hull-4': c.hull4, '--pc-hull-5': c.hull1, '--pc-hull-6': c.hull2,
      '--pc-text-accent': c.hullLabel, '--pc-tan-4': c.hullLabel,
      '--pc-green-5': c.pass, '--pc-red-4': c.fail, '--pc-gold-8': c.indet, '--pc-ci-status-error-text': c.fail, '--pc-red-7': c.fail,
      '--pc-green-7': c.select, '--pc-green-4': c.select, '--pc-green-2': mix(c.surface, c.select, 0.18),
      '--pc-gold-7': c.agent, '--pc-gold-6': c.agent, '--pc-gold-3': mix(c.agent, c.surface, 0.45), '--pc-gold-4': c.agent,
      '--pc-ink-1': c.agentInk, '--pc-ink-4': c.agentInk, '--pc-grey-9': c.agentInk, '--pc-gold-9': c.agentInk,
      '--pc-gold-5': c.bubble, '--pc-gold-1': c.bubble, '--pc-paper-8': c.bubble,
      '--pc-ink-6': c.bubbleInk, '--pc-ink-2': c.bubbleInk, '--pc-ink-7-a20': mix(c.bubble, c.bubbleInk, 0.2),
      '--pc-white': on(c.type, [c.surface, c.ui, c.text]), // the () drawn on a filled tuple node
    };
  }

  // ---- the candidates above, plus what the viewer has changed ----
  const KEY = 'portolan.schemes'; // id -> { colors, note, star, name, from }
  const load = () => { try { return JSON.parse(localStorage.getItem(KEY) || '{}') || {}; } catch (e) { return {}; } };
  const base = id => SCHEMES.find(s => s.id === id);
  // The viewer's own schemes (those with a `from`) come after the candidates.
  const all = saved => [...SCHEMES, ...Object.keys(saved).filter(id => saved[id] && saved[id].from).sort().map(id => ({ id, name: saved[id].name || 'Untitled', blurb: 'Your own, started from ' + (base(saved[id].from) || {}).name + '.', c: (base(saved[id].from) || SCHEMES[0]).c, custom: true }))];
  const scheme = (id, saved) => all(saved).find(s => s.id === id) || SCHEMES[0];
  const colours = (id, saved) => derive({ ...scheme(id, saved).c, ...((saved[id] || {}).colors || {}) });
  const paint = (el, c) => { const v = vars(c); for (const k in v) el.style.setProperty(k, v[k]); };

  // ---- for the page: one scheme for light, one for dark ----
  // The stylesheet already draws these two, so choosing one sets nothing unless it was edited.
  const FIRST = { light: 'hazel', dark: 'graphite' };
  const modeOf = c => (lum(c.ui) < 0.5 ? 'dark' : 'light');
  const NAMES = Object.keys(vars(SCHEMES[0].c));
  // The schemes a mode's menu offers: its first, then the others of that lightness, and the one in use whatever its lightness.
  const list = (mode, saved, used) => [base(FIRST[mode]), ...all(saved).filter(s => s.id !== FIRST[mode] && (s.id === used || modeOf(colours(s.id, saved)) === mode))];
  // Sets a mode's scheme on an element (the root), and says which it was: `id` if it still exists, else the first.
  function apply(el, mode, id, saved) {
    saved = saved || load();
    const s = all(saved).find(x => x.id === id) || base(FIRST[mode]), v = vars(colours(s.id, saved));
    const own = s.id === FIRST[mode] ? vars(s.c) : {};
    for (const k of NAMES) {
      if (String(v[k]).toLowerCase() === String(own[k]).toLowerCase()) el.style.removeProperty(k); else el.style.setProperty(k, v[k]);
    }
    return s.id;
  }

  return { KEY, ROLES, SCHEMES, FIRST, mix, on, derive, vars, paint, load, all, scheme, colours, list, apply };
})();
