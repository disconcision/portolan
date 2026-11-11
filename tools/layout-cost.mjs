#!/usr/bin/env node
// How often and how long the canvas lays a graph out: for each module named, the layouts
// computed and their time (a) when the module is picked and (b) for one paced change to it
// (its last function removed, then put back).
//
//   node tools/layout-cost.mjs --url http://127.0.0.1:8791/ [--layout ledger] [--edges metro] [--pick-only 1] Watcher Graph ''
//
// '' is the whole project. Needs puppeteer-core and Chrome, as tools/shot.mjs does.
import { createRequire } from 'node:module';
import { existsSync } from 'node:fs';

const opts = {}, names = [];
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (a.startsWith('--')) opts[a.slice(2)] = process.argv[++i]; else names.push(a);
}
const require = createRequire(import.meta.url);
const puppeteer = require(process.env.PUPPETEER_CORE || 'puppeteer-core');
const chrome = process.env.CHROME || [
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  '/usr/bin/google-chrome', '/usr/bin/chromium', '/usr/bin/chromium-browser',
].find(existsSync);
const browser = await puppeteer.launch({ executablePath: chrome, headless: 'new', protocolTimeout: 1200000 });
try {
  const page = await browser.newPage();
  await page.setViewport({ width: 2000, height: 1200 });
  await page.goto(opts.url || 'http://127.0.0.1:8791/', { waitUntil: 'domcontentloaded', timeout: 60000 });
  await new Promise(r => setTimeout(r, 3000));
  for (const name of names) {
    const row = await page.evaluate(async (name, opts) => {
      const PC = window.PortolanCanvas, sleep = ms => new Promise(r => setTimeout(r, ms));
      if (opts.layout && PC.setLayout) PC.setLayout(opts.layout);
      if (opts.edges && PC.setEdges) PC.setEdges(opts.edges);
      let shown = null;
      const show = PC.show;
      PC.show = g => { shown = g; show(g); };
      // Wait until no layout has run for a second (or two minutes have passed).
      const quiet = async () => {
        let last = -1, same = 0;
        for (let i = 0; i < 480 && same < 4; i++) {
          await sleep(250);
          const c = PC.layoutStats().calls;
          same = c === last ? same + 1 : 0;
          last = c;
        }
      };
      const measure = async act => {
        const a = PC.layoutStats(), t0 = performance.now();
        act();
        await sleep(0);
        const blocked = Math.round(performance.now() - t0);
        await quiet();
        const b = PC.layoutStats();
        return { layouts: b.calls - a.calls, ms: Math.round(b.ms - a.ms), each: Math.round((b.ms - a.ms) / Math.max(1, b.calls - a.calls)), first_block_ms: blocked };
      };
      const sel = document.querySelector('#module');
      const pick = await measure(() => { sel.value = name; sel.dispatchEvent(new Event('change', { bubbles: true })); });
      PC.show = show;
      const out = { module: name || '(whole project)', drawn: document.querySelectorAll('.canvas-node').length, pick };
      if (!opts['pick-only'] && shown && shown.edges.length > 1) {
        const less = { ...shown, edges: shown.edges.slice(0, -1) };
        const removed = await measure(() => PC.push(less, { label: 'remove' }));
        const added = await measure(() => PC.push(shown, { label: 'add' }));
        out.change = { layouts: removed.layouts + added.layouts, ms: removed.ms + added.ms };
      }
      return out;
    }, name, opts);
    console.log(JSON.stringify(row));
  }
} finally {
  await browser.close();
}
