#!/usr/bin/env node
// A headless screenshot of a page, for checking how something really draws.
//
//   node tools/shot.mjs --url http://127.0.0.1:8791/ --out local/shots/a.png
//     [--width 1400] [--height 900] [--scale 1] [--dark]
//     [--wait 3000]            ms to wait after load
//     [--eval 'await pick("Watcher")']   run in the page (async); its value is printed
//     [--after 1500]           ms to wait after --eval
//     [--clip '#canvas']       shoot only this element
//     [--hide '#chat']         hide these elements first (comma separated)
//
// Needs puppeteer-core and a Chrome: set PUPPETEER_CORE to the module's folder if it is
// not installed here, and CHROME to the browser binary if it is not in the usual place.
import { createRequire } from 'node:module';
import { existsSync, mkdirSync } from 'node:fs';
import { dirname, resolve } from 'node:path';

const args = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith('--')) continue;
  const next = process.argv[i + 1];
  args[a.slice(2)] = next === undefined || next.startsWith('--') ? true : (i++, next);
}
if (!args.url || !args.out) {
  console.error('usage: node tools/shot.mjs --url URL --out FILE.png [see the top of this file]');
  process.exit(2);
}

const require = createRequire(import.meta.url);
const puppeteer = require(process.env.PUPPETEER_CORE || 'puppeteer-core');
const chrome = process.env.CHROME || [
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  '/usr/bin/google-chrome', '/usr/bin/chromium', '/usr/bin/chromium-browser',
].find(existsSync);

const sleep = ms => new Promise(r => setTimeout(r, ms));
const browser = await puppeteer.launch({ executablePath: chrome, headless: 'new', args: ['--hide-scrollbars'] });
try {
  const page = await browser.newPage();
  await page.setViewport({
    width: +(args.width || 1400), height: +(args.height || 900), deviceScaleFactor: +(args.scale || 1),
  });
  if (args.dark) await page.emulateMediaFeatures([{ name: 'prefers-color-scheme', value: 'dark' }]);
  page.on('pageerror', e => console.error('page error:', e.message));
  await page.goto(args.url, { waitUntil: 'domcontentloaded', timeout: 60000 });
  await sleep(+(args.wait || 3000));
  // Pick a module by name in the top bar, then frame the whole graph.
  await page.evaluate(() => {
    window.pick = async name => {
      const sel = document.querySelector('#module, select');
      const o = [...sel.options].find(o => o.value === name || o.textContent.startsWith(name + ' '));
      if (!o) return 'no module ' + name;
      sel.value = o.value;
      sel.dispatchEvent(new Event('change', { bubbles: true }));
      await new Promise(r => setTimeout(r, 2500));
      window.PortolanCanvas.fit();
      await new Promise(r => setTimeout(r, 1200));
      return o.value;
    };
  });
  if (args.hide) await page.addStyleTag({ content: `${args.hide} { display: none !important; }` });
  if (args.eval) {
    const value = await page.evaluate(`(async () => { return (${args.eval}); })()`);
    if (value !== undefined) console.log(typeof value === 'string' ? value : JSON.stringify(value));
    await sleep(+(args.after || 1500));
  }
  const out = resolve(args.out);
  mkdirSync(dirname(out), { recursive: true });
  if (args.clip) {
    const el = await page.$(args.clip);
    if (!el) throw new Error('no element ' + args.clip);
    await el.screenshot({ path: out });
  } else {
    await page.screenshot({ path: out });
  }
  console.log(out);
} finally {
  await browser.close();
}
