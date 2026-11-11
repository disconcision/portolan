#!/usr/bin/env node
// Parses every inline script of every page under site/ and web/, so a stray quote in a page's
// data cannot ship a page that renders empty. Run before pushing: node tools/check-site.mjs
import { readdirSync, readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';

const pages = dir => readdirSync(dir).flatMap(n => {
  const p = join(dir, n);
  return statSync(p).isDirectory() ? (n === 'lib' || n === 'vendor' ? [] : pages(p)) : p.endsWith('.html') ? [p] : [];
});
let bad = 0, n = 0;
for (const p of [...pages('site'), ...pages('web')]) {
  const scripts = readFileSync(p, 'utf8').match(/<script(?![^>]*\bsrc=)[^>]*>[\s\S]*?<\/script>/g) || [];
  for (const s of scripts) {
    n++;
    try { new Function(s.replace(/^<script[^>]*>|<\/script>$/g, '')); }
    catch (e) { bad++; console.error(`${p}: ${e.message}`); }
  }
}
console.log(bad ? `${bad} of ${n} inline scripts do not parse` : `${n} inline scripts parse`);
process.exit(bad ? 1 : 0);
