#!/usr/bin/env node
// Writes ../../web/vendor/codemirror.js. Run `npm install && npm run build` in this folder
// after changing a version in package.json; the page itself needs neither npm nor a network.
import { build } from 'esbuild';
import { readFileSync } from 'node:fs';
import { fileURLToPath } from 'node:url';

const here = p => fileURLToPath(new URL(p, import.meta.url));
const deps = JSON.parse(readFileSync(here('package.json'), 'utf8')).devDependencies;
const versions = Object.keys(deps).filter(d => d.startsWith('@codemirror/')).map(d => d + ' ' + deps[d]).join(', ');

await build({
  entryPoints: [here('entry.js')],
  outfile: here('../../web/vendor/codemirror.js'),
  bundle: true,
  minify: true,
  format: 'iife',
  globalName: 'CM',
  target: 'es2020',
  legalComments: 'none',
  banner: {
    js: `/* CodeMirror 6 (${versions}) and what they depend on, bundled by tools/codemirror/build.mjs.\n` +
      '   Copyright (C) 2018 by Marijn Haverbeke and others. MIT licence: https://github.com/codemirror/dev/blob/main/LICENSE */',
  },
});
