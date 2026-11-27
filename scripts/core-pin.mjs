#!/usr/bin/env node
/*
 * core-pin — which abap2UI5 this repository is checked against.
 *
 * abaplint resolves the core as a git dependency, and without a "branch" key
 * it clones the DEFAULT branch - so this addon used to be linted against the
 * framework's development tip while every user of it installs a RELEASE.
 * CONVENTIONS §9 of abap2UI5 names that gap: code that only works on main
 * passes here, and a framework change reddens this repository without a
 * commit of its own.
 *
 * So every abaplint config names a release tag, and this script is the one
 * place that reads or moves them:
 *
 *   node scripts/core-pin.mjs get           print the pinned ref (fails when the configs disagree)
 *   node scripts/core-pin.mjs set <ref>     pin a tag (or `main` for the canary)
 *   node scripts/core-pin.mjs latest        print the newest release tag
 *
 * The 7.02 configs carry the downported tag of the same release (`1.146.0`
 * there is `1.146.0-702`, `main` there is `702`), so one repository stays on
 * one framework version. `set main` is what a weekly canary run does in its
 * own checkout - it is never committed. bump-core moves the pin to `latest`
 * after the gates passed on it.
 */
import { readFileSync, writeFileSync } from 'node:fs';
import { execFileSync } from 'node:child_process';

const CORE = 'https://github.com/abap2UI5/abap2UI5';
// every file that resolves the core, and whether it takes the downported
// (-702) form of the tag. An entry may name its own anchor and pin syntax
// (popups' list has its unit runner). The namespace rename has none of its
// own: .github/scripts/rename.mjs clones the core at the pin of
// abaplint.jsonc
const FILES = [
  { file: 'abaplint.jsonc', downport: false },
  { file: '.github/abaplint/abap_cloud.jsonc', downport: false },
  { file: '.github/abaplint/abap_702.jsonc', downport: true },
];
// the one line the pin lives on - kept on its own line next to the core URL
const PIN = /("branch":\s*")([^"]+)(")/;

const to702 = (ref) => (ref === 'main' ? '702' : `${ref}-702`);
const from702 = (ref) => (ref === '702' ? 'main' : ref.replace(/-702$/, ''));

function read(entry) {
  const file = new URL(`../${entry.file}`, import.meta.url);
  const text = readFileSync(file, 'utf8');
  const at = text.indexOf(entry.anchor ?? `"url": "${CORE}"`);
  if (at < 0) throw new Error(`${entry.file}: no dependency on ${CORE}`);
  const block = text.slice(at, text.indexOf('}', at));
  const m = block.match(entry.pin ?? PIN);
  if (!m) throw new Error(`${entry.file}: the core dependency has no pin`);
  const ref = entry.downport ? from702(m[2]) : m[2];
  return { file, text, at, block, ref };
}

const [cmd, arg] = process.argv.slice(2);

if (cmd === 'get') {
  const refs = FILES.map((entry) => ({ entry, ref: read(entry).ref }));
  const distinct = [...new Set(refs.map((r) => r.ref))];
  if (distinct.length > 1) {
    throw new Error('the abaplint configs pin different releases:\n' +
      refs.map((r) => `  ${r.entry.file}: ${r.ref}`).join('\n'));
  }
  console.log(distinct[0]);
} else if (cmd === 'set') {
  if (!arg) throw new Error('usage: core-pin.mjs set <ref>');
  for (const entry of FILES) {
    const { file, text, at, block } = read(entry);
    const ref = entry.downport ? to702(arg) : arg;
    const next = block.replace(entry.pin ?? PIN, `$1${ref}$3`);
    writeFileSync(file, text.slice(0, at) + next + text.slice(at + block.length));
    console.log(`${entry.file}: core pinned to ${ref}`);
  }
} else if (cmd === 'latest') {
  // plain release tags only - `1.146.0`, not the downported `1.146.0-702`
  const out = execFileSync('git', ['ls-remote', '--tags', '--refs', CORE], { encoding: 'utf8' });
  const tags = out.split('\n')
    .map((l) => l.split('refs/tags/')[1])
    .filter((t) => t && /^\d+\.\d+\.\d+$/.test(t))
    .sort((a, b) => {
      const pa = a.split('.').map(Number);
      const pb = b.split('.').map(Number);
      return pa[0] - pb[0] || pa[1] - pb[1] || pa[2] - pb[2];
    });
  if (!tags.length) throw new Error('no release tag found');
  console.log(tags[tags.length - 1]);
} else {
  console.error('usage: core-pin.mjs get | set <ref> | latest');
  process.exit(1);
}
