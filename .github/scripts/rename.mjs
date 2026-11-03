#!/usr/bin/env node
/*
 * rename.mjs - the namespace rename of an abap2UI5 addon, the same mechanism
 * abap2UI5 itself has (build-rename workflow, .github/abaplint/rename.jsonc):
 * `abaplint --rename` turns every z2ui5_ object into <prefix>..., where the
 * prefix is a customer name (ZMYUI5 -> zmyui5_) or a registered namespace
 * (/CA2UI5/ -> /ca2ui5/), at most 10 characters either way.
 *
 * What an addon adds to the core's mechanism: the result has to run on a
 * RENAMED abap2UI5 (and on the renamed addons it builds on), so every
 * reference to them is renamed with the same prefix. abaplint renames only
 * the objects of its own files - dependencies stay as they are - so this
 * script clones the abap2UI5 repositories the addon depends on (the
 * abap2UI5 / abap2UI5-addons entries of abaplint.jsonc, and theirs in turn)
 * into .github/abaplint/rename-deps, renames them together with src/, and
 * keeps only src/. Rename abap2UI5 and every addon of an installation with
 * the same name.
 *
 * After the rename it checks what abaplint does not:
 *   - the length of every renamed object (CLAS/INTF/PROG/DTEL/DOMA/TTYP 30,
 *     a transparent table 16, a message class 20, an ICF node 15)
 *   - string literals: --rename rewrites references, never a literal, so a
 *     literal naming a renamed object would look up the ORIGINAL one in the
 *     renamed installation (production code only, test classes excluded)
 *
 *   node .github/scripts/rename.mjs                 the check (npm run rename):
 *        the worst-case prefix the addon allows (package.json
 *        config.renameMaxLength, default 10 - the core's limit)
 *   node .github/scripts/rename.mjs --name ZMYUI5 --out <dir>
 *        the build-rename workflow: the renamed src/ plus .abapgit.xml and
 *        LICENSE in <dir>
 *   RENAME_DEPS_FROM=<dir> npm run rename
 *        a dependency <dir>/<repository> holds is copied from there instead
 *        of cloned - to check a change that spans several repositories
 *
 * The same file in every addon that has a rename - change it everywhere.
 */
import { execFileSync, spawnSync } from "node:child_process";
import { cpSync, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, statSync, writeFileSync } from "node:fs";
import { basename, dirname, join, relative, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = resolve(dirname(fileURLToPath(import.meta.url)), "../..");
const BASE = join(ROOT, ".github/abaplint");
const DEPS = join(BASE, "rename-deps");
const OUTPUT = join(BASE, "output");
const RUN_CONFIG = join(BASE, "rename_run.json");

const args = process.argv.slice(2);
const arg = (name) => {
  const i = args.indexOf(name);
  return i >= 0 ? args[i + 1] : undefined;
};
const fail = (msg) => {
  console.error(`rename: ${msg}`);
  process.exit(1);
};

// ---------------------------------------------------------------- the prefix
const pkg = JSON.parse(readFileSync(join(ROOT, "package.json"), "utf8"));
const maxLength = Number(pkg.config?.renameMaxLength ?? 10);
let prefix;
const name = arg("--name");
if (name === undefined) {
  // the worst case: a customer name of maxLength characters
  prefix = "z".padEnd(maxLength, "x") + "_";
} else {
  const ns = name.toLowerCase();
  if (/^[a-z][a-z0-9_]*$/.test(ns)) {
    if (ns === "z2ui5") fail(`'${name}' is the original name - nothing to rename`);
    prefix = `${ns}_`;
  } else if (/^\/[a-z0-9][a-z0-9_]*\/$/.test(ns)) {
    prefix = ns;
  } else {
    fail(`invalid name '${name}' - a customer name (letter + letters/digits/underscore, e.g. ZMYUI5) or a namespace with both slashes (e.g. /CA2UI5/)`);
  }
  const length = prefix.endsWith("_") ? prefix.length - 1 : prefix.length;
  if (length > maxLength) {
    fail(`'${name}' has ${length} characters, this addon allows at most ${maxLength} (package.json config.renameMaxLength)`);
  }
}

// ------------------------------------------------------- config and sources
// abaplint.jsonc is JSON with comments and trailing commas
function readJsonc(file) {
  const text = readFileSync(file, "utf8");
  let out = "";
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (c === '"') {
      let j = i + 1;
      while (j < text.length && text[j] !== '"') j += text[j] === "\\" ? 2 : 1;
      out += text.slice(i, j + 1);
      i = j;
    } else if (c === "/" && text[i + 1] === "/") {
      while (i < text.length && text[i] !== "\n") i++;
      out += "\n";
    } else if (c === "/" && text[i + 1] === "*") {
      i = text.indexOf("*/", i + 2) + 1;
    } else {
      out += c;
    }
  }
  return JSON.parse(out.replace(/,(\s*[}\]])/g, "$1"));
}

function lintConfig(dir) {
  for (const f of ["abaplint.jsonc", "abaplint.json"]) {
    if (existsSync(join(dir, f))) return readJsonc(join(dir, f));
  }
  return { dependencies: [] };
}

const isAbap2UI5 = (url) => /github\.com\/abap2ui5(-addons)?\//i.test(url ?? "");
const repoKey = (url) => url.toLowerCase().replace(/\.git$/, "").replace(/\/$/, "");
const ownUrl = (() => {
  try {
    return repoKey(execFileSync("git", ["remote", "get-url", "origin"], { cwd: ROOT, encoding: "utf8" }).trim()
      .replace(/^.*github\.com[/:]/, "https://github.com/"));
  } catch {
    return "";
  }
})();

// clone the abap2UI5 sources, transitively; everything else stays a plain
// abaplint dependency
rmSync(DEPS, { recursive: true, force: true });
mkdirSync(DEPS, { recursive: true });
const own = lintConfig(ROOT);
const cloned = new Map(); // key -> folder name
const others = new Map(); // key -> dependency entry
const queue = (own.dependencies ?? []).map((d) => ({ ...d, level: 0 }));
while (queue.length > 0) {
  const d = queue.shift();
  if (!d.url) continue;
  const key = repoKey(d.url);
  // some configs list the repository itself - it is src/ already
  if (key === ownUrl || (ownUrl === "" && key.split("/").at(-1) === basename(ROOT).toLowerCase())) continue;
  if (!isAbap2UI5(d.url)) {
    if (!others.has(key)) others.set(key, { url: d.url, ...(d.branch ? { branch: d.branch } : {}), files: d.files, level: d.level });
    continue;
  }
  if (cloned.has(key)) continue;
  const folder = key.split("/").at(-1);
  const target = join(DEPS, folder);
  const local = process.env.RENAME_DEPS_FROM
    ? readdirSync(process.env.RENAME_DEPS_FROM).find((n) => n.toLowerCase() === folder)
    : undefined;
  if (local !== undefined && existsSync(join(process.env.RENAME_DEPS_FROM, local, "src"))) {
    console.log(`rename: copy ${join(process.env.RENAME_DEPS_FROM, local)}`);
    mkdirSync(target, { recursive: true });
    cpSync(join(process.env.RENAME_DEPS_FROM, local, "src"), join(target, "src"), { recursive: true });
    for (const f of ["abaplint.jsonc", "abaplint.json"]) {
      if (existsSync(join(process.env.RENAME_DEPS_FROM, local, f))) cpSync(join(process.env.RENAME_DEPS_FROM, local, f), join(target, f));
    }
    cloned.set(key, folder);
    queue.push(...(lintConfig(target).dependencies ?? []).map((x) => ({ ...x, level: d.level + 1 })));
    continue;
  }
  const cloneArgs = ["-c", "advice.detachedHead=false", "clone", "--quiet", "--depth", "1", ...(d.branch ? ["--branch", d.branch] : []), d.url, target];
  console.log(`rename: clone ${d.url}${d.branch ? ` (${d.branch})` : ""}`);
  execFileSync("git", cloneArgs, { stdio: "inherit" });
  cloned.set(key, folder);
  queue.push(...(lintConfig(target).dependencies ?? []).map((x) => ({ ...x, level: d.level + 1 })));
}

// ------------------------------------------------------------ the abaplint run
const TYPES = "CLAS|INTF|TABL|DTEL|DOMA|TTYP|PROG|MSAG|SICF";
const walk = (dir) => readdirSync(dir).flatMap((n) => {
  const p = join(dir, n);
  return statSync(p).isDirectory() ? walk(p) : [p];
});
// an object two sources define (abap2UI5-ext took classes over from the core
// releases before it) is kept from the first - src/, then the clones in
// order - and deleted from the later clones: abaplint renames nothing while
// any issue is open, a duplicate definition included
const seen = new Set();
for (const dir of [join(ROOT, "src"), ...[...cloned.values()].map((f) => join(DEPS, f, "src"))].filter(existsSync)) {
  const mine = new Set();
  for (const f of walk(dir)) {
    const key = basename(f).split(".").slice(0, 2).join(".").toLowerCase();
    if (key.startsWith("package.")) continue;
    if (seen.has(key)) rmSync(f);
    else mine.add(key);
  }
  mine.forEach((k) => seen.add(k));
}

const config = {
  global: {
    files: ["/../../src/**/*.*", ...[...cloned.values()].map((f) => `/rename-deps/${f}/src/**/*.*`)],
  },
  // the clones' own dependencies first: abaplint takes the first definition
  // of an object, and abap2UI5 compiles against the one it lints with
  dependencies: [...others.values()].sort((a, b) => b.level - a.level).map(({ level, ...d }) => d),
  rename: {
    output: "output",
    patterns: [{ type: TYPES, oldName: "^z2ui5_(.*)$", newName: `${prefix}$1` }],
  },
  // the addon's own syntax settings - its errorNamespace decides what an
  // unknown type is, as in its main lint
  syntax: own.syntax ?? { version: "v750", errorNamespace: "." },
  rules: {
    begin_end_names: true,
    check_ddic: true,
    check_include: true,
    check_syntax: true,
    global_class: true,
    implement_methods: true,
    method_implemented_twice: true,
    parser_error: true,
    superclass_final: true,
    unknown_types: true,
    xml_consistency: true,
  },
};
writeFileSync(RUN_CONFIG, JSON.stringify(config, null, 2) + "\n");

const bin = join(ROOT, "node_modules/.bin/abaplint");
console.log(`rename: z2ui5_ -> ${prefix}`);
const run = spawnSync(bin, [RUN_CONFIG, "--rename"], { cwd: ROOT, encoding: "utf8", maxBuffer: 1 << 28 });
const log = `${run.stdout ?? ""}${run.stderr ?? ""}`;
const issues = log.split("\n").filter((l) => /\[E\]|\[W\]/.test(l));
for (const l of issues) console.error(l);
if (log.includes("Error: Name not allowed")) {
  // abaplint stops at the first new name over its object type's limit
  const last = log.split("\n").filter((l) => l.startsWith("Renaming ")).at(-1) ?? "";
  fail(`a new name is too long for its object type: ${last.replace(/^Renaming /, "").replace(/\s+/g, " ")} - keep object names at 25 characters (30 minus the 5 a 10-character name adds)`);
}
if (issues.length > 0 || run.status !== 0) {
  if (issues.length === 0) console.error(log.split("\n").slice(-20).join("\n"));
  fail(`abaplint found ${issues.length} issue(s) - it renames nothing while one is open`);
}

// the renamed src/ - abaplint writes <output>/<absolute path of the file>
const srcOut = join(OUTPUT, join(ROOT, "src"));
if (!existsSync(srcOut)) fail(`no renamed src/ under ${relative(ROOT, OUTPUT)}`);

// ---------------------------------------------------------- what was renamed
const objectOf = (file) => {
  const m = basename(file).match(/^(.+?)\.([a-z0-9]{4})\.xml$/);
  if (!m) return undefined;
  return { name: m[1].replace(/#/g, "/").split(" ")[0], type: m[2].toUpperCase(), file };
};
const original = new Set();
for (const dir of [join(ROOT, "src"), ...[...cloned.values()].map((f) => join(DEPS, f, "src"))]) {
  if (!existsSync(dir)) continue;
  for (const f of walk(dir)) {
    const o = objectOf(f);
    if (o && TYPES.split("|").includes(o.type) && /^z2ui5_/i.test(o.name)) original.add(o.name.toUpperCase());
  }
}

const problems = [];
const LIMIT = { CLAS: 30, INTF: 30, PROG: 30, DTEL: 30, DOMA: 30, TTYP: 30, MSAG: 20, SICF: 15 };
for (const f of walk(srcOut)) {
  const o = objectOf(f);
  if (!o || !TYPES.split("|").includes(o.type)) continue;
  let limit = LIMIT[o.type];
  if (o.type === "TABL") limit = /<TABCLASS>TRANSP<\/TABCLASS>/.test(readFileSync(f, "utf8")) ? 16 : 30;
  if (o.name.length > limit) {
    problems.push(`${relative(srcOut, f)}: ${o.type} ${o.name} has ${o.name.length} characters, at most ${limit}`);
  }
  if (/^z2ui5_/i.test(o.name)) problems.push(`${relative(srcOut, f)}: ${o.type} ${o.name} was not renamed`);
}

// literals naming a renamed object by its original name - the pieces of a
// line that are string literals and the test whether one NAMES an object are
// abap2UI5's rename-literal-gate.mjs, unchanged
function literalsOf(line) {
  if (line.startsWith("*")) return [];
  const out = [];
  let i = 0;
  while (i < line.length) {
    const ch = line[i];
    if (ch === '"') break;
    if (ch === "`" || ch === "'") {
      let body = "";
      i++;
      while (i < line.length) {
        if (line[i] === ch && line[i + 1] === ch) {
          body += ch;
          i += 2;
        } else if (line[i] === ch) {
          break;
        } else {
          body += line[i++];
        }
      }
      out.push(body);
      i++;
      continue;
    }
    if (ch === "|") {
      let text = "";
      let depth = 0;
      i++;
      while (i < line.length) {
        const c = line[i];
        if (depth === 0 && c === "\\") {
          text += line[i + 1] ?? "";
          i += 2;
          continue;
        }
        if (depth === 0 && c === "|") break;
        if (c === "{") {
          if (depth === 0) {
            out.push(text);
            text = "";
          }
          depth++;
        } else if (c === "}") {
          depth--;
        } else if (depth === 0) {
          text += c;
        }
        i++;
      }
      out.push(text);
      i++;
      continue;
    }
    i++;
  }
  return out;
}
const NAMING = /^(?:\\(?:class|interface|type)=)?(z2ui5_[a-z0-9_]+)(?:(?:~|=>|->|-)\S*)?$/i;
for (const f of walk(srcOut)) {
  if (!f.endsWith(".abap") || f.endsWith(".testclasses.abap")) continue;
  readFileSync(f, "utf8").split("\n").forEach((line, n) => {
    for (const lit of literalsOf(line)) {
      const word = lit.match(NAMING)?.[1].toUpperCase();
      if (word && original.has(word)) {
        problems.push(`${relative(srcOut, f)}:${n + 1}: the literal '${lit}' names ${word} - the rename does not rewrite a literal, derive the name from a typed reference`);
      }
    }
  });
}

for (const p of problems) console.error(p);
if (problems.length > 0) {
  fail(`${problems.length} problem(s) in the renamed src/`);
}
console.log(`rename: z2ui5_ -> ${prefix} - ${walk(srcOut).length} file(s), 0 issues`);

// ------------------------------------------------------------ the branch tree
const out = arg("--out");
if (out !== undefined) {
  const dir = resolve(ROOT, out);
  rmSync(dir, { recursive: true, force: true });
  mkdirSync(dir, { recursive: true });
  cpSync(srcOut, join(dir, "src"), { recursive: true });
  for (const f of ["LICENSE", ".abapgit.xml"]) {
    if (existsSync(join(ROOT, f))) cpSync(join(ROOT, f), join(dir, f));
  }
  console.log(`rename: branch content in ${relative(ROOT, dir)}`);
}
