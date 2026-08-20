# AGENTS.md

Single source of truth for agents working on **table-maintenance**.

Table maintenance in the browser - edit data and customizing entries with value help, fixed values and transport handling.

## What this repository is

A **source** repository in the abap2UI5 ecosystem: humans and agents edit here,
and CI gates every change. It is installed with abapGit and depends on the
[abap2UI5](https://github.com/abap2UI5/abap2UI5) core at runtime.

## Layout

| Path | Contents |
| --- | --- |
| `src/` | The maintenance app (`z2ui5_cl_tm_001`), its popup (`z2ui5_cl_tm_pop`) and the interface they share (`z2ui5_if_tm_001`) |

`z2ui5_if_tm_001~mo_parent_view` is the embedding contract: a view cluster hands
over **the element this app renders into**, normally the `sap.m.Page`. It used
to be the whole view, which the app then searched for a `Page` by name; the
generic view builder has no such lookup.

## Dependencies

- [abap2UI5](https://github.com/abap2UI5/abap2UI5) — the framework
- [layout-management](https://github.com/abap2UI5-addons/layout-management) — variant-managed table and form layouts
- [popups](https://github.com/abap2UI5-addons/popups) — value help and search help

There is no version pin on any of them — abaplint resolves each from its `main`
branch. That is why the check workflows also run on a weekly schedule: a rename
upstream breaks this repository silently, and with no pull request open nothing
else would notice.

## Build and verify

```sh
npm ci
npm run check
```

`npm run check` runs exactly what CI runs. See [CONTRIBUTING.md](CONTRIBUTING.md)
for what each gate proves and how the abap2UI5-linter baseline works.

## Views

Views are built with **`z2ui5_cl_ui5_view_builder`**, the core's generic builder
in `src/02`: `ele` (add a child and descend), `tag` (add a child and stay), `a`
(set an attribute on the element the chain points at), `end` (ascend),
`stringify` (render from the root).

Three rules that are easy to get wrong:

1. **`a( )` targets the last child** once a node has children, so a control's
   attributes belong immediately after the `ele( )`/`tag( )` that opened it.
2. **An ABAP boolean goes into `b =`.** Through `v =` an `abap_false` renders as
   an empty string, and UI5 reads an empty string as true.
3. **The root declares its own namespaces.** The frozen builder collected them
   and wrote the `xmlns` attributes itself; this one does not, and a prefix that
   is used but not declared makes the whole view fail to parse — a blank screen,
   not a bad layout. An aggregation tag takes the namespace of *its own control*:
   under `<form:SimpleForm>` it is `<form:content>`, never `<content>`.

The frozen `z2ui5_cl_xml_view` is gone from here and must not come back. It sits
in the core's `src/99`, outside the released API, and the view gate can read
nothing built with it.

## Conventions

- ABAP object names follow ``z2ui5_cl_tm_*``, enforced by `abaplint.jsonc`.
- English for code, comments, commit messages, pull requests and issues.
- All text files are LF-only (`.gitattributes`).
- The ecosystem-wide rules — workflow and npm-script naming, toolchain versions,
  which documentation files exist, commit style — live in
  [CONVENTIONS.md](https://github.com/abap2UI5/abap2UI5/blob/main/.github/shared/CONVENTIONS.md)
  and bind this repository too.
