# table-maintenance

[![abap2UI5-addons](https://img.shields.io/badge/abap2UI5--addons-app-1873b4)](https://github.com/abap2UI5-addons)
[![ABAP](https://img.shields.io/badge/ABAP-Cloud%20%7C%20Standard%20%E2%89%A5%207.50%20%7C%207.02-blue)](#installation)
[![abap2UI5](https://img.shields.io/badge/requires-abap2UI5-blue)](https://github.com/abap2UI5/abap2UI5)
[![layout-management](https://img.shields.io/badge/requires-layout--management-blue)](https://github.com/abap2UI5-addons/layout-management)
[![popups](https://img.shields.io/badge/requires-popups-blue)](https://github.com/abap2UI5-addons/popups)
[![License](https://img.shields.io/github/license/abap2UI5-addons/table-maintenance)](LICENSE)
<br>
[![CI](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/table-maintenance/ci.yaml?branch=main&label=CI)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/ci.yaml)
[![ABAP 7.02](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/table-maintenance/ci-702.yaml?branch=main&label=ABAP%207.02)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/ci-702.yaml)
[![check-abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Ftable-maintenance%2Fbadges%2Fcheck-abap2ui5.json)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/ci.yaml)
[![abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Ftable-maintenance%2Fbadges%2Fabap2ui5.json)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/ci.yaml)

**Table maintenance in your browser - no Eclipse or SAP GUI installation
needed.** One generic abap2UI5 app shows the entries of a database table and
lets you add, edit, copy and delete them, with value helps from check tables,
fixed values and search helps, and records the changes on a transport request.
For developers and admins who maintain customizing and Z tables, on ABAP
Cloud as well as on Standard ABAP.

> Part of [abap2UI5-addons](https://github.com/abap2UI5-addons) - addons and apps for [abap2UI5](https://github.com/abap2UI5/abap2UI5), installed with [abapGit](https://abapgit.org). Documentation: [abap2ui5-addons.github.io/table-maintenance](https://abap2ui5-addons.github.io/table-maintenance/).

<img width="700" alt="Table maintenance with the entries of a database table" src="https://github.com/user-attachments/assets/51a1d7e5-ca7e-4359-9e12-39b00b3c11bf">

## Why

Maintaining a table usually means SM30 with a generated maintenance view in
SAP GUI - and on ABAP Cloud there is no SAP GUI at all. table-maintenance
needs nothing generated per table: it reads the table's structure at runtime
and builds the maintenance screen from it, in the browser.

Good for:

- **Customizing and Z tables** that need a quick maintenance screen.
- **ABAP Cloud systems** without SM30.
- **Several tables on one page** - the view cluster `z2ui5_cl_tm_002` shows a
  tab per table and the maintenance of the selected one below it, with one
  transport request for all of them.
- **Embedding** - an app of your own can render the maintenance into its page
  through the interface `z2ui5_if_tm_001`, the way `z2ui5_cl_tm_002` does.

To only look into a table, without changing it, use
[abap2UI5-addons/se16n](https://github.com/abap2UI5-addons/se16n).

## Installation

**Requirements**

- ABAP Cloud (S/4 Public Cloud, BTP ABAP Environment, S/4 Private Cloud or
  On-Premise with ABAP for Cloud) or Standard ABAP on SAP NetWeaver AS ABAP
  7.50 or higher
- NetWeaver 7.02 - 7.40: the branch `702`, downported automatically from
  `main` on every push - pull abap2UI5, layout-management and popups from their `702` branches as well
- [abap2UI5](https://github.com/abap2UI5/abap2UI5)
- [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management) -
  the column layouts of the table; popups needs it too
- [abap2UI5-addons/popups](https://github.com/abap2UI5-addons/popups) - value
  help, search help and the transport request popup

**Steps** - with [abapGit](https://abapgit.org), in this order:

1. [abap2UI5](https://github.com/abap2UI5/abap2UI5)
2. [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management) -
   before popups, whose value help and search help popups use it and fail
   activation without it
3. [abap2UI5-addons/popups](https://github.com/abap2UI5-addons/popups)
4. this repository - branch `main`, or `702` on NetWeaver 7.02 - 7.40

**Start** - the app is `z2ui5_cl_tm_001`; pass the table to maintain as the
URL parameter `table`:

```
?app_start=z2ui5_cl_tm_001&table=<TABLE_NAME>
```

Without the parameter (and without a launchpad startup parameter `table`) the
app opens no table; it asks for the table name first. Add `&readonly=X` (URL
or launchpad startup parameter) to show the table without any way to change
it. In the launchpad, a tile's startup parameter `table` wins over a URL
parameter of the same name, and `readonly=X` from either source switches the
read-only mode on - a URL cannot widen or lift what the tile configured. Both
are parameters the browser sends, not an authorization: restrict who may
change which table with the authorizations of your system.

A table with a column `ROW_ID` or `SELKZ` of its own cannot be maintained
here: the app uses both names for its technical columns (the row id and the
multi-edit selection), and refuses such a table with a message.

From an app of your own, start it with the factory - the table, and
optionally a configuration (`z2ui5_if_tm_001=>ty_s_config`):

```abap
client->nav_app_call( z2ui5_cl_tm_001=>factory(
    i_table  = `ZMY_TABLE`
    i_config = VALUE #( read_only       = abap_false
                        layout_handle04 = lv_org_unit ) ) ).
```

- `read_only` - display only: no Add, Copy, Delete, multi-edit, Save or
  transport, and the row popup opens with every input disabled. The URL
  parameter `readonly` can only switch it on, never off.
- `layout_handle04` - the fourth key of the saved column layouts. The layouts
  of layout-management are stored under `handle01` = the class name
  (`Z2UI5_CL_TM_001`, or its renamed name), `handle02` = the table,
  `handle03` = `SIMPLE_VIEW` and `handle04` = this value - e.g. an
  organisational unit, so that each one keeps layouts of its own. Empty (the
  default): one set of layouts per table, as before.

The row popup `z2ui5_cl_tm_pop` can be called on its own as well, with a
table that has a component `ROW_ID` and the DFIES of the fields to show -
`z2ui5_cl_tm_pop=>factory( io_table = REF #( lt_rows ) iv_row_id = ...
it_dfies = ... iv_edit_mode = abap_true iv_tabname = ... )`, optionally
`is_layout`, `iv_copy_mode`, `iv_read_only`; read the result back from its
`mt_data` once the popup returned.

For **several tables on one page**, start the view cluster `z2ui5_cl_tm_002`
with a comma-separated list of tables and an optional page title - as URL
parameters or launchpad startup parameters of the same names:

```
?app_start=z2ui5_cl_tm_002&tables=<TABLE_1>,<TABLE_2>&title=<TITLE>
```

The cluster opens only the tables of that list - a tab key the browser sends
for any other table is ignored - and a tile's startup parameter `tables`
wins over the URL parameter.

### Parallel installation (namespace rename)

To run a second copy next to the original in the same system, rename it the
way abap2UI5 itself is renamed: run the workflow **build-rename** (Actions →
build-rename → Run workflow) with the name your abap2UI5 installation was
renamed to - e.g. `ZMYUI5` or `/CA2UI5/`. It pushes the branch `rename_<name>`, in
which every object and every reference to abap2UI5 and to the addons this one
needs carries that name; pull it with abapGit. Rename abap2UI5 and every addon
of the installation with the same name.

## Usage

1. Start the app with the table you want to maintain (see above). The entries
   come up in a table; search them with the search field, choose and arrange
   the columns with the layout settings button (layout-management).
2. Press a row to edit the entry in a popup (`z2ui5_cl_tm_pop`), with value
   help from check tables, fixed values and search helps. **Add** opens the
   same popup for a new entry.
3. Switch to multi-edit mode in the menu at the bottom to select several rows
   and delete or copy them.
4. **Save** writes the changes to the database. If no transport request has
   been chosen yet, a popup asks for one first; the menu also records all
   entries on a transport request or changes the request.

### Saving, transport and locking

- **Client settings (SCC4).** On Standard ABAP, for a client-dependent
  customizing table (delivery class C or G), the save follows the logon
  client's change option (`T000-CCCORACTIV`) as SM30 does: with *no changes
  allowed* nothing is saved, and with *automatic recording* a transport
  request is required - **No Transport** opens the request popup again. Not
  checked: the cross-client setting (`T000-CCNOCLIIND`) for cross-client
  tables, *no transports allowed* (a request chosen there is used as
  chosen) and tables of other delivery classes. The request popup offers your modifiable customizing
  tasks only, so the entries of a cross-client table may be refused by the
  request as well.
- **ABAP Cloud records nothing on a transport request.** The request popup
  of popups has no Cloud implementation yet, so there the save writes the
  database without asking for a request, and the menu has no transport
  entries.

- **One LUW.** Save writes the database first - the deleted rows, then the
  rows of the table - and records the rows and the deletions on the transport
  request after that, with one `COMMIT WORK` for both. A database error rolls
  everything back and records nothing; a transport error after the database
  write rolls the database changes back as well. Either way the message says
  that nothing was saved, and the changes stay on the screen.
- **Deletions are computed per save** from the keys read from the database
  and the rows on the screen. A save that is refused keeps them pending, the
  next one executes them; **Transport all entries** records the rows of the
  table only, never a deletion that was not saved.
- **Lock while editing** (Standard ABAP). The first change - **Add**, a row
  opened for editing, the switch to multi-edit - locks the table with
  `ENQUEUE_E_TABLE` (the table name and, for a client-dependent table, the
  logon client) and switches the abap2UI5 session to stateful
  (`client->set_session_stateful`), so the lock outlives the request. Another
  user who starts a change gets a message with your user name; a row they
  open is shown read-only. The lock is released, and the session made
  stateless again, as soon as nothing is left to save (after a save outside
  multi-edit, a refresh, a cancelled popup, leaving multi-edit without
  changes), on a tab change in `z2ui5_cl_tm_002`, and with the session
  itself when the browser is closed or the session times out. The page has
  no back button of its own: a launchpad back or home with unsaved changes
  keeps the lock until the session times out. An app that embeds the
  maintenance and drops the instance releases the lock with
  `release_lock( client )` of `z2ui5_if_tm_001`, the way `z2ui5_cl_tm_002`
  does on a tab change.
- **The save checks again**, whatever happened to the session: it takes the
  lock for its own request and compares the database with what was read (the
  key and a hash of every row). Rows added, changed or deleted by someone
  else since the read are not overwritten - the save is refused with a
  message, and a refresh shows the current entries.

Limits: the lock is the app's own whole-table lock - it keeps two users of
this app (and tools that lock the whole table with `E_TABLE`) apart, not a
program that writes the table without locking it; the optimistic check
covers that case at save time. A stateful session holds a work process per
editing user while the lock is held. A page reloaded while it holds a lock
leaves the lock to the old session until that session ends - the message
then says it is your own lock in another session. ABAP Cloud has no
`E_TABLE` lock object: there the app takes no lock and stays stateless, and
the optimistic check alone protects the save. The check reads the whole
table before and after each save, which is meant for customizing-sized
tables.

### Edit entries
<img width="700" alt="Popup to edit an entry with value help" src="https://github.com/user-attachments/assets/3dc1de8d-4025-48c0-9372-79fd20c4279c">

## Features

* Add, edit, copy and delete entries of any database table
* Value help from check tables, fixed values and search helps
* Column layouts saved as variants (layout-management)
* Changes recorded on a transport request, in one LUW with the database
  changes
* Edit lock with the name of the user who holds it, and an optimistic check
  before each save (see [Saving, transport and locking](#saving-transport-and-locking))
* Read-only mode (`readonly=X` or the factory configuration)
* Texts borrowed from SAP data elements and message classes, with an English
  fallback where a system lacks them (EWM, S/4HANA)
* Several tables on one page (`z2ui5_cl_tm_002`): a tab per table with its
  number of entries, one transport request handed on from table to table, and
  a confirmation before a tab change would discard unsaved changes
* After saving, the table buffer of a buffered table is reset
  (`SBUF_GENERIC_RESET`), so the change shows at once - Standard ABAP only,
  ABAP Cloud has no table buffer reset

## Commits

The abap2UI5 addons (layout-management, popups, table-maintenance) share one
commit model:

- **The addon commits only in its own apps.** A popup or app of the addon
  that saves on a button of its own runs in a roundtrip of its own and ends
  its own LUW - `COMMIT WORK AND WAIT`, or `ROLLBACK WORK` on failure.
- **A library method commits only with `iv_commit = abap_true`.** Every
  public method that writes on behalf of its caller takes
  `iv_commit TYPE abap_bool DEFAULT abap_true`. The default keeps the old
  behaviour: the method ends with `COMMIT WORK AND WAIT` - and so commits
  whatever the caller has open at that moment as well. With
  `iv_commit = abap_false` it neither commits nor rolls back: its writes join
  the caller's LUW, and the caller commits them once, together with its own
  changes.
- **On failure the method raises its exception.** With
  `iv_commit = abap_false` it never runs `ROLLBACK WORK` - whatever it wrote
  before the failure is still in the caller's LUW, and the caller decides,
  normally with `ROLLBACK WORK`. With `iv_commit = abap_true` it fails as it
  always did (the table below).
- **The caller must commit.** abap2UI5 runs `ROLLBACK WORK` before and after
  `main( )` of a stateless app, so what an app writes - itself, or through a
  method called with `iv_commit = abap_false` - is lost unless the app commits
  it in the same roundtrip.

In this addon:

| Where | Commits | On failure |
|---|---|---|
| Library methods | none - no public method of this addon writes to the database (`release_lock( )` of `z2ui5_if_tm_001` only releases the lock) | - |
| **Save** in `z2ui5_cl_tm_001` - its own app | always: the rows, the deletions and the transport entries with one `COMMIT WORK AND WAIT` (popups' `add_data_to_tranport( )` called with `iv_commit = abap_false`) | `ROLLBACK WORK` of all of it and a message, see [Saving, transport and locking](#saving-transport-and-locking) |
| **Transport all entries** in `z2ui5_cl_tm_001` - its own app | always: `add_data_to_tranport( )` with its default `iv_commit = abap_true`, in a roundtrip that writes nothing else | a message toast, nothing committed |
| Layout settings (layout-management's popup) | Save and Delete of a layout, the popup's own app | see layout-management |

An app that embeds the maintenance (`mo_parent_view`, as `z2ui5_cl_tm_002`
does) runs it inside its own `main( )`: the maintenance commits only in the
roundtrips of its own **Save** and **Transport** buttons, and an embedder that
has changes of its own open in such a roundtrip has them committed with it.

## Security

The app writes to whatever table is passed in the URL parameter `table` and
carries no authorization check of its own. Before using it beyond a
development system, add your own authorization checks and restrict who may
run the app.

**That is a deliberate decision, not an open item.** abap2UI5 itself has no
central authorization hook either: the framework leaves the check to the app,
and this addon leaves it to the installation. Which check is the right one -
`S_TABU_NAM`, `S_TABU_DIS`, an authorization object of your own, or what ABAP
Cloud releases - depends on the system the addon runs in, so none is built in
and none is planned. Whoever opens the app to more people than its developers
adds the check that fits their system.

## Development

```bash
npm ci
npm run check           # all gates below, as in CI
npm run lint            # abaplint, Standard ABAP (v750)
npm run check:cloud     # abaplint, ABAP Cloud
npm run check:abap2ui5  # abap2UI5-linter over apps and views
npm run rename          # namespace rename check
npm run downport        # downport the source to 7.02 syntax
npm run site            # the GitHub Pages site (VitePress) into build/site
npm run site:dev        # the same with the VitePress dev server
```

## Contributing

Issues and pull requests are welcome - whether you're fixing bugs, adding new
functionality, or improving documentation. See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

MIT - see [LICENSE](LICENSE).
