# table-maintenance

[![abap2UI5-addons](https://img.shields.io/badge/abap2UI5--addons-app-1873b4)](https://github.com/abap2UI5-addons)
[![ABAP](https://img.shields.io/badge/ABAP-Cloud%20%7C%20Standard%20%E2%89%A5%207.50-blue)](#installation)
[![abap2UI5](https://img.shields.io/badge/requires-abap2UI5-blue)](https://github.com/abap2UI5/abap2UI5)
[![popups](https://img.shields.io/badge/requires-popups-blue)](https://github.com/abap2UI5-addons/popups)
[![layout-management](https://img.shields.io/badge/requires-layout--management-blue)](https://github.com/abap2UI5-addons/layout-management)
[![License](https://img.shields.io/github/license/abap2UI5-addons/table-maintenance)](LICENSE)
<br>
[![ABAP Cloud](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/table-maintenance/abap-cloud.yaml?branch=main&label=ABAP%20Cloud)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/abap-cloud.yaml)
[![ABAP Standard](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/table-maintenance/abap-standard.yaml?branch=main&label=ABAP%20Standard)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/abap-standard.yaml)
[![rename](https://img.shields.io/github/actions/workflow/status/abap2UI5-addons/table-maintenance/check-rename.yaml?branch=main&label=rename)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/check-rename.yaml)
[![check-abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Ftable-maintenance%2Fbadges%2Fcheck-abap2ui5.json)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/check-abap2ui5.yaml)
[![abap2UI5](https://img.shields.io/endpoint?url=https%3A%2F%2Fraw.githubusercontent.com%2Fabap2UI5-addons%2Ftable-maintenance%2Fbadges%2Fabap2ui5.json)](https://github.com/abap2UI5-addons/table-maintenance/actions/workflows/check-abap2ui5.yaml)

**Table maintenance in your browser - no Eclipse or SAP GUI installation
needed.** One generic abap2UI5 app shows the entries of a database table and
lets you add, edit, copy and delete them, with value helps from check tables,
fixed values and search helps, and records the changes on a transport request.
For developers and admins who maintain customizing and Z tables, on ABAP
Cloud as well as on Standard ABAP.

> Part of [abap2UI5-addons](https://github.com/abap2UI5-addons) - addons and apps for [abap2UI5](https://github.com/abap2UI5/abap2UI5), installed with [abapGit](https://abapgit.org).

<img width="700" alt="Google Chrome 2024-09-09 12 14 58" src="https://github.com/user-attachments/assets/51a1d7e5-ca7e-4359-9e12-39b00b3c11bf">

## Why

Maintaining a table usually means SM30 with a generated maintenance view in
SAP GUI - and on ABAP Cloud there is no SAP GUI at all. table-maintenance
needs nothing generated per table: it reads the table's structure at runtime
and builds the maintenance screen from it, in the browser.

Good for:

- **Customizing and Z tables** that need a quick maintenance screen.
- **ABAP Cloud systems** without SM30.
- **Embedding** - a view cluster can render the app into its own page through
  the interface `z2ui5_if_tm_001`.

## Installation

**Requirements**

- ABAP Cloud (S/4 Public Cloud, BTP ABAP Environment, S/4 Private Cloud or
  On-Premise with ABAP for Cloud) or Standard ABAP on R/3 NetWeaver AS ABAP
  7.50 or higher
- [abap2UI5](https://github.com/abap2UI5/abap2UI5)
- [abap2UI5-addons/popups](https://github.com/abap2UI5-addons/popups) - value
  help, search help and the transport request popup
- [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management) -
  the column layouts of the table

**Steps** - with [abapGit](https://abapgit.org), in this order:

1. [abap2UI5](https://github.com/abap2UI5/abap2UI5)
2. [abap2UI5-addons/popups](https://github.com/abap2UI5-addons/popups)
3. [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management)
4. this repository (branch `main`)

**Start** - the app is `z2ui5_cl_tm_001`; pass the table to maintain as the
URL parameter `table`:

```
?app_start=z2ui5_cl_tm_001&table=<TABLE_NAME>
```

Without the parameter (and without a launchpad startup parameter `table`) the
app opens `USR01`.

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

### Edit entries
<img width="700" alt="Google Chrome 2024-09-09 12 14 58" src="https://github.com/user-attachments/assets/3dc1de8d-4025-48c0-9372-79fd20c4279c">

## Features

* Edit Data, Adjust Customizing entries with Value-Help, Fixed Values and Search-Helps, Add Entries, Delete Entires, Transport Changes

## Security

The app writes to whatever table is passed in the URL parameter `table` and
carries no authorization check of its own. Before using it beyond a
development system, add your own authorization checks and restrict who may
run the app.

## Development

```sh
npm ci
npm run check   # abaplint Standard + ABAP Cloud, abap2UI5-linter, rename check
```

`npm run check` runs the same steps as CI.

## Contributing

Issues and pull requests are welcome - whether you're fixing bugs, adding new
functionality, or improving documentation. See [CONTRIBUTING.md](CONTRIBUTING.md).

## License

MIT - see [LICENSE](LICENSE).
