---
layout: home

hero:
  name: "Table Maintenance"
  text: "Add, edit, copy and delete table entries, no SAP GUI needed."
  image:
    src: /favicon.png
    alt: abap2UI5
  actions:
    - theme: brand
      text: Get started
      link: /guide
    - theme: alt
      text: GitHub
      link: https://github.com/abap2UI5-addons/table-maintenance
    - theme: alt
      text: All addons
      link: https://abap2ui5-addons.github.io/

features:
  - title: "Generic maintenance screen built from the table structure at runtime"
  - title: "Edit entries in a popup with value help from check tables, fixed values and search helps"
  - title: "Add, copy and delete entries, multi-edit mode for several rows"
  - title: "Record changes on a transport request"
  - title: "Search field and column layouts via layout-management"
  - title: "Embeddable into a view cluster through the interface z2ui5_if_tm_001"
---

## What it does

One generic abap2UI5 app shows the entries of a database table and lets you add, edit, copy and delete them, with value helps from check tables, fixed values and search helps, and records the changes on a transport request. It reads the table's structure at runtime, so nothing has to be generated per table. It is for developers and admins who maintain customizing and Z tables, on ABAP Cloud as well as on Standard ABAP, including systems without SM30.

## Requirements

- abap2UI5
- Standard ABAP on SAP NetWeaver AS ABAP 7.50 or higher
- ABAP Cloud: yes
- abap2UI5-addons/popups (value help, search help and transport request popup)
- abap2UI5-addons/layout-management (column layouts of the table)

## Installation

With [abapGit](https://abapgit.org), from [the repository](https://github.com/abap2UI5-addons/table-maintenance):

- Install with abapGit in this order: abap2UI5, abap2UI5-addons/popups, abap2UI5-addons/layout-management, then this repository (branch main)
- Start with ?app_start=z2ui5_cl_tm_001&table=<TABLE_NAME>; without the parameter the app asks for the table name first
- The app carries no authorization check of its own; add your own before using it beyond a development system

The [Guide](/guide) has every step.

## A first app

```abap
?app_start=z2ui5_cl_tm_001&table=<TABLE_NAME>
```

## Links

- [abap2UI5-addons/popups](https://github.com/abap2UI5-addons/popups)
- [abap2UI5-addons/layout-management](https://github.com/abap2UI5-addons/layout-management)
- [The repository](https://github.com/abap2UI5-addons/table-maintenance) - issues and pull requests welcome
