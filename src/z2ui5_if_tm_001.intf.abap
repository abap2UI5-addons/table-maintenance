INTERFACE z2ui5_if_tm_001
  PUBLIC.

  " Rerendering needed
  DATA mv_view_display      TYPE abap_bool.
  " View from a view cluster: the element this app renders INTO - normally the
  " sap.m.Page. It used to be the whole view, which the app then searched for a
  " `Page`; the generic view builder cannot look an element up by name, so an
  " embedder now passes the node directly.
  DATA mo_parent_view       TYPE REF TO z2ui5_cl_ui5_view_builder.
  " The App is in an unsaved state - Message pops up when leaving
  DATA mv_change_active     TYPE abap_bool.

  DATA ms_transport         TYPE z2ui5_cl_tm_context=>ty_s_transport.

  TYPES:
    "! What a caller sets before the app runs - z2ui5_cl_tm_001=>factory( ),
    "! or ms_config directly on an instance it embeds
    BEGIN OF ty_s_config,
      "! show the entries only: no add, copy, delete, multi-edit, save or
      "! transport, and the row popup opens with every input disabled.
      "! Also the URL parameter (or launchpad startup parameter) readonly=X
      read_only       TYPE abap_bool,
      "! the fourth key (handle04) of the column layouts saved with
      "! layout-management, next to the class name, the table and
      "! SIMPLE_VIEW - e.g. an organisational unit, so that each one keeps
      "! layouts of its own. Empty (the default): one set per table
      layout_handle04 TYPE string,
    END OF ty_s_config.

  DATA ms_config            TYPE ty_s_config.

  METHODS set_app_data
    IMPORTING
      data TYPE string.

ENDINTERFACE.
