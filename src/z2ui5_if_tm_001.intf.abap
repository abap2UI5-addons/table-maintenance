INTERFACE z2ui5_if_tm_001
  PUBLIC.

  " Rerendering needed
  DATA mv_view_display      TYPE abap_bool.
  " Model Update needed
  DATA mv_view_model_update TYPE abap_bool.
  " view from Viewcluster
  DATA mo_parent_view       TYPE REF TO z2ui5_cl_xml_view.
  " The App is in an unsaves State - Message pops up when leaving
  DATA mv_change_active     TYPE abap_bool.

  DATA ms_transport         type z2ui5_cl_util_ext=>ty_s_transport .

  METHODS set_app_data
    IMPORTING
      data TYPE string.

ENDINTERFACE.
