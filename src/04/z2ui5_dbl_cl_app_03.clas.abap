CLASS z2ui5_dbl_cl_app_03 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    CLASS-METHODS factory_popup_by_itab
      IMPORTING
        itab            TYPE data
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_dbl_cl_app_03.

    DATA:
      BEGIN OF ms_app,
        check_initialized     TYPE abap_bool,
        check_popup           TYPE abap_bool,
        itab                  TYPE REF TO data,
        max_rows              TYPE string,
        file                  TYPE string,
        file_size             TYPE string,
        file_entries          TYPE string,
        check_appwidthlimited TYPE abap_bool VALUE abap_true,
        db_table              TYPE string VALUE 'SPFLI',
        db_table_entries      TYPE string,
      END OF ms_app.

    DATA mt_tab TYPE REF TO data.

  PROTECTED SECTION.

    DATA client TYPE REF TO z2ui5_if_client.

    METHODS z2ui5_on_init.
    METHODS z2ui5_on_event.
    METHODS z2ui5_view_display.

  PRIVATE SECTION.
    DATA mv_db_save_callback TYPE string.
ENDCLASS.



CLASS z2ui5_dbl_cl_app_03 IMPLEMENTATION.

  METHOD factory_popup_by_itab.

    result = NEW #( ).
    result->ms_app-itab = z2ui5_cl_util=>conv_copy_ref_data( itab ).
    result->ms_app-check_popup = abap_true.

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client     = client.

    IF ms_app-check_initialized = abap_false.
      ms_app-check_initialized = abap_true.
      z2ui5_on_init( ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true.
      z2ui5_view_display( ).
    ENDIF.

    IF client->get( )-event IS NOT INITIAL.
      z2ui5_on_event( ).
    ENDIF.

  ENDMETHOD.


  METHOD z2ui5_on_event.

    CASE client->get( )-event.

      WHEN 'DB_CHECK'.

        TRY.
            ms_app-db_table = to_upper( ms_app-db_table ).

            SELECT SINGLE COUNT( * )
            FROM (ms_app-db_table)
            INTO ms_app-db_table_entries.

*            IF to_upper( ms_app-db_table(1) ) <>  `Z` AND to_upper( ms_app-db_table(1) ) <> `Y`.
*              client->message_box_display( `Only Tables in namespace Z or Y allowed` ).
*            ENDIF.

            client->view_model_update( ).
          CATCH cx_root.
            client->message_box_display( `DB Table no found, check input: ` && ms_app-db_table ).
        ENDTRY.

      WHEN `PROCESS`.

        FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.

        CREATE DATA mt_tab TYPE STANDARD TABLE OF (ms_app-db_table).
        ASSIGN mt_tab->* TO <tab2>.

        SELECT *
        FROM (ms_app-db_table)
        INTO CORRESPONDING FIELDS OF TABLE <tab2>.

        TRY.

            ms_app-file = z2ui5_cl_util=>json_stringify( <tab2> ).
            client->message_toast_display( |JSON created| ).

          CATCH cx_root INTO DATA(x).
            client->message_box_display( x->get_text( ) ).
        ENDTRY.

      WHEN `PREVIEW`.

        CREATE DATA mt_tab TYPE STANDARD TABLE OF (ms_app-db_table).
        ASSIGN mt_tab->* TO <tab2>.

        SELECT *
        FROM (ms_app-db_table)
        INTO CORRESPONDING FIELDS OF TABLE <tab2>
        UP TO 10 ROWS.

        DATA(lv_prev_json) = z2ui5_cl_util=>json_stringify( <tab2> ).
        client->nav_app_call( z2ui5_cl_pop_textedit=>factory( lv_prev_json ) ).

      WHEN 'DOWNLOAD'.
        client->nav_app_call( z2ui5_cl_pop_file_dl=>factory( ms_app-file ) ).

      WHEN 'BUTTON_CANCEL'.
        client->message_toast_display( |cancel| ).

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

    ENDCASE.

  ENDMETHOD.


  METHOD z2ui5_on_init.

    z2ui5_view_display( ).

  ENDMETHOD.


  METHOD z2ui5_view_display.



    IF ms_app-check_popup = abap_true.
     DATA(view) = z2ui5_cl_xml_view=>factory_popup( ).
      DATA(page) = view->dialog( ).
    ELSE.
     view = z2ui5_cl_xml_view=>factory( ).
      page = view->shell( appwidthlimited = client->_bind_edit( ms_app-check_appwidthlimited ) )->page(
                  title          = 'a2UI5 App - JSON File Download'
                  navbuttonpress = client->_event( 'BACK' )
                  shownavbutton = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
            )->header_content(
                 )->overflow_toolbar(
                 )->toolbar_spacer(
                  )->label( `Shell`
                  )->switch( state = client->_bind_edit( ms_app-check_appwidthlimited )
                  )->get_parent( )->get_parent( ).
    ENDIF.

    DATA(content) = page->simple_form( editable = `true` ).

    content->label( `(1) Set DB Table`
    )->input( width = `30%` description =  `DB Table` value = client->_bind_edit( ms_app-db_table )
     )->label(
    )->button( text = `Go` width = `10%` press = client->_event( `DB_CHECK` )
    )->label(
    )->input( width = `30%` description = `DB Entries` value = client->_bind_edit( ms_app-db_table_entries ) enabled = abap_false
    )->label( `(2) DB -> JSON`
   )->button( text = `Go` width = `10%` press = client->_event( `PROCESS` )
*    )->label(
*    )->input( width = `30%` description = `Number of Entries` value = client->_bind_edit( ms_app-file_entries )  enabled = abap_false
    )->label( `(3) Preview JSON`
     )->button( text = `Go` width = `10%` press = client->_event( `PREVIEW` )
   )->label( `(4) Export`
   )->button( text = `Run` width = `10%` press = client->_event( `DOWNLOAD` )
   ).

    IF ms_app-check_popup = abap_true.
      client->popup_display( view->stringify( ) ).
    ELSE.
      client->view_display( view->stringify( ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
