CLASS z2ui5_dbl_cl_app_01 DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA:
      BEGIN OF ms_app,
        check_initialized     TYPE abap_bool,
        max_rows              TYPE string,
        file                  TYPE string,
        file_size             TYPE string,
        file_entries          TYPE string,
        check_appwidthlimited TYPE abap_bool VALUE abap_true,
        db_table              TYPE string VALUE 'z2ui5_dbl_t_01',
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



CLASS Z2UI5_DBL_CL_APP_01 IMPLEMENTATION.


  METHOD z2ui5_if_app~main.

    me->client     = client.

    IF ms_app-check_initialized = abap_false.
      ms_app-check_initialized = abap_true.
      z2ui5_on_init( ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true.
      TRY.
          DATA(lo_popup_file) = CAST z2ui5_cl_pop_file_ul( client->get_app( client->get( )-s_draft-id_prev_app ) ).
          IF lo_popup_file->result( )-check_confirmed = abap_true.
            ms_app-file = lo_popup_file->result( )-value.
            client->message_toast_display( `File uploaded sucessfully` ).
            ms_app-file_size = CONV i( ( strlen( ms_app-file ) ) / 1000 ).
            client->view_model_update( ).
          ENDIF.
          RETURN.
        CATCH cx_root.
      ENDTRY.
      TRY.
          DATA(lo_popup_confirm) = CAST z2ui5_cl_pop_to_confirm( client->get_app( client->get( )-s_draft-id_prev_app ) ).
          IF lo_popup_confirm->result( ) = abap_true.

            FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.
            ASSIGN mt_tab->* TO <tab2>.
            MODIFY (ms_app-db_table) FROM TABLE <tab2>.
            COMMIT WORK AND WAIT.
            client->message_box_display( `DB updated` ).
          ENDIF.
          RETURN.
        CATCH cx_root.
      ENDTRY.
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

            IF to_upper( ms_app-db_table(1) ) <>  `Z` AND to_upper( ms_app-db_table(1) ) <> `Y`.
              client->message_box_display( `Only Tables in namespace Z or Y allowed` ).
            ENDIF.

            client->view_model_update( ).
          CATCH cx_root.
            client->message_box_display( `DB Table no found, check input: ` && ms_app-db_table ).
        ENDTRY.

      WHEN `PROCESS`.
        TRY.
            FIELD-SYMBOLS <tab2> TYPE STANDARD TABLE.

            CREATE DATA mt_tab TYPE STANDARD TABLE OF (ms_app-db_table).
            ASSIGN mt_tab->* TO <tab2>.

            z2ui5_cl_util=>json_parse(
              EXPORTING
                val  = ms_app-file
              CHANGING
                data = <tab2>
            ).

            ms_app-file_entries = lines( <tab2> ).
            client->view_model_update( ).

          CATCH cx_root INTO DATA(x).
            client->message_box_display( x->get_text( ) ).
        ENDTRY.

      WHEN `PREVIEW`.
        ASSIGN mt_tab->* TO <tab2>.

        DATA lr_tab TYPE REF TO data.
        DATA lr_dummy TYPE REF TO data.
        CREATE DATA lr_tab TYPE STANDARD TABLE OF (ms_app-db_table).

        lr_tab = z2ui5_cl_util=>conv_copy_ref_data( mt_tab ).

        ASSIGN lr_tab->* TO <tab2>.
        LOOP AT <tab2> REFERENCE INTO lr_dummy.
          IF sy-tabix > 5.
            DELETE <tab2>.
          ENDIF.
        ENDLOOP.

        client->nav_app_call( z2ui5_cl_pop_table=>factory( <tab2> ) ).

      WHEN 'DB_SAVE'.
        mv_db_save_callback = client->nav_app_call( z2ui5_cl_pop_to_confirm=>factory( `Database will be deleted and new entries filled. Are you sure?`) ).

      WHEN 'UPLOAD'.
        client->nav_app_call( z2ui5_cl_pop_file_ul=>factory( ) ).

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

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    DATA(page) = view->shell( appwidthlimited = client->_bind_edit( ms_app-check_appwidthlimited ) )->page(
                title          = 'a2UI5 App - JSON File Upload'
                navbuttonpress = client->_event( 'BACK' )
                shownavbutton = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
          )->header_content(
               )->overflow_toolbar(
               )->toolbar_spacer(
                )->label( `Shell`
                )->switch( state = client->_bind_edit( ms_app-check_appwidthlimited )
                )->link(
                    text = 'Project on GitHub'
                    target = '_blank'
                    href = `https://github.com/oblomov-dev/a2UI5-db_table_loader`
                )->get_parent(  )->get_parent( ).

    DATA(content) = page->simple_form( editable = `true` ).

    content->label( `(1) JSON File Upload`
        )->button( text = `Go` width = `10%` press = client->_event( `UPLOAD` )
          )->label(
         )->input( width = `30%` description = `Size (kB)` value = client->_bind( ms_app-file_size ) enabled = abap_false
        )->label( `(2) Check DB Table`
        )->input( width = `30%` description =  `DB Table` value = client->_bind_edit( ms_app-db_table )
         )->label(
        )->button( text = `Go` width = `10%` press = client->_event( `DB_CHECK` )
        )->label(
        )->input( width = `30%` description = `DB Entries` value = client->_bind_edit( ms_app-db_table_entries ) enabled = abap_false
        )->label( `(3) JSON -> ITAB`
       )->button( text = `Go` width = `10%` press = client->_event( `PROCESS` )
        )->label(
        )->input( width = `30%` description = `Number of Entries` value = client->_bind_edit( ms_app-file_entries )  enabled = abap_false
        )->label( `(4) Preview Rows`
         )->button( text = `Go` width = `10%` press = client->_event( `PREVIEW` )
       )->label( `(5) Save Database`
       )->text( `Attention - Database Content will be deleted!!!`
       )->label(
       )->button( text = `Run` width = `10%` press = client->_event( `DB_SAVE` )
       ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.
ENDCLASS.
