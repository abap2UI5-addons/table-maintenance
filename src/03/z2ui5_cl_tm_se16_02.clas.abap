CLASS z2ui5_cl_tm_se16_02 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mo_sql     TYPE REF TO z2ui5_cl_layo_var_sql.
    DATA mo_variant TYPE REF TO z2ui5_cl_layo_var_db.
    DATA mo_layout  TYPE REF TO z2ui5_cl_layo_manager.

  PROTECTED SECTION.
    DATA client               TYPE REF TO z2ui5_if_client.
    DATA mv_check_initialized TYPE abap_bool.

    METHODS on_event.
    METHODS view_display.
    METHODS on_navigated.
    METHODS on_init.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_tm_se16_02 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.
      WHEN `BUTTON_START`.
        mo_sql->read( ).
        view_display( ).
      WHEN 'BACK'.
        client->nav_app_leave( ).
      WHEN OTHERS.
        z2ui5_cl_layo_pop=>on_event_layout( client = client
                                                      layout = mo_layout ).
    ENDCASE.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    DATA(page) = view->shell( )->page(
                     id             = `page_main`
                     title          = |abap2UI5 - SE16-CLOUD -{ mo_sql->ms_sql-tabname }|
                     navbuttonpress = client->_event( 'BACK' )
                     floatingfooter = abap_true
                     shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ) ).

    z2ui5_cl_layo_xml_builder=>xml_build_table( i_data   = mo_sql->ms_sql-t_ref
                                           i_xml    = page
                                           i_client = client
                                           i_layout = mo_layout ).

    page->footer( )->overflow_toolbar(
        )->button( text  = `Back`
                   press = client->_event( `BACK` )
        )->toolbar_spacer(
        )->button( text  = `Refresh`
                   press = client->_event( `REFRESH` ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    TRY.

        me->client = client.

        IF mv_check_initialized = abap_false.
          mv_check_initialized = abap_true.
          on_init( ).
          RETURN.

        ENDIF.

        IF client->get( )-check_on_navigated = abap_true.
          on_navigated( ).
          RETURN.
        ENDIF.

        IF client->get( )-event IS NOT INITIAL.
          on_event( ).
          RETURN.
        ENDIF.

      CATCH cx_root INTO DATA(x).
        client->message_box_display( text = x->get_text( )
                                     type = `error` ).
    ENDTRY.
  ENDMETHOD.

  METHOD on_navigated.

    TRY.

        DATA(app) = CAST z2ui5_cl_layo_pop( client->get_app( client->get( )-s_draft-id_prev_app ) ).
        mo_layout = app->mo_layout.

        IF app->mv_rerender = abap_true.
          " subcolumns need rerendering to work ..
          view_display( ).
        ELSE.
          "  for all other changes in Layout View Model Update is enough.
          client->view_model_update( ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD on_init.

    IF mo_sql IS NOT BOUND.
      mo_sql = z2ui5_cl_layo_var_sql=>factory( ).
      mo_sql->ms_sql-tabname = 'USR01'.
    ENDIF.

    mo_sql->read( ).

    IF mo_layout IS NOT BOUND.

      IF mo_sql->ms_sql-layout_id IS INITIAL.

        mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                              data     = mo_sql->ms_sql-t_ref
                                              handle01 = 'ZSE16'
                                              handle02 = mo_sql->ms_sql-tabname
                                              handle03 = ''
                                              handle04 = '' ).
      ELSE.
        mo_layout = z2ui5_cl_layo_manager=>factory_by_guid( layout_guid = mo_sql->ms_sql-layout_id ).
      ENDIF.

    ENDIF.

    view_display( ).

  ENDMETHOD.

ENDCLASS.
