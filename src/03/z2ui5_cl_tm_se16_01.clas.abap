CLASS z2ui5_cl_tm_se16_01 DEFINITION PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mo_ui_ranges TYPE REF TO z2ui5_cl_layo_var_ui.

  PROTECTED SECTION.
    DATA client TYPE REF TO z2ui5_if_client.

    METHODS on_event.
    METHODS view_display.
    METHODS on_navigated.
    METHODS on_init.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_tm_se16_01 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'DISPLAY_POPUP_SELECT_LAYOUT'.
        client->nav_app_call( z2ui5_cl_layo_manager=>choose_layout( handle01 = 'ZSE16'
                                                              handle02 = mo_ui_ranges->mo_sql->ms_sql-tabname ) ).

      WHEN 'GO'.
        DATA(lo_tab_output) = NEW z2ui5_cl_tm_se16_02( ).
        lo_tab_output->mo_sql = z2ui5_cl_layo_var_sql=>factory( mo_ui_ranges->mo_sql->ms_sql ).
        client->nav_app_call( lo_tab_output ).

      WHEN 'BACK'.
        client->nav_app_leave( ).

      WHEN OTHERS.
        mo_ui_ranges->on_event( client ).

    ENDCASE.

  ENDMETHOD.

  METHOD view_display.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    DATA(page) = view->shell( )->page( title          = 'abap2UI5 - SE16 CLOUD - Start'
                                       navbuttonpress = client->_event( 'BACK' )
                                       shownavbutton  = client->check_app_prev_stack( )
                                        floatingfooter = abap_true
                                        ).

    IF mo_ui_ranges->mo_sql->ms_sql-tabname IS NOT INITIAL.
      mo_ui_ranges->paint( view   = page
                           client = client ).
    ENDIF.

    page->footer( )->overflow_toolbar(
        )->toolbar_spacer(
        )->button( text  = z2ui5_cl_layo_var_ui=>go_button( )-text
                   type  = `Emphasized`
                   press = client->_event( z2ui5_cl_layo_var_ui=>go_button( )-event_name ) ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    TRY.

        me->client = client.

        IF client->check_on_init( ).
          on_init( ).
        ELSEIF client->check_on_navigated( ).
          on_navigated( ).
        ELSE.
          on_event( ).
        ENDIF.

      CATCH cx_root INTO DATA(x).
        client->message_box_display( x ).
    ENDTRY.
  ENDMETHOD.

  METHOD on_navigated.

    TRY.
        DATA(lo_popup) = CAST z2ui5_cl_layo_pop_w_sel( client->get_app_prev( ) ).
        DATA(lo_layout) = lo_popup->result( ).

        IF lo_layout-check_confirmed = abap_true.

          FIELD-SYMBOLS <layout> TYPE z2ui5_t_11.
          ASSIGN lo_layout-row->* TO <layout>.

          mo_ui_ranges->mo_sql->ms_sql-layout_name = <layout>-layout.
          mo_ui_ranges->mo_sql->ms_sql-layout_id   = <layout>-guid.
          client->view_model_update( ).

        ENDIF.

      CATCH cx_root.
    ENDTRY.

    TRY.
        CAST z2ui5_cl_tm_se16_02( client->get_app_prev( ) ).
        view_display( ).
        RETURN.
      CATCH cx_root.
    ENDTRY.

    mo_ui_ranges->on_navigated( client ).

  ENDMETHOD.

  METHOD on_init.

    DATA lr_table TYPE REF TO data.

    mo_ui_ranges = NEW z2ui5_cl_layo_var_ui( ).
    mo_ui_ranges->mo_sql = NEW #( ).
    mo_ui_ranges->mo_sql->ms_sql-tabname = `USR01`.
    mo_ui_ranges->mo_sql->ms_sql-count   = `500`.

    lr_table = z2ui5_cl_util=>rtti_create_tab_by_name( `T100` ).
    mo_ui_ranges->mo_layout = z2ui5_cl_layo_manager=>factory( control  = z2ui5_cl_layo_manager=>m_table
                                          data     = lr_table
                                          handle01 = 'Z2UI5_CL_SE16'
                                          handle02 = mo_ui_ranges->mo_sql->ms_sql-tabname
                                          handle03 = ''
                                          handle04 = '' ).

    mo_ui_ranges->init_filter_tab( ).
    view_display( ).

  ENDMETHOD.

ENDCLASS.
