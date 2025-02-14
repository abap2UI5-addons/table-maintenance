CLASS z2ui5_cl_ui_build_ranges DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object.

    DATA mo_sql    TYPE REF TO z2ui5_cl_util_sql.
    DATA mo_layout TYPE REF TO z2ui5_cl_layout.

    METHODS paint
      IMPORTING
        view   TYPE REF TO z2ui5_cl_xml_view
        client TYPE REF TO z2ui5_if_client.

    METHODS on_event
      IMPORTING
        client TYPE REF TO z2ui5_if_client.

    METHODS on_navigated
      IMPORTING
        client TYPE REF TO z2ui5_if_client.

    METHODS init_filter_tab.

  PROTECTED SECTION.
    DATA mv_popup_name TYPE string.
    METHODS init_layout.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_ui_build_ranges IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `SELSCREEN_POST`.
        init_filter_tab( ).
        init_layout( ).
        client->view_model_update( ).

      WHEN `UPDATE_TOKENS`.
        mo_sql->ms_sql-t_filter = z2ui5_cl_util=>filter_update_tokens(
            val    = mo_sql->ms_sql-t_filter
            name   = client->get_event_arg( ) ).
        mo_sql->count( ).
        client->view_model_update( ).

      WHEN 'LIST_OPEN'.
        mv_popup_name = client->get_event_arg( ).
        DATA(ls_sql) = mo_sql->ms_sql-t_filter[ name = client->get_event_arg( ) ].
        client->nav_app_call( z2ui5_cl_pop_get_range=>factory( ls_sql-t_range ) ).
        RETURN.

      WHEN 'LIST_DELETE'.
        CLEAR mo_sql->ms_sql-t_filter[ name = client->get_event_arg( ) ]-t_range.
        CLEAR mo_sql->ms_sql-t_filter[ name = client->get_event_arg( ) ]-t_token.
        client->view_model_update( ).

    ENDCASE.

  ENDMETHOD.

  METHOD paint.

*    init_filter_tab( ).

    DATA(tab) = view->table(
                             id = `tab`
                             items           = client->_bind_edit( mo_sql->ms_sql-t_filter )
                             selectionchange = client->_event( 'SELCHANGE' ) ).

    DATA(headder) = tab->header_toolbar(
               )->overflow_toolbar(
                )->button( press = client->_event( `CLEAR` )  icon = `sap-icon://clear-filter`
               )->input(
                    id = `selinput`
                    value = client->_bind_edit( mo_sql->ms_sql-tabname )
                    description = `Tablename`
                    width = '30%'
                    submit = client->_event( `SELSCREEN_POST` )
                )->button(
                    press = client->_event( z2ui5_cl_util_sql=>go_button( )-event_name )
                    icon = z2ui5_cl_util_sql=>go_button( )-icon_name
                    tooltip = z2ui5_cl_util_sql=>go_button( )-text
             )->input(
             width = '10%'
                    value = client->_bind_edit( mo_sql->ms_sql-count )
                    description = `Rows`
                    )->toolbar_spacer(
         )->checkbox( selected = client->_bind_edit( mo_sql->ms_sql-check_autoload )  text = `Skip`
                   )->input( value = client->_bind( mo_sql->ms_sql-layout_name ) width = `20%` description = `Layout`
                showvaluehelp = abaP_true
                valuehelprequest = client->_event( `DISPLAY_POPUP_SELECT_LAYOUT` )
                valuehelponly = abap_true ).

    tab->columns(
         )->column(
             )->text( 'Name' )->get_parent(
         )->column(
             )->text( 'Options' )->get_parent(
         )->column(
             )->text( 'Select' )->get_parent(
         )->column(
             )->text( 'Clear' )->get_parent( ).

    DATA(cells) =  tab->items( )->column_list_item( id = `items` )->cells(  ).
    cells->text( text = `{NAME}` ).
    DATA(multi) = cells->multi_input( tokens = `{T_TOKEN}`
             name = '{NAME}'
             enabled = abap_false
          valuehelprequest = client->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) ) ).
    multi->tokens(
         )->token(

                   key      = `{KEY}`
                   text     = `{TEXT}`
                   visible  = `{VISIBLE}`
                   selected = `{SELKZ}`
                   editable = `{EDITABLE}` ).

    cells->button( text  = `Select`
               press = client->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) ) ).
    cells->button( icon  = 'sap-icon://delete'
               type  = `Transparent`
               text  = `Clear`
               press = client->_event( val = `LIST_DELETE` t_arg = VALUE #( ( `${NAME}` ) ) )
     ).

    LOOP AT mo_sql->ms_sql-t_filter REFERENCE INTO DATA(lr_token).
      DATA(lv_tabix) = sy-tabix.

      view->_z2ui5( )->multiinput_ext(
                 multiinputname = client->_bind_edit( val = lr_token->name tab = mo_sql->ms_sql-t_filter tab_index = lv_tabix )
                 addedtokens      = client->_bind_edit( val = lr_token->t_token_added tab = mo_sql->ms_sql-t_filter tab_index =  lv_tabix )
                 removedtokens    = client->_bind_edit( val = lr_token->t_token_removed  tab = mo_sql->ms_sql-t_filter tab_index =  lv_tabix )
                 change           = client->_event( val = 'UPDATE_TOKENS'  t_arg = VALUE #( ( lr_token->name ) ) )
                   ).

    ENDLOOP.


  ENDMETHOD.

  METHOD init_filter_tab.

    TRY.
        DATA lr_table TYPE REF TO data.
        CREATE DATA lr_table TYPE STANDARD TABLE OF (mo_sql->ms_sql-tabname) WITH EMPTY KEY.
        ASSIGN lr_table->* TO FIELD-SYMBOL(<table>).
        mo_sql->ms_sql-t_filter = z2ui5_cl_util=>filter_get_multi_by_data( <table> ).
      CATCH cx_sy_create_data_error.
        IF mo_sql->ms_sql-tabname IS INITIAL.
          z2ui5_cl_util=>x_raise( |Table empty. Please enter a value.| ).
        ELSE.
          z2ui5_cl_util=>x_raise( |Table with Name { mo_sql->ms_sql-tabname } not found.| ).
        ENDIF.
    ENDTRY.
  ENDMETHOD.

  METHOD on_navigated.

    TRY.
        DATA(lo_popup) = CAST z2ui5_cl_pop_get_range( client->get_app( client->get( )-s_draft-id_prev_app ) ).
        IF lo_popup->result( )-check_confirmed = abap_true.
          ASSIGN mo_sql->ms_sql-t_filter[ name = mv_popup_name ] TO FIELD-SYMBOL(<tab>).
          <tab>-t_range = lo_popup->result( )-t_range.
          <tab>-t_token = z2ui5_cl_util=>filter_get_token_t_by_range_t( <tab>-t_range ).
          client->view_model_update( ).
        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD init_layout.

    mo_sql->ms_sql-layout_name = ``.

    DATA lr_table TYPE REF TO data.
    CREATE DATA lr_table TYPE TABLE OF spfli.
    mo_layout = z2ui5_cl_layout=>factory( control  = z2ui5_cl_layout=>m_table
                                          data     = lr_table
                                          handle01 = 'Z2UI5_CL_SE16'
                                          handle02 = mo_sql->ms_sql-tabname
                                          handle03 = ''
                                          handle04 = '' ).

  ENDMETHOD.

ENDCLASS.
