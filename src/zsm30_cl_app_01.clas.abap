CLASS zsm30_cl_app_01 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    INTERFACES z2ui5_if_app.

    DATA ms_layout        TYPE z2ui5_cl_pop_display_layout=>ty_s_layout.
    DATA mv_search_value  TYPE string.
    DATA mt_table         TYPE REF TO data.
    DATA mv_change_active TYPE abap_bool.
    DATA mv_table         TYPE string.
    DATA mt_table_tmp     TYPE REF TO data.
    DATA mt_table_del     TYPE REF TO data.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_keys,
        row_id TYPE string,
        key    TYPE string,
      END OF ty_s_keys.
    TYPES ty_t_keys TYPE STANDARD TABLE OF ty_s_keys WITH EMPTY KEY.

    DATA mt_dfies          TYPE z2ui5_cl_util=>ty_t_dfies.
    DATA client            TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.
    DATA mv_f4_fname       TYPE string.
    DATA mv_shlpfield      TYPE string.
    DATA ms_transport      TYPE z2ui5_cl_popup_show_tr=>ty_s_data.
    DATA mv_transport      TYPE string.
    DATA mv_multi_edit     TYPE abap_bool.
    DATA mv_key_error      TYPE abap_bool.

    DATA mv_ucomm_tmp      TYPE string.

    METHODS on_init.

    METHODS on_event.

    METHODS search.

    METHODS get_data.

    METHODS set_row_id.

    METHODS get_dfies.

    METHODS row_select.

    METHODS render_main.

    METHODS get_txt
      IMPORTING
        roll          TYPE string
        !type         TYPE char1 OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS button_save.

    METHODS get_keys
      IMPORTING
        !table        TYPE STANDARD TABLE
      RETURNING
        VALUE(result) TYPE ty_t_keys.

    METHODS get_comp
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    METHODS render_main_footer
      IMPORTING
        !page TYPE REF TO z2ui5_cl_xml_view.

    METHODS render_main_head
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_xml_view.

    METHODS on_event_main.

    METHODS on_event_layout.

    METHODS get_table_name.

    METHODS on_after_layout.

    METHODS get_layout.

    METHODS on_after_transport.

    METHODS transport_all.
    METHODS on_event_transport.

    METHODS render_ui_table
      IMPORTING
        !page TYPE REF TO z2ui5_cl_xml_view.

    METHODS render_table
      IMPORTING
        !page TYPE REF TO z2ui5_cl_xml_view.

    METHODS button_delete.
    METHODS check_input.

    METHODS button_copy.
    METHODS view_model_update.
    METHODS on_after_popup.

  PRIVATE SECTION.

ENDCLASS.


CLASS zsm30_cl_app_01 IMPLEMENTATION.

  METHOD get_txt.

    DATA(texts) = z2ui5_cl_util=>rtti_get_data_element_texts( roll ).

    CASE type.
      WHEN 'L'.
        result = texts-long.
      WHEN 'M'.
        result = texts-medium.
      WHEN 'S'.
        result = texts-short.
      WHEN 'H'.
        result = texts-header.
      WHEN OTHERS.
        result = texts-short.
    ENDCASE.

  ENDMETHOD.

  METHOD button_save.

    DATA t_table     TYPE REF TO data.
    DATA t_table_del TYPE REF TO data.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tmp>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del_org> TYPE STANDARD TABLE.

    IF ms_transport IS INITIAL.

      mv_ucomm_tmp = client->get( )-event.
      client->nav_app_call( z2ui5_cl_popup_show_tr=>factory( ) ).

    ELSE.

      CLEAR mv_ucomm_tmp.

      ASSIGN mt_table->* TO <tab>.
      ASSIGN mt_table_del->* TO <del>.
      ASSIGN mt_table_tmp->* TO <tmp>.

      " Compare all Key entries with the TMP. All entries that are not present are deleted.
      DATA(keys) = get_keys( <tab> ).
      DATA(keys_tmp) = get_keys( <tmp> ).

      LOOP AT keys_tmp INTO DATA(key_tmp).
        IF line_exists(  keys[ key = key_tmp-key ] ).
          DELETE keys_tmp.
        ENDIF.
      ENDLOOP.

      LOOP AT <tmp> ASSIGNING FIELD-SYMBOL(<line>).
        ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO FIELD-SYMBOL(<id>).
        IF line_exists(  keys_tmp[ row_id = <id> ] ).
          APPEND <line> TO <del>.
        ENDIF.
      ENDLOOP.

      " Duplicate Key error?!
      SORT keys BY key ASCENDING.
      keys_tmp = keys.
      DELETE ADJACENT DUPLICATES FROM keys COMPARING key.
      IF lines( keys ) <> lines( keys_tmp ).
        client->message_box_display( text = `Duplicate entries error.`
                                     type = `error`
                                     icon = `sap-icon://key` ).

        RETURN.
      ENDIF.

      DATA(comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( mv_table ).

      TRY.

          DATA(struct_desc) = cl_abap_structdescr=>create( comp ).

          DATA(table_desc) = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA t_table     TYPE HANDLE table_desc.
          CREATE DATA t_table_del TYPE HANDLE table_desc.

          ASSIGN t_table->* TO <tab_org>.
          ASSIGN t_table_del->* TO <del_org>.

          MOVE-CORRESPONDING <del> TO <del_org>.
          MOVE-CORRESPONDING <tab> TO <tab_org>.

        CATCH cx_root.

      ENDTRY.

      z2ui5_cl_popup_show_tr=>add_DATA_to_tranport( ir_data      = t_table
                                                    iv_tabname   = mv_table
                                                    is_transport = ms_transport ).

      z2ui5_cl_popup_show_tr=>add_DATA_to_tranport( ir_data      = t_table_del
                                                    iv_tabname   = mv_table
                                                    is_transport = ms_transport ).

      TRY.

          IF <del> IS ASSIGNED.
            IF <del> IS NOT INITIAL.

              DELETE (mv_table) FROM TABLE @<del_org>.
              IF sy-subrc = 0.
                COMMIT WORK AND WAIT.
                CLEAR <del>.
              ENDIF.

            ENDIF.
          ENDIF.

          MODIFY (mv_table) FROM TABLE @<tab_org>.

          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.
            client->message_toast_display( `Save successful` ).

            CLEAR mv_change_active.
          ENDIF.

          view_model_update( ).

        CATCH cx_root.
      ENDTRY.

    ENDIF.
  ENDMETHOD.

  METHOD get_keys.

    FIELD-SYMBOLS <line> TYPE any.

    LOOP AT table ASSIGNING <line>.

      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO FIELD-SYMBOL(<id>).

      DATA(keys) = VALUE ty_s_keys( row_id = <id> ).

      LOOP AT mt_dfies INTO DATA(dfies) WHERE keyflag = abap_true.

        ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <line> TO FIELD-SYMBOL(<key>).

        keys-key = keys-key && <key>.

      ENDLOOP.

      APPEND keys TO result.
      CLEAR keys.

    ENDLOOP.

  ENDMETHOD.

  METHOD Transport_all.

    DATA t_table     TYPE REF TO data.
    DATA t_table_del TYPE REF TO data.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del_org> TYPE STANDARD TABLE.

    IF ms_transport IS INITIAL.

      mv_ucomm_tmp = client->get( )-event.
      client->nav_app_call( z2ui5_cl_popup_show_tr=>factory( ) ).

    ELSE.

      CLEAR mv_ucomm_tmp.

      ASSIGN mt_table->* TO <tab>.
      ASSIGN mt_table_del->* TO <del>.

      DATA(comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( mv_table ).

      TRY.

          DATA(struct_desc) = cl_abap_structdescr=>create( comp ).

          DATA(table_desc) = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA t_table     TYPE HANDLE table_desc.
          CREATE DATA t_table_del TYPE HANDLE table_desc.

          ASSIGN t_table->* TO <tab_org>.
          ASSIGN t_table_del->* TO <del_org>.

          MOVE-CORRESPONDING <del> TO <del_org>.
          MOVE-CORRESPONDING <tab> TO <tab_org>.

        CATCH cx_root.

      ENDTRY.

      z2ui5_cl_popup_show_tr=>add_DATA_to_tranport( ir_data      = t_table
                                                    iv_tabname   = mv_table
                                                    is_transport = ms_transport ).

      z2ui5_cl_popup_show_tr=>add_DATA_to_tranport( ir_data      = t_table_del
                                                    iv_tabname   = mv_table
                                                    is_transport = ms_transport ).

      client->message_toast_display( 'Entries were added to the transport request' ).

    ENDIF.
  ENDMETHOD.

  METHOD get_comp.
    DATA index TYPE int4.
    DATA selkz TYPE abap_bool.

    TRY.

        DATA(comp) = z2ui5_cl_util=>rtti_get_t_attri_by_table_name( mv_table ).

        IF xsdbool( line_exists( comp[ name = 'ROW_ID' ] ) ) = abap_false.
          APPEND LINES OF VALUE cl_abap_structdescr=>component_table(
                                    ( name = 'ROW_ID'
                                      type = CAST #( cl_abap_datadescr=>describe_by_data( index ) ) ) ) TO result.
        ENDIF.
        IF xsdbool( line_exists( comp[ name = 'SELKZ' ] ) ) = abap_false.
          APPEND LINES OF VALUE cl_abap_structdescr=>component_table(
                                    ( name = 'SELKZ'
                                      type = CAST #( cl_abap_datadescr=>describe_by_data( selkz ) ) ) ) TO result.

        ENDIF.

        APPEND LINES OF comp TO result.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD get_data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    DATA(t_comp) = get_comp( ).
    TRY.

        DATA(new_struct_desc) = cl_abap_structdescr=>create( t_comp ).

        DATA(new_table_desc) = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mt_table     TYPE HANDLE new_table_desc.
        CREATE DATA mt_table_del TYPE HANDLE new_table_desc.
        CREATE DATA mt_table_tmp TYPE HANDLE new_table_desc.

        ASSIGN mt_table->* TO <table>.

        SELECT *
          FROM (mv_table)
          INTO CORRESPONDING FIELDS OF TABLE @<table>.

        SORT <table>.

      CATCH cx_root.

    ENDTRY.

    set_row_id( ).

    mt_table_tmp->* = mt_table->*.
  ENDMETHOD.

  METHOD get_dfies.

    mt_dfies = z2ui5_cl_util=>rtti_get_t_dfies_by_table_name( mv_table ).

    " Check if the DB for the F4 Help is released
    LOOP AT mt_dfies REFERENCE INTO DATA(dfies) WHERE checktable IS NOT INITIAL.
      TRY.
          cl_abap_typedescr=>describe_by_name( dfies->checktable ).
        CATCH cx_root.
          CLEAR dfies->checktable.
      ENDTRY.
    ENDLOOP.

  ENDMETHOD.

  METHOD on_event.
    on_event_main( ).

    on_event_layout( ).

    on_event_transport( ).

  ENDMETHOD.

  METHOD on_init.
    get_table_name( ).

    get_data( ).

    get_dfies( ).

    get_layout( ).

  ENDMETHOD.

  METHOD get_table_name.

    IF mv_table IS NOT INITIAL.
      RETURN.
    ENDIF.

    " TABLE per Parameter?
    DATA(tab) = z2ui5_cl_util=>url_param_get( val = 'TABLE'
                                              url = client->get( )-s_config-search ).

    IF tab IS INITIAL.
      DATA(startup_params) = client->get( )-t_comp_params.
      tab = VALUE #( startup_params[ n = 'table' ]-v OPTIONAL ).
    ENDIF.

    tab = to_upper( tab ).

    IF tab IS INITIAL.
      mv_table = 'USR01'. " FALLBACK
    ELSE.
      mv_table = tab.
    ENDIF.

  ENDMETHOD.

  METHOD render_main.
    DATA(page) = render_main_head( ).

    IF mv_multi_edit = abap_false.

      render_table( page ).

    ELSE.

      render_ui_table( page ).

    ENDIF.

    render_main_footer( page ).

  ENDMETHOD.

  METHOD render_table.

    DATA(table) = page->table( growing          = 'true'
                               growingthreshold = '100'
                               width            = 'auto'
                               autoPopinMode    = abap_true
                               items            = client->_bind_edit( val = mt_table->* )
                               headertext       = mv_table  ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(headder) = table->header_toolbar(
               )->overflow_toolbar(
                 )->title( text = mv_table
                 )->toolbar_spacer(
                 )->search_field( value  = client->_bind_edit( mv_search_value )
                                  search = client->_event( 'BUTTON_SEARCH' )
                                  change = client->_event( 'BUTTON_SEARCH' )
                                  id     = `SEARCH`
                                  width  = '17.5rem' ).

    headder = z2ui5_cl_pop_display_layout=>render_layout_function( xml    = headder
                                                              client = client ).

    DATA(columns) = table->columns( ).

    LOOP AT ms_layout-t_layout REFERENCE INTO DATA(layout).
      DATA(lv_index) = sy-tabix.

      columns->column( visible         = client->_bind( val       = layout->visible
                                                        tab       = ms_layout-t_layout
                                                        tab_index = lv_index )
                       halign          = client->_bind( val       = layout->halign
                                                        tab       = ms_layout-t_layout
                                                        tab_index = lv_index )
                       importance      = client->_bind( val       = layout->importance
                                                        tab       = ms_layout-t_layout
                                                        tab_index = lv_index )
                       mergeduplicates = client->_bind( val       = layout->merge
                                                        tab       = ms_layout-t_layout
                                                        tab_index = lv_index )
                       width           = client->_bind( val       = layout->width
                                                        tab       = ms_layout-t_layout
                                                        tab_index = lv_index )
       )->text( layout->tlabel ).

    ENDLOOP.

    DATA(cells) = columns->get_parent( )->items(
                                       )->column_list_item(
                                           valign = 'Middle'
                                           type   = 'Navigation'
*                                           type   = 'Active'
                                           press  = client->_event( val   = 'ROW_SELECT'
                                                                    t_arg = VALUE #( ( `${ROW_ID}`  ) ) )
                                       )->cells( ).

    LOOP AT ms_layout-t_layout REFERENCE INTO layout.

      IF layout->t_sub_col IS NOT INITIAL.

        DATA(sub_col) = ``.
        DATA(index) = 0.

        LOOP AT layout->t_sub_col INTO DATA(subcol).

          index = index + 1.

          READ TABLE ms_layout-t_layout INTO DATA(line) WITH KEY fname = subcol-fname.

          IF line-reference_field IS INITIAL.
            DATA(Column) = |{ line-tlabel }: \{{ subcol-fname }\}|.
          ELSE.
            column = |{ line-tlabel }: \{{ subcol-fname }\} \{{ line-reference_field }\}|.
          ENDIF.

          IF index = 1.
            sub_col = column.
          ELSE.
            sub_col = |{ sub_col }{ cl_abap_char_utilities=>cr_lf }{ column }|.
          ENDIF.
        ENDLOOP.

        IF layout->reference_field IS NOT INITIAL.
          cells->object_identifier( title = |\{{ layout->fname }\} \{{ layout->reference_field }\}|
                                    text  = sub_col ).
        ELSE.
          cells->object_identifier( title = |\{{ layout->fname }\}|
                                    text  = sub_col ).
        ENDIF.

      ELSE.
        IF layout->reference_field IS NOT INITIAL.
          cells->object_identifier( text = |\{{ layout->fname }\} \{{ layout->reference_field }\}| ).
        ELSE.
          cells->object_identifier( text = |\{{ layout->fname }\}| ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD render_main_head.


      DATA(view) = z2ui5_cl_xml_view=>factory( ). "->shell( ).

      IF client->get( )-check_launchpad_active = abap_true.

        result = view->page( showheader = abap_false  ).
        result->_z2ui5( )->lp_title( mv_table  ).

      ELSE.
        result = view->page( title         = mv_table
                             shownavbutton = abap_false ).
      ENDIF.


    result->header_content( )->scroll_container( height   = '70%'
                                                 vertical = abap_true ).
  ENDMETHOD.

  METHOD render_main_footer.
    DATA(footer) = page->footer( )->overflow_toolbar(
                     )->toolbar_spacer( ).

    IF mv_multi_edit = abap_true.
      footer->button( text  = 'Delete'
                      type  = 'Reject'
                      icon  = 'sap-icon://delete'
                      press = client->_event( val = 'BUTTON_DELETE' )
           )->button( text  = 'Copy'
                      icon  = 'sap-icon://copy'
                      press = client->_event( val = 'BUTTON_COPY' ) ).
    ENDIF.

    footer->button( icon  = 'sap-icon://add'
                    text  = 'Add'
                    press = client->_event( 'BUTTON_ADD' )
                    type  = 'Default'
         )->button( icon  = 'sap-icon://refresh'
                    text  = 'Refresh'
                    press = client->_event( 'BUTTON_REFRESH' )
                    type  = 'Default'
         )->button( enabled = client->_bind( mv_change_active )
                    text    = 'Save'
                    press   = client->_event( 'BUTTON_SAVE' )
                    type    = 'Success' ).

    footer->menu_button( activeicon = 'sap-icon://action-settings'
       )->_generic( `menu`
          )->_generic( `Menu`
             )->menu_item( icon  = 'sap-icon://shipping-status'
                           text  = 'Transport all'
                           press = client->_event( 'TRANSPORT_ALL' )
             )->menu_item( icon  = 'sap-icon://shipping-status'
                           text  = 'Change Transportrequest'
                           press = client->_event( 'TRANSPORT_CHANGE' )
             ).
*             ->menu_item( icon  = 'sap-icon://key-user-settings'
*                           text  = 'Admin Mode'
*                           press = client->_event( 'BUTTON_EDIT' ) ).

      client->view_display( page->stringify( ) ).

  ENDMETHOD.

  METHOD row_select.

    DATA(lt_arg) = client->get( )-t_event_arg.
    READ TABLE lt_arg INTO DATA(ls_arg) INDEX 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    client->nav_app_call( ZSM30_cl_app_001a=>factory( io_table     = mt_table
                                                           iv_row_id    = ls_arg
                                                           it_dfies     = mt_dfies
                                                           is_layout    = ms_layout
                                                           iv_edit_mode = abap_true
                                                           iv_tabname   = mv_table ) ).

  ENDMETHOD.

  METHOD search.
    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp> TYPE STANDARD TABLE.

    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_tmp->* TO <tab_tmp>.

    IF <tab_tmp> IS NOT INITIAL.
      <tab> = <tab_tmp>.
    ENDIF.

    IF mv_search_value IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<f_row>).
      DATA(lv_row) = ``.
      DATA(lv_index) = 1.
      DO.
        ASSIGN COMPONENT lv_index OF STRUCTURE <f_row> TO FIELD-SYMBOL(<field>).
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_row = lv_row && <field>.
        lv_index += 1.
      ENDDO.

      IF lv_row NS mv_search_value.
        DELETE <tab>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_row_id.
    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.

    ASSIGN mt_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO FIELD-SYMBOL(<row>).
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      on_init( ).

      render_main( ).

    ENDIF.

    on_after_popup( ).

    on_after_layout( ).

    on_after_transport( ).

    on_event( ).

  ENDMETHOD.

  METHOD on_event_main.
    CASE client->get( )-event.

      WHEN 'BACK'.

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'BUTTON_REFRESH'.

        get_data( ).
        view_model_update( ).

      WHEN 'BUTTON_SEARCH'.

        search( ).
        view_model_update( ).

      WHEN 'BUTTON_SAVE'.

        button_save( ).

      WHEN 'BUTTON_DELETE'.

        button_delete( ).

      WHEN 'BUTTON_ADD'.

        client->nav_app_call( ZSM30_cl_app_001a=>factory( io_table     = mt_table
                                                               iv_row_id    = ``
                                                               it_dfies     = mt_dfies
                                                               is_layout    = ms_layout
                                                               iv_edit_mode = abap_false
                                                               iv_tabname   = mv_table ) ).

      WHEN 'BUTTON_EDIT'.

        mv_multi_edit = COND #( WHEN mv_multi_edit = abap_false THEN abap_true ELSE abap_false ).
        DATA(selkz) = REF #( ms_layout-t_layout[ fname = 'SELKZ' ]-visible OPTIONAL ).
        selkz->* = COND #( WHEN mv_multi_edit = abap_true THEN abap_true ELSE abap_false ).

        get_layout( ).

        render_main( ).

      WHEN 'ROW_SELECT'.

        row_select( ).

      WHEN 'INPUT'.

        check_input( ).

      WHEN 'BUTTON_COPY'.

        button_copy( ).

    ENDCASE.
  ENDMETHOD.


  METHOD on_event_layout.
    client = z2ui5_cl_pop_display_layout=>on_event_layout( client = client
                                                      layout = ms_layout ).
  ENDMETHOD.

  METHOD on_after_layout.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.

        DATA(app) = CAST z2ui5_cl_pop_display_layout( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        ms_layout = app->ms_layout.

        IF app->mv_rerender = abap_true.
          " subcolumns need rerendering to work ..
          render_main( ).
        ELSE.

          view_model_update( ).

        ENDIF.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD get_layout.

    DATA(class) = cl_abap_classdescr=>get_class_name( me ).
    SHIFT class LEFT DELETING LEADING '\CLASS='.

*    DATA(lgnum) = ZSM30_cl_parameter_helper=>get_parameter_id( '/SCWM/LGN'  ).

    IF mv_multi_edit = abap_false.
      DATA(control) = z2ui5_cl_pop_display_layout=>m_table.
    ELSE.
      control = z2ui5_cl_pop_display_layout=>ui_table.
    ENDIF.
    ms_layout = z2ui5_cl_pop_display_layout=>init_layout( control  = control
                                                     data     = mt_table
                                                     handle01 = CONV #( class )
                                                     handle02 = CONV #( mv_table )
                                                     handle03 = CONV #( 'SIMPLE_VIEW' ) ).

  ENDMETHOD.

  METHOD on_after_transport.
    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " War es das Layout?
        DATA(app) = CAST z2ui5_cl_popup_show_tr( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        ms_transport = app->ms_transport.

        IF ms_transport IS NOT INITIAL.
          CASE mv_ucomm_tmp.
            WHEN 'TRANSPORT_ALL'.
              transport_all( ).
            WHEN 'TRANSPORT_CHANGE'.

              client->message_toast_display( |{ ms_transport-task } selected. |  ).

              CLEAR mv_ucomm_tmp.

            WHEN 'BUTTON_SAVE'.
              button_save( ).
          ENDCASE.
        ENDIF.

        view_model_update( ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD on_event_transport.

    CASE client->get( )-event.
      WHEN 'TRANSPORT_ALL'.

        transport_all( ).

      WHEN 'TRANSPORT_CHANGE'.

        mv_ucomm_tmp = client->get( )-event.
        client->nav_app_call( z2ui5_cl_popup_show_tr=>factory( ) ).

    ENDCASE.

  ENDMETHOD.

  METHOD render_ui_table.

    DATA(table) = page->flex_box( height = '85vh' )->ui_table(
                                                      alternaterowcolors  = 'true'
                                                      visiblerowcountmode = 'Auto'
*                                                      fixedrowcount       = '1'
                                                      selectionmode       = 'None'
                                                      SelectionBehavior   = 'RowSelector'
                                                      rows                = client->_bind_edit( val = mt_table->* ) ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(toolbar) = table->ui_extension( )->overflow_toolbar( )->toolbar_spacer( ).

    toolbar = z2ui5_cl_pop_display_layout=>render_layout_function( xml    = toolbar
                                                              client = client ).

    DATA(columns) = table->ui_columns( ).

    LOOP AT ms_layout-t_layout REFERENCE INTO DATA(layout).
      DATA(lv_index) = sy-tabix.

      DATA(col) = columns->ui_column( visible        = client->_bind( val       = layout->visible
                                                                      tab       = ms_layout-t_layout
                                                                      tab_index = lv_index )
                                      halign         = client->_bind( val       = layout->halign
                                                                      tab       = ms_layout-t_layout
                                                                      tab_index = lv_index )
                                      width          = COND #( WHEN layout->width IS NOT INITIAL
                                                               THEN client->_bind( val       = layout->width
                                                                                   tab       = ms_layout-t_layout
                                                                                   tab_index = lv_index ) )

                                      sortproperty   = layout->fname
                                      filterproperty = layout->fname
                              )->text( layout->tlabel )->ui_template( ).

      IF layout->fname = 'SELKZ'.

        col->checkbox( selected = |\{{ layout->fname }\}| ).

      ELSE.

        col->input( submit = client->_event( val = 'INPUT' )
                    value  = |\{{ layout->fname }\}| ).

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD button_delete.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del> TYPE STANDARD TABLE.

    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_del->* TO <del>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<line>).

      ASSIGN COMPONENT 'SELKZ' OF STRUCTURE <line> TO FIELD-SYMBOL(<selkz>).

      IF <selkz> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      IF <selkz> = abap_false.
        CONTINUE.
      ENDIF.

      APPEND <line> TO <del>.
      DELETE <tab>.

      view_model_update( ).
      mv_change_active = abap_true.

      UNASSIGN <selkz>.

    ENDLOOP.

    set_row_id( ).

  ENDMETHOD.

  METHOD check_input.

    CLEAR mv_key_error.

    IF mt_table_tmp->* <> mt_table->*.
      mv_change_active = abap_true.
    ELSE.
      mv_change_active = abap_false.
    ENDIF.

    view_model_update( ).

  ENDMETHOD.

  METHOD button_copy.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    ASSIGN mt_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING FIELD-SYMBOL(<line>).

      ASSIGN COMPONENT 'SELKZ' OF STRUCTURE <line> TO FIELD-SYMBOL(<selkz>).

      IF <selkz> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      IF <selkz> = abap_true.
        <selkz> = abap_false.
        APPEND <line> TO <tab>.

        mv_change_active = abap_true.

        view_model_update( ).

      ENDIF.

    ENDLOOP.

    set_row_id( ).

  ENDMETHOD.

  METHOD view_model_update.

         client->view_model_update( ).

  ENDMETHOD.

  METHOD on_after_popup.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        DATA(app) = CAST ZSM30_cl_app_001a( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        IF app->mt_data->* <> mt_table->*.
          mv_change_active = abap_true.
        ENDIF.

        " ROW Deleted?

        mt_table->* = app->mt_data->*.

        view_model_update( ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
