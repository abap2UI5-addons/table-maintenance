CLASS z2ui5_cl_tm_001 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    INTERFACES z2ui5_if_tm_001.

    ALIASES mo_parent_view       FOR z2ui5_if_tm_001~mo_parent_view.
    ALIASES mv_view_display      FOR z2ui5_if_tm_001~mv_view_display.
    ALIASES mv_view_model_update FOR z2ui5_if_tm_001~mv_view_model_update.
    ALIASES mv_change_active     FOR z2ui5_if_tm_001~mv_change_active.
    ALIASES set_main_app_data    FOR z2ui5_if_tm_001~set_app_data.
    ALIASES ms_transport         FOR z2ui5_if_tm_001~ms_transport.

    DATA mo_layout       TYPE REF TO z2ui5_cl_layo_manager.
    DATA mv_search_value TYPE string.
    DATA mt_table        TYPE REF TO data.
    DATA mt_table_tmp    TYPE REF TO data.
    DATA mt_table_del    TYPE REF TO data.
    DATA mt_table_org    TYPE REF TO data.
    " the table name typed in when the app was started without one
    DATA mv_table_input  TYPE string.
    " the table input is shown until a table is opened - embedded, it stays
    " in the embedder's page, which the builder cannot remove it from
    DATA mv_check_table_input TYPE abap_bool.

    CONSTANTS mc_row_id TYPE string VALUE `ROW_ID` ##NO_TEXT.

  PROTECTED SECTION.
    TYPES:
      BEGIN OF ty_s_keys,
        row_id TYPE string,
        key    TYPE string,
      END OF ty_s_keys.
    TYPES ty_t_keys TYPE STANDARD TABLE OF ty_s_keys WITH DEFAULT KEY.

    DATA mv_table          TYPE string.
    DATA mt_dfies          TYPE z2ui5_cl_tm_context=>ty_t_dfies.
    DATA client            TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.
    DATA mv_f4_fname       TYPE string.
    DATA mv_shlpfield      TYPE string.
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
        !type         TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS button_save.

    "! MOVE-CORRESPONDING between two tables, row by row - the table form needs
    "! 7.40 SP05 and has no 7.02 downport
    METHODS copy_corresponding
      IMPORTING
        it_source TYPE STANDARD TABLE
      CHANGING
        ct_target TYPE STANDARD TABLE.

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
        !page TYPE REF TO z2ui5_cl_ui5_view_builder.

    METHODS render_main_head
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_ui5_view_builder.

    METHODS on_event_main.

    METHODS on_event_layout.

    METHODS get_table_name.

    METHODS render_table_input.

    METHODS on_event_table_input.

    METHODS on_after_layout.

    METHODS get_layout.

    METHODS on_after_transport.

    METHODS transport_all.

    METHODS on_event_transport.

    METHODS render_ui_table
      IMPORTING
        !page TYPE REF TO z2ui5_cl_ui5_view_builder.

    METHODS render_table
      IMPORTING
        !page TYPE REF TO z2ui5_cl_ui5_view_builder.

    METHODS button_delete.
    METHODS check_input.

    METHODS button_copy.
    METHODS view_model_update.
    METHODS on_after_popup.

  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_cl_tm_001 IMPLEMENTATION.

  METHOD get_txt.

    DATA texts TYPE z2ui5_cl_tm_context=>ty_s_data_element_text.
    texts = z2ui5_cl_tm_context=>rtti_get_data_element_texts( roll ).

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
    FIELD-SYMBOLS <org>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del_org> TYPE STANDARD TABLE.
      DATA keys TYPE z2ui5_cl_tm_001=>ty_t_keys.
      DATA keys_tmp TYPE z2ui5_cl_tm_001=>ty_t_keys.
      DATA key_tmp LIKE LINE OF keys_tmp.
        DATA temp1 LIKE sy-subrc.
        DATA temp2 LIKE sy-subrc.
      FIELD-SYMBOLS <line> TYPE ANY.
        FIELD-SYMBOLS <id> TYPE any.
        DATA temp3 LIKE sy-subrc.
        DATA temp4 LIKE sy-subrc.
      DATA comp TYPE abap_component_tab.
          DATA struct_desc TYPE REF TO cl_abap_structdescr.
          DATA table_desc TYPE REF TO cl_abap_tabledescr.
          DATA ls_msg TYPE z2ui5_cl_tm_context=>ty_s_msg.
          DATA temp5 LIKE sy-subrc.
          DATA temp6 LIKE sy-subrc.
          DATA lx_db TYPE REF TO cx_root.

    IF ms_transport IS INITIAL.

      mv_ucomm_tmp = client->get( )-event.
      client->nav_app_call( z2ui5_cl_popup_show_tr=>factory( ) ).

    ELSE.

      CLEAR mv_ucomm_tmp.

      IF mv_multi_edit = abap_true.
        ASSIGN mt_table->* TO <tab>.
      ELSE.
        ASSIGN mt_table_tmp->* TO <tab>. " Filter issue
      ENDIF.
      ASSIGN mt_table_del->* TO <del>.
      ASSIGN mt_table_org->* TO <org>.

      " the delete buffer is rebuilt from <org> on every save - a save that
      " stopped early (duplicate key, transport error) left its rows in it, and
      " the next save appended them a second time
      CLEAR <del>.

      " Compare all Key entries with the TMP. All entries that are not present are deleted.

      keys = get_keys( <tab> ).

      keys_tmp = get_keys( <org> ).


      LOOP AT keys_tmp INTO key_tmp.


        temp2 = sy-subrc.
        READ TABLE keys WITH KEY key = key_tmp-key TRANSPORTING NO FIELDS.
        temp1 = sy-subrc.
        sy-subrc = temp2.
        IF temp1 = 0.
          DELETE keys_tmp USING KEY loop_key.
        ENDIF.
      ENDLOOP.


      LOOP AT <org> ASSIGNING <line>.

        UNASSIGN <id>.
        ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <id>.


        temp4 = sy-subrc.
        READ TABLE keys_tmp WITH KEY row_id = <id> TRANSPORTING NO FIELDS.
        temp3 = sy-subrc.
        sy-subrc = temp4.
        IF temp3 = 0.
          APPEND <line> TO <del>.
        ENDIF.
      ENDLOOP.

      " two rows with the same key cannot be saved
      SORT keys BY key ASCENDING.
      keys_tmp = keys.
      DELETE ADJACENT DUPLICATES FROM keys COMPARING key.
      IF lines( keys ) <> lines( keys_tmp ).
        client->message_box_display( text = `Two or more entries have the same key.`
                                     type = `error` ).

        RETURN.
      ENDIF.


      comp = z2ui5_cl_tm_context=>rtti_get_t_attri_by_table_name( mv_table ).

      TRY.


          struct_desc = cl_abap_structdescr=>create( comp ).


          table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA t_table     TYPE HANDLE table_desc.
          CREATE DATA t_table_del TYPE HANDLE table_desc.

          ASSIGN t_table->* TO <tab_org>.
          ASSIGN t_table_del->* TO <del_org>.

          copy_corresponding( EXPORTING it_source = <del>
                              CHANGING  ct_target = <del_org> ).
          copy_corresponding( EXPORTING it_source = <tab>
                              CHANGING  ct_target = <tab_org> ).

        CATCH cx_root ##NO_HANDLER.

      ENDTRY.

      TRY.

          z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = t_table
                                                      iv_tabname   = mv_table
                                                      is_transport = ms_transport ).

          z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = t_table_del
                                                      iv_tabname   = mv_table
                                                      is_transport = ms_transport ).

        CATCH cx_root.


          ls_msg = z2ui5_cl_tm_context=>msg_get_by_msg( id = '42'
                                                        no = `316` ).
          " MESSAGE e316(42) into DATA(msg).  " Transport request error
          client->message_toast_display( ls_msg-text ).
          RETURN.
      ENDTRY.

      TRY.

          " deletions and writes are one LUW, committed once both went through:
          " a deletion committed on its own could not be rolled back when the
          " write after it failed
          IF <del> IS NOT INITIAL.
            " sy-subrc 4 only means a row was no longer in the database
            DELETE (mv_table) FROM TABLE <del_org>.
          ENDIF.



          temp6 = sy-subrc.
          READ TABLE mt_dfies WITH KEY keyflag = abap_false TRANSPORTING NO FIELDS.
          temp5 = sy-subrc.
          sy-subrc = temp6.
          IF temp5 = 0.
            MODIFY (mv_table) FROM TABLE <tab_org>.
          ELSE.
            DELETE (mv_table) FROM TABLE <tab_org>.
            INSERT (mv_table) FROM TABLE <tab_org>.
          ENDIF.

          IF sy-subrc = 0.
            COMMIT WORK AND WAIT.

            " what was saved is the database state the next save compares
            " against - with the state of the first read, a row added, saved,
            " deleted and saved again was never deleted
            <org> = <tab>.
            CLEAR <del>.

            ls_msg = z2ui5_cl_tm_context=>msg_get_by_msg( id = 'i18n'
                                                    no = `002` ).
            " MESSAGE s002(i18n) INTO msg.
            client->message_toast_display( ls_msg-text ).

            CLEAR mv_change_active.
          ELSE.
            " not every row could be written - undone instead of left in
            " the LUW for the next commit to write half of it
            ROLLBACK WORK.
            client->message_box_display( text = |Not every row could be written to { mv_table } - the changes were not saved|
                                         type = `error` ).
          ENDIF.


        CATCH cx_root INTO lx_db.
          " a failed write (a duplicate key on INSERT, a value the column
          " does not take) used to end here without a word, the rows
          " deleted just before still waiting in the LUW
          ROLLBACK WORK.
          client->message_box_display( text = lx_db->get_text( )
                                       type = `error` ).
      ENDTRY.

    ENDIF.

    view_model_update( ).

  ENDMETHOD.

  METHOD get_keys.

    FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <id> TYPE any.
      DATA temp7 TYPE ty_s_keys.
      DATA keys LIKE temp7.
      DATA value TYPE string.
      DATA dfies LIKE LINE OF mt_dfies.
        FIELD-SYMBOLS <key> TYPE any.

    LOOP AT table ASSIGNING <line>.


      UNASSIGN <id>.
      ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <id>.


      CLEAR temp7.
      temp7-row_id = <id>.

      keys = temp7.

      value = ``.

      " the client is left out: the rows read are all of the logon client, a row
      " added in the popup has an empty one, and the database writes both to the
      " logon client - with it, an added row never matched the row it overwrites

      LOOP AT mt_dfies INTO dfies WHERE keyflag = abap_true AND datatype <> 'CLNT' AND fieldname <> 'MANDT'.


        ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <line> TO <key>.

        " each value with its length in front: plain concatenation made
        " A + BC and AB + C the same key and reported a duplicate that
        " was none
        value = |{ <key> }|.
        keys-key = |{ keys-key }{ strlen( value ) }:{ value }|.

      ENDLOOP.

      APPEND keys TO result.
      CLEAR keys.

    ENDLOOP.

  ENDMETHOD.

  METHOD transport_all.

    DATA t_table     TYPE REF TO data.
    DATA t_table_del TYPE REF TO data.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del_org> TYPE STANDARD TABLE.
      DATA comp TYPE abap_component_tab.
          DATA struct_desc TYPE REF TO cl_abap_structdescr.
          DATA table_desc TYPE REF TO cl_abap_tabledescr.
          DATA ls_msg TYPE z2ui5_cl_tm_context=>ty_s_msg.

    IF ms_transport IS INITIAL.

      mv_ucomm_tmp = client->get( )-event.
      client->nav_app_call( z2ui5_cl_popup_show_tr=>factory( ) ).

    ELSE.

      IF ms_transport-locl = abap_true.
        RETURN.
      ENDIF.

      CLEAR mv_ucomm_tmp.

      " outside multi-edit a search leaves only its hits in mt_table - all
      " rows are in mt_table_tmp, which is what the save writes as well
      IF mv_multi_edit = abap_true.
        ASSIGN mt_table->* TO <tab>.
      ELSE.
        ASSIGN mt_table_tmp->* TO <tab>.
      ENDIF.
      ASSIGN mt_table_del->* TO <del>.


      comp = z2ui5_cl_tm_context=>rtti_get_t_attri_by_table_name( mv_table ).

      TRY.


          struct_desc = cl_abap_structdescr=>create( comp ).


          table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA t_table     TYPE HANDLE table_desc.
          CREATE DATA t_table_del TYPE HANDLE table_desc.

          ASSIGN t_table->* TO <tab_org>.
          ASSIGN t_table_del->* TO <del_org>.

          copy_corresponding( EXPORTING it_source = <del>
                              CHANGING  ct_target = <del_org> ).
          copy_corresponding( EXPORTING it_source = <tab>
                              CHANGING  ct_target = <tab_org> ).

        CATCH cx_root ##NO_HANDLER.

      ENDTRY.
      TRY.
          z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = t_table
                                                      iv_tabname   = mv_table
                                                      is_transport = ms_transport ).

          z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = t_table_del
                                                      iv_tabname   = mv_table
                                                      is_transport = ms_transport ).

        CATCH cx_root.

          ls_msg = z2ui5_cl_tm_context=>msg_get_by_msg( id = '42'
                                                        no = `316` ).

          " MESSAGE e316(42) INTO DATA(msg).  " Transport request error
          client->message_toast_display( ls_msg-text ).
          RETURN.
      ENDTRY.
      ls_msg = z2ui5_cl_tm_context=>msg_get_by_msg( id = 'rmps_general'
                                              no = `215` ).
      " MESSAGE s215(rmps_general) WITH ms_transport-transport INTO msg. "Transporteintrag erfolgreich geschrieben (&1)
      client->message_toast_display( ls_msg-text ).

    ENDIF.
  ENDMETHOD.

  METHOD get_comp.
    DATA id    TYPE c LENGTH 32.
    DATA selkz TYPE abap_bool.
        DATA comp TYPE abap_component_tab.
        DATA temp8 LIKE sy-subrc.
        DATA temp3 TYPE xsdboolean.
          DATA temp9 TYPE cl_abap_structdescr=>component_table.
          DATA temp10 LIKE LINE OF temp9.
          DATA temp1 TYPE REF TO cl_abap_datadescr.
        DATA temp11 LIKE sy-subrc.
        DATA temp4 TYPE xsdboolean.
          DATA temp12 TYPE cl_abap_structdescr=>component_table.
          DATA temp13 LIKE LINE OF temp12.
          DATA temp2 TYPE REF TO cl_abap_datadescr.

    TRY.


        comp = z2ui5_cl_tm_context=>rtti_get_t_attri_by_table_name( mv_table ).


        READ TABLE comp WITH KEY name = mc_row_id TRANSPORTING NO FIELDS.
        temp8 = sy-subrc.

        temp3 = boolc( temp8 = 0 ).
        IF temp3 = abap_false.

          CLEAR temp9.

          temp10-name = mc_row_id.

          temp1 ?= cl_abap_datadescr=>describe_by_data( id ).
          temp10-type = temp1.
          INSERT temp10 INTO TABLE temp9.
          APPEND LINES OF temp9 TO result.
        ENDIF.

        READ TABLE comp WITH KEY name = 'SELKZ' TRANSPORTING NO FIELDS.
        temp11 = sy-subrc.

        temp4 = boolc( temp11 = 0 ).
        IF temp4 = abap_false.

          CLEAR temp12.

          temp13-name = 'SELKZ'.

          temp2 ?= cl_abap_datadescr=>describe_by_data( selkz ).
          temp13-type = temp2.
          INSERT temp13 INTO TABLE temp12.
          APPEND LINES OF temp12 TO result.

        ENDIF.

        APPEND LINES OF comp TO result.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD get_data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    DATA t_comp TYPE abap_component_tab.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <mt_table_tmp> TYPE data.
    FIELD-SYMBOLS <mt_table_org> TYPE data.
    t_comp = get_comp( ).
    TRY.


        new_struct_desc = cl_abap_structdescr=>create( t_comp ).


        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mt_table     TYPE HANDLE new_table_desc.
        CREATE DATA mt_table_del TYPE HANDLE new_table_desc.
        CREATE DATA mt_table_tmp TYPE HANDLE new_table_desc.
        CREATE DATA mt_table_org TYPE HANDLE new_table_desc.

        ASSIGN mt_table->* TO <table>.

        SELECT *
          FROM (mv_table)
          INTO CORRESPONDING FIELDS OF TABLE <table>.

        SORT <table>.

      CATCH cx_root ##NO_HANDLER.

    ENDTRY.

    set_row_id( ).


    ASSIGN mt_table_tmp->* TO <mt_table_tmp>.

    ASSIGN mt_table_org->* TO <mt_table_org>.

    <mt_table_tmp> = <table>.  " For Filter etc.
    <mt_table_org> = <table>.  " For DB alignment bevore Save
  ENDMETHOD.

  METHOD get_dfies.
    DATA temp14 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp14.

    mt_dfies = z2ui5_cl_tm_context=>rtti_get_t_dfies_by_table_name( mv_table ).

    " Check if the DB for the F4 Help is released


    LOOP AT mt_dfies REFERENCE INTO dfies WHERE checktable IS NOT INITIAL.
      TRY.
          " type_not_found is a classic exception - the functional call
          " form dumps on it instead of reaching the CATCH
          cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = dfies->checktable
                                               EXCEPTIONS type_not_found = 1
                                                          OTHERS         = 2 ).
          IF sy-subrc <> 0.
            CLEAR dfies->checktable.
          ENDIF.
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
    DATA tab TYPE string.
      DATA startup_params TYPE z2ui5_if_client=>ty_t_name_value.
      DATA temp15 TYPE string.
      DATA temp16 TYPE z2ui5_if_client=>ty_s_name_value.
    DATA descr TYPE REF TO cl_abap_typedescr.

    IF mv_table IS NOT INITIAL.
      RETURN.
    ENDIF.

    " TABLE per Parameter?

    tab = z2ui5_cl_tm_context=>url_param_get( val = 'table'
                                              url = client->get( )-s_config-search ).

    IF tab IS INITIAL.

      startup_params = client->get( )-t_comp_params.

      CLEAR temp15.

      READ TABLE startup_params INTO temp16 WITH KEY n = 'table'.
      IF sy-subrc = 0.
        temp15 = temp16-v.
      ENDIF.
      tab = temp15.
    ENDIF.

    " no fallback table: without the parameter the app asks for one
    " (render_table_input) instead of opening a table nobody chose
    mv_table = to_upper( tab ).
    IF mv_table IS INITIAL.
      RETURN.
    ENDIF.

    " a name that is no table left get_data without a table to fill - a
    " short dump; the app asks for the table instead, as without the parameter

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = mv_table
                                         RECEIVING  p_descr_ref    = descr
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 2 ).
    IF sy-subrc <> 0 OR descr->kind <> cl_abap_typedescr=>kind_struct.
      client->message_toast_display( |Table { mv_table } does not exist.| ).
      mv_table_input = mv_table.
      CLEAR mv_table.
    ENDIF.

  ENDMETHOD.

  METHOD render_main.

    DATA page TYPE REF TO z2ui5_cl_ui5_view_builder.
    page = render_main_head( ).

    IF mv_multi_edit = abap_false.

      render_table( page ).

    ELSE.

      render_ui_table( page ).

    ENDIF.

    render_main_footer( page ).

  ENDMETHOD.

  METHOD render_table.

    FIELD-SYMBOLS <tab> TYPE data.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA headder TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp17 LIKE LINE OF mo_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp17.
      DATA lv_index LIKE sy-tabix.
    DATA cells TYPE REF TO z2ui5_cl_ui5_view_builder.
        DATA sub_col TYPE string.
        DATA index TYPE i.
        DATA subcol LIKE LINE OF layout->t_sub_col.
          DATA line TYPE z2ui5_cl_layo_manager=>ty_s_positions.
            DATA column TYPE string.
    ASSIGN mt_table->* TO <tab>.


    table = page->ele( `Table`
                      )->a( n = `growing` v = 'true'
                      )->a( n = `growingThreshold` v = '100'
                      )->a( n = `width` v = 'auto'
                      )->a( n = `sticky` v = `ColumnHeaders`
                      )->a( n = `autoPopinMode` b = abap_true
                      )->a( n = `items` v = client->_bind_edit( <tab> )
                      )->a( n = `headerText` t = mv_table ).


    headder = table->ele( `headerToolbar`
                        )->ele( `OverflowToolbar`
                        )->tag( `Title`
                        )->a( n = `text` t = mv_table
                        )->tag( `ToolbarSpacer`
                        )->tag( `SearchField`
                        )->a( n = `value` v = client->_bind_edit( mv_search_value )
                        )->a( n = `search` v = client->_event( 'BUTTON_SEARCH' )
                        )->a( n = `change` v = client->_event( 'BUTTON_SEARCH' )
                        )->a( n = `id` v = `SEARCH`
                        )->a( n = `width` v = '17.5rem' ).

    headder->tag( `Button`
        )->a( n = `icon` v = 'sap-icon://action-settings'
        )->a( n = `press` v = client->_event( val = mo_layout->ms_layout-s_head-guid ) ).


    columns = table->ele( `columns` ).



    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      lv_index = sy-tabix.

      columns->ele( `Column`
          )->a( n = `visible` v = client->_bind( val       = layout->visible
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->a( n = `mergeDuplicates` v = client->_bind( val       = layout->merge
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->a( n = `width` v = client->_bind( val       = layout->width
                                                        tab       = mo_layout->ms_layout-t_layout
                                                        tab_index = lv_index )
          )->tag( `Text`
          )->a( n = `text` v = layout->tlabel ).

    ENDLOOP.


    cells = columns->end(
                      )->ele( `items`
                      )->ele( `ColumnListItem`
                      )->a( n = `vAlign` v = 'Middle'
                      )->a( n = `type` v = 'Navigation'
                      )->a( n = `press` v = client->_event( val   = 'ROW_SELECT'
                                                                    arg   = `${ROW_ID}` )
                      )->ele( `cells` ).

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      IF layout->t_sub_col IS NOT INITIAL.


        sub_col = ``.

        index = 0.


        LOOP AT layout->t_sub_col INTO subcol.

          index = index + 1.


          READ TABLE mo_layout->ms_layout-t_layout INTO line WITH KEY fname = subcol-fname.

          IF line-reference_field IS INITIAL.

            column = |{ line-tlabel }: \{{ subcol-fname }\}|.
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
          cells->ele( `ObjectIdentifier`
              )->a( n = `title` v = |\{{ layout->fname }\} \{{ layout->reference_field }\}|
              " abap2ui5lint-disable-next-line unescaped-text-in-attribute -- sub_col is a composite binding ({FIELD} per sub column), not literal text
              )->a( n = `text` v = sub_col ).
        ELSE.
          cells->ele( `ObjectIdentifier`
              )->a( n = `title` v = |\{{ layout->fname }\}|
              )->a( n = `text` v = sub_col ).
        ENDIF.

      ELSEIF layout->reference_field IS NOT INITIAL.
        cells->ele( `ObjectIdentifier`
            )->a( n = `text` v = |\{{ layout->fname }\} \{{ layout->reference_field }\}| ).
      ELSE.
        cells->ele( `ObjectIdentifier`
            )->a( n = `text` v = |\{{ layout->fname }\}| ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD render_main_head.
      DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
        DATA temp18 TYPE string_table.
    IF mo_parent_view IS INITIAL.


      view = z2ui5_cl_ui5_view_builder=>factory(
                       )->ele( n = `View` ns = `mvc`
                       )->a( n = `xmlns` v = `sap.m`
                       )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                       )->a( n = `xmlns:core` v = `sap.ui.core`
                       )->a( n = `xmlns:table` v = `sap.ui.table`
                       )->a( n = `displayBlock` v = `true`
                       )->a( n = `height` v = `100%` ). "->shell( ).

      IF client->get( )-check_launchpad_active = abap_true.

        result = view->ele( `Page`
                     )->a( n = `showHeader` b = abap_false ).
        " inside the launchpad the shell header carries the title, set once
        " the response has been rendered

        CLEAR temp18.
        INSERT mv_table INTO TABLE temp18.
        client->follow_up_action(
            val   = z2ui5_if_client=>cs_event-set_title_launchpad
            t_arg = temp18 ).

      ELSE.
        result = view->ele( `Page`
                     )->a( n = `title` t = mv_table
                     )->a( n = `showNavButton` b = abap_false ).
      ENDIF.

    ELSE.

      " embedded in a view cluster: the embedder hands over the node this app
      " renders into. The frozen builder let us search the tree for the `Page`
      " by name; the generic builder has no such lookup, so the node itself is
      " what mo_parent_view carries now
      result = mo_parent_view.

    ENDIF.

    result->ele( `headerContent`
        )->ele( `ScrollContainer`
        )->a( n = `height` v = '70%'
        )->a( n = `vertical` b = abap_true ).
  ENDMETHOD.

  METHOD render_main_footer.
    DATA footer TYPE REF TO z2ui5_cl_ui5_view_builder.
    footer = page->ele( `footer`
                       )->ele( `OverflowToolbar`
                       )->tag( `ToolbarSpacer` ).

    IF mv_multi_edit = abap_true.
      footer->tag( `Button`
          )->a( n = `text` v = get_txt( 'MLCCS_D_XDELETE' )
          )->a( n = `type` v = 'Reject'
          )->a( n = `icon` v = 'sap-icon://delete'
          )->a( n = `press` v = client->_event( val = 'BUTTON_DELETE' )
          )->tag( `Button`
          )->a( n = `text` v = get_txt( '/SCMTMS/COCO_COPY_IND' )
          )->a( n = `icon` v = 'sap-icon://copy'
          )->a( n = `press` v = client->_event( val = 'BUTTON_COPY' ) ).
    ENDIF.

    footer->tag( `Button`
        )->a( n = `icon` v = 'sap-icon://add'
        )->a( n = `text` v = get_txt( 'RSLPO_GUI_ADDPART' )
        )->a( n = `press` v = client->_event( 'BUTTON_ADD' )
        )->a( n = `type` v = 'Default'
        )->tag( `Button`
        )->a( n = `icon` v = 'sap-icon://refresh'
        )->a( n = `text` v = get_txt( 'REFRESH_F8' )
        )->a( n = `press` v = client->_event( 'BUTTON_REFRESH' )
        )->a( n = `type` v = 'Default'
        )->tag( `Button`
        )->a( n = `enabled` v = client->_bind( mv_change_active )
        )->a( n = `text` v = get_txt( 'SICHERN' )
        )->a( n = `press` v = client->_event( 'BUTTON_SAVE' )
        )->a( n = `type` v = 'Success' ).

    footer->ele( `MenuButton`
        )->a( n = `activeIcon` v = 'sap-icon://action-settings'
        )->ele( `menu`
        )->ele( `Menu`
        )->tag( `MenuItem`
        )->a( n = `icon`  v = 'sap-icon://shipping-status'
        )->a( n = `text`  v = get_txt( 'ALLES_FUB' )
        )->a( n = `press` v = client->_event( 'TRANSPORT_ALL' )
        )->tag( `MenuItem`
        )->a( n = `icon`  v = 'sap-icon://shipping-status'
        )->a( n = `text`  v = get_txt( 'FUNCCHANGE' )
        )->a( n = `press` v = client->_event( 'TRANSPORT_CHANGE' )
        )->tag( `MenuItem`
        )->a( n = `icon`  v = 'sap-icon://key-user-settings'
        )->a( n = `text`  v = get_txt( 'POWL_ADMIN_TY' )
        )->a( n = `press` v = client->_event( 'BUTTON_EDIT' ) ).

    IF mo_parent_view IS INITIAL.

      client->view_display( page->stringify( ) ).

    ELSE.

      mv_view_display = abap_true.

    ENDIF.
  ENDMETHOD.

  METHOD row_select.

    DATA lt_arg TYPE string_table.
    DATA ls_arg TYPE string.
    lt_arg = client->get( )-t_event_arg.

    READ TABLE lt_arg INTO ls_arg INDEX 1.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    client->nav_app_call( z2ui5_cl_tm_pop=>factory( io_table     = mt_table
                                                    iv_row_id    = ls_arg
                                                    it_dfies     = mt_dfies
                                                    is_layout    = mo_layout
                                                    iv_edit_mode = abap_true
                                                    iv_tabname   = mv_table ) ).

  ENDMETHOD.

  METHOD search.
    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <f_row> TYPE ANY.
      DATA lv_row TYPE string.
      DATA lv_index TYPE i.
        FIELD-SYMBOLS <field> TYPE any.

    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_tmp->* TO <tab_tmp>.

    IF <tab_tmp> IS NOT INITIAL.
      <tab> = <tab_tmp>.
    ENDIF.

    IF mv_search_value IS INITIAL.
      RETURN.
    ENDIF.


    LOOP AT <tab> ASSIGNING <f_row>.

      lv_row = ``.

      lv_index = 1.
      DO.

        ASSIGN COMPONENT lv_index OF STRUCTURE <f_row> TO <field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_row = lv_row && <field>.
        lv_index = lv_index + 1.
      ENDDO.

      IF lv_row NS mv_search_value.
        DELETE <tab>.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_row_id.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <row> TYPE any.

    ASSIGN mt_table->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <row>.
      ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <row>.
      IF <row> IS ASSIGNED.

        TRY.
            <row> = cl_system_uuid=>create_uuid_c32_static( ).
          CATCH cx_root ##NO_HANDLER.
        ENDTRY.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD z2ui5_if_app~main.
    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      get_table_name( ).
      IF mv_table IS INITIAL.
        render_table_input( ).
        RETURN.
      ENDIF.

      on_init( ).

      render_main( ).

    ENDIF.

    IF mv_table IS INITIAL.
      on_event_table_input( ).
      RETURN.
    ENDIF.

    on_after_popup( ).

    on_after_layout( ).

    on_after_transport( ).

    on_event( ).

  ENDMETHOD.

  METHOD on_event_main.
        DATA temp5 TYPE xsdboolean.
        FIELD-SYMBOLS <temp20> TYPE xsdboolean.
        FIELD-SYMBOLS <temp1> LIKE LINE OF mo_layout->ms_layout-t_layout.
        DATA temp2 LIKE sy-tabix.
DATA selkz LIKE REF TO <temp20>.
        DATA temp6 TYPE xsdboolean.
          FIELD-SYMBOLS <tab_edit> TYPE data.
          FIELD-SYMBOLS <tab_edit_tmp> TYPE data.
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

        client->nav_app_call( z2ui5_cl_tm_pop=>factory( io_table     = mt_table
                                                        iv_row_id    = ``
                                                        it_dfies     = mt_dfies
                                                        is_layout    = mo_layout
                                                        iv_edit_mode = abap_false
                                                        iv_tabname   = mv_table ) ).

      WHEN 'BUTTON_EDIT'.


        temp5 = boolc( mv_multi_edit = abap_false ).
        mv_multi_edit = temp5.



        temp2 = sy-tabix.
        READ TABLE mo_layout->ms_layout-t_layout WITH KEY fname = 'SELKZ' ASSIGNING <temp1>.
        sy-tabix = temp2.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        ASSIGN <temp1>-visible TO <temp20>.
IF sy-subrc <> 0.
  ASSERT 1 = 0.
ENDIF.

GET REFERENCE OF <temp20> INTO selkz.

        temp6 = boolc( mv_multi_edit = abap_true ).
        selkz->* = temp6.

        IF mv_multi_edit = abap_true.
          CLEAR mv_search_value.
          search( ).
        ELSE.
          " multi-edit changes mt_table only; normal mode saves and searches
          " from mt_table_tmp, which would drop those changes

          ASSIGN mt_table->* TO <tab_edit>.

          ASSIGN mt_table_tmp->* TO <tab_edit_tmp>.
          <tab_edit_tmp> = <tab_edit>.
        ENDIF.

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

  METHOD z2ui5_if_tm_001~set_app_data.
    mv_table = data.
  ENDMETHOD.

  METHOD on_event_layout.
    z2ui5_cl_layo_pop=>on_event_layout( client = client
                                        layout = mo_layout ).
  ENDMETHOD.

  METHOD on_after_layout.
        DATA temp21 TYPE REF TO z2ui5_cl_layo_pop.
        DATA app LIKE temp21.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.


        temp21 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp21.

        mo_layout = app->mo_layout.

        IF app->mv_rerender = abap_true.
          " subcolumns need rerendering to work ..
          render_main( ).
        ELSE.

          view_model_update( ).

        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD get_layout.

    DATA class TYPE abap_abstypename.
      DATA control LIKE z2ui5_cl_layo_manager=>m_table.
    class = cl_abap_classdescr=>get_class_name( me ).
    SHIFT class LEFT DELETING LEADING '\CLASS='.

    IF mv_multi_edit = abap_false.

      control = z2ui5_cl_layo_manager=>m_table.
    ELSE.
      control = z2ui5_cl_layo_manager=>ui_table.
    ENDIF.
    mo_layout = z2ui5_cl_layo_manager=>factory( control  = control
                                                data     = mt_table
                                                handle01 = class
                                                handle02 = mv_table
                                                handle03 = 'SIMPLE_VIEW'
                                                handle04 = `` ).

  ENDMETHOD.

  METHOD on_after_transport.
        DATA temp22 TYPE REF TO z2ui5_cl_popup_show_tr.
        DATA app LIKE temp22.
    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " War es das Layout?

        temp22 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp22.

        ms_transport = app->ms_transport.

        IF ms_transport IS NOT INITIAL.

          CASE mv_ucomm_tmp.
            WHEN 'TRANSPORT_ALL'.
              transport_all( ).
            WHEN 'TRANSPORT_CHANGE'.

              client->message_toast_display( ms_transport-task  ).
              CLEAR mv_ucomm_tmp.

            WHEN 'BUTTON_SAVE'.
              button_save( ).
          ENDCASE.

        ENDIF.

        view_model_update( ).

      CATCH cx_root ##NO_HANDLER.
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

    " Set Selkz active!
    FIELD-SYMBOLS <temp23> TYPE xsdboolean.
    FIELD-SYMBOLS <temp3> LIKE LINE OF mo_layout->ms_layout-t_layout.
    DATA temp4 LIKE sy-tabix.
DATA selkz LIKE REF TO <temp23>.
    FIELD-SYMBOLS <tab> TYPE data.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA toolbar TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp24 LIKE LINE OF mo_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp24.
      DATA lv_index LIKE sy-tabix.
      DATA temp25 TYPE string.
      DATA temp3 TYPE string.
      DATA temp5 TYPE string.
      DATA col TYPE REF TO z2ui5_cl_ui5_view_builder.
    temp4 = sy-tabix.
    READ TABLE mo_layout->ms_layout-t_layout WITH KEY fname = 'SELKZ' ASSIGNING <temp3>.
    sy-tabix = temp4.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ASSIGN <temp3>-visible TO <temp23>.
IF sy-subrc <> 0.
  ASSERT 1 = 0.
ENDIF.

GET REFERENCE OF <temp23> INTO selkz.
    selkz->* = abap_true.


    ASSIGN mt_table->* TO <tab>.


    table = page->ele( `FlexBox`
                      )->a( n = `height` v = '85vh'
                      )->ele( n = `Table` ns = `table`
                      )->a( n = `alternateRowColors` v = 'true'
                      )->a( n = `visibleRowCountMode` v = 'Auto'
                      )->a( n = `selectionMode` v = 'None'
                      )->a( n = `selectionBehavior` v = 'RowSelector'
                      )->a( n = `rows` v = client->_bind_edit( <tab> ) ).


    toolbar = table->ele( n = `extension` ns = `table`
                        )->ele( `OverflowToolbar`
                        )->tag( `ToolbarSpacer` ).

    z2ui5_cl_layo_pop=>render_layout_function( xml    = toolbar
                                               client = client
                                               layout = mo_layout ).


    columns = table->ele( n = `columns` ns = `table` ).



    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      lv_index = sy-tabix.


      temp25 = layout->fname.

      temp3 = layout->fname.

      IF layout->width IS NOT INITIAL.
        temp5 = client->_bind( val = layout->width tab = mo_layout->ms_layout-t_layout tab_index = lv_index ).
      ELSE.
        CLEAR temp5.
      ENDIF.

      col = columns->ele( n = `Column` ns = `table`
                      )->a( n = `visible` v = client->_bind( val       = layout->visible
                                                      tab       = mo_layout->ms_layout-t_layout
                                                      tab_index = lv_index )
                      )->a( n = `width` v = temp5
                      )->a( n = `sortProperty` v = temp25
                      )->a( n = `filterProperty` v = temp3
                      )->tag( `Text`
                      )->a( n = `text` v = layout->tlabel
                      )->ele( n = `template` ns = `table` ).

      IF layout->fname = 'SELKZ'.

        col->tag( `CheckBox`
            )->a( n = `selected` v = |\{{ layout->fname }\}| ).

      ELSE.

        col->tag( `Input`
            )->a( n = `submit` v = client->_event( val = 'INPUT' )
            )->a( n = `value` v = |\{{ layout->fname }\}| ).

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD button_delete.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE ANY.
      FIELD-SYMBOLS <selkz> TYPE any.
      FIELD-SYMBOLS <id> TYPE any.
      FIELD-SYMBOLS <line_tmp> TYPE ANY.
        FIELD-SYMBOLS <id_tmp> TYPE any.

    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_tmp->* TO <tab_tmp>.


    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <selkz>.
      ASSIGN COMPONENT 'SELKZ' OF STRUCTURE <line> TO <selkz>.

      IF <selkz> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.
      IF <selkz> = abap_false.
        CONTINUE.
      ENDIF.


      UNASSIGN <id>.
      ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <id>.

      IF <id> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      " We need to delete this in the tmp table aslwell ..

      LOOP AT <tab_tmp> ASSIGNING <line_tmp>.


        UNASSIGN <id_tmp>.
        ASSIGN COMPONENT mc_row_id OF STRUCTURE <line_tmp> TO <id_tmp>.

        IF <id_tmp> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.
        IF <id_tmp> = <id>.
          DELETE <tab_tmp>.
        ENDIF.

      ENDLOOP.

      DELETE <tab>.

      view_model_update( ).
      mv_change_active = abap_true.

      UNASSIGN <selkz>.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_input.
    FIELD-SYMBOLS <mt_table_tmp> TYPE data.
    FIELD-SYMBOLS <mt_table> TYPE data.
      DATA temp7 TYPE xsdboolean.

    CLEAR mv_key_error.


    ASSIGN mt_table_tmp->* TO <mt_table_tmp>.

    ASSIGN mt_table->* TO <mt_table>.

    " stays set once set: copies and deletions in multi-edit reach both
    " tables, so an equal comparison does not mean nothing is left to save
    IF mv_change_active = abap_false.

      temp7 = boolc( <mt_table_tmp> <> <mt_table> ).
      mv_change_active = temp7.
    ENDIF.

    view_model_update( ).

  ENDMETHOD.

  METHOD button_copy.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <copy>    TYPE any.
    FIELD-SYMBOLS <copy_id> TYPE any.
    FIELD-SYMBOLS <line> TYPE ANY.
      FIELD-SYMBOLS <selkz> TYPE any.

    ASSIGN mt_table->* TO <tab>.
    ASSIGN mt_table_tmp->* TO <tab_tmp>.


    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <selkz>.
      ASSIGN COMPONENT 'SELKZ' OF STRUCTURE <line> TO <selkz>.

      IF <selkz> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      IF <selkz> = abap_true.
        <selkz> = abap_false.
        APPEND <line> TO <tab> ASSIGNING <copy>.

        " only the copy gets a new id, and it goes to the working copy too:
        " re-keying every row left mt_table_tmp with ids nothing matched
        UNASSIGN <copy_id>.
        ASSIGN COMPONENT mc_row_id OF STRUCTURE <copy> TO <copy_id>.
        IF <copy_id> IS ASSIGNED.
          TRY.
              <copy_id> = cl_system_uuid=>create_uuid_c32_static( ).
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
        ENDIF.
        APPEND <copy> TO <tab_tmp>.

        mv_change_active = abap_true.

        view_model_update( ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD view_model_update.

    IF mo_parent_view IS INITIAL.
      client->view_model_update( ).
    ELSE.
      mv_view_model_update = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD on_after_popup.

    FIELD-SYMBOLS <tab>         TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_changed> TYPE STANDARD TABLE.
        DATA temp26 TYPE REF TO z2ui5_cl_tm_pop.
        DATA app LIKE temp26.
        FIELD-SYMBOLS <app_tab> TYPE data.
          FIELD-SYMBOLS <line> TYPE ANY.
            FIELD-SYMBOLS <row> TYPE any.
            DATA found LIKE abap_false.
            FIELD-SYMBOLS <line_changed> TYPE ANY.
              FIELD-SYMBOLS <row_changed> TYPE any.
              FIELD-SYMBOLS <line_tmp> TYPE ANY.
                FIELD-SYMBOLS <row_tmp> TYPE any.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.

        temp26 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp26.

        ASSIGN mt_table->* TO <tab>.

        ASSIGN app->mt_data->* TO <app_tab>.

        IF <app_tab> <> <tab>.
          mv_change_active = abap_true.

          ASSIGN mt_table_tmp->* TO <tab_tmp>.
          ASSIGN app->mt_data->* TO <tab_changed>.

          " Check for deleted lines

          LOOP AT <tab> ASSIGNING <line>.


            UNASSIGN <row>.
            ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <row>.
            IF <row> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.


            found = abap_false.


            LOOP AT <tab_changed> ASSIGNING <line_changed>.


              UNASSIGN <row_changed>.
              ASSIGN COMPONENT mc_row_id OF STRUCTURE <line_changed> TO <row_changed>.
              IF <row_changed> IS NOT ASSIGNED.
                CONTINUE.
              ENDIF.

              IF <row_changed> = <row>.
                found = abap_true.
                EXIT.
              ENDIF.

            ENDLOOP.

            IF found = abap_false.
              " append <row> to t_delete


              LOOP AT <tab_tmp> ASSIGNING <line_tmp>.


                UNASSIGN <row_tmp>.
                ASSIGN COMPONENT mc_row_id OF STRUCTURE <line_tmp> TO <row_tmp>.
                IF <row_tmp> IS NOT ASSIGNED.
                  CONTINUE.
                ENDIF.

                IF <row> = <row_tmp>.
                  DELETE <tab_tmp>.
                  EXIT.
                ENDIF.

              ENDLOOP.

            ENDIF.

          ENDLOOP.

          " Check for added lines
          LOOP AT <tab_changed> ASSIGNING <line_changed>.

            UNASSIGN <row_changed>.
            ASSIGN COMPONENT mc_row_id OF STRUCTURE <line_changed> TO <row_changed>.
            IF <row_changed> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            found = abap_false.

            LOOP AT <tab> ASSIGNING <line>.

              UNASSIGN <row>.
              ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <row>.
              IF <row> IS NOT ASSIGNED.
                CONTINUE.
              ENDIF.

              IF <row_changed> = <row>.
                found = abap_true.
                EXIT.
              ENDIF.

            ENDLOOP.

            IF found = abap_false.
              APPEND <line_changed> TO <tab_tmp>.
            ENDIF.

          ENDLOOP.


          <tab> = <app_tab>.

          " Transfer changes for TAB to TMP_TAB.
          LOOP AT <tab> ASSIGNING <line>.

            UNASSIGN <row>.
            ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <row>.
            IF <row> IS NOT ASSIGNED.
              CONTINUE.
            ENDIF.

            LOOP AT <tab_tmp> ASSIGNING <line_tmp>.

              UNASSIGN <row_tmp>.
              ASSIGN COMPONENT mc_row_id OF STRUCTURE <line_tmp> TO <row_tmp>.
              IF <row_tmp> IS NOT ASSIGNED.
                CONTINUE.
              ENDIF.

              IF <row_tmp> = <row>.
                IF <line> <> <line_tmp>.
                  <line_tmp> = <line>.
                ENDIF.
                EXIT.
              ELSE.
                CONTINUE.
              ENDIF.

            ENDLOOP.

          ENDLOOP.

        ENDIF.

        view_model_update( ).

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD render_table_input.
      DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
      DATA page TYPE REF TO z2ui5_cl_ui5_view_builder.

    IF mo_parent_view IS INITIAL.


      view = z2ui5_cl_ui5_view_builder=>factory(
                       )->ele( n = `View` ns = `mvc`
                       )->a( n = `xmlns` v = `sap.m`
                       )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                       )->a( n = `displayBlock` v = `true`
                       )->a( n = `height` v = `100%` ).


      page = view->ele( `Page`
                       )->a( n = `title` v = `Table Maintenance` ).

    ELSE.
      page = mo_parent_view.
    ENDIF.

    mv_check_table_input = abap_true.

    page->ele( `VBox`
        )->a( n = `class` v = `sapUiSmallMargin`
        )->a( n = `visible` v = client->_bind( mv_check_table_input )
        )->ele( `items`
        )->tag( `MessageStrip`
        )->a( n = `text` v = `Enter the table to maintain, or start the app with the URL parameter table.`
        )->a( n = `type` v = `Information`
        )->a( n = `showIcon` b = abap_true
        )->a( n = `class` v = `sapUiSmallMarginBottom`
        )->tag( `Input`
        )->a( n = `value` v = client->_bind( mv_table_input )
        )->a( n = `placeholder` v = `Table name`
        )->a( n = `width` v = `20rem`
        )->a( n = `submit` v = client->_event( `TABLE_OPEN` )
        )->tag( `Button`
        )->a( n = `text` v = `Open`
        )->a( n = `type` v = `Emphasized`
        )->a( n = `press` v = client->_event( `TABLE_OPEN` ) ).

    IF mo_parent_view IS INITIAL.
      client->view_display( view->stringify( ) ).
    ELSE.
      mv_view_display = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD on_event_table_input.
      DATA table TYPE string.
      DATA descr TYPE REF TO cl_abap_typedescr.

    IF client->get( )-event = `TABLE_OPEN`.


      table = to_upper( condense( mv_table_input ) ).
      IF table IS INITIAL.
        client->message_toast_display( `Enter a table name.` ).
        RETURN.
      ENDIF.


      cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = table
                                           RECEIVING  p_descr_ref    = descr
                                           EXCEPTIONS type_not_found = 1
                                                      OTHERS         = 2 ).
      IF sy-subrc <> 0 OR descr->kind <> cl_abap_typedescr=>kind_struct.
        client->message_toast_display( |Table { table } does not exist.| ).
        RETURN.
      ENDIF.

      mv_table = table.
      mv_check_table_input = abap_false.

      on_init( ).

      render_main( ).

    ENDIF.

  ENDMETHOD.

  METHOD copy_corresponding.

    FIELD-SYMBOLS <ls_source> TYPE any.
    FIELD-SYMBOLS <ls_target> TYPE any.

    CLEAR ct_target.
    LOOP AT it_source ASSIGNING <ls_source>.
      APPEND INITIAL LINE TO ct_target ASSIGNING <ls_target>.
      MOVE-CORRESPONDING <ls_source> TO <ls_target>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
