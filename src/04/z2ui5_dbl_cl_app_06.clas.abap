CLASS z2ui5_dbl_cl_app_06 DEFINITION PUBLIC.

  PUBLIC SECTION.

    INTERFACES z2ui5_if_app.

    DATA check_initialized TYPE abap_bool.
    DATA client TYPE REF TO z2ui5_if_client.
    DATA mv_file TYPE string.
    DATA mv_check_download_file TYPE abap_bool.

    TYPES:
      BEGIN OF ty_s_config_head,
        title TYPE string,
      END OF ty_s_config_head.

    DATA:
      BEGIN OF ms_draft,
        table_name               TYPE string,
        check_load_pressed       TYPE abap_bool,
        check_config_pressed     TYPE abap_bool,
        check_config_pos_pressed TYPE abap_bool,
        check_preview_pressed    TYPE abap_bool,
        check_download_pressed   TYPE abap_bool,
        t_tab                    TYPE REF TO data,
        max_rows                 TYPE i VALUE 10,
        t_fcat                   TYPE z2ui5add_cl_abap2xlsx_api=>ty_t_xlsx,
        t_config                 TYPE STANDARD TABLE OF z2ui5add_cl_abap2xlsx_api=>ty_s_xlsx_settings WITH EMPTY KEY,
        t_config_head            TYPE STANDARD TABLE OF ty_s_config_head WITH EMPTY KEY,
        check_download_active    TYPE abap_bool,
        check_file_row_limit     TYPE abap_bool VALUE abap_true,
        file_max_rows            TYPE i VALUE 10,
        file_rows                TYPE i,
        file_size                TYPE i,
      END OF ms_draft.

    METHODS set_view.
    METHODS load_table.

    METHODS on_event.
    METHODS on_callback.
    METHODS on_init.
    METHODS set_view_load
      IMPORTING
        page TYPE REF TO z2ui5_cl_xml_view.
    METHODS set_view_config
      IMPORTING
        page TYPE REF TO z2ui5_cl_xml_view.
    METHODS set_view_preview
      IMPORTING
        page TYPE REF TO z2ui5_cl_xml_view.
    METHODS set_view_config_pos
      IMPORTING
        page TYPE REF TO z2ui5_cl_xml_view.
    METHODS set_view_download
      IMPORTING
        page TYPE REF TO z2ui5_cl_xml_view.
    METHODS create_file.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS Z2UI5_DBL_CL_APP_06 IMPLEMENTATION.


  METHOD create_file.

    TRY.

        DATA lr_tab TYPE REF TO data.
        CREATE DATA lr_tab TYPE STANDARD TABLE OF (ms_draft-table_name).
        FIELD-SYMBOLS <tab> TYPE table.
        ASSIGN lr_tab->* TO <tab>.

        IF ms_draft-check_file_row_limit = abap_true.

          SELECT FROM (ms_draft-table_name)
            FIELDS *
            INTO CORRESPONDING FIELDS OF TABLE @<tab>
            UP TO @ms_draft-file_max_rows ROWS.

        ELSE.

          SELECT FROM (ms_draft-table_name)
            FIELDS *
            INTO CORRESPONDING FIELDS OF TABLE @<tab>.

        ENDIF.

        DATA: lo_excel     TYPE REF TO zcl_excel,
              lo_writer    TYPE REF TO zif_excel_writer,
              lo_worksheet TYPE REF TO zcl_excel_worksheet.

        " Creates active sheet
        CREATE OBJECT lo_excel.

        " Get active sheet
        lo_worksheet = lo_excel->get_active_worksheet( ).
        lo_worksheet->set_title( CONV #( ms_draft-t_config_head[ 1 ]-title ) ).

        lo_worksheet->bind_table( ip_table          = <tab>
                                  is_table_settings = ms_draft-t_config[ 1 ]
                                  it_field_catalog  = ms_draft-t_fcat ).

        lo_worksheet->freeze_panes( ip_num_rows = 1 ).

        CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.
        DATA(lv_result) = lo_writer->write_file( lo_excel ).
        mv_file = z2ui5_cl_util=>conv_encode_x_base64( lv_result ).

        ms_draft-file_rows = lines( <tab> ).
        ms_draft-file_size = xstrlen( lv_result ) / 1000.

      CATCH cx_root INTO DATA(lx).
        client->message_box_display(
            text = lx->get_text( )
            type = 'error' ).
    ENDTRY.
  ENDMETHOD.


  METHOD load_table.

    FIELD-SYMBOLS <tab> TYPE table.
    ASSIGN ms_draft-t_tab->* TO <tab>.

    SELECT FROM (ms_draft-table_name)
      FIELDS *
      INTO CORRESPONDING FIELDS OF TABLE @<tab>
      UP TO @ms_draft-max_rows ROWS.

  ENDMETHOD.


  METHOD on_callback.

    TRY.
        DATA(lo_prev) = client->get_app( client->get(  )-s_draft-id_prev_app ).
        ms_draft-table_name = CAST z2ui5_cl_pop_input_val( lo_prev )->result( )-value.
        ms_draft-check_load_pressed = abap_true.

        CREATE DATA ms_draft-t_tab TYPE STANDARD TABLE OF (ms_draft-table_name).
        FIELD-SYMBOLS <tab> TYPE table.
        ASSIGN  ms_draft-t_tab->* TO <tab>.

        ms_draft-t_fcat = zcl_excel_common=>get_fieldcatalog( ip_table = <tab> ).
        DATA ls_table_settings TYPE zexcel_s_table_settings.
        ls_table_settings-table_style  = zcl_excel_table=>builtinstyle_medium5.
        INSERT ls_table_settings INTO TABLE ms_draft-t_config.

        DATA ls_config_head TYPE ty_s_config_head.
        ls_config_head-title = `tabtitle`.
        INSERT ls_config_head INTO TABLE ms_draft-t_config_head.

        load_table( ).
        set_view(  ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `DOWNLOAD_FILE`.
        mv_check_download_file = abap_true.
        set_view( ).

      WHEN 'CREATE_FILE'.
        create_file( ).
        set_view( ).

      WHEN `VIEW_LOAD`.
        ms_draft-check_load_pressed = abap_true.
        ms_draft-check_config_pressed = abap_false.
        ms_draft-check_config_pos_pressed = abap_false.
        ms_draft-check_preview_pressed = abap_false.
        ms_draft-check_download_pressed = abap_false.
        set_view( ).

      WHEN `VIEW_CONFIG`.
        ms_draft-check_load_pressed = abap_false.
        ms_draft-check_config_pressed = abap_true.
        ms_draft-check_download_pressed = abap_false.
        ms_draft-check_preview_pressed = abap_false.
        ms_draft-check_download_pressed = abap_false.
        set_view( ).

      WHEN `VIEW_CONFIG_POS`.
        ms_draft-check_load_pressed = abap_false.
        ms_draft-check_config_pressed = abap_false.
        ms_draft-check_config_pos_pressed = abap_true.
        ms_draft-check_preview_pressed = abap_false.
        ms_draft-check_download_pressed = abap_false.
        set_view( ).

      WHEN `VIEW_PREVIEW`.
        ms_draft-check_load_pressed = abap_false.
        ms_draft-check_config_pressed = abap_false.
        ms_draft-check_config_pos_pressed = abap_false.
        ms_draft-check_preview_pressed = abap_true.
        ms_draft-check_download_pressed = abap_false.
        set_view( ).

      WHEN `VIEW_DOWNLOAD`.
        ms_draft-check_load_pressed = abap_false.
        ms_draft-check_config_pressed = abap_false.
        ms_draft-check_config_pos_pressed = abap_false.
        ms_draft-check_preview_pressed = abap_false.
        ms_draft-check_download_pressed = abap_true.
        set_view( ).

      WHEN `LOAD`.
        load_table( ).
        set_view(  ).

      WHEN 'DOWNLOAD'.
        IF ms_draft-t_tab IS NOT BOUND.
          client->message_box_display( `Table is empty, no export possible` ).
          RETURN.
        ENDIF.
        ms_draft-check_download_active = abap_true.
        set_view( ).

      WHEN `NEW`.
        DATA(lo_app) = z2ui5_cl_pop_input_val=>factory(
            title   = `Create a New XLSX Draft`
            text    = `Database Table:`
            val     = ms_draft-table_name ).
        client->nav_app_call( lo_app ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_init.

    set_view(  ).

  ENDMETHOD.


  METHOD set_view.

    DATA(view) = z2ui5_cl_xml_view=>factory( ).

    DATA(page) = view->page(
                title          = 'a2UI5 App - XLSX Download'
                navbuttonpress = client->_event( 'BACK' )
                shownavbutton = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
          )->header_content(
                )->link(
                    text = 'Project on GitHub'
                    target = '_blank'
                    href = `https://github.com/oblomov-dev/a2UI5-xlsx_loader`
                )->get_parent(  ).

    CASE abap_true.
      WHEN ms_draft-check_load_pressed.
        set_view_load( page ).
      WHEN ms_draft-check_config_pressed.
        set_view_config( page ).
      WHEN ms_draft-check_config_pos_pressed.
        set_view_config_pos( page ).
      WHEN ms_draft-check_preview_pressed.
        set_view_preview( page ).
      WHEN ms_draft-check_download_pressed.
        set_view_download( page ).
    ENDCASE.

    DATA(footer) = page->footer( )->overflow_toolbar( ).
    footer->button( icon = 'sap-icon://create'  text = `New` press = client->_event( 'NEW' )
        )->button( text  = 'Load' press = client->_event( 'CONFIG' ) icon  = `sap-icon://download-from-cloud`
        )->button(  text  = 'Save Draft' press = client->_event( 'DOWNLOAD' ) icon = `sap-icon://upload-to-cloud`
        )->input( description = `Table` value = client->_bind_edit( ms_draft-table_name ) width = `15%` enabled = abap_false
        )->toolbar_spacer( ).

    IF ms_draft-table_name IS NOT INITIAL.
      footer->button(
          text = '(1) Data Preview'
          type = `Emphasized`
          press = client->_event( 'VIEW_LOAD' )
          enabled = xsdbool( ms_draft-check_load_pressed = abap_false )
      )->button(
          text = '(2) Config Head'
          type = `Emphasized`
          press = client->_event( 'VIEW_CONFIG' )
          enabled = xsdbool( ms_draft-check_config_pressed = abap_false )
       )->button(
          text = '(3) Config Pos'
          type = `Emphasized`
          press = client->_event( 'VIEW_CONFIG_POS' )
          enabled = xsdbool( ms_draft-check_config_pos_pressed = abap_false )
      )->button(
          text = '(4) XLSX Preview'
          type = `Emphasized`
          press = client->_event( 'VIEW_PREVIEW' )
          enabled = xsdbool( ms_draft-check_preview_pressed = abap_false )
         )->button(
          text = '(5) Download'
          type = `Emphasized`
          press = client->_event( 'VIEW_DOWNLOAD' )
          enabled = xsdbool( ms_draft-check_download_pressed = abap_false )
      ).
    ENDIF.

    client->view_display( view->stringify( ) ).

  ENDMETHOD.


  METHOD set_view_config.
    .
    DATA(cont) = page->scroll_container(
         height     = `30%`
         width      = `100%`
         vertical   = abap_true
         horizontal = abap_true
     ).

    DATA(tab) = cont->table(
            items = client->_bind_edit( ms_draft-t_config )
       )->header_toolbar(
           )->overflow_toolbar(
               )->title( `Excel Configuration`
               )->toolbar_spacer(
               )->button( text = `Reset` press = client->_event( `RESET_CONFIG` ) icon = `sap-icon://refresh` type = `Emphasized`
      )->get_parent( )->get_parent( ).

    DATA(lt_fields) = z2ui5_cl_util=>rtti_get_t_attri_by_any( ms_draft-t_config ).

    DATA(lo_columns) = tab->columns( ).
    LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
      lo_columns->column( )->text( lv_field-name ).
    ENDLOOP.

    DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
    LOOP AT lt_fields INTO lv_field FROM 1.
      lo_cells->input( `{` && lv_field-name && `}` ).
    ENDLOOP.

    cont = page->scroll_container(
         height     = `30%`
         width      = `100%`
         vertical   = abap_true
         horizontal = abap_true
     ).

    tab = cont->table(
            items = client->_bind_edit( ms_draft-t_config_head )
       )->header_toolbar(
           )->overflow_toolbar(
               )->title( `Parameter`
               )->toolbar_spacer(
               )->button( text = `Reset` press = client->_event( `RESET_FCAT` ) icon = `sap-icon://refresh` type = `Emphasized`
      )->get_parent( )->get_parent( ).

    lt_fields = z2ui5_cl_util=>rtti_get_t_attri_by_any( ms_draft-t_config_head ).

    lo_columns = tab->columns( ).
    LOOP AT lt_fields INTO lv_field FROM 1.
      lo_columns->column( )->text( lv_field-name ).
    ENDLOOP.

    lo_cells = tab->items( )->column_list_item( )->cells( ).
    LOOP AT lt_fields INTO lv_field FROM 1.
      lo_cells->input( `{` && lv_field-name && `}` ).
    ENDLOOP.

  ENDMETHOD.


  METHOD set_view_config_pos.

    DATA(cont) = page->scroll_container(
         height     = `100%`
         width      = `100%`
         vertical   = abap_true
         horizontal = abap_true
     ).

    DATA(tab) = cont->table(
            items = client->_bind_edit( ms_draft-t_fcat )
       )->header_toolbar(
           )->overflow_toolbar(
               )->title( `Excel Fieldcatalog`
               )->toolbar_spacer(
               )->button( text = `Reset` press = client->_event( `RESET_FCAT` ) icon = `sap-icon://refresh` type = `Emphasized`
      )->get_parent( )->get_parent( ).

    DATA(lt_fields) = z2ui5_cl_util=>rtti_get_t_attri_by_any( ms_draft-t_fcat ).

    DATA(lo_columns) = tab->columns( ).
    LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
      lo_columns->column( )->text( lv_field-name ).
    ENDLOOP.

    DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
    LOOP AT lt_fields INTO lv_field FROM 1.
      lo_cells->input( `{` && lv_field-name && `}` ).
    ENDLOOP.

  ENDMETHOD.


  METHOD set_view_download.

    IF mv_check_download_file = abap_true.
      mv_check_download_file = abap_false.

**        view->_generic( ns = `html` name = `iframe` t_prop = VALUE #( ( n = `src` v = `data:application/xlsx;base64,` && lv_base ) ( n = `hidden` v = `hidden` ) ) ).
      page->_generic( ns = `html` name = `iframe` t_prop = VALUE #( ( n = `src` v = `data:text/csv;base64,` && mv_file ) ( n = `hidden` v = `hidden` ) ) ).
**        view->_generic( ns = `html` name = `a` t_prop = VALUE #( ( n = `href` v = `data:text/csv;base64,` && lv_base ) ( n = `download` v = `filename.csv` ) ) ).

    ENDIF.

    DATA(content) = page->simple_form( title    = `Create File .xlsx`
                                       layout   = `ResponsiveGridLayout`
                                       editable = `true` ).

    content->label( `Activate Row Limitation`
         )->checkbox( selected = client->_bind_edit( ms_draft-check_file_row_limit )
        )->label( `Rows`
        )->input( value = client->_bind_edit( ms_draft-file_max_rows ) enabled = client->_bind_edit( ms_draft-check_file_row_limit ) width = `10%`
        )->label( `Prepare File with abap2xlsx`
        )->button( text = `Create` width = `7%` press = client->_event( `CREATE_FILE` )
        )->label( `Number of Entries`
        )->input( value = client->_bind( ms_draft-file_rows ) width = `10%` enabled = abap_false
        )->label( `File Size`
        )->input( value = client->_bind( ms_draft-file_size ) width = `10%` description = `kB` enabled = abap_false
        )->label( `File`
        )->button( text = `Download` width = `7%` enabled = COND #( WHEN mv_file IS NOT INITIAL THEN abap_true ELSE abap_false ) press = client->_event( `DOWNLOAD_FILE` ) ).

  ENDMETHOD.


  METHOD set_view_load.

    IF ms_draft-t_tab IS BOUND.

      FIELD-SYMBOLS <tab> TYPE table.
      ASSIGN  ms_draft-t_tab->* TO <tab>.

      DATA(cont) = page->scroll_container(
           height     = `100%`
           width      = `100%`
           vertical   = abap_true
           horizontal = abap_true
       ).

      DATA(tab) = cont->table(
              items = client->_bind( <tab> )
         )->header_toolbar(
             )->overflow_toolbar(
                 )->title( `(1) Data Preview - ` && ms_draft-table_name
                 )->toolbar_spacer(
                 )->input( description = `rows` value = client->_bind_edit( ms_draft-max_rows ) width = `10%`
                 )->button( text = `Reset` press = client->_event( `LOAD` ) icon = `sap-icon://refresh` type = `Emphasized`
        )->get_parent( )->get_parent( ).

      DATA(lt_fields) = z2ui5_cl_util=>rtti_get_t_attri_by_any( <tab> ).

      DATA(lo_columns) = tab->columns( ).
      LOOP AT lt_fields INTO DATA(lv_field) FROM 1.
        lo_columns->column( )->text( lv_field-name ).
      ENDLOOP.

      DATA(lo_cells) = tab->items( )->column_list_item( )->cells( ).
      LOOP AT lt_fields INTO lv_field FROM 1.
        lo_cells->text( `{` && lv_field-name && `}` ).
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD set_view_preview.


  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      on_init( ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true.
      on_callback( ).
      RETURN.
    ENDIF.

    on_event( ).

  ENDMETHOD.
ENDCLASS.
