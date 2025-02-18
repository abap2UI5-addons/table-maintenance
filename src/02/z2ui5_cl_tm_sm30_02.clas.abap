CLASS z2ui5_cl_tm_sm30_02 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    INTERFACES z2ui5_if_app.

    DATA ms_fixval   TYPE REF TO data.
    DATA ms_data_row TYPE REF TO data.
    DATA mo_layout   TYPE REF TO z2ui5_cl_layo_manager. " Importing
    DATA mt_data     TYPE REF TO data.
    DATA mt_data_tmp TYPE REF TO data.

    CLASS-METHODS factory
      IMPORTING
        io_table      TYPE REF TO data
        iv_row_id     TYPE string
        it_dfies      TYPE z2ui5_cl_util=>ty_t_dfies
        io_layout     TYPE REF TO z2ui5_cl_layo_manager " z2ui5_cl_pop_display_layout=>ty_s_layout
        iv_edit_mode  TYPE abap_bool
        iv_tabname    TYPE string
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_tm_sm30_02.

  PROTECTED SECTION.


    DATA client       TYPE REF TO z2ui5_if_client.

    DATA mv_init      TYPE abap_bool.
    DATA mv_edit      TYPE abap_bool.

    DATA mv_row_id    TYPE string.
    DATA mv_f4_fname  TYPE string.
    DATA mv_shlpfield TYPE string.

    DATA mt_dfies     TYPE z2ui5_cl_util=>ty_t_dfies.
    DATA mv_tabname   TYPE string. " Importing

    METHODS on_init.
    METHODS render_popup.
    METHODS on_event.

    METHODS get_txt
      IMPORTING
        roll          TYPE string
        !type         TYPE char1 OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_fixval.

    METHODS get_input_type
      IMPORTING
        inttype       TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS popup_delete.

    METHODS check_key_already_present
      RETURNING
        VALUE(result) TYPE abap_bool.

    METHODS data_to_table
      CHANGING
        !row TYPE any.

    METHODS set_row_id.
    METHODS popup_edit.

    METHODS get_active_row
      RETURNING
        VALUE(result) TYPE string.

    METHODS call_popup_f4.
    METHODS call_popup_shlp.

    METHODS table_to_row.
    METHODS on_after_shlp.
    METHODS on_after_f4.
    METHODS Copy_table_line.
        METHODS get_view_settings.

  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_cl_tm_sm30_02 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF mv_init = abap_false.
      mv_init = abap_true.

      on_init( ).

      render_popup( ).

    ENDIF.

    on_event( ).

    on_after_f4( ).

    on_after_shlp( ).

  ENDMETHOD.

  METHOD on_init.

    table_to_row( ).

    get_fixval( ).

    get_view_settings( ).

  ENDMETHOD.

  METHOD table_to_row.

    FIELD-SYMBOLS <tab>       TYPE STANDARD TABLE.
    FIELD-SYMBOLS <table_row> TYPE any.

    ASSIGN mt_data->* TO <tab>.

    ASSIGN <tab>[ mv_row_id ] TO FIELD-SYMBOL(<row>).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT mt_dfies INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_tab>).
      ASSIGN ms_data_row->* TO <table_row>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <table_row> TO FIELD-SYMBOL(<value_struc>).

      IF <value_tab> IS ASSIGNED AND <value_struc> IS ASSIGNED.
        <value_struc> = <value_tab>.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD render_popup.

    FIELD-SYMBOLS <row>      TYPE any.
    FIELD-SYMBOLS <s_fixval> TYPE any.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(title) = COND #( WHEN mv_edit = abap_true THEN 'Edit' ELSE 'Add' ).

    DATA(simple_form) = popup->dialog( title        = title
                                       contentwidth = '60%'
                                       afterclose   = client->_event( 'POPUP_CLOSE' )
           )->simple_form( title    = ''
                           layout   = 'ResponsiveGridLayout'
                           editable = abap_true
           )->content( ns = 'form' ).

    " Gehe über alle Comps wenn wir im Edit sind dann sind keyfelder nicht eingabebereit.
    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      DATA(enabled) = COND #( WHEN dfies->keyflag = abap_true AND mv_edit = abap_true THEN abap_false ELSE abap_true ).

      IF dfies->fieldname = 'MANDT'.
        enabled = abap_false.
      ENDIF.

      ASSIGN ms_data_row->* TO <row>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<val>).
      IF <val> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      DATA(text) = mo_layout->ms_layout-t_layout[ fname = dfies->fieldname ]-tlabel.

      simple_form->label( design = COND #( WHEN dfies->keyflag = abap_true THEN 'Bold' )
                          text   = text ).

      ASSIGN ms_fixval->* TO <s_fixval>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <s_fixval> TO FIELD-SYMBOL(<struc>).
      IF <struc> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      DATA(type) = get_input_type( CONV #( dfies->inttype ) ).

      IF dfies->checktable IS NOT INITIAL.

        simple_form->input( value            = client->_bind_edit( <val> )
                            showvaluehelp    = abap_true
                            enabled          = enabled
                            type             = type
                            maxlength        = dfies->leng
                            valuehelprequest = client->_event( val   = 'CALL_POPUP_F4'
                                                               t_arg = VALUE #( ( CONV #( dfies->fieldname ) ) ) ) ).

*      ELSEIF dfies->mac IS NOT INITIAL.
*
*        simple_form->input( value            = client->_bind_edit( <val> )
*                            showvaluehelp    = abap_true
*                            enabled          = enabled
*                            type             = type
*                            maxlength        = dfies->leng
*                            valuehelprequest = client->_event( val   = 'CALL_POPUP_SHLP'
*                                                               t_arg = VALUE #( ( CONV #( dfies->fieldname ) ) ) ) ).

      ELSEIF <struc> IS NOT INITIAL.

        simple_form->combobox( enabled     = enabled
                               selectedkey = client->_bind_edit( <val> )
                               items       = client->_bind( <struc> )
                      )->item( key  = '{LOW}'
                               text = '{LOW} - {DESCR}' ).

      ELSE.

        IF dfies->inttype = 'D'.

          simple_form->date_picker( value = client->_bind_edit( <val> ) ).

        ELSEIF dfies->inttype = 'T'.

          simple_form->time_picker( value = client->_bind_edit( <val> )  ).

        ELSE.

          simple_form->input( value         = client->_bind_edit( <val> )
                              showvaluehelp = abap_false
                              enabled       = enabled
                              type          = type
                              maxlength     = dfies->leng ).

        ENDIF.

      ENDIF.

    ENDLOOP.

    DATA(toolbar) = simple_form->get_root( )->get_child(
              )->buttons( ).

    toolbar->button( text  = 'Close'
                     press = client->_event( 'POPUP_CLOSE' ) ).

    IF mv_edit = abap_true.

      toolbar->button( text  = 'Delete'
                       type  = 'Reject'
                       icon  = 'sap-icon://delete'
                       press = client->_event( val = 'POPUP_DELETE' ) ).

      toolbar->button( text  = 'Copy'
                       type  = 'Inform'
                       icon  = 'sap-icon://copy'
                       press = client->_event( val = 'POPUP_COPY' ) ).

    ENDIF.

    toolbar->button( text  = 'Okay'
                     press = client->_event( COND #( WHEN mv_edit = abap_true THEN `POPUP_EDIT` ELSE `POPUP_ADD` ) )
                     type  = 'Emphasized' ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'POPUP_DELETE'.

        popup_delete( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'POPUP_COPY'.

        mv_edit = abap_false.

        Copy_table_line( ).

        render_popup( ).

      WHEN 'POPUP_ADD'.

        " is there an entry with the same key?
        IF check_key_already_present( ) = abap_false.

          popup_edit( ).

          client->popup_destroy( ).

          client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

        ENDIF.

      WHEN 'POPUP_EDIT'.

        popup_edit( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'POPUP_CLOSE'.

        ASSIGN mt_data_tmp->* TO FIELD-SYMBOL(<data_tmp>).
        ASSIGN mt_data->* TO FIELD-SYMBOL(<data>).
        <data> = <data_tmp>.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'CALL_POPUP_F4'.

        call_popup_f4( ).

      WHEN 'CALL_POPUP_SHLP'.

        call_popup_SHLP( ).

    ENDCASE.

  ENDMETHOD.

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

  METHOD get_fixval.

    DATA comp        TYPE cl_abap_structdescr=>component_table.
    DATA lt_fixval   TYPE z2ui5_cl_util=>ty_t_fix_val.
    DATA structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS <s_fixval> TYPE any.

    LOOP AT mt_dfies REFERENCE INTO DATA(dfies).

      comp = VALUE cl_abap_structdescr=>component_table(
                       BASE comp
                       ( name = dfies->fieldname
                         type = CAST #( cl_abap_datadescr=>describe_by_data( lt_fixval ) ) ) ).
    ENDLOOP.

    structdescr = cl_abap_structdescr=>create( comp ).

    CREATE DATA ms_fixval TYPE HANDLE structdescr.

    LOOP AT mt_dfies REFERENCE INTO dfies.

      ASSIGN ms_fixval->* TO <s_fixval>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <s_fixval> TO FIELD-SYMBOL(<fixval>).

      IF <fixval> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      <fixval> = z2ui5_cl_util=>rtti_get_t_ddic_fixed_values( dfies->rollname  ).

    ENDLOOP.

  ENDMETHOD.

  METHOD get_input_type.

    " C   Character String
    " N   Character String with Digits Only
    " D   Date (YYYYMMDD)
    " T   Time (HHMMSS)
    " X   Byte string (hexadecimal), in DDIC metadata also INT1/2/4
    " I   Integer number (4-byte integer with sign)
    " b   1-byte integer, whole number <= 254
    " s   2-byte integer, only for length field before LCHR or LRAW
    " P   Packed number
    " F   Floating point number to accuracy of 8 bytes
    " g   Character string with variable length (ABAP type STRING)
    " y   Byte string with variable length (ABAP type XSTRING)
    " u   Structured type, flat
    " v   Structured type, deep
    " h   Table type
    " V   Character string (old dictionary type VARC)
    " r   Reference to class/interface
    " l   Reference to data object
    " a   Decimal Floating Point Number, 16 Digits
    " e   Decimal Floating Point Number, 34 Digits
    " j   Static Boxed Components
    " k   Generic Boxed Components
    " z   Node Line for Stuctured Objects
    " 8   Whole Number (8-Byte Integer with Sign)
    " p   Time stamp 'yyyy-mm-dd hh:mm:ss[.nnnnnnn]'

    CASE inttype.
      WHEN 'C' OR 'g'.
        result = 'Text'.
      WHEN 'I' OR 'b' OR 's' OR 'p' OR 'F' OR 'b' OR 'N'.
        result = 'Number'.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD popup_delete.

    DATA index TYPE int4.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    index = get_active_row( ).

    ASSIGN mt_data->* TO <tab>.

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(t_arg) = client->get( )-t_event_arg.

    IF sy-subrc = 0.
      DELETE <tab> INDEX index.
    ENDIF.

    set_row_id( ).

  ENDMETHOD.

  METHOD check_key_already_present.

    DATA lv_where TYPE string.

    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    " create dynamic where condition
    LOOP AT mt_dfies INTO DATA(dfies) WHERE keyflag = abap_true.

      ASSIGN ms_data_row->* TO <row>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).

      IF sy-subrc = 0.
        IF lv_where IS INITIAL.
          lv_where = |{ dfies-fieldname } = `{ <value_struc> }`|.
        ELSE.
          DATA(lv_where_and) = |AND { dfies-fieldname } = `{ <value_struc> }`|.
          CONCATENATE lv_where lv_where_and INTO lv_where SEPARATED BY space.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> TRANSPORTING NO FIELDS WHERE (lv_where).

      client->message_toast_display( 'Duplicate Key Error' ).
      result = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

  METHOD data_to_table.


    FIELD-SYMBOLS <row> TYPE any.

    LOOP AT mt_dfies INTO DATA(dfies).

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE row TO FIELD-SYMBOL(<value_tab>).

      ASSIGN ms_data_row->* TO <row>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).

      IF NOT ( <value_tab> IS ASSIGNED AND <value_struc> IS ASSIGNED ).
        CONTINUE.
      ENDIF.

      " Conversion Exit?
      IF dfies-convexit IS NOT INITIAL.

        TRY.

            DATA(conv) = |CONVERSION_EXIT_{ dfies-convexit }_INPUT|.

*            SELECT SINGLE funcname
*              FROM ZSM30_V_TFDIR
*              WHERE funcname = @conv
*              INTO @DATA(conex).

*            IF sy-subrc = 0.

              CALL FUNCTION conv
                EXPORTING  input  = <value_struc>
                IMPORTING  output = <value_tab>
                EXCEPTIONS OTHERS = 99.
              IF sy-subrc <> 0.

              ENDIF.

*            ENDIF.

          CATCH cx_root.
            <value_tab> = <value_struc>.
        ENDTRY.

      ELSE.

        IF dfies-lowercase = abap_false.
          <value_struc> = to_upper( <value_struc> ).
        ENDIF.

        " Conversion?!?!?!?!
        IF <value_tab> <> <value_struc>.
          <value_tab> = <value_struc>.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD set_row_id.

    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.

    ASSIGN mt_data->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      ASSIGN COMPONENT 'ROW_ID' OF STRUCTURE <line> TO FIELD-SYMBOL(<row>).
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD popup_edit.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    ASSIGN mt_data->* TO <tab>.

    ASSIGN <tab>[  CONV #( get_active_row( ) ) ] TO FIELD-SYMBOL(<row>).

    data_to_table( CHANGING row = <row> ).

    set_row_id( ).

  ENDMETHOD.

  METHOD get_active_row.

*    FIELD-SYMBOLS <row>      TYPE any.
*
*    ASSIGN ms_table_row->* TO <row>.
*    ASSIGN COMPONENT `ROW_ID` OF STRUCTURE <row> TO FIELD-SYMBOL(<id>).
*    IF <id> IS ASSIGNED.
*      result = <id>.
*    ENDIF.

    result = mv_row_id.

  ENDMETHOD.

  METHOD call_popup_f4.

    FIELD-SYMBOLS <row> TYPE any.

    DATA(lt_arg) = client->get( )-t_event_arg.

    mv_f4_fname = VALUE string( lt_arg[ 1 ] ).

    READ TABLE mt_dfies INTO DATA(dfies) WITH KEY fieldname = mv_f4_fname.

    ASSIGN ms_data_row->* TO <row>.
    ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).

    client->nav_app_call( z2ui5_cl_pop_show_value_help=>factory( i_table = mv_tabname
                                                               i_fname = mv_f4_fname
                                                               i_value = CONV #( <value_struc> ) ) ).

  ENDMETHOD.

  METHOD call_popup_shlp.

*    FIELD-SYMBOLS <row> TYPE any.
*
*    DATA(lt_arg) = client->get( )-t_event_arg.
*
*    mv_shlpfield = VALUE string( lt_arg[ 1 ] ).
*
*    READ TABLE mt_dfies INTO DATA(dfies) WITH KEY fieldname = mv_shlpfield.
*
*    ASSIGN ms_data_row->* TO <row>.
*    ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).
*
*    client->nav_app_call( ZSM30_cl_app_005=>factory( i_table = mv_tabname
*                                                          i_fname = mv_shlpfield
*                                                          i_value = CONV #( <value_struc> )
*                                                          i_data  = ms_data_row )   ).

  ENDMETHOD.

  METHOD factory.

    " Add new empty row
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    result = NEW #( ).

    result->mt_dfies   = it_dfies.
    result->mv_row_id  = iv_row_id.
    result->mo_layout  = io_layout.
    result->mv_edit    = iv_edit_mode.
    result->mv_tabname = iv_tabname.

    TRY.
        DATA(comp) = z2ui5_cl_util=>rtti_get_t_attri_by_any( io_table ).
      CATCH cx_root.
    ENDTRY.

    TRY.
        DATA(new_struct_desc) = cl_abap_structdescr=>create( comp ).

        DATA(new_table_desc) = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA result->mt_data     TYPE HANDLE new_table_desc.
        CREATE DATA result->mt_data_TMP TYPE HANDLE new_table_desc.
        CREATE DATA result->ms_data_row TYPE HANDLE new_struct_desc.

      CATCH cx_root.
    ENDTRY.

    FIELD-SYMBOLS <data> TYPE ANY TABLE.
    ASSIGN result->mt_data->* TO <data>.
    ASSIGN result->mt_data_tmp->* TO FIELD-SYMBOL(<data_tmp>).
    ASSIGN io_table->* TO FIELD-SYMBOL(<io_table>).

    <data> = <io_table>.
    <data_tmp> = <io_table>.

    IF iv_row_id IS INITIAL.

      ASSIGN result->mt_data->* TO <tab>.
      IF <tab> IS NOT ASSIGNED.
        RETURN.
      ENDIF.

      APPEND INITIAL LINE TO <tab> ASSIGNING FIELD-SYMBOL(<line>).

      IF <line> IS ASSIGNED.

        ASSIGN COMPONENT `ROW_ID` OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
        IF <value> IS ASSIGNED.

          <value> = lines( <data> ).
          result->mv_row_id = <value>.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD on_after_f4.
    FIELD-SYMBOLS <row> TYPE any.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " War es die F4 Hilfe?
        DATA(app) = CAST z2ui5_cl_pop_show_value_help( client->get_app( client->get( )-s_draft-id_prev_app ) ).

        IF app->mv_return_value IS NOT INITIAL.

          READ TABLE mt_dfies INTO DATA(dfies) WITH KEY fieldname = mv_f4_fname.

          ASSIGN ms_data_row->* TO <row>.
          ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).

          IF <value_struc> IS ASSIGNED.
            <value_struc> = app->mv_return_value.
          ENDIF.

        ENDIF.

        render_popup( ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.

  METHOD on_after_SHLP.
*    FIELD-SYMBOLS <row> TYPE any.
*
*    " Kommen wir aus einer anderen APP
*    IF client->get( )-check_on_navigated = abap_false.
*      RETURN.
*    ENDIF.
*
*    TRY.
*        " War es die Such-Hilfe?
*        DATA(app) = CAST ZSM30_cl_app_005( client->get_app( client->get( )-s_draft-id_prev_app ) ).
*
*        IF app->mv_return_value IS NOT INITIAL.
*
*          READ TABLE mt_dfies INTO DATA(dfies) WITH KEY fieldname = mv_shlpfield.
*
*          ASSIGN ms_data_row->* TO <row>.
*          ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO FIELD-SYMBOL(<value_struc>).
*
*          IF <value_struc> IS ASSIGNED.
*            <value_struc> = app->mv_return_value.
*          ENDIF.
*
*        ENDIF.
*
*        render_popup( ).
*
*      CATCH cx_root.
*    ENDTRY.
  ENDMETHOD.

  METHOD Copy_table_line.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.

    ASSIGN mt_data->* TO <tab>.
    IF <tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO <tab> ASSIGNING FIELD-SYMBOL(<line>).

    IF <line> IS ASSIGNED.

      ASSIGN COMPONENT `ROW_ID` OF STRUCTURE <line> TO FIELD-SYMBOL(<value>).
      IF <value> IS ASSIGNED.

        ASSIGN ms_data_row->* TO FIELD-SYMBOL(<row>).

        FIELD-SYMBOLS <data> TYPE ANY TABLE.
        ASSIGN mt_data->* TO <data>.

        <line> = <row>.
        <value> = lines( <data> ).
        mv_row_id = <value>.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_view_settings.

*            SELECT *
*              FROM ZSM30_V_DD27S
*              WHERE viewname = @mv_tabname
*              INTO table @DATA(dd07s).

  ENDMETHOD.

ENDCLASS.
