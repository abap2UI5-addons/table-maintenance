CLASS z2ui5_cl_tm_pop DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA ms_fixval   TYPE REF TO data.
    DATA ms_data_row TYPE REF TO data.
    " Importing
    DATA mo_layout   TYPE REF TO z2ui5_cl_layo_manager.
    " the caller's table with the change of this popup - read it back once
    " the popup returned; after a cancel it is the table as it came in
    DATA mt_data     TYPE REF TO data.

    "! The popup for one row of a table - edit, add, copy, delete, display.
    "! Read the table back from mt_data once the popup returned (the caller's
    "! on navigated); after a cancel it is io_table as it came in.
    "! @parameter io_table     | the rows - a standard table with a component
    "!                           ROW_ID (z2ui5_cl_tm_001=>mc_row_id)
    "! @parameter iv_row_id    | ROW_ID of the row to edit; empty: add a row
    "! @parameter it_dfies     | the fields shown, in this order - leave out
    "!                           what is not to be shown
    "! @parameter is_layout    | the layout whose labels the fields take (the
    "!                           data element of a field without one there)
    "! @parameter iv_edit_mode | abap_true: edit iv_row_id, keys not editable
    "! @parameter iv_copy_mode | offer Copy in edit mode (default abap_true)
    "! @parameter iv_tabname   | the table for the value helps
    "! @parameter iv_read_only | display only: every input disabled, only Close
    CLASS-METHODS factory
      IMPORTING
        io_table      TYPE REF TO data
        iv_row_id     TYPE string
        it_dfies      TYPE z2ui5_cl_tm_context=>ty_t_dfies
        is_layout     TYPE REF TO z2ui5_cl_layo_manager OPTIONAL
        iv_edit_mode  TYPE abap_bool
        iv_copy_mode  TYPE abap_bool OPTIONAL
        iv_tabname    TYPE string
        iv_read_only  TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_tm_pop.

  PROTECTED SECTION.
    DATA client       TYPE REF TO z2ui5_if_client.

    DATA mv_init      TYPE abap_bool.
    DATA mv_edit      TYPE abap_bool.
    DATA mv_copy      TYPE abap_bool.
    DATA mv_read_only TYPE abap_bool.

    DATA mv_row_id    TYPE string.
    " rows of the caller's table - before a cancel this popup has only
    " appended rows (the new row of an add, the rows of a copy), so the
    " cancel cuts mt_data back to this count instead of keeping a second
    " copy of the table in the app state
    DATA mv_lines_org TYPE i.
    DATA mv_f4_fname  TYPE string.
    DATA mv_shlpfield TYPE string.

    DATA mt_dfies     TYPE z2ui5_cl_tm_context=>ty_t_dfies.
    " Importing
    DATA mv_tabname   TYPE string.

    METHODS on_init.
    METHODS render_popup.
    METHODS on_event.

    "! A text of data element roll - or fallback, see
    "! z2ui5_cl_tm_001=>get_text_by_dtel
    METHODS get_txt
      IMPORTING
        roll          TYPE clike
        !type         TYPE clike  OPTIONAL
        fallback      TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! The text of message no of class id - or fallback, see
    "! z2ui5_cl_tm_001=>get_text_by_msg
    METHODS get_msg
      IMPORTING
        !id           TYPE clike
        !no           TYPE clike
        fallback      TYPE string
      RETURNING
        VALUE(result) TYPE string.

    METHODS get_fixval.

    "! The label of a field: the long text of the data element the layout
    "! names for it (or the field's own), else the DDIC field texts, else the
    "! field name - a popup opened by another app may come without a layout
    METHODS get_label
      IMPORTING
        dfies         TYPE z2ui5_cl_tm_context=>ty_s_dfies
      RETURNING
        VALUE(result) TYPE string.

    "! The buttons at the bottom - Close always, the others by mode
    METHODS render_popup_buttons
      IMPORTING
        dialog TYPE REF TO z2ui5_cl_ui5_view_builder.

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

    METHODS popup_edit.

    METHODS get_active_row
      RETURNING
        VALUE(result) TYPE string.

    METHODS call_popup_f4.
    METHODS call_popup_shlp.

    METHODS table_to_row.
    METHODS on_after_shlp.
    METHODS on_after_f4.
    METHODS copy_table_line.

    "! Undo what the popup did to mt_data: drop the appended rows. The row
    "! it was opened for is written only by popup_edit( ) and
    "! popup_delete( ), and both leave the popup - a cancel finds it as it
    "! came in
    METHODS cancel.

  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_cl_tm_pop IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF mv_init = abap_false.
      mv_init = abap_true.

      on_init( ).

      render_popup( ).
      RETURN.

    ENDIF.

    on_event( ).

    on_after_f4( ).

    on_after_shlp( ).

  ENDMETHOD.

  METHOD on_init.

    table_to_row( ).

    get_fixval( ).

  ENDMETHOD.

  METHOD table_to_row.

    FIELD-SYMBOLS <tab>       TYPE STANDARD TABLE.
    FIELD-SYMBOLS <table_row> TYPE any.
    DATA found LIKE abap_false.
    FIELD-SYMBOLS <line> TYPE ANY.
      FIELD-SYMBOLS <row_id> TYPE any.
    DATA dfies LIKE LINE OF mt_dfies.
      FIELD-SYMBOLS <value_tab> TYPE any.
      FIELD-SYMBOLS <value_struc> TYPE any.

    ASSIGN mt_data->* TO <tab>.


    found = abap_false.

    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <row_id>.
      ASSIGN COMPONENT z2ui5_cl_tm_001=>mc_row_id OF STRUCTURE <line> TO <row_id>.
      IF <row_id> IS ASSIGNED AND <row_id> = mv_row_id.
        found = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

    " a LOOP leaves <line> on the last row when nothing matched - that row
    " must not be shown as the one the user picked
    IF found = abap_false.
      client->message_toast_display( `The entry is no longer in the list.` ).
      RETURN.
    ENDIF.


    LOOP AT mt_dfies INTO dfies.


      UNASSIGN <value_tab>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <line> TO <value_tab>.
      ASSIGN ms_data_row->* TO <table_row>.

      UNASSIGN <value_struc>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <table_row> TO <value_struc>.

      IF <value_tab> IS ASSIGNED AND <value_struc> IS ASSIGNED.
        <value_struc> = <value_tab>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD render_popup.

    FIELD-SYMBOLS <row>      TYPE any.
    FIELD-SYMBOLS <s_fixval> TYPE any.

    DATA popup TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp1 TYPE string.
    DATA title LIKE temp1.
    DATA dialog TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA simple_form TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp2 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp2.
      DATA enabled TYPE abap_bool.
      DATA temp12 TYPE xsdboolean.
      FIELD-SYMBOLS <val> TYPE any.
      DATA text TYPE string.
      DATA temp3 TYPE string.
      FIELD-SYMBOLS <struc> TYPE any.
      DATA temp4 TYPE string.
      DATA type TYPE string.
        DATA temp5 TYPE string_table.
        DATA temp6 LIKE LINE OF temp5.
        DATA temp8 TYPE i.
        DATA temp7 TYPE string_table.
        DATA temp10 LIKE LINE OF temp7.
        DATA temp11 TYPE i.
            DATA temp9 TYPE i.
    popup = z2ui5_cl_ui5_view_builder=>factory(
                      )->ele( n = `FragmentDefinition` ns = `core`
                      )->a( n = `xmlns` v = `sap.m`
                      )->a( n = `xmlns:core` v = `sap.ui.core`
                      )->a( n = `xmlns:form` v = `sap.ui.layout.form` ).


    IF mv_read_only = abap_true.
      temp1 = `Display`.
    ELSEIF mv_edit = abap_true.
      temp1 = get_txt( roll = 'CRMST_UIU_EDIT' fallback = `Edit` ).
    ELSE.
      temp1 = get_txt( roll = 'RSLPO_GUI_ADDPART' fallback = `Add` ).
    ENDIF.

    title = temp1.


    dialog = popup->ele( `Dialog`
                          )->a( n = `title` t = title
                          )->a( n = `contentWidth` v = '60%'
                          )->a( n = `afterClose` v = client->_event( 'POPUP_CLOSE' ) ).


    simple_form = dialog->ele( n = `SimpleForm` ns = `form`
                                )->a( n = `layout` v = 'ResponsiveGridLayout'
                                )->a( n = `editable` b = abap_true
                                )->ele( n = `content` ns = `form` ).

    " all components - in edit mode the key fields are not editable


    LOOP AT mt_dfies REFERENCE INTO dfies.



      temp12 = boolc( NOT ( dfies->keyflag = abap_true AND mv_edit = abap_true ) ).
      enabled = temp12.

      " the client - named MANDT, or CLIENT on ABAP Cloud - is the logon
      " client on the save, whatever is entered
      IF dfies->datatype = 'CLNT' OR dfies->fieldname = 'MANDT' OR mv_read_only = abap_true.
        enabled = abap_false.
      ENDIF.

      ASSIGN ms_data_row->* TO <row>.

      UNASSIGN <val>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <row> TO <val>.
      IF <val> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.


      text = get_label( dfies->* ).


      IF dfies->keyflag = abap_true.
        temp3 = `Bold`.
      ELSE.
        temp3 = `Standard`.
      ENDIF.
      simple_form->tag( `Label`
          )->a( n = `design` v = temp3
          )->a( n = `text` t = text ).

      ASSIGN ms_fixval->* TO <s_fixval>.

      UNASSIGN <struc>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <s_fixval> TO <struc>.
      IF <struc> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.


      temp4 = dfies->inttype.

      type = get_input_type( temp4 ).

      IF dfies->checktable IS NOT INITIAL.


        CLEAR temp5.

        temp6 = dfies->fieldname.
        INSERT temp6 INTO TABLE temp5.

        temp8 = dfies->leng.
        simple_form->tag( `Input`
            )->a( n = `value` v = client->_bind( <val> )
            )->a( n = `showValueHelp` b = abap_true
            )->a( n = `enabled` b = enabled
            )->a( n = `type` v = type
            )->a( n = `maxLength` v = |{ temp8 }|
            )->a( n = `valueHelpRequest` v = client->_event( val   = 'CALL_POPUP_F4'
                                                               t_arg = temp5 ) ).

      ELSEIF dfies->mac IS NOT INITIAL.


        CLEAR temp7.

        temp10 = dfies->fieldname.
        INSERT temp10 INTO TABLE temp7.

        temp11 = dfies->leng.
        simple_form->tag( `Input`
            )->a( n = `value` v = client->_bind( <val> )
            )->a( n = `showValueHelp` b = abap_true
            )->a( n = `enabled` b = enabled
            )->a( n = `type` v = type
            )->a( n = `maxLength` v = |{ temp11 }|
            )->a( n = `valueHelpRequest` v = client->_event( val   = 'CALL_POPUP_SHLP'
                                                               t_arg = temp7 ) ).

      ELSEIF <struc> IS NOT INITIAL.

        simple_form->ele( `ComboBox`
            )->a( n = `enabled` b = enabled
            )->a( n = `selectedKey` v = client->_bind( <val> )
            )->a( n = `items` v = client->_bind( <struc> )
            )->tag( n = `Item` ns = `core`
            )->a( n = `key` v = '{LOW}'
            )->a( n = `text` v = '{LOW} - {DESCR}' ).

      ELSE.

        CASE dfies->inttype.
          WHEN 'D'.

            simple_form->tag( `DatePicker`
                )->a( n = `value` v = client->_bind( <val> )
                )->a( n = `enabled` b = enabled ).

          WHEN 'T'.

            simple_form->tag( `TimePicker`
                )->a( n = `value` v = client->_bind( <val> )
                )->a( n = `enabled` b = enabled ).

          WHEN OTHERS.


            temp9 = dfies->leng.
            simple_form->tag( `Input`
                )->a( n = `value` v = client->_bind( <val> )
                )->a( n = `showValueHelp` b = abap_false
                )->a( n = `enabled` b = enabled
                )->a( n = `type` v = type
                )->a( n = `maxLength` v = |{ temp9 }| ).

        ENDCASE.

      ENDIF.

    ENDLOOP.

    render_popup_buttons( dialog ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD render_popup_buttons.

    DATA toolbar TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp10 TYPE string.
    toolbar = dialog->ele( `buttons` ).

    " MESSAGE s512(prc_pri) INTO DATA(msg).
    toolbar->tag( `Button`
        )->a( n = `text` t = get_msg( id       = 'PRC_PRI'
                                      no       = `512`
                                      fallback = `Cancel` )
        )->a( n = `press` v = client->_event( 'POPUP_CLOSE' ) ).

    IF mv_read_only = abap_true.
      RETURN.
    ENDIF.

    IF mv_edit = abap_true.

      "  MESSAGE s160(islm_di_gen) INTO msg.
      toolbar->tag( `Button`
          )->a( n = `text` t = get_msg( id       = 'ISLM_DI_GEN'
                                        no       = `160`
                                        fallback = `Delete` )
          )->a( n = `type` v = 'Reject'
          )->a( n = `icon` v = 'sap-icon://delete'
          )->a( n = `press` v = client->_event( val = 'POPUP_DELETE' ) ).

      IF mv_copy = abap_true.
        " MESSAGE s229(cnv_iuuc_replication) INTO msg.
        toolbar->tag( `Button`
            )->a( n = `text` t = get_msg( id       = 'cnv_iuuc_replication'
                                          no       = `229`
                                          fallback = `Copy` )
            )->a( n = `icon` v = 'sap-icon://copy'
            )->a( n = `press` v = client->_event( val = 'POPUP_COPY' ) ).
      ENDIF.
    ENDIF.

    " MESSAGE s020(fsl_utilities) INTO msg.

    IF mv_edit = abap_true.
      temp10 = `POPUP_EDIT`.
    ELSE.
      temp10 = `POPUP_ADD`.
    ENDIF.
    toolbar->tag( `Button`
        )->a( n = `text` t = get_msg( id       = 'fsl_utilities'
                                      no       = `020`
                                      fallback = `OK` )
        )->a( n = `press` v = client->_event( temp10 )
        )->a( n = `type` v = 'Emphasized' ).

  ENDMETHOD.

  METHOD get_label.

    DATA temp11 TYPE string.
    DATA rollname LIKE temp11.
      DATA temp12 TYPE z2ui5_cl_layo_manager=>ty_s_positions.
      DATA temp13 TYPE z2ui5_cl_layo_manager=>ty_s_positions.
      DATA layout LIKE temp12.
    temp11 = dfies-rollname.

    rollname = temp11.
    IF mo_layout IS BOUND.

      CLEAR temp12.

      READ TABLE mo_layout->ms_layout-t_layout INTO temp13 WITH KEY fname = dfies-fieldname.
      IF sy-subrc = 0.
        temp12 = temp13.
      ENDIF.

      layout = temp12.
      IF layout-rollname IS NOT INITIAL.
        rollname = layout-rollname.
      ENDIF.
    ENDIF.

    IF rollname IS NOT INITIAL.
      TRY.
          result = z2ui5_cl_tm_context=>rtti_get_data_element_texts( rollname )-long.
        CATCH cx_root.
          CLEAR result.
      ENDTRY.
    ENDIF.

    IF result IS INITIAL.
      result = dfies-scrtext_l.
    ENDIF.
    IF result IS INITIAL.
      result = dfies-fieldtext.
    ENDIF.
    IF result IS INITIAL.
      result = dfies-fieldname.
    ENDIF.

  ENDMETHOD.

  METHOD on_event.

    " read-only: the popup only closes - whatever else a request asks for
    IF mv_read_only = abap_true AND client->get( )-event <> 'POPUP_CLOSE'.
      RETURN.
    ENDIF.

    CASE client->get( )-event.

      WHEN 'POPUP_DELETE'.

        popup_delete( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'POPUP_COPY'.

        mv_edit = abap_false.

        copy_table_line( ).

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

        cancel( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'CALL_POPUP_F4'.

        call_popup_f4( ).

      WHEN 'CALL_POPUP_SHLP'.

        call_popup_shlp( ).

    ENDCASE.

  ENDMETHOD.

  METHOD get_txt.

    result = z2ui5_cl_tm_001=>get_text_by_dtel( roll     = roll
                                                type     = type
                                                fallback = fallback ).

  ENDMETHOD.

  METHOD get_msg.

    result = z2ui5_cl_tm_001=>get_text_by_msg( id       = id
                                               no       = no
                                               fallback = fallback ).

  ENDMETHOD.

  METHOD get_fixval.

    DATA comp        TYPE cl_abap_structdescr=>component_table.
    DATA lt_fixval   TYPE z2ui5_cl_tm_context=>ty_t_fix_val.
    DATA structdescr TYPE REF TO cl_abap_structdescr.

    FIELD-SYMBOLS <s_fixval> TYPE any.

    DATA temp14 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp14.
      DATA temp15 TYPE cl_abap_structdescr=>component_table.
      DATA temp16 LIKE LINE OF temp15.
      DATA temp12 TYPE REF TO cl_abap_datadescr.
      FIELD-SYMBOLS <fixval> TYPE any.
    LOOP AT mt_dfies REFERENCE INTO dfies.


      CLEAR temp15.
      temp15 = comp.

      temp16-name = dfies->fieldname.

      temp12 ?= cl_abap_datadescr=>describe_by_data( lt_fixval ).
      temp16-type = temp12.
      INSERT temp16 INTO TABLE temp15.
      comp = temp15.
    ENDLOOP.

    structdescr = cl_abap_structdescr=>create( comp ).

    CREATE DATA ms_fixval TYPE HANDLE structdescr.

    LOOP AT mt_dfies REFERENCE INTO dfies.

      ASSIGN ms_fixval->* TO <s_fixval>.

      UNASSIGN <fixval>.
      ASSIGN COMPONENT dfies->fieldname OF STRUCTURE <s_fixval> TO <fixval>.

      IF <fixval> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      <fixval> = z2ui5_cl_tm_context=>rtti_get_t_ddic_fixed_values( dfies->rollname ).

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
    " z   Node Line for Structured Objects
    " 8   Whole Number (8-Byte Integer with Sign)
    " p   Time stamp 'yyyy-mm-dd hh:mm:ss[.nnnnnnn]'

    CASE inttype.
      WHEN 'C' OR 'g'.
        result = 'Text'.
      WHEN 'I' OR 'b' OR 's' OR 'P' OR 'F' OR 'N'.
        result = 'Number'.
      WHEN OTHERS.
        result = 'Text'.
    ENDCASE.

  ENDMETHOD.

  METHOD popup_delete.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE ANY.
      FIELD-SYMBOLS <row> TYPE any.

    ASSIGN mt_data->* TO <tab>.


    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <row>.
      ASSIGN COMPONENT z2ui5_cl_tm_001=>mc_row_id OF STRUCTURE <line> TO <row>.
      IF <row> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      IF <row> = get_active_row( ).
        DELETE <tab>.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD check_key_already_present.

    DATA lv_where TYPE string.
    DATA lr_row   TYPE REF TO data.

    FIELD-SYMBOLS <row> TYPE any.
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    DATA dfies LIKE LINE OF mt_dfies.
      FIELD-SYMBOLS <value_struc> TYPE any.
        DATA lv_value TYPE string.
          DATA lv_where_and TYPE string.

    " the stored rows hold the values as data_to_table writes them (conversion
    " exit, upper case) - the entered row is compared in that form, on a copy
    ASSIGN mt_data->* TO <tab>.
    CREATE DATA lr_row LIKE LINE OF <tab>.
    ASSIGN lr_row->* TO <row>.
    data_to_table( CHANGING row = <row> ).

    " create dynamic where condition - without the client: every row here is
    " of the logon client, but a new row carries an empty one until it is saved

    LOOP AT mt_dfies INTO dfies WHERE keyflag = abap_true AND datatype <> 'CLNT' AND fieldname <> 'MANDT'.


      UNASSIGN <value_struc>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO <value_struc>.

      IF sy-subrc = 0.
        " a backtick in the value would end the literal early

        lv_value = replace( val  = |{ <value_struc> }|
                                  sub  = '`'
                                  with = '``'
                                  occ  = 0 ).
        IF lv_where IS INITIAL.
          lv_where = |{ dfies-fieldname } = `{ lv_value }`|.
        ELSE.

          lv_where_and = |AND { dfies-fieldname } = `{ lv_value }`|.
          lv_where = |{ lv_where } { lv_where_and }|.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF lv_where IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT <tab> TRANSPORTING NO FIELDS WHERE (lv_where).

      client->message_toast_display( 'An entry with this key already exists.' ).
      result = abap_true.
      EXIT.
    ENDLOOP.

  ENDMETHOD.

  METHOD data_to_table.

    FIELD-SYMBOLS <row> TYPE any.

    DATA dfies LIKE LINE OF mt_dfies.
      FIELD-SYMBOLS <value_tab> TYPE any.
      FIELD-SYMBOLS <value_struc> TYPE any.
    LOOP AT mt_dfies INTO dfies.


      UNASSIGN <value_tab>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE row TO <value_tab>.

      ASSIGN ms_data_row->* TO <row>.

      UNASSIGN <value_struc>.
      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO <value_struc>.

      IF NOT ( <value_tab> IS ASSIGNED AND <value_struc> IS ASSIGNED ).
        CONTINUE.
      ENDIF.

      " Conversion Exit?
      IF dfies-convexit IS NOT INITIAL.

        " the context's conversion exit works in place, on the target field:
        " the entered value goes there first, as in the branch below
        <value_tab> = <value_struc>.
        TRY.
            z2ui5_cl_tm_context=>conv_exit( EXPORTING convexit = dfies-convexit
                                          CHANGING  value    = <value_tab> ).
          CATCH z2ui5_cx_tm_error ##NO_HANDLER.
            " the exit refused the value or does not exist - the value
            " stays as it was entered
        ENDTRY.
      ELSE.

        " upper case only where the DDIC says so: the field list of ABAP
        " Cloud carries neither the lower-case flag nor the internal type,
        " and a value is then kept as entered rather than changed
        IF dfies-lowercase = abap_false AND dfies-inttype IS NOT INITIAL.
          <value_struc> = to_upper( <value_struc> ).
        ENDIF.

        " Conversion?!?!?!?!
        IF <value_tab> <> <value_struc>.
          <value_tab> = <value_struc>.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD popup_edit.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    DATA active_row TYPE string.
    FIELD-SYMBOLS <line> TYPE ANY.
      FIELD-SYMBOLS <row_id> TYPE any.

    ASSIGN mt_data->* TO <tab>.


    active_row = get_active_row( ).


    LOOP AT <tab> ASSIGNING <line>.


      UNASSIGN <row_id>.
      ASSIGN COMPONENT z2ui5_cl_tm_001=>mc_row_id OF STRUCTURE <line> TO <row_id>.
      IF <row_id> IS ASSIGNED AND <row_id> = active_row.
        data_to_table( CHANGING row = <line> ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_active_row.

    result = mv_row_id.

  ENDMETHOD.

  METHOD call_popup_f4.

    FIELD-SYMBOLS <row> TYPE any.

    DATA lt_arg TYPE string_table.
    DATA temp17 TYPE string.
    DATA temp18 TYPE string.
    DATA temp19 LIKE sy-subrc.
    DATA temp20 LIKE sy-subrc.
    DATA dfies LIKE LINE OF mt_dfies.
    FIELD-SYMBOLS <temp13> LIKE LINE OF mt_dfies.
    DATA temp14 LIKE sy-tabix.
    FIELD-SYMBOLS <value_struc> TYPE any.
    DATA temp21 TYPE string.
    lt_arg = client->get( )-t_event_arg.


    CLEAR temp17.

    READ TABLE lt_arg INTO temp18 INDEX 1.
    IF sy-subrc = 0.
      temp17 = temp18.
    ENDIF.
    mv_f4_fname = temp17.



    temp20 = sy-subrc.
    READ TABLE mt_dfies WITH KEY fieldname = mv_f4_fname TRANSPORTING NO FIELDS.
    temp19 = sy-subrc.
    sy-subrc = temp20.
    IF NOT temp19 = 0.
      " no field named by the event - nothing to help with
      RETURN.
    ENDIF.



    temp14 = sy-tabix.
    READ TABLE mt_dfies WITH KEY fieldname = mv_f4_fname ASSIGNING <temp13>.
    sy-tabix = temp14.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    dfies = <temp13>.

    ASSIGN ms_data_row->* TO <row>.

    ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO <value_struc>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.


    temp21 = <value_struc>.
    client->nav_app_call( z2ui5_cl_popup_value_help=>factory( i_table = mv_tabname
                                                            i_fname = mv_f4_fname
                                                            i_value = temp21 ) ).

  ENDMETHOD.

  METHOD call_popup_shlp.

    FIELD-SYMBOLS <row> TYPE any.

    DATA lt_arg TYPE string_table.
    DATA temp22 TYPE string.
    DATA temp23 TYPE string.
    DATA temp24 LIKE sy-subrc.
    DATA temp25 LIKE sy-subrc.
    DATA dfies LIKE LINE OF mt_dfies.
    FIELD-SYMBOLS <temp15> LIKE LINE OF mt_dfies.
    DATA temp16 LIKE sy-tabix.
    FIELD-SYMBOLS <value_struc> TYPE any.
    DATA temp26 TYPE string.
    lt_arg = client->get( )-t_event_arg.


    CLEAR temp22.

    READ TABLE lt_arg INTO temp23 INDEX 1.
    IF sy-subrc = 0.
      temp22 = temp23.
    ENDIF.
    mv_shlpfield = temp22.



    temp25 = sy-subrc.
    READ TABLE mt_dfies WITH KEY fieldname = mv_shlpfield TRANSPORTING NO FIELDS.
    temp24 = sy-subrc.
    sy-subrc = temp25.
    IF NOT temp24 = 0.
      " no field named by the event - nothing to help with
      RETURN.
    ENDIF.



    temp16 = sy-tabix.
    READ TABLE mt_dfies WITH KEY fieldname = mv_shlpfield ASSIGNING <temp15>.
    sy-tabix = temp16.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    dfies = <temp15>.

    ASSIGN ms_data_row->* TO <row>.

    ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO <value_struc>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.


    temp26 = <value_struc>.
    client->nav_app_call( z2ui5_cl_popup_shlp=>factory( i_table = mv_tabname
                                                             i_fname = mv_shlpfield
                                                             i_value = temp26
                                                             i_data  = ms_data_row )   ).

  ENDMETHOD.

  METHOD factory.

    " Add new empty row
    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
        DATA comp TYPE abap_component_tab.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <io_table> TYPE data.
      FIELD-SYMBOLS <line> TYPE any.
        FIELD-SYMBOLS <value> TYPE any.

    CREATE OBJECT result.

    result->mt_dfies   = it_dfies.
    result->mv_row_id  = iv_row_id.
    result->mo_layout  = is_layout.
    result->mv_edit    = iv_edit_mode.
    result->mv_tabname = iv_tabname.
    result->mv_read_only = iv_read_only.

    IF iv_copy_mode IS NOT SUPPLIED.
      result->mv_copy = abap_true.
    ELSE.
      result->mv_copy = iv_copy_mode.
    ENDIF.

    TRY.

        comp = z2ui5_cl_tm_context=>rtti_get_t_attri_by_any( io_table ).
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

    TRY.

        new_struct_desc = cl_abap_structdescr=>create( comp ).


        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA result->mt_data     TYPE HANDLE new_table_desc.
        CREATE DATA result->ms_data_row TYPE HANDLE new_struct_desc.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.


    ASSIGN io_table->* TO <io_table>.
    ASSIGN result->mt_data->* TO <tab>.
    IF <io_table> IS NOT ASSIGNED OR <tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    <tab> = <io_table>.
    result->mv_lines_org = lines( <tab> ).

    " an existing row: the popup edits a copy (ms_data_row) and writes the
    " row only when it is left with the change - a cancel finds it as it was
    IF iv_row_id IS INITIAL.


      INSERT INITIAL LINE INTO TABLE <tab> ASSIGNING <line>.

      IF <line> IS ASSIGNED.


        ASSIGN COMPONENT z2ui5_cl_tm_001=>mc_row_id OF STRUCTURE <line> TO <value>.
        IF <value> IS ASSIGNED.

          " a UUID, as copy_table_line does: lines( ) repeats an id once a row
          " was deleted, and z2ui5_cl_tm_001->on_after_popup then took the new
          " row for one it already had and Save dropped it
          TRY.
              <value> = cl_system_uuid=>create_uuid_c32_static( ).
            CATCH cx_root ##NO_HANDLER.
          ENDTRY.
          result->mv_row_id = <value>.
        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD on_after_f4.
    FIELD-SYMBOLS <row> TYPE any.
        DATA temp27 TYPE REF TO z2ui5_cl_popup_value_help.
        DATA app LIKE temp27.
          DATA temp28 TYPE z2ui5_cl_tm_context=>ty_s_dfies.
          DATA temp29 TYPE z2ui5_cl_tm_context=>ty_s_dfies.
          DATA dfies LIKE temp28.
          FIELD-SYMBOLS <value_struc> TYPE any.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " War es die F4 Hilfe?

        temp27 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp27.

        IF app->mv_return_value IS NOT INITIAL.


          CLEAR temp28.

          READ TABLE mt_dfies INTO temp29 WITH KEY fieldname = mv_f4_fname.
          IF sy-subrc = 0.
            temp28 = temp29.
          ENDIF.

          dfies = temp28.

          ASSIGN ms_data_row->* TO <row>.

          ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO <value_struc>.

          IF <value_struc> IS ASSIGNED.
            <value_struc> = app->mv_return_value.
          ENDIF.

        ENDIF.

        render_popup( ).

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD on_after_shlp.
    FIELD-SYMBOLS <row> TYPE any.
        DATA temp30 TYPE REF TO z2ui5_cl_popup_shlp.
        DATA app LIKE temp30.
          DATA temp31 TYPE z2ui5_cl_tm_context=>ty_s_dfies.
          DATA temp32 TYPE z2ui5_cl_tm_context=>ty_s_dfies.
          DATA dfies LIKE temp31.
          FIELD-SYMBOLS <value_struc> TYPE any.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " War es die Such-Hilfe?

        temp30 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp30.

        IF app->mv_return_value IS NOT INITIAL.


          CLEAR temp31.

          READ TABLE mt_dfies INTO temp32 WITH KEY fieldname = mv_shlpfield.
          IF sy-subrc = 0.
            temp31 = temp32.
          ENDIF.

          dfies = temp31.

          ASSIGN ms_data_row->* TO <row>.

          ASSIGN COMPONENT dfies-fieldname OF STRUCTURE <row> TO <value_struc>.

          IF <value_struc> IS ASSIGNED.
            <value_struc> = app->mv_return_value.
          ENDIF.

        ENDIF.

        render_popup( ).

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD copy_table_line.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
    FIELD-SYMBOLS <value> TYPE any.
      FIELD-SYMBOLS <row> TYPE data.

    ASSIGN mt_data->* TO <tab>.
    IF <tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.


    INSERT INITIAL LINE INTO TABLE <tab> ASSIGNING <line>.

    IF <line> IS NOT ASSIGNED.
      RETURN.
    ENDIF.


    ASSIGN COMPONENT z2ui5_cl_tm_001=>mc_row_id OF STRUCTURE <line> TO <value>.
    IF <value> IS ASSIGNED.


      ASSIGN ms_data_row->* TO <row>.

      <line> = <row>.
      TRY.
          <value> = cl_system_uuid=>create_uuid_c32_static( ).
        CATCH cx_root ##NO_HANDLER.
      ENDTRY.
      mv_row_id = <value>.
    ENDIF.

  ENDMETHOD.

  METHOD cancel.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    DATA first_added TYPE i.

    ASSIGN mt_data->* TO <tab>.
    IF <tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.


    first_added = mv_lines_org + 1.
    IF lines( <tab> ) >= first_added.
      DELETE <tab> FROM first_added.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
