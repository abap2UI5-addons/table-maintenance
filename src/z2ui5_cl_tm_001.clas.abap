CLASS z2ui5_cl_tm_001 DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.
    INTERFACES z2ui5_if_tm_001.

    ALIASES mo_parent_view       FOR z2ui5_if_tm_001~mo_parent_view.
    ALIASES mv_view_display      FOR z2ui5_if_tm_001~mv_view_display.
    ALIASES mv_change_active     FOR z2ui5_if_tm_001~mv_change_active.
    ALIASES set_main_app_data    FOR z2ui5_if_tm_001~set_app_data.
    ALIASES ms_transport         FOR z2ui5_if_tm_001~ms_transport.
    ALIASES ms_config            FOR z2ui5_if_tm_001~ms_config.
    ALIASES release_lock         FOR z2ui5_if_tm_001~release_lock.

    DATA mo_layout       TYPE REF TO z2ui5_cl_layo_manager.
    DATA mv_search_value TYPE string.
    DATA mt_table        TYPE REF TO data.
    DATA mt_table_tmp    TYPE REF TO data.
    " the rows as in the database (read, or after the last successful
    " save), key fields and a hash of the row (mc_row_hash) only: save
    " deletes the keys that are no longer in the table, and refuses to
    " overwrite rows that changed in the database since - the other columns
    " are not needed for that, and the app state is serialized on every
    " roundtrip
    DATA mt_table_org    TYPE REF TO data.
    " the table name typed in when the app was started without one
    DATA mv_table_input  TYPE string.
    " the table input is shown until a table is opened - embedded, it stays
    " in the embedder's page, which the builder cannot remove it from
    DATA mv_check_table_input TYPE abap_bool.

    CONSTANTS mc_row_id TYPE string VALUE `ROW_ID` ##NO_TEXT.
    " the column of mt_table_org with the hash of the row
    CONSTANTS mc_row_hash TYPE string VALUE `TM_ROW_HASH` ##NO_TEXT.

    "! The maintenance of one table, to start with nav_app_call( ) or as the
    "! app of a launchpad tile of your own - next to the start by URL
    "! (?app_start=z2ui5_cl_tm_001&amp;table=...).
    "! @parameter i_table  | the database table
    "! @parameter i_config | read-only mode and the layout key handle04 -
    "!                       see z2ui5_if_tm_001=>ty_s_config
    CLASS-METHODS factory
      IMPORTING
        i_table       TYPE clike
        i_config      TYPE z2ui5_if_tm_001=>ty_s_config OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_tm_001.

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
    " the edit lock is held in a stateful session, from the first change
    " until the changes are saved or dropped - see edit_lock
    DATA mv_lock_active    TYPE abap_bool.

    METHODS on_init.

    METHODS on_event.

    METHODS search.

    METHODS get_data.

    METHODS set_row_id.

    METHODS get_dfies.

    METHODS row_select.

    METHODS render_main.

    "! A text of data element roll - or fallback where the data element is
    "! not on the system or has no text (an EWM or S/4HANA system may lack
    "! the data elements the texts are borrowed from)
    METHODS get_txt
      IMPORTING
        roll          TYPE string
        !type         TYPE clike  OPTIONAL
        fallback      TYPE string OPTIONAL
      RETURNING
        VALUE(result) TYPE string.

    "! The text of message no of class id - or fallback where the message
    "! class or number is not on the system (an EWM or S/4HANA system may
    "! lack the classes the texts are borrowed from; a missing message
    "! answers with its own class and number, or nothing)
    METHODS get_msg
      IMPORTING
        !id           TYPE clike
        !no           TYPE clike
        v1            TYPE clike OPTIONAL
        fallback      TYPE string
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

    "! The components of the database snapshot mt_table_org: the key fields
    "! of t_comp - needs mt_dfies, so get_dfies runs before get_data
    METHODS get_comp_key
      IMPORTING
        t_comp        TYPE abap_component_tab
      RETURNING
        VALUE(result) TYPE abap_component_tab.

    "! The database snapshot mt_table_org from the rows of a table that is
    "! the database state now - after the read and after a successful save
    METHODS set_snapshot
      IMPORTING
        !table TYPE STANDARD TABLE.

    "! The rows of the table as in the database now, past the table buffer,
    "! into a table of the type of mt_table
    METHODS get_db_rows
      RETURNING
        VALUE(result) TYPE REF TO data.

    "! A hash of the values of a row - every field but the client
    METHODS get_row_hash
      IMPORTING
        !row          TYPE any
      RETURNING
        VALUE(result) TYPE string.

    "! Whether the database still holds what the snapshot says - the
    "! optimistic check of the save: false if another user or program
    "! added, changed or deleted a row since it was read
    METHODS check_db_unchanged
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! ENQUEUE_E_TABLE for the table (and the logon client, if the table has
    "! one) with _scope 1, so a COMMIT WORK does not release it. A message
    "! names the user who holds it if it is taken. Standard ABAP only - on
    "! ABAP Cloud there is no such lock object and result is abap_true.
    METHODS lock
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! DEQUEUE_E_TABLE, the counterpart of lock( )
    METHODS unlock.

    "! The lock for the changes that start now (add, edit, multi-edit): taken
    "! once and kept in a stateful session until the changes are saved or
    "! dropped. result is abap_false if another user holds it.
    METHODS edit_lock
      RETURNING
        VALUE(result) TYPE abap_bool.

    "! Release the edit lock when nothing is left unsaved
    METHODS edit_unlock_if_idle.

    METHODS render_main_footer
      IMPORTING
        !page TYPE REF TO z2ui5_cl_ui5_view_builder.

    "! Show the view the page belongs to - or, embedded, tell the embedder
    METHODS render_main_display
      IMPORTING
        !page TYPE REF TO z2ui5_cl_ui5_view_builder.

    METHODS render_main_head
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_ui5_view_builder.

    METHODS on_event_main.

    METHODS on_event_layout.

    METHODS get_table_name.

    "! Settings from the URL or the launchpad startup parameters - they can
    "! only restrict what the caller configured (readonly=X)
    METHODS get_config_by_url.

    "! A request may change the table only outside read-only mode - the
    "! buttons are hidden then, this is the guard behind them
    METHODS check_change_allowed
      RETURNING
        VALUE(result) TYPE abap_bool.

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
    METHODS on_after_popup.

  PRIVATE SECTION.

ENDCLASS.


CLASS z2ui5_cl_tm_001 IMPLEMENTATION.

  METHOD get_txt.
        DATA texts TYPE z2ui5_cl_tm_context=>ty_s_data_element_text.

    TRY.

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

      CATCH cx_root.
        CLEAR result.
    ENDTRY.

    IF result IS INITIAL.
      result = fallback.
    ENDIF.

  ENDMETHOD.

  METHOD get_msg.

    TRY.
        result = z2ui5_cl_tm_context=>msg_get_by_msg( id = id
                                                      no = no
                                                      v1 = v1 )-text.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

    IF result IS INITIAL
        OR ( to_upper( result ) CS to_upper( id ) AND result CS no ).
      result = fallback.
    ENDIF.

  ENDMETHOD.

  METHOD button_save.

    DATA t_table     TYPE REF TO data.
    DATA t_table_del TYPE REF TO data.
    TYPES temp11 TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line.
DATA key_set     TYPE temp11.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <org>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del_org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <del>     TYPE any.
    FIELD-SYMBOLS <saved>   TYPE STANDARD TABLE.
      DATA keys TYPE z2ui5_cl_tm_001=>ty_t_keys.
      DATA key LIKE LINE OF keys.
      DATA comp TYPE abap_component_tab.
          DATA struct_desc TYPE REF TO cl_abap_structdescr.
          DATA table_desc TYPE REF TO cl_abap_tabledescr.
          DATA lx_type TYPE REF TO cx_root.
      DATA keys_org TYPE z2ui5_cl_tm_001=>ty_t_keys.
      FIELD-SYMBOLS <line_org> TYPE ANY.
        DATA index_org LIKE sy-tabix.
        DATA temp1 TYPE z2ui5_cl_tm_001=>ty_s_keys.
        DATA temp2 TYPE z2ui5_cl_tm_001=>ty_s_keys.
        DATA key_org LIKE temp1.
        DATA temp3 LIKE sy-subrc.
        DATA temp4 LIKE sy-subrc.
        DATA temp5 LIKE sy-tabix.
        DATA temp8 LIKE sy-subrc.
        DATA temp9 LIKE sy-subrc.
        DATA temp10 LIKE sy-tabix.
      DATA subrc TYPE i.
          DATA temp6 LIKE sy-subrc.
          DATA temp7 LIKE sy-subrc.
          DATA lx_db TYPE REF TO cx_root.
          DATA lx_tr TYPE REF TO cx_root.
      DATA db TYPE REF TO data.

    IF ms_transport IS INITIAL.

      mv_ucomm_tmp = client->get( )-event.
      client->nav_app_call( z2ui5_cl_popup_show_tr=>factory( ) ).

    ELSE.

      CLEAR mv_ucomm_tmp.

      IF mv_multi_edit = abap_true.
        ASSIGN mt_table->* TO <tab>.
      ELSE.
        " Filter issue
        ASSIGN mt_table_tmp->* TO <tab>.
      ENDIF.
      ASSIGN mt_table_org->* TO <org>.
      IF <tab> IS NOT ASSIGNED OR <org> IS NOT ASSIGNED.
        RETURN.
      ENDIF.

      " two rows with the same key cannot be saved

      keys = get_keys( <tab> ).

      LOOP AT keys INTO key.
        INSERT key-key INTO TABLE key_set.
        IF sy-subrc <> 0.
          client->message_box_display( text = `Two or more entries have the same key.`
                                       type = `error` ).
          RETURN.
        ENDIF.
      ENDLOOP.


      comp = z2ui5_cl_tm_context=>rtti_get_t_attri_by_table_name( mv_table ).

      TRY.


          struct_desc = cl_abap_structdescr=>create( comp ).


          table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA t_table     TYPE HANDLE table_desc.
          CREATE DATA t_table_del TYPE HANDLE table_desc.


        CATCH cx_root INTO lx_type.
          client->message_box_display( text = lx_type->get_text( )
                                       type = `error` ).
          RETURN.
      ENDTRY.

      ASSIGN t_table->* TO <tab_org>.
      ASSIGN t_table_del->* TO <del_org>.

      copy_corresponding( EXPORTING it_source = <tab>
                          CHANGING  ct_target = <tab_org> ).

      " the rows deleted since the last successful save or refresh: the keys
      " of the snapshot that are no longer in the table. Built anew on every
      " attempt - a refused save neither loses nor doubles them, and nothing
      " outside this save ever sees deletions that were not executed

      keys_org = get_keys( <org> ).

      LOOP AT <org> ASSIGNING <line_org>.

        index_org = sy-tabix.

        CLEAR temp1.

        READ TABLE keys_org INTO temp2 INDEX index_org.
        IF sy-subrc = 0.
          temp1 = temp2.
        ENDIF.

        key_org = temp1.



        temp4 = sy-subrc.
        temp5 = sy-tabix.
        READ TABLE keys_org INDEX index_org TRANSPORTING NO FIELDS.
        temp3 = sy-subrc.
        sy-subrc = temp4.
        sy-tabix = temp5.



        temp9 = sy-subrc.
        temp10 = sy-tabix.
        READ TABLE key_set WITH KEY table_line = key_org-key TRANSPORTING NO FIELDS.
        temp8 = sy-subrc.
        sy-subrc = temp9.
        sy-tabix = temp10.
        IF temp3 = 0 AND NOT temp8 = 0.
          " key fields only - enough for the DELETE and the transport keys
          INSERT INITIAL LINE INTO TABLE <del_org> ASSIGNING <del>.
          MOVE-CORRESPONDING <line_org> TO <del>.
        ENDIF.
      ENDLOOP.

      " the save holds the lock itself, whatever the session: in a stateless
      " one the edit lock is gone, in a stateful one this only counts it up
      IF lock( ) = abap_false.
        RETURN.
      ENDIF.

      " optimistic check: what the snapshot says must still be in the
      " database - changes of another user or program since the read are
      " not overwritten
      IF check_db_unchanged( ) = abap_false.
        unlock( ).
        client->message_box_display( text = |The entries of { mv_table } were changed in the database since they were read, by another user or program. | &&
                                            |Your changes were not saved - refresh the table and enter them again.|
                                     type = `error` ).
        RETURN.
      ENDIF.

      " the database first, the transport after it, one COMMIT for both: a
      " request entry written before a save that then failed (and was
      " rolled back) stayed on the request

      subrc = 0.
      TRY.

          IF <del_org> IS NOT INITIAL.
            " sy-subrc 4 only means a row was no longer in the database
            DELETE (mv_table) FROM TABLE <del_org>.
          ENDIF.



          temp7 = sy-subrc.
          READ TABLE mt_dfies WITH KEY keyflag = abap_false TRANSPORTING NO FIELDS.
          temp6 = sy-subrc.
          sy-subrc = temp7.
          IF temp6 = 0.
            MODIFY (mv_table) FROM TABLE <tab_org>.
          ELSE.
            DELETE (mv_table) FROM TABLE <tab_org>.
            INSERT (mv_table) FROM TABLE <tab_org>.
          ENDIF.
          subrc = sy-subrc.


        CATCH cx_root INTO lx_db.
          " a failed write (a duplicate key on INSERT, a value the column
          " does not take): the rows deleted just before are undone with it
          ROLLBACK WORK.
          unlock( ).
          client->message_box_display( text = lx_db->get_text( )
                                       type = `error` ).
          RETURN.
      ENDTRY.

      IF subrc <> 0.
        " not every row could be written - undone instead of left in the
        " LUW for the next commit to write half of it
        ROLLBACK WORK.
        unlock( ).
        client->message_box_display( text = |Not every row could be written to { mv_table } - the changes were not saved|
                                     type = `error` ).
        RETURN.
      ENDIF.

      TRY.

          " the rows of the table and the deletions, as before - without a
          " commit of their own, the COMMIT below takes them with the rows
          z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = t_table
                                                      iv_tabname   = mv_table
                                                      is_transport = ms_transport
                                                      iv_commit    = abap_false ).

          z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = t_table_del
                                                      iv_tabname   = mv_table
                                                      is_transport = ms_transport
                                                      iv_commit    = abap_false ).


        CATCH cx_root INTO lx_tr.
          " no transport, no save: the database changes go back as well
          ROLLBACK WORK.
          unlock( ).
          client->message_box_display( text = |{ get_msg( id       = '42'
                                                          no       = `316`
                                                          fallback = `Transport request error` ) }: { lx_tr->get_text( ) } - the changes were not saved|
                                       type = `error` ).
          RETURN.
      ENDTRY.

      COMMIT WORK AND WAIT.

      " a buffered table would show the old entries until the next buffer
      " synchronisation - here, in the app, after a refresh
      z2ui5_cl_tm_context=>db_reset_table_buffer( mv_table ).

      " what was saved is the database state the next save compares
      " against - the deletions are done, rows added since can be deleted
      " later. Read back, still under the lock, rather than taken from the
      " rows written: the hash is then the one of what the database holds

      db = get_db_rows( ).
      IF db IS BOUND.
        ASSIGN db->* TO <saved>.
        set_snapshot( <saved> ).
      ELSE.
        set_snapshot( <tab> ).
      ENDIF.

      unlock( ).

      client->message_toast_display( get_msg( id       = 'i18n'
                                              no       = `002`
                                              fallback = `Data was saved` ) ).

      CLEAR mv_change_active.
      edit_unlock_if_idle( ).

    ENDIF.

  ENDMETHOD.

  METHOD get_keys.

    FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <id> TYPE any.
      DATA temp8 TYPE ty_s_keys.
      DATA keys LIKE temp8.
      DATA value TYPE string.
      DATA dfies LIKE LINE OF mt_dfies.
        FIELD-SYMBOLS <key> TYPE any.

    LOOP AT table ASSIGNING <line>.


      UNASSIGN <id>.
      ASSIGN COMPONENT mc_row_id OF STRUCTURE <line> TO <id>.

      " the snapshot has no ROW_ID - its keys are matched by position

      CLEAR temp8.

      keys = temp8.
      IF <id> IS ASSIGNED.
        keys-row_id = <id>.
      ENDIF.

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

      INSERT keys INTO TABLE result.
      CLEAR keys.

    ENDLOOP.

  ENDMETHOD.

  METHOD transport_all.

    DATA t_table TYPE REF TO data.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_org> TYPE STANDARD TABLE.
      DATA comp TYPE abap_component_tab.
          DATA struct_desc TYPE REF TO cl_abap_structdescr.
          DATA table_desc TYPE REF TO cl_abap_tabledescr.

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


      comp = z2ui5_cl_tm_context=>rtti_get_t_attri_by_table_name( mv_table ).

      TRY.


          struct_desc = cl_abap_structdescr=>create( comp ).


          table_desc = cl_abap_tabledescr=>create( p_line_type  = struct_desc
                                                         p_table_kind = cl_abap_tabledescr=>tablekind_std ).

          CREATE DATA t_table TYPE HANDLE table_desc.

          ASSIGN t_table->* TO <tab_org>.

          copy_corresponding( EXPORTING it_source = <tab>
                              CHANGING  ct_target = <tab_org> ).

        CATCH cx_root ##NO_HANDLER.

      ENDTRY.
      TRY.
          " the entries of the table only - deletions reach the transport
          " with the save that executes them; pending ones are not in the
          " database, and a request must not delete what is still there
          z2ui5_cl_popup_show_tr=>add_data_to_tranport( ir_data      = t_table
                                                      iv_tabname   = mv_table
                                                      is_transport = ms_transport ).

        CATCH cx_root.
          " MESSAGE e316(42) INTO DATA(msg).  " Transport request error
          client->message_toast_display( get_msg( id       = '42'
                                                  no       = `316`
                                                  fallback = `Transport request error` ) ).
          RETURN.
      ENDTRY.
      " MESSAGE s215(rmps_general) WITH ms_transport-transport INTO msg. "Transporteintrag erfolgreich geschrieben (&1)
      client->message_toast_display( get_msg( id       = 'rmps_general'
                                              no       = `215`
                                              v1       = ms_transport-transport
                                              fallback = |Transport entries written ({ ms_transport-transport })| ) ).

    ENDIF.
  ENDMETHOD.

  METHOD get_comp.
    DATA id    TYPE c LENGTH 32.
    DATA selkz TYPE abap_bool.
        DATA comp TYPE abap_component_tab.
        DATA temp9 LIKE sy-subrc.
        DATA temp1 TYPE xsdboolean.
          DATA temp10 TYPE cl_abap_structdescr=>component_table.
          DATA temp11 LIKE LINE OF temp10.
          DATA temp15 TYPE REF TO cl_abap_datadescr.
        DATA temp12 LIKE sy-subrc.
        DATA temp2 TYPE xsdboolean.
          DATA temp13 TYPE cl_abap_structdescr=>component_table.
          DATA temp14 LIKE LINE OF temp13.
          DATA temp16 TYPE REF TO cl_abap_datadescr.

    TRY.


        comp = z2ui5_cl_tm_context=>rtti_get_t_attri_by_table_name( mv_table ).


        READ TABLE comp WITH KEY name = mc_row_id TRANSPORTING NO FIELDS.
        temp9 = sy-subrc.

        temp1 = boolc( temp9 = 0 ).
        IF temp1 = abap_false.

          CLEAR temp10.

          temp11-name = mc_row_id.

          temp15 ?= cl_abap_datadescr=>describe_by_data( id ).
          temp11-type = temp15.
          INSERT temp11 INTO TABLE temp10.
          INSERT LINES OF temp10 INTO TABLE result.
        ENDIF.

        READ TABLE comp WITH KEY name = 'SELKZ' TRANSPORTING NO FIELDS.
        temp12 = sy-subrc.

        temp2 = boolc( temp12 = 0 ).
        IF temp2 = abap_false.

          CLEAR temp13.

          temp14-name = 'SELKZ'.

          temp16 ?= cl_abap_datadescr=>describe_by_data( selkz ).
          temp14-type = temp16.
          INSERT temp14 INTO TABLE temp13.
          INSERT LINES OF temp13 INTO TABLE result.

        ENDIF.

        INSERT LINES OF comp INTO TABLE result.

      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD get_data.
    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    DATA t_comp TYPE abap_component_tab.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
        DATA key_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA key_table_desc TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <mt_table_tmp> TYPE data.
    t_comp = get_comp( ).
    TRY.


        new_struct_desc = cl_abap_structdescr=>create( t_comp ).


        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mt_table     TYPE HANDLE new_table_desc.
        CREATE DATA mt_table_tmp TYPE HANDLE new_table_desc.


        key_struct_desc = cl_abap_structdescr=>create( get_comp_key( t_comp ) ).


        key_table_desc = cl_abap_tabledescr=>create( p_line_type  = key_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mt_table_org TYPE HANDLE key_table_desc.

        ASSIGN mt_table->* TO <table>.

        " past the table buffer, as the check before the save reads it
        SELECT *
          FROM (mv_table)
          ORDER BY PRIMARY KEY
          INTO CORRESPONDING FIELDS OF TABLE <table>
          BYPASSING BUFFER.

        SORT <table>.

      CATCH cx_root ##NO_HANDLER.

    ENDTRY.

    IF <table> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    set_row_id( ).


    ASSIGN mt_table_tmp->* TO <mt_table_tmp>.

    " For Filter etc.
    <mt_table_tmp> = <table>.
    " For DB alignment before Save
    set_snapshot( <table> ).
  ENDMETHOD.

  METHOD get_comp_key.

    " a string: should the hash fail, the values themselves stand in
    DATA hash TYPE string.

    DATA comp LIKE LINE OF t_comp.
      DATA temp15 LIKE sy-subrc.
    DATA temp16 TYPE abap_componentdescr.
    DATA temp17 TYPE REF TO cl_abap_datadescr.
    LOOP AT t_comp INTO comp.

      READ TABLE mt_dfies WITH KEY fieldname = comp-name keyflag = abap_true TRANSPORTING NO FIELDS.
      temp15 = sy-subrc.
      IF temp15 = 0.
        INSERT comp INTO TABLE result.
      ENDIF.
    ENDLOOP.


    CLEAR temp16.
    temp16-name = mc_row_hash.

    temp17 ?= cl_abap_datadescr=>describe_by_data( hash ).
    temp16-type = temp17.
    INSERT temp16 INTO TABLE result.

  ENDMETHOD.

  METHOD set_snapshot.

    FIELD-SYMBOLS <org> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line_org> TYPE ANY.
      DATA index LIKE sy-tabix.
      FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <hash> TYPE any.

    ASSIGN mt_table_org->* TO <org>.
    IF <org> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    copy_corresponding( EXPORTING it_source = table
                        CHANGING  ct_target = <org> ).

    " copy_corresponding keeps the order - row n of <org> is row n of table

    LOOP AT <org> ASSIGNING <line_org>.

      index = sy-tabix.

      READ TABLE table INDEX index ASSIGNING <line>.

      ASSIGN COMPONENT mc_row_hash OF STRUCTURE <line_org> TO <hash>.
      IF sy-subrc = 0 AND <line> IS ASSIGNED.
        <hash> = get_row_hash( <line> ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_db_rows.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <db>  TYPE STANDARD TABLE.

    ASSIGN mt_table->* TO <tab>.
    IF <tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CREATE DATA result LIKE <tab>.
    ASSIGN result->* TO <db>.

    " past the table buffer: a buffered table may answer with the state of
    " before another server's change, and the save compares with the
    " database
    TRY.
        SELECT *
          FROM (mv_table)
          ORDER BY PRIMARY KEY
          INTO CORRESPONDING FIELDS OF TABLE <db>
          BYPASSING BUFFER.
      CATCH cx_root.
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

  METHOD get_row_hash.

    DATA values TYPE string.

    DATA dfies LIKE LINE OF mt_dfies.
      FIELD-SYMBOLS <value> TYPE any.
      DATA value TYPE string.
    LOOP AT mt_dfies INTO dfies WHERE datatype <> 'CLNT' AND fieldname <> 'MANDT'.

      ASSIGN COMPONENT dfies-fieldname OF STRUCTURE row TO <value>.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      " each value with its length in front, as in get_keys

      value = |{ <value> }|.
      values = |{ values }{ strlen( value ) }:{ value }|.
    ENDLOOP.

    TRY.
        cl_abap_message_digest=>calculate_hash_for_char( EXPORTING if_data       = values
                                                         IMPORTING ef_hashstring = result ).
      CATCH cx_root.
        " no hash - the values themselves compare just as well
        result = values.
    ENDTRY.

  ENDMETHOD.

  METHOD check_db_unchanged.

    TYPES temp12 TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
DATA state_db       TYPE temp12.
    TYPES temp13 TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
DATA state_snapshot TYPE temp13.

    FIELD-SYMBOLS <db>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <org> TYPE STANDARD TABLE.

    DATA db TYPE REF TO data.
    DATA keys TYPE z2ui5_cl_tm_001=>ty_t_keys.
    FIELD-SYMBOLS <line> TYPE ANY.
      DATA temp17 TYPE z2ui5_cl_tm_001=>ty_s_keys.
      DATA temp18 TYPE z2ui5_cl_tm_001=>ty_s_keys.
      DATA key LIKE temp17.
      DATA temp19 LIKE LINE OF state_db.
    FIELD-SYMBOLS <line_org> TYPE ANY.
      DATA temp20 TYPE z2ui5_cl_tm_001=>ty_s_keys.
      DATA temp21 TYPE z2ui5_cl_tm_001=>ty_s_keys.
      FIELD-SYMBOLS <hash> TYPE any.
        DATA temp22 LIKE LINE OF state_snapshot.
    DATA temp3 TYPE xsdboolean.
    db = get_db_rows( ).
    ASSIGN mt_table_org->* TO <org>.
    IF db IS NOT BOUND OR <org> IS NOT ASSIGNED.
      " nothing to compare with - the write itself reports what is wrong
      result = abap_true.
      RETURN.
    ENDIF.
    ASSIGN db->* TO <db>.


    keys = get_keys( <db> ).

    LOOP AT <db> ASSIGNING <line>.
      " get_keys( ) returns one key per row, in the order of the rows

      CLEAR temp17.

      READ TABLE keys INTO temp18 INDEX sy-tabix.
      IF sy-subrc = 0.
        temp17 = temp18.
      ENDIF.

      key = temp17.

      temp19 = |{ key-key }#{ get_row_hash( <line> ) }|.
      INSERT temp19 INTO TABLE state_db.
    ENDLOOP.

    keys = get_keys( <org> ).

    LOOP AT <org> ASSIGNING <line_org>.

      CLEAR temp20.

      READ TABLE keys INTO temp21 INDEX sy-tabix.
      IF sy-subrc = 0.
        temp20 = temp21.
      ENDIF.
      key = temp20.

      ASSIGN COMPONENT mc_row_hash OF STRUCTURE <line_org> TO <hash>.
      IF sy-subrc = 0.

        temp22 = |{ key-key }#{ <hash> }|.
        INSERT temp22 INTO TABLE state_snapshot.
      ENDIF.
    ENDLOOP.


    temp3 = boolc( state_db = state_snapshot ).
    result = temp3.

  ENDMETHOD.

  METHOD lock.

    DATA lv_fm      TYPE c LENGTH 30 VALUE `ENQUEUE_E_TABLE`.
    DATA lv_mode    TYPE c LENGTH 1 VALUE `E`.
    DATA lv_scope   TYPE c LENGTH 1 VALUE `1`.
    DATA lv_tabname TYPE c LENGTH 30.
    DATA lr_varkey  TYPE REF TO data.
    FIELD-SYMBOLS <varkey> TYPE any.
    DATA temp23 LIKE sy-subrc.
    DATA temp24 LIKE sy-subrc.
        DATA subrc LIKE sy-subrc.
        DATA temp25 TYPE string.
        DATA user LIKE temp25.
        DATA lx TYPE REF TO cx_root.
        DATA temp26 TYPE string.

    result = abap_true.
    IF z2ui5_cl_tm_context=>check_abap_cloud( ) = abap_true.
      RETURN.
    ENDIF.

    lv_tabname = mv_table.
    " a client-dependent table is locked in the logon client only


    temp24 = sy-subrc.
    READ TABLE mt_dfies WITH KEY keyflag = abap_true datatype = 'CLNT' TRANSPORTING NO FIELDS.
    temp23 = sy-subrc.
    sy-subrc = temp24.
    IF temp23 = 0.
      " VARKEY is passed by reference and must have the exact type of the
      " module's parameter (RSTABLE-VARKEY - its length differs between
      " releases); created by name, so the class also compiles on ABAP Cloud
      TRY.
          CREATE DATA lr_varkey TYPE (`RSTABLE-VARKEY`).
          ASSIGN lr_varkey->* TO <varkey>.
          <varkey> = sy-mandt.
        CATCH cx_root ##NO_HANDLER.
          " no such type: the whole table is locked, as without a client
      ENDTRY.
    ENDIF.

    " called by name, so the class compiles where the module does not exist
    TRY.
        IF <varkey> IS NOT ASSIGNED.
          CALL FUNCTION lv_fm
            EXPORTING
              mode_rstable   = lv_mode
              tabname        = lv_tabname
              _scope         = lv_scope
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              error_message  = 3
              OTHERS         = 4.
        ELSE.
          CALL FUNCTION lv_fm
            EXPORTING
              mode_rstable   = lv_mode
              tabname        = lv_tabname
              varkey         = <varkey>
              _scope         = lv_scope
            EXCEPTIONS
              foreign_lock   = 1
              system_failure = 2
              error_message  = 3
              OTHERS         = 4.
        ENDIF.

        subrc = sy-subrc.

        temp25 = sy-msgv1.

        user = temp25.

      CATCH cx_root INTO lx.
        client->message_box_display( text = |Table { mv_table } could not be locked: { lx->get_text( ) }|
                                     type = `error` ).
        result = abap_false.
        RETURN.
    ENDTRY.

    CASE subrc.
      WHEN 0.
      WHEN 1.

        temp26 = sy-uname.
        IF condense( user ) = condense( temp26 ).
          " the own lock of another session: a second browser tab, or the
          " session before a reload, which still runs until it times out
          client->message_box_display( text = |Table { mv_table } is being edited in another session of yours (another browser tab or window). | &&
                                              |Save or leave the table there - a closed session releases it when it times out.|
                                       type = `error` ).
        ELSE.
          client->message_box_display( text = |Table { mv_table } is being edited by user { user }. Try again once that user has saved or left the table.|
                                       type = `error` ).
        ENDIF.
        result = abap_false.
      WHEN OTHERS.
        client->message_box_display( text = |Table { mv_table } could not be locked - the lock server did not answer.|
                                     type = `error` ).
        result = abap_false.
    ENDCASE.

  ENDMETHOD.

  METHOD unlock.

    DATA lv_fm      TYPE c LENGTH 30 VALUE `DEQUEUE_E_TABLE`.
    DATA lv_mode    TYPE c LENGTH 1 VALUE `E`.
    DATA lv_scope   TYPE c LENGTH 1 VALUE `1`.
    DATA lv_tabname TYPE c LENGTH 30.
    DATA lr_varkey  TYPE REF TO data.
    FIELD-SYMBOLS <varkey> TYPE any.
    DATA temp27 LIKE sy-subrc.

    IF z2ui5_cl_tm_context=>check_abap_cloud( ) = abap_true.
      RETURN.
    ENDIF.

    lv_tabname = mv_table.

    READ TABLE mt_dfies WITH KEY keyflag = abap_true datatype = 'CLNT' TRANSPORTING NO FIELDS.
    temp27 = sy-subrc.
    IF temp27 = 0.
      " VARKEY is passed by reference and must have the exact type of the
      " module's parameter (RSTABLE-VARKEY - its length differs between
      " releases); created by name, so the class also compiles on ABAP Cloud
      TRY.
          CREATE DATA lr_varkey TYPE (`RSTABLE-VARKEY`).
          ASSIGN lr_varkey->* TO <varkey>.
          <varkey> = sy-mandt.
        CATCH cx_root ##NO_HANDLER.
          " no such type: the whole table is locked, as without a client
      ENDTRY.
    ENDIF.

    TRY.
        IF <varkey> IS NOT ASSIGNED.
          CALL FUNCTION lv_fm
            EXPORTING
              mode_rstable = lv_mode
              tabname      = lv_tabname
              _scope       = lv_scope
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.
        ELSE.
          CALL FUNCTION lv_fm
            EXPORTING
              mode_rstable = lv_mode
              tabname      = lv_tabname
              varkey       = <varkey>
              _scope       = lv_scope
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
        " a lock that cannot be released goes with the session
    ENDTRY.

  ENDMETHOD.

  METHOD edit_lock.

    result = abap_true.
    IF mv_lock_active = abap_true
        OR ms_config-read_only = abap_true
        OR z2ui5_cl_tm_context=>check_abap_cloud( ) = abap_true.
      RETURN.
    ENDIF.

    result = lock( ).
    IF result = abap_false.
      RETURN.
    ENDIF.

    " a lock outlives the request only in a stateful session - kept until
    " edit_unlock_if_idle, release_lock or the end of the session (browser
    " closed, session timeout)
    mv_lock_active = abap_true.
    client->set_session_stateful( abap_true ).

  ENDMETHOD.

  METHOD edit_unlock_if_idle.

    IF mv_change_active = abap_false AND mv_multi_edit = abap_false.
      release_lock( ).
    ENDIF.

  ENDMETHOD.

  METHOD z2ui5_if_tm_001~release_lock.

    IF client IS BOUND.
      me->client = client.
    ENDIF.

    IF mv_lock_active = abap_false.
      RETURN.
    ENDIF.

    unlock( ).
    CLEAR mv_lock_active.
    IF me->client IS BOUND.
      me->client->set_session_stateful( abap_false ).
    ENDIF.

  ENDMETHOD.

  METHOD get_dfies.
    DATA temp28 LIKE LINE OF mt_dfies.
    DATA dfies LIKE REF TO temp28.

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

    " before get_data - the key fields of the snapshot come from mt_dfies
    get_dfies( ).

    get_data( ).

    get_layout( ).

  ENDMETHOD.

  METHOD get_table_name.
      DATA tab TYPE string.
        DATA startup_params TYPE z2ui5_if_client=>ty_t_name_value.
        DATA temp29 TYPE string.
        DATA temp30 TYPE z2ui5_if_client=>ty_s_name_value.
    DATA descr TYPE REF TO cl_abap_typedescr.

    " a table set by factory( ) or set_app_data( ) is checked as well
    IF mv_table IS INITIAL.

      " TABLE per Parameter?

      tab = z2ui5_cl_tm_context=>url_param_get( val = 'table'
                                                url = client->get( )-s_config-search ).

      IF tab IS INITIAL.

        startup_params = client->get( )-t_comp_params.

        CLEAR temp29.

        READ TABLE startup_params INTO temp30 WITH KEY n = 'table'.
        IF sy-subrc = 0.
          temp29 = temp30-v.
        ENDIF.
        tab = temp29.
      ENDIF.

      " no fallback table: without the parameter the app asks for one
      " (render_table_input) instead of opening a table nobody chose
      mv_table = to_upper( tab ).
      IF mv_table IS INITIAL.
        RETURN.
      ENDIF.

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
    DATA temp31 LIKE LINE OF mo_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp31.
      DATA lv_index LIKE sy-tabix.
    DATA cells TYPE REF TO z2ui5_cl_ui5_view_builder.
        DATA sub_col TYPE string.
        DATA index TYPE i.
        DATA line LIKE LINE OF mo_layout->ms_layout-t_layout.
        DATA subcol LIKE LINE OF layout->t_sub_col.
          DATA temp32 LIKE sy-subrc.
            FIELD-SYMBOLS <temp33> LIKE LINE OF mo_layout->ms_layout-t_layout.
            DATA temp34 LIKE sy-tabix.
          DATA column TYPE string.
      DATA cell TYPE string.
    ASSIGN mt_table->* TO <tab>.


    table = page->ele( `Table`
                      )->a( n = `growing` v = 'true'
                      )->a( n = `growingThreshold` v = '100'
                      )->a( n = `width` v = 'auto'
                      )->a( n = `sticky` v = `ColumnHeaders`
                      )->a( n = `autoPopinMode` b = abap_true
                      )->a( n = `items` v = client->_bind( <tab> )
                      )->a( n = `headerText` t = mv_table ).


    headder = table->ele( `headerToolbar`
                        )->ele( `OverflowToolbar`
                        )->tag( `Title`
                        )->a( n = `text` t = mv_table
                        )->tag( `ToolbarSpacer`
                        )->tag( `SearchField`
                        )->a( n = `value` v = client->_bind( mv_search_value )
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

          " a subcolumn that names no position keeps the previous one

          READ TABLE mo_layout->ms_layout-t_layout WITH KEY fname = subcol-fname TRANSPORTING NO FIELDS.
          temp32 = sy-subrc.
          IF temp32 = 0.


            temp34 = sy-tabix.
            READ TABLE mo_layout->ms_layout-t_layout WITH KEY fname = subcol-fname ASSIGNING <temp33>.
            sy-tabix = temp34.
            IF sy-subrc <> 0.
              ASSERT 1 = 0.
            ENDIF.
            line = <temp33>.
          ENDIF.

          " relative to the row: a namespaced column (/CWM/XCWMAT) is a
          " level of the row in the model, {CWM/XCWMAT}

          column = |{ line-tlabel }: \{{ z2ui5_cl_ui5_view_builder=>component_path( subcol-fname ) }\}|.
          IF line-reference_field IS NOT INITIAL.
            column = |{ column } \{{ z2ui5_cl_ui5_view_builder=>component_path( line-reference_field ) }\}|.
          ENDIF.

          IF index = 1.
            sub_col = column.
          ELSE.
            sub_col = |{ sub_col }{ cl_abap_char_utilities=>cr_lf }{ column }|.
          ENDIF.
        ENDLOOP.
      ENDIF.


      cell = |\{{ z2ui5_cl_ui5_view_builder=>component_path( layout->fname ) }\}|.
      IF layout->reference_field IS NOT INITIAL.
        cell = |{ cell } \{{ z2ui5_cl_ui5_view_builder=>component_path( layout->reference_field ) }\}|.
      ENDIF.

      IF layout->t_sub_col IS NOT INITIAL.
        cells->ele( `ObjectIdentifier`
            )->a( n = `title` v = cell
            )->a( n = `text` v = sub_col ).
      ELSE.
        cells->ele( `ObjectIdentifier`
            )->a( n = `text` v = cell ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD render_main_head.
      DATA view TYPE REF TO z2ui5_cl_ui5_view_builder.
        DATA temp35 TYPE string_table.
    IF mo_parent_view IS INITIAL.


      view = z2ui5_cl_ui5_view_builder=>factory(
                       )->ele( n = `View` ns = `mvc`
                       )->a( n = `xmlns` v = `sap.m`
                       )->a( n = `xmlns:mvc` v = `sap.ui.core.mvc`
                       )->a( n = `xmlns:core` v = `sap.ui.core`
                       )->a( n = `xmlns:table` v = `sap.ui.table`
                       )->a( n = `displayBlock` v = `true`
                       )->a( n = `height` v = `100%` ).

      IF client->get( )-check_launchpad_active = abap_true.

        result = view->ele( `Page`
                     )->a( n = `showHeader` b = abap_false ).
        " inside the launchpad the shell header carries the title, set once
        " the response has been rendered

        CLEAR temp35.
        INSERT mv_table INTO TABLE temp35.
        client->follow_up_action(
            val   = z2ui5_if_client=>cs_event-set_title_launchpad
            t_arg = temp35 ).

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

    IF mv_multi_edit = abap_true AND ms_config-read_only = abap_false.
      footer->tag( `Button`
          )->a( n = `text` v = get_txt( roll = 'MLCCS_D_XDELETE' fallback = `Delete` )
          )->a( n = `type` v = 'Reject'
          )->a( n = `icon` v = 'sap-icon://delete'
          )->a( n = `press` v = client->_event( val = 'BUTTON_DELETE' )
          )->tag( `Button`
          )->a( n = `text` v = get_txt( roll = '/SCMTMS/COCO_COPY_IND' fallback = `Copy` )
          )->a( n = `icon` v = 'sap-icon://copy'
          )->a( n = `press` v = client->_event( val = 'BUTTON_COPY' ) ).
    ENDIF.

    IF ms_config-read_only = abap_true.
      " display only: refresh and the layout, nothing that changes the
      " table or a transport request
      footer->tag( `Button`
          )->a( n = `icon` v = 'sap-icon://refresh'
          )->a( n = `text` v = get_txt( roll = 'REFRESH_F8' fallback = `Refresh` )
          )->a( n = `press` v = client->_event( 'BUTTON_REFRESH' )
          )->a( n = `type` v = 'Default' ).
      render_main_display( page ).
      RETURN.
    ENDIF.

    footer->tag( `Button`
        )->a( n = `icon` v = 'sap-icon://add'
        )->a( n = `text` v = get_txt( roll = 'RSLPO_GUI_ADDPART' fallback = `Add` )
        )->a( n = `press` v = client->_event( 'BUTTON_ADD' )
        )->a( n = `type` v = 'Default'
        )->tag( `Button`
        )->a( n = `icon` v = 'sap-icon://refresh'
        )->a( n = `text` v = get_txt( roll = 'REFRESH_F8' fallback = `Refresh` )
        )->a( n = `press` v = client->_event( 'BUTTON_REFRESH' )
        )->a( n = `type` v = 'Default'
        )->tag( `Button`
        )->a( n = `enabled` v = client->_bind( mv_change_active )
        )->a( n = `text` v = get_txt( roll = 'SICHERN' fallback = `Save` )
        )->a( n = `press` v = client->_event( 'BUTTON_SAVE' )
        )->a( n = `type` v = 'Success' ).

    footer->ele( `MenuButton`
        )->a( n = `activeIcon` v = 'sap-icon://action-settings'
        )->ele( `menu`
        )->ele( `Menu`
        )->tag( `MenuItem`
        )->a( n = `icon`  v = 'sap-icon://shipping-status'
        )->a( n = `text`  v = get_txt( roll = 'ALLES_FUB' fallback = `Transport All Entries` )
        )->a( n = `press` v = client->_event( 'TRANSPORT_ALL' )
        )->tag( `MenuItem`
        )->a( n = `icon`  v = 'sap-icon://shipping-status'
        )->a( n = `text`  v = get_txt( roll = 'FUNCCHANGE' fallback = `Change Transport Request` )
        )->a( n = `press` v = client->_event( 'TRANSPORT_CHANGE' )
        )->tag( `MenuItem`
        )->a( n = `icon`  v = 'sap-icon://key-user-settings'
        )->a( n = `text`  v = get_txt( roll = 'POWL_ADMIN_TY' fallback = `Multi-Edit` )
        )->a( n = `press` v = client->_event( 'BUTTON_EDIT' ) ).

    render_main_display( page ).
  ENDMETHOD.

  METHOD render_main_display.

    IF mo_parent_view IS INITIAL.

      client->view_display( page->stringify( ) ).

    ELSE.

      mv_view_display = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD row_select.

    DATA lt_arg TYPE string_table.
    DATA temp37 LIKE sy-subrc.
    DATA ls_arg LIKE LINE OF lt_arg.
    FIELD-SYMBOLS <temp18> LIKE LINE OF lt_arg.
    DATA temp19 LIKE sy-tabix.
    DATA read_only LIKE ms_config-read_only.
    lt_arg = client->get( )-t_event_arg.

    READ TABLE lt_arg INDEX 1 TRANSPORTING NO FIELDS.
    temp37 = sy-subrc.
    IF NOT temp37 = 0.
      RETURN.
    ENDIF.



    temp19 = sy-tabix.
    READ TABLE lt_arg INDEX 1 ASSIGNING <temp18>.
    sy-tabix = temp19.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_arg = <temp18>.

    " the row is shown in any case - read-only while another user edits
    " the table (edit_lock has said who)

    read_only = ms_config-read_only.
    IF read_only = abap_false AND edit_lock( ) = abap_false.
      read_only = abap_true.
    ENDIF.

    client->nav_app_call( z2ui5_cl_tm_pop=>factory( io_table     = mt_table
                                                    iv_row_id    = ls_arg
                                                    it_dfies     = mt_dfies
                                                    is_layout    = mo_layout
                                                    iv_edit_mode = abap_true
                                                    iv_tabname   = mv_table
                                                    iv_read_only = read_only ) ).

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

      get_config_by_url( ).

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
        DATA temp4 TYPE xsdboolean.
        FIELD-SYMBOLS <selkz_column> TYPE z2ui5_cl_layo_manager=>ty_s_positions.
          DATA temp5 TYPE xsdboolean.
          FIELD-SYMBOLS <tab_edit> TYPE data.
          FIELD-SYMBOLS <tab_edit_tmp> TYPE data.
    CASE client->get( )-event.

      WHEN 'BACK'.

        release_lock( ).
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'BUTTON_REFRESH'.

        " the database state again - nothing left to save
        get_data( ).
        CLEAR mv_change_active.
        edit_unlock_if_idle( ).

      WHEN 'BUTTON_SEARCH'.

        search( ).

      WHEN 'BUTTON_SAVE'.

        IF check_change_allowed( ) = abap_true.
          button_save( ).
        ENDIF.

      WHEN 'BUTTON_DELETE'.

        IF check_change_allowed( ) = abap_true.
          button_delete( ).
        ENDIF.

      WHEN 'BUTTON_ADD'.

        IF check_change_allowed( ) = abap_false OR edit_lock( ) = abap_false.
          RETURN.
        ENDIF.

        client->nav_app_call( z2ui5_cl_tm_pop=>factory( io_table     = mt_table
                                                        iv_row_id    = ``
                                                        it_dfies     = mt_dfies
                                                        is_layout    = mo_layout
                                                        iv_edit_mode = abap_false
                                                        iv_tabname   = mv_table ) ).

      WHEN 'BUTTON_EDIT'.

        IF check_change_allowed( ) = abap_false.
          RETURN.
        ENDIF.
        IF mv_multi_edit = abap_false AND edit_lock( ) = abap_false.
          RETURN.
        ENDIF.


        temp4 = boolc( mv_multi_edit = abap_false ).
        mv_multi_edit = temp4.
        " ASSIGN, not REF #( ... OPTIONAL ): the 702 downport drops the OPTIONAL

        READ TABLE mo_layout->ms_layout-t_layout WITH KEY fname = 'SELKZ' ASSIGNING <selkz_column>.
        IF <selkz_column> IS ASSIGNED.

          temp5 = boolc( mv_multi_edit = abap_true ).
          <selkz_column>-visible = temp5.
        ENDIF.

        IF mv_multi_edit = abap_true.
          CLEAR mv_search_value.
          search( ).
        ELSE.
          " multi-edit changes mt_table only; normal mode saves and searches
          " from mt_table_tmp, which would drop those changes

          ASSIGN mt_table->* TO <tab_edit>.

          ASSIGN mt_table_tmp->* TO <tab_edit_tmp>.
          " a cell changed without a submit has not set mv_change_active
          IF <tab_edit_tmp> <> <tab_edit>.
            mv_change_active = abap_true.
          ENDIF.
          <tab_edit_tmp> = <tab_edit>.
          edit_unlock_if_idle( ).
        ENDIF.

        get_layout( ).

        render_main( ).

      WHEN 'ROW_SELECT'.

        row_select( ).

      WHEN 'INPUT'.

        check_input( ).

      WHEN 'BUTTON_COPY'.

        IF check_change_allowed( ) = abap_true.
          button_copy( ).
        ENDIF.

    ENDCASE.
  ENDMETHOD.

  METHOD factory.

    CREATE OBJECT result.
    result->mv_table  = to_upper( condense( i_table ) ).
    result->ms_config = i_config.

  ENDMETHOD.

  METHOD get_config_by_url.

    DATA value TYPE string.
      DATA startup_params TYPE z2ui5_if_client=>ty_t_name_value.
      DATA temp38 TYPE string.
      DATA temp39 TYPE z2ui5_if_client=>ty_s_name_value.
    value = z2ui5_cl_tm_context=>url_param_get( val = 'readonly'
                                                      url = client->get( )-s_config-search ).
    IF value IS INITIAL.

      startup_params = client->get( )-t_comp_params.

      CLEAR temp38.

      READ TABLE startup_params INTO temp39 WITH KEY n = 'readonly'.
      IF sy-subrc = 0.
        temp38 = temp39-v.
      ENDIF.
      value = temp38.
    ENDIF.

    " only ever switched on here: a URL must not lift the read-only mode a
    " caller configured
    value = to_upper( condense( value ) ).
    IF value = `X` OR value = `TRUE` OR value = `1`.
      ms_config-read_only = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_change_allowed.

    DATA temp6 TYPE xsdboolean.
    temp6 = boolc( ms_config-read_only = abap_false ).
    result = temp6.
    IF result = abap_false.
      client->message_toast_display( `The table is shown in read-only mode.` ).
    ENDIF.

  ENDMETHOD.

  METHOD z2ui5_if_tm_001~set_app_data.
    mv_table = data.
  ENDMETHOD.

  METHOD on_event_layout.
    z2ui5_cl_layo_pop=>on_event_layout( client = client
                                        layout = mo_layout ).
  ENDMETHOD.

  METHOD on_after_layout.
        DATA temp40 TYPE REF TO z2ui5_cl_layo_pop.
        DATA app LIKE temp40.

    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.


        temp40 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp40.

        mo_layout = app->mo_layout.

        IF app->mv_rerender = abap_true.
          " subcolumns need rerendering to work ..
          render_main( ).
        ENDIF.
      CATCH cx_root ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD get_layout.

    DATA class TYPE abap_abstypename.
      DATA control LIKE z2ui5_cl_layo_manager=>m_table.
    class = cl_abap_classdescr=>get_class_name( me ).
    class = shift_left( val = class sub = `\CLASS=` ).

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
                                                handle04 = ms_config-layout_handle04 ).

  ENDMETHOD.

  METHOD on_after_transport.
        DATA temp41 TYPE REF TO z2ui5_cl_popup_show_tr.
        DATA app LIKE temp41.
    " Kommen wir aus einer anderen APP
    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " War es das Layout?

        temp41 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp41.

        " a cancelled popup returns no request - the one chosen before stays
        IF app->ms_transport IS NOT INITIAL.
          ms_transport = app->ms_transport.
        ENDIF.

        IF app->ms_transport IS NOT INITIAL.

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


      CATCH cx_root ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD on_event_transport.

    IF ms_config-read_only = abap_true.
      RETURN.
    ENDIF.

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
    " ASSIGN, not REF #( ... OPTIONAL ): the 702 downport drops the OPTIONAL
    FIELD-SYMBOLS <selkz_column> TYPE z2ui5_cl_layo_manager=>ty_s_positions.
    FIELD-SYMBOLS <tab> TYPE data.
    DATA table TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA toolbar TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA columns TYPE REF TO z2ui5_cl_ui5_view_builder.
    DATA temp42 LIKE LINE OF mo_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp42.
      DATA lv_index LIKE sy-tabix.
      DATA path TYPE string.
      DATA temp43 TYPE string.
      DATA col TYPE REF TO z2ui5_cl_ui5_view_builder.
    READ TABLE mo_layout->ms_layout-t_layout WITH KEY fname = 'SELKZ' ASSIGNING <selkz_column>.
    IF <selkz_column> IS ASSIGNED.
      <selkz_column>-visible = abap_true.
    ENDIF.


    ASSIGN mt_table->* TO <tab>.


    table = page->ele( `FlexBox`
                      )->a( n = `height` v = '85vh'
                      )->ele( n = `Table` ns = `table`
                      )->a( n = `alternateRowColors` v = 'true'
                      )->a( n = `visibleRowCountMode` v = 'Auto'
                      )->a( n = `selectionMode` v = 'None'
                      )->a( n = `selectionBehavior` v = 'RowSelector'
                      )->a( n = `rows` v = client->_bind( <tab> ) ).


    toolbar = table->ele( n = `extension` ns = `table`
                        )->ele( `OverflowToolbar`
                        )->tag( `ToolbarSpacer` ).

    z2ui5_cl_layo_pop=>render_layout_function( xml    = toolbar
                                               client = client
                                               layout = mo_layout ).


    columns = table->ele( n = `columns` ns = `table` ).



    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.

      lv_index = sy-tabix.
      " relative to the row: a namespaced column (/CWM/XCWMAT) is a level
      " of the row in the model - the cell, the sorter and the filter read
      " CWM/XCWMAT
      " abap2ui5lint-disable-next-line unknown-builder-method -- component_path( ) is a static of the builder since abap2UI5 40e8a073, the linter 0.11 does not know it yet

      path = z2ui5_cl_ui5_view_builder=>component_path( layout->fname ).


      IF layout->width IS NOT INITIAL.
        temp43 = client->_bind( val = layout->width tab = mo_layout->ms_layout-t_layout tab_index = lv_index ).
      ELSE.
        CLEAR temp43.
      ENDIF.

      col = columns->ele( n = `Column` ns = `table`
                      )->a( n = `visible` v = client->_bind( val       = layout->visible
                                                      tab       = mo_layout->ms_layout-t_layout
                                                      tab_index = lv_index )
                      )->a( n = `width` v = temp43
                      )->a( n = `sortProperty` v = path
                      )->a( n = `filterProperty` v = path
                      )->tag( `Text`
                      )->a( n = `text` v = layout->tlabel
                      )->ele( n = `template` ns = `table` ).

      IF layout->fname = 'SELKZ'.

        col->tag( `CheckBox`
            )->a( n = `selected` v = |\{{ path }\}| ).

      ELSE.

        col->tag( `Input`
            )->a( n = `submit` v = client->_event( val = 'INPUT' )
            )->a( n = `value` v = |\{{ path }\}| ).

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
        INSERT <line> INTO TABLE <tab> ASSIGNING <copy>.

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
        INSERT <copy> INTO TABLE <tab_tmp>.

        mv_change_active = abap_true.


      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD on_after_popup.

    FIELD-SYMBOLS <tab>         TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_changed> TYPE STANDARD TABLE.
        DATA temp44 TYPE REF TO z2ui5_cl_tm_pop.
        DATA app LIKE temp44.
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

        temp44 ?= client->get_app( client->get( )-s_draft-id_prev_app ).

        app = temp44.

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
              INSERT <line_changed> INTO TABLE <tab_tmp>.
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

        " cancelled, or shown read-only: no change - no lock needed
        edit_unlock_if_idle( ).

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
      INSERT INITIAL LINE INTO TABLE ct_target ASSIGNING <ls_target>.
      MOVE-CORRESPONDING <ls_source> TO <ls_target>.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
