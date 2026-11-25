"! <p class="shorttext synchronized" lang="en">Table maintenance - view cluster</p>
"!
"! Start with: <em>?app_start=z2ui5_cl_tm_002&amp;tables=T001,T001W&amp;title=Customizing</em>
"!
"! Several tables on one page: a tab per table with its description and its
"! number of entries, the maintenance of the selected table below it -
"! z2ui5_cl_tm_001, embedded through z2ui5_if_tm_001 - and one transport
"! request for all of them, handed on from table to table. A tab change while
"! the shown table has unsaved changes asks first.
"!
"! The tables come from the URL parameter <em>tables</em> (comma separated)
"! or the launchpad startup parameter of the same name; <em>title</em> names
"! the page.
CLASS z2ui5_cl_tm_002 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_table,
        tabname TYPE string,
        text    TYPE string,
        count   TYPE string,
      END OF ty_s_table.
    TYPES ty_t_table TYPE STANDARD TABLE OF ty_s_table WITH EMPTY KEY.

    DATA mv_selected_key TYPE string.
    " PUBLIC: the embedded app binds its own attributes, and the framework
    " reaches them through this reference
    DATA mo_app          TYPE REF TO z2ui5_cl_tm_001.

  PROTECTED SECTION.
    DATA mt_table          TYPE ty_t_table.
    DATA mv_title          TYPE string.
    DATA client            TYPE REF TO z2ui5_if_client.
    DATA check_initialized TYPE abap_bool.
    " the tab whose maintenance is shown
    DATA mv_shown_key      TYPE string.
    " the tab asked for while the shown one had unsaved changes
    DATA mv_pending_key    TYPE string.
    DATA ms_transport      TYPE z2ui5_cl_tm_context=>ty_s_transport.

    METHODS init.

    "! The page the embedded app renders into - built anew on every
    "! roundtrip, the tab bar with the current counts on top
    METHODS render_page
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_ui5_view_builder.

    METHODS run_sub_app.

    "! Back from the confirmation popup: switch to the asked tab, or stay
    METHODS on_after_confirm.

    METHODS get_count
      IMPORTING
        tabname       TYPE string
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
ENDCLASS.


CLASS z2ui5_cl_tm_002 IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.
      init( ).
    ENDIF.

    IF mt_table IS INITIAL.
      client->message_box_display( text = `No tables - start the app with the URL parameter tables, e.g. tables=T001,T001W`
                                   type = `error` ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true AND mv_pending_key IS NOT INITIAL.
      on_after_confirm( ).
    ENDIF.

    run_sub_app( ).

  ENDMETHOD.


  METHOD init.

    DATA lt_name TYPE string_table.
    DATA lv_name TYPE string.

    DATA(lv_tables) = z2ui5_cl_tm_context=>url_param_get( val = `tables`
                                                        url = client->get( )-s_config-search ).
    mv_title = z2ui5_cl_tm_context=>url_param_get( val = `title`
                                                 url = client->get( )-s_config-search ).

    DATA(lt_startup) = client->get( )-t_comp_params.
    IF lv_tables IS INITIAL.
      lv_tables = VALUE #( lt_startup[ n = `tables` ]-v OPTIONAL ).
    ENDIF.
    IF mv_title IS INITIAL.
      mv_title = VALUE #( lt_startup[ n = `title` ]-v OPTIONAL ).
    ENDIF.
    IF mv_title IS INITIAL.
      mv_title = `Table Maintenance`.
    ENDIF.

    SPLIT lv_tables AT `,` INTO TABLE lt_name.
    LOOP AT lt_name INTO lv_name.
      lv_name = to_upper( condense( lv_name ) ).
      IF lv_name IS INITIAL OR line_exists( mt_table[ tabname = lv_name ] ).
        CONTINUE.
      ENDIF.
      INSERT VALUE #( tabname = lv_name
                      text    = z2ui5_cl_tm_context=>rtti_get_table_desrc( lv_name )
                      count   = get_count( lv_name ) ) INTO TABLE mt_table.
    ENDLOOP.

    mv_selected_key = VALUE #( mt_table[ 1 ]-tabname OPTIONAL ).

  ENDMETHOD.


  METHOD on_after_confirm.

    DATA lo_confirm TYPE REF TO z2ui5_cl_popup_to_confirm.

    TRY.
        lo_confirm ?= client->get_app( client->get( )-s_draft-id_prev_app ).
      CATCH cx_root.
        " back from a popup of the embedded app, not from ours
        RETURN.
    ENDTRY.

    IF lo_confirm->result( ) = abap_true.
      " discard the unsaved changes and go to the tab that was asked for
      mo_app->mv_change_active = abap_false.
      mv_selected_key = mv_pending_key.
    ELSE.
      mv_selected_key = mv_shown_key.
    ENDIF.
    CLEAR mv_pending_key.

  ENDMETHOD.


  METHOD run_sub_app.

    DATA lv_new TYPE abap_bool.

    IF mv_selected_key <> mv_shown_key.

      IF mo_app IS BOUND.

        IF mo_app->mv_change_active = abap_true.
          mv_pending_key  = mv_selected_key.
          mv_selected_key = mv_shown_key.
          client->nav_app_call( z2ui5_cl_popup_to_confirm=>factory(
              i_question_text       = |{ mv_shown_key } has unsaved changes. They will be lost - switch anyway?|
              i_title               = `Unsaved Changes`
              i_button_text_confirm = `Switch`
              i_button_text_cancel  = `Stay` ) ).
          RETURN.
        ENDIF.

        " the transport request chosen in one table holds for the next
        IF mo_app->ms_transport IS NOT INITIAL.
          ms_transport = mo_app->ms_transport.
        ENDIF.

        " the entries may have changed - the count of the tab left behind
        ASSIGN mt_table[ tabname = mv_shown_key ] TO FIELD-SYMBOL(<left>).
        IF <left> IS ASSIGNED.
          <left>-count = get_count( mv_shown_key ).
        ENDIF.

      ENDIF.

      " the table left behind keeps no edit lock (and no stateful session)
      IF mo_app IS BOUND.
        mo_app->release_lock( client ).
      ENDIF.

      mo_app = NEW #( ).
      mo_app->ms_transport = ms_transport.
      mo_app->set_main_app_data( mv_selected_key ).
      mv_shown_key = mv_selected_key.
      lv_new = abap_true.

    ENDIF.

    DATA(page) = render_page( ).
    mo_app->mo_parent_view = page.

    TRY.
        mo_app->z2ui5_if_app~main( client ).
      CATCH cx_root INTO DATA(lx).
        client->message_box_display( text = lx->get_text( )
                                     type = `error` ).
    ENDTRY.

    IF lv_new = abap_true OR mo_app->mv_view_display = abap_true.
      mo_app->mv_view_display = abap_false.
      client->view_display( page->stringify( ) ).
    ENDIF.

  ENDMETHOD.


  METHOD render_page.

    DATA(view) = z2ui5_cl_ui5_view_builder=>factory(
        )->ele( n = `View` ns = `mvc`
            )->a( n = `xmlns`        v = `sap.m`
            )->a( n = `xmlns:mvc`    v = `sap.ui.core.mvc`
            )->a( n = `xmlns:core`   v = `sap.ui.core`
            )->a( n = `xmlns:table`  v = `sap.ui.table`
            )->a( n = `displayBlock` v = `true`
            )->a( n = `height`       v = `100%` ).

    IF client->get( )-check_launchpad_active = abap_true.
      result = view->ele( `Page`
          )->a( n = `showHeader` b = abap_false ).
      client->follow_up_action( val   = z2ui5_if_client=>cs_event-set_title_launchpad
                                t_arg = VALUE #( ( mv_title ) ) ).
    ELSE.
      result = view->ele( `Page`
          )->a( n = `title` t = mv_title ).
    ENDIF.

    DATA(items) = result->ele( `IconTabBar`
        )->a( n = `class`       v = `sapUiResponsiveContentPadding`
        )->a( n = `selectedKey` v = client->_bind( mv_selected_key )
        " abap2ui5lint-disable-next-line event-without-handler -- the roundtrip carries the bound selectedKey; run_sub_app reacts to that
        )->a( n = `select`      v = client->_event( `TAB_SELECT` )
        )->ele( `items` ).

    LOOP AT mt_table INTO DATA(ls_table).
      items->tag( `IconTabFilter`
          )->a( n = `key`   t = ls_table-tabname
          )->a( n = `text`  t = ls_table-text
          )->a( n = `count` t = ls_table-count ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_count.

    DATA lv_count TYPE i.

    TRY.
        SELECT COUNT(*) FROM (tabname) INTO @lv_count.
        result = |{ lv_count }|.
      CATCH cx_root.
        " no such table, or no access - the tab shows no count
        CLEAR result.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
