*&---------------------------------------------------------------------*
*& Include          Z_MLC_LOAD_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_GET_FILES_PATH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_P_FILE  text
*----------------------------------------------------------------------*

FORM f_get_files_path  USING p_path_file.
  REFRESH gt_files.

  gv_title = 'Select the directory with the desired import files!'.
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = gv_title
*     initial_folder       =
    CHANGING
      selected_folder      = p_path_file
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.
  IF sy-subrc <> 0.
    gs_message-id = z_cl_mlc_dataload=>gc_message_class.
    gs_message-number = '007'.
    gs_message-type = cl_esh_adm_constants=>gc_msgty_e.

    zcl_mlc_message_buffer=>add_from_bapiret2( is_return = gs_message ).
    gv_error = abap_true.
  ELSE.
    gv_path_file = p_path_file.
  ENDIF.

ENDFORM.                    "f_get_files_path
*&---------------------------------------------------------------------*
*&      Form  GET_META_DATA_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_meta_data_files.

  DATA lv_result(1) TYPE c.

  CHECK gv_error = abap_false.
  CREATE OBJECT go_dataload.

  gv_path_file = p_upath.

*  Check existence of directory
  CALL METHOD cl_gui_frontend_services=>directory_exist
    EXPORTING
      directory            = gv_path_file
    RECEIVING
      result               = lv_result
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      wrong_parameter      = 3
      not_supported_by_gui = 4
      OTHERS               = 5.

*  lv_result is X if valid directory
  IF lv_result IS INITIAL.
    gs_message-id = z_cl_mlc_dataload=>gc_message_class.
    gs_message-number = '016'.
    gs_message-type = cl_esh_adm_constants=>gc_msgty_e.

    zcl_mlc_message_buffer=>add_from_bapiret2( is_return = gs_message ).
    gv_error = abap_true.
  ELSE.

*  Get all the .txt files in this path.
    CALL METHOD cl_gui_frontend_services=>directory_list_files
      EXPORTING
        directory                   = gv_path_file
        filter                      = '*.TXT'
        files_only                  = 'X'
      CHANGING
        file_table                  = gt_files
        count                       = gv_file_count
      EXCEPTIONS
        cntl_error                  = 1
        directory_list_files_failed = 2
        wrong_parameter             = 3
        error_no_gui                = 4
        not_supported_by_gui        = 5
        OTHERS                      = 6.

    IF sy-subrc <> 0.
      gs_message-id = z_cl_mlc_dataload=>gc_message_class.
      gs_message-number = '008'.
      gs_message-type = cl_esh_adm_constants=>gc_msgty_e.

      zcl_mlc_message_buffer=>add_from_bapiret2( is_return = gs_message ).
      gv_error = abap_true.
    ELSEIF  gt_files[] IS INITIAL.
      gs_message-id = z_cl_mlc_dataload=>gc_message_class.
      gs_message-number = '009'.
      gs_message-type = cl_esh_adm_constants=>gc_msgty_e.

      zcl_mlc_message_buffer=>add_from_bapiret2( is_return = gs_message ).
      gv_error = abap_true.
    ELSE.
      LOOP AT gt_files ASSIGNING <gs_file_info>.

        CLEAR: gv_filename, gv_ext.
        SPLIT <gs_file_info>-filename AT '.' INTO gv_filename gv_ext.

        CONCATENATE gv_path_file '\' <gs_file_info>-filename INTO gv_file_fullpath.

        CALL METHOD go_dataload->fill_files_and_paths
          EXPORTING
            iv_filename      = gv_filename
            iv_file_fullpath = gv_file_fullpath.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_GET_META_DATA_FILES
*&---------------------------------------------------------------------*
*&      Form  PREPARE_DATALOAD_INSTANCE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM prepare_dataload_instance .

  DATA lt_obj_flag TYPE z_cl_mlc_dataload=>tt_sc_obj_flag.
  DATA ls_obj_flag TYPE z_cl_mlc_dataload=>ty_s_sc_obj_flag.

  CHECK gv_error = abap_false.

* Set which object types are required for import
  ls_obj_flag-object_name = z_cl_mlc_dataload=>gc_sales.
  ls_obj_flag-required = p_sales.
  APPEND ls_obj_flag TO lt_obj_flag.

  ls_obj_flag-object_name = z_cl_mlc_dataload=>gc_proc.
  ls_obj_flag-required = p_proc.
  APPEND ls_obj_flag TO lt_obj_flag.

  ls_obj_flag-object_name = z_cl_mlc_dataload=>gc_fb50.
  ls_obj_flag-required = p_fb50.
  APPEND ls_obj_flag TO lt_obj_flag.

  ls_obj_flag-object_name = z_cl_mlc_dataload=>gc_serpro.
  ls_obj_flag-required = p_serpro.
  APPEND ls_obj_flag TO lt_obj_flag.

*  ls_obj_flag-object_name = z_cl_mlc_dataload=>gc_routing.
*  ls_obj_flag-required = p_rout.
*  APPEND ls_obj_flag TO lt_obj_flag.

*  IF  p_rout  = abap_true.
*    ls_obj_flag-object_name = z_cl_mlc_dataload=>gc_prodvers.
*    ls_obj_flag-required = p_rout.
*    APPEND ls_obj_flag TO lt_obj_flag.
*  ENDIF.
*
*  ls_obj_flag-object_name = z_cl_mlc_dataload=>gc_vendor.
*  ls_obj_flag-required = p_vend.
*  APPEND ls_obj_flag TO lt_obj_flag.

  CALL METHOD go_dataload->fill_required_objects
    EXPORTING
      it_obj_flag = lt_obj_flag.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CHECK_INPUT_FILES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_input_files .

  CHECK gv_error = abap_false.
  go_dataload->check_input_files(
    IMPORTING
      ev_error = gv_error
  ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_messages .

  DATA: ls_bal_log        TYPE bal_s_log,
        lv_log_handle     TYPE balloghndl,
        ls_message_buffer TYPE zdl_s_message_buffer,
        ls_bal_msg        TYPE bal_s_msg,
        lt_log_handle     TYPE bal_t_logh,
        ls_disp_prof      TYPE bal_s_prof,
        lt_loghandle      TYPE bal_t_logh.

  IF                gv_error = 'X'.
    zcl_mlc_message_buffer=>clear(
        iv_msg_type = 'S'
    ).
  ENDIF.

  ls_bal_log-object = 'Z_DATALOAD'.
  ls_bal_log-subobject = 'Z_DATALOAD_SUBOBJ'.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log      = ls_bal_log
    IMPORTING
      e_log_handle = lv_log_handle.

  IF lv_log_handle IS NOT INITIAL.
    LOOP AT zcl_mlc_message_buffer=>gt_message_buffer INTO ls_message_buffer.
      ls_bal_msg-msgty = ls_message_buffer-type.
      ls_bal_msg-msgid = ls_message_buffer-id.
      ls_bal_msg-msgno = ls_message_buffer-number.
      ls_bal_msg-msgv1 = ls_message_buffer-message_v1.
      ls_bal_msg-msgv2 = ls_message_buffer-message_v2.
      ls_bal_msg-msgv3 = ls_message_buffer-message_v3.
      ls_bal_msg-msgv4 = ls_message_buffer-message_v4.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle = lv_log_handle
          i_s_msg      = ls_bal_msg.

      CLEAR ls_bal_msg.
    ENDLOOP.

    APPEND lv_log_handle TO lt_log_handle.
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_client       = sy-mandt
        i_t_log_handle = lt_log_handle.

    zcl_mlc_message_buffer=>clear( ).

    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = ls_disp_prof
      EXCEPTIONS
        OTHERS              = 1.
    INSERT lv_log_handle INTO TABLE lt_loghandle.

    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = ls_disp_prof
        i_t_log_handle      = lt_loghandle
      EXCEPTIONS
        OTHERS              = 1.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  process_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .

  CHECK gv_error = abap_false.
  go_dataload->process_data( IMPORTING ev_error = gv_error ).

ENDFORM.
