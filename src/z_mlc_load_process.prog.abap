*&---------------------------------------------------------------------*
*& Include          Z_MLC_LOAD_PROCESS
*&---------------------------------------------------------------------*
INITIALIZATION.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
  AUTHORITY-CHECK OBJECT 'Z_DATALOAD'
                      ID 'ACTVT'    FIELD '16'.

  IF sy-subrc <> 0.
    gs_message-id = z_cl_mlc_dataload=>gc_message_class.
    gs_message-number = '015'.
    gs_message-type = cl_esh_adm_constants=>gc_msgty_e.

    zcl_mlc_message_buffer=>add_from_bapiret2( is_return = gs_message ).
    gv_error = abap_true.

  ENDIF.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_upath.
  PERFORM f_get_files_path USING p_upath.

*----------------------------------------------------------------------*
START-OF-SELECTION.
*----------------------------------------------------------------------*
*  PERFORM process_data.
*----------------------------------------------------------------------*

  PERFORM f_get_meta_data_files.

  PERFORM prepare_dataload_instance.

  PERFORM check_input_files.

  PERFORM process_data.

  PERFORM show_messages.

*----------------------------------------------------------------------*
END-OF-SELECTION.
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
TOP-OF-PAGE.
