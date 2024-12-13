class ZCL_MLC_MESSAGE_BUFFER definition
  public
  final
  create public .

public section.

  class-data GV_DETLEVEL type BALLEVEL read-only .
  class-data GT_MESSAGE_BUFFER type ZDL_T_MESSAGE_BUFFER read-only .

  class-methods INIT .
  class-methods ADD_FROM_BAPIRET2
    importing
      !IS_RETURN type BAPIRET2 .
  class-methods ADD_FROM_BAPIRET2_T
    importing
      !IT_RETURN type BAPIRET2_T .
  class-methods INCREMENT_DETLEVEL .
  class-methods DECREMENT_DETLEVEL .
  class-methods CLEAR
    importing
      !IV_MSG_TYPE type CHAR1 optional .
protected section.
private section.
ENDCLASS.



CLASS ZCL_MLC_MESSAGE_BUFFER IMPLEMENTATION.


  METHOD ADD_FROM_BAPIRET2.

    DATA ls_message_buffer TYPE zdl_s_message_buffer.

    MOVE-CORRESPONDING is_return TO ls_message_buffer.
    ls_message_buffer-detlevel = gv_detlevel.
    APPEND ls_message_buffer TO gt_message_buffer.

  ENDMETHOD.


  METHOD ADD_FROM_BAPIRET2_T.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      ADD_FROM_BAPIRET2( <fs_return> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD CLEAR.

    IF iv_msg_type IS INITIAL.
      CLEAR gt_message_buffer[].
    ELSE.
      DELETE gt_message_buffer WHERE type = cl_esh_adm_constants=>gc_msgty_s AND id <> 'ZMLC_CL_MESSAGE'.
    ENDIF.

  ENDMETHOD.


  METHOD DECREMENT_DETLEVEL.
    IF gv_detlevel > 1.
      gv_detlevel = gv_detlevel - 1.
    ENDIF.
  ENDMETHOD.


  method INCREMENT_DETLEVEL.
    if gv_detlevel < 9.
      gv_detlevel = gv_detlevel + 1.
    endif.
  endmethod.


  method INIT.
    clear gt_message_buffer[].
    gv_detlevel = 1.
  endmethod.
ENDCLASS.
