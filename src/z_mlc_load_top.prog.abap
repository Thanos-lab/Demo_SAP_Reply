*&---------------------------------------------------------------------*
*& Include          Z_MLC_LOAD_TOP
*&---------------------------------------------------------------------*


* Internal tables
DATA gt_files TYPE filetable.

* Global variables
DATA: go_dataload      TYPE REF TO z_cl_mlc_dataload,
      gv_error         TYPE boolean,
      gv_file_count    TYPE i,
      gv_ext           TYPE string,
      gv_file_fullpath TYPE string,
      gv_filename      TYPE string,
      gv_title         TYPE string,
      gv_path_file     TYPE string,
      gs_message       TYPE bapiret2.

* Field symbols
FIELD-SYMBOLS <gs_file_info> LIKE LINE OF gt_files.
