*&---------------------------------------------------------------------*
*& Report ZMLC_UPLOAD_TEXTFILES
*&---------------------------------------------------------------------*
*& This report allows to upload Demo Sales data , Procurement data,
*& FB50GL posting data and Opex Data based on pre-defined
*& tab-delimited text documents.
*&---------------------------------------------------------------------*
REPORT ZMLC_UPLOAD_TEXTFILES.


INCLUDE z_mlc_load_top.     " Global Data

INCLUDE z_mlc_load_sel.     " Selection Screen

INCLUDE z_mlc_load_process. " Processing Part

INCLUDE z_mlc_load_f01.     " Subroutines
