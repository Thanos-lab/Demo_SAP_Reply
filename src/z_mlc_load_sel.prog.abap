*&---------------------------------------------------------------------*
*& Include          Z_MLC_LOAD_SEL
*&---------------------------------------------------------------------*


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_upath   TYPE string DEFAULT TEXT-002.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_sales   AS CHECKBOX,  "Demo sales Data   p_seg
            p_proc     AS CHECKBOX,  "procurement data  p_fa
            p_fb50 AS CHECKBOX,  "Opex FB50 GL Posting   p_mat_gr
            p_serpro AS CHECKBOX.  "Opex Service Procurement   p_pur_gr
*            p_cust_g AS CHECKBOX,
*            p_acc_cl AS CHECKBOX,
*            p_pf_hr  AS CHECKBOX,
*            p_cc_hr  AS CHECKBOX,
*            p_subst  AS CHECKBOX,
*            p_mrp    AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b02.
