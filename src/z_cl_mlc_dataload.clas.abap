CLASS z_cl_mlc_dataload DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_s_sales_data ,
        dateposted_str TYPE c LENGTH 8,
        doc_type       TYPE c LENGTH 4,
        collect_no     TYPE c LENGTH 35,
        sales_org      TYPE  c LENGTH 4,
        distr_chan     TYPE  c LENGTH 2,
        division       TYPE  c LENGTH 2,
        req_date_h     TYPE c LENGTH 20,
        material       TYPE  c LENGTH 40,
        quantity       TYPE  c LENGTH 13,
        sold_to        TYPE  c LENGTH 10,
        ship_to        TYPE  c LENGTH 10,
        dateposted     TYPE c LENGTH 14,
      END OF ty_s_sales_data .
    TYPES:
      tt_sales_data TYPE TABLE OF ty_s_sales_data .
    TYPES:
      BEGIN OF ty_s_procurement_data,
        material        TYPE c LENGTH 40,
        materialtype   TYPE c LENGTH 4,
        component       TYPE c LENGTH 40,
        comp_qty_factor TYPE i,
        netprice       TYPE c LENGTH 30,
        vendor          TYPE c LENGTH 10,
        plant           TYPE c LENGTH 4,
        storageloc      TYPE c LENGTH 4,
        purchorg        TYPE c LENGTH 4,
        compcode        TYPE c LENGTH 4,
        purchgrp        TYPE c LENGTH 3,
        doctype         TYPE c LENGTH 4,
        payterm         TYPE c LENGTH 4,
      END OF ty_s_procurement_data .
    TYPES:
      tt_procurement_data TYPE TABLE OF ty_s_procurement_data .
    TYPES:
      BEGIN OF ty_s_fb50,
        companycode      TYPE c LENGTH 4,
        documenttype       TYPE c LENGTH 2,
        currency       TYPE c LENGTH 5,
        documentdate  TYPE  c LENGTH 8,
        postingdate   TYPE  c LENGTH 8,
        drglaccount     TYPE  c LENGTH 10,
*         dr_indicator   TYPE  c LENGTH 1,
        dramount         TYPE  c LENGTH 13,
        drtaxcode     TYPE  c LENGTH 2,
        drtaxjurisdictioncode    TYPE  c LENGTH 15,
        drcostcenter TYPE  c LENGTH 10,
        crglaccount     TYPE  c LENGTH 10,
*         cr_indicator   TYPE  c LENGTH 1,
        cramount         TYPE  c LENGTH 13,
        crtaxcode     TYPE  c LENGTH 2,
        crtaxjurisdictioncode    TYPE  c LENGTH 15,
      END OF ty_s_fb50 .
    TYPES:
      tt_fb50 TYPE TABLE OF ty_s_fb50 .
    TYPES:
      BEGIN OF ty_s_serpro,
        ordertype    TYPE c LENGTH 4,
        vendor      TYPE  c LENGTH 10,
        documentdate    TYPE  c LENGTH 8,
        purchasingorganization   TYPE  c LENGTH 4,
        PurchasingGroup   TYPE c LENGTH 3,
        CompanyCode   TYPE c LENGTH 4,
        Item     TYPE c LENGTH 5,
        AccountAssignmentCategory  TYPE c LENGTH 1,
        material    TYPE c LENGTH 40,
        POQuantity         TYPE c LENGTH 13,
        netprice   TYPE c LENGTH 11,
        currency    TYPE c LENGTH 5,
        plant       TYPE c LENGTH 4,
        glaccount  TYPE c LENGTH 10,
        costcenter TYPE c LENGTH 10,
        taxcode    TYPE c LENGTH 2,
*        taxjurisdictioncode  TYPE c LENGTH 15,
      END OF ty_s_serpro .
    TYPES:
      tt_serpro TYPE TABLE OF ty_s_serpro .


    TYPES:
      BEGIN OF ty_s_sc_mapping,
        sequence        TYPE i,
        object_name(30) TYPE c,
        file_stamp(40)  TYPE c,
*        object_name TYPE string,
*        file_stamp  TYPE string,
      END OF ty_s_sc_mapping .

    TYPES:
      BEGIN OF ty_s_sc_file,
        file_name(40) TYPE c,
*        file_name TYPE string,
        file_path     TYPE string,
      END OF ty_s_sc_file .
    TYPES:
      tt_sc_mapping TYPE SORTED TABLE OF ty_s_sc_mapping WITH UNIQUE KEY sequence .
    TYPES:
      tt_sc_file TYPE TABLE OF ty_s_sc_file .
    TYPES:
      BEGIN OF ty_s_sc_obj_flag,
        object_name(30) TYPE c,
        required        TYPE boolean,
        sequence        TYPE i,
      END OF ty_s_sc_obj_flag .
    TYPES:
      tt_sc_obj_flag TYPE TABLE OF ty_s_sc_obj_flag .

    CONSTANTS gc_sales TYPE char40 VALUE 'SALESDATA' ##NO_TEXT.
    CONSTANTS gc_proc TYPE char40 VALUE 'PROCUREMENTDATA' ##NO_TEXT.
    CONSTANTS gc_fb50 TYPE char40 VALUE 'FB50' ##NO_TEXT.
    CONSTANTS gc_serpro TYPE char40 VALUE 'SERVICEPROCUREMENT' ##NO_TEXT.
    CONSTANTS gc_message_class TYPE string VALUE 'ZMLC_CL_MESSAGE' ##NO_TEXT.
    CONSTANTS gc_max_end_date TYPE char8 VALUE '99991231' ##NO_TEXT.
    CONSTANTS gc_prodvers TYPE char40 VALUE 'PRODVERS' ##NO_TEXT.
    DATA mv_valid_from_date TYPE char10 .
    DATA mv_valid_to_date TYPE char10 .

    METHODS constructor .
    METHODS process_data
      EXPORTING
        !ev_error TYPE boolean .
    METHODS fill_required_objects
      IMPORTING
        !it_obj_flag TYPE tt_sc_obj_flag
        !iv_test     TYPE boolean OPTIONAL .
    METHODS fill_files_and_paths
      IMPORTING
        !iv_filename      TYPE string
        !iv_file_fullpath TYPE string .
    METHODS check_input_files
      EXPORTING
        !ev_error TYPE boolean .
protected section.
PRIVATE SECTION.

  CONSTANTS gc_prodvers_f TYPE char40 VALUE 'Production Version' ##NO_TEXT.
  DATA mt_mapping TYPE tt_sc_mapping .
  DATA mt_files TYPE tt_sc_file .
  DATA mt_object_flag TYPE tt_sc_obj_flag .
  CONSTANTS gc_sales_f TYPE char40 VALUE 'Salesdata' ##NO_TEXT.
  CONSTANTS gc_proc_f TYPE char40 VALUE 'Procurementdata' ##NO_TEXT.
  CONSTANTS gc_fb50_f TYPE char40 VALUE 'Fb50' ##NO_TEXT.
  CONSTANTS gc_serpro_f TYPE char40 VALUE 'Serviceprocurement' ##NO_TEXT.
  DATA mv_test TYPE boolean .

  METHODS retrieve_file_content
    IMPORTING
      !iv_object_name TYPE char30
      !iv_sequence    TYPE int4
    EXPORTING
      !ev_error       TYPE boolean
      !et_data        TYPE ANY TABLE .
  METHODS call_save_to_db
    IMPORTING
      !it_table       TYPE data
      !iv_object_name TYPE char30
      !iv_sequence    TYPE i
    EXPORTING
      !ev_error       TYPE boolean .
  METHODS save_procurement_data
    IMPORTING
      !it_procurement TYPE tt_procurement_data
    EXPORTING
      !ev_error       TYPE boolean .
  METHODS save_sales_data
    IMPORTING
      !it_sales TYPE tt_sales_data
    EXPORTING
      !ev_error TYPE boolean .
  METHODS save_fb50_data
    IMPORTING
      !it_fb50  TYPE tt_fb50
    EXPORTING
      !ev_error TYPE boolean .
  METHODS save_serproc_data
    IMPORTING
      !it_serproc TYPE tt_serpro
    EXPORTING
      !ev_error   TYPE boolean .
*  methods CALL_BAPI_PRODUCTION_VERSION
*    importing
*      !IT_PRODUCTION_VERSION type TT_PROD_VERS
*    exporting
*      !EV_ERROR type BOOLEAN .
ENDCLASS.



CLASS Z_CL_MLC_DATALOAD IMPLEMENTATION.


  METHOD CALL_SAVE_TO_DB.

    DATA: lt_sales_data         TYPE tt_sales_data,
          ls_sales_data         TYPE ty_s_sales_data,
          lt_procurement_data           TYPE tt_procurement_data,
          ls_procurement_data           TYPE ty_s_procurement_data,
          lt_fb50                TYPE tt_fb50,
          ls_fb50                TYPE ty_s_fb50,
          lt_serpro            TYPE tt_serpro,
          ls_serpro            TYPE ty_s_serpro.
*          lt_customer           TYPE tt_customer,
*          ls_customer           TYPE ty_s_customer,
*          lt_vendor             TYPE tt_vendor,
*          ls_vendor             TYPE ty_s_vendor,
*          lt_activity_rate      TYPE tt_activity_rate,
*          ls_activity_rate      TYPE ty_s_activity_rate,
*          lt_prod_vers          TYPE tt_prod_vers,
*          lt_sale_price         TYPE tt_sale_price,
*          lt_segment            TYPE tt_segments,
*          lt_fct_area           TYPE tt_fct_area,
*          lt_acc_clerk          TYPE tt_acc_clerk,
*          lt_mat_grp            TYPE tt_mat_group,
*          lt_pur_grp            TYPE tt_pur_group,
*          lt_cust_grp           TYPE tt_cust_group,
*          lt_profit_center      TYPE tt_profit_center,
*          lt_profit_center_hier TYPE tt_profit_center_hier,
*          lt_cost_center        TYPE tt_cost_center,
*          lt_cost_center_hier   TYPE tt_cost_center_hier,
*          lt_mrp_controller     TYPE tt_mrp_controller.


    CASE iv_object_name.
      WHEN gc_sales.

        lt_sales_data = it_table.
        SAVE_SALES_DATA(
          EXPORTING
            it_sales = lt_sales_data
          IMPORTING
            ev_error      = ev_error
        ).
*
      WHEN gc_proc.

        lt_procurement_data = it_table.
        SAVE_PROCUREMENT_DATA(
          EXPORTING
            it_procurement = lt_procurement_data
          IMPORTING
            ev_error    =  ev_error
        ).

      WHEN gc_fb50.

        lt_fb50 = it_table.
        SAVE_FB50_DATA(
          EXPORTING
            it_fb50   = lt_fb50
          IMPORTING
            ev_error = ev_error
        ).

      WHEN gc_serpro.

        lt_serpro = it_table.
        SAVE_SERPROC_DATA(
          EXPORTING
            it_serproc = lt_serpro
          IMPORTING
            ev_error   =  ev_error
        ).
**
**        IF ev_error = abap_false.
***          set_procurement_data_version_flag(
***            EXPORTING
***              it_serpro = lt_serpro
***            IMPORTING
***              ev_error    = ev_error    " Boolean Variable (X=True, -=False, Space=Unknown)
***          ).
**
**        ENDIF.
*      WHEN gc_prodvers.
*        lt_prod_vers = it_table.
*
*        call_bapi_production_version(
*        EXPORTING
*          it_production_version = lt_prod_vers
*        IMPORTING
*          ev_error              = ev_error    " Boolean Variable (X=True, -=False, Space=Unknown)
*      ).
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.


  METHOD CHECK_INPUT_FILES.


    DATA: ls_obj_flag     TYPE ty_s_sc_obj_flag,
          ls_mapping      TYPE ty_s_sc_mapping,
          ls_files        TYPE ty_s_sc_file,
          lv_check_all    TYPE boolean,
          lv_error_flag   TYPE boolean,
          lv_length_stamp TYPE i,
          lv_check        TYPE boolean,
          ls_message      TYPE bapiret2.

    LOOP AT mt_object_flag INTO ls_obj_flag WHERE required  = abap_true.
      LOOP AT mt_mapping INTO ls_mapping WHERE object_name = ls_obj_flag-object_name. "#EC CI_SORTSEQ

        lv_length_stamp = strlen( ls_mapping-file_stamp ).
        LOOP AT mt_files INTO ls_files.
          IF ls_files-file_name+0(lv_length_stamp) = ls_mapping-file_stamp.
            lv_check = abap_true.
          ENDIF.
        ENDLOOP.

        IF lv_check = abap_false.
          ls_message-id = gc_message_class.
          ls_message-number = '001'.
          ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
          ls_message-message_v1 = ls_mapping-file_stamp.
          ls_message-message_v2 = ls_mapping-object_name.

          zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
          ev_error = abap_true.
        ENDIF.

        CLEAR lv_check.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD CONSTRUCTOR.

    DATA: ls_defaults TYPE bapidefaul,
          lt_return   TYPE TABLE OF bapiret2,
          lv_datfm TYPE XUDATFM.

* As some of the objects are required to exist in order for the others to be created, a certain sequence has to be implemented
    mt_mapping = VALUE #(
      ( sequence = '1001'       object_name = 'SALESDATA'           file_stamp = gc_sales_f  )
      ( sequence = '1002'       object_name = 'PROCUREMENTDATA'   file_stamp = gc_proc_f  )
      ( sequence = '1003'       object_name = 'FB50'         file_stamp = gc_fb50_f  )
      ( sequence = '1004'       object_name = 'SERVICEPROCUREMENT'  file_stamp = gc_serpro_f  )
*      ( sequence = '1005'       object_name = 'CUST GROUP'        file_stamp = gc_cust_grp_f  )
*      ( sequence = '1006'       object_name = 'ACCOUNTING CLERK'  file_stamp = gc_acc_clerks_f  )
*      ( sequence = '1007'       object_name = 'PROFIT HIERARCHY'  file_stamp = gc_profit_hier_f  )
*      ( sequence = '1008'       object_name = 'COST HIERARCHY'    file_stamp = gc_cost_hier_f  )
**      ( sequence = '1009'       object_name = 'SUBSTITUTION'      file_stamp = gc_substitution_f  )
*      ( sequence = '1010'       object_name = 'MRP CONTROLLER'    file_stamp = gc_mrp_controller_f )
*      ( sequence = '2001'       object_name = 'PROFIT CENTER'     file_stamp = gc_profit_center_f  )
*      ( sequence = '2002'       object_name = 'COST CENTER'       file_stamp = gc_cost_center_f  )
*      ( sequence = '2003'       object_name = 'CUSTOMER'          file_stamp = gc_customer_f  )
*      ( sequence = '2004'       object_name = 'VENDOR'            file_stamp = gc_vendor_f  )
*      ( sequence = '2005'       object_name = 'MATERIAL'          file_stamp = gc_material_f  )
*      ( sequence = '2006'       object_name = 'BOM'               file_stamp = gc_bom_f  )
*      ( sequence = '2007'       object_name = 'ACTIVITY'          file_stamp = gc_activity_f  )
*      ( sequence = '2008'       object_name = 'WORKCENTER'        file_stamp = gc_workcenter_f  )
*      ( sequence = '2009'       object_name = 'ROUTING'           file_stamp = gc_routing_f  )
*      ( sequence = '2010'       object_name = 'PRODVERS'          file_stamp = gc_prodvers_f  )
*      ( sequence = '2011'       object_name = 'SALE_PRICE'        file_stamp = gc_sale_price_f  )
      ).

* Initialize the message buffer class
    zcl_mlc_message_buffer=>init( ).

     call function 'SUSR_USER_DEFAULT_DATE_FORMAT'
                      importing datfm = lv_datfm.
    UPDATE usr01
       SET dcpfm = ''
       datfm = lv_datfm " '1'
       WHERE bname = sy-uname .

    COMMIT WORK.

** Retrieve Date Format from User Settings
*    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*      EXPORTING
*        username = sy-uname
*      IMPORTING
*        defaults = ls_defaults
*      TABLES
*        return   = lt_return.
*
*    CASE ls_defaults-datfm.                                 "#EC DATFM
    CASE lv_datfm .
      WHEN '1'.
*        mv_valid_from_date = '01.01.2016'.
        CONCATENATE '01.01.' sy-datum+0(4) INTO mv_valid_from_date .
        mv_valid_to_date =  '31.12.9999' .

      WHEN '2'.
*        mv_valid_from_date = '01/01/2016'.
        CONCATENATE '01/01/' sy-datum+0(4) INTO mv_valid_from_date .
        mv_valid_to_date =  '12/31/9999' .

      WHEN '3'.
*        mv_valid_from_date = '01-01-2016'.
        CONCATENATE '01-01-' sy-datum+0(4) INTO mv_valid_from_date .
        mv_valid_to_date =  '12-31-9999' .

      WHEN '4'.
*        mv_valid_from_date = '2016.01.01'.
        CONCATENATE sy-datum+0(4) '.01.01'  INTO mv_valid_from_date .
        mv_valid_to_date =  '9999.12.31' .

      WHEN '5'.
*        mv_valid_from_date = '2016/01/01'.
        CONCATENATE sy-datum+0(4) '/01/01'  INTO mv_valid_from_date .
        mv_valid_to_date =  '9999/12/31' .

      WHEN '6'.
*        mv_valid_from_date = '2016-01-01'.
        CONCATENATE sy-datum+0(4) '-01-01'  INTO mv_valid_from_date .
        mv_valid_to_date =  '9999-12-31' .

      WHEN OTHERS.
    ENDCASE.




  ENDMETHOD.


  METHOD FILL_FILES_AND_PATHS.

    DATA ls_file TYPE ty_s_sc_file.


    IF iv_filename IS NOT INITIAL AND iv_file_fullpath IS NOT INITIAL.
      ls_file-file_name = iv_filename.
      ls_file-file_path = iv_file_fullpath.
      APPEND ls_file TO mt_files.
    ENDIF.

  ENDMETHOD.


  METHOD FILL_REQUIRED_OBJECTS.


    DATA: ls_object_flag LIKE LINE OF mt_object_flag,
          ls_mapping     LIKE LINE OF mt_mapping.

    LOOP AT it_obj_flag INTO ls_object_flag.            "#EC CI_SORTSEQ
      LOOP AT mt_mapping INTO ls_mapping WHERE object_name = ls_object_flag-object_name.
        ls_object_flag-sequence = ls_mapping-sequence.
        APPEND ls_object_flag TO mt_object_flag.
      ENDLOOP.
    ENDLOOP.

    SORT mt_object_flag ASCENDING BY sequence.

  ENDMETHOD.


  METHOD PROCESS_DATA.


    DATA: ls_object_flag        LIKE LINE OF mt_object_flag,
          ls_file               LIKE LINE OF mt_files,
          ls_mapping            LIKE LINE OF mt_mapping,
          lt_sales_data            TYPE REF TO data,
          lt_procurement_data      TYPE REF TO data,
          lt_fb50_data           TYPE REF TO data,
          lt_serpro_data      TYPE REF TO data,
          lt_customer_data      TYPE REF TO data,
          ls_message            TYPE        bapiret2.
*          lt_vendor_data        TYPE REF TO data,
*          lt_cost_center_data   TYPE REF TO data,
*          lt_routing_data       TYPE REF TO data,
*          lt_prodvers_data      TYPE REF TO data,
*          lt_sale_price         TYPE REF TO data,
*          lt_segment            TYPE REF TO data,
*          lt_mat_group          TYPE REF TO data,
*          lt_pur_group          TYPE REF TO data,
*          lt_cust_group         TYPE REF TO data,
*          lt_acc_clerk          TYPE REF TO data,
*          lt_fct_area           TYPE REF TO data,
*          lt_profit_center      TYPE REF TO data,
*          lt_profit_center_hier TYPE REF TO data,
*          lt_cost_center        TYPE REF TO data,
*          lt_cost_center_hier   TYPE REF TO data,
*          lt_mrp_controller     TYPE REF TO data.

    FIELD-SYMBOLS: <lft_data> TYPE ANY TABLE.

    SORT mt_object_flag ASCENDING BY sequence.

    READ TABLE mt_object_flag INTO ls_object_flag WITH KEY required = abap_true.
    IF ls_object_flag IS INITIAL.
      ls_message-id = gc_message_class.
      ls_message-number = '029'.
      ls_message-type = cl_esh_adm_constants=>gc_msgty_e.

      zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
      ev_error = abap_true.
      RETURN.
    ENDIF.

    LOOP AT mt_object_flag INTO ls_object_flag WHERE required = abap_true.

      CASE ls_object_flag-object_name.
        WHEN gc_sales.
          CREATE DATA lt_sales_data TYPE tt_sales_data.
          ASSIGN lt_sales_data->* TO <lft_data>.

        WHEN gc_proc.
          CREATE DATA lt_procurement_data TYPE tt_procurement_data.
          ASSIGN lt_procurement_data->* TO <lft_data>.

        WHEN gc_fb50.
          CREATE DATA lt_fb50_data TYPE tt_fb50.
          ASSIGN lt_fb50_data->* TO <lft_data>.

         WHEN gc_serpro.
          CREATE DATA lt_serpro_data TYPE tt_serpro.
          ASSIGN lt_serpro_data->* TO <lft_data>.

        WHEN OTHERS.

      ENDCASE.

      retrieve_file_content(
        EXPORTING
          iv_object_name = ls_object_flag-object_name    " 30 Characters
          iv_sequence    = ls_object_flag-sequence    " Natural number
        IMPORTING
          et_data        = <lft_data>
          ev_error       = ev_error
      ).

      IF <lft_data> IS NOT INITIAL AND ev_error = abap_false.
        call_save_to_db(
          EXPORTING
            it_table       = <lft_data>
            iv_object_name = ls_object_flag-object_name
            iv_sequence    = ls_object_flag-sequence
          IMPORTING
            ev_error       = ev_error
        ).

        IF ev_error = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF ev_error = abap_true.
      ls_message-id = gc_message_class.
      ls_message-number = '060'.
      ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
      zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
    ENDIF.

  ENDMETHOD.


  METHOD retrieve_file_content.


    TYPES: BEGIN OF l_typ_confrontation,
             intfieldname TYPE string,
             intfieldpos  TYPE i,
             intfieldtyp  TYPE string,
             csvfieldpos  TYPE i,
             csvfieldname TYPE string,
           END OF l_typ_confrontation.

    DATA: lt_csv_data         TYPE stringtab,
          lt_fields           TYPE STANDARD TABLE OF string,
          ls_mapping          LIKE LINE OF mt_mapping,
          lv_full_path        TYPE string,
          ls_file             LIKE LINE OF mt_files,
          lv_length           TYPE i,
          lv_string           TYPE string,
          l_rda_data          TYPE REF TO data,
          l_rda_wa            TYPE REF TO data,
          l_rcl_descr_tab     TYPE REF TO cl_abap_tabledescr,
          l_rcl_descr_struc   TYPE REF TO cl_abap_structdescr,
          l_comp_descr        TYPE abap_compdescr,
          l_tab_content       TYPE STANDARD TABLE OF string,
          l_line              TYPE string VALUE '',
          l_tab_confrontation TYPE STANDARD TABLE OF l_typ_confrontation WITH KEY csvfieldpos,
          ls_confrontation    TYPE l_typ_confrontation,
          l_fieldname         TYPE string VALUE '',
          l_content           TYPE string VALUE '',
          l_conf              TYPE l_typ_confrontation,
          ls_message          TYPE bapiret2.

    FIELD-SYMBOLS: <l_table> TYPE STANDARD TABLE,
                   <l_comp>  TYPE any,
                   <l_wa>    TYPE any,
                   <line>    TYPE string.

    READ TABLE mt_mapping INTO ls_mapping WITH KEY object_name = iv_object_name
                                                   sequence    = iv_sequence.

    IF ls_mapping-file_stamp IS NOT INITIAL.
      lv_length = strlen( ls_mapping-file_stamp ).

      LOOP AT mt_files INTO ls_file.
        lv_string = ls_file-file_name+0(lv_length).

        IF lv_string = ls_mapping-file_stamp.
          lv_full_path = ls_file-file_path.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_full_path IS NOT INITIAL.

        CALL FUNCTION 'GUI_UPLOAD'
          EXPORTING
            filename = lv_full_path
            filetype = 'ASC'
          TABLES
            data_tab = lt_csv_data
          EXCEPTIONS
            OTHERS   = 1.
        IF sy-subrc  <> 0.
          ls_message-id = gc_message_class.
          ls_message-number = '002'.
          ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
          ls_message-message_v1 = ls_mapping-object_name.

          zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).

          ev_error = abap_true.
        ENDIF.

        IF lt_csv_data IS NOT INITIAL.

*** Manipulate Headline
          READ TABLE lt_csv_data INDEX 1 ASSIGNING  <line>.
          IF <line> IS ASSIGNED.
            REPLACE ALL OCCURRENCES OF '-' IN <line> WITH space.
            REPLACE ALL OCCURRENCES OF '.' IN <line> WITH space.
            REPLACE ALL OCCURRENCES OF ':' IN <line> WITH space.
            REPLACE ALL OCCURRENCES OF '(' IN <line> WITH space.
            REPLACE ALL OCCURRENCES OF ')' IN <line> WITH space.
            REPLACE ALL OCCURRENCES OF '%' IN <line> WITH space.
            REPLACE ALL OCCURRENCES OF '/' IN <line> WITH space.
            CONDENSE  <line> NO-GAPS.
            TRANSLATE <line> TO LOWER CASE.
            SPLIT  <line> AT cl_abap_char_utilities=>horizontal_tab INTO TABLE lt_fields.

            CASE iv_object_name.
              WHEN gc_sales.
                CREATE DATA l_rda_data TYPE tt_sales_data.
              WHEN gc_proc.
                CREATE DATA l_rda_data TYPE tt_procurement_data.
              WHEN gc_fb50.
                CREATE DATA l_rda_data TYPE tt_fb50.
              WHEN gc_serpro.
                CREATE DATA l_rda_data TYPE tt_serpro.
            ENDCASE.
            ASSIGN l_rda_data->* TO <l_table>.

*** Get Structure of Table
            l_rcl_descr_tab ?= cl_abap_typedescr=>describe_by_data( <l_table> ).
            l_rcl_descr_struc ?= l_rcl_descr_tab->get_table_line_type( ).

*** Define Line of Table
            CREATE DATA l_rda_wa LIKE LINE OF  <l_table>.
            ASSIGN l_rda_wa->* TO  <l_wa>.

*-Compare field names of the table with headline of the import file
*
*- Within this step the position of the column is indiferent. It
*- is only necessary that the field of the table and the column
*- of the import file must have the same name.

            LOOP AT l_rcl_descr_struc->components INTO l_comp_descr.
              l_conf-intfieldname = l_comp_descr-name.
              l_conf-intfieldpos = sy-tabix.
              l_conf-intfieldtyp = l_comp_descr-type_kind.
              LOOP AT lt_fields INTO l_fieldname.
                l_conf-csvfieldpos = -1.
                TRANSLATE l_fieldname TO UPPER CASE.
                l_conf-csvfieldname = 'UNKNOWN'.
                IF l_comp_descr-name = l_fieldname.
                  l_conf-csvfieldname = l_fieldname.
                  l_conf-csvfieldpos = sy-tabix.
                  EXIT.
                ENDIF.
              ENDLOOP.
              APPEND l_conf TO l_tab_confrontation.
            ENDLOOP.

*            DELETE l_tab_confrontation WHERE csvfieldpos = -1.
            READ TABLE l_tab_confrontation INTO ls_confrontation WITH KEY csvfieldpos = -1.
            IF sy-subrc IS INITIAL.
              " all column names do not match.
              CLEAR ls_confrontation.
              LOOP AT l_tab_confrontation INTO ls_confrontation WHERE csvfieldpos = -1 .
                ls_message-id = gc_message_class.
                ls_message-number = '059'.
                ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
                ls_message-message_v1 = iv_object_name.
                ls_message-message_v2 = ls_confrontation-csvfieldname.
                zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
                ev_error = abap_true.
              ENDLOOP.
              return.
            ENDIF.

            SORT l_tab_confrontation BY csvfieldpos.
            LOOP AT lt_csv_data INTO l_line FROM 2.
              SPLIT l_line AT cl_abap_char_utilities=>horizontal_tab INTO TABLE l_tab_content.
              LOOP AT l_tab_content INTO l_content.
                CONDENSE l_content.
                READ TABLE l_tab_confrontation WITH KEY csvfieldpos = sy-tabix
                  INTO l_conf.
                IF sy-subrc = 0.
                  ASSIGN COMPONENT l_conf-intfieldname OF STRUCTURE  <l_wa>
                    TO  <l_comp>.
                  IF l_conf-intfieldtyp = 'P'.
                    REPLACE ALL OCCURRENCES OF '.' IN l_content WITH ''.
                    REPLACE ',' IN l_content WITH '.'.
                    REPLACE ALL OCCURRENCES OF '"' IN l_content WITH ''.
                    CONDENSE l_content.
                    <l_comp> = l_content.
                  ELSE.
                    <l_comp> = l_content.
                  ENDIF.
                ENDIF.
              ENDLOOP.
              APPEND  <l_wa> TO  <l_table>.
              CLEAR  <l_wa>.
            ENDLOOP.

            IF sy-subrc IS NOT INITIAL.
              ls_message-id = gc_message_class.
              ls_message-number = '022'.
              ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
              ls_message-message_v1 = ls_mapping-file_stamp.
              ls_message-message_v2 = ls_mapping-object_name.

              zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
            ELSE.
*** Write Data into Table
              et_data = <l_table>.
            ENDIF.
          ENDIF.
        ELSE.
* Import file data is initial
          ls_message-id = gc_message_class.
          ls_message-number = '022'.
          ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
          ls_message-message_v1 = ls_mapping-file_stamp.
          ls_message-message_v2 = ls_mapping-object_name.

          zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).

          ev_error = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD save_fb50_data.
    data : ls_message        TYPE bapiret2.

    IF it_fb50[] IS INITIAL.
*write relevant msg
          ls_message-id = gc_message_class.
          ls_message-number = '050'.
          ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
          ls_message-message_v1 = gc_fb50_f.
          zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
    ELSE.
      DATA : ls_fb50 TYPE ty_s_fb50.
      DATA: ls_glrec              TYPE zglpostings_data,
            wa_glrec              TYPE zglpostings_data,
            lt_glrec              TYPE TABLE OF zglpostings_data,
            rec_count             TYPE i VALUE 0,
            rec_count_char        TYPE c LENGTH 5,
            vendor_rec_count      TYPE i VALUE 0,
            vendor_rec_count_char TYPE c LENGTH 5,
            upload_rec_count      TYPE i VALUE 0,
            upload_rec_count_char TYPE c LENGTH 5,
            lv_sl_no              TYPE int4.

      DESCRIBE TABLE it_fb50 LINES upload_rec_count.

      SELECT MAX( sl_no ) FROM zglpostings_data INTO lv_sl_no.

      LOOP AT it_fb50 INTO ls_fb50.
        ls_glrec-mandt = sy-mandt.
*    ls_glrec_sl_no = lv_sl_no + 1.
        ls_glrec-comp_code   = ls_fb50-companycode.
        ls_glrec-doc_type = ls_fb50-documenttype.
        ls_glrec-currency = ls_fb50-currency.
        ls_glrec-document_date   = ls_fb50-documentdate.
        ls_glrec-posting_date   = ls_fb50-postingdate.
        ls_glrec-dr_cost_center = ls_fb50-drcostcenter.

        ls_glrec-dr_glaccnt     = ls_fb50-drglaccount .
        ls_glrec-dr_indicator   = 'S' ."ls_fb50-DR_INDICATOR
        ls_glrec-dr_amt          = ls_fb50-dramount.
        ls_glrec-dr_taxcode      = ls_fb50-drtaxcode.
        ls_glrec-dr_taxjuris     = ls_fb50-drtaxjurisdictioncode.

        ls_glrec-cr_glaccnt      = ls_fb50-crglaccount.
        ls_glrec-cr_indicator     = 'H'."ls_fb50-CR_INDICATOR
        ls_glrec-cr_amt           = ls_fb50-cramount.

        IF ls_fb50-crtaxcode IS NOT INITIAL.
          ls_glrec-cr_taxcode = ls_fb50-crtaxcode.
        ENDIF.

        IF ls_fb50-crtaxjurisdictioncode IS NOT INITIAL.
          ls_glrec-cr_taxjuris = ls_fb50-crtaxjurisdictioncode.
        ENDIF.

        IF ls_glrec-cr_glaccnt EQ '21790000'.
          ls_glrec-cr_taxcode       = 'O0'. "ls_fb50-CR_TAXCODE.
          ls_glrec-cr_taxjuris      = '7700000000'. ""'3300100201 '. "ls_fb50-CR_TAXJURIS.
        ENDIF.

**    ls_glrec-vendor            = ls_fb50-vendor.
**    ls_glrec-vendor_service    = ls_fb50-vendor_service.
**    ls_glrec-CHANGED_ON        = ls_fb50-CHANGED_ON.
**    ls_glrec-DOCUMENT_NO       = ls_fb50-DOCUMENT_NO.
**    ls_glrec-STATUS             = ls_fb50-STATUS.
**    ls_glrec-STATUS_DESC        = ls_fb50-STATUS_DESC.
*
**  append ls_glrec to lt_glrec.
**  clear ls_glrec.
**  IF lt_glrec[] is not INITIAL.
**    MODIFY ZGLPOSTINGS_DATA FROM TABLE lt_glrec.
**  ENDIF.
*
*    IF ls_glrec-vendor IS NOT INITIAL. " do not process records with Vendor and vendor service type
*      vendor_rec_count =  vendor_rec_count + 1.
*      CONTINUE.
*    ELSE.
*      lv_sl_no = lv_sl_no + 1.
*      ls_glrec-sl_no = lv_sl_no.
*      APPEND ls_glrec TO lt_glrec.
*      rec_count = rec_count + 1.
***  ****  process each records by checking if already in the dbtable
**select single * from zglpostings_data into wa_glrec where COMP_CODE = ls_glrec-COMP_CODE
**                                      AND POSTING_DATE = ls_glrec-POSTING_DATE
**                                      AND DR_COST_CENTER = ls_glrec-DR_COST_CENTER.
**  IF sy-subrc is not INITIAL.  "insert/modify if not in db
**    INSERT ZGLPOSTINGS_DATA from ls_glrec.
**    rec_count = rec_count + 1.
**  ENDIF.
**  clear : ls_glrec, wa_glrec.
*    ENDIF.

        lv_sl_no = lv_sl_no + 1.
        ls_glrec-sl_no = lv_sl_no.
        APPEND ls_glrec TO lt_glrec.
        rec_count = rec_count + 1.

        CLEAR ls_glrec.
      ENDLOOP.

      IF lt_glrec[] IS NOT INITIAL.
        write : upload_rec_count  to upload_rec_count_char.
        INSERT zglpostings_data FROM TABLE lt_glrec.
        IF sy-subrc is INITIAL.
        ls_message-id = gc_message_class.
        ls_message-number = '055'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_s.
        ls_message-message_v1 = gc_fb50_f.
        ls_message-message_v2 = upload_rec_count_char.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
        ELSE.
        ls_message-id = gc_message_class.
        ls_message-number = '056'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
        ls_message-message_v1 = gc_fb50_f.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
        ENDIF.
      ENDIF.


    ENDIF.
  ENDMETHOD.


  METHOD save_procurement_data.
    data : ls_message        TYPE bapiret2.
    IF it_procurement[] IS INITIAL.
*      Write  the message
          ls_message-id = gc_message_class.
          ls_message-number = '050'.
          ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
          ls_message-message_v1 = gc_proc_f.
          zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).

    ELSE.
*      process the records and save to DB
      DATA : ls_procurement        TYPE ty_s_procurement_data, "LIKE LINE OF tt_procurement_data,
             ls_procure            TYPE zdatagen_procure,
             lt_procure            TYPE TABLE OF zdatagen_procure,
             rec_count             TYPE i VALUE 0,
             rec_count_char        TYPE c LENGTH 5,
             upload_rec_count      TYPE i VALUE 0,
             upload_rec_count_char TYPE c LENGTH 5.
      DATA : lv_p TYPE p DECIMALS 9.

      DESCRIBE       TABLE it_procurement LINES upload_rec_count.

      LOOP AT it_procurement INTO ls_procurement.
        ls_procure-mandt            = sy-mandt.
        ls_procure-matnr            = ls_procurement-material.
        ls_procure-mtart            = ls_procurement-materialtype .
        ls_procure-component        = ls_procurement-component.
        ls_procure-comp_qty_factor  = ls_procurement-comp_qty_factor.

        REPLACE ALL OCCURRENCES OF ',' IN ls_procurement-netprice WITH '.'.
        lv_p = ls_procurement-netprice.

        ls_procure-net_price        = lv_p. "ls_procurement-netprice
        ls_procure-vendor           = ls_procurement-vendor         .
        ls_procure-plant            = ls_procurement-plant          .
        ls_procure-stge_loc         = ls_procurement-storageloc       .
        ls_procure-purch_org        = ls_procurement-purchorg      .
        ls_procure-comp_code        = ls_procurement-compcode      .
        ls_procure-purch_grp        = ls_procurement-purchgrp      .
        ls_procure-doc_type         = ls_procurement-doctype       .
        ls_procure-pmtterms        = ls_procurement-payterm       .

        APPEND ls_procure TO lt_procure.
        rec_count = rec_count + 1.
        CLEAR ls_procure.
      ENDLOOP.

      IF lt_procure[] IS NOT INITIAL.
        SORT lt_procure BY matnr component ASCENDING.
*        INSERT zdatagen_procure FROM TABLE lt_procure.
        MODIFY zdatagen_procure FROM TABLE lt_procure.
        IF sy-subrc IS INITIAL.
*          WRITE : rec_count,'records uploaded successfully'.
          write upload_rec_count to upload_rec_count_char.
*          CONCATENATE gc_proc_f upload_rec_count_char INTO ls_message-message_v1 SEPARATED BY space.
        ls_message-id = gc_message_class.
        ls_message-number = '053'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_s.
        ls_message-message_v1 = gc_proc_f.
        ls_message-message_v2 = upload_rec_count_char.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
        ELSE.
        ls_message-id = gc_message_class.
        ls_message-number = '054'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
        ls_message-message_v1 = gc_proc_f.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD save_sales_data.
    data : ls_message        TYPE bapiret2.
    IF it_sales[] IS INITIAL.
**      write message buffer content
                ls_message-id = gc_message_class.
          ls_message-number = '050'.
          ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
          ls_message-message_v1 = gc_sales_f.
          zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
    ELSE.
      DATA : ls_sales    TYPE ty_s_sales_data,
             ls_demodata TYPE zdemo_data,
             lt_demodata TYPE TABLE OF zdemo_data.
      DATA : rec_count             TYPE i VALUE 0,
             rec_count_char        TYPE c LENGTH 5,
             upload_rec_count      TYPE i VALUE 0,
             upload_rec_count_char TYPE c LENGTH 5.
      DATA : lv_collectno TYPE numc10,
             rc           TYPE  inri-returncode.
      DATA: lv_timestamp  TYPE timestamp,
            lv_tzntimestp TYPE tzntimestp.

      DESCRIBE TABLE it_sales LINES upload_rec_count.

      LOOP AT it_sales INTO ls_sales.
        ls_demodata-mandt = sy-mandt.
        ls_demodata-collect_no = ls_sales-collect_no.
        ls_demodata-dateposted_str = ls_sales-dateposted_str.
        ls_demodata-doc_type = ls_sales-doc_type.

        ls_demodata-sales_org = ls_sales-sales_org.
        ls_demodata-distr_chan = ls_sales-distr_chan.
        ls_demodata-division = ls_sales-division.
        IF ls_sales-division EQ 0 OR ls_sales-division EQ '00'.
          ls_demodata-division = '00'.
        ENDIF.

        CONVERT DATE ls_demodata-dateposted_str TIME sy-uzeit
                         INTO TIME STAMP lv_timestamp
                         TIME ZONE 'UTC'.
        WRITE:lv_timestamp TO lv_tzntimestp.

        ls_demodata-req_date_h = lv_timestamp.
        ls_demodata-material = ls_sales-material.
        ls_demodata-quantity = ls_sales-quantity.
        ls_demodata-sold_to = ls_sales-sold_to.
        ls_demodata-ship_to = ls_sales-ship_to.
        ls_demodata-dateposted = lv_tzntimestp.

        APPEND ls_demodata TO lt_demodata.
        rec_count = rec_count + 1.
        CLEAR ls_demodata.
      ENDLOOP.

      IF lt_demodata[] IS NOT INITIAL.
        SORT lt_demodata BY collect_no.
        DELETE ADJACENT DUPLICATES FROM  lt_demodata  COMPARING collect_no.
        SORT lt_demodata BY dateposted_str.
*        INSERT zdemo_data FROM TABLE lt_demodata.
        MODIFY zdemo_data FROM TABLE lt_demodata.
        IF sy-subrc IS INITIAL.
*          WRITE : rec_count,'records uploaded successfully'.
          write : upload_rec_count  to upload_rec_count_char.
        ls_message-id = gc_message_class.
        ls_message-number = '051'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_s.
        ls_message-message_v1 = gc_sales_f.
        ls_message-message_v2 = upload_rec_count_char.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
        ELSE.
        ls_message-id = gc_message_class.
        ls_message-number = '052'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
        ls_message-message_v1 = gc_sales_f.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).

        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD save_serproc_data.
data : ls_message        TYPE bapiret2.
    IF it_serproc[] IS INITIAL.
***write proper msg*
          ls_message-id = gc_message_class.
          ls_message-number = '050'.
          ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
          ls_message-message_v1 = gc_serpro_f.
          zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
    ELSE.
      DATA : wa_serproc TYPE ty_s_serpro.
      DATA: ls_serproc            TYPE zserviceprocdata,
*            wa_serproc            TYPE zserviceprocdata,
            lt_serproc            TYPE TABLE OF zserviceprocdata,
            rec_count             TYPE i VALUE 0,
            rec_count_char        TYPE c LENGTH 5,
            vendor_rec_count      TYPE i VALUE 0,
            vendor_rec_count_char TYPE c LENGTH 5,
            upload_rec_count      TYPE i VALUE 0,
            upload_rec_count_char TYPE c LENGTH 5,
            lv_sl_no              TYPE int4.

      DESCRIBE TABLE it_serproc LINES upload_rec_count.

      SELECT MAX( sl_no ) FROM zserviceprocdata INTO lv_sl_no.

      LOOP AT it_serproc INTO wa_serproc.
        ls_serproc-mandt = sy-mandt.
        ls_serproc-sl_no = lv_sl_no + 1.
        ls_serproc-mandt =  sy-mandt.

        ls_serproc-doc_type  = wa_serproc-ordertype. "'NB'."
        ls_serproc-vendor  = wa_serproc-vendor.
        ls_serproc-doc_date  = wa_serproc-documentdate.
        ls_serproc-purch_org  = wa_serproc-purchasingorganization.
        ls_serproc-purch_grp  = wa_serproc-purchasinggroup.
        ls_serproc-comp_code  = wa_serproc-companycode.
        ls_serproc-item_no  = wa_serproc-item. "10 . "
        ls_serproc-acctasscat  = wa_serproc-accountassignmentcategory.
        ls_serproc-material  = wa_serproc-material.
        ls_serproc-quantity  = wa_serproc-poquantity.
        ls_serproc-net_price  = wa_serproc-netprice.
        ls_serproc-currency  = wa_serproc-currency.
        ls_serproc-plant  = wa_serproc-plant.
        ls_serproc-gl_account  = wa_serproc-glaccount.
        ls_serproc-cost_center  = wa_serproc-costcenter.
        ls_serproc-tax_code  = wa_serproc-taxcode.
*        ls_serproc-taxjurcode  = wa_serproc-taxjurisdictioncode.



**    ls_serproc-CHANGED_ON        = wa_serproc-CHANGED_ON.
**    ls_serproc-DOCUMENT_NO       = wa_serproc-DOCUMENT_NO.
**    ls_serproc-STATUS             = wa_serproc-STATUS.
**    ls_serproc-STATUS_DESC        = wa_serproc-STATUS_DESC.
*
**  append ls_serproc to lt_serproc.
**  clear ls_serproc.
**  IF lt_serproc[] is not INITIAL.
**    MODIFY ZSERVICEPROCDATA FROM TABLE lt_serproc.
**  ENDIF.
*
*    IF ls_serproc-vendor IS NOT INITIAL. " do not process records with Vendor and vendor service type
*      vendor_rec_count =  vendor_rec_count + 1.
*      CONTINUE.
*    ELSE.
*      lv_sl_no = lv_sl_no + 1.
*      ls_serproc-sl_no = lv_sl_no.
*      APPEND ls_serproc TO lt_serproc.
*      rec_count = rec_count + 1.
***  ****  process each records by checking if already in the dbtable
**select single * from ZSERVICEPROCDATA into wa_serproc where COMP_CODE = ls_serproc-COMP_CODE
**                                      AND POSTING_DATE = ls_serproc-POSTING_DATE
**                                      AND COST_CENTER = ls_serproc-COST_CENTER.
**  IF sy-subrc is not INITIAL.  "insert/modify if not in db
**    INSERT ZSERVICEPROCDATA from ls_serproc.
**    rec_count = rec_count + 1.
**  ENDIF.
**  clear : ls_serproc, wa_serproc.
*    ENDIF.

        lv_sl_no = lv_sl_no + 1.
        ls_serproc-sl_no = lv_sl_no.
        APPEND ls_serproc TO lt_serproc.
        rec_count = rec_count + 1.

        CLEAR ls_serproc.
      ENDLOOP.

      IF lt_serproc[] IS NOT INITIAL.
        write : upload_rec_count  to upload_rec_count_char.
        INSERT zserviceprocdata FROM TABLE lt_serproc.
        IF sy-subrc is INITIAL.
        ls_message-id = gc_message_class.
        ls_message-number = '057'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_s.
        ls_message-message_v1 = gc_serpro_f.
        ls_message-message_v2 = upload_rec_count_char.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
        ELSE.
        ls_message-id = gc_message_class.
        ls_message-number = '058'.
        ls_message-type = cl_esh_adm_constants=>gc_msgty_e.
        ls_message-message_v1 = gc_serpro_f.
        zcl_mlc_message_buffer=>add_from_bapiret2( is_return = ls_message ).
        ENDIF.
      ENDIF.

    ENDIF.
  ENDMETHOD..
ENDCLASS.
