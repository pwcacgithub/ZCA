class ZCL_CA_UTILITY definition
  public
  create public .

public section.

  data GT_PARAM type ZTTCA_PARAM .
  data GF_IS_ERROR type ABAP_BOOL .
  data GT_HDRINFO type ZTTCA_CONTENT .
  data GT_COPIES type PRI_PARAMS-PRCOP value 1 ##NO_TEXT.
  data GT_IMMEDIATELY type PRI_PARAMS-PRIMM value 'X' ##NO_TEXT.
  data GT_EXPIRATION type PRI_PARAMS-PEXPI value 5 ##NO_TEXT.
  data GT_RECEIVER type PRI_PARAMS-PRREC .
  data GT_DEPARTMENT type PRI_PARAMS-PRABT .
  data GT_LINE_COUNT type PRI_PARAMS-LINCT value 65 ##NO_TEXT.
  data GT_LINE_SIZE type PRI_PARAMS-LINSZ value 132 ##NO_TEXT.
  data GT_AUTHORITY type PRI_PARAMS-PRBER .
  data GT_NEW_LIST_ID type PRI_PARAMS-PRNEW value 'X' ##NO_TEXT.
  data GT_NO_DIALOG type C value 'X' ##NO_TEXT.
  data GT_DESTINATION type PRI_PARAMS-PDEST .
  data C_COPIES type STRING value 'COPIES' ##NO_TEXT.
  data C_IMMEDIATELY type STRING value 'IMMEDIATELY' ##NO_TEXT.
  data C_EXPIRATION type STRING value 'EXPIRATION' ##NO_TEXT.
  data C_RECEIVER type STRING value 'RECEIVER' ##NO_TEXT.
  data C_DEPARTMENT type STRING value 'DEPARTMENT' ##NO_TEXT.
  data C_LINE_COUNT type STRING value 'LINE_COUNT' ##NO_TEXT.
  data C_LINE_SIZE type STRING value 'LINE_SIZE' ##NO_TEXT.
  data C_AUTHORITY type STRING value 'AUTHORITY' ##NO_TEXT.
  data C_NEW_LIST_ID type STRING value 'NEW_LIST_ID' ##NO_TEXT.
  data C_NO_DIALOG type STRING value 'NO_DIALOG' ##NO_TEXT.
  data C_DESTINATION type STRING value 'DESTINATION' ##NO_TEXT.
  constants GC_APP_SERVER type CHAR1 value 'A' ##NO_TEXT.
  constants GC_LOC_SERVER type CHAR1 value 'P' ##NO_TEXT.
  constants GC_CHAR_X type CHAR1 value 'X' ##NO_TEXT.
  constants GC_FLD_FNAME type CHAR5 value 'FNAME' ##NO_TEXT.
  constants GC_FLD_ROWNO type CHAR6 value 'ROWCNT' ##NO_TEXT.

  methods GET_PARAMETER
    importing
      !I_BUSPROC type Z_BUSPROC
      !I_BUSACT type Z_BUSACT
      !I_VALIDDATE type Z_VALIDFROM default SY-DATUM
      !I_PARAM type Z_PARAM optional
    changing
      !T_VALUE type ZTTCA_PARAM_VALUE optional
      !T_PARAM type ZTTCA_PARAM optional
    exceptions
      INVALID_BUSPROCESS
      INVALID_BUSACTIVITY .
  methods SEND_EMAIL
    importing
      !I_REC_TYPE type N default 000
      !I_RECEIVER type STRING optional
      !I_PRIORITY type SO_SND_PRI optional
      !I_CONTENT type SO_OBJ_TP default 'RAW'
      !I_SUBJECT type SO_OBJ_DES
      !I_BODY type BCSY_TEXT
      !I_ATTACHMENT_ATTRIBUTE type ZTTCA_PACKLIST optional
      !I_ATTACHMENT type SOLIX_TAB optional
      !I_IMMEDIATE type CHAR1
    exporting
      !E_RETCODE type I
      !E_ERR_STR type STRING .
  methods GET_EMAIL_CONTENT
    importing
      !I_TEXT_NAME_SUB type THEAD-TDNAME
      !I_TEXT_NAME_BODY type THEAD-TDNAME
      !I_TEXT_REPLACE type ZTTCA_EMAIL_TEXTSYMBOL_REPLACE
    exporting
      !E_SUBJECT type SO_OBJ_DES
      !E_BODY type BCSY_TEXT
      !ET_RETURN type BAPIRET2_T .
  methods DISPLAY_ALV
    changing
      !C_DATATAB type TABLE .
  methods LAYOUT_SET
    importing
      !I_SALVTAB type ref to CL_SALV_TABLE .
  methods OBJTREF_CREATE
    importing
      !IF_LIST type SAP_BOOL optional
    exporting
      !R_SALVREF type ref to CL_SALV_TABLE
    changing
      !C_DATATAB type TABLE .
  methods ADD_TO_LOG
    importing
      !I_MESSAGE type STRING
      !I_MTYPE type BAPI_MTYPE
      !I_MSGNO type SYMSGNO optional
      !I_MSGID type SYMSGID optional
      !I_MSGV1 type SYMSGV optional
      !I_MSGV2 type SYMSGV optional
      !I_MSGV3 type SYMSGV optional
      !I_MSGV4 type SYMSGV optional
      !I_PARAM type BAPI_PARAM optional
      !I_FIELD type BAPI_FLD optional .
  methods COLUMNS_SET
    importing
      !I_SALVTAB type ref to CL_SALV_TABLE .
  methods HDRINFO_ADD
    changing
      !C_INFOTAB type ZTTCA_CONTENT .
  methods SET_PRINT_SPOOL
    importing
      !I_BUSPROC type Z_BUSPROC
      !I_BUSACT type Z_BUSACT
    exporting
      !E_RETCODE type INTEGER
      !E_RETURN type BAPIRET2_T .
  methods GET_BPROC_BACTV
    importing
      !I_INTID type Z_INTFID
    exporting
      !E_BPROC type Z_BUSPROC
      !E_BACTV type Z_BUSACT
    exceptions
      BPROC_BACTV_NOTFOUND .
  methods READ_FILE
    importing
      !I_FILENAME type STRING
      !I_SOURCE type CHAR1
      !I_DELIMITER type CHAR1 optional
      !I_CODEPAGE type ABAP_ENCOD optional
      !I_HDR type ABAP_ENCOD optional
    changing
      !E_DATATAB type TABLE
    exceptions
      CANNOT_OPEN_FILE
      INVALID_DELIMETER
      ERROR_IN_READ
      INVALID_SOURCE .
  methods FCAT_FROM_INTERNAL_TABLE
    importing
      !I_TABLE type ANY TABLE
    returning
      value(RT_FCAT) type LVC_T_FCAT .
protected section.
private section.

  data GF_LIST type SAP_BOOL value SPACE ##NO_TEXT.
  data GR_TABLREF type ref to CL_SALV_TABLE .
  data GR_COLUMNS type ref to CL_SALV_COLUMNS_TABLE .
  data GT_PRSLOG type BAPIRET2_T .
  constants GC_DELMS type CHAR5 value ',|T;' ##NO_TEXT.
  constants GC_BSLASH type CHAR1 value '/' ##NO_TEXT.

  methods ADD_ERROR
    importing
      !I_MESSAGE type STRING .
ENDCLASS.



CLASS ZCL_CA_UTILITY IMPLEMENTATION.


  METHOD add_error.
************************************************************************
* Program ID   : ZCL_CA_UTILITY->ADD_ERROR
* Program Title: Adding Error
* Created By   : Ganesh Salunke
* Creation Date: 11/17/2016
* RICEFW ID    : E299
* Description  : Adding Error
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       E299        D10K900292
*                                      Initial version
************************************************************************
    me->add_to_log( EXPORTING i_mtype = 'E'
                              i_message = i_message ).
  ENDMETHOD.


  METHOD add_to_log.
************************************************************************
* Program ID   : ZCL_CA_UTILITY->ADD_TO_LOG
* Program Title: Adding to LOg
* Created By   : Ganesh Salunke
* Creation Date: 11/17/2016
* RICEFW ID    : E299
* Description  : Adding to Log
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       E299        D10K900292
*                                      Initial version
************************************************************************
    DATA: lw_prclog LIKE LINE OF gt_prslog.

    lw_prclog-type = i_mtype.
    lw_prclog-id = i_msgid.
    lw_prclog-message    = i_message.
    lw_prclog-message_v1 = i_msgv1.
    lw_prclog-message_v2 = i_msgv2.
    lw_prclog-message_v3 = i_msgv3.
    lw_prclog-message_v4 = i_msgv4.
    lw_prclog-parameter  = i_param.
    lw_prclog-number     = i_msgno.
    lw_prclog-field      = i_field.

    APPEND lw_prclog TO gt_prslog.
  ENDMETHOD.


  METHOD columns_set.
************************************************************************
* Program ID   : ZCL_CA_UTILITY->COLUMNS_SET
* Program Title: Set Columns for ALV
* Created By   : Ganesh Salunke
* Creation Date: 11/17/2016
* RICEFW ID    : E299
* Description  : Set Columns for ALV
*
* Additional Information:This Method Used to Change Column Properties
*   Sample Code Provided Below/ Redefine in local class for any added
*   functionality
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       1           D10K900292
*                                      Initial version
* 02/27/2016  x51429       2           Removal of commented code.
************************************************************************
*-----------------------------------------------------------------------
* Data Declarations
*-----------------------------------------------------------------------

    DATA lr_columns    TYPE REF TO cl_salv_columns_table.
    DATA lr_column     TYPE REF TO cl_salv_column_table.
    DATA lt_column_ref TYPE salv_t_column_ref.
    DATA ls_column_ref TYPE salv_s_column_ref.
*-----------------------------------------------------------------------
* Getting Column properties
*-----------------------------------------------------------------------
    lr_columns = i_salvtab->get_columns( ).
    IF lr_columns IS NOT BOUND.
      RETURN.
    ENDIF.
    REFRESH : lt_column_ref.
    CLEAR   : ls_column_ref.
    lt_column_ref = lr_columns->get( ).
*
*
    LOOP AT lt_column_ref INTO ls_column_ref.
*
      TRY.
          lr_column ?= lr_columns->get_column( ls_column_ref-columnname ).
        CATCH cx_salv_not_found.
      ENDTRY.
*-----------------------------------------------------------------------
* Making column invisible / in this case the MANDT/Client field
*-----------------------------------------------------------------------
**    Make Mandt column invisible **
      IF lr_column->get_ddic_datatype( ) = 'CLNT'.
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD display_alv.
************************************************************************
* Program ID   : ZCL_CA_UTILITY->ALV_DISPLAY
* Program Title: ALV Display Method
* Created By   : Ganesh Salunke
* Creation Date: 11/17/2016
* RICEFW ID    : E299
* Description  : Cross-App ALV Display Utility
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       1           D10K900292
*                                      Initial version
* 02/27/2017  x51429       2           D10K901295
*                                      Removing commented code
************************************************************************
    DATA: lr_root      TYPE REF TO cx_root.
    DATA: lr_tablref TYPE REF TO cl_salv_table.


    TRY.
**  Create SALV Reference ----------------------------*

        CALL METHOD me->objtref_create
          IMPORTING
            r_salvref = lr_tablref
          CHANGING
            c_datatab = c_datatab.

        IF lr_tablref IS NOT BOUND.
          RETURN.
        ENDIF.

**  Layout Settings -----------------------------------*
        me->layout_set( lr_tablref ).

**  Set ALV table Columns -----------------------------*
        me->columns_set( lr_tablref ).

**  Display ALV ----------------------------------------*
        lr_tablref->display( ).


      CATCH cx_root INTO lr_root.
        me->add_error( i_message = lr_root->get_text( ) ).
    ENDTRY.

  ENDMETHOD.


  METHOD fcat_from_internal_table.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD FCAT_FROM_INTERNAL_TABLE                                                                                                                       |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

    DATA:
      lt_table TYPE REF TO data.

    CREATE DATA lt_table LIKE i_table.
    ASSIGN lt_table->* TO FIELD-SYMBOL(<fs_table>).
    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = DATA(v_ref_salv_table)
                                CHANGING
                                  t_table        = <fs_table>  ).
        rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = v_ref_salv_table->get_columns( )      " ALV Filter
            r_aggregations = v_ref_salv_table->get_aggregations( ) " ALV Aggregations
    ).
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


METHOD get_bproc_bactv.
************************************************************************
* Program ID:      ZCL_CA_UTILITY-GET_BPROC_BACTV
* Program Title:   Get Businesss process and Business Activity for RICEFW Id
* Created By:      Atique Maroof
* Creation Date:   04/25/2017
* RICEFW ID:       E299
* Description:
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 04/25/2017  X51385       1          D10K902893   1 / Initial version
************************************************************************
  SELECT SINGLE bproc
                bactv
          INTO (e_bproc,e_bactv)
          FROM ztca_intf_track
         WHERE intid = i_intid.
*--Raise exception if Business Process or Business activity
*  is not maintained in Interface Run Tracking table
  IF e_bproc IS INITIAL OR
     e_bactv IS INITIAL.
    RAISE bproc_bactv_notfound.
  ENDIF.
ENDMETHOD.


METHOD get_email_content.

************************************************************************
* Program ID    : GET_EMAIL_CONTENT
* Program Title : Methods for Email Notification
* Created By    : Kunal Gursahani
* Creation Date: 11/20/2016
* RICEFW ID    : E306
* Description  : Methods for Email Notification
*
* Additional Information: This method  is for body and subject of the email
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        Req#       Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/20/2016  x51384      D10K900236       Initial version
************************************************************************


*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
  CONSTANTS : lc_id              TYPE tdid     VALUE 'ST',
              lc_object          TYPE tdobject VALUE 'TEXT',
              lc_keytype_sub(1)  VALUE 'S', "To Identify the  Text Symbols for Email Subject
              lc_keytype_body(1) VALUE 'B', "To Identify the  Text Symbols  for Email Body
              lc_x               TYPE  c VALUE 'X'.

  DATA lv_count TYPE i.
  DATA : lt_tline TYPE TABLE OF tline.
  DATA : lw_tline        TYPE tline,
         lw_body         TYPE soli,
         lw_text_replace TYPE zsca_email_textsymbol_replace,
         ls_head         TYPE thead,
         lw_return       TYPE  bapiret2.
  REFRESH:lt_tline.
  CLEAR: lw_tline,lw_body.


*----------------------------------------*
*--Get Subject Maintained as standard text
*-----------------------------------------*
  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      client                  = sy-mandt
      id                      = lc_id            " ST
      language                = sy-langu          " EN
      name                    = i_text_name_sub
      object                  = lc_object        " TEXT
    IMPORTING
      header                  = ls_head
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    lw_return-type = sy-msgty.
    lw_return-id   = sy-msgid.
    lw_return-number = sy-msgno.
    lw_return-message_v1 = sy-msgv1.
    lw_return-message_v2 = sy-msgv2.
    lw_return-message_v3 = sy-msgv3.
    lw_return-message_v4 = sy-msgv4.
    MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
             WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
             INTO lw_return-message.
    APPEND lw_return TO et_return.
    CLEAR lw_return.
    RETURN.
  ENDIF.
*----------------------------------------*
*--Text Symbol Replace for Subject
*----------------------------------------*
  READ TABLE i_text_replace WITH KEY key_type = lc_keytype_sub TRANSPORTING NO FIELDS BINARY SEARCH.
  IF sy-subrc = 0.
    CALL FUNCTION 'INIT_TEXTSYMBOL'. ##FM_SUBRC_OK
    LOOP AT i_text_replace INTO lw_text_replace WHERE key_type = lc_keytype_sub.
      CALL FUNCTION 'SET_TEXTSYMBOL'
        EXPORTING
          header  = ls_head
          name    = lw_text_replace-name
          value   = lw_text_replace-value
          replace = lc_x.
      CLEAR : lw_text_replace.
    ENDLOOP.
    DESCRIBE TABLE lt_tline LINES lv_count.

    CALL FUNCTION 'REPLACE_TEXTSYMBOL'
      EXPORTING
        endline   = lv_count
        startline = 1
      TABLES
        lines     = lt_tline.
  ENDIF.
*----------------------------------------*
*--Take first 50 characters as subject
*----------------------------------------*
  READ TABLE lt_tline INTO lw_tline INDEX 1 .
  IF sy-subrc = 0.
    e_subject = lw_tline-tdline+0(50).
  ENDIF.

  REFRESH:lt_tline.
  CLEAR:lw_tline,lv_count,ls_head.
*----------------------------------------*
*--Get Body maintained as standard text
*----------------------------------------*
  CALL FUNCTION 'READ_TEXT' ##FM_SUBRC_OK
    EXPORTING
      client                  = sy-mandt
      id                      = lc_id            " ST
      language                = sy-langu          " EN
      name                    = i_text_name_body
      object                  = lc_object        " TEXT
    IMPORTING
      header                  = ls_head
    TABLES
      lines                   = lt_tline
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.
  IF sy-subrc <> 0.
    lw_return-type = sy-msgty.
    lw_return-id   = sy-msgid.
    lw_return-number = sy-msgno.
    lw_return-message_v1 = sy-msgv1.
    lw_return-message_v2 = sy-msgv2.
    lw_return-message_v3 = sy-msgv3.
    lw_return-message_v4 = sy-msgv4.
    MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
             WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
             INTO lw_return-message.
    APPEND lw_return TO et_return.
    CLEAR lw_return.
    RETURN.
  ENDIF.
*----------------------------------------*
*--Text Symbol Replace for Body
*----------------------------------------*

  READ TABLE i_text_replace WITH KEY key_type = lc_keytype_body TRANSPORTING NO FIELDS BINARY SEARCH.
  CALL FUNCTION 'INIT_TEXTSYMBOL'. ##FM_SUBRC_OK
  IF sy-subrc = 0.
    LOOP AT i_text_replace INTO lw_text_replace WHERE key_type = lc_keytype_body.
      CALL FUNCTION 'SET_TEXTSYMBOL'
        EXPORTING
          header  = ls_head
          name    = lw_text_replace-name
          value   = lw_text_replace-value
          replace = lc_x.
      CLEAR : lw_text_replace.
    ENDLOOP.
    DESCRIBE TABLE lt_tline LINES lv_count.

    CALL FUNCTION 'REPLACE_TEXTSYMBOL'
      EXPORTING
        endline   = lv_count
        startline = 1
      TABLES
        lines     = lt_tline.
  ENDIF.

  LOOP AT lt_tline INTO lw_tline.
    lw_body-line = lw_tline-tdline.
    APPEND lw_body TO e_body.
    CLEAR:lw_tline,lw_body.
  ENDLOOP.

  CLEAR : lv_count,ls_head.
  REFRESH: lt_tline.

ENDMETHOD.


METHOD get_parameter.
************************************************************************
* Program ID:      ZCL_CA_UTILITY-GET_RECORD
* Program Title:   Get Parameter range/Mapping values
* Created By:      Atique Maroof
* Creation Date:   11/14/2016
* RICEFW ID:       E299.
* Description:     Get Parameter range/Mapping values
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/14/2016  X51385       1          D10K900236   1 / Initial version
* 2/21/2017  X48737        2          D10K902496  2 / Additional table for param
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
  CONSTANTS: lc_i  TYPE  tvarv_sign    VALUE 'I',
             lc_x  TYPE  c             VALUE 'X',
             lc_eq TYPE  tvarv_opti    VALUE 'EQ'.
*----------------------------------------------------------------------*
* Declaration for Internal Tables
*----------------------------------------------------------------------*
  DATA: lt_param         TYPE TABLE OF ztca_param,
        lt_param_range   TYPE TABLE OF zsca_param_range,
        lt_mapping_value TYPE TABLE OF zsca_map_value.
*----------------------------------------------------------------------*
* Declaration for Work areas
*----------------------------------------------------------------------*
  DATA: lw_param         TYPE    ztca_param,
        lw_t_values      TYPE    zsca_param_value,
        lw_param_range   TYPE    zsca_param_range,
        lw_mapping_value TYPE    zsca_map_value.
************************************************************************
** Validate Business Process and Business Activity
************************************************************************
*--Validate Business Process
  SELECT COUNT(*)
    FROM ztca_busprocess
    WHERE busproc = i_busproc.
  IF sy-dbcnt = 0.
    RAISE invalid_busprocess.
  ENDIF.

*--Validate Business Activity
  SELECT COUNT(*)
    FROM ztca_busactivity
    WHERE busact = i_busact.
  IF sy-dbcnt = 0.
    RAISE invalid_busactivity.
  ENDIF.
**************************************************************
** Read Data From Buffer
** If not found get from DB
**************************************************************
  READ TABLE gt_param WITH KEY busproc = i_busproc
                               busact  = i_busact
              BINARY SEARCH TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    SELECT *
      INTO TABLE gt_param
      FROM ztca_param
      WHERE busproc    =  i_busproc   AND
            busact     like  i_busact    AND
            validfrom  <= i_validdate AND
            validto    >= i_validdate.
    SORT gt_param.
  ENDIF.
**************************************************************
** Filter the Parameter range and Mapping value
** Populate Return Table
**************************************************************
  lt_param = gt_param.
  IF i_param IS NOT INITIAL.
    DELETE lt_param WHERE param <> i_param.
  ENDIF.
  t_param[] = lt_param[].
*--Prepare final output table based on Parameter values
  LOOP AT lt_param INTO lw_param.
*--Populate the range of parameter values
    IF lw_param-mapflag IS INITIAL.
      lw_param_range-sign  = lc_i.
      lw_param_range-opti  = lc_eq.
      lw_param_range-low   = lw_param-value.
      lw_param_range-high  = lw_param-valueto.
      APPEND lw_param_range TO lt_param_range.
    ELSE.
** Populate mapping values
      lw_mapping_value-source_value   = lw_param-value.
      lw_mapping_value-target_value   = lw_param-valueto.
      APPEND lw_mapping_value TO lt_mapping_value.
    ENDIF.
    AT END OF param.
** For each parameter append multiple values
      lw_t_values-param         = lw_param-param.
      lw_t_values-param_range   = lt_param_range.
      lw_t_values-mapping_value = lt_mapping_value.
      APPEND lw_t_values TO t_value.
      CLEAR:lw_t_values.
      REFRESH:lt_param_range, lt_mapping_value.
    ENDAT.
    CLEAR:lw_param,lw_param_range,lw_mapping_value.
  ENDLOOP.
  FREE: lt_param.
ENDMETHOD.


  METHOD hdrinfo_add.

** ************************************************************
    " This Method will be used to dispaly additional information
    " Define and Implement a local class like lcl_event_handler
** *************************************************************

*    DATA: lw_info LIKE LINE OF c_infotab.
*    DATA: lv_date TYPE sy-datum.
*
*    " User Info--------------------------------"
*    lw_info-field = 'User'.
*    lw_info-value = sy-uname.
*    APPEND lw_info TO c_infotab.
*
*    " Date--------------------------------------"
*    lw_info-field = 'Processing Date'.
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        input  = sy-datum
*      IMPORTING
*        output = lv_date.
*    lw_info-value = lv_date .
*    APPEND lw_info TO c_infotab.
*
*    " File Name--------------------------------"
*    IF gv_intf_type = gc_outbound OR
*       gf_dir_read  = abap_false.
*      lw_info-field = 'File Name'.
*    ELSE.
*      lw_info-field = 'Folder Name'.
*    ENDIF.
*    lw_info-value = gv_file_path.
*    APPEND lw_info TO c_infotab.
*
*    " Record COunt-----------------------------"
*    lw_info-field = 'No Of Records'.
*    lw_info-value = gv_recd_scnt.
*    APPEND lw_info TO c_infotab.
*
*    CHECK gv_intf_type = gc_inbound.
*
*    " Error Count-------------------------------"
*    lw_info-field = 'No Of Records Error'.
*    lw_info-value = gv_recd_ecnt.
*    APPEND lw_info TO c_infotab.
*
*    " User Info---------------------------------"
*    lw_info-field = 'Error File Name'.
*    lw_info-value =  gv_errr_path.
*    APPEND lw_info TO c_infotab.
*
*    " User Info---------------------------------"
*    lw_info-field = 'Archive File Name'.
*    lw_info-value = gv_arch_path.
*    APPEND lw_info TO c_infotab.
  ENDMETHOD.


  METHOD layout_set.
************************************************************************
* Program ID   : ZCL_CA_UTILITY->Layout_set
* Program Title: Setting the layout for the ALV
* Created By   : Ganesh Salunke
* Creation Date: 11/17/2016
* RICEFW ID    : E299
* Description  : Setting the ALV Layout
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       E299        D10K900292
*                                      Initial version
************************************************************************
**********************************************************
    " Thie Method will be used to change the layout
    " Sample Code Being Provided/ Redefine in local class
**********************************************************

    DATA lr_functions  TYPE REF TO cl_salv_functions_list .
    DATA lr_columns    TYPE REF TO cl_salv_columns_table.

    CHECK i_salvtab IS BOUND.

**   Layout Setting -------------------------------------------*
*    lr_layout  = I_salvtab->get_layout( ).
*    MOVE sy-repid TO ls_layout_key-report.
*    lr_layout->set_key( ls_layout_key ).
*    lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*
**   Optimize Columns-------------------------------------------*
    lr_columns = i_salvtab->get_columns( ).
    lr_columns->set_optimize( abap_true ).
*    lr_columns->set_key_fixation( if_salv_c_bool_sap=>true ).
*    TRY.
*      lr_columns->set_color_column( 'T_COLOR' ).
*      CATCH cx_salv_data_error .
*    ENDTRY.
*
**   Display Setting--------------------------------------------*
*    lr_display = I_salvtab->get_display_settings( ).
*    lr_display->set_striped_pattern( if_salv_c_bool_sap=>true ).
*    lr_display->set_list_header( sy-title ).
*
**  Get functions details---------------------------------------*
    lr_functions = i_salvtab->get_functions( ).
    lr_functions->set_all( if_salv_c_bool_sap=>true ).
*    " Activate All Buttons in Tool Bar
*
**   Row Selection------------------------------------------------*
*    lr_select = I_salvtab->get_selections( ).
*    IF lr_select IS NOT INITIAL.
*      lr_select->set_selection_mode( if_salv_c_selection_mode=>row_column ).
*      "Allow single row Selection"
*    ENDIF.

  ENDMETHOD.


  METHOD objtref_create.
************************************************************************
* Program ID   : ZCL_CA_UTILITY->objtref_create
* Program Title: This Method return the CL_SALV_TABLE reference
* Created By   : Ganesh Salunke
* Creation Date: 11/17/2016
* RICEFW ID    : E299
* Description  : Cross-App ALV Display Utility
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       E299        D10K900292
*                                      Initial version
************************************************************************
    DATA: lf_list TYPE sap_bool.
    DATA: lr_except TYPE REF TO cx_salv_msg.

    IF if_list IS NOT INITIAL.
      lf_list = if_list.
    ELSE.
      lf_list = gf_list.
    ENDIF.

    TRY.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display = lf_list
          IMPORTING
            r_salv_table = gr_tablref
          CHANGING
            t_table      = c_datatab.

      CATCH cx_salv_msg INTO lr_except.
        me->add_error( 'Exception in CL_SALV_TABLE=>FACTORY').
        me->add_error( lr_except->get_text( ) ).
        gf_is_error = abap_true.
    ENDTRY.
    r_salvref = gr_tablref.
  ENDMETHOD.


  METHOD read_file.
*&-----------------------------------------------------------------------------------------------------------------------------------------*
*& METHOD READ_FILE                                                                                                                        |
*&-----------------------------------------------------------------------------------------------------------------------------------------*

*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
    DATA:
      lt_split  TYPE TABLE OF char200,
      lt_string TYPE TABLE OF string.

*----------------------------------------------------------------------*
* Declaration for work areas
*----------------------------------------------------------------------*
    DATA:
      lw_string TYPE          string,
      lw_split  TYPE          char200,
      lw_flcat  TYPE LINE OF  lvc_t_fcat.

*----------------------------------------------------------------------*
* Declaration for Reference Objects
*----------------------------------------------------------------------*
    DATA:
      lo_ref_line   TYPE REF TO   data,
      lo_exceptions TYPE REF TO cx_root.

*----------------------------------------------------------------------*
* Declaration for Field Symbols
*----------------------------------------------------------------------*
    FIELD-SYMBOLS:
      <fs_data_tab> TYPE STANDARD TABLE,
      <fs_input>    TYPE any, " Generic input file structure
      <fs_fld>      TYPE any.

*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
    DATA:
      lv_filename TYPE string,
      lv_len      TYPE i,
      lv_file     TYPE string,
      lv_tabix    TYPE sy-tabix,
      lv_delm     TYPE char1,
      lv_t        TYPE char1.

    lv_t = 'T'.

* Refresh internal table and clears work area
    REFRESH: lt_split, lt_string.
    CLEAR:lw_string, lw_split, lw_flcat,
          lv_filename, lv_len, lv_file, lv_tabix, lv_delm.

    lv_delm = i_delimiter.
    lv_filename = i_filename.

* Check Delimeter is valid
    IF lv_delm IS INITIAL OR
       lv_delm CA gc_delms.

    ELSE.
      RAISE invalid_delimeter.
    ENDIF.

* Use tab char util for excel
    IF lv_delm = lv_t.
      lv_delm = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.

    CASE i_source.
* Server File - Application Server
      WHEN gc_app_server.
        TRY.
            IF i_codepage IS NOT INITIAL.
              OPEN DATASET lv_filename FOR INPUT IN LEGACY
                             TEXT MODE CODE PAGE i_codepage .
            ELSE.
              OPEN DATASET lv_filename FOR INPUT IN TEXT MODE
                                             ENCODING DEFAULT.
            ENDIF.

            IF sy-subrc <> 0.
              RAISE cannot_open_file.
            ENDIF.

* read the record from file and move it to string type variable
            DO.
              READ DATASET lv_filename INTO  lv_file .
              IF sy-subrc NE 0.
                EXIT.
              ELSE.
                APPEND lv_file TO lt_string.
              ENDIF.
            ENDDO.

            CLOSE DATASET  lv_filename.
          CATCH cx_root INTO lo_exceptions.
            RAISE cannot_open_file.
        ENDTRY.

* Local File - Presentation Server
      WHEN gc_loc_server.

        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = lv_filename
            codepage                = i_codepage
          CHANGING
            data_tab                = lt_string
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.
        IF sy-subrc <> 0.
          RAISE error_in_read.
        ENDIF.

      WHEN OTHERS.
        RAISE invalid_source.
    ENDCASE.

* If the file has header row delete the first row
    IF i_hdr = gc_char_x.
      DELETE lt_string INDEX 1.
    ENDIF.

* Create Output structure and Fill dynamically
    ASSIGN: e_datatab TO <fs_data_tab>.
    CALL METHOD me->fcat_from_internal_table
      EXPORTING
        i_table = e_datatab
      RECEIVING
        rt_fcat = DATA(it_fcat).
    DESCRIBE TABLE it_fcat LINES DATA(lv_lines).

    CREATE DATA lo_ref_line LIKE LINE OF <fs_data_tab>.
    ASSIGN lo_ref_line->* TO <fs_input>.

    "--> Write File Data to Internal Table
    LOOP AT lt_string INTO lw_string.
      CLEAR lv_tabix.

* If File name Column exsists assign File Name
      ASSIGN COMPONENT gc_fld_fname OF STRUCTURE <fs_input> TO <fs_fld>.
      IF sy-subrc = 0.
        lv_tabix  = lv_tabix + 1.
        <fs_fld>  = i_filename.
      ENDIF.

* If Row  Column exsists assign Row
      ASSIGN COMPONENT gc_fld_rowno OF STRUCTURE <fs_input> TO <fs_fld>.
      IF sy-subrc = 0.
        lv_tabix  = lv_tabix + 1.
        <fs_fld>     = sy-tabix.
      ENDIF.

* Assign Value to Column Dynamically
      IF lv_delm IS NOT INITIAL.
        SPLIT lw_string  AT lv_delm INTO TABLE lt_split.
        LOOP AT lt_split INTO lw_split.
          lv_tabix = lv_tabix + 1.
          ASSIGN COMPONENT lv_tabix OF STRUCTURE
                          <fs_input> TO <fs_fld>.
          IF sy-subrc EQ 0.
            <fs_fld> = lw_split.
          ENDIF.
        ENDLOOP.

* String Table
      ELSEIF lv_lines = 1.
        <fs_fld> = lw_string.
      ELSE.
        LOOP AT it_fcat INTO lw_flcat WHERE fieldname NE gc_fld_fname OR
                                            fieldname NE gc_fld_rowno.

          ASSIGN COMPONENT lw_flcat-fieldname OF STRUCTURE <fs_input> TO <fs_fld>.
          <fs_fld>  = lw_string.
          lv_len   = lw_flcat-intlen.
          SHIFT lw_string BY lv_len PLACES.
        ENDLOOP.
      ENDIF.

      APPEND <fs_input> TO e_datatab.
      CLEAR <fs_input>.
    ENDLOOP.
    UNASSIGN: <fs_data_tab>, <fs_input>.

  ENDMETHOD.


METHOD send_email.

************************************************************************
* Method ID     : SEND_MAIL
* Program Title : Methods for Email Notification
* Created By    : Kunal Gursahani
* Creation Date: 11/19/2016
* RICEFW ID    : E306
* Description  : Methods for Email Notification
*
* Additional Information: This method is for sending mail.
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID       Req#     Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/19/2016  x51384     D10K900236     Initial version
************************************************************************

*----------------------------------------------------------------------*
* Declaration for referance variables
*----------------------------------------------------------------------*

  DATA: lr_send_request  TYPE REF TO cl_bcs,
        lr_document      TYPE REF TO cl_document_bcs,
        lr_recipient     TYPE REF TO if_recipient_bcs,
        lr_bcs_exception TYPE REF TO cx_bcs,
        lr_ca_utility    TYPE REF TO zcl_ca_utility.
*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
  DATA: lv_sent_to_all TYPE os_boolean,
        lv_idx1        TYPE i,
        lv_idx2        TYPE i,
        lv_temp_idx    TYPE i,
        lv_xstring     TYPE xstring,
        lv_error       TYPE string,
        lv_size        TYPE so_obj_len,
        lv_sysid       TYPE ad_symbdst.
*----------------------------------------------------------------------*
* Declaration for Internal Tables
*----------------------------------------------------------------------*
  DATA: lt_attach TYPE solix_tab,
        lt_adrs   TYPE TABLE OF string.
*----------------------------------------------------------------------*
* Declaration for Work Areas
*----------------------------------------------------------------------*
  DATA: lw_receiver   TYPE ad_smtpadr,
        lw_username   TYPE ad_uname,
        lw_attribute  TYPE zsca_packlist,
        lw_dl_list    TYPE so_obj_nam,
        lw_attachment TYPE solix,
        lw_adrs       LIKE LINE OF lt_adrs.
*--------------------------------------------------------------------*
* Constant
*---------------------------------------------------------------------*
  CONSTANTS : lc_x   TYPE c VALUE 'X',
              lc_000 TYPE n VALUE '000',
              lc_001 TYPE n VALUE '001',
              lc_002 TYPE n VALUE '002'.

  TRY.

      "--Create persistent send request
      lr_send_request = cl_bcs=>create_persistent( ).

*----------------------------------------------------------------*
* add recipient (e-mail address)
* Create a internet address if the receiver type is INT
* Check whether we have an Email Receiver, Distribution List,
* or SAPUser
*---------------------------------------------------------------*
      CASE i_rec_type.
        WHEN lc_000. "Email Address
          SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
          LOOP AT lt_adrs INTO lw_receiver.

            lr_recipient = cl_cam_address_bcs=>create_internet_address(
                                          lw_receiver ).
            "-- Add Recepient
            CALL METHOD lr_send_request->add_recipient
              EXPORTING
                i_recipient = lr_recipient.
          ENDLOOP.
        WHEN lc_001. "Distribution List
          lw_dl_list = i_receiver.
          lr_recipient = cl_distributionlist_bcs=>getu_persistent(
                                                           i_dliname = lw_dl_list
                                                           i_private = space ).
          "-- Add Recepient
          CALL METHOD lr_send_request->add_recipient
            EXPORTING
              i_recipient = lr_recipient.
        WHEN lc_002. "SAP User
          lv_sysid = sy-sysid.

          SPLIT i_receiver AT ';' INTO TABLE lt_adrs.
          LOOP AT lt_adrs INTO lw_username.

*            lr_recipient = cl_cam_address_bcs=>create_rml_address(
*              i_syst     = lv_sysid
*              i_client   = sy-mandt
*              i_username = lw_username ).
            lr_recipient = cl_sapuser_bcs=>create( lw_username ).

            "-- Add Recepient
            CALL METHOD lr_send_request->add_recipient
              EXPORTING
                i_recipient = lr_recipient.
          ENDLOOP.
        WHEN OTHERS.
*          RAISE EXCEPTION.
      ENDCASE.
*----------------------------------------------------------------*
* create subject and body of the mail
* Create the document with both subject and body.
*---------------------------------------------------------------*
      lr_document = cl_document_bcs=>create_document(
              i_type    = i_content
              i_text    = i_body
              i_length  = '12'
              i_subject = i_subject ).

*---------------------------------------------------------------*
* Attachment handling with the packing list
* Check contents of attachement is not empty
* If the contents are there for the attcahemnt then the packing
* list should not be empty
*---------------------------------------------------------------*
      READ TABLE i_attachment_attribute INTO lw_attribute INDEX 1.
      IF sy-subrc = 0 AND lw_attribute = 'XML'.

        DATA: lv_length TYPE i.
        CALL METHOD cl_bcs_convert=>solix_to_xstring
          EXPORTING
            it_solix   = lt_attach
          RECEIVING
            ev_xstring = lv_xstring.

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lv_xstring
          IMPORTING
            output_length = lv_length
          TABLES
            binary_tab    = lt_attach.

        MOVE lv_length TO lv_size.
        CALL METHOD lr_document->add_attachment
          EXPORTING
            i_attachment_type    = lw_attribute-doc_type
            i_attachment_subject = lw_attribute-obj_descr
            i_attachment_size    = lv_size
            i_att_content_hex    = lt_attach.

      ELSE.

        IF i_attachment IS NOT INITIAL.
          IF i_attachment_attribute IS NOT INITIAL.
            LOOP AT i_attachment_attribute INTO lw_attribute.
              CLEAR : lv_idx1, lv_idx2, lv_temp_idx.
              lv_idx1 = lw_attribute-body_start.
              lv_temp_idx = lw_attribute-body_num.
              lv_idx2 = ( lv_idx1 + lv_temp_idx ) - 1.
*---------------------------------------------------------------------------*
* Loop across the attachement contents internal table and transfer the data
* for particular attachments into another internal table and attach it to
* the mail sending class : cl_document_bcs.
*---------------------------------------------------------------------------*
              REFRESH lt_attach.
              LOOP AT i_attachment INTO lw_attachment FROM lv_idx1 TO lv_idx2.
                APPEND lw_attachment TO lt_attach.
                CLEAR lw_attachment.
              ENDLOOP.
*-------------------------------------------------------------------------------------*
* Now the attachement data for particular attachment is ready in lt_attach so
* now we add the attachment to the docuement.
* Calculate the size of the attachment after converting it to xstring (binary content)
*-------------------------------------------------------------------------------------*
              CLEAR lv_size.
              CALL METHOD cl_bcs_convert=>solix_to_xstring
                EXPORTING
                  it_solix   = lt_attach
                RECEIVING
                  ev_xstring = lv_xstring.

              lv_size = xstrlen( lv_xstring ).
              CALL METHOD lr_document->add_attachment
                EXPORTING
                  i_attachment_type    = lw_attribute-doc_type
                  i_attachment_subject = lw_attribute-obj_descr
                  i_attachment_size    = lv_size
                  i_att_content_hex    = lt_attach.
              CLEAR lw_attribute.
            ENDLOOP.
          ELSE.
*-------------------------------------------------------------------*
** If attachement attribute is not given then send the mail as
** default dpcument type RAW.
*-------------------------------------------------------------------*
            CALL METHOD cl_bcs_convert=>solix_to_xstring
              EXPORTING
                it_solix   = i_attachment
              RECEIVING
                ev_xstring = lv_xstring.

            lv_size = xstrlen( lv_xstring ).
            CALL METHOD lr_document->add_attachment
              EXPORTING
                i_attachment_type    = 'RAW'
                i_attachment_subject = i_subject
                i_attachment_size    = lv_size
                i_att_content_hex    = i_attachment.

          ENDIF.
        ENDIF.
      ENDIF.
*-------------------------------------------------------------------*
*Add document to send request
*-------------------------------------------------------------------*
      CALL METHOD lr_send_request->set_document( lr_document ).
*------------------------------------------------------------------*
*Set the priority
*------------------------------------------------------------------*
      IF i_priority IS NOT INITIAL.
        CALL METHOD lr_send_request->set_priority
          EXPORTING
            i_priority = i_priority.
      ENDIF.
*------------------------------------------------------------------*
* Send Immediately
*------------------------------------------------------------------*
      CALL METHOD lr_send_request->set_send_immediately( i_immediate ).
*------------------------------------------------------------------*
* Send document
*------------------------------------------------------------------*
      CALL METHOD lr_send_request->send(
        EXPORTING
          i_with_error_screen = lc_x
        RECEIVING
          result              = lv_sent_to_all ).

      IF lv_sent_to_all = lc_x.
        e_retcode = 0.
        IF i_immediate IS NOT INITIAL.
          COMMIT WORK.
        ENDIF.
      ELSE.
        e_retcode = 4.
      ENDIF.
*-------------------------------------------------------------------*
*Exception handling
*-------------------------------------------------------------------*
    CATCH cx_bcs INTO lr_bcs_exception.
      e_retcode = 4.
      lv_error = lr_bcs_exception->get_text( ).
      e_err_str = lv_error.
  ENDTRY.

ENDMETHOD.


METHOD set_print_spool.
************************************************************************
* Program ID:      ZCL_CA_UTILITY-SET_PRINT_SPOOL
* Program Title:   Set Spool Parameters
* Created By:      Anila Augusthy
* Creation Date:   11/23/2016
* RICEFW ID:       E310
* Description:     Set Spool Parameters
*
* Additional Information: Spool Parameters are passed to the FM
*  GET_PRINT_PARAMETERS from the ZTCA_PARAM table which maintained against
*  print properties,and making the print propertie dialog box to skip
*  to pop up from the any driver program which deals with printing.
*  If no values found for the table, default values arealso maintained in
*  this method
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/24/2016  X51466        E310         D10K900236
*                                      Initial version
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Internal Tables
*----------------------------------------------------------------------*
  DATA : lt_param TYPE  zttca_param_value.
*----------------------------------------------------------------------*
* Declaration for Work areas
*----------------------------------------------------------------------*
  DATA : gw_param       LIKE LINE OF  lt_param,
         lw_spool       TYPE pri_params,
         lw_param_range TYPE zsca_param_range,
         lw_return      TYPE  bapiret2.
*----------------------------------------------------------------------*
* message types
  CONSTANTS:
    c_msgty_x    TYPE sy-msgty            VALUE 'X',
    c_msgty_a    TYPE sy-msgty            VALUE 'A',
    c_msgty_e    TYPE sy-msgty            VALUE 'E',
    c_msgty_w    TYPE sy-msgty            VALUE 'W',
    c_msgty_i    TYPE sy-msgty            VALUE 'I',
    c_msgty_s    TYPE sy-msgty            VALUE 'S',
    c_msgty_none TYPE sy-msgty            VALUE ' ',
    c_msgclass   TYPE sy-msgid            VALUE 'ZCA_MSG'.

**Get spool parameter values from the parameter table ZTCA_PARAM
*by passing business process and activity and return the print parameter values
*to be used in a driver program
  CALL METHOD me->get_parameter
    EXPORTING
      i_busproc           = i_busproc
      i_busact            = i_busact
      i_validdate         = sy-datum
*     i_param             =
    CHANGING
      t_value             = lt_param
    EXCEPTIONS
      invalid_busprocess  = 1
      invalid_busactivity = 2
      OTHERS              = 3.
  IF lt_param IS INITIAL.
    " Fill Retutn Structure with message
    lw_return-type   =  c_msgty_e.
    lw_return-number =  '010'.
    lw_return-id     =  c_msgclass.
    MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
             WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
             INTO lw_return-message.
    APPEND lw_return TO e_return.
    CLEAR lw_return.
    EXIT.
  ENDIF.
*Looping parameter table to extract the values, if no values found, default values has to be updated
  IF lt_param IS  NOT INITIAL.
    LOOP AT lt_param INTO gw_param.
      CASE gw_param-param.
          "-- Authority
        WHEN c_authority.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_authority = lw_param_range-low.
          ENDIF.
          "-- No of copies
        WHEN c_copies.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_copies = lw_param_range-low.
          ENDIF.
          "-- Department
        WHEN c_department.
          IF gt_department IS   INITIAL.
            READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
            IF sy-subrc EQ 0.
              gt_department = lw_param_range-low.
            ENDIF.
          ENDIF.
          "-- Destination
        WHEN c_destination.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_destination = lw_param_range-low.
          ENDIF.
          "-- Expiration
        WHEN c_expiration.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_expiration = lw_param_range-low.
          ENDIF.
          "-- Immeadietly
        WHEN c_immediately.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_immediately = lw_param_range-low.
          ENDIF.
          "-- Line count
        WHEN c_line_count.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_line_count = lw_param_range-low.
          ENDIF.
          "-- Line Size
        WHEN c_line_size.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_line_size = lw_param_range-low.
          ENDIF.
          "--No Dialog Window
        WHEN c_new_list_id.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_new_list_id = lw_param_range-low.
          ENDIF.
          "-- New Spool Req
        WHEN c_no_dialog.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_no_dialog = lw_param_range-low.
          ENDIF.
          "-- Receiver
        WHEN c_receiver.
          READ TABLE gw_param-param_range[] INTO lw_param_range INDEX 1.
          IF sy-subrc EQ 0.
            gt_receiver = lw_param_range-low.
          ELSE.
            gt_receiver = sy-uname.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.
  ENDIF.

*Get the spool parameters for setting in the driver program
  CALL FUNCTION 'GET_PRINT_PARAMETERS'
    EXPORTING
      authority              = gt_authority
      copies                 = gt_copies
      department             = gt_department
      destination            = gt_destination
      expiration             = gt_expiration
      immediately            = gt_immediately
      line_count             = gt_line_count
      line_size              = gt_line_size
      new_list_id            = gt_new_list_id
      no_dialog              = gt_no_dialog
      receiver               = gt_receiver
    IMPORTING
*     OUT_ARCHIVE_PARAMETERS =
      out_parameters         = lw_spool
*     VALID                  =
*     VALID_FOR_SPOOL_CREATION =
    EXCEPTIONS
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      OTHERS                 = 4.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*  statement used to bypass the dialog box which pop-up when
*  clicking on print from any executable program and set the paraemter
*  values defualted automatically
  NEW-PAGE PRINT ON
               NEW-SECTION
               NO DIALOG
               PARAMETERS lw_spool.

ENDMETHOD.
ENDCLASS.
