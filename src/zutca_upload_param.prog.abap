REPORT zutca_upload_param MESSAGE-ID zca_msg.
* Additional Information:
*This program helps to extract the data from ZTCA_PARAM and to create/modify
*the same table using a .csv file.The file can be either with create entries/
*change the already exisiting.For the exisiting records to modify, the file
*should contain a 'X' in the 'Delimit' column at the end of the file.And the
*file should contain one record with the already exiisting data with a
*delimiter 'X' and new record with changed values as new record
*&---------------------------------------------------------------------*
*&*Types
*&---------------------------------------------------------------------*

TYPE-POOLS:slis.

TYPES: BEGIN OF ty_param,
         busproc   TYPE ztca_param-busproc,
         busact    TYPE ztca_param-busact,
         param     TYPE ztca_param-param,
         value     TYPE ztca_param-value,
         valueto   TYPE ztca_param-valueto,
         validfrom TYPE ztca_param-validfrom,
         validto   TYPE ztca_param-validto,
         mapflag   TYPE ztca_param-mapflag,
         descr     TYPE ztca_param-descr,
         orgauth   TYPE ztca_param-orgauth,
       END OF ty_param.

TYPES: BEGIN OF ty_out,
         busproc   TYPE ztca_param-busproc,
         busact    TYPE ztca_param-busact,
         param     TYPE ztca_param-param,
         value     TYPE ztca_param-value,
         valueto   TYPE ztca_param-valueto,
         validfrom TYPE ztca_param-validfrom,
         validto   TYPE ztca_param-validto,
         mapflag   TYPE ztca_param-mapflag,
         descr     TYPE ztca_param-descr,
         orgauth   TYPE ztca_param-orgauth,
         delimit   TYPE c,
       END OF ty_out.

TYPES: BEGIN OF ty_field,
         field_name(20),
       END OF ty_field.
*&---------------------------------------------------------------------*
*Constants
*&---------------------------------------------------------------------*
CONSTANTS : gc_tabname   TYPE rstable-tabname VALUE 'ZTCA_PARAM',
            gc_zca_msg   TYPE string          VALUE 'ZCA_MSG',
            gc_310       TYPE string          VALUE 'E310',
            gc_validto   TYPE sy-datum        VALUE '99991231',
            gc_org       TYPE c               VALUE '*',
            gc_comma     TYPE c               VALUE ',',
            gc_busproc   TYPE xufield         VALUE 'Z_BUSPROC',
            gc_busact    TYPE xufield         VALUE 'Z_BUSACT',
            gc_authc     TYPE xufield         VALUE 'AUTHC',
            gc_orgauth   TYPE xufield         VALUE 'Z_ORGAUTH',
            gc_zca_param TYPE ust12-objct     VALUE 'ZCA_PARAM',
            gc_i         TYPE tvarv_sign      VALUE 'I',
            gc_eq        TYPE tvarv_opti      VALUE 'EQ',
            gc_e         TYPE c               VALUE 'E',
            gc_x         TYPE c               VALUE 'X',
            gc_s         TYPE c               VALUE 'S',
            gc_c         TYPE c               VALUE 'C',
            gc_r         TYPE c               VALUE 'R',
            gc_p         TYPE c               VALUE 'P',
            gc_u         TYPE c               VALUE 'U'.
*&---------------------------------------------------------------------*
*Global variables for class object
*&---------------------------------------------------------------------*
DATA : gr_data    TYPE REF TO zcl_ca_utility,
       gr_display TYPE REF TO zcl_ca_utility.

*&---------------------------------------------------------------------*
*Global variables
*&---------------------------------------------------------------------*
DATA: gv_line TYPE int4,
      gv_flag TYPE c.

*&---------------------------------------------------------------------*
*Interal table & Work area
*&---------------------------------------------------------------------*
DATA: gt_upload     TYPE TABLE OF ty_out,
      gt_param      TYPE TABLE OF ty_param,
      gt_busproc    TYPE TABLE OF ztca_busprocess,
      gt_busact     TYPE TABLE OF ztca_busactivity,
      gt_parameters TYPE TABLE OF ztca_param,
      lt_filefld    TYPE TABLE OF ty_field,
      gt_return     TYPE TABLE OF bapiret2,
      lw_filefld    TYPE ty_field,
      gw_busact     TYPE ztca_busactivity,
      gw_busproc    TYPE ztca_busprocess,
      gw_upload     TYPE ty_out, "ty_out,
      gv_logobj     TYPE char3.


*---------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*---------------------------------------------------------------------*
* define a local class for handling events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.
ENDCLASS.                    "lcl_handle_events DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*---------------------------------------------------------------------*
* implement the events for handling the events of cl_salv_table
*---------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.
  METHOD on_user_command.
    PERFORM show_function_info USING e_salv_function." TEXT-i08.
  ENDMETHOD.                    "on_user_command

ENDCLASS.                    "lcl_handle_events IMPLEMENTATION
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ca_utility.
  PUBLIC SECTION.
    METHODS display_alv REDEFINITION.
    METHODS columns_set REDEFINITION.
ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.
  METHOD: columns_set.
    DATA lr_columns    TYPE REF TO cl_salv_columns_table.
    DATA lr_column     TYPE REF TO cl_salv_column_table.
    DATA lt_column_ref TYPE salv_t_column_ref.
    DATA ls_column_ref TYPE salv_s_column_ref.
*
    lr_columns = i_salvtab->get_columns( ).
    CHECK lr_columns IS NOT INITIAL.
    REFRESH : lt_column_ref.
    CLEAR   : ls_column_ref.
    lt_column_ref = lr_columns->get( ).
*
*
    LOOP AT lt_column_ref INTO ls_column_ref.
      TRY.
          lr_column ?= lr_columns->get_column( ls_column_ref-columnname ).
        CATCH cx_salv_not_found.
      ENDTRY.

**    Make Mandt column invisible **
      IF ( ( lr_column->get_ddic_datatype( ) = 'CLNT' ) OR
         (   lr_column->get_columnname( )    = 'CHANGEDBY' ) OR
         (   lr_column->get_columnname( )    = 'LASTCHANGED' ) ).
        .
        lr_column->set_technical( if_salv_c_bool_sap=>true ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD  display_alv.
    DATA: lr_root    TYPE REF TO cx_root,
          lr_tablref TYPE REF TO cl_salv_table,
          lr_events  TYPE REF TO cl_salv_events_table,
          gr_events  TYPE REF TO  lcl_handle_events.

    TRY.
        CALL METHOD me->objtref_create
          IMPORTING
            r_salvref = lr_tablref
          CHANGING
            c_datatab = c_datatab.

        CHECK lr_tablref IS BOUND.
        lr_tablref->set_screen_status(  report        = sy-repid
                                        pfstatus      = 'ZPFSTAT'
                                        set_functions = lr_tablref->c_functions_all ).

        lr_events = lr_tablref->get_event( ).

        CREATE OBJECT gr_events.

*.. register to the event USER_COMMAND
        SET HANDLER gr_events->on_user_command FOR lr_events.
**  Layout Settings -----------------------------------*
        me->layout_set( lr_tablref ).

**  Set ALV table Columns -----------------------------*
        me->columns_set( lr_tablref ).

**  Display ALV ----------------------------------------*
        lr_tablref->display( ).

      CATCH cx_root INTO lr_root.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_extr RADIOBUTTON GROUP grp1 USER-COMMAND param DEFAULT 'X', "Extract Data
             p_crup RADIOBUTTON GROUP grp1. "Create/Update
SELECTION-SCREEN END OF BLOCK b1 .

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME.
PARAMETERS : p_bpro TYPE ztca_param-busproc MODIF ID m2,
             p_bact TYPE ztca_param-busact  MODIF ID m2.
PARAMETERS : p_file  TYPE localfile MODIF ID m1.            "D10K901221
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  IF  p_crup = 'X'.
    CALL FUNCTION 'CNV_20000_F4_FILENAME'
      EXPORTING
        program_name  = syst-cprog
        dynpro_number = syst-dynnr
        field_name    = 'P_FILE'
      IMPORTING
        file_name     = p_file.
  ENDIF.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_extr = 'X'.
      IF screen-group1   = 'M1'.
        screen-input     = 0.     "Iput field
        screen-invisible = 1. "*Display field
        screen-required  = 0.  "Mandatory
      ENDIF.

    ELSEIF p_crup = 'X'.
      IF screen-group1   = 'M1'.
        screen-input     = 1.
        screen-invisible = 0.
        screen-active    = 1.
      ENDIF.

      IF screen-group1   = 'M2'.
        screen-input     = 0.
        screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

START-OF-SELECTION.

  IF p_extr = abap_true.
    PERFORM extract_data.   "Extract data of ZTC_PARAM
  ELSEIF p_crup = abap_true.
    PERFORM modify_data.    "Create/Update the data of ZTC_PARAM.
  ENDIF.
  IF gt_return IS   NOT INITIAL.
    PERFORM display_message.
    CLEAR gt_return.
  ENDIF.
*&---------------------------------------------------------------------*
*&  Include           ZUTCA_UPLOAD_PARAM_FORM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  EXTRACT_DATA
*&---------------------------------------------------------------------*
* Extract the data from ZTCA_PARAM by selecting with input parameters
*  with proper authorization checks
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM extract_data .
  TYPES : BEGIN OF ty_orgauth,
            sign TYPE tvarv_sign,
            opti TYPE tvarv_opti,
            low  TYPE ztca_param-orgauth,
            high TYPE ztca_param-orgauth,
          END OF ty_orgauth.
  DATA : lr_alv        TYPE REF TO lcl_alv,
         lt_auth_value TYPE TABLE OF usvalues,
         lw_auth_value TYPE usvalues,
         lt_orgauth    TYPE TABLE OF  ty_orgauth,
         lw_orgauth    TYPE ty_orgauth.



  CLEAR :gr_data,
         gr_display.
** Authorization check for read
  AUTHORITY-CHECK OBJECT gc_zca_param
    ID gc_busproc FIELD p_bpro
    ID gc_busact  FIELD p_bact
*    ID gc_orgauth FIELD gc_org " Default
    ID gc_authc   FIELD gc_r.
  IF sy-subrc EQ 0.
*Fetching data as per selection screen
    SELECT
      busproc
      busact
      param
      value
      valueto
      validfrom
      validto
      mapflag
      descr
      orgauth
      FROM ztca_param
      INTO TABLE gt_param
      WHERE busproc  = p_bpro
      AND   busact   = p_bact
      AND   validto  >= sy-datum.  "Avoid the due records

    IF gt_param IS NOT INITIAL.
**--Get User's authorization for ZCA_PARAM
      CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
        EXPORTING
          mandant    = sy-mandt
          user_name  = sy-uname
          sel_object = gc_zca_param
        TABLES
          values     = lt_auth_value.
      IF lt_auth_value IS   NOT INITIAL.
        SORT lt_auth_value BY field.
        LOOP AT lt_auth_value INTO lw_auth_value
          WHERE field = gc_orgauth.
          CLEAR:lw_orgauth.
          lw_orgauth-sign   = gc_i.
          lw_orgauth-opti   = gc_eq.
          lw_orgauth-low    = lw_auth_value-von.
          lw_orgauth-high   = lw_auth_value-bis.
          APPEND lw_orgauth TO lt_orgauth.
          CLEAR:lw_auth_value.
        ENDLOOP.
      ENDIF.
**--Filter the internal table based on Orgauth authorization
      IF lt_orgauth IS NOT INITIAL.
**--If Authorization object contains * then don't delete
        READ TABLE lt_orgauth
          WITH KEY low = gc_org TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          DELETE gt_param WHERE orgauth NOT IN lt_orgauth.
        ENDIF.
      ENDIF.

      IF gt_param IS NOT INITIAL..
*    call method display_alv
        CREATE OBJECT lr_alv.
        CALL METHOD lr_alv->display_alv
          CHANGING
            c_datatab = gt_param.
      ELSE.
        MESSAGE s010.
      ENDIF.
    ELSE.
      MESSAGE s010.
    ENDIF.
  ELSE.
    MESSAGE s014 DISPLAY LIKE gc_e WITH p_bpro p_bact.
  ENDIF.


ENDFORM.                    "extract_data
*&---------------------------------------------------------------------*
*&      Form  MODIFY_DATA
*&---------------------------------------------------------------------*
* flow for the program when clicking on the radio button Modify
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM modify_data .
  PERFORM upload_file.
  PERFORM validate_data.
  PERFORM process_data.
ENDFORM.                    "modify_data
*&---------------------------------------------------------------------*
*&      Form  UPLOAD_FILE
*&---------------------------------------------------------------------*
*Uploading the .csv file with data to be updated in ZCTA_PARAM table.
*Read the file and convert it to internal table for validation prupose
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM upload_file .

  DATA: lv_file TYPE string.

  DATA: lr_file_util TYPE REF TO zcl_ca_utility_file.

  DATA: lv_start_col TYPE i  VALUE 1,
        lv_start_row TYPE i  VALUE 1,
        lv_end_col   TYPE i  VALUE 11,
        lv_end_row   TYPE i  VALUE 100000,
        lv_ex_fnam   TYPE rlgrap-filename,
        lt_excel     TYPE TABLE OF cnv_10940_alsmex_tabline,
        ls_excel     TYPE cnv_10940_alsmex_tabline,
        lv_length    TYPE i.

  lv_ex_fnam = lv_file = p_file.
  lv_length = strlen( lv_ex_fnam ) - 4.
  TRANSLATE lv_ex_fnam TO UPPER CASE.
  IF lv_ex_fnam+lv_length(4) = 'XLSX'.
    CALL FUNCTION 'CNV_10940_EXCEL_TO_IN'
      EXPORTING
        filename                = lv_ex_fnam
        i_begin_col             = lv_start_col
        i_begin_row             = lv_start_row
        i_end_col               = lv_end_col
        i_end_row               = lv_end_row
      TABLES
        intern                  = lt_excel
      exceptions
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.
    IF sy-subrc <> 0.
      MESSAGE TEXT-002 TYPE gc_s DISPLAY LIKE gc_e.
    ELSE.
      DELETE lt_excel WHERE row = '0001'.
      SORT lt_excel BY row col.
      LOOP AT lt_excel INTO ls_excel.
        CASE ls_excel-col.
          WHEN '0001'.
            gw_upload-busproc   = ls_excel-value.
          WHEN '0002'.
            gw_upload-busact    = ls_excel-value.
          WHEN '0003'.
            gw_upload-param     = ls_excel-value.
          WHEN '0004'.
            gw_upload-value     = ls_excel-value.
          WHEN '0005'.
            gw_upload-valueto   = ls_excel-value.
          WHEN '0006'.
            gw_upload-validfrom = ls_excel-value.
          WHEN '0007'.
            gw_upload-validto   = ls_excel-value.
          WHEN '0008'.
            gw_upload-mapflag   = ls_excel-value.
          WHEN '0009'.
            gw_upload-descr     = ls_excel-value.
          WHEN '0010'.
            gw_upload-orgauth   = ls_excel-value.
          WHEN '0011'.
            gw_upload-delimit   = ls_excel-value.
        ENDCASE.
        AT END OF row.
          APPEND gw_upload TO gt_upload.
          CLEAR gw_upload.
        ENDAT.
      ENDLOOP.
    ENDIF.
  ELSE.
*Download file to presentation server
    CREATE OBJECT lr_file_util.

    CALL METHOD lr_file_util->read_file
      EXPORTING
        i_filename        = lv_file
        i_source          = gc_p
        i_delimiter       = gc_comma
*       i_codepage        =
        i_hdr             = gc_x
      CHANGING
        e_datatab         = gt_upload
      EXCEPTIONS
        cannot_open_file  = 1
        invalid_delimeter = 2
        error_in_read     = 3
        invalid_source    = 4
        OTHERS            = 5.
    IF sy-subrc <> 0.
      CASE  sy-subrc.
        WHEN 1.
          MESSAGE TEXT-002 TYPE gc_s DISPLAY LIKE gc_e.
        WHEN 2.
          MESSAGE TEXT-003 TYPE gc_s DISPLAY LIKE gc_e.
        WHEN 3.
          MESSAGE TEXT-004 TYPE gc_s DISPLAY LIKE gc_e.
        WHEN 4.
          MESSAGE TEXT-005 TYPE gc_s DISPLAY LIKE gc_e.
        WHEN 5.
          MESSAGE TEXT-006 TYPE gc_s DISPLAY LIKE gc_e.
      ENDCASE..
*   to do
    ENDIF.
  ENDIF.
ENDFORM.                    "upload_file
*&---------------------------------------------------------------------*
*&      Form  PROCESS_DATA
*&---------------------------------------------------------------------*
* All the validations will be in checked record by record which is read
*from the file of an internal table gt_upload
*After the proper validation, table ZTCA_PARAM has been modified with
*proper lock
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM process_data .
  DATA: lt_param       TYPE TABLE OF ztca_param,
        lt_upload_temp TYPE TABLE OF ty_out,
        lv_ind         TYPE sy-tabix,
        lw_param       TYPE ztca_param,
        lv_par1        TYPE string,
        lv_par2        TYPE string.

  DATA: lv_varkey  LIKE rstable-varkey,
        lv_line(6) TYPE i,
        lv_type    TYPE c,
        lv_msgid   TYPE sy-msgid,
        lv_var     TYPE int4,
        lv_tabix   TYPE sy-tabix,
        lv_date    TYPE sy-datum,
        lr_applog  TYPE REF TO zcl_ca_utility_applog.


  FIELD-SYMBOLS: <fs_parameters> TYPE ztca_param,
                 <fs_upload>     TYPE ty_out.

  CLEAR : gw_upload,
          gw_busproc,
          gw_busact,
          lv_varkey,
          lv_type,
          lv_msgid,
          lv_var,
          lv_tabix,
          lv_date,
          lr_applog,
          lv_line,
          lv_par1,
          lv_par2.

* Uploading the data into the database table
  LOOP AT gt_upload ASSIGNING FIELD-SYMBOL(<fs>).

*For the line number of the record in the upload file
    CLEAR lv_tabix.
    lv_tabix = sy-tabix.

** Authorization check for Update
    IF <fs>-delimit = abap_true.
      AUTHORITY-CHECK OBJECT gc_zca_param
      ID gc_busproc FIELD <fs>-busproc
      ID gc_busact  FIELD <fs>-busact
      ID gc_authc   FIELD gc_u.
      IF sy-subrc <> 0.
        lv_type = gc_e.
        lv_msgid = 013.
        lv_line = lv_tabix.
        PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                        lv_msgid
                                        <fs>-busproc
                                        <fs>-busact
                                        lv_line.

        CONTINUE.
      ENDIF.
** Authorization check for Create
    ELSE.
      AUTHORITY-CHECK OBJECT gc_zca_param
      ID gc_busproc FIELD <fs>-busproc
      ID gc_busact  FIELD <fs>-busact
      ID gc_authc   FIELD gc_c.
      IF sy-subrc <> 0.
        lv_type = gc_e.
        lv_msgid = 000.
        lv_line = lv_tabix.
        PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                        lv_msgid
                                        <fs>-busproc
                                        <fs>-busact
                                        lv_line.
        CONTINUE.
      ENDIF.
    ENDIF.
**  Validation for bussiness process
    READ TABLE gt_busproc INTO gw_busproc WITH KEY busproc = <fs>-busproc.
    IF sy-subrc <> 0.
      lv_type = gc_e.
      lv_msgid = 001.
      lv_line = lv_tabix.
      PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                      lv_msgid
                                      lv_par1
                                      lv_par2
                                      lv_line.
      CONTINUE.
    ENDIF.

**  Validation for bussiness activity
    READ TABLE gt_busact INTO gw_busact WITH KEY busact  = <fs>-busact.
    IF sy-subrc <> 0.
      lv_type = gc_e.
      lv_msgid = 003.
      lv_line = lv_tabix.
      PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                      lv_msgid
                                      lv_par1
                                      lv_par2
                                      lv_line.
      CONTINUE.
    ENDIF.
*--Begin Of Change X51385 05/16/2017 D10K903920
*--Validate the parameter overlapping in database
    IF gt_parameters IS NOT INITIAL AND <fs>-delimit = abap_false.
      CLEAR: lv_type, lv_msgid,lv_par1,lv_par2,lv_line.
*--For Parameter value
      IF <fs>-mapflag IS INITIAL.
        LOOP AT gt_parameters ASSIGNING <fs_parameters>
                    WHERE busproc   = <fs>-busproc
                    AND   busact    = <fs>-busact
                    AND   param     = <fs>-param
                    AND   value     = <fs>-value
                    AND   valueto   = <fs>-valueto
                    AND   mapflag   = space.
          IF <fs_parameters>-validfrom <= <fs>-validto AND
             <fs_parameters>-validto   >= <fs>-validfrom.  " Overlaps
            lv_type = gc_e.
            lv_msgid = 032.
            lv_par1 = <fs>-param.
            lv_line = lv_tabix.
            PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                            lv_msgid
                                            lv_par1
                                            lv_par2
                                            lv_line.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
*--For Mapping Value
        LOOP AT gt_parameters ASSIGNING <fs_parameters>
                    WHERE busproc   = <fs>-busproc
                    AND   busact    = <fs>-busact
                    AND   param     = <fs>-param
                    AND   value     = <fs>-value
                    AND   mapflag   = abap_true.
          IF <fs_parameters>-validfrom <= <fs>-validto AND
             <fs_parameters>-validto   >= <fs>-validfrom.  " Overlaps
            lv_type = gc_e.
            lv_msgid = 038.
            lv_par1 = <fs>-param.
            lv_line = lv_tabix.
            PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                            lv_msgid
                                            lv_par1
                                            lv_par2
                                            lv_line.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
*--If the parameters overlaps then skip the record
      IF lv_type = gc_e.
        CONTINUE.
      ENDIF.
    ENDIF.
*--Validate the parameter overlapping in the load file
    IF lt_upload_temp IS NOT INITIAL AND <fs>-delimit = abap_false.
      CLEAR: lv_type, lv_msgid,lv_par1,lv_par2,lv_line.
*--For Parameter Value
      IF <fs>-mapflag IS INITIAL.
        LOOP AT lt_upload_temp ASSIGNING <fs_upload>
                    WHERE busproc   = <fs>-busproc
                    AND   busact    = <fs>-busact
                    AND   param     = <fs>-param
                    AND   value     = <fs>-value
                    AND   valueto   = <fs>-valueto
                    AND   mapflag   = space.

          IF <fs_upload>-validfrom <= <fs>-validto AND
             <fs_upload>-validto   >= <fs>-validfrom.  " Overlaps
            lv_type = gc_e.
            lv_msgid = 035.
            lv_par1 = <fs>-param.
            lv_line = lv_tabix.
            PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                            lv_msgid
                                            lv_par1
                                            lv_par2
                                            lv_line.
            EXIT.

          ENDIF.
        ENDLOOP.
*--For Mapping Value
      ELSE.
        LOOP AT lt_upload_temp ASSIGNING <fs_upload>
                    WHERE busproc   = <fs>-busproc
                    AND   busact    = <fs>-busact
                    AND   param     = <fs>-param
                    AND   value     = <fs>-value
                    AND   mapflag   = abap_true.

          IF <fs_upload>-validfrom <= <fs>-validto AND
             <fs_upload>-validto   >= <fs>-validfrom.  " Overlaps
            lv_type = gc_e.
            lv_msgid = 039.
            lv_par1 = <fs>-param.
            lv_line = lv_tabix.
            PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                            lv_msgid
                                            lv_par1
                                            lv_par2
                                            lv_line.
            EXIT.

          ENDIF.
        ENDLOOP.
      ENDIF.
*--If the parameters overlaps then skip the record
      IF lv_type = gc_e.
        CONTINUE.
      ENDIF.
    ENDIF.
*--End Of Change X51385 05/16/2017 D10K903920
*Validating the record which already exist
    lv_date = sy-datum - 1.
    READ TABLE gt_parameters TRANSPORTING NO FIELDS WITH KEY busproc   = <fs>-busproc
                                                             busact    = <fs>-busact
                                                             param     = <fs>-param
                                                             value     = <fs>-value
                                                             valueto   = <fs>-valueto
                                                             validfrom = <fs>-validfrom.
*Change the validto to current date - 1
    IF sy-subrc EQ 0.
      IF <fs>-delimit EQ abap_true.
*--Begin Of Change X51385 07/03/2017 D10K906627
*        AND <fs>-validto NE lv_date.
        IF <fs>-validto IS INITIAL.
          <fs>-validto = lv_date.
        ENDIF.
*--End Of Change X51385 07/03/2017 D10K906627
        gv_flag = abap_true.
*Already record can not update with same details again in the same date
      ELSE.
        lv_type = gc_s.
        lv_msgid = 012.
        lv_line = lv_tabix.
        PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                        lv_msgid
                                        lv_par1
                                        lv_par2
                                        lv_line.
        CONTINUE.
      ENDIF.
    ELSE.
      gv_flag = abap_true.

*Defaulting valid from if the changed data is of past valid from date
*      IF <fs>-validfrom < sy-datum.
*        <fs>-validfrom = sy-datum.
*      ENDIF.
    ENDIF.
    IF gv_flag EQ abap_true.
*Defaulting valid to
      IF <fs>-validto EQ ' '.
        <fs>-validto = gc_validto.
      ENDIF.

*Defaulting valid to
      IF <fs>-validfrom EQ ' '.
        <fs>-validfrom = sy-datum.
      ENDIF.

** Validate Date format for Valid to date
      CALL FUNCTION 'RP_CHECK_DATE'
        EXPORTING
          date         = <fs>-validto
        EXCEPTIONS
          date_invalid = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        lv_type = gc_e.
        lv_msgid = 016.
        lv_line = lv_tabix.
        PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                        lv_msgid
                                        <fs>-validto
                                        lv_par2
                                        lv_line.
        CONTINUE.
      ENDIF.

** Validate Date format for Valid from date
      CALL FUNCTION 'RP_CHECK_DATE'
        EXPORTING
          date         = <fs>-validfrom
        EXCEPTIONS
          date_invalid = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        lv_type = gc_e.
        lv_msgid = 016.
        lv_line = lv_tabix.
        PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                        lv_msgid
                                        <fs>-validfrom
                                        lv_par2
                                        lv_line.
        CONTINUE.
      ENDIF.


** Valid to date must be greater than or equal to start date
      IF  <fs>-validto < <fs>-validfrom .
        lv_type = gc_e.
        lv_msgid = 015.
        lv_line = lv_tabix.
        PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                        lv_msgid
                                        lv_par1
                                        lv_par2
                                        lv_line.
        CONTINUE.
      ENDIF.
*Defaulting chnaged by and date
      lw_param-changedby   = sy-uname.
      lw_param-lastchanged = sy-datum.

** Default Organisational authorization
      IF <fs>-orgauth IS INITIAL.
        <fs>-orgauth       = gc_org.
      ENDIF.

      MOVE-CORRESPONDING <fs> TO lw_param.
      lw_param-mandt = sy-mandt.
      APPEND lw_param TO lt_param.
    ENDIF.
    APPEND <fs> TO lt_upload_temp.
    CLEAR : lv_par1, lv_par2.
  ENDLOOP.

  DESCRIBE TABLE lt_param LINES gv_line.
*Database update
  IF lt_param IS   NOT INITIAL.
*Lock details in system before update
    lv_varkey = sy-mandt.
    CALL FUNCTION 'ENQUEUE_E_TABLE'
      EXPORTING
        tabname        = gc_tabname
        varkey         = lv_varkey
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
*Modify the table ZTCA_PARAM
      MODIFY ztca_param FROM TABLE lt_param.
      IF sy-subrc EQ 0.
        COMMIT WORK.
*Data records updated successfully'.

        lv_type = gc_s.
        lv_msgid = 011.
        lv_par1 = gv_line.
        PERFORM return_message USING gc_zca_msg CHANGING lv_type
                                        lv_msgid
                                        lv_par1
                                        lv_par2
                                        lv_line.


*Logging the message for succefull updation
        gv_logobj = p_bpro.
        CREATE OBJECT lr_applog.
        CALL METHOD lr_applog->create_log
          EXPORTING
            iv_object   = gv_logobj
            iv_subobj   = p_bact
            iv_external = gc_310
            it_logs     = gt_return.
      ENDIF.
    ENDIF.

*Unloack the table
    CALL FUNCTION 'DEQUEUE_E_TABLE'
      EXPORTING
        tabname = gc_tabname
        varkey  = lv_varkey.
  ENDIF.
ENDFORM.                    "process_data
*&---------------------------------------------------------------------*
*&      Form  VALIDATE_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM validate_data .

  CLEAR : gt_busproc,
          gt_busact,
          gt_parameters.
*fetching the busprocess and activity from master table for validation purpose
  SELECT *
    FROM ztca_busprocess
    INTO TABLE gt_busproc.

  SELECT *
    FROM  ztca_busactivity
    INTO  TABLE gt_busact.

  IF gt_upload IS NOT INITIAL.

    SELECT *
      FROM ztca_param
      INTO TABLE gt_parameters
      FOR ALL ENTRIES IN gt_upload
      WHERE busproc   = gt_upload-busproc
      AND   busact    = gt_upload-busact
      AND   param     = gt_upload-param
      AND   value     = gt_upload-value
      AND   valueto   = gt_upload-valueto.
*--Begin Of Change X51385 05/16/2017 D10K903920
*      AND   validfrom = gt_upload-validfrom
*      AND   validto   = gt_upload-validto.
*--End Of Change X51385 05/16/2017 D10K903920
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  show_function_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_function_info USING i_function TYPE salv_de_function.

  DATA : lv_filename TYPE string,
         lv_path     TYPE string,
         lv_fullpath TYPE string.


  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Select The Path Where The File Should Be Saved'
      default_file_name = 'ParameterUpload.csv'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath.

  DATA: lr_file_util TYPE REF TO zcl_ca_utility_file,
        lt_fcat      TYPE lvc_t_fcat,
        lw_fcat      LIKE LINE OF lt_fcat.

  DATA :lr_fcat TYPE REF TO zcl_ca_utility_file.

*  DATA : lt_fcat TYPE lvc_t_fcat.
  CREATE OBJECT lr_fcat.
  CALL METHOD lr_fcat->fcat_from_internal_table
    EXPORTING
      i_table = gt_upload
    RECEIVING
      rt_fcat = lt_fcat.

*  getting filed label for the header
  LOOP AT lt_fcat INTO lw_fcat.

    IF lw_fcat-scrtext_l IS NOT INITIAL.
      lw_filefld-field_name = lw_fcat-scrtext_l.
    ELSE.
      lw_filefld-field_name = TEXT-007.
    ENDIF.
    APPEND lw_filefld TO lt_filefld.
    CLEAR lw_filefld.
  ENDLOOP.

* Move the extracted data to the internal table format
* to be downloaded
  gt_upload[] = gt_param[].

*Write file to gt_upload
  CREATE OBJECT lr_file_util.

  CALL METHOD lr_file_util->write_file
    EXPORTING
      i_filename        = lv_fullpath
      i_source          = gc_p
      i_delimiter       = gc_comma
      i_addheader       = gc_x
      i_fields          = lt_filefld
    CHANGING
      i_datatab         = gt_upload
    EXCEPTIONS
      invalid_delimiter = 1
      invalid_source    = 2
      write_error       = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
*     Implement suitable error handling here
  ENDIF.


ENDFORM.                    " show_function_info
*&---------------------------------------------------------------------*
*&      Form  RETURN_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_ZCA_MSG  text
*      <--P_lv_type  text
*      <--P_LV_NUMBER  text
*----------------------------------------------------------------------*
FORM return_message  USING    p_c_zca_msg
                     CHANGING p_lv_type
                              p_lv_msgid
                              p_lv_par1
                              p_lv_par2
                              p_lv_line.

  DATA: ls_return LIKE bapiret2,
        lv_cl     TYPE sy-msgid,
        lv_no     TYPE sy-msgno,
        lv_par1   TYPE sy-msgv1,
        lv_par2   TYPE sy-msgv2,
        lv_row    TYPE bapiret2-row.

  lv_no = p_lv_msgid.
  lv_cl = p_c_zca_msg.
  lv_par1 = p_lv_par1.
  lv_par2 = p_lv_par2.
  lv_row = p_lv_line.
  CLEAR ls_return.
*  CALL FUNCTION 'AIPB_BAPIRET2_FILL'
*    EXPORTING
*      type   = p_lv_type
*      cl     = p_c_zca_msg
*      par1   = p_lv_par1
*      par2   = p_lv_par2
*      number = p_lv_msgid
*      row    = p_lv_line
*    IMPORTING
*      return = ls_return.

  CALL FUNCTION 'BALW_BAPIRETURN_GET2'
    EXPORTING
      type   = p_lv_type
      cl     = lv_cl
      number = lv_no
      par1   = lv_par1
      par2   = lv_par2
      row    = lv_row
    IMPORTING
      return = ls_return.

  APPEND ls_return TO gt_return.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_MESSAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_message .

  DATA : lr_alv_msg TYPE REF TO zcl_ca_utility.

  CREATE OBJECT lr_alv_msg.
  CALL METHOD lr_alv_msg->display_alv
    CHANGING
      c_datatab = gt_return.

ENDFORM.
