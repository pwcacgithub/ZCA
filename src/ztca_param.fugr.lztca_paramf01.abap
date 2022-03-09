*----------------------------------------------------------------------*
***INCLUDE LZTCA_PARAMF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  BEFORE_DELETE
*&---------------------------------------------------------------------*
*   This form is called before Delete operation and it will validate the
*   authorization for deletion abd give a popup message asking for
*   Deletion confirmation
*----------------------------------------------------------------------*
FORM before_delete .
*----------------------------------------------------------------------*
* Declaration for Structures
*----------------------------------------------------------------------*
  TYPES: BEGIN OF ty_tot_param.
      INCLUDE STRUCTURE ztca_param.
      INCLUDE STRUCTURE vimflagtab.
  TYPES: END OF ty_tot_param.
*----------------------------------------------------------------------*
* Declaration for Internal table, Work area and Variable
*----------------------------------------------------------------------*
  DATA : lt_tot_param TYPE TABLE OF ty_tot_param,
         lv_result    TYPE c,
         lw_tot_param TYPE ty_tot_param.

  CONSTANTS : lc_m TYPE c VALUE 'M'.

  CLEAR:lv_result ,lw_tot_param.
  REFRESH: lt_tot_param.
**--Get the selected records for Deletion
  lt_tot_param[] = total[].
  SORT lt_tot_param BY mark.
  LOOP AT lt_tot_param INTO lw_tot_param WHERE mark = lc_m.
**--Authorization check for Delete entry
    AUTHORITY-CHECK OBJECT gc_zca_param
        ID gc_busproc FIELD lw_tot_param-busproc
        ID gc_busact  FIELD lw_tot_param-busact
        ID gc_orgauth FIELD lw_tot_param-orgauth
        ID gc_authc   FIELD gc_d.
    IF sy-subrc <> 0.
      vim_abort_saving = gc_x.
      MESSAGE TEXT-004 TYPE gc_s DISPLAY LIKE gc_e.
      LEAVE SCREEN.
    ENDIF.
    CLEAR lw_tot_param.
  ENDLOOP.
**--Call FM to Pop up a message
  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = TEXT-003
      text_question         = TEXT-000
      text_button_1         = TEXT-001   " Yes
      text_button_2         = TEXT-002   " No
      display_cancel_button = ''
    IMPORTING
      answer                = lv_result.
  IF lv_result IS NOT INITIAL.
**--If Answer is No then take back to overview screen
    IF lv_result = 2.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_NEW_ENTRY
*&---------------------------------------------------------------------*
* This form is called after the create operation and it defaults the
* fields Valid from, Valid to, Value to, Organization authorization
* Changed by user and Last changed data
*----------------------------------------------------------------------*
FORM create_new_entry.
*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
  DATA : lw_param  TYPE ztca_param,
         lv_index  TYPE sy-tabix,
         lv_length TYPE i.
  DATA: lt_param TYPE STANDARD TABLE OF ztca_param.
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
  CONSTANTS :lc_validto TYPE sy-datum VALUE '99991231',
             lc_orgauth TYPE c        VALUE '*'.

  CLEAR: lw_param,lt_param.
*----------------------------------------------------------------------*
* Duplicate Parameter/Mapping Values check within a given date range
*----------------------------------------------------------------------*
  lt_param = total[].
*--Duplicate value check for a Parameter within the validity range
  IF ztca_param-mapflag IS INITIAL.
    LOOP AT lt_param INTO lw_param WHERE busproc   = ztca_param-busproc     AND
                                         busact    = ztca_param-busact      AND
                                         param     = ztca_param-param       AND
                                         value     = ztca_param-value       AND
                                         valueto   = ztca_param-valueto     AND
                                         mapflag   = space.
      IF lw_param-validfrom <= ztca_param-validto AND
         lw_param-validto   >= ztca_param-validfrom.
        vim_abort_saving = gc_x.
        sy-subrc = 4.
        MESSAGE e037(zca_msg) WITH lw_param-param.
        EXIT.
      ENDIF.
      CLEAR: lw_param.
    ENDLOOP.
*--Duplicate value check for a Mapping value within the validity range
  ELSE.
    LOOP AT lt_param INTO lw_param WHERE busproc   = ztca_param-busproc     AND
                                         busact    = ztca_param-busact      AND
                                         param     = ztca_param-param       AND
                                         value     = ztca_param-value       AND
                                         mapflag   = abap_true.
      IF lw_param-validfrom <= ztca_param-validto AND
         lw_param-validto   >= ztca_param-validfrom.
        vim_abort_saving = gc_x.
        sy-subrc = 4.
        MESSAGE e038(zca_msg) WITH lw_param-param.
        EXIT.
      ENDIF.
      CLEAR: lw_param.
    ENDLOOP.
  ENDIF.
*----------------------------------------------------------------------*
* Default the Fields if blank
*----------------------------------------------------------------------*
**--If start date is not entered then default the current date
  IF ztca_param-validfrom IS INITIAL.
    ztca_param-validfrom  = sy-datum.
  ENDIF.
**--If end date is not entered then default 99991231
  IF ztca_param-validto IS INITIAL .
    ztca_param-validto = lc_validto.
  ENDIF.
**--For parameter range Copy the Value to Valueto
  IF ztca_param-mapflag IS INITIAL AND
     ztca_param-valueto IS INITIAL.
    ztca_param-valueto =  ztca_param-value.
  ENDIF.
**--Default ORGAUTH as *
  IF ztca_param-orgauth IS INITIAL .
    ztca_param-orgauth = lc_orgauth.
  ENDIF.
**--Changed by and last changed on
  ztca_param-changedby    = sy-uname.
  ztca_param-lastchanged  = sy-datum.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BEFORE_SAVE
*&---------------------------------------------------------------------*
* This form is called Before saving the data and it defaults the
* fields Changed by user and Last changed data.
* Validates the input like 1. Valid to date should not be less than Valid
* Valid from date 2. A parameter with same values cannot be created
* within the alreday created parameter validity date range.
*----------------------------------------------------------------------*
FORM before_save.
*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
  DATA : lw_param     TYPE ztca_param,
         lw_old_param TYPE ztca_param,
         lv_index     TYPE sy-tabix,
         lv_length    TYPE i.
  DATA: lt_param TYPE STANDARD TABLE OF ztca_param.

  FIELD-SYMBOLS : <fs_param> TYPE ztca_param.

  CLEAR: lt_param,lw_param.

  LOOP AT total.
    IF <vim_total_struc> IS ASSIGNED.
      ASSIGN <vim_total_struc> TO <fs_param>.
    ELSE.
      CONTINUE.
    ENDIF.
    IF <action> EQ gc_u.
***--Authorization check for Change  entry
      AUTHORITY-CHECK OBJECT gc_zca_param
        ID gc_busproc FIELD <fs_param>-busproc
        ID gc_busact  FIELD <fs_param>-busact
        ID gc_orgauth FIELD <fs_param>-orgauth
        ID gc_authc   FIELD gc_u.
      IF sy-subrc <> 0.
        vim_abort_saving = gc_x.
        sy-subrc = 4.
        MESSAGE s018(zca_msg) DISPLAY LIKE gc_e.
        EXIT.
      ENDIF.
**--Valid to date must be greater than or equal to Valid from date
      IF <fs_param>-validto < <fs_param>-validfrom.
        vim_abort_saving = gc_x.
        sy-subrc = 4.
        MESSAGE s036(zca_msg) DISPLAY LIKE gc_e WITH <fs_param>-param.
        EXIT.
      ENDIF.
*----------------------------------------------------------------------*
* Check for Duplicate Values within given Validity range
*----------------------------------------------------------------------*
*--Get the Old record from database
      SELECT SINGLE *
        INTO lw_old_param
        FROM ztca_param
        WHERE busproc   = <fs_param>-busproc     AND
              busact    = <fs_param>-busact      AND
              param     = <fs_param>-param       AND
              validfrom = <fs_param>-validfrom   AND
              value     = <fs_param>-value       AND
              valueto   = <fs_param>-valueto.
      IF sy-subrc = 0 AND lw_old_param IS NOT INITIAL.
*--If there is a change in the Valid to date then check for duplicate record
*--within the new date range
        IF lw_old_param-validto <> <fs_param>-validto.
          lt_param = total[].
          SORT lt_param BY busproc busact param validfrom value valueto.
*--Delete the current record to validate the date range for duplicate entry
          READ TABLE lt_param WITH KEY  busproc   = lw_old_param-busproc
                                        busact    = lw_old_param-busact
                                        param     = lw_old_param-param
                                        validfrom = lw_old_param-validfrom
                                        value     = lw_old_param-value
                                        valueto   = lw_old_param-valueto
                                        TRANSPORTING NO FIELDS BINARY SEARCH.
          IF sy-subrc = 0.
            DELETE lt_param INDEX sy-tabix.
*--Duplicate value check for a Parameter value within the validity range
            IF <fs_param>-mapflag IS INITIAL.
              LOOP AT lt_param INTO lw_param WHERE  busproc   = <fs_param>-busproc     AND
                                                    busact    = <fs_param>-busact      AND
                                                    param     = <fs_param>-param       AND
                                                    value     = <fs_param>-value       AND
                                                    valueto   = <fs_param>-valueto     AND
                                                    mapflag   = space.
                IF lw_param-validfrom <= <fs_param>-validto AND
                   lw_param-validto   >= <fs_param>-validfrom.
                  vim_abort_saving = gc_x.
                  sy-subrc = 4.
                  MESSAGE s037(zca_msg) WITH <fs_param>-param DISPLAY LIKE gc_e.
                  EXIT.
                ENDIF.
                CLEAR: lw_param.
              ENDLOOP.
*--Duplicate value check for a Mapping value within the validity range
            ELSE.
              CLEAR: lw_param.
              LOOP AT lt_param INTO lw_param WHERE  busproc   = <fs_param>-busproc     AND
                                                    busact    = <fs_param>-busact      AND
                                                    param     = <fs_param>-param       AND
                                                    value     = <fs_param>-value       AND
                                                    mapflag   = abap_true.
                IF lw_param-validfrom <= <fs_param>-validto AND
                   lw_param-validto   >= <fs_param>-validfrom.
                  vim_abort_saving = gc_x.
                  sy-subrc = 4.
                  MESSAGE s038(zca_msg) WITH <fs_param>-param DISPLAY LIKE gc_e.
                  EXIT.
                ENDIF.
                CLEAR: lw_param.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*----------------------------------------------------------------------*
* Fill the Changed by and Last changed on for modified records
*----------------------------------------------------------------------*
**--Populate the last changed on and changed by
      <fs_param>-changedby    = sy-uname.
      <fs_param>-lastchanged  = sy-datum.
**--Modify Total and Extract
      MODIFY total.
      extract = total.
      MODIFY extract INDEX sy-tabix.
    ELSEIF <action> = gc_n.
***--Authorization check for Create new entry
      AUTHORITY-CHECK OBJECT gc_zca_param
        ID gc_busproc FIELD <fs_param>-busproc
        ID gc_busact  FIELD <fs_param>-busact
        ID gc_orgauth FIELD <fs_param>-orgauth
        ID gc_authc   FIELD gc_c.
      IF sy-subrc <> 0.
        vim_abort_saving = gc_x.
        sy-subrc = 4.
        MESSAGE s017(zca_msg) WITH <fs_param>-busproc <fs_param>-busact <fs_param>-orgauth  DISPLAY LIKE gc_e.
        EXIT.
      ENDIF.
**--Valid to date must be greater than or equal to Valid from date
      IF <fs_param>-validto < <fs_param>-validfrom.
        vim_abort_saving = gc_x.
        sy-subrc = 4.
        MESSAGE s036(zca_msg) DISPLAY LIKE gc_e.
        EXIT.
      ENDIF.
    ENDIF.
    CLEAR lw_param.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_AUTHORIZED_DATA
*&---------------------------------------------------------------------*
* This form is called before displaying the data on maintenance screen
* It restricts the displayed data as per the User's authorization for
* Business process and Business activity.
* It displays only valid parameter till the current date
*----------------------------------------------------------------------*
FORM display_authorized_data.
*----------------------------------------------------------------------*
* Declaration for Structures
*----------------------------------------------------------------------*
  TYPES: BEGIN OF ty_total.
      INCLUDE STRUCTURE ztca_param.
      INCLUDE STRUCTURE vimflagtab.
  TYPES: END OF ty_total.
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
  CONSTANTS : lc_all TYPE c VALUE '*'.
*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
  DATA: lt_total      TYPE TABLE OF ty_total,
        lt_auth_value TYPE TABLE OF usvalues.
*----------------------------------------------------------------------*
* Declaration for Work areas
*----------------------------------------------------------------------*
  DATA: lw_total      TYPE ty_total,
        lw_auth_value TYPE usvalues,
        lw_busproc    TYPE ty_busproc,
        lw_busact     TYPE ty_busact,
        lw_orgauth    TYPE ty_orgauth,
        lw_authc      TYPE ty_authc.
**--Refresh interbal table and clear work area
  CLEAR: lw_total,lw_auth_value,lw_busproc,lw_busact,lw_orgauth,lw_authc.
  REFRESH: lt_total.
**--Call the standard subroutine to get the existing table data
  PERFORM table_get_data.
  lt_total = total[].
**--Check user's authorization for Business process and activity
  CALL FUNCTION 'SUSR_USER_AUTH_FOR_OBJ_GET'
    EXPORTING
      mandant    = sy-mandt
      user_name  = sy-uname
      sel_object = gc_zca_param
    TABLES
      values     = lt_auth_value.
  IF sy-subrc = 0 AND lt_auth_value IS NOT INITIAL.
    SORT lt_auth_value BY field.
    LOOP AT lt_auth_value INTO lw_auth_value.
      CASE lw_auth_value-field.
        WHEN gc_busproc.
          lw_busproc-sign   = gc_i.
          lw_busproc-opti   = gc_eq.
          lw_busproc-low    = lw_auth_value-von.
          lw_busproc-high   = lw_auth_value-bis.
          APPEND lw_busproc TO gt_busproc.
          CLEAR:lw_busproc.
        WHEN gc_busact.
          lw_busact-sign    = gc_i.
          lw_busact-opti    = gc_eq.
          lw_busact-low     = lw_auth_value-von.
          lw_busact-high    = lw_auth_value-bis.
          APPEND lw_busact TO gt_busact.
          CLEAR:lw_busact.
        WHEN gc_orgauth.
          lw_orgauth-sign   = gc_i.
          lw_orgauth-opti   = gc_eq.
          lw_orgauth-low    = lw_auth_value-von.
          lw_orgauth-high   = lw_auth_value-bis.
          APPEND lw_orgauth TO gt_orgauth.
          CLEAR:lw_orgauth.
        WHEN gc_authc.
          lw_authc-sign     = gc_i.
          lw_authc-opti     = gc_eq.
          lw_authc-low      = lw_auth_value-von.
          lw_authc-high     = lw_auth_value-bis.
          APPEND lw_authc TO gt_authc.
          CLEAR:lw_authc.
        WHEN OTHERS.
      ENDCASE.
      CLEAR:lw_auth_value.
    ENDLOOP.
  ELSE.
**-- Not authorized to display
    MESSAGE TEXT-009 TYPE gc_e.
  ENDIF.
**--Filter the internal table based on Business process, Business activity,
* and Orgauth authorization
  IF gt_busproc IS NOT INITIAL.
**--If Authorization object contains * then don't delete
    READ TABLE gt_busproc
      WITH KEY low = lc_all TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DELETE lt_total WHERE busproc NOT IN gt_busproc.
    ENDIF.
  ENDIF.
  IF gt_busact IS NOT INITIAL AND lt_total IS NOT INITIAL.
**--If Authorization object contains * then don't delete
    READ TABLE gt_busact
      WITH KEY low = lc_all TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DELETE lt_total WHERE busact NOT IN gt_busact.
    ENDIF.
  ENDIF.
  IF gt_orgauth IS NOT INITIAL AND lt_total IS NOT INITIAL.
**--If Authorization object contains * then don't delete
    READ TABLE gt_orgauth
      WITH KEY low = lc_all TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      DELETE lt_total WHERE orgauth NOT IN gt_orgauth.
    ENDIF.
  ENDIF.
**--Delete the inactive records.
  IF lt_total IS NOT INITIAL.
    DELETE lt_total WHERE validfrom > sy-datum AND validto < sy-datum.
  ENDIF.
**--Move the filtered data to total
  IF lt_total IS NOT INITIAL.
    total[] = lt_total.
  ELSE.
**--No data availble to display
    FREE : total[].
  ENDIF.
  FREE : lt_total.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  SCREEN_MODIFY_0006  OUTPUT
*&---------------------------------------------------------------------*
*       Disable the input fields for Create/Change/Copy
*----------------------------------------------------------------------*
MODULE screen_modify_pbo_0006 OUTPUT.
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
  CONSTANTS:lc_changedby   TYPE screen-name VALUE 'ZTCA_PARAM-CHANGEDBY',
            lc_lastchanged TYPE screen-name VALUE 'ZTCA_PARAM-LASTCHANGED',
            lc_mapflag     TYPE screen-name VALUE 'ZTCA_PARAM-MAPFLAG',
            lc_descr       TYPE screen-name VALUE 'ZTCA_PARAM-DESCR',
            lc_orgauth     TYPE screen-name VALUE 'ZTCA_PARAM-ORGAUTH',
            lc_busproc     TYPE screen-name VALUE 'ZTCA_PARAM-BUSPROC',
            lc_busact      TYPE screen-name VALUE 'ZTCA_PARAM-BUSACT',
            lc_param       TYPE screen-name VALUE 'ZTCA_PARAM-PARAM'.
*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
  DATA: lv_ok_code_l TYPE sy-ucomm.
************************************************************
*** Loop the screen structure and disable the fields
************************************************************
  LOOP AT SCREEN.
**--Changed By User
    IF screen-name = lc_changedby.
      screen-input = 0.
    ENDIF.
*--Last Changed On
    IF screen-name = lc_lastchanged.
      screen-input = 0.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.

  IF sy-ucomm IS NOT INITIAL.
    lv_ok_code_l = sy-ucomm.
  ENDIF.
  CASE lv_ok_code_l.
****************************************************
** Create new entry
****************************************************
*    WHEN 'NEWL'.


****************************************************
* Update entry Detailed view/Change View
****************************************************
    WHEN gc_deta OR gc_aend OR gc_detm.
      LOOP AT SCREEN.
**--Mapping Indicator
        IF screen-name = lc_mapflag.
          screen-input = 0.
        ENDIF.
**--Organizational authorization
        IF screen-name = lc_orgauth.
          screen-input = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDLOOP.
****************************************************
* Copy entry
****************************************************
    WHEN gc_kope.
      "...If user has update authorization...."
      IF  gv_copy = gc_u.
        LOOP AT SCREEN.
**--Business Process
          IF screen-name = lc_busproc.
            screen-input = 0.
          ENDIF.
**--Business Activity
          IF screen-name = lc_busact.
            screen-input = 0.
          ENDIF.
**--Parameter
          IF screen-name = lc_param.
            screen-input = 0.
          ENDIF.
**--Organizational authorization
          IF screen-name = lc_orgauth.
            screen-input = 0.
          ENDIF.
          MODIFY SCREEN.
        ENDLOOP.
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALIDATE_INPUT_PAI_0006
*&---------------------------------------------------------------------*
*       Validate the input data before saving the entry
*----------------------------------------------------------------------*
MODULE validate_input_pai_0006.
  IF sy-ucomm = gc_save OR
     sy-ucomm = gc_kopf.
**--Authorization check for Create new entry
    AUTHORITY-CHECK OBJECT gc_zca_param
      ID gc_busproc FIELD ztca_param-busproc
      ID gc_busact  FIELD ztca_param-busact
      ID gc_orgauth FIELD ztca_param-orgauth
      ID gc_authc   FIELD gc_c.
    IF sy-subrc <> 0.
      MESSAGE e000(zca_msg) WITH ztca_param-busproc ztca_param-busact.
    ENDIF.
**--Duplicate value check for a parameter within VALIDFROM and VALIDTO
    SELECT COUNT(*)
        FROM ztca_param
        WHERE busproc   = ztca_param-busproc     AND
              busact    = ztca_param-busact     AND
              param     = ztca_param-param       AND
              validfrom <= ztca_param-validfrom  AND
              value     = ztca_param-value       AND
              valueto   = ztca_param-valueto     AND
              validto   >= ztca_param-validto.
    IF sy-dbcnt <> 0.
      MESSAGE TEXT-011 TYPE gc_e.
    ENDIF.
**--Valid to date must be greater than or equal to Valid from date
    IF ztca_param-validto < ztca_param-validfrom.
      MESSAGE TEXT-012 TYPE gc_e.
    ENDIF.
**--If Mapping flag is checked then VALUE and VALUETO should not be blank
    IF ztca_param-mapflag IS NOT INITIAL AND
       ( ztca_param-value IS INITIAL OR ztca_param-valueto IS INITIAL ).
      MESSAGE TEXT-013 TYPE gc_e.
    ENDIF.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_AUTHORIZATION_PAI_0005
*&---------------------------------------------------------------------*
*  Check the create/change/copy/delete authorization before performing
* the respective opeartion and if not authorized give an error message
*----------------------------------------------------------------------*
MODULE check_authorization_pai_0005.
************************************************************
** Authorization check for Create/Change/Delete/Copy
************************************************************
  CASE sy-ucomm.
*****************************************************
** Create new entry
*****************************************************
    WHEN gc_newl.
***--Authorization check for Create new entry
      AUTHORITY-CHECK OBJECT gc_zca_param
            ID gc_authc FIELD gc_c.
      IF sy-subrc <> 0.
        MESSAGE TEXT-005 TYPE gc_s DISPLAY LIKE gc_e.
        SET SCREEN 0005.
        LEAVE SCREEN.
      ENDIF.
*****************************************************
** Update entry
*****************************************************
    WHEN gc_deta OR gc_aend OR gc_detm.
***--Authorization check for Update entry
      AUTHORITY-CHECK OBJECT gc_zca_param
            ID gc_authc FIELD gc_u.
      IF sy-subrc <> 0.
        MESSAGE TEXT-006 TYPE gc_s DISPLAY LIKE gc_e.
        SET SCREEN 0005.
        LEAVE SCREEN.
      ENDIF.
*****************************************************
** Copy entry
*****************************************************
    WHEN gc_kope.
**--For Copy user should have either Create or update authorization
      AUTHORITY-CHECK OBJECT gc_zca_param
            ID gc_authc FIELD gc_c.
      IF sy-subrc <> 0.
        AUTHORITY-CHECK OBJECT gc_zca_param
               ID gc_authc FIELD gc_u.
        IF sy-subrc <> 0 .
          MESSAGE TEXT-007 TYPE gc_s DISPLAY LIKE gc_e.
          SET SCREEN 0005.
          LEAVE SCREEN.
        ELSE.
          gv_copy = gc_u. " Update authorization
        ENDIF.
      ELSE.
        gv_copy = gc_c.   " Create authorization
      ENDIF.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_HELP_BUSPROC_0006
*&---------------------------------------------------------------------*
*  Provide F4 help for Business process
*----------------------------------------------------------------------*
MODULE f4_help_busproc_0006.
*----------------------------------------------------------------------*
* Declaration for Structures
*----------------------------------------------------------------------*
  TYPES: BEGIN OF ty_busproc_f4,
           busproc TYPE ztca_busprocess-busproc,
           descr   TYPE ztca_busprocess-descr,
         END OF ty_busproc_f4.
*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
  DATA : lt_return_bp TYPE TABLE OF ddshretval,
         lt_busproc   TYPE TABLE OF ty_busproc_f4.
*----------------------------------------------------------------------*
* Declaration for Work areas
*----------------------------------------------------------------------*
  DATA : lw_return_bp TYPE ddshretval.
  CONSTANTS : lc_buspro TYPE dfies-fieldname VALUE 'BUSPROC'.

** Refresh interbal tables and clear work area
  REFRESH: lt_return_bp,lt_busproc.
  CLEAR:lw_return_bp.
*********************************************************
** Get Business process and description
*********************************************************
  SELECT busproc
         descr
    INTO TABLE lt_busproc
    FROM ztca_busprocess
    FOR ALL ENTRIES IN gt_busproc
    WHERE busproc = gt_busproc-low.
*********************************************************
** Call FM for F4 help
*********************************************************
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = lc_buspro
      value_org  = gc_value_org
    TABLES
      value_tab  = lt_busproc
      return_tab = lt_return_bp.
  IF sy-subrc = 0.
    READ TABLE lt_return_bp INTO lw_return_bp INDEX 1.
    MOVE lw_return_bp-fieldval TO ztca_param-busproc.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  F4_HELP_BUSACT_0006
*&---------------------------------------------------------------------*
*  Provide F4 help for Business Activity
*----------------------------------------------------------------------*
MODULE f4_help_busact_0006.
*----------------------------------------------------------------------*
* Declaration for Structures
*----------------------------------------------------------------------*
  TYPES: BEGIN OF ty_busact_f4,
           busact TYPE ztca_busactivity-busact,
           descr  TYPE ztca_busactivity-descr,
         END OF ty_busact_f4.
*----------------------------------------------------------------------*
* Declaration for Internal tables
*----------------------------------------------------------------------*
  DATA : lt_return_ba TYPE TABLE OF ddshretval,
         lt_busact    TYPE TABLE OF ty_busact_f4.
*----------------------------------------------------------------------*
* Declaration for Work areas
*----------------------------------------------------------------------*
  DATA : lw_return_ba TYPE ddshretval.
  CONSTANTS : lc_busac    TYPE dfies-fieldname VALUE 'BUSACT'.
**--Refresh interbal table and clear work area
  REFRESH: lt_return_ba,lt_busact.
  CLEAR:lw_return_ba.
*********************************************************
** Get Business activity and description
*********************************************************
  SELECT busact
         descr
    INTO TABLE lt_busact
    FROM ztca_busactivity
    FOR ALL ENTRIES IN gt_busact
    WHERE busact = gt_busact-low.
*********************************************************
** Call FM for F4 help
*********************************************************
  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = lc_busac
      value_org  = gc_value_org
    TABLES
      value_tab  = lt_busact
      return_tab = lt_return_ba.
  IF sy-subrc = 0.
    READ TABLE lt_return_ba INTO lw_return_ba INDEX 1.
    MOVE lw_return_ba-fieldval TO ztca_param-busact.
  ENDIF.
ENDMODULE.
