*&---------------------------------------------------------------------*
*& Report ZUTCA_DEMO_APPLOG
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUTCA_DEMO_APPLOG.
* Description  : Cross-App Application Log Utility class demo program
* shows the functionality that we can achieve by using Application log
* utility class and its methods -> application log and display the
* application log by providing the Business process, business activity
* and RICEF ID
*----------------------------------------------------------------------*
* Declaration for Tables
*----------------------------------------------------------------------*

TABLES: ztca_busprocess, ztca_busactivity, balhdr.
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*

CONSTANTS : c_e TYPE c VALUE 'E'.
*----------------------------------------------------------------------*
* Declaration for Internal tables and work areas
*----------------------------------------------------------------------*

DATA: it_bapiret   TYPE bapiret2_t,
      it_logs      TYPE bapiret2_t,
      lt_object    TYPE bal_r_obj,
      lt_subobject TYPE bal_r_sub,
      lt_external  TYPE bal_r_extn,
      lw_bapi      TYPE bapiret2,
      lw_logfilter TYPE bal_s_lfil,
      lw_object    LIKE LINE OF lt_object,
      lw_subobject LIKE LINE OF lt_subobject,
      lw_external  LIKE LINE OF lt_external.
*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
DATA: lv_object    TYPE c Length 3, "ztca_busprocess-busproc,
      lv_subobject TYPE ztca_busactivity-busact,
      lv_external  TYPE string,
      lv_busrproc  TYPE c LENGTH 3.
*----------------------------------------------------------------------*
*Selection Screen
*----------------------------------------------------------------------*
SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001 .
SELECT-OPTIONS : s_obj FOR lv_busrproc  NO INTERVALS NO-EXTENSION OBLIGATORY, " ++GSALUNK 01/02/2017
                 s_sub FOR ztca_busactivity-busact   NO INTERVALS NO-EXTENSION OBLIGATORY,
                 s_ext FOR balhdr-extnumber NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN : BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002 .
PARAMETERS : p_cre RADIOBUTTON GROUP rg1 DEFAULT 'X',
             p_get RADIOBUTTON GROUP rg1.
SELECTION-SCREEN: END OF BLOCK b2.

AT SELECTION-SCREEN ON s_ext.
  IF p_cre EQ abap_true AND s_ext[] IS INITIAL.
    MESSAGE e007(zca_msg)."   Please input External ID / RICEF ID
  ENDIF.
*----------------------------------------------------------------------*
* Initialisation
*----------------------------------------------------------------------*
INITIALIZATION.
  DATA : lr_applog    TYPE REF TO zcl_ca_utility_applog.
* Object Instantiation
  CREATE OBJECT lr_applog.

*----------------------------------------------------------------------*
* Start of Selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CASE abap_true.
    WHEN p_cre.
      PERFORM fill_structures_log.
      PERFORM create_log.
    WHEN p_get.
      PERFORM fill_log_filter.
      PERFORM fetch_log_details.
      IF it_logs[] IS NOT INITIAL.
        PERFORM display_alv.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

*----------------------------------------------------------------------*
* Subroutines
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  FILL_STRUCTURES_LOG
*&---------------------------------------------------------------------*
*       Add details to the Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_structures_log .
*     Input parameters for the method
  lv_object    = s_obj-low.
  lv_subobject = s_sub-low.
  lv_external  = s_ext-low.

* Message Table for the  method
* Add the message data that needs to be transferred to applicaiton log here
* In case of multiple messages append to the it_logs table.
  lw_bapi-type         = 'E'.
  lw_bapi-id           = 'V0'.
  lw_bapi-number       = '000'.
  lw_bapi-message_v1   = 'MSG1'.
  lw_bapi-message_v2   = 'MSG2'.
  lw_bapi-message_v3   = 'MSG3'.
  lw_bapi-message_v4   = 'MSG4'.

**   Below section to be filled if necessary
*     lw_bapi-PARAMETER  =
*     lw_bapi-ROW        =
*     lw_bapi-FIELD      =
*     lw_bapi-SYSTEM     =

  MESSAGE ID lw_bapi-id TYPE lw_bapi-type NUMBER  lw_bapi-number
           WITH lw_bapi-message_v1 lw_bapi-message_v2 lw_bapi-message_v3 lw_bapi-message_v4
           INTO lw_bapi-message.

  APPEND lw_bapi TO it_logs.
  CLEAR lw_bapi.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  CREATE_LOG
*&---------------------------------------------------------------------*
*       To Create application Log
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_log .

  CALL METHOD lr_applog->create_log
    EXPORTING
      iv_object   = lv_object
      iv_subobj   = lv_subobject
      iv_external = lv_external
      it_logs     = it_logs
    IMPORTING
      et_return   = it_bapiret.

  READ TABLE it_bapiret TRANSPORTING NO FIELDS WITH KEY type = c_e.
  IF sy-subrc EQ 0.
*     Display the table in the ALV list by passing the table to the method.
    CALL METHOD lr_applog->display_alv
      CHANGING
        c_datatab = it_bapiret.
  ELSE.
    MESSAGE s008(zca_msg)."   Application Log successfully created
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FILL_LOG_FILTER
*&---------------------------------------------------------------------*
*       To Extract application log details, fill the log filter as shown
*       below
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_log_filter .
*     Passing the selection screen details to the log filter.
  lw_object-sign   =  lw_subobject-sign  = lw_external-sign   = 'I'.
  lw_object-option = lw_subobject-option = lw_external-option = 'EQ'.
  lw_object-low    = s_obj-low.
  lw_subobject-low = s_sub-low.
  APPEND lw_object    TO lt_object.
  APPEND lw_subobject TO lt_subobject.

  IF s_ext-low IS NOT INITIAL.
    lw_external-low  = s_ext-low.
    APPEND lw_external  TO lt_external.
    lw_logfilter-extnumber = lt_external .
  ELSE.
    CLEAR lw_external.
  ENDIF.
*      Add to the work Log filter structure.
  lw_logfilter-object    = lt_object.
  lw_logfilter-subobject = lt_subobject.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FETCH_LOG_DETAILS
*&---------------------------------------------------------------------*
*      Feth the application log details into a bapireturn table(it_logs)
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fetch_log_details .
  DATA : it_return  TYPE bapiret2_t.
*     Getting the  Application Log details through the method
  CALL METHOD lr_applog->get_log
    EXPORTING
      i_log_filter = lw_logfilter
    IMPORTING
      et_logs      = it_logs
      et_return    = it_return.

  READ TABLE it_return TRANSPORTING NO FIELDS WITH KEY type = c_e.
  IF sy-subrc = 0.
* Error Display ALV
    CALL METHOD lr_applog->display_alv
      CHANGING
        c_datatab = it_return.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_ALV
*&---------------------------------------------------------------------*
*       Display ALV
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM display_alv .
*     Display the table in the ALV list by passing the table to the method.
  CALL METHOD lr_applog->display_alv
    CHANGING
      c_datatab = it_logs.
ENDFORM.
