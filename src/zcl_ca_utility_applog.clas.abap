class ZCL_CA_UTILITY_APPLOG definition
  public
  inheriting from ZCL_CA_UTILITY
  final
  create public .

public section.

  data C_MSGCLASS type SYST_MSGID value 'ZCA_MSG' ##NO_TEXT.

  methods CREATE_LOG
    importing
      !IV_OBJECT type CHAR3
      !IV_SUBOBJ type Z_BUSACT
      !IV_EXTERNAL type STRING
      !IT_LOGS type BAPIRET2_T
    exporting
      !ET_RETURN type BAPIRET2_T .
  methods GET_LOG
    importing
      !I_LOG_FILTER type BAL_S_LFIL
    exporting
      !ET_LOGS type BAPIRET2_T
      !ET_RETURN type BAPIRET2_T .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CA_UTILITY_APPLOG IMPLEMENTATION.


  METHOD create_log.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_APPLOG-CREATE_LOG
* Program Title: Create Application Log
* Created By   : Ganesh Salunke
* Creation Date: 11/15/2016
* RICEFW ID    : E308
* Description  : Method to create application log
*
* Additional Information:
* Method is used to create the application log by passing
* the Busines process , Business Activity and RICEF ID
* and passing the log data into the IT_LOGS table.
* In case, we encounter error we will fill ET_RETURN table
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       E308        D10K900292
*                                      Initial version
* 11/15/2016  x51380       E308        D10K900292
*                                      Initial version
* 11/07/2016  X51429       E308        D10K914386
*                                      Defect #6506
************************************************************************

*----------------------------------------------------------------------*
* Declaration for Variables
*----------------------------------------------------------------------*
    DATA : lv_log_handle TYPE  balloghndl,
           lv_object     LIKE  iv_object,
           lv_subobject  LIKE  iv_subobj.
*----------------------------------------------------------------------*
* Declaration for Work Areas
*----------------------------------------------------------------------*
    DATA : lw_s_log  TYPE  bal_s_log,
           lw_msg    TYPE  bal_s_msg,
           lw_return TYPE  bapiret2.
    DATA:  lw_intftrack TYPE  ztca_intf_track.
    DATA:  lv_balobj TYPE balobj_d.
    DATA:  lv_balsub TYPE balsubobj.

    DATA:  lt_blog TYPE bal_t_logh.
    DATA:  lw_blog LIKE LINE OF lt_blog.
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*

    CONSTANTS:
* problem class
      lc_probclass_very_high TYPE bal_s_msg-probclass VALUE '1',
      lc_probclass_medium    TYPE bal_s_msg-probclass VALUE '3',
      lc_probclass_low       TYPE bal_s_msg-probclass VALUE '4',
* message types
      lc_msgty_x             TYPE sy-msgty            VALUE 'X',
      lc_msgty_a             TYPE sy-msgty            VALUE 'A',
      lc_msgty_e             TYPE sy-msgty            VALUE 'E',
      lc_msgty_w             TYPE sy-msgty            VALUE 'W',
      lc_msgty_i             TYPE sy-msgty            VALUE 'I',
      lc_msgty_s             TYPE sy-msgty            VALUE 'S',
      lc_msgclass            TYPE sy-msgid            VALUE 'ZCA_MSG'.


*----------------------------------------------------------------------*

** Added By X51380.
    SELECT SINGLE *
    FROM ztca_intf_track
    INTO lw_intftrack
    WHERE intid = iv_external.

**  Ignore input if ZTCA_INTF_TRACKER is maintained.
    IF lw_intftrack-object IS NOT INITIAL.
      lv_balobj = lw_intftrack-object.
    ELSE.
      lv_balobj = iv_object.
      DATA(lv_busproc) = iv_object+1(2)." To seperate the 3 characters into two
**  Check Business Process is Valid ?
      SELECT SINGLE *
        FROM ztca_busprocess
        INTO @DATA(lw_busprocess)
        WHERE busproc = @lv_busproc.
      IF sy-subrc <> 0.
        lw_return-type   =  lc_msgty_e.
        lw_return-number =  '001'.
        lw_return-id     =  lc_msgclass.
        lw_return-message_v1 = iv_object.
        MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
                 WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
                 INTO lw_return-message.
        APPEND lw_return TO et_return.
        CLEAR lw_return.
        RETURN.
      ENDIF.
    ENDIF.
** Check Business Process Maintained in SLG0
    SELECT SINGLE   object
                    FROM balobj
                    INTO lv_object
                    WHERE object = lv_balobj.
    IF sy-subrc <> 0.
*--  Invalid Business Process
      lw_return-type   =  lc_msgty_e.
      lw_return-number =  '002'.
      lw_return-id     =  lc_msgclass.
      lw_return-message_v1 = iv_object.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      RETURN.
    ENDIF.
**  Ignore input if ZTCA_INTF_TRACKER is maintained.
    IF lw_intftrack-object IS NOT INITIAL.
      lv_balsub = lw_intftrack-subobject.
    ELSE.
      lv_balsub = iv_subobj.
**  Check Business Activity is Valid ?
      SELECT SINGLE *
        FROM ztca_busactivity
        INTO @DATA(lw_busactivity)
        WHERE busact = @iv_subobj.
      IF sy-subrc <> 0.
        lw_return-type   =  lc_msgty_e.
        lw_return-number =  '003'.
        lw_return-id     =  lc_msgclass.
        lw_return-message_v1 = iv_subobj.
        MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
                 WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
                 INTO lw_return-message.
        APPEND lw_return TO et_return.
        CLEAR lw_return.
        RETURN.
      ENDIF.
    ENDIF.
** Check Business Activity Maintained in SLG0
    SELECT SINGLE subobject
                  FROM balsub
                  INTO lv_subobject
                  WHERE object    = lv_balobj
                  AND   subobject = lv_balsub.
    IF sy-subrc <> 0.
      lw_return-type   =  lc_msgty_e.
      lw_return-number =  '004'.
      lw_return-id     =  lc_msgclass.
      lw_return-message_v1 = iv_subobj.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      RETURN.
    ENDIF.

    FREE: lv_object, lv_subobject.

    lw_s_log-extnumber = iv_external.
*    lw_s_log-object    = iv_object.
    lw_s_log-object    = lv_balobj.
*    lw_s_log-subobject = iv_subobj.
    lw_s_log-subobject = lv_balsub.
    lw_s_log-aldate    = sy-datum.
    lw_s_log-altime    = sy-uzeit.
    lw_s_log-aluser    = sy-uname.
    lw_s_log-altcode   = sy-tcode.
    lw_s_log-alprog    = sy-repid.

*  Creating log handle to consume in the BAL_LOG_MSG_ADD FM
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log                 = lw_s_log
      IMPORTING
        e_log_handle            = lv_log_handle
      EXCEPTIONS
        log_header_inconsistent = 1
        OTHERS                  = 2.

    IF sy-subrc = 1.
      lw_return-type   =  lc_msgty_e.
      lw_return-number =  '005'.
      lw_return-id     =  lc_msgclass.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      RETURN.
    ELSEIF sy-subrc = 2.
      lw_return-type   =  lc_msgty_e.
      lw_return-number =  '006'.
      lw_return-id     =  lc_msgclass.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      RETURN.
    ELSE.
      LOOP AT it_logs ASSIGNING FIELD-SYMBOL(<fs>).
*                      WHERE type IS NOT INITIAL AND
*                              id IS NOT INITIAL AND
*                           number IS NOT INITIAL.
*--     define data of message for Application Log
        lw_msg-msgty     = <fs>-type.
        lw_msg-msgid     = <fs>-id.
        lw_msg-msgno     = <fs>-number.
        lw_msg-msgv1     = <fs>-message_v1.
        lw_msg-msgv2     = <fs>-message_v2.
        lw_msg-msgv3     = <fs>-message_v3.
        lw_msg-msgv4     = <fs>-message_v4.

        CASE <fs>-type.
          WHEN lc_msgty_a OR lc_msgty_e OR lc_msgty_x.
            lw_msg-probclass = lc_probclass_very_high.
          WHEN lc_msgty_w.
            lw_msg-probclass = lc_probclass_medium.
          WHEN lc_msgty_s OR lc_msgty_i.
            lw_msg-probclass = lc_probclass_low.
          WHEN OTHERS.
        ENDCASE.
*--     Add msg to log
        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle     = lv_log_handle
            i_s_msg          = lw_msg
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
        CASE sy-subrc.
          WHEN 1 OR 2 OR 3 OR 4.
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
*            RETURN.
        ENDCASE.
      ENDLOOP.
** Save the log message

      MOVE lv_log_handle TO lw_blog.
      APPEND lw_blog TO lt_blog.
      CALL FUNCTION 'BAL_DB_SAVE'
        EXPORTING
          i_client         = sy-mandt
*          i_in_update_task = 'X'                        " Mod by X51787
          i_t_log_handle   = lt_blog
*         i_save_all       = 'X'   "                     " Mod by X51787
        EXCEPTIONS
          log_not_found    = 1
          save_not_allowed = 2
          numbering_error  = 3
          OTHERS           = 4.
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
    ENDIF.
  ENDMETHOD.


  METHOD get_log.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_APPLOG-GET_LOG
* Program Title: Create Application Log
* Created By   : Ganesh Salunke
* Creation Date: 11/15/2016
* RICEFW ID    : E308
* Description  : Cross-App Application Log Utility class- Get Log
*
* Additional Information:
* The method is used to get the application log data by passing the
* Business process, Business Activity and RICEF ID into the I_LOG_FILTER
* table. Please refer YUTCA_DEMO_APPLOG for demo program.
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/15/2016  x51429       E308        D10K900292
*                                      Initial version
************************************************************************

*----------------------------------------------------------------------*
* Declaration for internal tables and Work areas
*----------------------------------------------------------------------*
    DATA : it_log_header TYPE balhdr_t,
           lw_bapiret2   TYPE bapiret2,
           lw_return     TYPE bapiret2,
           it_msg_handle TYPE bal_t_msgh,
           lw_msg        TYPE bal_s_msg.

*----------------------------------------------------------------------*

*  FM to get the log header by passing the Business process, Business Act
*  and RICEF ID in the I_LOG_FILTER
    CALL FUNCTION 'BAL_DB_SEARCH'
      EXPORTING
        i_client           = sy-mandt
        i_s_log_filter     = i_log_filter
      IMPORTING
        e_t_log_header     = it_log_header
      EXCEPTIONS
        log_not_found      = 1
        no_filter_criteria = 2
        OTHERS             = 3.
    IF sy-subrc <> 0.
*     In case exceptions are raised, the messages are stored into the return table which will hold the errors
      lw_return-type   =  sy-msgty.
      lw_return-number =  sy-msgno.
      lw_return-id     =  sy-msgid.
      lw_return-message_v1 = sy-msgv1.
      lw_return-message_v2 = sy-msgv2.
      lw_return-message_v3 = sy-msgv3.
      lw_return-message_v4 = sy-msgv4.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      EXIT.
    ENDIF.
* Based on the log header fetched from the previous FM, respective msg_handle's are fetched from the below FM
    CALL FUNCTION 'BAL_DB_LOAD'
      EXPORTING
        i_t_log_header     = it_log_header
      IMPORTING
*        e_t_log_handle     = it_log_handle
        e_t_msg_handle     = it_msg_handle
*       E_T_LOCKED         =
      EXCEPTIONS
        no_logs_specified  = 1
        log_not_found      = 2
        log_already_loaded = 3
        OTHERS             = 4.
    IF sy-subrc <> 0.
      lw_return-type       = sy-msgty.
      lw_return-number     = sy-msgno.
      lw_return-id         = sy-msgid.
      lw_return-message_v1 = sy-msgv1.
      lw_return-message_v2 = sy-msgv2.
      lw_return-message_v3 = sy-msgv3.
      lw_return-message_v4 = sy-msgv4.
      MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
               WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
               INTO lw_return-message.
      APPEND lw_return TO et_return.
      CLEAR lw_return.
      EXIT.
    ENDIF.
** Based on the msg_log_handle, the application log is read into a BAPIRETURN structure
    LOOP AT it_msg_handle ASSIGNING FIELD-SYMBOL(<fs_msg>).
      CALL FUNCTION 'BAL_LOG_MSG_READ'
        EXPORTING
          i_s_msg_handle = <fs_msg>
          i_langu        = sy-langu
        IMPORTING
          e_s_msg        = lw_msg
        EXCEPTIONS
          log_not_found  = 1
          msg_not_found  = 2
          OTHERS         = 3.
      IF sy-subrc <> 0.
*        Add records to the ET_RETURN table, in case FM encounters any errors.
        lw_return-type        = sy-msgty.
        lw_return-number      = sy-msgno.
        lw_return-id          = sy-msgid.
        lw_return-message_v1  = sy-msgv1.
        lw_return-message_v2  = sy-msgv2.
        lw_return-message_v3  = sy-msgv3.
        lw_return-message_v4  = sy-msgv4.
        MESSAGE ID lw_return-id TYPE lw_return-type NUMBER lw_return-number
                 WITH lw_return-message_v1 lw_return-message_v2 lw_return-message_v3 lw_return-message_v4
                 INTO lw_return-message.
        APPEND lw_return TO et_return.
        CLEAR lw_return.
        EXIT.
      ENDIF.
*     Add the log data to the ET_LOGS, which can be further displayed in the program that this method is used.
      lw_bapiret2-type       = lw_msg-msgty.
      lw_bapiret2-id         = lw_msg-msgid.
      MESSAGE ID lw_bapiret2-id TYPE lw_bapiret2-type NUMBER  lw_msg-msgno
               WITH lw_msg-msgv1 lw_msg-msgv2 lw_msg-msgv3 lw_msg-msgv4
               INTO lw_bapiret2-message.
      lw_bapiret2-number    = lw_msg-msgno.
      lw_bapiret2-message_v1 = lw_msg-msgv1.
      lw_bapiret2-message_v2 = lw_msg-msgv2.
      lw_bapiret2-message_v3 = lw_msg-msgv3.
      lw_bapiret2-message_v4 = lw_msg-msgv4.
      APPEND lw_bapiret2 TO et_logs.
      CLEAR lw_bapiret2.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
