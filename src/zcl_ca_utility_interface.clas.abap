class ZCL_CA_UTILITY_INTERFACE definition
  public
  inheriting from ZCL_CA_UTILITY
  create public .

public section.

  data GW_INTFTRACK type ZTCA_INTF_TRACK .

  methods FEH_TRIGGER
    importing
      !I_BUSPROC type ECH_STR_OBJECT optional
      !I_INTFID type Z_INTFID
      !I_COMPONENT type ECH_DTE_COMPONENT optional
      !I_PROCESS type ECH_DTE_PROCESS optional
      !I_MESSAGE type BAPIRET2_T optional
      !I_DATA type ANY optional
    changing
      !I_REF_REGISTRATION type ref to CL_FEH_REGISTRATION .
  methods ADD_LOG
    importing
      !I_TYPE type BAPI_MTYPE
      !I_ID type SYMSGID optional
      !I_NUMBER type SYMSGNO optional
      !I_MESSAGE type STRING optional
      !I_MESSAGE_V1 type SYMSGV optional
      !I_MESSAGE_V2 type SYMSGV optional
      !I_MESSAGE_V3 type SYMSGV optional
      !I_MESSAGE_V4 type SYMSGV optional
      !I_DATETIME type ABAP_BOOL optional .
  methods CLEAR .
  methods UPDATE_TRACKER
    importing
      !I_INTFID type Z_INTFID
      !I_STATUS type Z_STATUS
      !I_IMMEDT type C .
  methods GET_LOG
    returning
      value(R_LOG) type BAPIRET2_T .
  methods GET_STATUS
    returning
      value(R_STATUS) type CHAR1 .
  methods IS_PLACEHOLDER
    returning
      value(R_STATUS) type ABAP_BOOL .
  methods GET_RUN_DATETIME
    importing
      !I_INTID type Z_INTFID
    exporting
      !E_SDATE type SY-DATUM
      !E_STIME type SY-UZEIT
      !E_EDATE type SY-DATUM
      !E_ETIME type SY-UZEIT
      !E_CNTRL type ZCNTRL
    exceptions
      NO_DATA .
  methods SET_RUN_DATETIME
    importing
      !I_INTID type Z_INTFID
      !I_EDATE type SY-DATUM default SY-DATUM
      !I_ETIME type SY-UZEIT default SY-UZEIT
      !I_RDATE type SY-DATUM default SY-DATUM
      !I_RTIME type SY-UZEIT default SY-UZEIT
      !I_STATU type Z_STATUS default 002
      !I_IMMED type C default SPACE
      !I_CNTRL type ZCNTRL default 0
    exceptions
      ENTRY_LOCK
      MODIFY_ERROR .
  methods READ_INTF_TRACKER
    importing
      !I_INTID type Z_INTFID
      !I_IGNORELOG type ABAP_BOOL optional
    exceptions
      NO_DATA .
  methods INITIALIZE
    importing
      !I_ID type Z_INTFID .
  methods GET_BUSPROC
    importing
      !I_INTFID type Z_INTFID
    returning
      value(R_BUSPROC) type Z_BUSPROC .
  methods GET_BUSACTV
    importing
      !I_INTFID type Z_INTFID
    returning
      value(R_BUSACTV) type Z_BUSACT .
  methods ADD_LOGS
    importing
      !I_LOG type BAPIRET2_T .
  methods UPDATE_LOG
    importing
      !I_EXTRNL type STRING
      !I_OBJECT type CHAR3 optional
      !I_SUBOBJ type Z_BUSACT optional .
protected section.
private section.

  data GV_EXTR_DATE type Z_LEDATE .
  data GV_EXTR_TIME type Z_LETIME .
  data GV_INTFID type Z_INTFID .
  data GV_BUSPRC type Z_BUSPROC .
  data GV_BUSACT type Z_BUSACT .
  data GT_MSGLOG type BAPIRET2_T .
  data GO_FAULT type ref to CX_AI_SYSTEM_FAULT .
  data GV_STATUS type STRING .

  methods MESSAGE_SPLIT
    importing
      !I_MESSAGE type STRING
      !I_DATETIME type ABAP_BOOL
    exporting
      !E_MESSAGE_V1 type SYMSGV
      !E_MESSAGE_V2 type SYMSGV
      !E_MESSAGE_V3 type SYMSGV
      !E_MESSAGE_V4 type SYMSGV .
ENDCLASS.



CLASS ZCL_CA_UTILITY_INTERFACE IMPLEMENTATION.


  METHOD add_log.


    DATA: lw_log LIKE LINE OF gt_msglog.
    DATA: lv_msg1 TYPE symsgv.

    lw_log-id      = i_id.
    lw_log-type    = i_type.
    lw_log-number  = i_number.

    IF i_id IS INITIAL.
      lw_log-id  = 'ZCA_MSG'.
    ENDIF.

    IF i_number IS INITIAL.
      lw_log-number = '999'.
    ENDIF.

    IF lw_log-number  <> '999'.
      MESSAGE ID lw_log-id TYPE lw_log-type
                       NUMBER lw_log-number
                       WITH i_message_v1
                          i_message_v2
                          i_message_v3
                          i_message_v4
                        INTO lw_log-message.
      IF i_datetime EQ abap_true.
        CONCATENATE '|' sy-datum 'T' sy-uzeit '|' lw_log-message INTO lw_log-message.
      ENDIF.
    ELSE.
      lw_log-message = i_message.
      CALL METHOD me->message_split
        EXPORTING
          i_message    = i_message
          i_datetime   = i_datetime
        IMPORTING
          e_message_v1 = lw_log-message_v1
          e_message_v2 = lw_log-message_v2
          e_message_v3 = lw_log-message_v3
          e_message_v4 = lw_log-message_v4.
    ENDIF.
    APPEND lw_log TO gt_msglog.


  ENDMETHOD.


  METHOD add_logs.

    CHECK i_log IS NOT INITIAL.
    APPEND LINES OF i_log TO gt_msglog.

  ENDMETHOD.


  METHOD clear.

    CLEAR:
      gw_intftrack,
      gv_busact,
      gv_busprc,
      gv_extr_date,
      gv_extr_time,
      gv_intfid,
      gv_status.

    REFRESH: gt_msglog.

    gv_status = 'S'.

  ENDMETHOD.


  METHOD feh_trigger.

    CONSTANTS: lc_error  TYPE c VALUE 'E',
               lc_high   TYPE c LENGTH 4 VALUE 'HIGH',
               lc_errcat TYPE ech_dte_error_category VALUE 'CON'.

    DATA: lo_fault TYPE REF TO cx_ai_system_fault.

    DATA: lt_return TYPE TABLE OF  bapiret2,
          ls_return TYPE           bapiret2,
          lw_return TYPE           bapiret2.

    DATA: lv_objkey TYPE ech_dte_objkey,
          ls_object TYPE ech_str_object.

    DATA: lv_comp TYPE ech_dte_component.
    DATA: lv_proc TYPE ech_dte_process.
    DATA: lv_key  TYPE string.


    TRY.

        CALL METHOD me->read_intf_tracker
          EXPORTING
            i_intid     = i_intfid
            i_ignorelog = 'X'
          EXCEPTIONS
            no_data     = 1
            OTHERS      = 2.

        "--> Object Key
        IF i_busproc-objkey IS INITIAL.
          CONCATENATE i_intfid gw_intftrack-descp INTO ls_object-objkey.
        ELSE.
          MOVE i_busproc-objkey TO ls_object-objkey.
        ENDIF.
        "--> Object Type
        IF gw_intftrack-fehobjc IS INITIAL.
          MOVE i_busproc-objtype TO ls_object-objtype.
        ELSE.
          MOVE gw_intftrack-fehobjc TO ls_object-objtype.
        ENDIF.
        "--> Object Category
        IF i_busproc-objcat IS INITIAL .
          MOVE '1' TO ls_object-objcat.
        ELSE.
          MOVE i_busproc-objcat TO ls_object-objcat.
        ENDIF.
        "--> Software Component
        IF gw_intftrack-fehcomp  IS INITIAL.
          MOVE i_component TO lv_comp.
        ELSE.
          MOVE gw_intftrack-fehcomp TO lv_comp.
        ENDIF.
        "--> Business Process
        IF gw_intftrack-fehproc IS INITIAL.
          MOVE i_process TO lv_proc.
        ELSE.
          MOVE gw_intftrack-fehproc TO lv_proc.
        ENDIF.

        IF i_ref_registration IS NOT BOUND.
          i_ref_registration = cl_feh_registration=>s_initialize( ).
        ENDIF.

        CHECK i_ref_registration IS BOUND.

        LOOP AT i_message INTO lw_return.
          IF lw_return-type = lc_error.
            MOVE-CORRESPONDING lw_return TO ls_return.
            CONCATENATE lw_return-message
                        lw_return-message_v1
                        lw_return-message_v2
                        INTO ls_return-message
                        SEPARATED BY space.

            APPEND ls_return TO lt_return.
          ENDIF.
        ENDLOOP.


        CALL METHOD i_ref_registration->collect
          EXPORTING
            i_single_bo      = i_data
            i_component      = lv_comp    "--SWCV Defined
            i_process        = lv_proc    "--ECH Business process
            i_error_category = lc_errcat
            i_main_message   = ls_return
            i_messages       = lt_return
            i_main_object    = ls_object.


      CATCH cx_ai_system_fault INTO lo_fault.

        CALL METHOD me->add_log
          EXPORTING
            i_type    = 'E'
            i_message = 'FEH Registration failed'.

        CALL METHOD me->add_log
          EXPORTING
            i_type    = 'E'
            i_message = lo_fault->get_text( ).

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD GET_BUSACTV.

    CALL METHOD me->read_intf_tracker
      EXPORTING
        i_intid = i_intfid
      EXCEPTIONS
        no_data = 1
        OTHERS  = 2.

    IF sy-subrc = 2.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'I'
          i_message = 'INTF TRACKER: No Business Activity maintained'.
    ENDIF.

    r_busactv = gw_intftrack-bactv.

  ENDMETHOD.


  METHOD GET_BUSPROC.

    CALL METHOD me->read_intf_tracker
      EXPORTING
        i_intid = i_intfid
      EXCEPTIONS
        no_data = 1
        OTHERS  = 2.

    IF sy-subrc = 2.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'I'
          i_message = 'INTF TRACKER: No Business  Process maintained'.
    ENDIF.

    r_busproc = gw_intftrack-bproc.

  ENDMETHOD.


  METHOD get_log.

    r_log = gt_msglog.
  ENDMETHOD.


  METHOD get_run_datetime.

* ***********************************************************************
*  Class ID       : ZCL_CA_UTILITY_INTERFACE
*  Method Title   : GET_RUN_DATETIME
*  Created By     : Kunal Gursahani
*  Creation Date  : 11/23/2016
*  RICEFW ID      : E309
*  Description    : To Get the Interface Run details
*
*  Additional Information: In this method using function module ENQUEUE_EZTCA_INTF_TRACK
* and DEQUEUE_EZTCA_INTF_TRACK to do LOCK & UNLOCK the table value and update the table.
*
* ***********************************************************************
*  Modification History
* ***********************************************************************
*  Date        User ID        REQ#        Transport# / Description
*  ----------  ------------ ----------  ------------------------
*  11/23/2016  x51384        D10K900236   Initial version
*  11/28/2016  X51380        D10K900236   Updated Version ( Call Function 'ADD_TIME_TO_DATE' & 'CATT_ADD_TO_TIME' updated)
* 05/18/2016  x52158         D10K904410   Updated Vesion   Added the importing parameter E_CNTRL
****************************************************************************************************

    DATA: lv_timea TYPE cesmodti.
    DATA: lv_sdate TYPE sy-datum.
    DATA: lv_stime TYPE sy-uzeit.
    DATA: lv_edate TYPE sy-datum.
    DATA: lv_etime TYPE sy-uzeit.

    DATA: lv_iprkz TYPE dattp.
    DATA: lv_stdaz TYPE thour.
    DATA: lv_atime TYPE mhdhb.
    DATA: lv_text  TYPE string.
*****************************************************
**  Read Exisitng Record -----------------
****************************************************
    CALL METHOD me->read_intf_tracker
      EXPORTING
        i_intid = i_intid
      EXCEPTIONS
        no_data = 1
        OTHERS  = 2.

    IF sy-subrc = 2.
      gv_status = 'E'.
      lv_text = 'INTF TRACKER ERROR: Read Failed'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'E'
          i_message = lv_text.
      RAISE no_data.
    ENDIF.

    CHECK gv_status NE 'E'.

**  Get Time Interval ---------------------
    IF gw_intftrack-timea = space  . "IS INITIAL.
      lv_timea = 1.
    ELSE.
      lv_timea = gw_intftrack-timea.
    ENDIF.

    IF gw_intftrack-edate IS INITIAL AND
       gw_intftrack-statu IS INITIAL.
      gw_intftrack-edate = '20170101'.
    ENDIF.
****************************************************
**  <<Get Start date and time>>
****************************************************
    CALL FUNCTION 'C14B_ADD_TIME'
      EXPORTING
        i_starttime = gw_intftrack-etime
        i_startdate = gw_intftrack-edate
        i_addtime   = lv_timea
      IMPORTING
        e_endtime   = lv_stime
        e_enddate   = lv_sdate.
    IF lv_stime IS INITIAL OR lv_sdate IS INITIAL.
      gv_status = 'E'.
      lv_text = 'INTF TRACKER ERROR: Error Getting Start Date & Time'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'E'
          i_message = lv_text.
      RAISE no_data.
    ENDIF.
****************************************************
**  << Get End Date >>
****************************************************
    IF gw_intftrack-pintl IS INITIAL.
      lv_edate = sy-datum.
      lv_etime = sy-uzeit.
    ELSE.

      CASE gw_intftrack-priod.
          "---- Daily/Week/Month/Year
        WHEN '' OR '1' OR '2' OR '3'.

          MOVE gw_intftrack-priod TO lv_iprkz.
          MOVE gw_intftrack-pintl TO lv_atime.

          CALL FUNCTION 'ADD_TIME_TO_DATE'
            EXPORTING
              i_idate               = lv_sdate
              i_time                = lv_atime
              i_iprkz               = lv_iprkz
            IMPORTING
              o_idate               = lv_edate
            EXCEPTIONS
              invalid_period        = 1
              invalid_round_up_rule = 2
              internal_error        = 3
              OTHERS                = 4.
          IF sy-subrc <> 0.
            gv_status = 'E'.
            lv_text = 'INTF TRACKER ERROR: Error Getting End Date & Time'.
            CALL METHOD me->add_log
              EXPORTING
                i_type    = 'E'
                i_message = lv_text.
            RAISE no_data.
          ENDIF.
          lv_etime = gw_intftrack-etime.
          "-- Hourly
        WHEN '4'.

          MOVE gw_intftrack-pintl TO lv_stdaz.

          CALL FUNCTION 'CATT_ADD_TO_TIME'
            EXPORTING
              idate = lv_sdate
              itime = lv_stime
              stdaz = lv_stdaz
            IMPORTING
              edate = lv_edate
              etime = lv_etime.

          IF lv_stime IS INITIAL OR lv_sdate IS INITIAL.
            gv_status = 'E'.
            lv_text = 'INTF TRACKER ERROR: Error Getting End Date & Time'.
            CALL METHOD me->add_log
              EXPORTING
                i_type    = 'E'
                i_message = lv_text.
            RAISE no_data.
          ENDIF.

        WHEN OTHERS.
          lv_edate = sy-datum.
          lv_etime = sy-uzeit.
      ENDCASE.
    ENDIF.
****************************************************
** Return Start and End Date time
****************************************************

    IF lv_sdate GT sy-datum.
      e_sdate = sy-datum.
    ELSE.
      e_sdate = lv_sdate.
    ENDIF.

    IF lv_sdate EQ sy-datum  AND
       lv_stime GT sy-uzeit.
      e_stime = sy-uzeit.
    ELSE.
      e_stime = lv_stime.
    ENDIF.

    IF lv_edate GT sy-datum.
      gv_extr_date = e_edate = sy-datum.
    ELSE.
      gv_extr_date = e_edate = lv_edate.
    ENDIF.

    IF lv_edate EQ sy-datum  AND
       lv_etime GT sy-uzeit.
      gv_extr_time = e_etime = sy-uzeit.
    ELSE.
      gv_extr_time = e_etime = lv_etime.
    ENDIF.

    " **** Return the control number****
    e_cntrl = gw_intftrack-cntrl.
  ENDMETHOD.


  METHOD get_status.

    r_status = gv_status.
    IF me->is_placeholder( ) AND gv_status EQ 'E'.
      r_status = 'I'.
    ENDIF.
  ENDMETHOD.


  METHOD initialize.

    DATA: lv_text TYPE string.

    me->clear( ).
    CLEAR gw_intftrack.

    gv_intfid = i_id.

    CALL METHOD me->read_intf_tracker
      EXPORTING
        i_intid     = gv_intfid
        i_ignorelog = abap_true
      EXCEPTIONS
        no_data     = 1
        OTHERS      = 2.

    IF gw_intftrack-descp IS INITIAL.
      CONCATENATE   gv_intfid'|Processing Log ********'
          INTO lv_text SEPARATED BY space.
    ELSE.
      CONCATENATE   gv_intfid '|' gw_intftrack-descp
     INTO lv_text SEPARATED BY space.
    ENDIF.

    CALL METHOD me->add_log
      EXPORTING
        i_type    = 'S'
        i_message = lv_text.

    CALL METHOD me->read_intf_tracker
      EXPORTING
        i_intid = gv_intfid
      EXCEPTIONS
        no_data = 1
        OTHERS  = 2.

    IF sy-subrc = 0.
      lv_text = 'Entry Found in INTF Tracker'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'S'
          i_message = lv_text.
    ELSEIF sy-subrc = 2.
      gv_status = 'E'.
      lv_text = 'Read Failed - System Error'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'E'
          i_message = lv_text.
      RETURN.
    ENDIF.


  ENDMETHOD.


  METHOD is_placeholder.

    CALL METHOD me->read_intf_tracker
      EXPORTING
        i_intid = gv_intfid
        i_ignorelog = abap_true
      EXCEPTIONS
        no_data = 1
        OTHERS  = 2.

    IF gw_intftrack-descp = 'DUMMY'.
      r_status = abap_true.
    ELSE.
      r_statuS = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD message_split.

    DATA: lv_rest TYPE c LENGTH 250.

    CASE i_datetime.
      WHEN abap_true.

**      Message V1
        CONCATENATE '|' sy-datum 'T' sy-uzeit '|' INTO e_message_v1.
        lv_rest = i_message.
      WHEN abap_false.
        move i_message to lv_rest.
        CALL FUNCTION 'TEXT_SPLIT'
          EXPORTING
            length = 50
            text   = lv_rest
          IMPORTING
            line   = e_message_v1
            rest   = lv_rest.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

**  Message V2
    IF lv_rest IS NOT INITIAL.
      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = 50
          text   = lv_rest
        IMPORTING
          line   = e_message_v2
          rest   = lv_rest.
    ENDIF.
**  Message V3
    IF lv_rest IS NOT INITIAL.
      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = 50
          text   = lv_rest
        IMPORTING
          line   = e_message_v3
          rest   = lv_rest.
    ENDIF.

**  Message V4
    IF lv_rest IS NOT INITIAL.
      CALL FUNCTION 'TEXT_SPLIT'
        EXPORTING
          length = 50
          text   = lv_rest
        IMPORTING
          line   = e_message_v4
          rest   = lv_rest.
    ENDIF.

  ENDMETHOD.


  METHOD read_intf_tracker.

************************************************************************
*  Class ID    : ZCL_CA_UTILITY_INTERFACE
* MEthod Title : READ_INTF_TRACKER
* Created By    : Kunal Gursahani
* Creation Date: 11/29/2016
* RICEFW ID    : E309
* Description  :
*
* Additional Information: This method reads the Info type ID's from the table ZTCA_INTF_TRACK.


*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/29/2016  x51384      D10K900236    Initial version
***********************************************************************

*--------------------------------------------------------------------*
** CALL METHOD -----**
*-----------------------------------------------------------------*


    DATA: lv_text TYPE string.

    CHECK i_intid is NOT INITIAL.

    CHECK gw_intftrack IS INITIAL OR
          gw_intftrack-intid <> i_intid.

    IF i_ignorelog <> abap_true.
      CALL METHOD me->add_log
        EXPORTING
          i_type     = 'S'
          i_message  = 'INTF TRACKER : READ'
          i_datetime = abap_true.

    ENDIF.
    SELECT SINGLE *
     FROM ztca_intf_track
     INTO gw_intftrack
     WHERE intid = i_intid.

    CHECK sy-subrc <> 0 .

    IF i_ignorelog <> abap_true.
      gv_status = 'E'.
      lv_text = 'Please maintain configuration in ZTCA_INTF_TRACK'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'E'
          i_message = lv_text.

      RAISE no_data.
    ENDIF.
  ENDMETHOD.


  METHOD set_run_datetime.

************************************************************************
* Class ID       : ZCL_CA_UTILITY_INTERFACE
* Method Title   : SET_RECORD
* Created By    : Kunal Gursahani
* Creation Date: 11/25/2016
* RICEFW ID    : E309
* Description  : To Set the Interface Run details
*
* Additional Information: This method read the existing records of the table ZTCA_INTF_TRACK,
*which consists of start date, end date, start time and end time.
*
************************************************************************
* Modification History
*****************************************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/17/2016  x51384      D10K900236       Initial version
* 05/18/2016  x52158      D10K904410       Updated Vesion   Added the importing parameter I_CNTRL
****************************************************************************************************


    DATA: lv_subrc TYPE sy-subrc.
    DATA: lv_text TYPE string.

    CONSTANTS : lc_e TYPE c VALUE 'E',
                lc_x TYPE c VALUE 'X',
                lc_d TYPE c VALUE 'D'.

    CALL METHOD me->read_intf_tracker
      EXPORTING
        i_intid = i_intid
      EXCEPTIONS
        no_data = 1
        OTHERS  = 2.

    IF sy-subrc = 2.
      gv_status = 'E'.
      lv_text = 'INTF TRACKER ERROR - No Record Exsist'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'I'
          i_message = lv_text.
    ENDIF.

    IF i_edate LT gw_intftrack-edate.

      lv_text = 'INTF TRACKER Update: Ignore as end Date/Time in Table is in future'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'S'
          i_message = lv_text.
      RETURN.
    ENDIF.

    IF i_edate EQ gw_intftrack-edate AND
       i_etime LT gw_intftrack-etime.

      lv_text = 'INTF TRACKER Update: Ignore as end Date/Time in Table is in future'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'S'
          i_message = lv_text.
      RETURN.
    ENDIF.

    gw_intftrack-intid = i_intid.
    gw_intftrack-rdate = i_rdate.
    gw_intftrack-rtime = i_rtime.
    gw_intftrack-aenam = sy-uname.
    gw_intftrack-statu = i_statu.
    gw_intftrack-batch = sy-batch.
    gw_intftrack-cntrl = i_cntrl.

    IF i_statu = '002'.

      IF i_edate GT sy-datum.
        gw_intftrack-edate = sy-datum.
      ELSE.
        gw_intftrack-edate = i_edate.
      ENDIF.

      IF i_edate EQ sy-datum  AND
         i_etime GT sy-uzeit.
        gw_intftrack-etime = sy-uzeit.
      ELSE.
        gw_intftrack-etime = i_etime.
      ENDIF.

    ENDIF.
****************************************************
** Lock Entry
****************************************************
    DO 3 TIMES.

      CALL FUNCTION 'ENQUEUE_EZTCA_INTF_TRACK'
        EXPORTING
          mode_ztca_intf_track = lc_e
          mandt                = sy-mandt
          intid                = i_intid
        EXCEPTIONS
          foreign_lock         = 1
          system_failure       = 2
          OTHERS               = 3.

      IF sy-subrc = 0.
        lv_subrc = 0.
        EXIT.
      ELSE.
        WAIT UP TO 2 SECONDS.
      ENDIF.

    ENDDO.

    IF lv_subrc <> 0.
      gv_status = 'E'.
      lv_text = 'INTF TRACKER ERROR - Record is Locked'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'E'
          i_message = lv_text.
      RAISE entry_lock.
    ENDIF.
****************************************************
** Update Table
****************************************************
    MODIFY ztca_intf_track FROM gw_intftrack.
    IF sy-subrc = 0 AND i_immed = lc_x.
      COMMIT WORK.
    ELSE.
      gv_status = 'E'.
      lv_text = 'INTF TRACKER ERROR - Update Failed'.
      CALL METHOD me->add_log
        EXPORTING
          i_type    = 'E'
          i_message = lv_text.
      RAISE modify_error.
    ENDIF.
****************************************************
** Unlock Entry
****************************************************
    CALL FUNCTION 'DEQUEUE_EZTCA_INTF_TRACK'
      EXPORTING
        mode_ztca_intf_track = lc_d
        mandt                = sy-mandt
        intid                = i_intid.

  ENDMETHOD.


  METHOD update_log.


    DATA: lo_applog  TYPE REF TO zcl_ca_utility_applog.
    DATA:lt_bapiret TYPE bapiret2_t.


    TRY .
        CREATE OBJECT lo_applog.
      CALL METHOD lo_applog->create_log
        EXPORTING
          iv_object   = i_object
          iv_subobj   = i_subobj
          iv_external = i_extrnl
          it_logs     = gt_msglog
        IMPORTING
          et_return   = lt_bapiret.

      CATCH cx_ai_system_fault.

    ENDTRY.

  ENDMETHOD.


  METHOD update_tracker.

    DATA: lo_fault TYPE REF TO cx_ai_system_fault.
    DATA: lv_text TYPE string.
    DATA: lv_status TYPE z_status.

    TRY.

        CALL METHOD me->add_log
          EXPORTING
            i_type    = 'I'
            i_message = 'Update Interface Run Tracker'.

        CALL METHOD me->set_run_datetime
          EXPORTING
            i_intid      = i_intfid
            i_edate      = sy-datum
            i_etime      = sy-uzeit
            i_rdate      = sy-datum
            i_rtime      = sy-uzeit
            i_statu      = i_status  " 002 =  Sucess, 003 = Error
*           i_cntrl      = gv_control_no
            i_immed      = i_immedt
          EXCEPTIONS
            entry_lock   = 1
            modify_error = 2
            OTHERS       = 3.
        IF sy-subrc EQ 3.
          lv_text = 'UPDATE Failed due to system error'.
        ENDIF.
      CATCH cx_ai_system_fault INTO lo_fault .
        lv_text = lo_fault->get_text( ).
    ENDTRY.

    CHECK lv_text IS NOT INITIAL.

    CALL METHOD me->add_log
      EXPORTING
        i_type    = 'W'
        i_message = lv_text.

  ENDMETHOD.
ENDCLASS.
