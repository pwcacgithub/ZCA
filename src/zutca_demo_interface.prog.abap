*&---------------------------------------------------------------------*
*& Report ZUTCA_DEMO_INTERFACE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUTCA_DEMO_INTERFACE.
* Additional Information:
* This Program will help in implementing Interface RUn tracker. Interface
* runtracker is maint to be provide the selection data time for delata read.
************************************************************************

DATA: lo_intf   TYPE REF TO zcl_ca_utility_interface.
DATA: gv_intfid  TYPE z_intfid.
DATA: gv_status  TYPE z_status.
DATA: lv_rdate TYPE sy-datum.
DATA: lv_rtime TYPE sy-uzeit.
DATA: lv_edate TYPE sy-datum.
DATA: lv_etime TYPE sy-uzeit.

SELECTION-SCREEN BEGIN OF BLOCK bl1 WITH FRAME TITLE TEXT-002.
PARAMETERS : p_sdate TYPE  sy-datum.
PARAMETERS : p_edate TYPE  sy-datum.
PARAMETERS : p_stime TYPE  sy-uzeit.
PARAMETERS : p_etime TYPE  sy-uzeit.
SELECTION-SCREEN END OF BLOCK bl1.


INITIALIZATION.
*******************************************************
**  Hard Coded for DEMO Purpose
**  Use respectin Interface ID and Processing status
*******************************************************
  gv_intfid = 'IDEMO'.
  gv_status = '002'.
*******************************************************
** Get Date Time selection from Interface Run Tracker
*******************************************************
  CREATE OBJECT lo_intf.

  CALL METHOD lo_intf->get_run_datetime
    EXPORTING
      i_intid = gv_intfid
    IMPORTING
      e_sdate = P_sdate
      e_stime = p_stime
      e_edate = p_edate
      e_etime = p_etime.


  lv_rdate = sy-datum.
  lv_rtime = sy-uzeit.

*------------------------------------------------------------------------*
*Start of selection
*------------------------------------------------------------------------*
START-OF-SELECTION.

**<< Processing Logic >>
WAIT UP TO 5 SECONDS.

END-OF-SELECTION.

**<< Processing Logic  >>
**
*******************************************************
** Update Interface Tracker at the End
*******************************************************
  MOVE p_edate TO lv_edate.
  MOVE p_etime TO lv_etime.

  CALL METHOD lo_intf->set_run_datetime
    EXPORTING
      i_intid = gv_intfid
      i_edate = lv_edate
      i_etime = lv_etime
      i_rdate = lv_rdate
      i_rtime = lv_rtime
      i_statu = gv_status
      i_immed = 'X'.

  IF sy-subrc = 0.
    WRITE:/ 'Set record is successful'.
  ENDIF.
