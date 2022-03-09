*&---------------------------------------------------------------------*
*& Report ZUTCA_DEMO_SPOOL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUTCA_DEMO_SPOOL.
************************************************************************
*
* Additional Information: This is a DEMO prgogram which shows the
*reusability for the spool parameters utility method created.
*The method SET_PRINT_SPOOL will helps to set the values from the
*parameter table ZTCA_PARAM. If there is no values found in the tablle,
*automatically default values will set. The print parameters dialog box will
*be bypassed and the values automaticaaly set for the print functionality
* using the SET_PRINT_SPOOL.
************************************************************************

*----------------------------------------------------------------------*
* Declaration for Local variables
*----------------------------------------------------------------------*
TYPE-POOLS: slis.
DATA : lr_spool TYPE REF TO zcl_ca_utility,
       lr_data  TYPE REF TO zcl_ca_utility.
*----------------------------------------------------------------------*
* Declaration for work area
*----------------------------------------------------------------------*
DATA : gt_param TYPE TABLE OF ztca_param.
DATA : gw_spool TYPE pri_params,
       gw_print TYPE slis_print_alv,
       gt_cat   TYPE slis_t_fieldcat_alv.
*----------------------------------------------------------------------*
*Selection Screen
*----------------------------------------------------------------------*
PARAMETERS : p_buspro TYPE ztca_param-busproc,
             p_busact TYPE ztca_param-busact.

*Setting method for spool parameter
CREATE OBJECT lr_spool.
CALL METHOD lr_spool->set_print_spool
  EXPORTING
    i_busproc = p_buspro
    i_busact  = p_busact.


*Getting data to print (make use of printing )
SELECT * FROM ztca_param INTO TABLE gt_param
  WHERE busproc = p_buspro
    AND busact  = p_busact.

*Spool creation and automatically set the alv to create spool using the
*set_print_spool using above method
IF gt_param IS NOT INITIAL.
  CREATE OBJECT lr_data.
  CALL METHOD lr_data->display_alv
    CHANGING
      c_datatab = gt_param.
ENDIF.
