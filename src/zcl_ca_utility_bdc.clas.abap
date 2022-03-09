class ZCL_CA_UTILITY_BDC definition
  public
  create public .

public section.

  data GT_BDCDATA type TAB_BDCDATA .
  data GW_BDCDATA type BDCDATA .

  methods BDC_SCREEN
    importing
      !I_PROGRAM type BDC_PROG
      !I_DYNPRO type BDC_DYNR .
  methods BDC_FIELDVALUE
    importing
      !I_FNAM type FNAM_____4
      !I_FVAL type BDCDATA-FVAL .
  methods CLEAR_TABLE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CA_UTILITY_BDC IMPLEMENTATION.


  METHOD bdc_fieldvalue.

    gw_bdcdata-fnam = i_fnam. "field name
    gw_bdcdata-fval = i_fval. "field value
    append gw_bdcdata to gt_bdcdata.
    CLEAR gw_bdcdata.

  ENDMETHOD.


  METHOD bdc_screen.

    gw_bdcdata-program  = i_program.
    gw_bdcdata-dynpro   = i_dynpro.
    gw_bdcdata-dynbegin = abap_true.

*** Append bdc table and clear work area
    APPEND gw_bdcdata TO gt_bdcdata.
    CLEAR gw_bdcdata.


  ENDMETHOD.


  method CLEAR_TABLE.
* To clear the bdc table.
    CLEAR:GT_BDCDATA.

  endmethod.
ENDCLASS.
