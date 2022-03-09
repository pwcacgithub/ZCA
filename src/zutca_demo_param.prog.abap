*&---------------------------------------------------------------------*
*& Report ZUTCA_DEMO_PARAM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUTCA_DEMO_PARAM.
*
**----------------------------------------------------------------------*
** Declaration for Local variables
**----------------------------------------------------------------------*
DATA : lr_param TYPE REF TO zcl_ca_utility.
*----------------------------------------------------------------------*
* Declaration for Internal Tables
*----------------------------------------------------------------------*
DATA : gt_param         TYPE zttca_param_value,
       gt_param_range   TYPE zttca_param_range,
       gt_mapping_value TYPE zttca_map_value.
*----------------------------------------------------------------------*
* Declaration for Work areas
*----------------------------------------------------------------------*
DATA : gw_param       LIKE LINE OF  gt_param,
       gw_msg      TYPE bapiret2.
*----------------------------------------------------------------------*
* message types
*----------------------------------------------------------------------*
CONSTANTS:
  c_msgty_x    TYPE sy-msgty            VALUE 'X',
  c_msgty_a    TYPE sy-msgty            VALUE 'A',
  c_msgty_e    TYPE sy-msgty            VALUE 'E',
  c_msgty_w    TYPE sy-msgty            VALUE 'W',
  c_msgty_i    TYPE sy-msgty            VALUE 'I',
  c_msgty_s    TYPE sy-msgty            VALUE 'S',
  c_msgty_none TYPE sy-msgty            VALUE ' ',
  c_msgclass   TYPE sy-msgid            VALUE 'ZCA_MSG'.
**----------------------------------------------------------------------*
**Selection Screen
**----------------------------------------------------------------------*
PARAMETERS : p_buspro TYPE ztca_param-busproc,
             p_busact TYPE ztca_param-busact,

             p_range  RADIOBUTTON GROUP gr1,
             p_map    RADIOBUTTON GROUP gr1.

*
CREATE OBJECT lr_param.
CALL METHOD lr_param->get_parameter
  EXPORTING
    i_busproc           = p_buspro
    i_busact            = p_busact
    i_validdate         = sy-datum
*   i_param             =
  CHANGING
    t_value             = gt_param
  EXCEPTIONS
    invalid_busprocess  = 1
    invalid_busactivity = 2
    OTHERS              = 3.

IF gt_param IS INITIAL.
  " Fill Return Structure with message
  gw_msg-type   =  c_msgty_e.
  gw_msg-number =  '010'.
  gw_msg-id     =  c_msgclass.
  MESSAGE ID gw_msg-id TYPE gw_msg-type NUMBER gw_msg-number.

ELSE.

  LOOP AT gt_param INTO gw_param.
    IF p_range = abap_true.
      "Range parameters (non-mapping) are returned to range table param_range
      APPEND LINES OF gw_param-param_range TO gt_param_range.
    ELSE.
      "Mapped parameter values are returned to table mapping_value
      APPEND LINES OF gw_param-mapping_value TO gt_mapping_value.
    ENDIF.
  ENDLOOP.
ENDIF.

*
IF p_range = abap_true.
  IF gt_param_range IS INITIAL.
    " Fill Return Structure with message
    gw_msg-type   =  c_msgty_e.
    gw_msg-number =  '010'.
    gw_msg-id     =  c_msgclass.
    MESSAGE ID gw_msg-id TYPE gw_msg-type NUMBER gw_msg-number.
  ELSE.
    CALL METHOD lr_param->display_alv
      CHANGING
        c_datatab = gt_param_range.
  ENDIF.
ELSE.

  IF gt_mapping_value IS INITIAL.
    " Fill Return Structure with message
    gw_msg-type   =  c_msgty_e.
    gw_msg-number =  '010'.
    gw_msg-id     =  c_msgclass.
    MESSAGE ID gw_msg-id TYPE gw_msg-type NUMBER gw_msg-number.
  ELSE.
    CALL METHOD lr_param->display_alv
      CHANGING
        c_datatab = gt_mapping_value.
  ENDIF.
ENDIF.
