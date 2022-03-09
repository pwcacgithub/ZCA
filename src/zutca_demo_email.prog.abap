*&---------------------------------------------------------------------*
*& Report ZUTCA_DEMO_EMAIL
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUTCA_DEMO_EMAIL.
*--------------------------------------------------------*
*--- Declaration of structure
*--------------------------------------------------------*
TYPES: BEGIN OF ty_mara,
         matnr TYPE mara-matnr,
         mtart TYPE mara-mtart,
         matkl TYPE mara-matkl,
         meins TYPE mara-meins,
       END OF ty_mara.

*--------------------------------------------------------*
* Declaration of internal table
*---------------------------------------------------------*
DATA : lt_mara        TYPE TABLE OF ty_mara,
       lt_attachment  TYPE solix_tab,
       lt_attach_attr TYPE TABLE OF zsca_packlist.
*--------------------------------------------------------*
* Declaration of workarea
*--------------------------------------------------------*

DATA : lw_mara        TYPE ty_mara,
       lw_attach_attr TYPE zsca_packlist,
       lv_size        TYPE so_obj_len,
       lv_string      TYPE string,
       lv_data_string TYPE string,
       lv_total_lines TYPE sy-tabix,
       lv_ob_dec      TYPE string.


*----------------------------------------------------------------------*
* Declaration for constants
*---------------------------------------------------------------------*
CONSTANTS : lc_b    TYPE c VALUE 'B',
            lc_name TYPE char6 VALUE  '&Nam1&',
            lc_date TYPE char6 VALUE '&Date&',
            lc_x    TYPE c VALUE  'X',
            lc_o    TYPE abap_encod VALUE '4103',
            lc_r    TYPE n VALUE '1',
            lc_csv  TYPE char3 VALUE 'CSV',
            lc_000  TYPE n VALUE 000.

*----------------------------------------------------------------------*
* Selection Screen
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t10.


PARAMETERS: p_sub  TYPE thead-tdname,
            p_body TYPE thead-tdname.

SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t11.

PARAMETERS : p_recver         TYPE string.

SELECTION-SCREEN END OF BLOCK b2.


DATA obj TYPE REF TO zcl_ca_utility.
DATA : lv_sub          TYPE  so_obj_des,
       lt_body         TYPE  bcsy_text,
       lt_return       TYPE TABLE OF  bapiret2,
       lv_retcode      TYPE i,
       lv_err_str      TYPE string,
       lt_text_replace TYPE zttca_email_textsymbol_replace,
       lw_text_replace TYPE zsca_email_textsymbol_replace.

CREATE OBJECT obj.

*----------------------------------------------------------------------*
* Assign values to replace in email text
*----------------------------------------------------------------------*
lw_text_replace-key_type = lc_b.
lw_text_replace-name = lc_name.
lw_text_replace-value = sy-uname.
APPEND lw_text_replace TO lt_text_replace.
CLEAR lw_text_replace.

lw_text_replace-key_type = lc_b.
lw_text_replace-name = lc_date.
lw_text_replace-value = sy-uzeit.
APPEND lw_text_replace TO lt_text_replace.
CLEAR lw_text_replace.
*----------------------------------------------------------------------*
* Get the Subject and Body of the email
*----------------------------------------------------------------------*
CALL METHOD obj->get_email_content
  EXPORTING
    i_text_name_sub  = p_sub
    i_text_name_body = p_body
    i_text_replace   = lt_text_replace
  IMPORTING
    e_subject        = lv_sub
    e_body           = lt_body
    et_return        = lt_return.

IF sy-subrc <> 0.
  CALL METHOD obj->display_alv
    CHANGING
      c_datatab = lt_return.

* Implement suitable error handling here
ENDIF.
*----------------------------------------------------------------------*
* Build attachment
*----------------------------------------------------------------------*
SELECT matnr mtart matkl meins
  FROM mara
  UP TO 10 ROWS
  INTO TABLE lt_mara.
IF NOT lt_mara[] IS INITIAL.
  SORT lt_mara BY matnr.
ENDIF.

LOOP AT lt_mara INTO lw_mara.
  CONCATENATE lw_mara-matnr lw_mara-mtart lw_mara-matkl  lw_mara-meins
         INTO lv_string
         SEPARATED BY cl_abap_char_utilities=>horizontal_tab.
  CONCATENATE lv_data_string lv_string cl_abap_char_utilities=>cr_lf
         INTO lv_data_string.
  CLEAR : lw_mara.
ENDLOOP.

TRY.
    cl_bcs_convert=>string_to_solix(
      EXPORTING
        iv_string   = lv_data_string
        iv_codepage = lc_o  "suitable for MS Excel, leave empty
        iv_add_bom  = lc_x     "for other doc types
      IMPORTING
        et_solix  = lt_attachment
        ev_size   = lv_size ).
  CATCH cx_bcs.
    MESSAGE e445(so).
ENDTRY.
*-----------------------------------------------------------------------*
**--Build attachment attribute
*-----------------------------------------------------------------------*

lw_attach_attr-body_start = lc_r.
DESCRIBE TABLE lt_attachment LINES lw_attach_attr-body_num.
lw_attach_attr-doc_type = lc_csv.
lw_attach_attr-obj_name = TEXT-t04.
CONCATENATE TEXT-t05 ':' sy-datum sy-uzeit INTO lv_ob_dec.
lw_attach_attr-obj_descr = lv_ob_dec.
APPEND lw_attach_attr TO lt_attach_attr.
CLEAR lw_attach_attr.
*----------------------------------------------------------------------*
*Call Method to send email
*----------------------------------------------------------------------*

CALL METHOD obj->send_email
  EXPORTING
    i_rec_type             = lc_000
    i_receiver             = p_recver
    i_subject              = lv_sub
    i_body                 = lt_body
    i_attachment_attribute = lt_attach_attr
    i_attachment           = lt_attachment
    i_immediate            = lc_x
  IMPORTING
    e_retcode              = lv_retcode
    e_err_str              = lv_err_str.

IF lv_retcode IS INITIAL.

  WRITE: / TEXT-001.
ENDIF.
