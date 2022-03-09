*&---------------------------------------------------------------------*
*& Report ZSDCI_SEC_COCKPIT_MAINT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdci_sec_cockpit_maint.

INCLUDE z_sdci_ccckpit_dec.

INCLUDE z_sdci_ccckpit_sel.

INCLUDE z_sdci_ccckpit_form.

*&---------------------------------------------------------------------*
*&                     INITIALIZATION
*&---------------------------------------------------------------------*
INITIALIZATION.
  CLEAR: p_file, p_file1, p_file2, p_file3,
         p_file4, p_file5, p_file6.

*&---------------------------------------------------------------------*
*&                   AT SELECTION SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  button2 = 'Download Template for Role Update / Delete / Lock / Change '.

  button3 = 'Download Template for Create User'.
  button8 = 'Download Template for Delete User'.
  button4 = 'Download Template for Lock User'.
  button5 = 'Download Template for Unlock User'.
  button6 = 'Download Template for Role Update for User'.
  button7 = 'Download Template for Password Update User'.

*Perform to modify the screen.
  PERFORM f_modify_screen.

AT SELECTION-SCREEN.
  CASE sscrfields.
    WHEN 'BUT1'.
      MESSAGE 'Download the Template for User Changes ' TYPE 'I'.
    WHEN 'BUT2'.
      MESSAGE 'Download the Template for Role Changes ' TYPE 'I'.
  ENDCASE.

  PERFORM download_template.

*----------------------------------------------------------------------*
* At Selection Screen on value Request
*-------------------------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_filename CHANGING p_file.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file1.
  PERFORM f4_filename CHANGING p_file1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file2.
  PERFORM f4_filename CHANGING p_file2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file3.
  PERFORM f4_filename CHANGING p_file3.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file4.
  PERFORM f4_filename CHANGING p_file4.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file5.
  PERFORM f4_filename CHANGING p_file5.

*&---------------------------------------------------------------------*
*&                   START-OF-SELECTION
*&---------------------------------------------------------------------*

IF r_user = 'X'.
  PERFORM check_update_user.
ELSEIF r_role = 'X'.

ENDIF.
