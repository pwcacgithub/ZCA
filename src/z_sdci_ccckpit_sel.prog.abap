*&---------------------------------------------------------------------*
*& Include          Z_SDCI_CCCKPIT_SEL
*&---------------------------------------------------------------------*

TABLES sscrfields.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-002.
* selection screen file input

PARAMETERS : r_user RADIOBUTTON GROUP rad1 MODIF ID m1 DEFAULT 'X' USER-COMMAND flg,
             r_role RADIOBUTTON GROUP rad1 MODIF ID m2.

SELECTION-SCREEN SKIP 1.
PARAMETERS: p_file TYPE string.   "LIKE rlgrap-filename .

PARAMETERS: p_create RADIOBUTTON GROUP rad2 DEFAULT 'X' USER-COMMAND flg.
SELECTION-SCREEN PUSHBUTTON /10(50) button3 USER-COMMAND but3 .
PARAMETERS: p_file1 TYPE string.   "LIKE rlgrap-filename .

PARAMETERS: p_delete RADIOBUTTON GROUP rad2.
SELECTION-SCREEN PUSHBUTTON /10(50) button8 USER-COMMAND but8 .
PARAMETERS: p_file2 TYPE string.   "LIKE rlgrap-filename .

PARAMETERS: p_lock   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN PUSHBUTTON /10(50) button4 USER-COMMAND but4 .
PARAMETERS: p_file3 TYPE string.   "LIKE rlgrap-filename .

PARAMETERS: p_unlock RADIOBUTTON GROUP rad2.
SELECTION-SCREEN PUSHBUTTON /10(50) button5 USER-COMMAND but5 .
PARAMETERS: p_file4 TYPE string.   "LIKE rlgrap-filename .

PARAMETERS: p_role   RADIOBUTTON GROUP rad2.
SELECTION-SCREEN PUSHBUTTON /10(50) button6 USER-COMMAND but6 .
PARAMETERS: p_file5 TYPE string.   "LIKE rlgrap-filename .

PARAMETERS: p_pwd    RADIOBUTTON GROUP rad2.
SELECTION-SCREEN PUSHBUTTON /10(50) button7 USER-COMMAND but7 .
PARAMETERS: p_file6 TYPE string. "LIKE rlgrap-filename .

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN PUSHBUTTON /10(79) button2 USER-COMMAND but2 .

SELECTION-SCREEN END OF BLOCK b1.
