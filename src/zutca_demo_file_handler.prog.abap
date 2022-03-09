*&---------------------------------------------------------------------*
*& Report ZUTCA_DEMO_FILE_HANDLER
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZUTCA_DEMO_FILE_HANDLER MESSAGE-ID zca_param.

*----------------------------------------------------------------------*
* Declaration for Type group
*----------------------------------------------------------------------*
TYPE-POOLS:slis.
*----------------------------------------------------------------------*
* Declaration for Structures
*----------------------------------------------------------------------*
TYPES : BEGIN OF ty_upload,
          busproc TYPE ztca_param-busproc,
          busact  TYPE ztca_param-busact,
          param   TYPE ztca_param-param,
          value   TYPE ztca_param-value,
          valueto TYPE ztca_param-valueto,
        END OF   ty_upload.
*----------------------------------------------------------------------*
* Declaration for Internal table
*----------------------------------------------------------------------*
DATA: gt_fcat   TYPE slis_t_fieldcat_alv,
      gt_param  TYPE TABLE OF ty_upload,
      gt_upload TYPE TABLE OF ty_upload.
*----------------------------------------------------------------------*
* Declaration for Work area
*----------------------------------------------------------------------*
DATA: gw_upload TYPE ty_upload,
      gw_param  TYPE ty_upload,
      gw_fcat   TYPE slis_fieldcat_alv.
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
DATA : lr_utility_file TYPE REF TO zcl_ca_utility_file,
       gv_s            TYPE   c,
       gv_file         TYPE   string,
       gv_spath        TYPE   string,
       gv_tpath        TYPE   string,
       gv_source       TYPE c,
       gv_result       TYPE c,
       gv_flag         TYPE c,
       gv_exist        TYPE c,
       gv_fpath        TYPE dxfields-longpath,
       gv_apath        TYPE string.
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
CONSTANTS: gc_a         TYPE c      VALUE 'A',
           gc_p         TYPE c      VALUE 'P',
           gc_x         TYPE c      VALUE 'X',
           gc_s         TYPE c      VALUE 'S',
           gc_e         TYPE c      VALUE 'E',
           gc_tab       TYPE c      VALUE 'T',
           gc_comma_sep TYPE c      VALUE ',',
           gc_delim     TYPE char4  VALUE ',|T;',
           gc_fmask     TYPE string VALUE '*',
           gc_fpath     TYPE char7  VALUE 'LP_E307',
           gc_src       TYPE char3  VALUE 'SRC',
           gc_tgt       TYPE char3  VALUE 'TGT',
           gc_del       TYPE char3  VALUE 'DEL',
           gc_pre       TYPE char3  VALUE 'PRE',
           gc_tim       TYPE char3  VALUE 'TIM',
           gc_dir       TYPE string VALUE 'E:\'.
*----------------------------------------------------------------------*
* Selection screen design
*----------------------------------------------------------------------*
**********************************************
**  File Location
**********************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-000.
".......Application server........."
PARAMETERS:p_rba TYPE char1 DEFAULT 'X' RADIOBUTTON GROUP rb2 USER-COMMAND uc2,
           ".......Presentation server........."
           p_rbp TYPE char1 MODIF ID pre RADIOBUTTON GROUP rb2.
SELECTION-SCREEN END OF BLOCK b1.
**********************************************
**  File Operations
**********************************************
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-001.
".........Read..........."
PARAMETERS: p_read  TYPE char1 DEFAULT 'X' RADIOBUTTON GROUP rb1 USER-COMMAND uc1,
            ".......Write........."
            p_write TYPE char1 RADIOBUTTON GROUP rb1,
            ".......Get Directory Files........."
            p_rdir  TYPE char1  RADIOBUTTON GROUP rb1,
            ".......Copy........."
            p_rcopy TYPE char1  RADIOBUTTON GROUP rb1,
            ".......Delete........."
            p_rdel  TYPE char1  RADIOBUTTON GROUP rb1,
            ".......Archive........."
            p_rarch TYPE char1  RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b2.
**********************************************
**  File paths, Delimiter and Time stamp
**********************************************
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME.
".......Source file path........."
PARAMETERS : p_spath  TYPE string MODIF ID src LOWER CASE DEFAULT gc_fpath,
             ".......Target File path........."
             p_tpath  TYPE string MODIF ID tgt LOWER CASE,
             ".......Delimiter........."
             p_delim  TYPE c MODIF ID del,
             ".......Time stamp........."
             p_tstamp TYPE c MODIF ID tim AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.
**--Show/Hide Selection screen field
  PERFORM set_fields.
*----------------------------------------------------------------------*
* AT SELECTION-SCREEN ON VALUE-REQUEST
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_spath.
  "....F4 help for directory....."
  IF p_rdir = gc_x.
    PERFORM f4_help_dir CHANGING p_spath.
  ELSE.
    "....F4 help for source path....."
    PERFORM f4_help CHANGING p_spath.
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_tpath.
  "......F4 help for Target path........."
  PERFORM f4_help CHANGING p_tpath.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
* Object Instantiation
  CREATE OBJECT lr_utility_file.
*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
**--Get the Source of file
  "......Application server....."
  IF p_rba = gc_x.
    gv_s = gc_a.
    "......Presentation server....."
  ELSEIF p_rbp = gc_x.
    gv_s = gc_p.
  ENDIF.

** Get logical file path
  "........Source path....."
  IF p_spath IS NOT INITIAL.
    PERFORM get_logical_path USING p_spath CHANGING gv_spath.
  ENDIF.
  "........Source path....."
  IF p_tpath IS NOT INITIAL.
    PERFORM get_logical_path USING p_tpath CHANGING gv_tpath.
  ENDIF.

****************************************************
**Write data to Presentation & Application Servers
****************************************************
  IF p_write = gc_x.
**--If source path is blank then return an error msg
    IF gv_spath IS INITIAL.
      MESSAGE TEXT-016 TYPE gc_s DISPLAY LIKE gc_e.
      RETURN.
    ENDIF.
**--If delimiter is other than ,T; then return
**-- error message
    IF p_delim IS INITIAL OR
       p_delim CA gc_delim.
    ELSE.
      MESSAGE TEXT-022 TYPE gc_s DISPLAY LIKE gc_e.
      RETURN.
    ENDIF.
    "......Write file....."
    PERFORM write_file.
  ENDIF.

****************************************************
** Read data from Presentation & Application Servers
****************************************************
  IF p_read = gc_x.
**--If source path is blank then return an error msg
    IF gv_spath IS INITIAL.
      MESSAGE TEXT-016 TYPE gc_s DISPLAY LIKE gc_e.
      RETURN.
    ENDIF.
    "....Read file and display read data...."
    PERFORM read_file.
  ENDIF.
****************************************************
**  Get Directory files
****************************************************
  IF p_rdir = gc_x.
**--If source path is blank then return error msg
    IF gv_spath IS INITIAL.
      MESSAGE TEXT-016 TYPE gc_s DISPLAY LIKE gc_e.
      RETURN.
    ENDIF.
    "..Get files from directory and display..."
    PERFORM get_dir_files.
  ENDIF.
****************************************************
**  Copy
****************************************************
  IF p_rcopy = gc_x.
    PERFORM copy_file.
  ENDIF.
****************************************************
**  Delete
****************************************************
  IF p_rdel = gc_x.
    PERFORM delete_file.
  ENDIF.
****************************************************
**  Archive
****************************************************
  IF p_rarch = gc_x.
    PERFORM archive_file.
  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  set_fields
*&---------------------------------------------------------------------*
*       Hide/Disable input fields based on the radiobutton selection
*----------------------------------------------------------------------*
FORM set_fields .
  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN gc_tgt.
        IF p_rcopy = gc_x OR p_rarch = gc_x .
          screen-input = 1.
          screen-invisible = 0.
        ELSE.
          screen-input = 0.
          screen-invisible = 1.
          CLEAR p_tpath.
        ENDIF.

      WHEN gc_del.
        IF p_write = gc_x.
          screen-input = 1.
          screen-invisible = 0.
        ELSE.
          screen-input = 0.
          screen-invisible = 1.
          CLEAR p_delim.
        ENDIF.

      WHEN gc_tim.
        IF p_rarch = gc_x.
          screen-input = 1.
          screen-invisible = 0.
        ELSE.
          screen-input = 0.
          screen-invisible = 1.
          CLEAR p_tstamp.
        ENDIF.
    ENDCASE.
    IF p_rcopy = gc_x OR
       p_rdel  = gc_x OR
       p_rarch = gc_x.
      IF screen-group1 = gc_pre.
        screen-input = 0.
        CLEAR p_rbp.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.                    "set_fields
*&---------------------------------------------------------------------*
*&      Form  F4_HELP
*&---------------------------------------------------------------------*
*       F4 help to select the file from presentation/application server
*----------------------------------------------------------------------*
*      <--P_P_FPATH  - Selected file path
*----------------------------------------------------------------------*
FORM f4_help CHANGING p_p_fpath.
  DATA : lv_source TYPE c.
  CLEAR lv_source.
**--Get source of file
  IF p_rba = gc_x.
    lv_source = gc_a.
  ELSEIF p_rbp = gc_x.
    lv_source = gc_p.
  ENDIF.
  "...Call the method for F4 help..."
  CALL METHOD zcl_ca_utility_file=>select_file
    EXPORTING
      i_source   = lv_source
    IMPORTING
      e_filename = p_p_fpath.
  IF sy-subrc <> 0.
    MESSAGE TEXT-020 TYPE gc_e.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_HELP_DIR
*&---------------------------------------------------------------------*
*       F4 help to select the directory from presentation/application
*       server
*----------------------------------------------------------------------*
*      <--P_P_DPATH  - Selected directory
*----------------------------------------------------------------------*
FORM f4_help_dir CHANGING p_p_dpath.
  DATA : lv_source TYPE c.
  CLEAR lv_source.
**-- Get the source file
  IF p_rba = gc_x.
    lv_source = gc_a.
  ELSEIF p_rbp = gc_x.
    lv_source = gc_p.
  ENDIF.
  "...Call the method for F4 help..."
  CALL METHOD zcl_ca_utility_file=>open_dir
    EXPORTING
      i_source        = lv_source
      i_directory     = gc_dir
      i_filemask      = gc_fmask
    CHANGING
      c_screen_dir    = p_p_dpath
    EXCEPTIONS
      open_dir_failed = 1
      OTHERS          = 2.
  IF sy-subrc <> 0.
    MESSAGE TEXT-020 TYPE gc_e.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  READ_FILE
*&---------------------------------------------------------------------*
*       Reads file from prsentation/application server
*----------------------------------------------------------------------*
FORM read_file .
**--Read the data
  REFRESH gt_param.
  CALL METHOD lr_utility_file->read_file
    EXPORTING
      i_filename        = gv_spath
      i_source          = gv_s
*     i_delimiter       = p_delim
    CHANGING
      e_datatab         = gt_param
    EXCEPTIONS
      cannot_open_file  = 1
      invalid_delimeter = 2
      error_in_read     = 3
      invalid_source    = 4
      OTHERS            = 5.
  IF sy-subrc <> 0.
    MESSAGE TEXT-017 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ELSE.
**--Display the read in the ALV list
    CALL METHOD lr_utility_file->display_alv
      CHANGING
        c_datatab = gt_param.
  ENDIF.
*  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  WRITE_FILE
*&---------------------------------------------------------------------*
*       Writes file to prsentation/application server
*----------------------------------------------------------------------*
FORM write_file .
**--Get the data to be written
**--Since we need set of records for test purpose,
**--We are not using the Parameter method here
  REFRESH gt_param.
  SELECT busproc
         busact
         param
         value
         valueto
      INTO TABLE gt_param
      UP TO 5 ROWS
      FROM ztca_param.
  IF gt_param IS NOT INITIAL.
**--Write the data
    CALL METHOD lr_utility_file->write_file
      EXPORTING
        i_filename        = gv_spath
        i_source          = gv_s
        i_delimiter       = p_delim
      CHANGING
        i_datatab         = gt_param
      EXCEPTIONS
        invalid_delimiter = 1
        invalid_source    = 2
        write_error       = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      MESSAGE TEXT-018 TYPE gc_s DISPLAY LIKE gc_e.
      RETURN.
    ELSE.
      "..File Written Successfully..."
      WRITE TEXT-024.
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_DIR_FILES
*&---------------------------------------------------------------------*
*       Get the files from selected directory and display
*----------------------------------------------------------------------*
FORM get_dir_files .
  TYPES : BEGIN OF ty_list,
            fname TYPE file_info-filename,
          END OF ty_list.
  DATA : lt_list TYPE TABLE OF string,
         lt_file TYPE TABLE OF ty_list,
         lw_list TYPE string.

  REFRESH lt_list.
  "...Call method to get the files from directory..."
  CALL METHOD lr_utility_file->get_dir_files
    EXPORTING
      i_directory              = gv_spath
      i_source                 = gv_s
      i_filemask               = gc_fmask
    IMPORTING
      r_list                   = lt_list
    EXCEPTIONS
      invalid_directory        = 1
      failed_directory_listing = 2
      OTHERS                   = 3.
  IF sy-subrc <> 0.
    MESSAGE TEXT-019 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ELSE.
    "...Display files..."
    LOOP AT lt_list INTO lw_list.
      WRITE : / lw_list.
      CLEAR lw_list.
    ENDLOOP.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  GET_LOGICAL_PATH
*&---------------------------------------------------------------------*
*       Get the full logical file path from logical path/file name
*----------------------------------------------------------------------*
*      -->P_P_SPATH  - Logical file name
*      <--P_GV_SPATH - Full Logical path
*----------------------------------------------------------------------*
FORM get_logical_path  USING    p_p_spath
                       CHANGING p_gv_spath.
  DATA : lv_length   TYPE i,
         lv_filename TYPE filename-fileintern.

  CONSTANTS: lc_lf TYPE char2 VALUE 'LF',
             lc_lp TYPE char2 VALUE 'LP'.

  CLEAR: lv_length, lv_filename.
  lv_length   = strlen( p_p_spath ).
  lv_length   = lv_length - 2.

  IF lv_length < 0.
    lv_length = 0.
  ENDIF.
**--IF the input file path name contains LP/LF then
**--get the full logical path
  IF  p_p_spath+0(2) = lc_lf         OR
      p_p_spath+0(2) = lc_lp         OR
      p_p_spath+lv_length(2) = lc_lf OR
      p_p_spath+lv_length(2) = lc_lp.

    lv_filename = p_p_spath.
**--Call the method to get the logical path
    CALL METHOD zcl_ca_utility_file=>get_logical_path
      EXPORTING
        i_filename     = lv_filename
      IMPORTING
        e_filepath     = p_gv_spath
      EXCEPTIONS
        path_not_found = 1
        OTHERS         = 2.
    "..If file path not maintained then return an error msg.."
    IF sy-subrc <> 0.
      MESSAGE TEXT-023 TYPE gc_s DISPLAY LIKE gc_e.
      RETURN.
    ENDIF.
  ELSE.
**--Return the same input file path
    p_gv_spath =  p_p_spath.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  COPY_FILE
*&---------------------------------------------------------------------*
*       Copies file from source to target application server path
*----------------------------------------------------------------------*
FORM copy_file .
**--If source path and targetpath is blank then
**--return error msg
  IF gv_spath IS INITIAL AND gv_tpath IS INITIAL.
    MESSAGE TEXT-012 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ENDIF.
  CLEAR gv_result.
  ".....Check source file exists...."
  CALL METHOD zcl_ca_utility_file=>check_fileexsist
    EXPORTING
      i_source   = gc_a
      i_filename = gv_spath
    RECEIVING
      e_exist    = gv_result.
  "..If source file not found then return error.."
  IF gv_result IS INITIAL.
    MESSAGE TEXT-003 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ENDIF.
  CLEAR gv_result.
  "....Call the method to Copy file....."
  CALL METHOD lr_utility_file->copy_file
    EXPORTING
      i_src_file   = gv_spath
      i_trg_file   = gv_tpath
    RECEIVING
      r_copy_error = gv_result.
  "...if error in copying then return an error msg..."
  IF gv_result = abap_true.
    MESSAGE TEXT-006 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ELSE.
    "....File Copied Successfully..."
    WRITE TEXT-007.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  DELETE_FILE
*&---------------------------------------------------------------------*
*       Delete file from application server
*----------------------------------------------------------------------*
FORM delete_file .
**--If source path is blank then return ab error msg
  IF gv_spath IS INITIAL.
    MESSAGE TEXT-014 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ENDIF.
**--Check file to be deleted exists
  CLEAR gv_result.
  "...Check file to be deleted exists..."
  CALL METHOD zcl_ca_utility_file=>check_fileexsist
    EXPORTING
      i_source   = gv_s
      i_filename = gv_spath
    RECEIVING
      e_exist    = gv_result.
  "..If file does not exist then return an error msg.."
  IF gv_result IS INITIAL.
    MESSAGE TEXT-003 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ENDIF.
  "..Call the method to Delete file...."
  CALL METHOD lr_utility_file->delete_file
    EXPORTING
      i_filename   = gv_spath
    EXCEPTIONS
      delete_error = 1
      OTHERS       = 2.
  "..If erorr while deleting reurn an error msg.."
  IF sy-subrc <> 0.
    MESSAGE TEXT-008 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ELSE.
    "....File deleted successfully..."
    WRITE TEXT-009.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ARCHIVE_FILE
*&---------------------------------------------------------------------*
*       Archive file
*----------------------------------------------------------------------*
FORM archive_file .
**--If source and target path is blank then return
**-- an error msg
  IF gv_spath IS INITIAL AND gv_tpath IS INITIAL.
    MESSAGE TEXT-015 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ENDIF.
  CLEAR gv_result.
  "..Check file to be archived exists.."
  CALL METHOD zcl_ca_utility_file=>check_fileexsist
    EXPORTING
      i_source   = gv_s
      i_filename = p_spath
    RECEIVING
      e_exist    = gv_result.
  "...If source file dosen't exist return an error msg.."
  IF gv_result IS INITIAL.
    MESSAGE TEXT-003 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ENDIF.
  "..Call the method to archive file.."
  CALL METHOD lr_utility_file->archive_file
    EXPORTING
      i_file               = gv_spath
      add_time_stamp       = p_tstamp
      i_arch_path          = gv_tpath
    RECEIVING
      r_arch_path          = gv_apath
    EXCEPTIONS
      error_archiving_file = 1
      OTHERS               = 2.
  "..If error while archiving file then return an error msg.."
  IF sy-subrc <> 0.
    MESSAGE TEXT-010 TYPE gc_s DISPLAY LIKE gc_e.
    RETURN.
  ELSE.
    "...File archived successfully..."
    WRITE TEXT-011.
  ENDIF.
ENDFORM.
