*&---------------------------------------------------------------------*
*& Include          Z_SDCI_CCCKPIT_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*& Form F_MODIFY_SCREEN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f_modify_screen .

  LOOP AT SCREEN.
    IF r_user = 'X'.
*Create User
      IF p_create = 'X'.
        IF screen-name = 'BUTTON3'. " OR screen-name = 'BUTTON4'.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'BUTTON4' OR screen-name = 'BUTTON5' OR screen-name = 'BUTTON6'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'BUTTON7' OR screen-name = 'BUTTON8'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'P_FILE' OR screen-name = '%_P_FILE_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE2' OR screen-name = '%_P_FILE2_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE3' OR screen-name = '%_P_FILE3_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE4' OR screen-name = '%_P_FILE4_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE5' OR screen-name = '%_P_FILE5_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE6' OR screen-name = '%_P_FILE6_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*Delete User
      IF p_delete = 'X'.
        IF screen-name = 'BUTTON8'.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'BUTTON4' OR screen-name = 'BUTTON5' OR screen-name = 'BUTTON6'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'BUTTON7' OR screen-name = 'BUTTON3'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'P_FILE' OR screen-name = '%_P_FILE_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE1' OR screen-name = '%_P_FILE1_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE3' OR screen-name = '%_P_FILE3_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE4' OR screen-name = '%_P_FILE4_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE5' OR screen-name = '%_P_FILE5_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE6' OR screen-name = '%_P_FILE6_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*For Lock User
      IF p_lock = 'X'.
        IF screen-name = 'BUTTON4'.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'BUTTON3' OR screen-name = 'BUTTON5' OR screen-name = 'BUTTON6'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'BUTTON7' OR screen-name = 'BUTTON8'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'P_FILE' OR screen-name = '%_P_FILE_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE1' OR screen-name = '%_P_FILE1_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE2' OR screen-name = '%_P_FILE2_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE4' OR screen-name = '%_P_FILE4_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE5' OR screen-name = '%_P_FILE5_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE6' OR screen-name = '%_P_FILE6_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*For Unlock User
      IF p_unlock = 'X'.
        IF screen-name = 'BUTTON5'.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'BUTTON4' OR screen-name = 'BUTTON3' OR screen-name = 'BUTTON6'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'BUTTON7' OR screen-name = 'BUTTON8'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'P_FILE' OR screen-name = '%_P_FILE_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE1' OR screen-name = '%_P_FILE1_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE2' OR screen-name = '%_P_FILE2_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE3' OR screen-name = '%_P_FILE3_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE5' OR screen-name = '%_P_FILE5_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE6' OR screen-name = '%_P_FILE6_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*For Role Update
      IF p_role = 'X'.
        IF screen-name = 'BUTTON6'.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'BUTTON4' OR screen-name = 'BUTTON5' OR screen-name = 'BUTTON3'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'BUTTON7' OR screen-name = 'BUTTON8'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'P_FILE' OR screen-name = '%_P_FILE_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE1' OR screen-name = '%_P_FILE1_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE2' OR screen-name = '%_P_FILE2_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE3' OR screen-name = '%_P_FILE3_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE4' OR screen-name = '%_P_FILE4_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE6' OR screen-name = '%_P_FILE6_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

*For Password Update
      IF p_pwd = 'X'.
        IF screen-name = 'BUTTON7'.
          screen-active = 1.
          screen-invisible = 0.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'BUTTON4' OR screen-name = 'BUTTON5' OR screen-name = 'BUTTON6'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'BUTTON3' OR screen-name = 'BUTTON8'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.

        IF screen-name = 'P_FILE' OR screen-name = '%_P_FILE_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE1' OR screen-name = '%_P_FILE1_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE2' OR screen-name = '%_P_FILE2_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE3' OR screen-name = '%_P_FILE3_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE4' OR screen-name = '%_P_FILE4_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
        IF screen-name = 'P_FILE5' OR screen-name = '%_P_FILE5_%_APP_%-TEXT'.
          screen-active = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

* button 2 should not be visible
      IF screen-name EQ 'BUTTON2' OR screen-name EQ '%_17NNS0007217682_%_%_%_%_%_%_'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.

    ELSEIF r_role = 'X'.
      IF screen-name EQ 'P_UNLOCK' OR screen-name EQ '%_P_UNLOCK_%_APP_%-TEXT'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'P_PWD' OR screen-name EQ '%_P_PWD_%_APP_%-TEXT'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'BUTTON1' OR screen-name EQ '%_17NNS0007217682_%_%_%_%_%_%_'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'P_LOCK' OR screen-name EQ '%_P_LOCK_%_APP_%-TEXT'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'BUTTON3' OR screen-name = 'BUTTON4' OR screen-name = 'BUTTON5'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'BUTTON6' OR screen-name = 'BUTTON7' OR screen-name = 'BUTTON8'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.

      IF screen-name = 'P_FILE1' OR screen-name = '%_P_FILE1_%_APP_%-TEXT'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_FILE2' OR screen-name = '%_P_FILE2_%_APP_%-TEXT'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_FILE3' OR screen-name = '%_P_FILE3_%_APP_%-TEXT'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_FILE4' OR screen-name = '%_P_FILE4_%_APP_%-TEXT'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_FILE5' OR screen-name = '%_P_FILE5_%_APP_%-TEXT'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = 'P_FILE6' OR screen-name = '%_P_FILE6_%_APP_%-TEXT'.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form DOWNLOAD_TEMPLATE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM download_template .

  CREATE OBJECT go_write_file.

  CLEAR: gv_source, gv_del, gv_header.
  gv_source = 'P'.
  gv_del = 'T'.
  gv_header = 'X'.

  CASE sscrfields.
    WHEN 'BUT3'.
      REFRESH: t_header[].
      APPEND: 'USER NAME'  TO t_header,
              'FIRST NAME' TO t_header,
              'LAST NAME'  TO t_header,
              'EMAIL'      TO t_header,
              'FUNCTION'   TO t_header,
              'DEPARTMENT' TO t_header,
              'USER TYPE'  TO t_header,
              'PASSWORD'   TO t_header,
              'USER GROUP' TO t_header,
              'VALID FROM' TO t_header,
              'VALID TO'   TO t_header,
              'ROLE NAME'  TO t_header.

*Selecting a file to save too, plus inserting default file extension .xls  Display save dialog window
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'User Changes'
          default_extension = 'XLS'
          default_file_name = 'Create User Data'
          initial_directory = 'c:\temp\'
        CHANGING
          filename          = ld_filename
          path              = ld_path
          fullpath          = ld_fullpath
          user_action       = ld_result.

*download file in excel
      IF ld_fullpath IS NOT INITIAL.
        CLEAR file.
        file = ld_fullpath.

        PERFORM write_file USING    file
                                    gv_source
                                    gv_del
                                    gv_header
                                    t_header
                           CHANGING t_table.

        IF NOT t_table[] IS INITIAL.
          MESSAGE 'File Downloaded Successfully' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'File Download Cancelled ' TYPE 'I'.
      ENDIF.

    WHEN 'BUT8'.
      REFRESH: t_header[].
      APPEND: 'USER NAME'   TO t_header.

*Selecting a file to save too, plus inserting default file extension .xls  Display save dialog window
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'User Changes '
          default_extension = 'XLS'
          default_file_name = 'Delete User data'
          initial_directory = 'c:\temp\'
        CHANGING
          filename          = ld_filename
          path              = ld_path
          fullpath          = ld_fullpath
          user_action       = ld_result.

*download file in excel
      IF ld_fullpath IS NOT INITIAL.

        CLEAR file.
        file = ld_fullpath.
        PERFORM write_file USING    file
                                    gv_source
                                    gv_del
                                    gv_header
                                    t_header
                           CHANGING t_table.

        IF NOT t_table[] IS INITIAL.
          MESSAGE 'File Downloaded Successfully' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'File Download Cancelled ' TYPE 'I'.
      ENDIF.

    WHEN 'BUT4'.
      REFRESH: t_header[].
      APPEND: 'USER NAME'      TO t_header.

*Selecting a file to save too, plus inserting default file extension .xls  Display save dialog window
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'User Changes '
          default_extension = 'XLS'
          default_file_name = 'Lock User data'
          initial_directory = 'c:\temp\'
        CHANGING
          filename          = ld_filename
          path              = ld_path
          fullpath          = ld_fullpath
          user_action       = ld_result.

*download file in excel
      IF ld_fullpath IS NOT INITIAL.

        CLEAR file.
        file = ld_fullpath.
        PERFORM write_file USING    file
                                    gv_source
                                    gv_del
                                    gv_header
                                    t_header
                           CHANGING t_table.

        IF NOT t_table[] IS INITIAL.
          MESSAGE 'File Downloaded Successfully' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'File Download Cancelled ' TYPE 'I'.
      ENDIF.

    WHEN 'BUT5'.
      REFRESH: t_header[].
      APPEND: 'USER NAME'        TO t_header.

*Selecting a file to save too, plus inserting default file extension .xls  Display save dialog window
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'User Changes '
          default_extension = 'XLS'
          default_file_name = 'Unlock User data'
          initial_directory = 'c:\temp\'
        CHANGING
          filename          = ld_filename
          path              = ld_path
          fullpath          = ld_fullpath
          user_action       = ld_result.

*download file in excel
      IF ld_fullpath IS NOT INITIAL.

        CLEAR file.
        file = ld_fullpath.
        PERFORM write_file USING    file
                                    gv_source
                                    gv_del
                                    gv_header
                                    t_header
                           CHANGING t_table.

        IF NOT t_table[] IS INITIAL.
          MESSAGE 'File Downloaded Successfully' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'File Download Cancelled ' TYPE 'I'.
      ENDIF.

    WHEN 'BUT6'.
      REFRESH: t_header[].
      APPEND: 'USER NAME'    TO t_header,
              'FIRST NAME'   TO t_header,
              'LAST NAME'    TO t_header,
              'EMAIL'        TO t_header,
              'FUNCTION'     TO t_header,
              'DEPARTMENT'   TO t_header,
              'USER TYPE'    TO t_header,
              'NEW PASSWORD' TO t_header,
              'USER GROUP'   TO t_header,
              'VALID FROM'   TO t_header,
              'VALID TO'     TO t_header,
              'ROLE NAME'    TO t_header,
              'FLAG'         TO t_header.

*Selecting a file to save too, plus inserting default file extension .xls  Display save dialog window
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'User Changes '
          default_extension = 'XLS'
          default_file_name = 'Role Change User data'
          initial_directory = 'c:\temp\'
        CHANGING
          filename          = ld_filename
          path              = ld_path
          fullpath          = ld_fullpath
          user_action       = ld_result.

*download file in excel
      IF ld_fullpath IS NOT INITIAL.

        CLEAR file.
        file = ld_fullpath.
        PERFORM write_file USING    file
                                    gv_source
                                    gv_del
                                    gv_header
                                    t_header
                           CHANGING t_table.

        IF NOT t_table[] IS INITIAL.
          MESSAGE 'File Downloaded Successfully' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'File Download Cancelled ' TYPE 'I'.
      ENDIF.

    WHEN 'BUT7'.
      REFRESH: t_header[].
      APPEND: 'USER NAME'    TO t_header,
              'NEW PASSWORD' TO t_header.

*Selecting a file to save too, plus inserting default file extension .xls  Display save dialog window
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'User Changes '
          default_extension = 'XLS'
          default_file_name = 'Password Change User data'
          initial_directory = 'c:\temp\'
        CHANGING
          filename          = ld_filename
          path              = ld_path
          fullpath          = ld_fullpath
          user_action       = ld_result.

*download file in excel
      IF ld_fullpath IS NOT INITIAL.

        CLEAR file.
        file = ld_fullpath.
        PERFORM write_file USING    file
                                    gv_source
                                    gv_del
                                    gv_header
                                    t_header
                           CHANGING t_table.

        IF NOT t_table[] IS INITIAL.
          MESSAGE 'File Downloaded Successfully' TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'File Download Cancelled ' TYPE 'I'.
      ENDIF.

    WHEN 'BUT2'. " Role Changes button
*set header
      APPEND 'ROLE NAME'       TO t_header.       " Role
      APPEND 'ROLE TEXT'       TO t_header.       " Role
      APPEND 'ACTIONS'         TO t_header.       " Role
      APPEND 'TCODE'           TO t_header.       " Role


*Selecting a file to save too, plus inserting default file extension .xls  Display save dialog window
      CALL METHOD cl_gui_frontend_services=>file_save_dialog
        EXPORTING
          window_title      = 'Role Changes '
          default_extension = 'XLS'
          default_file_name = 'Role data'
          initial_directory = 'c:\temp\'
        CHANGING
          filename          = ld_filename
          path              = ld_path
          fullpath          = ld_fullpath
          user_action       = ld_result.

*download file in excel
      IF ld_fullpath IS NOT INITIAL.

        CLEAR file.
        file = ld_fullpath.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            bin_filesize        = ''
            filename            = file
            filetype            = 'DAT'
          TABLES
            data_tab            = t_table
            fieldnames          = t_header
          EXCEPTIONS
            file_open_error     = 1
            file_write_error    = 2
            invalid_filesize    = 3
            invalid_table_width = 4
            invalid_type        = 5
            no_batch            = 6
            unknown_error       = 7
            OTHERS              = 8.
        IF sy-subrc = 0.
          MESSAGE 'File Downloaded Successfully' TYPE 'I'.
        ENDIF.

      ELSE.
        MESSAGE 'File Download Cancelled ' TYPE 'I'.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form WRITE_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM write_file USING    file
                         gv_source
                         gv_del
                         gv_header
                         t_header
                CHANGING t_table.

  CALL METHOD go_write_file->write_file
    EXPORTING
      i_filename        = file
      i_source          = gv_source
      i_delimiter       = gv_del
      i_addheader       = gv_header
      i_fields          = t_header
*     i_accessmode      = 'O'
    CHANGING
      i_datatab         = t_table
    EXCEPTIONS
      invalid_delimiter = 1
      invalid_source    = 2
      write_error       = 3
      OTHERS            = 4.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form F4_FILENAME
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM f4_filename CHANGING p_fpath TYPE string.

* Call the method for F4 help
  CALL METHOD zcl_ca_utility_adsk=>select_file
    EXPORTING
      i_source   = 'P'
    IMPORTING
      e_filename = p_fpath.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_UPDATE_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM check_update_user.

*Create User
  IF p_create = 'X'.
    REFRESH: gt_file.
    PERFORM read_file USING p_file1 CHANGING gt_file.

    IF NOT gt_file[] IS INITIAL.
*      DESCRIBE TABLE gt_file LINES lv_string.
*      IF NOT lv_string IS INITIAL.
      CLEAR lv_strg.
      lv_strg = 'Users will be created as per the data provided'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation '
          text_question         = lv_strg                 " Do you want to Continue'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = popup_return            " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc = 0 AND popup_return IS NOT INITIAL.
        IF  popup_return = '1'.
          PERFORM create_user TABLES gt_file.
        ELSEIF popup_return = '2' OR popup_return = 'A'.
          MESSAGE 'Update Cancelled' TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
*      ENDIF.
    ELSE.
      MESSAGE 'No records found from the file ' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*Modify User
  IF p_role = 'X'.
    REFRESH: gt_file.
    PERFORM read_file USING p_file5 CHANGING gt_file.

    IF NOT gt_file[] IS INITIAL.
      CLEAR lv_strg.
      lv_strg = 'Users will be modified as per the data provided'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation '
          text_question         = lv_strg                 " Do you want to Continue'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = popup_return            " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc = 0 AND popup_return IS NOT INITIAL.
        IF  popup_return = '1'.
          PERFORM modify_user TABLES gt_file.
        ELSEIF popup_return = '2' OR popup_return = 'A'.
          MESSAGE 'Update Cancelled' TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'No records found from the file ' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*Delete User
  IF p_delete = 'X'.
    REFRESH: gt_file_n.
    PERFORM read_file USING p_file2 CHANGING gt_file_n.

    IF NOT gt_file_n[] IS INITIAL.
      CLEAR lv_strg.
      lv_strg = 'Users will be deleted as per the data provided'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation '
          text_question         = lv_strg                 " Do you want to Continue'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = popup_return            " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc = 0 AND popup_return IS NOT INITIAL.
        IF  popup_return = '1'.
          PERFORM delete_user TABLES gt_file_n.
        ELSEIF popup_return = '2' OR popup_return = 'A'.
          MESSAGE 'Update Cancelled' TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'No records found from the file ' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*Lock User
  IF p_lock = 'X'.
    REFRESH: gt_file_n.
    PERFORM read_file USING p_file3 CHANGING gt_file_n.

    IF NOT gt_file_n[] IS INITIAL.
      CLEAR lv_strg.
      lv_strg = 'Users will be locked as per the data provided'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation '
          text_question         = lv_strg                 " Do you want to Continue'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = popup_return            " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc = 0 AND popup_return IS NOT INITIAL.
        IF  popup_return = '1'.
          PERFORM lock_user TABLES gt_file_n.
        ELSEIF popup_return = '2' OR popup_return = 'A'.
          MESSAGE 'Update Cancelled' TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'No records found from the file ' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*Unlock User
  IF p_unlock = 'X'.
    REFRESH: gt_file_n.
    PERFORM read_file USING p_file4 CHANGING gt_file_n.

    IF NOT gt_file_n[] IS INITIAL.
      CLEAR lv_strg.
      lv_strg = 'Users will be unlocked as per the data provided'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation '
          text_question         = lv_strg                 " Do you want to Continue'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = popup_return            " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc = 0 AND popup_return IS NOT INITIAL.
        IF  popup_return = '1'.
          PERFORM unlock_user TABLES gt_file_n.
        ELSEIF popup_return = '2' OR popup_return = 'A'.
          MESSAGE 'Update Cancelled' TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'No records found from the file ' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

*Password Reset
  IF p_pwd = 'X'.
    REFRESH: gt_file_p.
    PERFORM read_file USING p_file6 CHANGING gt_file_p.

    IF NOT gt_file_p[] IS INITIAL.
      CLEAR lv_strg.
      lv_strg = 'Users password will be reset as per the data provided'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmation '
          text_question         = lv_strg                 " Do you want to Continue'
          text_button_1         = 'Yes'
          text_button_2         = 'No'
          default_button        = '2'
          display_cancel_button = 'X'
        IMPORTING
          answer                = popup_return            " to hold the FM's return value
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc = 0 AND popup_return IS NOT INITIAL.
        IF  popup_return = '1'.
          PERFORM pwd_reset_user TABLES gt_file_p.
        ELSEIF popup_return = '2' OR popup_return = 'A'.
          MESSAGE 'Update Cancelled' TYPE 'I'.
          LEAVE LIST-PROCESSING.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE 'No records found from the file ' TYPE 'I'.
      LEAVE LIST-PROCESSING.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form READ_FILE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM read_file USING p_path TYPE string
               CHANGING pt_data TYPE STANDARD TABLE.

  DATA: lo_read_file TYPE REF TO zcl_ca_utility_adsk.
  CREATE OBJECT lo_read_file.

  CALL METHOD lo_read_file->read_file
    EXPORTING
      i_filename        = p_path
      i_source          = 'P'
      i_delimiter       = 'T'
*     i_codepage        =
      i_hdr             = 'X'
    CHANGING
      e_datatab         = pt_data
    EXCEPTIONS
      cannot_open_file  = 1
      invalid_delimeter = 2
      error_in_read     = 3
      invalid_source    = 4
      OTHERS            = 5.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form CREATE_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM create_user TABLES gt_file .

  REFRESH: gt_c_user, lt_table.
  lt_table = gt_file[].

  LOOP AT lt_table INTO DATA(lw_table).
    gw_c_user = VALUE #( user_name = lw_table-user_name
                         agr_name  = lw_table-agr_name ).
    APPEND gw_c_user TO gt_c_user.
    CLEAR gw_c_user.
  ENDLOOP.

  SORT lt_table BY user_name.
  DELETE ADJACENT DUPLICATES FROM lt_table COMPARING user_name.

  LOOP AT lt_table INTO lw_table.

*User Name
    lv_user = lw_table-user_name.

*User Type
    lt_logondata-ustyp = lw_table-ustyp.

*User Group
    lt_logondata-class = lw_table-user_grp.

*Valid To Date
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lw_table-gltgv
      IMPORTING
        date_internal            = lw_table-gltgv
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    lt_logondata-gltgv = lw_table-gltgv.

*Valid From Date
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lw_table-gltgb
      IMPORTING
        date_internal            = lw_table-gltgb
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    lt_logondata-gltgb = lw_table-gltgb.

*Address
    lt_address-firstname  = lw_table-name_first.
    lt_address-lastname   = lw_table-name_last.
    lt_address-department = lw_table-department.
    lt_address-function   = lw_table-function.
    lt_address-e_mail     = lw_table-smtp_addr.

*Password
    lt_password-bapipwd = lw_table-password.

*Call the BAPI to create the user
    CALL FUNCTION 'BAPI_USER_CREATE1'
      EXPORTING
        username  = lv_user
        logondata = lt_logondata
        password  = lt_password
        address   = lt_address
      TABLES
        parameter = lt_parameter
        return    = lt_return.

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc = 0.
      DELETE gt_c_user WHERE user_name = lv_user.
      LOOP AT lt_return INTO ls_return.
        gw_message-message = ls_return-message.
        gw_message-user = lv_user.
        APPEND gw_message TO gt_message.
        CLEAR: gw_message, ls_return.
      ENDLOOP.
    ELSE.
      gw_message-message = ls_return-message.
      gw_message-user = lv_user.
      APPEND gw_message TO gt_message.
      CLEAR: gw_message.
    ENDIF.
    CLEAR: lw_table, lv_user, lt_logondata, lt_address, lt_password.
    REFRESH: lt_return, lt_parameter.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form MODIFY_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM modify_user TABLES gt_file.

  REFRESH: gt_c_user, lt_table.
  lt_table = gt_file[].

  LOOP AT lt_table INTO DATA(lw_table).
    gw_c_user = VALUE #( user_name = lw_table-user_name
                         agr_name  = lw_table-agr_name
                         flag      = lw_table-flag ).
    APPEND gw_c_user TO gt_c_user.
    CLEAR gw_c_user.
  ENDLOOP.

  CLEAR: lw_table.
  LOOP AT lt_table INTO lw_table.

*Valid To Date
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lw_table-gltgv
      IMPORTING
        date_internal            = lw_table-gltgv
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    ls_logondata_chg-gltgv = lw_table-gltgv.

*Valid From Date
    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lw_table-gltgb
      IMPORTING
        date_internal            = lw_table-gltgb
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    ls_logondata_chg-gltgb = lw_table-gltgb.

*User Type
    ls_logondata_chg-ustyp = ls_table-ustyp.

    ls_logondatax_chg-class = 'X'.
    ls_logondatax_chg-gltgv = 'X'.
    ls_logondatax_chg-gltgb = 'X'.
    ls_logondatax_chg-ustyp = 'X'.

    ls_addressx_chg-firstname  = 'X'.
    ls_addressx_chg-lastname   = 'X'.
    ls_addressx_chg-e_mail     = 'X'.
    ls_addressx_chg-department = 'X'.
    ls_addressx_chg-function   = 'X'.

* Address Data
    ls_address_chg-firstname  = lw_table-name_first.
    ls_address_chg-lastname   = lw_table-name_last.
    ls_address_chg-e_mail     = lw_table-smtp_addr.
    ls_address_chg-department = lw_table-department.
    ls_address_chg-function   = lw_table-function.

* Password
    ls_password_chg-bapipwd  = lw_table-password.
    ls_passwordx_chg-bapipwd = 'X'.

    REFRESH lt_return_chg.
    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING
        username   = lw_table-user_name
        logondata  = ls_logondata_chg
        logondatax = ls_logondatax_chg
        address    = ls_address_chg
        addressx   = ls_addressx_chg
        password   = ls_password_chg
        passwordx  = ls_passwordx_chg
      TABLES
        return     = lt_return_chg.

    READ TABLE lt_return_chg TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc = 0.
      LOOP AT lt_return INTO ls_return_chg.
        gw_message-message = ls_return_chg-message.
        gw_message-user    = lw_table-user_name.
        APPEND gw_message TO gt_message.
        CLEAR: gw_message, ls_return.
      ENDLOOP.
    ELSE.
      gw_message-message = ls_return_chg-message.
      gw_message-user    = lw_table-user_name.
      APPEND gw_message TO gt_message.
      CLEAR: gw_message.
    ENDIF.

    CLEAR: lw_table, ls_return_chg, ls_logondata_chg, ls_logondatax_chg,
           ls_address_chg, ls_addressx_chg, ls_password_chg, ls_passwordx_chg.
    REFRESH: lt_return_chg.
  ENDLOOP.

  DELETE gt_c_user WHERE flag IS INITIAL.

  SORT gt_c_user BY user_name flag.
  CLEAR: gw_c_user.
  LOOP AT gt_c_user INTO gw_c_user.

    REFRESH: lt_activitygroup, lt_return_usrdet.
    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username       = gw_c_user-user_name
      TABLES
        activitygroups = lt_activitygroup
        return         = lt_return_usrdet.

    IF NOT lt_activitygroup IS INITIAL.
      LOOP AT lt_activitygroup INTO DATA(lw_activitygroup).
        ls_activity-agr_name = lw_activitygroup-agr_name.
        CLEAR ls_text.
        READ TABLE lt_text INTO ls_text WITH KEY agr_name = lw_activitygroup-agr_name BINARY SEARCH.
        IF sy-subrc = 0.
          ls_activity-agr_text = ls_text-text.
        ENDIF.
        APPEND ls_activity TO lt_activity.
        CLEAR: ls_activity.
      ENDLOOP.
    ENDIF.

    IF gw_c_user-flag = 'D'.
      DELETE lt_activity WHERE agr_name = gw_c_user-agr_name.
    ENDIF.

    IF gw_c_user-flag = 'A'.
      ls_activity-agr_name = gw_c_user-agr_name.
      APPEND ls_activity TO lt_activity.
      CLEAR: ls_activity.
    ENDIF.

    AT END OF user_name.
      IF NOT lt_activity IS INITIAL.
        SORT lt_activity BY agr_name.
        DELETE ADJACENT DUPLICATES FROM lt_activity COMPARING agr_name.
        DELETE lt_activity WHERE agr_name IS INITIAL.

        REFRESH lt_return_role.
        CALL FUNCTION 'BAPI_USER_ACTGROUPS_ASSIGN'
          EXPORTING
            username       = gw_c_user-user_name "ls_table-user_name
          TABLES
            activitygroups = lt_activity
            return         = lt_return_role.

        READ TABLE lt_return_role TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc = 0.
          LOOP AT lt_return_role INTO ls_return_role.
            gw_message-message = ls_return_role-message.
            gw_message-user    = lw_table-user_name.
            APPEND gw_message TO gt_message.
            CLEAR: gw_message, ls_return.
          ENDLOOP.
        ELSE.
          gw_message-message = ls_return_role-message.
          gw_message-user    = lw_table-user_name.
          APPEND gw_message TO gt_message.
          CLEAR: gw_message.
        ENDIF.
      ELSE.
        REFRESH: lt_return_role.
        CALL FUNCTION 'BAPI_USER_ACTGROUPS_DELETE'
          EXPORTING
            username = gw_c_user-user_name
          TABLES
            return   = lt_return_role.

        READ TABLE lt_return_role TRANSPORTING NO FIELDS WITH KEY type = 'E'.
        IF sy-subrc = 0.
          LOOP AT lt_return_role INTO ls_return_role.
            gw_message-message = ls_return_role-message.
            gw_message-user    = lw_table-user_name.
            APPEND gw_message TO gt_message.
            CLEAR: gw_message, ls_return.
          ENDLOOP.
        ELSE.
          gw_message-message = ls_return_role-message.
          gw_message-user    = lw_table-user_name.
          APPEND gw_message TO gt_message.
          CLEAR: gw_message.
        ENDIF.
      ENDIF.
      REFRESH: lt_activity, lt_return_role.
    ENDAT.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DELETE_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM delete_user TABLES gt_file_n.

  REFRESH: lt_table_n[].
  lt_table_n[] = gt_file_n[].

  LOOP AT lt_table_n INTO DATA(lw_table_n).
    REFRESH: lt_return_delete.
    CALL FUNCTION 'BAPI_USER_DELETE'
      EXPORTING
        username = lw_table_n-user_name
      TABLES
        return   = lt_return_delete.

    LOOP AT lt_return_delete INTO ls_return_delete.
      gw_message-message = ls_return_delete-message.
      gw_message-user = lw_table_n-user_name.
    ENDLOOP.
    CLEAR lw_table_n.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form LOCK_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM lock_user TABLES gt_file_n.

  REFRESH: lt_table_n[].
  lt_table_n[] = gt_file_n[].

  LOOP AT lt_table_n INTO DATA(lw_table_n).
    REFRESH: lt_return_lock.
    CALL FUNCTION 'BAPI_USER_LOCK'
      EXPORTING
        username = lw_table_n-user_name
      TABLES
        return   = lt_return_lock.

    LOOP AT lt_return_lock INTO ls_return_lock.
      gw_message-message = ls_return_lock-message.
      gw_message-user = lw_table_n-user_name.
    ENDLOOP.
    CLEAR lw_table_n.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form UNLOCK_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM unlock_user TABLES gt_file_n.

  REFRESH: lt_table_n[].
  lt_table_n[] = gt_file_n[].

  LOOP AT lt_table_n INTO DATA(lw_table_n).
    REFRESH: lt_return_unlock.
    CALL FUNCTION 'BAPI_USER_UNLOCK'
      EXPORTING
        username = lw_table_n-user_name
      TABLES
        return   = lt_return_unlock.

    READ TABLE lt_return_unlock TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc = 0.
      LOOP AT lt_return_unlock INTO ls_return_unlock.
        gw_message-message = ls_return_unlock-message.
        gw_message-user    = lw_table_n-user_name.
        APPEND gw_message TO gt_message.
        CLEAR: gw_message, ls_return.
      ENDLOOP.
    ELSE.
      gw_message-message = ls_return_unlock-message.
      gw_message-user    = lw_table_n-user_name.
      APPEND gw_message TO gt_message.
      CLEAR: gw_message.
    ENDIF.
    CLEAR: ls_return_unlock, lw_table_n.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form PWD_RESET_USER
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
FORM pwd_reset_user TABLES gt_file_p.

  DATA : ls_password   TYPE bapipwd,
         ls_passwordx  TYPE bapipwdx,
         lt_return_pwd TYPE STANDARD TABLE OF bapiret2,
         ls_return_pwd LIKE LINE OF lt_return_pwd.

  REFRESH: lt_table_p[].
  lt_table_p[] = gt_file_p[].

  LOOP AT lt_table_p INTO DATA(lw_table_p).

* password change
    ls_password-bapipwd = lw_table_p-password.
    ls_passwordx-bapipwd = 'X'.

    REFRESH lt_return_pwd.
    CALL FUNCTION 'BAPI_USER_CHANGE'
      EXPORTING
        username  = lw_table_p-user_name
        password  = ls_password
        passwordx = ls_passwordx
      TABLES
        return    = lt_return_pwd.

    READ TABLE lt_return_pwd TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc = 0.
      LOOP AT lt_return_pwd INTO ls_return_pwd.
        gw_message-message = ls_return_pwd-message.
        gw_message-user    = lw_table_p-user_name.
        APPEND gw_message TO gt_message.
        CLEAR: gw_message, ls_return.
      ENDLOOP.
    ELSE.
      gw_message-message = ls_return_pwd-message.
      gw_message-user    = lw_table_p-user_name.
      APPEND gw_message TO gt_message.
      CLEAR: gw_message.
    ENDIF.
    CLEAR: ls_return_pwd, lw_table_p.
  ENDLOOP.

ENDFORM.
