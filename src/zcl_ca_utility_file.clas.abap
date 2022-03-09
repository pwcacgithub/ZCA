class ZCL_CA_UTILITY_FILE definition
  public
  inheriting from ZCL_CA_UTILITY
  final
  create public .

public section.
  type-pools ABAP .

  types:
    ty_t_filename TYPE TABLE OF string .
  types:
    BEGIN OF ty_field,
        value TYPE char100,
      END OF ty_field .
  types:
    it_field TYPE TABLE OF ty_field .

  constants GC_FLD_FNAME type CHAR5 value 'FNAME' ##NO_TEXT.
  constants GC_FLD_ROWNO type CHAR6 value 'ROWCNT' ##NO_TEXT.
  constants GC_CHAR_X type CHAR1 value 'X' ##NO_TEXT.
  constants GC_LOC_SERVER type CHAR1 value 'P' ##NO_TEXT.
  constants GC_APP_SERVER type CHAR1 value 'A' ##NO_TEXT.
  constants GC_LPATH type CHAR1 value 'L' ##NO_TEXT.
  constants GC_DEV type CHAR3 value 'DEV' ##NO_TEXT.
  constants GC_QA type CHAR3 value 'QA' ##NO_TEXT.
  constants GC_PRD type CHAR3 value 'PRD' ##NO_TEXT.
  constants GC_ED1 type CHAR3 value 'ED1' ##NO_TEXT.
  constants GC_EQ1 type CHAR3 value 'EQ1' ##NO_TEXT.
  constants GC_EP1 type CHAR3 value 'EP1' ##NO_TEXT.
  constants GC_ERROR_PATH type CHAR6 value 'Error/' ##NO_TEXT.

  class-methods SELECT_FILE
    importing
      value(I_SOURCE) type CHAR1
      !I_GUI_INITIAL_DIRECTORY type STRING optional
      value(I_APPPATH_TYPE) type CHAR1 optional
      !I_TITLE type STRING default 'Select File'
      !I_GUI_EXTENSION type STRING optional
      !I_GUI_EXT_FILTER type STRING optional
    exporting
      !E_FILENAME type STRING
    exceptions
      F4_HELP_FAILED .
  class-methods CHECK_DIREXIST
    importing
      !I_DIRECTORY type STRING
      !I_SOURCE type CHAR1
    returning
      value(E_EXIST) type XFELD .
  class-methods CHECK_FILEEXSIST
    importing
      !I_SOURCE type CHAR1
      !I_FILENAME type STRING
    returning
      value(E_EXIST) type XFELD .
  class-methods OPEN_DIR
    importing
      !I_SOURCE type CHAR1
      !I_DIRECTORY type STRING optional
      !I_FILEMASK type STRING optional
    changing
      !C_SCREEN_DIR type STRING
    exceptions
      OPEN_DIR_FAILED .
  methods GET_DIR_FILES
    importing
      !I_DIRECTORY type STRING
      !I_SOURCE type CHAR1
      !I_FILEMASK type STRING default '*'
    exporting
      !R_LIST type TY_T_FILENAME
    exceptions
      INVALID_DIRECTORY
      FAILED_DIRECTORY_LISTING .
  class-methods CONVERT_TABLE_TO_STRING
    importing
      !I_DELIMITER type CHAR1
      !I_DATATAB type TABLE
      !I_FCAT type LVC_T_FCAT
    exporting
      !E_DATATAB type TABLE .
  methods FCAT_FROM_INTERNAL_TABLE
    importing
      !I_TABLE type ANY TABLE
    returning
      value(RT_FCAT) type LVC_T_FCAT .
  methods READ_FILE
    importing
      !I_FILENAME type STRING
      !I_SOURCE type CHAR1
      !I_DELIMITER type CHAR1 optional
      !I_CODEPAGE type ABAP_ENCOD optional
      !I_HDR type XFELD optional
    changing
      !E_DATATAB type TABLE
    exceptions
      CANNOT_OPEN_FILE
      INVALID_DELIMETER
      ERROR_IN_READ
      INVALID_SOURCE .
  methods READ_DIR_FILES
    importing
      !I_DIRECTORY type STRING
      !I_FILENAME type STRING default '*'
      !I_SOURCE type CHAR1
      value(I_DELIMITER) type CHAR1 optional
      !I_CODEPAGE type ABAP_ENCOD optional
      !I_HDR type XFELD optional
    changing
      !E_DATATAB type TABLE
    exceptions
      ERROR_IN_READ
      INVALID_DIRECTORY
      INVALID_SOURCE
      INVALID_DELIMITER
      DIR_LISTING_NOT_FOUND .
  methods WRITE_FILE
    importing
      !I_FILENAME type STRING
      !I_SOURCE type CHAR1
      !I_DELIMITER type CHAR1 optional
      !I_ADDHEADER type ABAP_BOOL optional
      !I_FIELDS type TABLE optional
    changing
      !I_DATATAB type TABLE
    exceptions
      INVALID_DELIMITER
      INVALID_SOURCE
      WRITE_ERROR .
  methods COPY_FILE
    importing
      !I_SRC_FILE type STRING
      !I_TRG_FILE type STRING
    returning
      value(R_COPY_ERROR) type ABAP_BOOL .
  methods DELETE_FILE
    importing
      !I_FILENAME type STRING
    exceptions
      DELETE_ERROR .
  methods ARCHIVE_FILE
    importing
      !I_FILE type STRING
      !ADD_TIME_STAMP type ABAP_BOOL optional
      !I_ARCH_PATH type STRING optional
    returning
      value(R_ARCH_PATH) type STRING
    exceptions
      ERROR_ARCHIVING_FILE .
  methods GET_FPATH
    importing
      !I_FILE type STRING
      !I_TYPE type CHAR01
    exporting
      !E_FPATH type STRING
      !E_FNAME type STRING
      !E_FULLPATH type STRING .
  class-methods GET_LOGICAL_PATH
    importing
      !I_FILENAME type FILENAME-FILEINTERN
      !I_PARA2 type STRING optional
      !I_PARA3 type STRING optional
      !I_PARA1 type STRING optional
    exporting
      !E_FILEPATH type STRING
    exceptions
      PATH_NOT_FOUND .
protected section.
private section.

  data GT_FILES type TY_T_FILENAME .
  constants GC_DELMS type CHAR5 value ',|T;' ##NO_TEXT.
  constants GC_BSLASH type CHAR1 value '/' ##NO_TEXT.
  constants GC_FSLASH type CHAR1 value '\' ##NO_TEXT.
  constants GC_INCLUDE type CHAR1 value 'I' ##NO_TEXT.
  constants GC_EQUAL type CHAR2 value 'EQ' ##NO_TEXT.
  constants GC_ARCHIVE_PATH type CHAR8 value 'Archive/' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CA_UTILITY_FILE IMPLEMENTATION.


  METHOD archive_file.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->ARCHIVE_FILE
* Program Title: Archive Application server File
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method Archives Application server file
* It Copies the current file to archived file path location and deletes
* the current file
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Constant
*----------------------------------------------------------------------*
    CONSTANTS: lc_a   TYPE c      VALUE 'A',
               lc_dot TYPE c      VALUE '.'.
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA: lv_string     TYPE string,
          lv_path       TYPE string,
          lv_filename   TYPE string,
          lv_target     TYPE string,
          lv_extnesion  TYPE string,
          lv_len        TYPE i,
          lv_msg1       TYPE msgv1,
          lv_msg2       TYPE msgv2,
          lv_msg3       TYPE msgv3,
          lv_msg4       TYPE msgv4,
          lv_copy_error TYPE abap_bool.


    CLEAR: lv_len,lv_filename ,lv_path,lv_target.

    IF i_arch_path IS NOT INITIAL.
      lv_target = i_arch_path.
    ELSE.
      me->get_fpath( EXPORTING
                      i_file = i_file
                      i_type = lc_a
                    IMPORTING
                      e_fullpath = lv_target ).
    ENDIF.

    IF add_time_stamp EQ abap_true.

      SPLIT lv_target AT lc_dot INTO lv_filename lv_extnesion.
      IF sy-subrc = 0.
        CONCATENATE lv_filename sy-datum sy-timlo lc_dot lv_extnesion INTO lv_target.
        CONDENSE lv_target.
      ENDIF.

    ENDIF.

    lv_copy_error = me->copy_file( EXPORTING i_src_file = i_file
                                       i_trg_file = lv_target ).

    IF lv_copy_error = abap_false.
      me->delete_file( EXPORTING i_filename = i_file ).
      r_arch_path = lv_target.
    ELSE.
      RAISE error_archiving_file.
    ENDIF.

  ENDMETHOD.


  METHOD check_direxist.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->CHECK_DIREXIST
* Program Title: Check directory exists
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method is used to check the existence of directory
* If it exists then it will return the parametere E_EXIST = X esle blank
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
************************************************************************
*------------------------------------------------------*
* Declaration for Variables
*------------------------------------------------------*
    DATA: lv_path TYPE salfile-longname.
*------------------------------------------------------*
* Declaration for Internal tables
*------------------------------------------------------*
    DATA: lt_salfldir TYPE TABLE OF salfldir.
**--Refresh Internal tables and clear variables
    CLEAR: lv_path.
    REFRESH: lt_salfldir.

    CASE i_source.
**--Check Server Directory Exsists
      WHEN gc_app_server.
        MOVE i_directory TO lv_path.
        CALL FUNCTION 'RZL_READ_DIR_LOCAL'
          EXPORTING
            name               = lv_path
          TABLES
            file_tbl           = lt_salfldir
          EXCEPTIONS
            argument_error     = 1
            not_found          = 2
            no_admin_authority = 3
            OTHERS             = 4.
        IF sy-subrc <> 0.
          e_exist = ' '.
        ELSE.
          e_exist = gc_char_x.
          REFRESH lt_salfldir.
        ENDIF.
**--Check Local Directory Exsists
      WHEN gc_loc_server.
        CALL METHOD cl_gui_frontend_services=>directory_exist
          EXPORTING
            directory            = i_directory
          RECEIVING
            result               = e_exist
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5.
        IF sy-subrc <> 0.
*         Implement suitable error handling here
        ENDIF.
**--Directory Does not Exsist
      WHEN OTHERS.
        e_exist = ' '.
    ENDCASE.
  ENDMETHOD.


  METHOD check_fileexsist.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->CHECK_FILEEXSIST
* Program Title: Check file exists
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method is used to check the existence of file
* If it exists then it will return the parametere E_EXIST = X esle blank
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236    1 / Initial version
* 11/28/2017  X51385       D10K915916    2 / Exception Handling for
*                                           Authorization check
************************************************************************
*------------------------------------------------------*
* Declaration for Variables
*------------------------------------------------------*
    DATA: lv_string     TYPE string,
          lo_exceptions TYPE REF TO cx_root.
**--Refresh Internal tables and clear variables
    CLEAR:lv_string.

    CASE i_source.
**--Check Server File Exsists
      WHEN gc_app_server.
        TRY .
            OPEN DATASET i_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
            IF sy-subrc = 0.
              READ DATASET i_filename INTO lv_string.
              IF sy-subrc = 0 .
                e_exist = gc_char_x.
              ENDIF.
              CLOSE DATASET i_filename.
            ENDIF.
          CATCH cx_root INTO lo_exceptions.
            e_exist = ' '.
        ENDTRY.
**--Check Local File Exsists
      WHEN gc_loc_server.

        CALL METHOD cl_gui_frontend_services=>file_exist
          EXPORTING
            file                 = i_filename
          RECEIVING
            result               = e_exist
          EXCEPTIONS
            cntl_error           = 1
            error_no_gui         = 2
            wrong_parameter      = 3
            not_supported_by_gui = 4
            OTHERS               = 5.
        IF sy-subrc <> 0.
*         Implement suitable error handling here
        ENDIF.
**--File Does Not exsis
      WHEN OTHERS.
        e_exist = ' '.
    ENDCASE.
  ENDMETHOD.


  METHOD convert_table_to_string.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->CONVERT_TABLE_TO_STRING
* Program Title: Convert internal table to string
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method Converts the  internal table data
*  into string
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236  1 / Initial version
************************************************************************
*------------------------------------------------------*
* Declaration for Variable & Field symbol
*------------------------------------------------------*
    FIELD-SYMBOLS: <fs_input> TYPE any,
                   <fs_fld>   TYPE any.

    DATA  : lv_tabix  TYPE sy-tabix,
            lv_string TYPE  string,
            lv_constr TYPE string.
*------------------------------------------------------*
* Declaration for Work area
*------------------------------------------------------*
    DATA:wa_fcat   TYPE LINE OF  lvc_t_fcat.
*******************************************************
** Loop the input internal table data and build output
*******************************************************
    LOOP AT i_datatab ASSIGNING <fs_input>.
      CLEAR : lv_string,lv_tabix.
      IF i_delimiter IS INITIAL.
        MOVE <fs_input> TO lv_string.
      ELSE.
        LOOP AT i_fcat INTO wa_fcat.
          lv_tabix = lv_tabix + 1.
          ASSIGN COMPONENT lv_tabix OF STRUCTURE <fs_input> TO <fs_fld>.
          IF sy-subrc EQ 0.
            MOVE <fs_fld> TO lv_constr.
            IF sy-tabix EQ '1'.
              CONCATENATE lv_string lv_constr
                     INTO lv_string.
            ELSE.
              CONCATENATE lv_string lv_constr
                     INTO lv_string
                     SEPARATED BY i_delimiter.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      APPEND lv_string TO e_datatab.
    ENDLOOP.
    UNASSIGN: <fs_input>,<fs_fld>.
  ENDMETHOD.


  METHOD copy_file.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->COPY_FILE
* Program Title: Copy File
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method Copies File from source to target path
* at application server
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA: lv_source TYPE dxfile-filename,
          lv_target TYPE dxfile-filename.

    CLEAR: lv_source,lv_target.

    MOVE i_src_file TO lv_source.
    MOVE i_trg_file TO lv_target.

    CALL FUNCTION 'DX_FILE_COPY_NO_CONVERSION'
      EXPORTING
        source_filename  = lv_source
        source_pc        = ' '
        target_filename  = lv_target
        target_pc        = ' '
      EXCEPTIONS
        file_read_error  = 1
        file_write_error = 2
        canceled_by_user = 3
        db_error         = 4
        no_authority     = 5
        OTHERS           = 6.
    IF sy-subrc <> 0.
      r_copy_error = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD delete_file.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->DELETE_FILE
* Program Title: Delete Application server File
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method deletes the application server File
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236  1 / Initial version
* 11/28/2017  X51385       D10K915916   2 / Exception Handling for
*                                           Authorization check
************************************************************************
    DATA: lo_exceptions TYPE REF TO cx_root.
    TRY .
        DELETE DATASET i_filename.
        IF sy-subrc NE 0.
          RAISE delete_error.
        ENDIF.
      CATCH cx_root INTO lo_exceptions.
        RAISE delete_error.
    ENDTRY.
  ENDMETHOD.


  METHOD fcat_from_internal_table.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->FCAT_FROM_INTERNAL_TABLE
* Program Title: Build fieldcatalog from internal table
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method returns the fieldcatalog from internal
* table data
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       E307         D10K900236
*                                      Initial version
************************************************************************
    DATA: lt_table TYPE REF TO data.
    CREATE DATA lt_table LIKE i_table.
    ASSIGN lt_table->* TO FIELD-SYMBOL(<fs_table>).
    TRY.
        cl_salv_table=>factory( IMPORTING
                                  r_salv_table   = DATA(v_ref_salv_table)
                                CHANGING
                                  t_table        = <fs_table>  ).
        rt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns      = v_ref_salv_table->get_columns( )      " ALV Filter
            r_aggregations = v_ref_salv_table->get_aggregations( ) " ALV Aggregations
    ).
      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD get_dir_files.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->GET_DIR_FILES
* Program Title: Get files directory
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method returns all the files available under the
* given directory for prsenatation/application server
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       1            D10K900236 / Initial version
* 09/26/2017  X51385       2            D10K911294 / Application server
*                                       directory files listing issue
************************************************************************
*------------------------------------------------------*
* Declaration for Constants
*------------------------------------------------------*
    CONSTANTS : lc_filemask TYPE c     VALUE '*',
                lc_cp       TYPE char2 VALUE 'CP',
                lc_filter   TYPE string VALUE '*.*'.
*------------------------------------------------------*
* Declaration for Variables
*------------------------------------------------------*
    DATA: lv_directory TYPE string,
          lv_epsf_dir  TYPE eps2filnam,
          lv_filemask  TYPE epsf-epsfilnam,
          lv_filename  TYPE string,
          lv_len       TYPE i,
          lv_cnt       TYPE i,
          lv_slash     TYPE char1,
          lv_slerr     TYPE char1.
*------------------------------------------------------*
* Declaration for Internal tables and ranges
*------------------------------------------------------*
    DATA: lw_file_table  TYPE file_info,
          lt_file_list   LIKE TABLE OF lw_file_table,
          r_file         TYPE RANGE OF string,
          lt_file_list_a TYPE STANDARD TABLE OF eps2fili.  " For Files at application server
*------------------------------------------------------*
* Declaration for Work areas
*------------------------------------------------------*
    DATA: lw_file        LIKE LINE OF r_file,
          lw_file_list_a TYPE         eps2fili.            " For Files at application server

    CLEAR: lv_directory,
           lv_epsf_dir,
           lv_filemask,
           lv_filename,
           lv_len,
           lv_cnt,
           lv_slash,
           lv_slerr,
           lw_file_table,
           lt_file_list,
           lt_file_list_a,
           lw_file_list_a,
           r_file.

*    IF i_source = gc_loc_server.
    lv_slash = gc_fslash.
    lv_slerr = gc_bslash.
*    ELSE.
*    lv_slash = gc_bslash.
*      lv_slerr = gc_fslash.
*    ENDIF.

    FIND lv_slerr IN i_directory .
    IF sy-subrc = 0.
      RAISE invalid_directory.
    ENDIF.
*******************************************
** Create range for Filter Files
*******************************************
    lw_file-sign = gc_include.
    lw_file-option = gc_equal.
    IF i_filemask = lc_filemask.
      lw_file-option = lc_cp.
    ENDIF.
    lw_file-low  = i_filemask.
    APPEND lw_file TO r_file.
*******************************************
** ADD "/" in the Begining of Server File
*******************************************
*    IF i_source = gc_app_server AND
*   i_directory+0(1) NE gc_bslash.
*      CONCATENATE gc_bslash i_directory INTO lv_directory.
*    ELSE.
    lv_directory = i_directory.
*    ENDIF.

    CASE i_source.
**--Application Server file
      WHEN gc_app_server.

        lv_epsf_dir = i_directory.
        lv_filemask = i_filemask.

        CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
          EXPORTING
            iv_dir_name            = lv_epsf_dir
            file_mask              = lv_filemask
          TABLES
            dir_list               = lt_file_list_a
          EXCEPTIONS
            invalid_eps_subdir     = 1
            sapgparam_failed       = 2
            build_directory_failed = 3
            no_authorization       = 4
            read_directory_failed  = 5
            too_many_read_errors   = 6
            empty_directory_list   = 7
            OTHERS                 = 8.
        IF sy-subrc <> 0.
          RAISE failed_directory_listing.
        ELSE.
********************************************************************
** Create Output List
********************************************************************
          LOOP AT lt_file_list_a INTO lw_file_list_a WHERE name IN r_file.

            lv_len = strlen( lv_directory ).
            lv_len = lv_len - 1.
            CLEAR lv_filename.
            IF lv_directory+lv_len(1) NE lv_slash .
              CONCATENATE  lv_directory  lv_slash
                           lw_file_list_a-name INTO lv_filename.
            ELSE.
              CONCATENATE  lv_directory  lw_file_list_a-name  INTO lv_filename.
            ENDIF.

            APPEND lv_filename TO r_list.
            CLEAR: lw_file_list_a.
          ENDLOOP.
        ENDIF.

      WHEN gc_loc_server.
**-- Local File - Presentation Server
        CALL METHOD cl_gui_frontend_services=>directory_list_files
          EXPORTING
            directory                   = lv_directory
            filter                      = lc_filter
            files_only                  = gc_char_x
          CHANGING
            file_table                  = lt_file_list
            count                       = lv_cnt
*--Begin of change X51385 09/26/2017
          EXCEPTIONS
            cntl_error                  = 1
            directory_list_files_failed = 2
            wrong_parameter             = 3
            error_no_gui                = 4
            not_supported_by_gui        = 5.
        IF sy-subrc <> 0.
          RAISE failed_directory_listing.
        ELSE.
********************************************************************
** Create Output List
********************************************************************
          LOOP AT lt_file_list INTO lw_file_table WHERE filename IN r_file.

            lv_len = strlen( lv_directory ).
            lv_len = lv_len - 1.
            CLEAR lv_filename.
            IF lv_directory+lv_len(1) NE lv_slash .
              CONCATENATE  lv_directory  lv_slash
                           lw_file_table-filename INTO lv_filename.
            ELSE.
              CONCATENATE  lv_directory  lw_file_table-filename  INTO lv_filename.
            ENDIF.

            APPEND lv_filename TO r_list.
            CLEAR: lw_file_table.
          ENDLOOP.
        ENDIF.
*--End of change X51385 09/26/2017
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.


  METHOD get_fpath.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->GET_FPATH
* Program Title: Get Archive/Error File path
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method returns the Error/Archive file path
*
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236  1 / Initial version
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Constant
*----------------------------------------------------------------------*
    CONSTANTS: lc_a    TYPE c      VALUE 'A',
               lc_e    TYPE c      VALUE 'E',
               lc_err  TYPE char3  VALUE 'Err',
               lc_uscr TYPE c      VALUE '_',
               lc_dot  TYPE c      VALUE '.'.
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA: lv_path  TYPE string,
          lv_dummy TYPE string,
          lv_spath TYPE string,
          lv_file  TYPE string,
          lv_len   TYPE i.

    CLEAR: lv_path,lv_dummy,lv_spath,lv_file,lv_len.

    IF i_type EQ lc_a.
      lv_spath = gc_archive_path.
    ELSEIF i_type EQ lc_e.
      lv_spath = gc_error_path.
    ENDIF.

    CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = i_file
      IMPORTING
        stripped_name = lv_file
        file_path     = lv_path
      EXCEPTIONS
        x_error       = 1
        OTHERS        = 2.

    IF sy-subrc = 0.

      IF i_type = lc_e.
        IF lv_file+0(3) = lc_err.
          SPLIT lv_file AT lc_uscr INTO lv_dummy lv_file.
        ENDIF.
        CONCATENATE lc_err sy-datum sy-timlo lc_uscr lv_file INTO lv_file.
      ELSEIF i_type = lc_a.
        SPLIT lv_file AT lc_dot INTO lv_file lv_dummy.
        IF sy-subrc = 0.
          CONCATENATE lv_file lc_uscr sy-datum sy-timlo lc_dot lv_dummy INTO lv_file.
          CONDENSE lv_file.
        ENDIF.
      ENDIF.

      e_fname = lv_file.
      "-- Read the character length of the directory
      lv_len = strlen( lv_path ).
      lv_len = lv_len - 1.
      IF lv_path+lv_len(1) NE gc_bslash .

        CONCATENATE  lv_path
                     gc_bslash
                     lv_spath
                     lv_file INTO e_fullpath.

        CONCATENATE  lv_path
                     gc_bslash
                     lv_spath INTO e_fpath.
      ELSE.
        CONCATENATE  lv_path lv_spath lv_file  INTO e_fullpath.
        CONCATENATE  lv_path lv_spath INTO e_fpath.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_logical_path.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->GET_LOGICAL_PATH
* Program Title: Get Logical File path
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method returns the logical file path
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA : lv_logic_path TYPE filepath-pathintern,
           lv_filename   TYPE string,
           lv_para       TYPE char3.

    CLEAR: lv_logic_path, lv_filename, lv_para.
******************************************************
** FM to get LOGICAL Path and Filename
******************************************************
    CALL FUNCTION 'FILE_GET_NAME_AND_LOGICAL_PATH'
      EXPORTING
        client                     = sy-mandt
        logical_filename           = i_filename
        operating_system           = sy-opsys
        parameter_1                = i_para1
        parameter_2                = i_para2
        parameter_3                = i_para3
      IMPORTING
        file_name                  = lv_filename
        logical_path               = lv_logic_path
      EXCEPTIONS
        file_not_found             = 1
        operating_system_not_found = 2
        file_system_not_found      = 3
        OTHERS                     = 4.
    IF sy-subrc <> 0.
      RAISE path_not_found.
    ENDIF.
******************************************************
** FM to get Physical Path and Filename
******************************************************
    IF lv_logic_path IS NOT INITIAL.
      CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
        EXPORTING
          client                     = sy-mandt
          logical_path               = lv_logic_path
          operating_system           = sy-opsys
          parameter_1                = lv_para
          file_name                  = lv_filename
        IMPORTING
          file_name_with_path        = e_filepath
        EXCEPTIONS
          path_not_found             = 1
          missing_parameter          = 2
          operating_system_not_found = 3
          file_system_not_found      = 4
          OTHERS                     = 5.
      IF sy-subrc <> 0.
        RAISE path_not_found.
      ENDIF.
    ELSE.
      e_filepath = lv_filename.
    ENDIF.
  ENDMETHOD.


  METHOD open_dir.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->OPEN_DIR
* Program Title: Open directory
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method provides F4 help to select a directory
* from prsentation/application server
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236  1 / Initial version
************************************************************************
**--Presentation Server
    IF i_source = gc_loc_server.

      CALL METHOD cl_gui_frontend_services=>directory_browse
        CHANGING
          selected_folder      = c_screen_dir
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF sy-subrc <> 0.
        RAISE open_dir_failed.
      ENDIF.
**--" Application server
    ELSEIF i_source = gc_app_server.
      CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
        EXPORTING
          directory        = i_directory
          filemask         = i_filemask
        IMPORTING
          serverfile       = c_screen_dir
        EXCEPTIONS
          canceled_by_user = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        RAISE open_dir_failed.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD read_dir_files.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->READ_DIR_FILES
* Program Title: Read file from directory
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method reads files from the directory and returns
* the content in an internal table
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
* 11/28/2017  X51385       D10K915916   2 / Exception Handling for
*                                           Authorization check
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
    CONSTANTS: lc_x        TYPE char1  VALUE 'X', " Yes
               lc_mandt    TYPE char5  VALUE 'MANDT',
               lc_del      TYPE char5  VALUE ',|T;',
               lc_a        TYPE char1  VALUE 'A',
               lc_p        TYPE char1  VALUE 'P',
               lc_t        TYPE char1  VALUE 'T',
               lc_bslash   TYPE char1  VALUE '/',
               lc_fslash   TYPE char1  VALUE '\',
               lc_i        TYPE char1  VALUE 'I',
               lc_eq       TYPE char2  VALUE 'EQ',
               lc_filter   TYPE string VALUE '*.*',
               lc_filemask TYPE c      VALUE '*',
               lc_cp       TYPE char2  VALUE 'CP'.
*----------------------------------------------------------------------*
* Declaration for Internal table and range
*----------------------------------------------------------------------*
    DATA: lt_file_list TYPE TABLE OF eps2fili,
          lt_split     TYPE TABLE OF char40,
          lt_string    TYPE TABLE OF string,
          r_file       TYPE RANGE OF string.
*----------------------------------------------------------------------*
* Declaration for Work area
*----------------------------------------------------------------------*
    DATA: lw_file       LIKE LINE OF r_file,
          lw_string     TYPE         string,
          lw_split      TYPE         char200,
          v_ref_line    TYPE REF TO  data,
          lo_exceptions TYPE REF TO cx_root,
          lw_flcat      TYPE LINE OF lvc_t_fcat,
          lw_file_list  TYPE         eps2fili.
*----------------------------------------------------------------------*
* Declaration for Field symbols
*----------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_data_tab> TYPE STANDARD TABLE,
                   <fs_input>    TYPE any, " Generic input file structure
                   <fs_fld>      TYPE any.
*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
    DATA: lv_filename TYPE string,
          lv_dir1     TYPE eps2filnam,
          lv_file1    TYPE epsf-epsfilnam,
          lv_len1     TYPE i,
          lv_dir      TYPE string,
          lv_file     TYPE string,
          lv_len      TYPE i,
          lv_cnt      TYPE i,
          lv_tabix    TYPE sy-tabix.

    ASSIGN: e_datatab TO <fs_data_tab>.
*--Delimeter is not found
    IF i_delimiter CA  lc_del.
    ELSE.
      RAISE invalid_delimiter.
    ENDIF.
*--Add tab char util
    IF i_delimiter = lc_t.
      i_delimiter = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.
*--Check the source
    IF i_source = lc_a OR i_source = lc_p."Source should be presentation or application server
    ELSE.
      RAISE invalid_source.
    ENDIF.
* Creating dynamic work area for input file structure.
    CALL METHOD me->fcat_from_internal_table
      EXPORTING
        i_table = e_datatab
      RECEIVING
        rt_fcat = DATA(it_fcat).
    DESCRIBE TABLE it_fcat LINES DATA(lv_lines).

*--Add file name to range table.
    lw_file-sign = lc_i.
    lw_file-option = lc_eq.
    IF i_filename = lc_filemask.
      lw_file-option = lc_cp.
    ENDIF.
    lw_file-low  = i_filename.
    APPEND lw_file TO r_file.
*--Create object
    CREATE DATA v_ref_line LIKE LINE OF <fs_data_tab>.
    ASSIGN v_ref_line->* TO <fs_input>.
    IF i_source = lc_a.
      IF i_directory+0(1) NE lc_bslash.
        CONCATENATE lc_bslash i_directory INTO lv_dir.
      ELSE.
        lv_dir = i_directory.
      ENDIF.
      FIND lc_fslash IN i_directory .
      IF sy-subrc = 0.
        RAISE invalid_directory.
      ENDIF.
* Call the funtion module to get list of files in the directory
      lv_dir1 = i_directory.
      lv_file1 = i_filename.
      CALL FUNCTION 'EPS2_GET_DIRECTORY_LISTING'
        EXPORTING
          iv_dir_name            = lv_dir1
          file_mask              = lv_file1
        TABLES
          dir_list               = lt_file_list
        EXCEPTIONS
          invalid_eps_subdir     = 1
          sapgparam_failed       = 2
          build_directory_failed = 3
          no_authorization       = 4
          read_directory_failed  = 5
          too_many_read_errors   = 6
          empty_directory_list   = 7
          OTHERS                 = 8.
      IF sy-subrc <> 0.
        RAISE dir_listing_not_found.
      ENDIF.
      TRY .

* Loop all the files in the directory
          LOOP AT lt_file_list INTO lw_file_list WHERE name IN r_file.
            CLEAR: lv_filename,
                   lv_len.
*Read the character length of the directory
            lv_len = strlen( lv_dir ).
            lv_len = lv_len - 1.
*Check last chararcter is directory seprator ie /
            CLEAR lv_filename.
            IF lv_dir+lv_len(1) NE lc_bslash .
              CONCATENATE  lv_dir  lc_bslash
                           lw_file_list-name INTO lv_filename.
            ELSE.
              CONCATENATE  lv_dir lw_file_list-name  INTO lv_filename.
            ENDIF. " lw_file_list-dirname+lv_len(1) NE i_delimeter .

            IF i_codepage IS NOT INITIAL.
              OPEN DATASET lv_filename FOR INPUT IN LEGACY TEXT MODE
                CODE PAGE i_codepage.
            ELSE.
              OPEN DATASET lv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.
            ENDIF.
* Check file exits
            CHECK sy-subrc = 0.
*--Append the contents to file table
            APPEND lv_filename TO gt_files.
            DO.
              CLEAR: <fs_data_tab>.
* read the record from file and move it to string type variable
              READ DATASET lv_filename INTO  lv_file.
              IF sy-subrc NE 0.
                EXIT.
              ELSE.
*--Dont include header
                IF i_hdr = lc_x AND sy-index = 1.
                ELSE.
                  APPEND lv_file TO lt_string.
                ENDIF.
              ENDIF. " SY-SUBRC NE 0.
            ENDDO. "DO statement
            IF i_delimiter = space.
              LOOP AT lt_string INTO lw_string.
                SPLIT lw_string  AT i_delimiter INTO TABLE lt_split.
                LOOP AT it_fcat INTO lw_flcat.
*--Field name
                  ASSIGN COMPONENT lw_flcat-fieldname OF STRUCTURE <fs_input> TO <fs_fld>.
                  IF sy-subrc = 0 AND lw_flcat-fieldname = gc_fld_fname.
                    <fs_fld>     = lw_file_list-name.
                  ELSEIF sy-subrc = 0 AND lw_flcat-fieldname = gc_fld_rowno.
                    <fs_fld>     = sy-tabix.
                  ELSE.
                    <fs_fld>     = lw_string.
                    lv_len1 = lw_flcat-intlen.
                    SHIFT lw_string BY lv_len1 PLACES.
                  ENDIF.
                ENDLOOP.
                APPEND <fs_input> TO e_datatab.
              ENDLOOP.
            ELSE.
              LOOP AT lt_string INTO lw_string.
                CLEAR: lv_tabix.
                SPLIT lw_string  AT i_delimiter INTO TABLE lt_split.
*--Field name
                ASSIGN COMPONENT gc_fld_fname OF STRUCTURE <fs_input> TO <fs_fld>.
                IF sy-subrc = 0.
                  lv_tabix  = lv_tabix + 1.
                  <fs_fld>     = lw_file_list-name.
                ENDIF.
*--Row Number
                ASSIGN COMPONENT gc_fld_rowno OF STRUCTURE <fs_input> TO <fs_fld>.
                IF sy-subrc = 0.
                  lv_tabix  = lv_tabix + 1.
                  <fs_fld>     = sy-tabix.
                ENDIF.

                LOOP AT lt_split INTO lw_split.
                  lv_tabix = lv_tabix + 1.
                  ASSIGN COMPONENT lv_tabix OF STRUCTURE <fs_input> TO <fs_fld>.
                  IF sy-subrc EQ 0.
                    <fs_fld> = lw_split.
                  ENDIF.
                ENDLOOP.
                APPEND <fs_input> TO e_datatab.
              ENDLOOP.
            ENDIF.
          ENDLOOP.
        CATCH cx_root INTO lo_exceptions.
          RAISE error_in_read.
      ENDTRY.
*--Unassign the data
      UNASSIGN: <fs_data_tab>,
                <fs_input>.
    ELSEIF i_source = lc_p.
      FIND lc_bslash IN i_directory .
      IF sy-subrc = 0.
        RAISE invalid_directory.
      ENDIF.
      lv_dir = i_directory.
      CALL METHOD cl_gui_frontend_services=>directory_list_files
        EXPORTING
          directory  = lv_dir
          filter     = lc_filter
          files_only = gc_char_x
        CHANGING
          file_table = lt_file_list
          count      = lv_cnt.
      LOOP AT lt_file_list INTO lw_file_list WHERE name IN r_file.
        CLEAR: lv_filename,
               lv_len.
*Read the character length of the directory
        lv_len = strlen( lv_dir ).
        lv_len = lv_len - 1.
*Check last chararcter is directory seprator ie \
        CLEAR lv_filename.
        IF lv_dir+lv_len(1) NE lc_fslash .

          CONCATENATE  lv_dir  lc_fslash
                       lw_file_list-name INTO lv_filename.
        ELSE.
          CONCATENATE  lv_dir lw_file_list-name  INTO lv_filename.
        ENDIF. " lw_file_list-dirname+lv_len(1) NE i_delimeter .
*--Upload using GUI
        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = lv_filename
            codepage                = i_codepage
          CHANGING
            data_tab                = lt_string
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.
        IF sy-subrc <> 0.
          RAISE error_in_read.
        ELSE.
          IF i_hdr = lc_x.
            DELETE lt_string INDEX 1.
          ENDIF.
        ENDIF.
        IF i_delimiter = space.
          LOOP AT lt_string INTO lw_string.
            SPLIT lw_string  AT i_delimiter INTO TABLE lt_split.
            LOOP AT it_fcat INTO lw_flcat.
*--Field name
              ASSIGN COMPONENT lw_flcat-fieldname OF STRUCTURE <fs_input> TO <fs_fld>.
              IF sy-subrc = 0 AND lw_flcat-fieldname = gc_fld_fname.
                <fs_fld>     = lw_file_list-name.
              ELSEIF sy-subrc = 0 AND lw_flcat-fieldname = gc_fld_rowno.
                <fs_fld>     = sy-tabix.
              ELSE.
                <fs_fld>     = lw_string.
                lv_len1 = lw_flcat-intlen.
                SHIFT lw_string BY lv_len1 PLACES.
              ENDIF.
            ENDLOOP.
            APPEND <fs_input> TO e_datatab.
          ENDLOOP.
        ELSE.
          LOOP AT lt_string INTO lw_string.
            CLEAR: lv_tabix.
            SPLIT lw_string  AT i_delimiter INTO TABLE lt_split.
*--Field name
            ASSIGN COMPONENT gc_fld_fname OF STRUCTURE <fs_input> TO <fs_fld>.
            IF sy-subrc = 0.
              lv_tabix  = lv_tabix + 1.
              <fs_fld>     = lw_file_list-name.
            ENDIF.
*--Row Number
            ASSIGN COMPONENT gc_fld_rowno OF STRUCTURE <fs_input> TO <fs_fld>.
            IF sy-subrc = 0.
              lv_tabix  = lv_tabix + 1.
              <fs_fld>     = sy-tabix.
            ENDIF.

            LOOP AT lt_split INTO lw_split.
              lv_tabix = lv_tabix + 1.
              ASSIGN COMPONENT lv_tabix OF STRUCTURE <fs_input> TO <fs_fld>.
              IF sy-subrc EQ 0.
                <fs_fld> = lw_split.
              ENDIF.
            ENDLOOP.
            APPEND <fs_input> TO e_datatab.
          ENDLOOP.
        ENDIF.
      ENDLOOP.
*--Unassign the data
      UNASSIGN: <fs_data_tab>,
                <fs_input>.
    ENDIF.
  ENDMETHOD.


  METHOD read_file.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->READ_FILE
* Program Title: Read file from Presenation/Application server
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method reads file data from presenation/application
* server
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
* 11/28/2017  X51385       D10K915916   2 / Exception Handling for
*                                           Authorization check
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Constant
*----------------------------------------------------------------------*
    CONSTANTS : lc_t   TYPE c VALUE 'T'.
*----------------------------------------------------------------------*
* Declaration for Internal table
*----------------------------------------------------------------------*
    DATA: lt_split  TYPE TABLE OF char200,
          lt_string TYPE TABLE OF string.
*----------------------------------------------------------------------*
* Declaration for Work area
*----------------------------------------------------------------------*
    DATA: lw_string     TYPE          string,
          lw_split      TYPE          char200,
          v_ref_line    TYPE REF TO   data,
          lo_exceptions TYPE REF TO cx_root,
          lw_flcat      TYPE LINE OF  lvc_t_fcat.
*----------------------------------------------------------------------*
* Declaration for Field symbol
*----------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_data_tab> TYPE STANDARD TABLE,
                   <fs_input>    TYPE any, " Generic input file structure
                   <fs_fld>      TYPE any.
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
    DATA: lv_filename TYPE string,
          lv_len      TYPE i,
          lv_file     TYPE string,
          lv_tabix    TYPE sy-tabix,
          lv_delm     TYPE char1.
**--Refresh internal table and clears work area
    REFRESH: lt_split, lt_string.
    CLEAR:lw_string, lw_split, lw_flcat,
          lv_filename, lv_len, lv_file, lv_tabix, lv_delm.

    lv_delm = i_delimiter.
    lv_filename = i_filename.

**--Check Delimeter is valid
    IF lv_delm IS INITIAL OR
       lv_delm CA gc_delms.

    ELSE.
      RAISE invalid_delimeter.
    ENDIF.

**-- Use tab char util for excel----------------------------"
    IF lv_delm = lc_t.
      lv_delm = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.

**  Creating dynamic work area for input file structure----"
**  Create Object Reference

    CASE i_source.
**--Server File - Application Server
      WHEN gc_app_server.
        TRY.
            IF i_codepage IS NOT INITIAL.
              OPEN DATASET lv_filename FOR INPUT IN LEGACY
                             TEXT MODE CODE PAGE i_codepage .
            ELSE.
              OPEN DATASET lv_filename FOR INPUT IN TEXT MODE
                                             ENCODING DEFAULT." WITH SMART LINEFEED.
            ENDIF.

            IF sy-subrc <> 0.
              RAISE cannot_open_file.
            ENDIF.

            DO.
              "-- read the record from file and
              "-- move it to string type variable
              READ DATASET lv_filename INTO  lv_file .
              IF sy-subrc NE 0.
                EXIT.
              ELSE.
                APPEND lv_file TO lt_string.
              ENDIF.
            ENDDO.

            CLOSE DATASET  lv_filename.
          CATCH cx_root INTO lo_exceptions.
            RAISE cannot_open_file.
        ENDTRY.
************************************************************
** Local File - Presentation Server
************************************************************
      WHEN gc_loc_server.

        CALL METHOD cl_gui_frontend_services=>gui_upload
          EXPORTING
            filename                = lv_filename
            codepage                = i_codepage
          CHANGING
            data_tab                = lt_string
          EXCEPTIONS
            file_open_error         = 1
            file_read_error         = 2
            no_batch                = 3
            gui_refuse_filetransfer = 4
            invalid_type            = 5
            no_authority            = 6
            unknown_error           = 7
            bad_data_format         = 8
            header_not_allowed      = 9
            separator_not_allowed   = 10
            header_too_long         = 11
            unknown_dp_error        = 12
            access_denied           = 13
            dp_out_of_memory        = 14
            disk_full               = 15
            dp_timeout              = 16
            not_supported_by_gui    = 17
            error_no_gui            = 18
            OTHERS                  = 19.
        IF sy-subrc <> 0.
          RAISE error_in_read.
        ENDIF.

      WHEN OTHERS.
        RAISE invalid_source.
    ENDCASE.
    IF i_hdr = gc_char_x.
      DELETE lt_string INDEX 1.
    ENDIF.
*********************************************************
**  Create Output structure and Fill dynamically
*********************************************************

    ASSIGN: e_datatab TO <fs_data_tab>.
    "--> Get Field Catalog
    CALL METHOD me->fcat_from_internal_table
      EXPORTING
        i_table = e_datatab
      RECEIVING
        rt_fcat = DATA(it_fcat).
    DESCRIBE TABLE it_fcat LINES DATA(lv_lines).

    CREATE DATA v_ref_line LIKE LINE OF <fs_data_tab>.
    ASSIGN v_ref_line->* TO <fs_input>.

    "--> Write File Data to Internal Table
    LOOP AT lt_string INTO lw_string.
      CLEAR lv_tabix.
      "---------------------------------------------------------------"
      " ** If File name Column exsists assign File Name
      "---------------------------------------------------------------"
      ASSIGN COMPONENT gc_fld_fname OF STRUCTURE <fs_input> TO <fs_fld>.
      IF sy-subrc = 0.
        lv_tabix  = lv_tabix + 1.
        <fs_fld>  = i_filename.
      ENDIF.
      "---------------------------------------------------------------"
      " ** If Row  Column exsists assign Row
      "---------------------------------------------------------------"
      ASSIGN COMPONENT gc_fld_rowno OF STRUCTURE <fs_input> TO <fs_fld>.
      IF sy-subrc = 0.
        lv_tabix  = lv_tabix + 1.
        <fs_fld>     = sy-tabix.
      ENDIF.
      "---------------------------------------------------------------"
      " ** Assign Value to Column Dynamically
      "---------------------------------------------------------------"
      IF lv_delm IS NOT INITIAL.
        SPLIT lw_string  AT lv_delm INTO TABLE lt_split.
        LOOP AT lt_split INTO lw_split.
          lv_tabix = lv_tabix + 1.
          ASSIGN COMPONENT lv_tabix OF STRUCTURE
                          <fs_input> TO <fs_fld>.
          IF sy-subrc EQ 0.
            <fs_fld> = lw_split.
          ENDIF.
        ENDLOOP.

        "---------------------------------------------------------------"
        " ** String Table
        "---------------------------------------------------------------"
      ELSEIF lv_lines = 1..
        <fs_fld>     = lw_string.
        "---------------------------------------------------------------"
        " Fixed Length File
        "---------------------------------------------------------------"
      ELSE.
        LOOP AT it_fcat INTO lw_flcat WHERE fieldname NE gc_fld_fname OR
                                            fieldname NE gc_fld_rowno.

          ASSIGN COMPONENT lw_flcat-fieldname OF STRUCTURE <fs_input> TO <fs_fld>.
          <fs_fld>  = lw_string.
          lv_len   = lw_flcat-intlen.
          SHIFT lw_string BY lv_len PLACES.
        ENDLOOP.
      ENDIF.
      "-> Append data to table
      APPEND <fs_input> TO e_datatab.
      CLEAR <fs_input>.
    ENDLOOP.
    UNASSIGN: <fs_data_tab>, <fs_input>.

  ENDMETHOD.


  METHOD select_file.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->SELECT_FILE
* Program Title: F4 help for File
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method provides F4 help for selecting file
* path from prsentation/application server
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
************************************************************************
*------------------------------------------------------*
* Declaration for Variables
*------------------------------------------------------*
    DATA : lv_appl    TYPE as4flag,
           lv_pre     TYPE as4flag,
           lv_al11    TYPE as4flag,
           lv_logical TYPE as4flag,
           lv_msg1    TYPE msgv1.
********************************************************
** A-APP Server , P-Presentation Server
********************************************************
    IF i_source = gc_app_server.
      lv_appl = gc_char_x.
    ENDIF.
********************************************************
** A-AL11 , L-Logical Server
********************************************************
    CASE i_apppath_type.
      WHEN gc_app_server.
        lv_al11 = gc_char_x.
      WHEN gc_lpath.
        lv_logical = gc_char_x.
    ENDCASE.

    IF lv_al11 IS INITIAL AND lv_logical IS INITIAL.
      lv_al11 = gc_char_x.
      lv_logical = gc_char_x.
    ENDIF.
********************************************************
** Call method for F4 help
********************************************************
    CALL METHOD cl_rsan_ut_files=>f4
      EXPORTING
        i_applserv              = lv_appl
        i_title                 = i_title
        i_applserv_logical      = lv_logical
        i_applserv_al11         = lv_al11
        i_gui_extension         = i_gui_extension
        i_gui_ext_filter        = i_gui_ext_filter
        i_gui_initial_directory = i_gui_initial_directory
      CHANGING
        c_file_name             = e_filename
      EXCEPTIONS
        failed                  = 1
        OTHERS                  = 2.
    IF sy-subrc <> 0.
** Raise exception
      RAISE f4_help_failed.
    ENDIF.
  ENDMETHOD.


  METHOD write_file.
************************************************************************
* Program ID   : ZCL_CA_UTILITY_FILE->WRITE_FILE
* Program Title: Write file to Presenation/Application server
* Created By   : Atique Maroof
* Creation Date: 11/21/2016
* RICEFW ID    : E307
* Description  : This method Writes the data to Presentation/
* Application server
*
* Additional Information:
*
************************************************************************
* Modification History
************************************************************************
* Date        User ID        REQ#        Transport# / Description
* ----------  ------------ ----------  ------------------------
* 11/21/2016  X51385       D10K900236   1 / Initial version
* 11/28/2017  X51385       D10K915916   2 / Exception Handling for
*                                           Authorization check
************************************************************************
*----------------------------------------------------------------------*
* Declaration for Constants
*----------------------------------------------------------------------*
    CONSTANTS: lc_t        TYPE char1  VALUE 'T'.
*----------------------------------------------------------------------*
* Declaration for Table and Range
*----------------------------------------------------------------------*
    DATA:
      lt_string TYPE TABLE OF string,
      r_file    TYPE RANGE OF string,
      lt_fcat   TYPE lvc_t_fcat.
*----------------------------------------------------------------------*
* Declaration for Work area
*----------------------------------------------------------------------*
    DATA:
      lw_string     TYPE          string,
      v_ref_line    TYPE REF TO   data,
      lo_exceptions TYPE REF TO cx_root.
*----------------------------------------------------------------------*
* Declaration for Field symbols
*----------------------------------------------------------------------*
    FIELD-SYMBOLS: <fs_data_tab> TYPE STANDARD TABLE,
                   <fs_input>    TYPE any, " Generic input file structure
                   <fs_fld>      TYPE any.
*----------------------------------------------------------------------*
* Declaration for variables
*----------------------------------------------------------------------*
    DATA: lv_filename TYPE string,
          lv_fname    TYPE salfile-longname,
          lv_len1     TYPE i,
          lv_dir      TYPE string,
          lv_file     TYPE string,
          lv_len      TYPE i,
          lv_cnt      TYPE i,
          lv_tabix    TYPE sy-tabix,
          lv_string   TYPE string,
          lv_temp     TYPE string,
          lv_delm     TYPE char1.

    lv_delm = i_delimiter.
    lv_filename = i_filename.
**---------------------------------------------------------------*
**-- Check Delimeter is valid
**-- Use tab char util for excel
**---------------------------------------------------------------*
    IF lv_delm IS INITIAL OR
       lv_delm CA  gc_delms.

    ELSE.
      RAISE invalid_delimiter.
    ENDIF.

    IF lv_delm = lc_t.
      lv_delm = cl_abap_char_utilities=>horizontal_tab.
    ENDIF.

**---------------------------------------------------------------*
** Get internal table into String table with Separator
**---------------------------------------------------------------*

    CALL METHOD me->fcat_from_internal_table
      EXPORTING
        i_table = i_datatab
      RECEIVING
        rt_fcat = lt_fcat.

*--Conver parametrs to string
    CALL METHOD me->convert_table_to_string
      EXPORTING
        i_delimiter = lv_delm
        i_datatab   = i_datatab
        i_fcat      = lt_fcat
      IMPORTING
        e_datatab   = lt_string.


    IF i_addheader = gc_char_x.

      LOOP AT i_fields ASSIGNING <fs_input>.
        IF sy-tabix = 1.
          MOVE <fs_input> TO lv_string.
          CONTINUE.
        ENDIF.
        CLEAR lv_temp.
        MOVE <fs_input> TO lv_temp.
        CONCATENATE lv_string lv_temp
               INTO lv_string SEPARATED BY lv_delm.
      ENDLOOP.

      INSERT lv_string  INTO lt_string INDEX 1.
    ENDIF.
**---------------------------------------------------------------*
**            File Dowanload
**---------------------------------------------------------------*
    CASE i_source.
        "********************************************************"
        " Server File - Application Server
        "********************************************************"
      WHEN gc_app_server.
        TRY.
*--Download the file to app server
            OPEN DATASET lv_filename FOR OUTPUT IN TEXT MODE ENCODING  DEFAULT.
            IF sy-subrc <> 0.
              RAISE write_error.
            ENDIF.
            LOOP AT lt_string INTO lw_string.
              TRANSFER lw_string TO lv_filename.
            ENDLOOP.
            CLOSE DATASET lv_filename.

            IF sy-subrc <> 0.
              RAISE write_error.
            ENDIF.
          CATCH cx_root INTO lo_exceptions.
            RAISE write_error.
        ENDTRY.
        "********************************************************"
        " Local File - Presentation Server
        "********************************************************"
      WHEN gc_loc_server.

        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            filename                = lv_filename
            write_field_separator   = gc_char_x
          CHANGING
            data_tab                = lt_string
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            not_supported_by_gui    = 22
            error_no_gui            = 23
            OTHERS                  = 24.
        IF sy-subrc <> 0.
          RAISE write_error.
        ENDIF.
      WHEN OTHERS.
        RAISE invalid_delimiter.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
