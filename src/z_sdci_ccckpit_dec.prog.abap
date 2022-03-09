*&---------------------------------------------------------------------*
*& Include          Z_SDCI_CCCKPIT_DEC
*&---------------------------------------------------------------------*

  TYPE-POOLS:truxs.

  TYPES : BEGIN OF ty_file,
            user_name  TYPE bapibname-bapibname,
            name_first TYPE ad_namefir,
            name_last  TYPE ad_namelas,
            smtp_addr  TYPE ad_smtpadr,
            function   TYPE ad_fnctn,
            department TYPE ad_dprtmnt,
            ustyp      TYPE xuustyp,
            password   TYPE xuncode,
            user_grp   TYPE xuclass,
            gltgv      TYPE char10,   "xugltgv,      " Valid from
            gltgb      TYPE char10,   "xugltgb,      " Valid to
            agr_name   TYPE agr_name,
            flag       TYPE char1,
          END OF ty_file,

          BEGIN OF ty_file_n,
            user_name TYPE bapibname-bapibname,
          END OF ty_file_n,

          BEGIN OF ty_file_p,
            user_name TYPE bapibname-bapibname,
            password  TYPE xuncode,
          END OF ty_file_p.

  TYPES : BEGIN OF ty_agr_define_db,
            agr_name TYPE agr_name,
          END OF ty_agr_define_db.

  TYPES : BEGIN OF ty_file_pfcg,
            agr_name  TYPE agr_name,
            role_text TYPE agr_title,
            action    TYPE char10, " Action needed
            tcode     TYPE tcode,  " Tcode
          END OF ty_file_pfcg.

  TYPES : BEGIN OF ty_text,
            agr_name TYPE agr_name,
            text     TYPE agr_title,
            action   TYPE char10,         " Action needed
            tcode    TYPE tcode,          " Tcode
          END OF ty_text.

  TYPES : BEGIN OF ty_intern,
            row   TYPE  kcd_ex_row_n,
            col   TYPE  kcd_ex_col_n,
            value TYPE  char80,
          END OF ty_intern.

  DATA: BEGIN OF wa_header,
          name TYPE c LENGTH 30,
        END OF wa_header.



  DATA: it_raw                TYPE truxs_t_text_data,
        lt_file               TYPE STANDARD TABLE OF ty_file,
        lt_usgrp              TYPE STANDARD TABLE OF usgrp,
        ls_usgrp              LIKE LINE OF lt_usgrp,
        lt_text               TYPE STANDARD TABLE OF ty_text,
        ls_text               LIKE LINE OF lt_text,
        t_intern              TYPE STANDARD TABLE OF alsmex_tabline,
        t_table               TYPE TABLE OF ty_file, " WITH HEADER LINE,
        t_table_pfcg          TYPE STANDARD TABLE OF ty_file_pfcg WITH HEADER LINE,
        lt_table              TYPE STANDARD TABLE OF ty_file,
        lt_table_n            TYPE TABLE OF ty_file_n,
        lt_table_p            TYPE TABLE OF ty_file_p,
        lt_return_pfcg_create TYPE STANDARD TABLE OF bapiret2,
        lt_messages           TYPE sprot_u_tab,
        ls_return_pfcg_create LIKE LINE OF lt_return_pfcg_create,
        lt_table_pfcg         TYPE STANDARD TABLE OF ty_file_pfcg,
        lt_table_create       TYPE STANDARD TABLE OF ty_file_pfcg,
        lt_table_create_tmp   TYPE STANDARD TABLE OF ty_file_pfcg,
        lt_table_change       TYPE STANDARD TABLE OF ty_file_pfcg,
        lt_table_change_tmp   TYPE STANDARD TABLE OF ty_file_pfcg,
        lt_agr_define_db      TYPE STANDARD TABLE OF ty_agr_define_db,
        ls_table_create       LIKE LINE OF lt_table_create,
        ls_table_create_tmp   LIKE LINE OF lt_table_create_tmp,
        ls_table_change       LIKE LINE OF lt_table_change,
        ls_table_change_tmp   LIKE LINE OF lt_table_change_tmp,
        ls_agr_define_db      LIKE LINE OF lt_agr_define_db,
        lw_table_pfcg         LIKE LINE OF lt_table_pfcg,
        ls_table              LIKE LINE OF lt_table,
        ls_table_pfcg         LIKE LINE OF t_table_pfcg,
        lw_table              LIKE LINE OF t_table,
        lv_profile_name       TYPE xuprofname,
        lt_template           TYPE tprpattern,
        it_gen_prof           TYPE STANDARD TABLE OF agr_st_name,
        st_gen_prof           TYPE agr_st_name,
        lt_tcodes             TYPE STANDARD TABLE OF ssm_tcodes,
        ls_tcodes             LIKE LINE OF lt_tcodes,
        lv_profile_text       TYPE xutext,
        gt_file               TYPE TABLE OF ty_file, " WITH HEADER LINE..
        gt_file_n             TYPE TABLE OF ty_file_n,
        gt_file_p             TYPE TABLE OF ty_file_p.

  DATA: lv_user           TYPE  bapibname-bapibname,
        lv_string         TYPE string,
        lv_str1           TYPE string,
        lv_str2           TYPE string,
        lv_str3           TYPE string,
        lv_char1          TYPE char2,
        lv_char2          TYPE char2,
        lt_logondata      TYPE bapilogond,
        lt_password       TYPE bapipwd,
        lt_address        TYPE bapiaddr3,
        popup_return      TYPE string,
        lv_strg           TYPE string,
        lt_activity       TYPE STANDARD TABLE OF bapiagr,
        ls_activity       LIKE LINE OF lt_activity,
        lt_return_role    TYPE STANDARD TABLE OF bapiret2,
        ls_return_role    LIKE LINE OF lt_return_role,
        lt_parameter      TYPE STANDARD TABLE OF bapiparam,
        lt_return         TYPE STANDARD TABLE OF bapiret2,
        ls_return         LIKE LINE OF lt_return,
        lt_return_delete  TYPE STANDARD TABLE OF bapiret2,
        ls_return_delete  LIKE LINE OF lt_return_delete,
        ls_logondata_chg  TYPE bapilogond,
        ls_logondatax_chg TYPE bapilogonx,
        ls_address_chg    TYPE bapiaddr3,
        ls_addressx_chg   TYPE bapiaddr3x,
        ls_password_chg   TYPE bapipwd,
        ls_passwordx_chg  TYPE bapipwdx,
        lt_return_chg     TYPE STANDARD TABLE OF bapiret2,
        ls_return_chg     LIKE LINE OF lt_return_chg,
        lt_log_lock       TYPE bapilogond,
        lt_log_lockx      TYPE bapilogonx,
        lv_date           TYPE datum,
        lv_flag           TYPE boolean,
        lt_return_lock    TYPE STANDARD TABLE OF bapiret2,
        lt_return_unlock  TYPE STANDARD TABLE OF bapiret2,
        ls_return_lock    LIKE LINE OF lt_return_lock,
        ls_return_unlock  LIKE LINE OF lt_return_unlock,
        t_header          LIKE TABLE OF wa_header,
        file              TYPE string,
        ld_filename       TYPE string,
        ld_path           TYPE string,
        ld_fullpath       TYPE string,
        v_index           TYPE i,
        ld_result         TYPE i,
        gd_file           TYPE c.

  FIELD-SYMBOLS: <fs_intern> LIKE LINE OF t_intern,
                 <fs_aux>.

  DATA: gv_source TYPE char1,
        gv_del    TYPE char1,
        gv_header TYPE char1.

  DATA: go_write_file TYPE REF TO zcl_ca_utility_adsk.

  TYPES: BEGIN OF ty_c_user,
           user_name TYPE bapibname-bapibname,
           agr_name  TYPE agr_name,
           flag      TYPE char1,
         END OF ty_c_user,

         BEGIN OF ty_message,
           user    TYPE bapibname-bapibname,
           message TYPE bapi_msg,
         END OF ty_message.

  DATA: gt_c_user        TYPE STANDARD TABLE OF ty_c_user,
        gw_c_user        TYPE ty_c_user,
        gt_message       TYPE TABLE OF ty_message,
        gw_message       TYPE ty_message,
        lt_activitygroup TYPE TABLE OF bapiagr,
        lt_return_usrdet TYPE TABLE OF bapiret2.
