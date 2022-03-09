* regenerated at 20.12.2017 05:59:56
FUNCTION-POOL ZTCA_PARAM                 MESSAGE-ID SV.

* INCLUDE LZTCA_PARAMD...                    " Local class definition
  INCLUDE LSVIMDAT                                . "general data decl.
  INCLUDE LZTCA_PARAMT00                          . "view rel. data dcl.


*----------------------------------------------------------------------*
* Declaration for Structures
*----------------------------------------------------------------------*
TYPES: BEGIN OF ty_busproc,
         sign TYPE tvarv_sign,
         opti TYPE tvarv_opti,
         low  TYPE ztca_param-busproc,
         high TYPE ztca_param-busproc,
       END OF ty_busproc,
       BEGIN OF ty_busact,
         sign TYPE tvarv_sign,
         opti TYPE tvarv_opti,
         low  TYPE ztca_param-busact,
         high TYPE ztca_param-busact,
       END OF ty_busact,
       BEGIN OF ty_orgauth,
         sign TYPE tvarv_sign,
         opti TYPE tvarv_opti,
         low  TYPE ztca_param-orgauth,
         high TYPE ztca_param-orgauth,
       END OF ty_orgauth,
       BEGIN OF ty_authc,
         sign TYPE tvarv_sign,
         opti TYPE tvarv_opti,
         low  TYPE c,
         high TYPE c,
       END OF ty_authc.

*----------------------------------------------------------------------*
* Declaration for Constant
*----------------------------------------------------------------------*
CONSTANTS:gc_zca_param TYPE ust12-objct     VALUE 'ZCA_PARAM',
          gc_i         TYPE tvarv_sign      VALUE 'I',
          gc_e         TYPE c               VALUE 'E',
          gc_x         TYPE c               VALUE 'X',
          gc_s         TYPE c               VALUE 'S',
          gc_c         TYPE c               VALUE 'C',
          gc_r         TYPE c               VALUE 'R',
          gc_u         TYPE c               VALUE 'U',
          gc_d         TYPE c               VALUE 'D',
          gc_n         TYPE c               VALUE 'N',
          gc_eq        TYPE tvarv_opti      VALUE 'EQ',
          gc_busproc   TYPE xufield         VALUE 'Z_BUSPROC',
          gc_busact    TYPE xufield         VALUE 'Z_BUSACT',
          gc_authc     TYPE xufield         VALUE 'AUTHC',
          gc_orgauth   TYPE xufield         VALUE 'Z_ORGAUTH',
          gc_newl      TYPE sy-ucomm        VALUE 'NEWL',
          gc_deta      TYPE sy-ucomm        VALUE 'DETA',
          gc_detm      TYPE sy-ucomm        VALUE 'DETM',
          gc_aend      TYPE sy-ucomm        VALUE 'AEND',
          gc_kope      TYPE sy-ucomm        VALUE 'KOPE',
          gc_kopf      TYPE sy-ucomm        VALUE 'KOPF',
          gc_save      TYPE sy-ucomm        VALUE 'SAVE',
          gc_value_org TYPE ddbool_d        VALUE 'S'.
*----------------------------------------------------------------------*
* Declaration for Internal table
*----------------------------------------------------------------------*
DATA: gt_auth_value TYPE TABLE OF usvalues,
      gt_busproc    TYPE TABLE OF ty_busproc,
      gt_busact     TYPE TABLE OF ty_busact,
      gt_orgauth    TYPE TABLE OF ty_orgauth,
      gt_authc      TYPE TABLE OF ty_authc.
*----------------------------------------------------------------------*
* Declaration for Variable
*----------------------------------------------------------------------*
DATA : gv_copy TYPE c.  " To Check the authorization for Copy opeartion
