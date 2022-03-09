*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.12.2017 at 05:59:56
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_PARAM......................................*
DATA:  BEGIN OF STATUS_ZTCA_PARAM                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_PARAM                    .
CONTROLS: TCTRL_ZTCA_PARAM
            TYPE TABLEVIEW USING SCREEN '0005'.
*.........table declarations:.................................*
TABLES: *ZTCA_PARAM                    .
TABLES: ZTCA_PARAM                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
