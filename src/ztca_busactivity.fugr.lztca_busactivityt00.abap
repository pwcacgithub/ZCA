*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.12.2017 at 05:55:13
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_BUSACTIVITY................................*
DATA:  BEGIN OF STATUS_ZTCA_BUSACTIVITY              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_BUSACTIVITY              .
CONTROLS: TCTRL_ZTCA_BUSACTIVITY
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCA_BUSACTIVITY              .
TABLES: ZTCA_BUSACTIVITY               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
