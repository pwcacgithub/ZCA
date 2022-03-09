*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.12.2017 at 05:58:27
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_INTF_TRACK.................................*
DATA:  BEGIN OF STATUS_ZTCA_INTF_TRACK               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_INTF_TRACK               .
CONTROLS: TCTRL_ZTCA_INTF_TRACK
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCA_INTF_TRACK               .
TABLES: ZTCA_INTF_TRACK                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
