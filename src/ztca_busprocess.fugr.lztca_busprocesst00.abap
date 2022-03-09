*---------------------------------------------------------------------*
*    view related data declarations
*   generation date: 20.12.2017 at 05:52:00
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
*...processing: ZTCA_BUSPROCESS.................................*
DATA:  BEGIN OF STATUS_ZTCA_BUSPROCESS               .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZTCA_BUSPROCESS               .
CONTROLS: TCTRL_ZTCA_BUSPROCESS
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZTCA_BUSPROCESS               .
TABLES: ZTCA_BUSPROCESS                .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
