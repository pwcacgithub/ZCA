*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_ZTCA_INTF_TRACK
*   generation date: 20.12.2017 at 05:58:27
*   view maintenance generator version: #001407#
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_ZTCA_INTF_TRACK    .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
