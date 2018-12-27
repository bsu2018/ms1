       SUBROUTINE APFSEE ( II , R , INDEX ,DATAR , KINDEX )
CXXX****KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)*************
C*                                                                     *
C*  O„POƒPAMMA :  APFSEE ( II , R , INDEX ,DATAR , KINDEX )          *
C*  ------------                                                       *
C*                                                                     *
C*  HA‡HA—EHˆE :    OPE„E‹EHˆE —ˆC‹EHHOƒO ‡HA—EHˆŸ ˆ„EHTˆ”ˆKATOPA.    *
C*  ----------                                                         *
C*                                                                     *
C*  METO„ :         OˆCK COOTBETCTBˆŸ B TA‹ˆ–E ˆMEH ˆ„EHTˆ”ˆKATOPOB  *
C*  -----           C OMO™œž OC‹E„OBATE‹œHOƒO EPEOPA.              *
C*                                                                     *
C*  APAMETP› :     II - ˆMŸ ˆ„EHTˆ”ˆKATOPA .                          *
C*  ---------       R - ‡HA—EHˆE ˆ„EHTˆ”ˆKATOPA .                      *
C*                  INDEX - MACCˆB ˆMEH ˆ„EHTˆ”ˆKATOPOB .              *
C*                  DATAR - MACCˆB —ˆC‹EHH›X ‡HA—EHˆ‰ ˆ„EHTˆ”ˆKATOPOB .*
C*                  KINDEX - PA‡MEPHOCTœ  INDEX ˆ DATAR .              *
C*                                                                     *
C*  O„POƒPAMMA :  HET , ( SUBERR(1) , COMMON : PRINTN )              *
C*  ------------                                                       *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION INDEX(1) , DATAR(1)
C!!!-------------------------------------------------------------------
       COMMON / PRINTN / NW6
C!!!-------------------------------------------------------------------
       DO 1 J=1,KINDEX
       IF ( II .NE. INDEX(J) ) GO TO 1
       R = DATAR(J)
       RETURN
 1     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NW6,10)
 10    FORMAT(1X,'***** CO‰ B APFSEE.‡HA—EHˆE ˆ„EHT. HE HA‰„EHO.')
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
       RETURN
       END
