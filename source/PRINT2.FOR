         SUBROUTINE PRINT2( KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C    O„POƒPAMMA:  PRINT2( KOD )
C   -------------
C    HA‡HA—EHˆE: KOHTPO‹œHAŸ E—ATœ MACCˆBOB COƒ‹ACHO ‡APOCA KOD;
C    ----------
C   APAMETP›:     KOD = 1 E—ATœ MASPRT B Bˆ„E 16-ƒO „AMA ˆ
C   ----------             B Bˆ„E "BE™ECTBEHH›X" C‹OB REAL*4;
C                  KOD = 2 E—ATœ MASTAB B Bˆ„E INTEGER*4;
C   PˆME—AHˆE: 1. PAO—ˆ‰ BAPˆAHT--> OT‹A„O—HAŸ E—ATœ;
C   ----------  2. E—ATœ C‹“†EH›Mˆ / FORTRAN CˆCTEM›;
C               3. „‹Ÿ MASPRT --> MAX_M1 = 80;
C                  „‹Ÿ MACTAB --> MAX_M2 = 128;
C
CZZZ-------------------------------------------------------------------
        DIMENSION BA0(1), KNOT(1), PAR0(1)
         COMMON/PROF1/ MASPRT(1)
         COMMON/PROF2/ MACTAB(1)
         COMMON/PROF3/ FUNTAB(1)
         DATA M1/80/, M2/128/
         GO TO (1,2), KOD
C======================================================================
         WRITE(6,10) KOD
10       FORMAT(' ***  PRINT2:  KOD = ', I10, ' ***' )
C######################################################################
1        CONTINUE
         WRITE(6,11)
11       FORMAT(1X, 128('*')/'     MACCˆB MASPRT  [ 16-‰ Bˆ„ ]' )
         CALL PDUMP(MASPRT(1), MASPRT(M1), 0 )
         WRITE(6,12)
12       FORMAT(1X, 128('*') /'    MACCˆB MASPRT [ REAL*4 ]')
         CALL PDUMP(MASPRT(1), MASPRT(M1), 5 )
         WRITE(6,13)
13       FORMAT(1X, 128('*') )
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
2        CONTINUE
         WRITE(6,14)
14       FORMAT(1X, 128('*')/'    MACCˆB MACTAB  [ INTEGER*4 ]')
         CALL PDUMP(MACTAB(1), MACTAB(M2), 4 )
         WRITE(6,13)
         RETURN
         END
