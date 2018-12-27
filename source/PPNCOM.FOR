       SUBROUTINE PPNCOM(KOD, NWTR )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA: PPNCOM(KOD, NWTR )
C   -------------
C   HAáHAóEHàE:  èEóATú èO KAHAãì NWTR, B áABàCàMOCTà OT KOD:
C  -----------   KOD = 1 - àMEHà ñEèà (#àMü);
C                    = 2 - KOMEHTAPàEB (#èPàM);
C                    = 3 - àMEHà ñEèà à KOMEHTAPàEB;
C   METOÑ:       OóEBàÑEH;
C  -------
C   èAPAMETPõ:   CM. BõòE;
C   ---------
C   èOÑèPOÉPAMMõ 1. COMMON SERVZ4, SERVZ5, PROF1
C  -------------
C   èPàMEóAHàE:  HET
C   -----------
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       COMMON/ SERVZ4/ NAMECR(80)
       COMMON/ SERVZ5/ KCOM, COMENT(100)
       INTEGER COMENT
       GO TO (1,2,3),KOD
       CALL SUBERR(KOD)
1      WRITE(NWTR,10)   ( NAMECR(I), I=1,72 )
10     FORMAT(/1X,'CXEMA: ', 72A1 /)
       RETURN
2      CONTINUE
       IF ( KCOM .EQ. 0 ) RETURN
       DO 20 IP=1,KCOM
       I1 = (IP-1)*20+1
       I2 = I1+17
       IF( MASPRT(15) .EQ. 0 ) WRITE(NWTR,21)(COMENT(I), I=I1,I2 )
       IF( MASPRT(15) .EQ. 1 ) WRITE(NWTR,22)(COMENT(I), I=I1,I2 )
21     FORMAT(1X, 20A4)
22     FORMAT(    20A4)
20     CONTINUE
       RETURN
3      CONTINUE
       WRITE(NWTR,10) (NAMECR(I), I=1, 72 )
       GO TO 2
       END
