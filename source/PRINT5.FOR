c      CALL PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
       SUBROUTINE PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
CXXX----KOMEKC "MS1" --> BEPC 1.x  (OT EKAP 1995 .)------------
C   OPOPAMMA:  PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
C   ------------
C   HAHAEHE:    á¬. DM0007....
C   ----------
C
C
C   METO:
C   -----
C
C   OPOPAMM: COMMON PROF1;
C  -------------
C
C  PMEAHE:       .....
C  -----------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION RMSW(*), TOKN(*), TOKN1(*), ITOK(*)
         EQUIVALENCE ( NWTRE     , MASPRT(14) )
C
C--------------------------------------------------------------------
C       
C--------------------------------------------------------------------
         WRITE(NWTRE,1)
1        FORMAT( 1X, 128('*')/' PRINT5 ­¥ à ¡®â ¥â..')
         RETURN
         END
