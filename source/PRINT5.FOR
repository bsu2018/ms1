c      CALL PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
       SUBROUTINE PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.x  (OT „EKAPŸ 1995 ƒ.)------------
C   O„POƒPAMMA:  PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
C   ------------
C   HA‡HA—EHˆE:    á¬. DM0007....
C   ----------
C
C
C   METO„:
C   -----
C
C   O„POƒPAMM›: COMMON PROF1;
C  -------------
C
C  PˆME—AHˆE:     … €Ž’€…’ .....
C  -----------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION RMSW(*), TOKN(*), TOKN1(*), ITOK(*)
         EQUIVALENCE ( NWTRE     , MASPRT(14) )
C
C--------------------------------------------------------------------
C    …—€’œ ‚ ”€‰‹ Ž˜ˆŽŠ
C--------------------------------------------------------------------
         WRITE(NWTRE,1)
1        FORMAT( 1X, 128('*')/' PRINT5 ­¥ à ¡®â ¥â..')
         RETURN
         END
