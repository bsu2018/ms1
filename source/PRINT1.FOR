         SUBROUTINE PRINT1( MSWALL, RMSWALL )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  PRINT1( MSWALL )
C   ------------
C   HA‡HA—EHˆE:    KOHTPO‹œHAŸ E—ATœ MACCˆBA
C   ----------     “PAB‹Ÿž™ˆX C‹OB MAKPOMO„E‹E‰
C
C
C   METO„:         E—ATœ OT MSWALL(1) „O MSWALL( MSWALL(3)+10 )
C   -----
C
C   O„POƒPAMM›: COMMON PROF1;
C  -------------
C
C  PˆME—AHˆE:
C  -----------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION MSWALL(*), RMSWALL(*)
         EQUIVALENCE ( NWTRE     , MASPRT(14) )
C
         IMAX = MSWALL(3)
C
         IM1 = IMAX + 10
C--------------------------------------------------------------------
C    …—€’œ ‚ ”€‰‹ Ž˜ˆŽŠ
C--------------------------------------------------------------------
         WRITE(NWTRE,1) IM1
1        FORMAT( 1X, 128('*')/'    MACCˆB MSWALL OT [ 1 „O ',I5,' ]')
         WRITE(NWTRE,2) ( MSWALL(I), I=1,IM1 )
2        FORMAT(1X, 10I10/ )
         WRITE(NWTRE,3) IM1
3        FORMAT( 1X, 128('*')/'    MACCˆB RMSWALL OT [ 1 „O ',I5,' ]')
         WRITE(NWTRE,4) ( RMSWALL(I), I=1,IM1 )
4        FORMAT(1X, 10E10.4/ )
         WRITE(NWTRE,5)
5        FORMAT( 1X, 128('*') )
         RETURN
         END
