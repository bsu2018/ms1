       SUBROUTINE DRDIGP(KAR, DAT, MSWALL, TOKN, TOKN1,
     *  VALLN, VALLN1, YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA: DRDIGP(KAR, DAT, MSWALL, TOKN, TOKN1,
C   -------------  VALLN,VALLN1,YALL,JALL,LOGYTR,WORKX, YALLHP,WORKY)
C
C   HAáHAóEHàE:        OÅPAÅOTóàK ÑàPEKTàBõ:
C   ----------         #èCóET   [ TM ]
C                      *********
C
C                      TM - BPEMü KOHñA PACóETA CEK.
C
C
C   METOÑ:             CM. è/è DIGITA;
C   -----
C                      -> BõXOÑ HA TEK. áAèàCú B NSPOOL BõèOãHüETCü
C                         è/è  READ00
C
C
C   èAPAMETPõ:         KAR, DAT - CTPOKA à ÑAHHõE ÑàPEKTàBõ
C  ----------          OCTAãúHõE CM. è/è  DIGITA
C
C   èOÑèPOÉPAMMõ:      1. DIGITA, PPNCOM,
C   -------------      2. COMMON PROF1
C                      3. TIME, DATER$, TIMER$, READ00
C
C   èPàMEóAHàE:        1. áHAóEHàü èO ìMOãóAHàû -->  CTAPOE TMAX;
C   ----------         2. èPà "FRESTE"=0 CóàTAETCü, óTO ñEèú HE BOCTA-
C                         HOBãEHA;
C                      3. BOáMOÜHO àCèOãúáOBATú èOCãE #CóET;
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( H0DATA, MASPRT(40) )
       EQUIVALENCE( T0,    MASPRT(42) )
       EQUIVALENCE( TMAX,  MASPRT(43) )
       EQUIVALENCE( HOUTT, MASPRT(48) )
       EQUIVALENCE( NWTR,  MASPRT(13) )
       EQUIVALENCE( NWTRE,  MASPRT(14) )
       EQUIVALENCE( FEND,   MASPRT(55) )
       EQUIVALENCE( FMSWAL, MASPRT(74) )
       EQUIVALENCE( FREST,  MASPRT(45) )
       EQUIVALENCE( FRESTE,  MASPRT(98) )
       INTEGER FMSWAL, FREST,  FRESTE, FEND
       DIMENSION MSWALL(*), TOKN(*), TOKN1(*), VALLN(*), VALLN1(*)
       DIMENSION YALL(1), JALL(*), LOGYTR(*), KAR(*), DAT(*)
       DIMENSION WORKX(1), YALLHP(*), WORKY(*)
       REAL JALL
       REAL*8 TCPU , ICPU
       INTEGER HXX, MXX, SXX, MSXX, YYY, MYY, DYY , DCPU
       DIMENSION TIMEX(3), DATAX(2)
       IF ( FMSWAL .EQ. 1 ) GO TO 1
       WRITE(NWTR, 100)
100    FORMAT(/' *** HET OèàCAHàü ñEèà - ÑàPEKTàBA àÉHOPàPìETCü ***'/)
       RETURN
1      CONTINUE
C-->
       IF ( FRESTE .EQ. 1 ) GO TO 2
C-->  MOÜET ùTO èOCãE #CóET....
       IF  ( FEND .EQ. 1 ) GO TO 2
       WRITE( NWTR, 101 )
101    FORMAT(/' *** ñEèú HE BOCTAHOBãEHA, ÑàPEKTàBA àÉHOPàPìETCü',
     =         ' ***'/ ' *** BõèOãHàTE <#BOCT ... > ***'/)
       RETURN
2      CONTINUE
       KKK = IFIX( DAT(1) )
       IF ( KKK .EQ. 0 ) GO TO 3
C-->  HOBOE TM .....HE KOHTPOãàPìEM èOKA
       TMAX = DAT(2)
3      CONTINUE
C-->  èPHüãà
       HHH= HOUTT * 1.8E+4
       WRITE(NWTR,102) T0, TMAX, HOUTT,HHH
102   FORMAT(/' *** #èCóET Ø‡®≠Ô‚†:==> T0=',G12.5,' TMAX=',G12.5,' ***'/
     =    ' ***                ==> Ë†£=',G12.5, ' (', G12.5,')   ***'/)
       CALL PPNCOM(3, NWTR)
C----------------------------------------------------------------------
C-->  BõXOÑ HA èOCãEÑ. áAèàCú NSPOOL..
       KOD = 3
       CALL READ00(KOD, TXXX, TOKN1, VALLN1 )
       GO TO ( 1000, 2000, 3000 ), KOD
1000   CONTINUE
C----------------------------------------------------------------------
C      CALL TIME(ICPU)
C      CALL TIMER$(TIMEX)
C      CALL DATER$(DATAX)
C      WRITE(NWTR, 103) DATAX, TIMEX
C103    FORMAT(20X, 2A4, 5X, 3A4 )
C######################################################################
C----------------------------------------------------------------------
       CALL GETTIM_(HXX,MXX,SXX,MSXX)
       CALL GETDAT_(YYY,MYY,DYY)
       YYY = MOD( YYY,100 )
       DCPU = 24 * DYY + HXX
       ICPU = 60 * MXX + SXX + 1E-3 * MSXX
       WRITE(NWTR, 103) DYY,MYY,YYY,HXX,MXX,SXX,MSXX
103    FORMAT(20X, 2(I2.2,'/'),I2.2,5X, 2(I2.2,':'),I2.2,'.',I3.3  )
C######################################################################
C--->  îãAÉ "TPEÅìETCü PECTAPT PACóETA"********
       FREST = 1
C
       CALL DIGITA( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =       YALL, JALL, LOGYTR,  WORKX, YALLHP, WORKY )
C######################################################################
C######################################################################
       CALL GETTIM_(HXX,MXX,SXX,MSXX)
       CALL GETDAT_(YYY,MYY,DYY)
       DCPU = 24 * DYY + HXX  - DCPU
       ICPU = 60 * MXX + SXX + 1E-3 * MSXX  -  ICPU
       TCPU = 3600. * DCPU + ICPU
C      CALL TIME(I)
C      TCPU = 1D-6 * (I-ICPU)
       WRITE(NWTR, 104) TCPU
104    FORMAT(/' *** à·ØÆ´ÏßÆ¢†≠Æ ',G12.5,' ·•™.            ***'/)
       RETURN
C----------------------------------------------------------------------
2000   CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
3000   CONTINUE
       WRITE(NWTR, 105 ) KOD
105    FORMAT(/' *** CÅOâ èPà èOèõTKE #BOCT OTKPõTú "NSPOOL" ***'/
     =         ' *** KOÑ_READ00 = ', I3,' - #èCóET BõèOãHàTú',
     = ' HE BOáMOÜHO ***'/)
       RETURN
       END
