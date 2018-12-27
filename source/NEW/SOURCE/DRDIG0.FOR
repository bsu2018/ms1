       SUBROUTINE DRDIG0(KAR, DAT, MSWALL, TOKN, TOKN1,
     *  VALLN, VALLN1, YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA: DRDIG0(KAR, DAT, MSWALL, TOKN, TOKN1,
C   ------------- VALLN, VALLN1, YALL, JALL, LOGYTR,WORKX,YALLHP,WORKY)
C
C   HAáHAóEHàE:        OÅPAÅOTóàK ÑàPEKTàBõ #CóET:
C   ----------         #CóET T0  TM  òAÉ
C
C                      T0 - BPEMA HAóAãA PACóETA CEK.
C                      TM - BPEMü KOHñA PACóETA CEK.
C                      òAÉ - òAÉ COXOAHEHàü PEòEHàü ùã. ÉPAÑìCõ;
C
C   METOÑ:             CM. è/è DIGITA;
C   -----
C   èAPAMETPõ:         KAR, DAT - CTPOKA à ÑAHHõE ÑàPEKTàBõ
C  ----------          OCTAãúHõE CM. è/è  DIGITA
C   èOÑèPOÉPAMMõ:      1. DIGITA, PPNCOM,
C   -------------      2. COMMON PROF1
C                      3. TIME, DATER$, TIMER$
C   èPàMEóAHàE:        1. áHAóEHàâ èO ìMOãóAHàû HET, èPà
C   ----------            OòàÅKAX PAóCET HE èPOBOÑàTCü;
C                      àCKãûóEHàE: èPà "òAÉ" OèìôEHO -> èPàHàMAETCü
C                                                       PABHõM H0DATA
C
C
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( H0DATA, MASPRT(40) )
       EQUIVALENCE( T0,    MASPRT(42) )
       EQUIVALENCE( TMAX,  MASPRT(43) )
       EQUIVALENCE( HOUTT, MASPRT(48) )
       EQUIVALENCE( NWTR,  MASPRT(13) )
       EQUIVALENCE( NWTRE,  MASPRT(14) )
       EQUIVALENCE( FMSWAL, MASPRT(74) )
       EQUIVALENCE( FREST,  MASPRT(45) )
       INTEGER FMSWAL, FREST
       DIMENSION MSWALL(*), TOKN(*), TOKN1(*), VALLN(*), VALLN1(*)
       DIMENSION YALL(*), JALL(*), LOGYTR(*), KAR(*), DAT(*)
       DIMENSION WORKX(*), YALLHP(*), WORKY(*)
       REAL JALL
       REAL*8 TCPU , ICPU
       INTEGER HXX, MXX, SXX, MSXX, YYY, MYY, DYY , DCPU
       IF ( FMSWAL .EQ. 1 ) GO TO 1
       WRITE(NWTR, 100)
100    FORMAT(/' *** HET OèàCAHàü ñEèà - ÑàPEKTàBA àÉHOPàPìETCü ***'/)
       RETURN
1      CONTINUE
       KKK = IFIX( DAT(1) )
       IF ( KKK .GE. 2 ) GO TO 2
       WRITE(NWTR, 101 )
101   FORMAT(/' *** HET "T0" à/àãà "TMAX" - ÑàPEKTàAB àÉHOPàPìETCü',
     =     ' ***'/)
2      CONTINUE
       T0 = DAT(2)
       TMAX = DAT(3)
       HOUTT = H0DATA
       IF ( KKK .EQ. 2 ) GO TO 3
       HOUTT = DAT(4)/1.8E+4
3      CONTINUE
       IF ( HOUTT .LT. H0DATA ) HOUTT = H0DATA
C-->  èPHüãà
       HHH= HOUTT * 1.8E+4
       WRITE(NWTR,102) T0, TMAX, HOUTT,HHH
102   FORMAT(/' *** #CóET Ø‡®≠Ô‚†:==> T0=', G12.5,' TMAX=',G12.5,' ***'/
     =    ' ***               ==> Ë†£=',G12.5, ' (', G12.5,')   ***'/)
       CALL PPNCOM(3, NWTR)
C----------------------------------------------------------------------
       CALL GETTIM(HXX,MXX,SXX,MSXX)
       CALL GETDAT(YYY,MYY,DYY)
       YYY = MOD( YYY,100 )
       DCPU = 24 * DYY + HXX
       ICPU = 60 * MXX + SXX + 1E-3 * MSXX
       WRITE(NWTR, 103) DYY,MYY,YYY,HXX,MXX,SXX,MSXX
103    FORMAT(20X, 2(I2.2,'/'),I2.2,5X, 2(I2.2,':'),I2.2,'.',I3.3  )
C######################################################################
C--->  CÅPOC îãAÉA "TPEÅìETCü PECTAPT PACóETA"********
       FREST = 0
C
       CALL DIGITA( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =      YALL, JALL, LOGYTR,  WORKX, YALLHP, WORKY )
C######################################################################
       CALL GETTIM(HXX,MXX,SXX,MSXX)
       CALL GETDAT(YYY,MYY,DYY)
       DCPU = 24 * DYY + HXX  - DCPU
       ICPU = 60 * MXX + SXX + 1E-3 * MSXX  -  ICPU
       TCPU = 3600. * DCPU + ICPU
       WRITE(NWTR, 104) TCPU
104    FORMAT(/' *** à·ØÆ´ÏßÆ¢†≠Æ ',G12.5,' ·•™. Ø‡ÆÊ•··Æ‡† ***'/)
       RETURN
       END
