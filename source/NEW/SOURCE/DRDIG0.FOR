       SUBROUTINE DRDIG0(KAR, DAT, MSWALL, TOKN, TOKN1,
     *  VALLN, VALLN1, YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: DRDIG0(KAR, DAT, MSWALL, TOKN, TOKN1,
C   ------------- VALLN, VALLN1, YALL, JALL, LOGYTR,WORKX,YALLHP,WORKY)
C
C   HA�HA�EH�E:        O�PA�OT��K ��PEKT�B� #C�ET:
C   ----------         #C�ET T0  TM  �A�
C
C                      T0 - BPEMA HA�A�A PAC�ETA CEK.
C                      TM - BPEM� KOH�A PAC�ETA CEK.
C                      �A� - �A� COXOAHEH�� PE�EH�� ��. �PA��C�;
C
C   METO�:             CM. �/� DIGITA;
C   -----
C   �APAMETP�:         KAR, DAT - CTPOKA � �AHH�E ��PEKT�B�
C  ----------          OCTA��H�E CM. �/�  DIGITA
C   �O��PO�PAMM�:      1. DIGITA, PPNCOM,
C   -------------      2. COMMON PROF1
C                      3. TIME, DATER$, TIMER$
C   �P�ME�AH�E:        1. �HA�EH�� �O �MO��AH�� HET, �P�
C   ----------            O���KAX PA�CET HE �POBO��TC�;
C                      �CK���EH�E: �P� "�A�" O���EHO -> �P�H�MAETC�
C                                                       PABH�M H0DATA
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
100    FORMAT(/' *** HET O��CAH�� �E�� - ��PEKT�BA ��HOP�P�ETC� ***'/)
       RETURN
1      CONTINUE
       KKK = IFIX( DAT(1) )
       IF ( KKK .GE. 2 ) GO TO 2
       WRITE(NWTR, 101 )
101   FORMAT(/' *** HET "T0" �/��� "TMAX" - ��PEKT�AB ��HOP�P�ETC�',
     =     ' ***'/)
2      CONTINUE
       T0 = DAT(2)
       TMAX = DAT(3)
       HOUTT = H0DATA
       IF ( KKK .EQ. 2 ) GO TO 3
       HOUTT = DAT(4)/1.8E+4
3      CONTINUE
       IF ( HOUTT .LT. H0DATA ) HOUTT = H0DATA
C-->  �PH���
       HHH= HOUTT * 1.8E+4
       WRITE(NWTR,102) T0, TMAX, HOUTT,HHH
102   FORMAT(/' *** #C�ET �ਭ��:==> T0=', G12.5,' TMAX=',G12.5,' ***'/
     =    ' ***               ==> 蠣=',G12.5, ' (', G12.5,')   ***'/)
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
C--->  C�POC ��A�A "TPE��ETC� PECTAPT PAC�ETA"********
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
104    FORMAT(/' *** �ᯮ�짮���� ',G12.5,' ᥪ. ������ ***'/)
       RETURN
       END
