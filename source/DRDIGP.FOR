       SUBROUTINE DRDIGP(KAR, DAT, MSWALL, TOKN, TOKN1,
     *  VALLN, VALLN1, YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: DRDIGP(KAR, DAT, MSWALL, TOKN, TOKN1,
C   -------------  VALLN,VALLN1,YALL,JALL,LOGYTR,WORKX, YALLHP,WORKY)
C
C   HA�HA�EH�E:        O�PA�OT��K ��PEKT�B�:
C   ----------         #�C�ET   [ TM ]
C                      *********
C
C                      TM - BPEM� KOH�A PAC�ETA CEK.
C
C
C   METO�:             CM. �/� DIGITA;
C   -----
C                      -> B�XO� HA TEK. �A��C� B NSPOOL B��O�H�ETC�
C                         �/�  READ00
C
C
C   �APAMETP�:         KAR, DAT - CTPOKA � �AHH�E ��PEKT�B�
C  ----------          OCTA��H�E CM. �/�  DIGITA
C
C   �O��PO�PAMM�:      1. DIGITA, PPNCOM,
C   -------------      2. COMMON PROF1
C                      3. TIME, DATER$, TIMER$, READ00
C
C   �P�ME�AH�E:        1. �HA�EH�� �O �MO��AH�� -->  CTAPOE TMAX;
C   ----------         2. �P� "FRESTE"=0 C��TAETC�, �TO �E�� HE BOCTA-
C                         HOB�EHA;
C                      3. BO�MO�HO �C�O���OBAT� �OC�E #C�ET;
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
100    FORMAT(/' *** HET O��CAH�� �E�� - ��PEKT�BA ��HOP�P�ETC� ***'/)
       RETURN
1      CONTINUE
C-->
       IF ( FRESTE .EQ. 1 ) GO TO 2
C-->  MO�ET �TO �OC�E #C�ET....
       IF  ( FEND .EQ. 1 ) GO TO 2
       WRITE( NWTR, 101 )
101    FORMAT(/' *** �E�� HE BOCTAHOB�EHA, ��PEKT�BA ��HOP�P�ETC�',
     =         ' ***'/ ' *** B��O�H�TE <#BOCT ... > ***'/)
       RETURN
2      CONTINUE
       KKK = IFIX( DAT(1) )
       IF ( KKK .EQ. 0 ) GO TO 3
C-->  HOBOE TM .....HE KOHTPO��P�EM �OKA
       TMAX = DAT(2)
3      CONTINUE
C-->  �PH���
       HHH= HOUTT * 1.8E+4
       WRITE(NWTR,102) T0, TMAX, HOUTT,HHH
102   FORMAT(/' *** #�C�ET �ਭ��:==> T0=',G12.5,' TMAX=',G12.5,' ***'/
     =    ' ***                ==> 蠣=',G12.5, ' (', G12.5,')   ***'/)
       CALL PPNCOM(3, NWTR)
C----------------------------------------------------------------------
C-->  B�XO� HA �OC�E�. �A��C� NSPOOL..
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
C--->  ��A� "TPE��ETC� PECTAPT PAC�ETA"********
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
104    FORMAT(/' *** �ᯮ�짮���� ',G12.5,' ᥪ.            ***'/)
       RETURN
C----------------------------------------------------------------------
2000   CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
3000   CONTINUE
       WRITE(NWTR, 105 ) KOD
105    FORMAT(/' *** C�O� �P� �O��TKE #BOCT OTKP�T� "NSPOOL" ***'/
     =         ' *** KO�_READ00 = ', I3,' - #�C�ET B��O�H�T�',
     = ' HE BO�MO�HO ***'/)
       RETURN
       END
