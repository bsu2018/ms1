         SUBROUTINE DRFURE(KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRFURE(KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
C   ------------
C   HA�HA�EH�E:    *** �C�O�H�TE�� � O�PA�OT��K ��PEKT�B� #C�EKTP ***
C   -----------      #C�EKTP T0 TM [ F0FUR FMFUR DFFUR FSTD ]
C
C                  T0 - HA�A��HOE BPEM� O�PE�E�EH�� BE����H;
C                  TM - KOHE�HOE  BPEM� O�PE�E�EH�� BE����H;
C                  F0FUR - HA�A��HA� �ACTOTA ( �EP�� ) C�EKTP. XAP-K;
C                  FMFUR - KOHE�HA�  �ACTOTA ( �EP�� ) C�EKTP. XAP-K;
C                  DFFUR - �A� C�EKTP. XAP-K;
C                  FSTD - "OCHOBHA�" �ACTOTA ( �EP� );
C
C  !!!  �P� "O��CKAH��" B��OPKA �� MASPRT !!!
C
C   METO�:         ��� KA��O�O �� "�HA�EH��" B���C�EH�E  �HTE�PA�A
C   -----          ��P�E HA �HTEPBA�E [ T0, TM ] ��� C�EKTPA �ACTOT
C                  OT F0FUR �O FMFUR  C �A�OM DFFUR;
C
C
C
C   �APAMETP�:     KAR - C�MBO��HA� CTPOKA TEKCTA ��PEKT�B�(A-KOD,80);
C   ---------      DAT - MACC�B �P�H�T�X �AHH�X ��PEKT�B�;
C                  MSWALL - C�OBA COCTO�H�� �E��;
C                  TOKN, VALLN - MACC�B� XPAHEH�� "TO�EK";
C                  NDX - PA�MEPHOCT� XGRAF;
C                  XGRAF - "C����H�" ��� �OCTPOEH��;
C
C   �O��PO�PAMM�:  1. COMMON PROF1;
C   -------------  2. GETSPS, FUR000, READ00 , PPNCOM, PLOT2;
C
C   �P�ME�AH�E:    1. "�MO��AH�E" -->
C   ----------         F0FUR = 0, FMFUR=975, DFFUR=25, FSTD=50;
C                   2. MACC�B XGRAF->I FTAB(KTF) I A(KTF) I FI I B I C I
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BF0(9), CF0(9), ZMID(9), ZTEP(9)
         DIMENSION ZAD(9), ZADN(9)
         COMMON /SERVZ2/ TABTOK(1)
         COMMON /SERVZ3/ TABTIP(1)
         INTEGER TABTOK, TABTIP
C-->  B���PAETC� �� NSPOOL 1-� �A��C�
         EQUIVALENCE ( MASPRT(48), HOUTT )
         EQUIVALENCE ( F0FUR,  MASPRT(102) )
         EQUIVALENCE ( FMFUR,  MASPRT(103) )
         EQUIVALENCE ( DFFUR,  MASPRT(101) )
         EQUIVALENCE ( FSTD,   MASPRT(100) )
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( NRDR,   MASPRT(12) )
         EQUIVALENCE ( DIAL,   MASPRT(15) )
         EQUIVALENCE ( PPFUR,  MASPRT(104) )
         INTEGER SPIS, DIAL
         DIMENSION SPIS(55), MSWALL(*), TOKN(*), VALLN(*) , XGRAF(*)
         DIMENSION KAR(*), DAT(*)
C----------------------------------------------------------------------
C  A�PECA B XGRAF_E:
C
       INTEGER ADRA, ADRB, ADRC, ADRFI
C
       DATA GRAD / 57.29578 /
       ADRA( NPOINT ) = KTF + 4*(NPOINT-1)*KTF + 1
       ADRFI(NPOINT ) = KTF + 4*(NPOINT-1)*KTF + KTF + 1
       ADRB( NPOINT ) = KTF + 4*(NPOINT-1)*KTF + 2*KTF + 1
       ADRC( NPOINT ) = KTF + 4*(NPOINT-1)*KTF + 3*KTF + 1
C----------------------------------------------------------------------
C-->  ECT� �� MSWALL ?
       IF ( MASPRT(74) .EQ. 1 ) GO TO 59
       WRITE(NWTR, 60)
60     FORMAT(/' *** HET O��CAH�� �E�� - ��PEKT�BA ��HOP�P�ETC� ***'/)
       RETURN
59     CONTINUE
C======================================================================
       IF ( DIAL .EQ. 0 ) GO TO 67891
       WRITE(NWTR,67892)
67892  FORMAT(/' *** �B�..., �AHHA� BEPC�� �P� "#��A�O� �A"',
     =         '  -  #C�EKTP HE B��O�H�ET ***'/)
       RETURN
67891  CONTINUE
C======================================================================
C===>
         F0 = F0FUR
         FM = FMFUR
         DF = DFFUR
         FS = FSTD
C=======>READ(NRDR, GRAF)
       KKK = IFIX( DAT(1) )
       IF ( KKK .GE. 2 ) GO TO 5600
       WRITE(NWTR, 57 )
57     FORMAT(/' *** HET "T0" �/��� "TM" - ��PEKT�BA ��HOP�P�ETC� ***'/)
       RETURN
5600   CONTINUE
       T0 = DAT(2)
       TM = DAT(3)
       IF ( KKK. EQ. 2 ) GO TO 58
       F0    =      DAT(4)
       IF ( KKK .EQ. 3 ) GO TO 58
       FM  =       DAT(5)
       IF ( KKK .EQ. 4 ) GO TO 58
       DF    = DAT(6)
       IF ( KKK .EQ. 5 ) GO TO 58
       FS = DAT(7)
58     CONTINUE
C=======>WRITE(NWTR, GRAF)
C====>
         IF ( F0 .GT. FM                 ) F0 = F0FUR
         IF ( FM .LT. F0     ) FM = FMFUR
         IF ( FS   .LT. 0 ) FS = FSTD
C===>
         WRITE(NWTR, 50) T0, TM, F0, FM, DF, FS
50     FORMAT(/' *** #C�EKTP �P�H�TA:==>   T0  =', G12.5,'   TM=  ',
     =          G12.5,' ***'/
     =         ' ***                 ==> F0_FUR=',G12.5,' FM_FUR=',
     =          G12.5, ' ***'/
     =         ' ***                 ==> DF_FUR=',G12.5,'  F_STD=',
     =          G12.5, ' ***'/)
C
C
C----------------------------------------------------------------------
         CALL GETSPS( MSWALL, SPIS, IRC )
         GO TO ( 1,2,3), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR, 51)
51      FORMAT(//' *** K���� 䠩�� �� "RDR" - ࠡ�� ���뢠���� ***'//)
         call STOPP(16)
C----------------------------------------------------------------------
2        CONTINUE
C-->  SPIS ��CT
         RETURN
C**********************************************************************
1        CONTINUE
C--> �O��OTOBKA K B�BO��...
       TIME = 0.0
C--->  KO�-BO TO�EK �O �ACTOTE...
       KTF = (FM-F0)/ DF  + 1
C-->  �O��CT�MOCT� KP
       KP = SPIS(1)
       NDXKP =  4*KTF*KP + KTF
       IF ( NDXKP .LE. NDX ) GO TO 10
C-->  KP �O���OE, �A PA� HE BO�MEM, CKO��KO CMO�EM ?????
       KPX = ( NDX-KTF ) / ( 4*KTF )
C======================================================================
       IF( KPX .LE. 0 ) WRITE(NWTR, 67893 )      NDX, KTF
67893  FORMAT(/' *** �HTEPBA� [F0,FM] BE��K ��� �ME��E�C� O.�. ***'/
     =         ' *** B HA����� BCE�O ', I10, ' C�OB O.�. ***'/
     =         ' *** �A�POC K #C�EKTP �O KTF=', I5,' TO�KAM ��HOP�P�ETC�
     = ***'/' *** �BE���TE O.�. ��� �MEH���TE B �HTEPBA�E [F0,FM]',
     =     ' KTF ***'/)
       IF ( KPX .LE. 0 ) RETURN
C======================================================================
       WRITE(NWTR,500) KTF, KP, KPX
500    FORMAT(/' *** ��� C�EKTPA��HO�O AHA���A HA ', I5, ' TO�KAX ',
     = ' O�PE�E�EH�� �ACTOT� ***'/
     = ' *** KO���ECTBO B�BO�H�X �HA�EH�� ', I2, ' BE��KO (O�PAH��EH�E
     =BEPC��) ***'/ ' *** C�EKTPA��H�� AHA��� �POBO��TC� TO��KO',
     = ' ��� ', I2, ' �A�PO�EHH�X �HA�EH�� ***'/)
       KP = KPX
       SPIS(1) = KP
C      *************
10     CONTINUE
C--> OTKPOEMC�
       KTXX = 0
       TIME = 0.0
         KOD = 1
         CALL READ00( KOD, HOUTT,  TOKN, VALLN)
         GO TO ( 100, 200, 300 ), KOD
         CALL SUBERR(KOD)
100      CONTINUE
C----------------------------------------------------------------------
       HOUTTX = HOUTT * 1.8E+4
       WRITE(NWTR, 5667 ) HOUTT, HOUTTX
5667   FORMAT(/' *** �HA�EH�� �EPEMEHH�X �E�� O�PE�E�EH� C �A�OM',
     = 1X,G12.5, ' (', G10.4, ') ***'/)
C----------------------------------------------------------------------
C****CTPAXOBKA*********************************************************
       IF ( TM .LE. T0 ) WRITE(NWTR,5666) T0, TM
5666  FORMAT(//' *** �HTEPBA� �HA�EH�� [T0,TM]:=> ','[',G12.5,',',G12.5,
     =   '] ***'/ ' *** O�PE�E�EH O���O�HO --> "#C�EKTP" - ',
     =        ' ��HOP�P�ETC� ***'//)
       IF ( TM .LE. T0 ) RETURN
C**********************************************************************
C--> �O��OTOBKA.....
       DO 400 I=1,NDX
400    XGRAF(I) = 0.0
C--> MACC�B �ACTOT
       DO 401 I=1,KTF
401    XGRAF(I) = F0 + (I-1) * DF
C-->  #�HHTE�PATOP�#
       DO 402 I=1,9
       BF0(I) = 0.0
       CF0(I) = 0.0
       ZMID(I) = 0.0
       ZTEP(I) = 0.0
402    CONTINUE
C--> �EP�O�
       PERIOD = TM - T0
C--
C    B���C�EH�E �HTE�PA�OB
C---
        IFLAG = 0
C-->  ��K� HAKO��EH��
403      CONTINUE
         KOD = 2
         CALL READ00(KOD, TIME, TOKN, VALLN )
         GO TO (404, 200, 300 ), KOD
         CALL SUBERR(KOD)
404      CONTINUE
C-->  HA�A �� TO�KA
         IF ( TIME .LT. T0 ) GO TO 403
         IF ( TIME .GT. TM ) GO TO 405
         KTXX = KTXX + 1
C--> �P�H�T� TO�K� B MACC�B ZAD,  PA��OP SPIS
       KPP = KP
       ISPIS = 2
       IHELP = 1
406    CONTINUE
       IF ( KPP .LE. 0 ) GO TO 407
       IPX = SPIS(ISPIS)
       I2 = SPIS(ISPIS+1)
       GO TO (408, 409, 410 ), IPX
       CALL SUBERR(IPX)
408    CONTINUE
C-->  HA�P��EH�E
       VI2 = 0.0
       IF( I2 .NE. 0 ) VI2 = VALLN(I2)
       ZAD(IHELP) = VI2
       GO TO 411
409    CONTINUE
C-->  PA�HOCT�
       VI2 = 0.0
       I3 = SPIS(ISPIS+2)
       IF ( I2 .NE. 0 ) VI2 = VALLN(I2)
       IF ( I3 .NE. 0 ) VI3 = VALLN(I3)
       ZAD( IHELP) = VI2 - VI3
       GO TO 411
410    CONTINUE
C-->  TOK�
       ZAD(IHELP) = TOKN(I2)
C******GO TO 411
411    CONTINUE
       KPP = KPP - 1
       IHELP = IHELP + 1
       ISPIS = ISPIS + 6
       GO TO 406
C----------------------------------------------------------------------
407    CONTINUE
C-->  B ZAD_E C���T �HA�EH�� �P� T = TIME
C EC�� �TO B �EPB�� PA�, TO TO��KO �X �A�OMH�M, �HA�E "�EPEMECT�M"
       IF ( IFLAG .EQ. 1 ) GO TO 412
C-->  B �EPB�� PA� ( �..)
       TN = TIME
       DO 413  I = 1, KP
413    ZADN(I) = ZAD(I)
       IFLAG = 1
C-->  HA  C�E�. BPEMEHH�� TO�K�
       GO TO 403
412    CONTINUE
C-->  HE �..., MO�HO �HTE�P�POBAT�..
       DO 4130 I=1,KP
       IBX = ADRB(I)
       CALL FUR000(KTF, XGRAF(1), FS, HOUTT, TN, ZADN(I), ZAD(I),
     *             ZMID(I), ZTEP(I), BF0(I), CF0(I), XGRAF(IBX) )
4130   CONTINUE
C-->  �EPE�A��C�..
       DO 414 I=1,KP
414    ZADN(I) = ZAD(I)
       TN = TIME
C--> HA HOB��
       IF ( TIME .EQ. TM ) GO TO 405
       GO TO 403
C----------------------------------------------------------------------
405    CONTINUE
C---> BCE HAKO���� � "B" + "C" B���C����
C-->  PAC�ET AM���T��, �A� � T.�.
C--> �E��M HA �EP�O�
       DO 415  I=1, KP
       DO 416  J=1, KTF
       OMEGA = XGRAF(J)
       B = XGRAF( ADRB(I) - 1 + J )
       C = XGRAF( ADRC(I) - 1 + J )
       IF ( OMEGA .NE. 0.0 ) GO TO 417
C-->  �O H���M...
       B = B / PERIOD
       C = C / PERIOD
       GO TO 418
417    CONTINUE
       B = 2.0 * B / PERIOD
       C = 2.0 * C / PERIOD
418    CONTINUE
       A = SQRT( B**2 + C**2 )
C******FI = 0.0
C******IF ( C .EQ. 0.0  .AND. B .EQ. 0.0  ) GO TO 12345
C******FI = ATAN2( B, C ) * GRAD
C******12345  CONTINUE
C--> B MACC�B:   --> **A**  **FI**  **B**  **C**
       XGRAF( ADRA(I) - 1 + J ) = A
       XGRAF( ADRFI(I) - 1 + J ) = FI
       XGRAF( ADRB(I) - 1 + J ) = B
       XGRAF( ADRC(I) - 1 + J ) = C
416    CONTINUE
C-->  ��� CPE�. & �E�CT.
       ZMID(I) = ZMID(I) / PERIOD
       ZTEP(I) = SQRT(     ZTEP(I) / PERIOD        )
C
       IF ( FS .EQ. 0.0 ) BF0(I) = BF0(I) / PERIOD
       IF ( FS .EQ. 0.0 ) CF0(I) = CF0(I) / PERIOD
       IF ( FS .NE. 0.0 ) BF0(I) =  2.0 * BF0(I) / PERIOD
       IF ( FS .NE. 0.0 ) CF0(I) =  2.0 * CF0(I) / PERIOD
415    CONTINUE
C-->  MO�HO B�BO��T�...
C-->  IHELP  - HOMEP B�B. �HA�EH��
       WRITE(NWTR, 5000 )
5000   FORMAT(' '// 20X, '�APMOH��ECK�� AHA���'/20X,20('*') //)
       CALL PPNCOM( 3, NWTR )
       KPP = KP
       ISPIS = 2
       IHELP = 1
420    CONTINUE
       IF ( KPP .LE. 0 ) GO TO 421
       IPX = SPIS(ISPIS)
       GO TO ( 422, 423, 424 ), IPX
       CALL SUBERR(IPX)
422    CONTINUE
       NV = SPIS(ISPIS+1)
       WRITE( NWTR, 501) NV
501    FORMAT(/5X, ' AHA����P�ETC� HA�P��EH�E ���A ', I5/ 6X,36('-')/)
       GO TO 425
423    CONTINUE
       NV = SPIS(ISPIS+1)
       NV1 = SPIS(ISPIS+2)
       WRITE( NWTR, 502) NV, NV1
502    FORMAT(/5X,' AHA����P�ETC� HA�P��EH�E ME��� ���AM� ', I5,
     =           '  � ', I5 / 6X, 53('-') / )
       GO TO 425
424    CONTINUE
       NEL = SPIS(ISPIS+2)
       IANM = SPIS( ISPIS + 3 )
       IANM2 = IANM + 4
       IANT  = SPIS( ISPIS + 4 )
       IANT2 = IANT + 2
       NSM = SPIS(ISPIS+5)
       WRITE( NWTR,503) (TABTOK(I424), I424=IANT, IANT2),
     *      NEL, NSM,   (TABTIP(I424), I424=IANM, IANM2)
503    FORMAT(/2X,' AHA����P�ETC� ', 3A4, '--> ��EMEHT ', I3,
     *        ' �HA�EH�E ', I2, '(', 5A4, ')'/ 3X, 76('-') / )
425    CONTINUE
C-->
       KTTT = ( TM - T0 ) / HOUTT   + 1
       KPPP = (TM-T0) *       FS    + 1
       WRITE(NWTR,5020) T0, TM, KTTT, FS, KPPP, PPFUR
5020   FORMAT(/1X,'�HTEPBA� AHA���A OT ', G12.5, ' CEK. �O ',
     *   G12.5,' CEK. (', I8, ' TO�EK )'  /
     * ' KO���ECTBO �EP�O�OB (HA OCHOBHO� �ACTOTE ', G12.5, ' �EP� )  ',
     *  2X,   I7 / ' [ ��ET KO��. PA��. OTHOC������� ',
     * 'A(F)_MAX �O ', F6.3, ' %-M� �PEB��EH�� ]' /)
       WRITE( NWTR, 504 )
504    FORMAT(/1X, 79('-')/
     * 1X, '  �ACTOTA !', ' B(F)->COS !', ' C(F)->SIN !',
     * 'A(F)->AM��.!', '�(F)->�A�A!', 'A/A_MAX->%!',
     *  'A/A(F0)->%'/  1X, 79('-') )
C-->  HAXO��EH�E MAX...
       IMAX = 1
       AMAX = XGRAF( ADRA(IHELP) )
       ASUM = AMAX**2
C*********************
       DO 426 J=2, KTF
       ATEK = XGRAF( ADRA(IHELP) + J - 1 )
       ASUM = ASUM + ATEK**2
       IF ( ATEK .LE. AMAX ) GO TO 426
       AMAX = ATEK
       IMAX = J
426    CONTINUE
C--->
       IF ( AMAX .EQ. 0.0 ) WRITE(NWTR,56789)
56789  FORMAT(/' *** MAKC�MA��HA� AM���T��A C��HA�A H��EBA� !!! ***'/
     =         '     ##########################################'//)
       IF ( AMAX .EQ. 0.0 ) GO TO 45678
C===
       FIFS = 0.0
C
       AFS = SQRT( BF0(IHELP)**2 + CF0(IHELP)**2 )
C-->  "O�PE�KA"
       IF ( (ABS(CF0(IHELP))/AMAX)*100.0  .LT. PPFUR ) CF0(IHELP) = 0.0
       IF ( (ABS(BF0(IHELP))/AMAX)*100.0  .LT. PPFUR ) BF0(IHELP) = 0.0
C==
       IF ( CF0(IHELP).EQ.0.0  .AND.  BF0(IHELP).EQ.0.0 ) GO TO 54321
       FIFS = ATAN2( BF0(IHELP), CF0(IHELP) ) *  GRAD
54321  CONTINUE
C
       IF ( FS .EQ. 0.0 ) FIFS = 0.0
C
       DO 427 J=1,KTF
       OMEGA = XGRAF(J)
       B = XGRAF( ADRB(IHELP) + J - 1 )
C--> O�PE�KA
       IF ( ((ABS(B)/AMAX) * 100.0) .LT. PPFUR ) B = 0.0
       XGRAF( ADRB(IHELP) + J - 1 ) = B
C-->
       C = XGRAF( ADRC(IHELP) + J - 1 )
C--> O�PE�KA
       IF ( ((ABS(C)/AMAX) * 100.0) .LT. PPFUR ) C = 0.0
       XGRAF( ADRC(IHELP) + J - 1 ) = C
C-->
       A = XGRAF( ADRA(IHELP) + J - 1 )
       IF ( ((ABS(A)/AMAX)*100.0) .LT. PPFUR ) A = 0.0
       XGRAF( ADRA(IHELP) + J - 1 ) = A
C===>
       FI = 0.0
       IF ( OMEGA .EQ. 0.0 ) GO TO 12345
       IF ( C .EQ. 0.0 .AND. B .EQ. 0.0 ) GO TO 12345
       FI = ATAN2( B, C  ) * GRAD
12345  CONTINUE
       XGRAF( ADRFI(IHELP) + J - 1 ) = FI
C====
       AP1 = ( A/ AMAX ) * 100.0
       AP2 = ( A/ AFS  ) * 100.0
C
       WRITE( NWTR, 505 ) OMEGA, B, C, A, FI, AP1, AP2
505    FORMAT(1X, G9.3, 1X, '!' ,    3( G10.4, 1X, '!' ),
     *            2( G9.3, 1X, '!' ) ,  G9.3, '!'      )
C
427    CONTINUE
       AP1=  (AFS/ AMAX ) * 100.0
       AP2 = (AFS/AFS ) * 100.0
C
       WRITE( NWTR, 506 ) FS, BF0(IHELP), CF0(IHELP), AFS,
     *   FIFS,  AP1, AP2
506    FORMAT(1X, 79('+')/
     *        1X, G9.3, 1X, '!' ,    3( G10.4, 1X, '!' ),
     *            2( G9.3, 1X, '!' ) ,  G9.3, '!' /
     *              1X, 79('-')/)
C
       WRITE(NWTR, 507 ) PERIOD, ZMID( IHELP)
       WRITE(NWTR, 508 ) PERIOD, ZTEP( IHELP)
       RKG2 = (ASUM - AMAX**2) / ASUM
       RKG  = SQRT( RKG2 )  * 100.0
       OMM = XGRAF( IMAX )
       WRITE( NWTR, 509 ) OMM, RKG
507    FORMAT(1X, 'CPE�HEE �HA�EH�E     ( �HTEPBA� AHA���A ',
     =  G10.4, ' CEK. ) ........', G12.5)
508    FORMAT(1X,'�E�CTB�E�EE �HA�EH�E ( �HTEPBA� AHA���A ',
     =  G10.4, ' CEK. ) ........', G12.5 )
509    FORMAT(1X,'KO������EHT �APMOH�K ( K MAX. AM���T��E',
     = ' HA �ACTOTE ', G10.4, '�� )..', G12.5 //)
C----------------------------------------------------------------------
       WRITE(NWTR, 510)
510    FORMAT(//20X,' AM���T��HO � �A�O �ACTOTH�E XAPAKTEP�CT�K�'/
     =      21X, 42('-')//
     =  ' C�MBO� "1" - AM���T��A AHA����P�EMO� BE����H�' /
     =  ' C�MBO� "2" - �A�A      AHA����P�EMO� BE����H�'/)
       ISX =  KTF+10
       IXA = ADRA(IHELP)
       CALL PLOT2( XGRAF(1), XGRAF( IXA ), KTF, 2, KTF, 80, ISX, 10,
     =            NWTR, 0 )
C
45678  CONTINUE
C
       IHELP = IHELP + 1
       KPP = KPP - 1
       ISPIS = ISPIS + 6
       GO TO 420
C=== BCE
421    CONTINUE
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
200      CONTINUE
C--->   �O�. KOHE�, BCE�O B �A��E KTXX TO�EK
         WRITE(NWTR, 54) KTXX
54       FORMAT(/' *** �OCT��H�T KOHE� �A��OB HAKO��EH��    ***'/
     =           ' *** B �A�PO�EHHOM ��A�O�OHE OT T0 �O TM  ***'/
     =           ' *** O�HAP��EHO BCE�O ', I5, ' TO�EK PE�EH�� ***'/
     = ' *** O�PA�OTKA ��PEKT�B� #C�EKTP �PEKPA�EHA            ***'/)
C****= ' *** �OC�E�H�� TO�KA O�PE�E�EHA ��� T =',G12.5, ' CEK. ***'/
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
300      CONTINUE
C-->  ���. KOHE�
         WRITE(NWTR,55) KTXX,         TIME
55       FORMAT(/' *** �A��� HAKO��EH�� PE�EH�� HE ���� �AKP�T� ***'/
     = ' *** B �A�PO�EHHOM ��A�O�OHE OT T0 �O TM ***'/
     = ' *** O�HAP��EHO BCE�O ', I5, ' TO�EK PE�EH�� ***'/
     = ' *** �OC�E�H�� TO�KA O�PE�E�EHA ��� T =',G12.5, ' CEK. ***'/
     = ' *** PA�OTA �PEP�BAETC� *** [ DRFURE ] ********************'///)
         call STOPP(16)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
