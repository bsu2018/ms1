         SUBROUTINE DIGITA( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     *            YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.5  (OT AB�KCTA 1989 �.)------------
C   �O��PO�PAMMA:  DIGITA( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
C   ------------      YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
C
C   HA�HA�EH�E:    O��EE ��PAB�EH�E �PO�ECCOM PAC�ETA ��EKTP��ECKO�
C   ----------     �E��; �CXO�HOE O��CAH�E �E�� CO�EP��TC� B C��CKE
C                   MSW_ALL;
C
C                  == �P� FRUNGE = 1 B��O�HEH�E "TECTA" P�H�E ;
C                  == TECT �A�PA��BAETC� �OC�E KA��O� KOMM�TA��� ===
C                     �P� HEXBATKE MITER �TEPA��� ��� EITER "�OKA��HO�"
C                     O���K� - CMEHA H0DATA HE PEA���OBAHA
C                  == B GETNEW �TO PEA���OBAHO;
C
C              == �P� HXMIN > H0DATA -> �CTAHOBKA HXMIN=H0DATA/10;
C              ==
C              == �P� METVET=1 � HXMIN=0 -> �CTAHOBKA HXMIN=H0DATA/100.0
C
C
C                  == ��MEHEHO ��PAB�EH�E C����O� BPEMEH� ---->
C                     BCE��A C�ET C H0DATA, �P� "�POCKOKE" �EPE�
C!!! C TO�HOCT�� !!!  TO�K� B�BO�A -> ��HE�HA� �HTEP�O����� K T=TOUT;
C!!! �O H0MIN !!!    == �P� IPOL = 1 --> C�E�. �HTEP�O����� K T=TOUT;
C                  == K.T. CO��AETC� B CETKE T0+HOUTT � �P� RESTARTE
C                     (FREST=1) ��CK C �HTEP�O��POBAHHO�O �HA�EH��,
C                      �TO MO�ET BHECT� O���K� (?????) -> �TO HE �CHO;
C              === �P� BO�����EH�� ��A�A FREST [MASPRT(45)] -->
C                  �A�POC HA �PO�O��EH�E PE�EH�� �� TO�K� TN....
C                  �P� �TOM �PE��O�O�AETC�, �TO BCE �AHH�E �� MASPRT
C                  BOCTAHOB�EHH�;
C              ===
C
C              == �P� FCHEK [MASPRT(91)] �/� WRIT00 CO��AET
C                K.T. ABTOMAT��ECK� HA �A�� NFACHK[MASPRT(97)];
C
C                 DIGITA ��� �TO�O O�PE�E��ET �O�E TACHEK -->
C                 BPEM� CO��AH�� O�EPE�HO� K.T. �
C                 KOPPEKT�P�ET �O�E DTCHEK -> �E�AET E�O
C                 �HA�EH�E KPATH�M "HOUTT"
C                      *** �P� FCHEK = 1 ---->
C                 B KOH�E PAC�ETA CO��AETC� K.T. HA NFACHK
C                 BHE �AB�C�MOCT� OT DTCHEK;
C              ==
C
C              ==  �EPBOE C�OBO �EPBO� �A��C� HA NSPOOL -->
C                  �HA�EH�E HOUTT;
C              ==
C
C    @@@@@@@@@@@@@--> "KOHTPO��" C����� BPEMEH� ��OK�POBAH;
C
C    $$$ OT 8.02.89 �P�  T0OUT(MASPRT(115)->REAL*4) > 0.0 B�BO� B
C    $$$$$$$$$$ �A�� HAKO��EH�� (SPOOL) B��O�H�ETC� �P�  TN >= T0OUT
C    $$$ OT 3.08.89 �P� T0OUT ^=0 � H0OUTT(MASPRT(116) B TO�KE
C    $$$$$$$$$$ T0OUT �CTAHAB��BAETC� �A� B�BO�A HOUTT = H0OUTT
C    $$$$$$$$$$ �P� HEO�XO��MOCT� KOPPEKT�P�ETC� H0DATA.......
C    $$$$$$$$$$  �P� �A�POCE #ACOX COXPAHEH�E �CXO�� �� CTAPO�O
C    $$$$$$$$$$  �HA�EH�� HOUTT
C    $$$=> T0OUT=T0OUT-H0DATA(CTPAXOBKA)
C    $$$=> OUTFLG=0(HET),=1(TO��KO T0HOUT),=2(MEH�EM H0DATA...)
C
C   METO�:         - �OC�E�OBATE��HOE �C�O���OBAH�E �/� "B���C��T�
C   -----            COCTO�H�E �E�� ��� T = TN + H0" ��� BCE�O �HTEPBA�A
C                    PAC�ETA;  ��PAB�EH�E METO�OM ��CEHHO�O �HTE�P�PO-
C                    BAH�� �POBO��TC� HA OCHOBAH�� ��A�A HA�����
C                    KOMM�TA��� HA O�EPE�HOM �A�E �HTE�P�POBAH��;
C
C   !!!!!!!!!!== �P� KODMU = 1 --->  "PA��OH" HE �POBO��TC� ===
C
C                  - B��O�H�ETC� ��PAB�EH�E B�BO�OM �H�OPMA��� B
C                    �A��� HAKO��EH��;
C
C
C              === �P� NSYS = 1 ---> BCE��A KODSYS=1; ====
C
C
C   �APAMETP�:     MSWALL - C�OPM�POBAHH�� C��COK C�OB COCTO�H��
C   ---------               MO�E�E� ��EMEHTOB �E��;
C                  VALLN, TOKN - HA�P��EH�� � "TOK�" �E�� �P�
C                                T = T0, T.E. B HA�A��H�� MOMEHT BPEMEH�
C                                B �OC�E�CTB�� - B TEK���� "CTAPTOB��"
C                                MOMEHT BPEMEH� T.E. B T = TN;
C                  VALLN1 � TOKN1 - TO �E ���  TN = TN + H0;
C                  YALL - MATP��� ���OB�X �POBO��MOCTE�;
C                  JALL - BEKTOP �CTO�H�KOB TOKA;
C                  LOGYTR - MACC�B �P��HAKOB K.�. ���OB;
C                           ( ��� MAKPOMO�E�E� ��EA��H�X BEHT��E� );
C                   WORKX - PA�. MACC�B PA�MEPHOCT�� NSYS;
C                   ��� METO�A PE�EH�� C�A� KODSYS=2 HEO�XO��MO:
C                   YALLHP(NDALL,NDALL) - KO��� YALL �
C                   WORKY( >= NSYS ) - PA�. MACC�B.
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   -------------  2. GETNEW, WRIT00, SVCHEK
C
C   �P�ME�AH�E:    1. �P� �ABEP�EH�� PAC�ETA ��A� FEND [MASPRT(55)] = 1;
C   ----------     2. �A�O�H�ET �O��: TN, H0, H1, H2, NSYS, ZEROCP, FEND
C                     KKCHEK      B "MASPRT";
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(1)
         COMMON /PROF3/ FUNTAB(1)
         DIMENSION RMASPT(1)
         DIMENSION MSWALL(1), TOKN(1), TOKN1(1), VALLN(1), VALLN1(1)
         DIMENSION YALL(1), JALL(1), LOGYTR(1), WORKX(1), YALLHP(1)
         DIMENSION WORKY(1)
         REAL JALL, MU
         INTEGER FEND, FKOM, PON, FRUN0
         EQUIVALENCE ( NNTOK, MASPRT(46) )
         EQUIVALENCE ( FRUN0, MASPRT(82) )
         EQUIVALENCE ( H0OLD, MASPRT(81) )
         EQUIVALENCE ( NDIVH0, MASPRT(84) )
         EQUIVALENCE ( SEQTIM, MASPRT(71) )
         EQUIVALENCE ( H0MIN,  MASPRT(72) )
         EQUIVALENCE ( NRUNGE, MASPRT(70) )
         EQUIVALENCE ( FRUNGE, MASPRT(68) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( MITER,  MASPRT(66) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( FREST,   MASPRT(45) )
         EQUIVALENCE ( TOUT,    MASPRT(44) )
         EQUIVALENCE ( FCHEK,   MASPRT(91) )
         EQUIVALENCE ( NFACHK,  MASPRT(97) )
         EQUIVALENCE ( KKCHEK,  MASPRT(96) )
         EQUIVALENCE ( DTCHEK,  MASPRT(90) )
         EQUIVALENCE ( METVET,  MASPRT(107) )
         EQUIVALENCE ( HXMIN,   MASPRT(62) )
         EQUIVALENCE ( EPSKOM,  MASPRT(39) )
         EQUIVALENCE ( GVENT6,  MASPRT(59) )
         EQUIVALENCE ( IPOL,    MASPRT(108) )
         EQUIVALENCE ( KODSYS,  MASPRT(109) )
         EQUIVALENCE ( EPSSYS,  MASPRT(110) )
         EQUIVALENCE ( KODMU,   MASPRT(113) )
         EQUIVALENCE ( KODMUM,  MASPRT(114) )
         EQUIVALENCE ( T0OUT ,  MASPRT(115) )
         EQUIVALENCE ( H0OUTT,  MASPRT(116) )
         EQUIVALENCE ( H0DATX,  MASPRT(117) )
         EQUIVALENCE ( OUTFLG,  MASPRT(118) )
C ----->
         INTEGER FITER, FRUNGE, FRUNDG, SEQTIM, FREST, FCHEK, OUTFLG
C--->  FRUGDG - ��A� ��PAB�EH��....
         EQUIVALENCE ( MASPRT(29), TN ),
     =               ( MASPRT(30), H0 ),
     =               ( MASPRT(31), MU ),
     =               ( MASPRT(32), H1 ),
     =               ( MASPRT(33), H2 ),
     =               ( MASPRT(40), H0DATA ),
     =               ( MASPRT(42), T0 ),
     =               ( MASPRT(43), TMAX ),
     =               ( MASPRT(50), ZEROCP ),
     =               ( MASPRT(55), FEND ),
     =               ( MASPRT(48), HOUTT ),
     =               ( MASPRT(13), NWTR ),
     =               ( MASPRT(14), NWTRE ),
     =               ( MASPRT(35), TXG ),
     =               ( MASPRT(61), HXG ),
     =               ( MASPRT(23), NDALL ),
     =               ( MASPRT(56), PON   ),
     =               ( MASPRT(34), FKOM )
C======================================================================
C**********************************************************************
C@@@@@@  C�POC "PAC�ET OKOH�EH":
       FEND = 0
C-->  B��OP KKCHEK �P� FCHEK = 1
       IF ( FCHEK .EQ. 0 ) GO TO 1
       KKCHEK = INT( DTCHEK/HOUTT )
       IF ( KKCHEK .LE. 0 ) KKCHEK = 1
       DTCHEK = KKCHEK * HOUTT
       WRITE( NWTR , 100) DTCHEK, KKCHEK
       WRITE( NWTRE, 100) DTCHEK, KKCHEK
100    FORMAT(/' *** �A� ��� "#ACOXP" �CTAHOB�EH PABH�M ', G12.5,
     = ' C. (�EPE� ', I5, ' TO�EK) ***'/)
1      CONTINUE
C@@@@@ KOHTPO�� @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( HXMIN .GT. H0DATA ) WRITE(NWTRE,3456) HXMIN
       IF ( HXMIN .GT. H0DATA ) HXMIN = H0DATA / 10.0
3456   FORMAT(/' *** �HA�EH�E HXMIN =',G12.5,' O�EH� �O���OE ***'/)
C@@@
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) WRITE(NWTRE,3457)
3457   FORMAT(/' *** �P� METVET=1 �A�AHO HXMIN=0 --> �EPEO�PE�E��ETC�',
     = ' HA 1% OT H0DATA ***'/)
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) HXMIN = H0DATA / 100.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  EC�� BXO� �O K.T., TO HA�. �CTAHOBK� HE �E�AT� !!!
        IF ( FREST .EQ. 1 ) GO TO 2
C???????????????????????????????????????????????????????????????????????
C-->  HA�A��H�E �CTAHOBK�....
       OUTFLG=0
       HOUTTX = HOUTT
       H0DATX = H0DATA
C �TO �A C�T�A���....
C  OUTFLG = 1 -  TO��KO OT�O�EHH�� B�BO� B SPOOL....
C  OUTFLG = 2 -  OT�O�EHH�� B�BO� B SPOOL + CMEHE HOUTT ( �EPE� T0OUT)
C  OUTFLG = 2 -  TO�E, �TO � "2" + CMEHA H0DATA.....
       IF ( T0OUT .LE. T0  ) GO TO 11110
C$$ �TO�� HE �PCKO��T� T0OUT.......
       T0OUT = T0OUT - H0DATA
C --> �P��OE HA�A�O BPEMEH� B�BO�A, E�O ��KC�P�EM....
       OUTFLG=1
C  ���EM �� MEH�T� �A� B�BO�A.....
       IF ( H0OUTT .EQ. 0.0 ) GO TO 11110
C �A, �POC�T HOB�� �A� B�BO�A, HA�O �� CMEH�T� H0DATA ???????
       OUTFLG = 2
       IF ( H0OUTT .LT. H0DATA ) H0DATX = H0OUTT
       IF ( H0OUTT .LT. H0DATA ) OUTFLG = 3
C  ��� OPEN SPOOL.....
       HOUTTX = H0OUTT
11110  CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C===>      MA��HH�� HO�� [ MAX 16777215 MANTISA ]
          H0000 = H0OUTT * 1.8E+4
          H0001 = H0DATX * 1.8E+4
        WRITE(NWTRE,2345) ZEROCP, H0DATA, H0MIN, METVET, HXMIN, EPSKOM,
     =  IPOL, KODSYS, EPSSYS, MU, KODMU, KODMUM, T0OUT, H0000,  H0001
2345  FORMAT(/' *** "MA��HH�� HO��" �CTAHOB�EH PABH�M  ', E20.10, ' ***'
     =  /    ' *** HA�A��H�� �A� �HTE�P�POBAH�� H0DATA', E20.10,' ***'/
     =    ' *** "�O�PE�HOCT�" CETK� B�BO�A H0MIN...', E20.10,' ***'/
     =    ' *** METO� O�PE�E�EH�� KOMM�TA��� METVET', I20,' ***'/
     =    ' *** MIN BPEM� �O�. KOMM�TA���     HXMIN', E20.10,' ***'/
     =    ' *** �HTEPBA� "PABH�E" KOMM�TA��� EPSKOM', E20.10,' ***'/
     =    ' *** METO� �HTEP�O����� HA [TN,TN1] IPOL', I20, ' ***'/
     =    ' *** KO� METO�A PE�EH�� "C�A�"    KODSYS', I20, ' ***'/
     =    ' *** TO�HOCT� PE�EH�� "C�A�"      EPSSYS', E20.10, ' ***'/
     =    ' *** KOHCTAHTA METO�A �HTE�P�POBAH�� MU ', E20.10, ' ***'/
     =    ' *** KO� METO�A "PA��OHA"         KODMU ', I20,  ' ***'/
     =    ' *** KO� METO�A "PA��OHA" ��� �-M KODMUM', I20,  ' ***'/
     =    ' *** BPEM� HA�A�A B�BO�A B SPOOL .......', E20.10, ' ***'/
     =    ' *** HOB�� �A� B�BO�A (��.�PA�) ........', E20.10, ' ***'/
     =    ' *** HOB�� �A� PAC�ETA (��.�PA�) .......', E20.10, ' ***'//)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C???????????????????????????????????????????????????????????????????????
C      OTKP�T� SPOOLX ....
       KOD = 1
       CALL WRIT00( KOD, HOUTTX, MSWALL, TOKN1, VALLN1 )
       MASPRT(26) = MASPRT(7)
C-->  �AH���T� ��A��....
       IF ( NSYS .EQ. 1 ) KODSYS=1
       TN = T0
       FEND = 0
       FRUNDG = 0
C-->  EC�� FRUN0 = 1 -> TECT P�H�E BCE��A HA �EPBOM �A�E....
       FRUN0 = 0
       H0OLD = 0.0
       NDIVH0 = 0
       FKOM = 0
       TOUT = T0 + HOUTT
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C??????IF( T0OUT .GT. T0 ) TOUT = T0OUT
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
       H0 = H0DATA
       H1 = H0
       H2 = 0.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( KODMU .EQ. 1 ) H1 = ( 1 - MU ) * H0
       IF ( KODMU .EQ. 1 ) H2 =  MU  * H0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  B�BO� �EPBO� TO�K� B SPOOLX
       KOD = 2
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
       IF( T0OUT .LE. T0 ) CALL WRIT00( KOD, TN, MSWALL, TOKN, VALLN )
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--> KOHE� HA�. �CT.
C
2      CONTINUE
C----------------------------------------------------------------------
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C--> OTKP�T�E �E�AT� � B�BO� �EPBO� TO�K� �� "N", TN -->
       KOD = 1
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN1, VALLN1 )
       KOD = 2
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN,  VALLN  )
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C----------------------------------------------------------------------
3      CONTINUE
C--> HE KOHE� �� �TO,  TOUT O���AEMOE BPEM� B�BO�A O�EPE�HO� TO�K�
       IF ( TOUT .GT. TMAX + HOUTT ) GO TO 99
C .......... METO�..
       H0 = H0DATA
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( KODMU .EQ. 1 ) GO TO 33333
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       H1 = H0
       H2 = 0.0
       IF ( FKOM .EQ. 1 ) GO TO 30
C--> "TPA�E���"
33333  H1 = H0 * (1.0 - MU )
       H2 = MU * H0
30     CONTINUE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C-->  "C�ET"
       HXG = H0
       TXG = TN + H0
C-->  �A�POC TECTA P�H�E �P� KOMM�TA���
       FRUN0 = FKOM
C----------------------------------------------------------------------
       CALL GETNEW(MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =      YALL, JALL, LOGYTR, WORKX, IRC, YALLHP, WORKY )
C      CALL SUBERR(3)
C----------------------------------------------------------------------
C  ****  �A� HXG C TN B��O�HEH *******
C   �HA�EH�� VALLN1  �  TOKN1  ���  T = TN + TXG  O�PE�E�EH�:
C
C   H E O � X O � � M O   "���T�" ....
C  =====================
C
C  A) �P� IRC = 2 �MEH���T� H0DATA, C�POC�T� TO�K� � �OBTOP�T�..
C        (�OKA ��HOP�P�ETC� )
C  B) �P� IRC = 3 -  HE �PO�E� TECT P�H�E,
C      ( �A� �MEH��EH, C�T�A��� �OKA ��HOP�P�EM )
C
C----------------------------------------------------------------------
C
C  PA��EPEMC� B B�BO�E � �O��OTOB�M HOB�� CTAPTOB�� TO�K�
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C-->  BPEM� PAC��TAHHO� TO�K�
       TXG = TN + HXG
C-----------------------------------------------------------------------
C �EPEXO� EC�� "OT�O�EHH��" B�BO�....
        IF ( OUTFLG .GT. 0  .AND. TXG .GE. T0OUT ) GO TO 11111
C???????
C--> "�A�OP" K BPEMEH� B�BO�A...
       HXOUT = ( TN - TOUT )  +  HXG
C-->  B�BO��T� �� HAM?
       IF ( ABS(HXOUT) .LE. H0MIN  ) GO TO 31
C-->  HET, H��HO ��  �HTEP�O��POBAT�?
       IF ( TOUT .LT. TXG ) GO TO 32
C-->  TO�KA �POME��TO�HA�....
       TN = TXG
C-->  �EPE��CAT� �� "N1" B "N"  � �PO�O���T�
       DO 40 I = 1, NSYS
40     VALLN(I) = VALLN1(I)
C
       DO 41 I = 1, NNTOK
41     TOKN(I) = TOKN1(I)
C
       GO TO 3
C----------------------------------------------------------------------
31     CONTINUE
C-->  M� B PA�OHE B�BO�A, B�BE�EM...
       KOD = 2
       TN = TXG
       DO 310 I = 1, NSYS
310    VALLN(I) = VALLN1(I)
       DO 311 I = 1, NNTOK
311    TOKN(I) = TOKN1(I)
C-->  �E�AT�
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN, VALLN )
C-->  B SPOOLX, HO CMEH�M TOUT..
       TOUT = TOUT + HOUTT
       IF ( OUTFLG .EQ. 0 )
     +       CALL WRIT00( KOD, TN, MSWALL, TOKN, VALLN )
C-->  HA HOBO�O..
       GO TO 3
C----------------------------------------------------------------------
32     CONTINUE
C-->  TO�K� B�BO�A �POCKO����, �HTEP�O��P�EM..
C-->  KOHTPO��
       IF ( TN .LT. TOUT ) GO TO 320
C--> C�O�
       WRITE(NWTRE, 3210) TN, H0, TOUT, HOUTT, TXG, HXG, HXOUT,OUTFLG
3210   FORMAT(/' *** C�O� C����� BPEMEH� B DIGITA.................***'/
     = ' *** TN=', E15.8, ' H0=',E15.8, ' TOUT=',E15.8, ' ***'/
     = ' *** HOUTT=',E15.8, ' TXG=',E15.8, ' HXG=',E15.8, ' ***'/
     = ' *** HXOUT=',E15.8, '--->  B���BA�TE �OP� CEP�E�..***'/
     = ' *** OUTFLG=',I14,  '--->  B���BA�TE �OP� CEP�E�..***'//)
       WRITE(NWTR, 3210) TN, H0, TOUT, HOUTT, TXG, HXG, HXOUT,OUTFLG
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       STOP 20
C----------------------------------------------------------------------
320    CONTINUE
C-->  �HTEP�O��P�EM B "N"  K  TO�KE  TOUT   HA �HTEPBA�E HXG....
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( IPOL .EQ. 0 ) GO TO 900
C-->> C�E�. �HTEP�O�. �� TN <-> TN1 B TOUT ( XPAH�T� B "N" )
       DTX = TOUT - TN
       D1X = (H1 - DTX) / HXG
       D2X = (H2 - DTX) / HXG
       DO 901 I = 1, NSYS
       VN1 = VALLN1(I)
       VN  = VALLN (I)
       VALLN (I) = D1X * VN1 + D2X * VN  - DTX * ( VN1 - VN )
901    CONTINUE
C---
       DO 902 I=1, NNTOK
       TTN1 = TOKN1(I)
       TTN  = TOKN (I)
       TOKN (I) = D1X * TTN1 + D2X * TTN - DTX * ( TTN1 - TTN )
902    CONTINUE
       GO TO 903
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
900    CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C
       DTX = ( TOUT - TN ) / HXG
C
       DO 321 I = 1,NSYS
       VN = VALLN(I)
       VN1 = VALLN1(I)
       DVNN1 = VN1 - VN
       VALLN(I) = VN + DVNN1 * DTX
321    CONTINUE
C
       DO 322 I = 1, NNTOK
       TTN = TOKN(I)
       TTN1 = TOKN1(I)
       DTTNN1 = TTN1 - TTN
       TOKN(I) = TTN + DTTNN1 * DTX
322    CONTINUE
C----------------------------------------------------------------------
903    CONTINUE
C-->  �TO BCE COOTBETCTB�ET BPEMEH� TOUT...
       TN = TOUT
       KOD = 7
       IF ( PON .EQ. 1 )   CALL DRPR01( KOD,  TN, TOKN, VALLN )
C-->  ��MEH�M  TOUT..
       TOUT = TOUT + HOUTT
        KOD = 2
       IF ( OUTFLG .EQ. 0 )
     +     CALL WRIT00(KOD, TN, MSWALL, TOKN, VALLN )
C-->  BOCTAHOB�M CTAPTOB�� TO�K� B "N"..
       TN = TXG
C-->  �� N1  B N......
       DO 323 I = 1,  NSYS
323    VALLN(I) = VALLN1(I)
C
       DO 324 I = 1, NNTOK
324    TOKN(I) = TOKN1(I)
C-->  HA HOB�H�KO�O...
       GO TO 3
C**********************************************************************
C**********************************************************************
C**********************************************************************
99     CONTINUE
C-->  PAC�ET OKOH�EH..... B TN � "N" �OC�E�H�� TO�KA..
       FEND = 1
C-->  EC�� "#ACOXP" HE ���O, TO �POCTO �AKPOEMC�..
       IF ( FCHEK .EQ. 0 ) GO TO 990
C-->  �E�ATAEM O K.T.
       KOD = 6
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN, VALLN )
C-->  K.T. CO��A��M..
       CALL SVCHEK( NFACHK, TN, MSWALL, TOKN, VALLN )
990    CONTINUE
C-->  CLOSE>>>
       KOD = 3
       CALL WRIT00(KOD, TN1,  MSWALL, TOKN1, VALLN1 )
       IF ( PON .EQ. 1 ) CALL  DRPR01(KOD, TN1, TOKN1, VALLN1 )
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
11111    CONTINUE
C--->  CMEHA B�B. XAPAKTEP�CT�K....
         TOUT = TXG
         WRITE(NWTRE,11114) TOUT
         WRITE(NWTR ,11114) TOUT
11114    FORMAT(/' *** B MOMEHT BPEMEH� "MO�E��" ', E12.5, ' (CEK.)'/
     +           ' *** CTAPTOBA� �PO�ECC HAKO��EH�� �HA�EH�� PE�EH��'/)
         IF ( OUTFLG .GE. 2 ) HOUTT  = H0OUTT
         IF ( OUTFLG .GE. 2 ) H0002 = H0OUTT * 1.8E+4
         IF ( OUTFLG .GE. 2 ) WRITE(NWTRE,11113) HOUTT,  H0002,  TXG
         IF ( OUTFLG .GE. 2 ) WRITE(NWTR ,11113) HOUTT,  H0002,  TXG
11113    FORMAT(/' *** BH�MAH�E: �A� B�BO�A �HA�EH�� �EPEO�PE�E�EH:'/
     *        ' *** HOUTT=',E15.5,' (CEK.), ',E15.5,' (��. �PA�.)'/
     *        ' *** BPEM� "MO�E��" TN = ',E15.5,' (CEK.)'/)
         IF ( OUTFLG .EQ. 3 ) H0DATA = H0DATX
         IF ( OUTFLG .EQ. 3 ) H0DATQ=H0DATX*1.8E+4
         IF ( OUTFLG .EQ. 3 ) WRITE(NWTRE,11112) H0DATX, H0DATQ, TXG
         IF ( OUTFLG .EQ. 3 ) WRITE(NWTR ,11112) H0DATX, H0DATQ, TXG
11112    FORMAT(/' *** BH�MAH�E: �A� �HTE�P�POBAH�� �EPEO�PE�E�EH:'/
     *        ' *** H0DATA=',E15.5,' (CEK.), ',E15.5,' (��. �PA�.)'/
     *        ' *** BPEM� "MO�E��" TN = ',E15.5,' (CEK.)'/)
C@@@@@ KOHTPO�� @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( HXMIN .GT. H0DATA ) WRITE(NWTRE,3456) HXMIN
       IF ( HXMIN .GT. H0DATA ) HXMIN = H0DATA / 10.0
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) WRITE(NWTRE,3457)
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) HXMIN = H0DATA / 100.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         OUTFLG = 0
         GO TO 31
C***** DEBUG INIT(TN,H0,TOUT, HOUTT,TXG,HXG, HXOUT)
       END
