         SUBROUTINE DM0036 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1988 �.)------------
C   �O��PO�PAMMA:  DM0036 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA�HA�EH�E:
C   -----------
C   HA�HA�EH�E:  MOH�TOP   MAKPOMO�E�� M�KPO�PO�ECCOPHO� C�CTEM�,
C   ----------   �OCTPOEHHO� HA �A�E M�KPO�PO�ECCOPA CEP��
C                K1801BM1(2,3), �ME��E�O C�CTEM� KOMAH� ��PM� "DEC".
C                ******************************************************
C                �P��HAK TIP # 5 -> �O�. MAKPOMO�E��.
C
C
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                   HE �E�AET H��E�O
C
C
C                  B) ZAPROS = 2
C                     ----------
C                   HE �E�AET H��E�O
C
C                                                                      )
C                  C) ZAPROS = 3
C                     -----------
C                     �O��BO��TC� PAC�ET �HA�EH�� B�XO�H�X C��HA�OB
C                     �O ��BECTH�M BXO�H�M C��HA�AM.
C                     �P� �TOM: FKOM = 0 - COCTO�H�E B�XO�H�X
C                                          C��HA�OB HE ��MEH��OC�;
C                               FKOM = 1 - HA TEK��EM �A�E COCTO�H�E
C                                         �O��HO "���O" ��MEH�T�C� B
C                                         B T = TX MOMEHT BPEMEH�;
C                                         PEA��HO B �PO�PAMME O�PE�E-
C                                         ��ETC� HX-�A� BO�MO�HO� CME-
C                                         H� COCTO�H��, �ATEM TX=TN+HX;
C                                         �P� HX < HXMIN [MASPRT(62)]
C                                         �P�H�MAETC� HX = HXMIN;
C                                         COCTO�H�E HX=0.0 B���BAET
C                                         �E�AT� COO��EH��;
C                                         �PO��BE�EH� COTBETCTB����E
C                                         ��MEHEH�� B MASPRT:
C                                          FKOM <== "1"
C                                         HXG = MIN( HX, HXG )
C                                         TXG = TN + HXG
C
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX[RMSW(10)] -
C               ( PABEHCTBO �POBEP�ETC� C TO�. �O "EPSKOM"[MASPRT(39)])
C                      ��KC�P�ETC� CMEHA COCTO�H�� B�XO�HO�O C��HA�A,
C                      B �POT�BHOM C���AE, �HA�EH�E B�XO�HO�O C��HA�A
C                      BOCCTAHAB��BAETC�;
C
C   METO�:         �O���ECKOE MO�E��POBAH�E
C   -----
C
C   �APAMETP�:     MSW, RMSW - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------      ( "OTPE�OK" MACC�B� MSWALL -> C.M. �/� GETMSW );
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  T  =  TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  TN + H0;
C                  TOKN  � TOKN1 - �HA�EH�E �EPEMEHH�X "COCTO�H��"
C                                  MO�E�� �P� TN  �  TN + H0;
C                          ( PA�MEPHOCT� -> CM. MASPRT(46) = NNTOK )
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 6;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. SEQSOS, ADEC**
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=  1, 2, 3, 4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST >=1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C
C
C                    "�OCTO�HHA�" �ACT� MSW
C                     ----------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I�HA�.  I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I        I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                   �OPMAT RMSW T�� # 380:
C                  -----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0032    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I TX - BPEM� BO�MO�HO�O �EPEK���EH��;
C    11      I     2        I  DM  I KX - �KA�ATE�� ��MEHEH�� COCTO�H��
C            I              I      I      �OPTA  B/B HA �A�E ;
C    12      I     3        I  FM  I TTAKT - �EP�O� TAKTOBO� �ACTOT� ;
C    13      I     4        I  FM  I P - ��A� OT�A�O�HO� �E�AT�:
C            I              I      I     P=0 - OT�A�O�HA� �E�AT� OTK�.
C            I              I      I     P=1 - OT�A�O�HA� �E�AT� BK�.
C    14      I     5        I  FM  I           PE�EPB
C    15      I     6        I  FM  I           PE�EPB
C    16      I     7        I  DM  I KX - �KA�ATE�� ��MEHEH�� COCTO�H��
C            I              I      I      �OPTA B/B HA �A�E ;
C    17      I     8        I  DM  I TT - TEK��EE BPEM� ;
C    18      I     9        I  DM  I R0 - CO�EP��MOE POH R0 �P� T=TT ;
C    19      I    10        I  DM  I R1 - CO�EP��MOE POH R1 �P� T=TT ;
C    20      I    11        I  DM  I R2 - CO�EP��MOE POH R2 �P� T=TT ;
C    21      I    12        I  DM  I R3 - CO�EP��MOE POH R3 �P� T=TT ;
C    22      I    13        I  DM  I R4 - CO�EP��MOE POH R4 �P� T=TT ;
C    23      I    14        I  DM  I R5 - CO�EP��MOE POH R5 �P� T=TT ;
C    24      I    15        I  DM  I R6 - CO�EP��MOE POH R6 �P� T=TT ;
C    25      I    16        I  DM  I R7 - CO�EP��MOE POH R7 �P� T=TT ;
C    26      I    17        I  DM  I FLC-�HA�EH�E ��A�A "C" CC� �P� T=TT
C    27      I    18        I  DM  I FLV-�HA�EH�E ��A�A "V" CC� �P� T=TT
C    28      I    19        I  DM  I FLZ-�HA�EH�E ��A�A "Z" CC� �P� T=TT
C    29      I    20        I  DM  I FLN-�HA�EH�E ��A�A "N" CC� �P� T=TT
C    30      I    21        I  DM  I FLT-�HA�EH�E ��A�A "T" CC� �P� T=TT
C    31      I    22        I  DM  I FLIO-�HA�EH�E ��A�A "I/O" CC�
C            I              I      I             �P� T=TT ;
C    32      I    23        I  DM  I IPORT  -CO�EP��MOE �OPTA B�BO�A
C    33      I    24        I  DM  I IPORT1 -CO�EP��MOE �OPTA BBO�A
C            I              I      I             �P� T=TT ;
C    34      I    25        I  FM  I ADRPROG-MAKC�MA��H�� A�PEC O�� ;
C    35      I    26        I  FM  I ADRPRTO-A�PEC �OPTA B�BO�A;
C    36      I    27        I  FM  I ADRPRTI-A�PEC �OPTA BBO�A ;
C    37      I    28        I  DM  I TO - CTAPOE BPEM� ;
C    38      I    29        I  DM  I R0 - CO�EP��MOE POH R0 �P� T=TO ;
C    39      I    30        I  DM  I R1 - CO�EP��MOE POH R1 �P� T=TO ;
C    40      I    31        I  DM  I R2 - CO�EP��MOE POH R2 �P� T=TO ;
C    41      I    32        I  DM  I R3 - CO�EP��MOE POH R3 �P� T=TO ;
C    42      I    33        I  DM  I R4 - CO�EP��MOE POH R4 �P� T=TO ;
C    43      I    34        I  DM  I R5 - CO�EP��MOE POH R5 �P� T=TO ;
C    44      I    35        I  DM  I R6 - CO�EP��MOE POH R6 �P� T=TO ;
C    45      I    36        I  DM  I R7 - CO�EP��MOE POH R7 �P� T=TO ;
C    46      I    37        I  DM  I FLC-�HA�EH�E ��A�A "C" CC� �P� T=TO
C    47      I    38        I  DM  I FLV-�HA�EH�E ��A�A "V" CC� �P� T=TO
C    48      I    39        I  DM  I FLZ-�HA�EH�E ��A�A "Z" CC� �P� T=TO
C    49      I    40        I  DM  I FLN-�HA�EH�E ��A�A "N" CC� �P� T=TO
C    50      I    41        I  DM  I FLT-�HA�EH�E ��A�A "T" CC� �P� T=TO
C    51      I    42        I  DM  I FLIO-�HA�EH�E ��A�A "I/O" CC�
C            I              I      I             �P� T=TO ;
C    52      I    43        I  DM  I IPORT  -CO�EP��MOE �OPTA B�BO�A
C    53      I    44        I  DM  I IPORT1 -CO�EP��MOE �OPTA BBO�A
C            I              I      I             �P� T=TO ;
C    54      I    45        I  FM  I  #�� - HOMEP ��EMEHTA;
C    55      I    46        I  FM  I  #CM - HOMEP TOKA ��EMEHTA #�� ;
C    56      I    47        I  LM  I  ATOKM - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEPA #CM ;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 56
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 47
C----------------------------------------------------------------------
C
C
C
C        OP�AH��A��� �AM�T� B PEA��HO� M�KPO�PO�ECCOPHO� C�CTEME
C        � B MO�E��P���E� EE �PO�PAMME (MACC�B� MEM � MEM1)
C        -------------------------------------------------------
C
C        OP�AH��A��� �AM�T� B PEA��HO� M�KPO�PO�ECCOPHO� C�CTEME
C
C
C               C�OBO
C           �A�T      �A�T                      �A�T
C        *---------+---------*               *---------*
C        ! CTAP��� ! M�A���� ! 000000        ! M�A���� ! 000000
C        ! CTAP��� ! M�A���� ! 000002        ! CTAP��� ! 000001
C        ! CTAP��� ! M�A���� ! 000004        ! M�A���� ! 000002
C        ! CTAP��� ! M�A���� ! 000006        ! CTAP��� ! 000003
C        ! CTAP��� ! M�A���� ! 000010        ! M�A���� ! 000004
C        ! CTAP��� ! M�A���� ! 000012        ! CTAP��� ! 000005
C        ! CTAP��� ! M�A���� ! 000014        ! M�A���� ! 000006
C        ! ................. ! ......        ! CTAP��� ! 000007
C        ! ................. ! ......        ! ....... ! ......
C        ! ................. ! ......        ! ....... ! ......
C        ! CTAP��� ! M�A���� ! 003774        ! M�A���� ! 003776
C        ! CTAP��� ! M�A���� ! 003776        ! M�A���� ! 003777
C        *---------+---------*               *---------*
C
C
C        OP�AH��A��� �AM�T� B MO�E��P���E� �PO�PAMME
C            (MACC�B�   MEM  �  MEM1 )
C     *-------------------------------------*
C     ! C�OBO C A�PECOM 000000              ! 0001
C     ! CTAP��� �A�T C�OBA C A�PECOM 000000 ! 0002
C     ! C�OBO C A�PECOM 000002              ! 0003
C     ! CTAP��� �A�T C�OBA C A�PECOM 000002 ! 0004
C     ! C�OBO C A�PECOM 000004              ! 0005
C     ! CTAP��� �A�T C�OBA C A�PECOM 000004 ! 0006
C     ! C�OBO C A�PECOM 000006              ! 0007
C     ! CTAP��� �A�T C�OBA C A�PECOM 000006 ! 0008
C     ! C�OBO C A�PECOM 000010              ! 0009
C     ! CTAP��� �A�T C�OBA C A�PECOM 000010 ! 0010
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! C�OBO C A�PECOM 003776              ! 2047
C     ! CTAP��� �A�T C�OBA C A�PECOM 003776 ! 2048
C     *-------------------------------------*
C
C
C----------------------------------------------------------------------
C
C
C     B�BO��M�E �HA�EH��:
C
C     I #.1 - I #.8  -  POH M�KPO�PO�ECCOPA ( B �EC�T��HOM B��E ) ;
C     I #.9          -  CC� M�KPO�PO�ECCOPA ( B �EC�T��HOM B��E ) ;
C     I #.10         -  CO�EP��MOE �OPTA B�BO�A( B �EC�T��HOM B��E ) ;
C     I #.11         -  BPEM�, ��� KOTOPO�O XPAH�TC� �KA�AHH�E �HA�EH��;
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         COMMON /MEMER/ MEM(2048), MEM1(2048)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADPORT, ADPROG
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(60), FTEST )
         INTEGER FTEST
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( HXG,    MASPRT(61) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FKOM,   MASPRT(34) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( RN, N )
         INTEGER FKOM, ZAPROS, TIP
         NOUT = 10
C--->  KOHTPO�� T��A
         TIP = MSW(2)
         IF ( TIP .EQ. 380 ) GO TO 11
C######################################################################
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), TIP
9998     FORMAT( ' *** �O�. C�O� B DM0036 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C######################################################################
11       CONTINUE
C---> B��OP PE��MA:
         GO TO ( 667, 666, 600, 500       ), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C======================================================================
C---------------Z A P R O S = 1----------------------------------------
667      CONTINUE
         KOD = 0
         RETURN
C======================================================================
C---------------Z A P R O S = 2----------------------------------------
666      CONTINUE
C---> C�POC BPEMEH�  TX - BO�MO�HO�O �EPEK���EH�� ...
         MSW(11) = 0
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- Z A P R O S = 3 ---- TOKN1  �  VALLN1  O�PE�E�EH� ---------------
600      CONTINUE
C---> �A�OMH�M CTAP�� TO�K�.
         RMSW(37) = RMSW(17)
         DO 602 I=1,16
         MSW(37+I) = MSW(17+I)
602      CONTINUE
         IADPRG = MSW(34)
         DO 605 I=1,IADPRG
         MEM1(I) = MEM(I)
605      CONTINUE
C-->     �TO B �OPT� BBO�A ?...
         MSW(33) = IFIX( TOKN1(MSW(56)))
C--> �PO�O���M B��O�H�T� ��PAB������ �PO�PAMM�.
         TN1 = TN + H0
CCCC     WRITE (6,*) MSW(33)
C400     FORMAT (8X,E15.6)
         CALL ADEC00( RMSW(17), TN1, MSW(13), NOUT, MSW(11), MSW(18),
     *   MSW(26), MSW(32), MSW(33),MSW(35),MSW(36), MSW(34) , IER )
         IF( IER .NE. 0 ) GO TO 9999
         RMSW(10) = RMSW(17)
         DO 422 I=1,8
         TOKN1(ITOK+I-1) =  MSW(17+I)*1.
422      CONTINUE
         ICCP = MSW(31)*128 + MSW(30)*16 + MSW(29)*8 +
     +          MSW(28)*4   + MSW(27)*2  + MSW(26)
         TOKN1(ITOK+8) =  ICCP*1.
         TOKN1(ITOK+9) = MSW(32)*1.
         TOKN1(ITOK+10)= RMSW(10)
CCCCC    WRITE (6,430) (TOKN1(ITOK+I-1),I=1,11)
C430     FORMAT (5X,F12.8)
         IF( MSW(11) .NE. 1 ) RETURN
         FKOM = 1
         HX = RMSW(10) - TN
         HXG = MIN( HX,HXG )
         TXG = TN + HXG
         RETURN
C######################################################################
500      CONTINUE
C--- Z A P R O S = 4  --- ��KCA��� CMEH� COCTO�H�� B�XO�HO�O C��HA�A.
C----------------------------------------------------------------------
         KOD = 0
         MSW(33) = IFIX(TOKN1(MSW(56)))
         IF( MSW(11) .EQ. 0 ) GO TO 520
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---> EC�� TXG  "�P�MEPHO" PABHO  TX MO�E��, TO CMEHA H��HA !
         TX = RMSW(10)
         EPST = ABS( TX - TXG )
         IF( EPST .GT. EPSKOM ) GO TO 510
C---> �HA�E CMEH�M COCTO�H�E...
         GO TO 520
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
510      CONTINUE
C---> B C�CTEME �PO��O��A KOMM�TA��� PAH��E, �EM � HAC. BEPHEMC� !!!
         RMSW(17) = RMSW(37)
         DO 515 I=1,16
         MSW(17+I) = MSW(37+I)
515      CONTINUE
         IADPRG = MSW(34)
         DO 516 I=1,IADPRG
         MEM(I) = MEM1(I)
516      CONTINUE
C-->  B��O�H�M ��PAB������ �PO�PAMM� �O MOMEHTA BPEMEH�  T=TXG
         CALL ADEC00( RMSW(17), TXG, MSW(13), NOUT, MSW(11), MSW(18),
     *   MSW(26), MSW(32), MSW(33),MSW(35),MSW(36), MSW(34) , IER )
         IF( IER .NE. 0 ) GO TO 9999
         RMSW(10) = RMSW(17)
C---> B�BE�EM TEK��EE COCTO�H�E M�-C�CTEM�
520      CONTINUE
         ITOK = MSW(7)
         DO 522 I=1,8
         TOKN1(ITOK+I-1) =  MSW(17+I)*1.
522      CONTINUE
         ICCP = MSW(31)*128 + MSW(30)*16 + MSW(29)*8 +
     +          MSW(28)*4   + MSW(27)*2  + MSW(26)
         TOKN1(ITOK+8) =  ICCP*1.
         TOKN1(ITOK+9) = MSW(32)*1.
         TOKN1(ITOK+10)= RMSW(10)
C****    WRITE (6,530) (TOKN1(ITOK+I-1),I=1,11)
C530     FORMAT (5X,F12.8)
         RETURN
C######################################################################
C-->  �E�AT� COO��EH�� O� O���KAX.
9999     CONTINUE
         NWTRE = MASPRT(14)
         GO TO ( 9991, 9992, 9993, 9994, 9995, 9996 ),IER
         RETURN
C----------------------------------------------------------------------
9991     CONTINUE
         WRITE(NWTRE,99991) MSW(1), TIP
99991    FORMAT( ' *** �O���ECK�� C�O� B ��PAB����E� �PO�PAMME ��� ',
     =   'M�KPO�PO�ECCOPHO� C�CTEM� '/
     =   ' *** O���O�HA� �A��C� KOMAH��,���O B�XO� �A �PE�E�� O�� ***'/
     =   ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9992     CONTINUE
         WRITE(NWTRE,99992) MSW(1), TIP
99992    FORMAT( ' *** �O���ECK�� C�O� B ��PAB����E� �PO�PAMME ��� ',
     =   'M�KPO�PO�ECCOPHO� C�CTEM� '/
     =   ' *** O�PA�EH�E K ��E�KE �AM�T�,�ME��E� HE�ETH�� A�PECC  ***'/
     =   ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9993     CONTINUE
         WRITE(NWTRE,99993) MSW(1), TIP
99993    FORMAT( ' *** �O���ECK�� C�O� B ��PAB����E� �PO�PAMME ��� ',
     =   'M�KPO�PO�ECCOPHO� C�CTEM� '/
     =   ' *** O���O�HA� �A��C� KOMAH��                           ***'/
     =   ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9994     CONTINUE
         WRITE(NWTRE,99994) MSW(1), TIP
99994    FORMAT( ' *** �O���ECK�� C�O� B ��PAB����E� �PO�PAMME ��� ',
     =   'M�KPO�PO�ECCOPHO� C�CTEM� '/
     =   ' *** �A�POC HA BBO� �AHH�X �� �OPTA BBO�A-B�BO�A        ***'/
     =   ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9995     CONTINUE
         WRITE(NWTRE,99995) MSW(1), TIP
99995    FORMAT( ' *** �O���ECK�� C�O� B ��PAB����E� �PO�PAMME ��� ',
     =   'M�KPO�PO�ECCOPHO� C�CTEM� '/
     =   ' *** �AHHA� KOMAH�A �OKA HE PEA���OBAHA                 ***'/
     =   ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9996     CONTINUE
         WRITE(NWTRE,99996) MSW(1), TIP
99996    FORMAT( ' *** �O���ECK�� C�O� B ��PAB����E� �PO�PAMME ��� ',
     =   'M�KPO�PO�ECCOPHO� C�CTEM� '/
     =   ' *** KO� KOMAH�� OTC�TCTB�ET B �EPE�HE KOMAH�           ***'/
     =   ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
         END
