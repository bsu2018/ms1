         SUBROUTINE  CALLDM( KF, MSWALL, YALL, JALL, LOGYTR ,
     *                       TOKN, TOKN1, VALLN, VALLN1, KODM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT OKT��P� 1987 �.)------------
C   �O��PO�PAMMA:  CALLDM( KF, MSWALL, YALL, JALL, LOGYTR ,
C   -------------         TOKN, TOKN1, VALLN, VALLN1, KODM )
C
C   HA�HA�EH�E:    �OC�E�OBATE��H�� B��OB DM MOH�TOPOB ��� ��EMEHTOB,
C  -----------     MAKPOMO�E�� KOTOP�X �ME�T PA����H�E �HA�EH��
C                  �P��HAKA T��A ( B �AB�C�MOCT� OT KF - �A�POCA );
C
C
C   METO�:         CKAH�POBAH�E � O�POC DM MOH�TOPOB CO��ACHO C��CKA
C   -----           MSWALL;
C
C   �APAMETP�:     KF - KO� �A�POCA HA �E�CTB�E:
C   ---------         = 1 O�POC BCEX DM MOH�TOPOB;
C                     = 2 O�POC DM MOH�TOPOB, MO�E��P�EM�X MAKPO-
C                         MO�E��M� C �P��HAKAM� T��A #5 � #6
C                         ( "�EPEK���ATE��H�E" ��EMEHT� );
C                     = 3 O�POC DM MOH�TOPOB TO��KO BEHT���H�X
C                         ��EMEHTOB ( �P��. T��A #6);
C                     = 4 O�POC DM MOH�TOPOB TO��KO ��� ��EMEHTOB
C                         "�OCTO�HHO�" CTP�KT�P� ( #1, #2, #3, #4 );
C                     = 5 O�POC DM MOH�TOPOB TO��KO ��� ��EMEHTOB
C                         "�OCTO�HO�" � "TO�O�O���ECK� �OCTO�HHO�"
C                          CTP�KT�P� ( #1, #2, #3, #4, #5 );
C                     = 6  O�POC DM MOH�TOPOB TO��KO ��� ��EMEHTOB
C                           C �P��. T��A # 5 ;
C
C                        MSWALL - MACC�B C��CKA ��P. C�OB ��EMEHTOB.
C
C
C                  TOKN - MACC�B �HA�EH�� "TOKOB" � �P. �EPEMEHH�X
C                         COCTO�H�� MO�E�E� ��EMEHTOB ��� TN MOMEHTA
C                         BPEMEH�;
C                  TOKN1 - TO �E, HO ��� TN = TN + H0 MOMEHTA BPEMEH�;
C
C                  VALLN - �HA�EH�� ���OB�X HA�P��EH�� B TN;
C                  VALLN1 - �HA�EH�� ���OB�X HA�P��EH�� B TN + H0;
C                  KODM - �APAMETP O�PATHO� CB��� MOH�TOPOB,
C                         B���PAETC� KAK MAX OT KOD - BCEX
C                         B��BAH�X MOH�TOPOB;
C                  YALL   - MATP��A �POBO��MOCTE�;
C                  VALL   - BEKTOP ���OB�X HA�P��EH�� ( �CKOM�� );
C                  JALL   - BEKTOP �CTO�H�KOB TOKA BETBE�;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB;
C
C             ====== BCE �APAMETP�, KPOME KF �EPE�A�TC� DM MOH�TOPAM,
C                    �P��EM: �EPE�A�TC� "OTPE�K�" (INTEGER + REAL )
C                    ******  MSW_ALL ��� �AHHO�O ��EMEHTA, A TAK �E
C                            BEC� MACC�B MSW_ALL ( INT. + REAL ) �
C                            A�PEC "OTPE�KA" B MSW_ALL C��CKE TEK��E�O
C                            C�OBA ( ��� O�PA�AT�BAEMO�O ��EMEHTA );
C            ========
C
C
C   �O��PO�PAMM�: 1. HA�OP �/� C �PE��KCOM DM.....;
C   ------------  2. COMMON PROF1;         SUBERR;
C
C  �P�ME�AH�E:    1. HA����E MOH�TOPOB DM..... HE �POBEP�ETC�;
C  ----------     2. �P� KF > 1 - B C���. OTC�TCTB�� ��EMEHTOB
C                    C �.T��A B COOTBETCTB�� C �A�POCOM, B�BO� COO�-
C                    �EH�� � BO�BPAT B B���BA���� �PO�PAMM�;
C                 3. PEA���OBAHO O�PA�EH�E K 41 �/� DM.....;
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSWALL(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION TOKN(*), TOKN1(*), VALLN(*), VALLN1(*)
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE ( T5MSW, MASPRT(41) )
         EQUIVALENCE ( T6MSW, MASPRT(49) )
         EQUIVALENCE ( T4MSW, MASPRT(93) )
         INTEGER T5MSW, T6MSW, T4MSW
         REAL JALL
         INTEGER ADRN,  TIP,  DM, PTIP
         KODM = 0
         KD = 0
C===> MAX - MAX �O�. HOMEP B MSWALL
         MAX = MSWALL(3)
C
C==>  B��OP KF �A�POCA, IEND = A�P. �OC�. C�OBA B MSWALL;
C
         IEND = 0
         GO TO (100, 200, 300, 400, 500, 600 ), KF
C!!!-------------------------------------------------------------------
         NWTRE=MASPRT(14)
         WRITE(NWTRE,2102) KF
2102     FORMAT(' *** O���KA B��OBA CALLDM, KF = ', I5,' ***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
100      CONTINUE
C===> HA�. �CTAHOB. TEK. �KA�. ADRN - �KA�. C�. �BEHA:= <A.NEXT><MSW>
         ADRN = 6
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
200      CONTINUE
C===> �POCMOTP C �.T��A #5 ( T5MSW ) T.E.    �.T�� #5  � �.T�� # 6
         ADRN = T5MSW
         IF ( ADRN .NE. 0 ) GO TO 900
C===>  MO�ET ECT� �.T�� # 6 ?
         GO TO 300
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
201      NWTRE = MASPRT(14)
         WRITE(NWTRE,2103) KF, ADRN, IEND
2103     FORMAT(' *** �A�POC B��OBA MOH�TOPOB B CALLDM HE �O�TBEP��AETC�
     = ***' / ' *** KF = ', I5, ' ADRN = ', I5, ' IEND = ', I5, ' ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
300      CONTINUE
C===> �POCMOTP C �.T��A #6 ( T6MSW)
         ADRN = T6MSW
         IF ( ADRN .NE. 0 ) GO TO 900
         GO TO 201
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
400      CONTINUE
C===> �POCMOTP �O �.T��A #5 ( �O T5MSW )
         IEND = T5MSW
         ADRN = 6
         IF ( ADRN .EQ. IEND  ) GO TO 201
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
C===> �POCMOTP �O �.T��A #6 ( �O T6MSW )
         ADRN = 6
         IEND = T6MSW
         IF ( ADRN .EQ. IEND ) GO TO 201
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C===>  �POCMOTP TO��KO �.T��_A # 5
         ADRN = T5MSW
         IEND = T6MSW
         IF ( ADRN .EQ. 0 ) GO TO 201
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$   �O��OTOBKA �
C$   B��OB               M E T K A  # 900
C$   MOH�TOPOB DM....    -----------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
900      CONTINUE
         KODM = MAX0( KD, KODM)
C===> HE �OC�E�H�� �� �TO MSW?
         I1 = MSWALL(ADRN)
         IF ( I1 .EQ. 0 ) GO TO 666
C===>  HE �OC�E�HEE �� �TO C�OBO B MSWALL, �� �AKA�AHH�X
         IF ( ADRN .EQ. IEND ) GO TO 666
C===> KOHTPO�� C�O�..
         IF ( I1 .GT. MAX ) GO TO 667
C===> A�P. TEK. MSW ==> IM  ( 1-E C�OBO - �KA. HA C�E�. MSW ):
         IM = ADRN + 1
C===> A�P. C�E�. MSW:
         ADRN = I1
C===> B��OB MOH�TOPOB CO��ACHO HOMEPOB MOH�TOPOB DM <=========
C$$$$$$$$TIP = MSWALL(IM + 1)
         DM = MSWALL(IM + 2 )
         KD = 0
C===> � � O � A � � H � E   � E P E X O � � ==========
         GO TO (  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
     =           11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
     =           21,  22,  23,  24,  25 , 26,  27,  28,  29,  30,
     =           31,  32,  33,  34,  35 , 36,  37,  38,  39,  40,
     =           41,  42, 43                                     ), DM
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE,2101) DM
2101     FORMAT(' *** �O�. C�O� B CALLDM -> CTPAHH�� # DM MOH�TOPA   ***
     ='/ ' ***  DM  = ', I10, ' ***')
C!!!-------------------------------------------------------------------
         GO TO 667
667      CONTINUE
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 2100) ADRN, I1, MAX
2100     FORMAT(' *** �O�. C�O� B CALLDM -> MSW_ALL ��OXO� ***'/
     =   ' *** ADRN = ', I10, ' I1 = ', I10, ' MAX = ', I10, ' ***')
C********CALL PRINT1(MSWALL)
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$  C�CTEMA C�OPM�POBAHA      M E T K A  # 666
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C***  B���BA����K MOH�TOPOB DM..... ***********************************
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
1     CALL DM0001(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
2     CALL DM0002(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
3     CALL DM0003(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
4     CALL DM0004(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
5     CALL DM0005(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
6     CALL DM0006(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
7     CALL DM0007(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
8     CALL DM0008(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
9     CALL DM0009(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
10    CALL DM0010(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
11    CALL DM0011(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
12    CALL DM0012(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
13    CALL DM0013(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
14    CALL DM0014(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
15    CALL DM0015(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
16    CALL DM0016(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
17    CALL DM0017(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
18    CALL DM0018(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
19    CALL DM0019(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
20    CALL DM0020(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
21    CALL DM0021(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
22    CALL DM0022(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
23    CALL DM0023(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
24    CALL DM0024(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
25    CALL DM0025(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
26    CALL DM0026(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
27    CALL DM0027(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
28    CALL DM0028(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
29    CALL DM0029(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
30    CALL DM0030(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
31    CALL DM0031(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
32    CALL DM0032(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
33    CALL DM0033(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
34    CALL DM0034(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
35    CALL DM0035(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
36    CALL DM0036(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
37    CALL DM0037(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
38    CALL DM0038(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
39    CALL DM0039(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
40    CALL DM0040(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
41    CALL DM0041(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
42    CALL DM0042(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
43    CALL DM0043(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
         END
