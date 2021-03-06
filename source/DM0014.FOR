         SUBROUTINE DM0014 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0014 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  MO�E�� #40 --> PE����TOP BO�����EH�� ��� ��. MA��H�:
C                                MO�E�� # 11, 110, 12, 120;
C
C
C
C              ��EMEHT O��C�BAETC�:
C
C      � # T 40 � KA KB KC
C          � T_BK� U_�OTO�. #��-TA_��._MA��H�_3X3X0
C              PSUB #P < .........>;
C
C              �EPE� A��APAT PSUB �A�A�TC� �AKOH� ��PAB�EH�� "H��E";
C
C      ��E
C              KA, KB, KC - ���� �E��, �P�H�T�E �A O�OPH�� C�CTEM�
C                           HA�P��EH�� ( CET� �ECKOHE�HO� MO�HOCT� );
C              T_BK� - BPEM�, HA��HA� C KOTOPO�O BK�. PE�. BO����.
C              U_�OT. - �OTO�O�HOE HA�P��EH�E HA B�XO�E PE����TOPA
C                      ( O�PAH���BA�TC� "C�H�CO���" B�X. HA�P��EH�� );
C              #��. MA�. - HOMEP ��EMEHTA, O��C�BA���� ��. MA��H�,
C                          KOTOP�� PE����P�EM;
C
C
C***********
C BHH�MAH�E:  MOH�TOP KOPPEKTHO O�PE�E��ET OMEGA_O�OPHA� � ���OBOE
C              �O�O�EH�E O�OPHO�O BEKTOPA C�CTEM� ( �A�ABAEMO�O
C              KAK CET� �ECKOHE�HO� MO�HOCT�, ���AM� KA, KB, KC ),
C              TO��KO �P� �C�OB�� HE PABEHCTBA H��� M�HOBEHH�X
C              ���OB�X HA�P��EH�� O�HOBPEMEHHO.
C
C  !!!           B C���AE PABEHCTBA 0.0 �T�X BE����H �CTAHAB��BAETC�
C                                                    ---------------
C               OMEGA_O�OPHO�_C�CTEM� = 314.15927,
C               COS( OMEGA*TN1 ) = 1.0,
C               SIN( OMEGA*TN1 ) = 0.0;
C  !!!
C              ��� O�EC�E�EH�� B��O�HEH�� �TO�O �C�OB��:
C              1. �A�ABAT� T_BK� HE PABHOE T0 (BPEMEH� HA�A�A PAC�ETA);
C                              � � �
C              2. �A�ABAT� HA�A��HOE �HA�EH�E ���OB�X HA�P��EH�� HA
C                 ���AX KA, KB, KC;
C
C              3.  B O � M O � H O ------------>
C                 KO��POBAT� HOMEP ���A KA PABH�M 0.
C                 ----------------------------------
C                 �P� �TOM C��TAETC� OMEG_O�OPHA� = 314.15927,
C                 �A�A = 0 ( B MOMEHT BK���EH�� "PE�. BO�����EH��" )
C
C
C***********
C
C      B�BO�H�E BE����H� B MACC�BE TOKN/TOKN1:
C
C      U_R_��_A, U_R_��_B, U_R_��_C,  -->  HA�P��EH�� ��PAB�EH��
C       (#1)      (#2)       (#3)          B C�CTEME OCE� POTOPA;
C     OME�A_O�POHA�, S_CKO���EH�E,         !!! BBE�EHO �MHO�EH�E HA
C          (#4)         (#5)               !!! KO��. 2/3 �P� �EPEXO�E
C    U_Q_��P.  U_D_��P;                    !!! OT �A�H�X K O�OPH�M
C     (#6)      (#7)                       !!! B Q-D C�CTEME BEKTOPAM;
C                                          !!!!!!!!!!!!!!!!!!!!!!!!!!!!
C  �O�O�H�TE��HO ��KC�P�ETC�:==>
C
C   U_S_Q_0,   U_S_D_0,  I_S_Q_0,  I_S_D_0,
C     (#8)      (#9)      (#10)     (#11)
C   U_R_Q_0,   U_R_D_0,  I_R_Q_0,  I_R_D_0
C    (#12)      (#13)     (#14)     (#15)
C   I_S_D_�A�,   S_CKO���EH�E_�A�,  INT(ISD-ISD_ZAD),  INT(S-S_ZAD),
C    (#16)       (#17)               (#18)               (#19)
C    PE�EPB;
C    (#20)                                 -->   BCE�O 20 �HA�EH��;
C
C*****  �AHHA� MO�E�� O�C����BAET MAKPOMO�E�� C T��.# 11, 110, 12, 120;
C
C
C                  A) ZAPROS = 1
C                  -------------
C                    C�POC ��A�A FX - BE����H� ��P. HA�P. O�PE�E�EH�;
C                  === H� �E�O HE PAC��T�BAEM ==
C
C                  B) ZAPROS = 2
C                     ----------
C                      == B C���AE H��EBO�O �HA�EH�� ��A�A FQ -->
C                  B���C�EH�E �HA�P��EH�� ��PAB�EH�� B C�CTEME OCE�
C                  POTOPA MA��H�;
C
C
C              FQ - PAC�ET ��P. HA�P��EH�� B��O�HEH -->
C                  MEXAH��M �TO�O ��A�A �O�BO��ET C�HXPOH���POBAT�
C                  B��O�HEH�� PA����H�X DM_MOH�TOPOB C �.T��A # 4;
C
C                  �AKOH� ��PAB�EH�� --> �EPE� �O��PO�PAMM� �O���OBATE��
C
C
C                  C) ZAPROS = 3
C                     **********
C                     HE �O��EP��BAETC� ( PE�EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE �O��EP��BAETC�;
C
C   METO�:         O�PE�E�EH�E OMEGA_O�OPHO� C�CTEM�, PA��O�EH�E
C   -----          BCEX BE����H (TOK, HA�P��EH�E) B �TO� C�CTEME,
C                  B���C�EH�E CKO���EH�E � ��P. HA�P. (�EPE� PSUB)
C                  �POEK��� BEKTOPA HA�P. ��PAB. HA OC� POTOPA;
C
C
C      1) .........��� O��CAH�� �AKOHOB ��PAB�EH�� PEA���OBAHA PSUB1-->
C
C      PSUB 1 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD XAF   >;
C              ( 8-M� �APAMETPOB )
C
C
C      2) .........��� O��CAH�� �AKOHOB ��PAB�EH�� PEA���OBAHA PSUB2-->
C
C      PSUB 2 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD  XAF
C
C      KTI  TI1 ISZAD_1  TI2 ISZAD_2 .....
C                        I         I
C                        I<---*--->I
C
C      KTS  TS1 S_1      TS2    S_2  ...... >;
C                        I         I
C                        I         I
C                        I<---*--->I
C
C
C    ��E:
C          -*-  �BA �HA�EH��, O��C�BA���E O�H� TO�K� O�PE�E�EH��
C               COOTBETCTB���E� ��HK��OHA��HO� �AB�C�MOCT� -->
C               OHA �A�AETC� KAK K�CO�HO-��HE�HA� ��HK���;
C
C       KTI  - KO�-BO TO�EK ��� O�PE�E�EH�� �AB�C�MOCT� ISZAD OT "TN";
C
C       KTS  - KO�-BO TO�EK ��� O�PE�E�EH�� �AB�C�MOCT� SZAD  OT "TN";
C
C  !!! FM0010 KTI � KTS ��BA�BAET !!!
C
C   B �PO��BO��H�� �/� �O���OBATE�� �EPE�AETC� C�E����EE:
C
C  �APAMETP�:   NEL - HOMEP ��EMEHTA, ��� KOTOPO�O PSUB PA�OTAET;
C  ---------    URMAS - MACC�B �AHH�X �O���OBATE��: (��� PSUB1):
C               *****
C       C�OBO        �HA�EH�E
C      1               8 - KO�-BO �APAMETPOB B C��CKE URMAS
C      2               KE         �HA�EH�E
C      3               URD00      �HA�EH�E
C      4               KI         �HA�EH�E
C      5               ISDZAD     �HA�EH�E
C      6               URQ00      �HA�EH�E
C      7               KS         �HA�EH�E
C      8               BETA       �HA�EH�E
C      9               SZAD       �HA�EH�E
C      10              XAF        �HA�EH�E
C  - - - - - - -- - - - - - - - - - - - - - - - - - - - - -
C     �A�EE ---> AHA�O���HO CO��ACHO
C                C��CKA �AKO��POBAHH�X B O��CATE�E PSUB <....>
C                �HA�EH��, �CK���A� �KA�AHH�E;
C
C
C              STMAS - MACC�B �AHH�X, �EPE�ABAEM�X B PSUB:
C              *****
C      C�OBO           O�O�HA�EHE   -   �HA�EH�E
C      1               TN - BPEM� N - �O �A�A PAC�ETA;
C      2               H0 - �A� �HTE�P�POBAH��;
C-��� TN+H0***��� TN ***
C   I           I
C   Y           Y
C   13         3       ISQ0 - TOK CTATOPA Q OC� O�OPHA�;
C   14         4       ISD0 - TOK CTATOPA D OC� O�OPHA�;
C   15         5       USQ0 - HA�P��EH�E CTATOPA Q OC� O�OPHA�;
C   16         6       USD0 - HA�P��EH�E CTATOPA D OC� O�OPHA�;
C   17         7       IRQ0 - TOK POTOPA Q OC� O�OPHA�;
C   18         8       IRD0 - TOK POTOPA D OC� O�OPHA�;
C   19         9       OMEG0 - O�OPHA� �ACTOTA;
C   20        10       OMEGA - ���OBA� CKOPOCT� POTOPA;
C   21        11       GAMMA - ��O� ME��� CTATOP. � POTOP. �A�A "A";
C   22        12       S - CKO���EH�E;
C------------------------
C   14 *--* 22 - TO �E ��� T = TN+H0;
C   23, 24, 25 -  P E � E P B;
C
C------------------------------------------------------
C
C
C          DGMAS  -  MACC�B B��EC�EHH�X �HA�EH�E HA�P��EH��
C          *****                        ��PAB�EH�� B Q_D O�.
C
C   C�OBO        O�O�HA�EHE - �HA�EH�E:
C   1          UYQ0    - ��P. HA�P��. �O Q OC� O�. C�CTEM�;
C   2          UYD0      - ��P. HA�P��. �O D OC� O�. C�CTEM�;
C   3          UYMOD     - MO���� BEKTOPA ��P. HA�P��EH��;
C   4          I_S_D_ZAD - �A��AHHOE ISD;
C   5          S_ZAD     - �A�AHHOE CKO���EH�E;
C -----------------------------------------------------
C
C              INTMAS - MACC�B ��� PEA���A��� �HTE�PATOPOB,
C              ******  ( 5-T� C�OB );
C INT(TOK) � INT(S) XPAH�T�C� B TOKN/N1 #18 � #19; ( �EPB�E 2-A C�OBA);
C
C  PSUB1 --->  B���C��ET:
C  *****
C     UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* ( KE+1 ) - KE*IRD0 * XAF;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]* ( KE+1 ) - KE*IRQ0 * XAF;
C
C  ��E:  INT - �HTE�PA�;
C
C
C  PSUB2 --->  B���C��ET:
C  *****
C     UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* ( KE+1 ) - KE*IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]* ( KE+1 ) - KE*IRQ0;
C
C  ��E:  INT - �HTE�PA�, A ISDZAD � SZAD - K�CO�HO-��HE�HA� ��HK���
C                                                     B P E M E H �
C                                                     *************
C
C
C   �APAMETP�:     MSW, RMSW - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------      ( "OTPE�OK" MACC�B� MSWALL -> C.M. �/� GETMSW );
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          B T = TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          B T = TN + H0;
C                  TOKN  �  TOKN1  -  �HA�EH�� "TOKOB" �P� T :=
C                                     TN  �  TN + H0;
C                                   PA�MEPHOCT� CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 5;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C                   ( =1 �P� �O���O� �OK. �O�PE�HOCT� );
C                 MSWALL( (RMSWAL ) - BEC� C��COK MSW_ALL,
C                 ADRMSW - A�PEC TEK. C�OAB MSW ( RMSW );
C
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST, CALL_PS --> B��.  PSUB �/�;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C
C              --->  OT 3.03.87 �OP�, �P� HE BK�. APB KO��POBAH�E
C              --->                   �� TOKN  B TOKN1
C              --->  �P���HA:  �P� "#�C�ET" EP�H�A B TOKN1 (?)
C              ---> HO �TO HA �BM "BH���", � HAC BCE O'KEY
C              --->          C T P A X O B K A !!!!!!!
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 40:
C                --------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C
C              �OPMAT RMSW T�� # 40:
C              ---------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I       O�O�HA�EH�E - �HA�EH�E
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I
C O��CAH��   I    FM0010    I      I
C            I              I      I
C------------I--------------I------I-----------------------------------
C     9      I    -2        I      I HOMEP ���A KA ( �A�A A )
C    10      I    -1        I      I HOMEP ���A KB ( �A�A B )
C    11      I     0        I      I HOMEP ���A KC ( �A�A C )
C----------------------------------------------------------------------
C    12      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4
C    13      I     2        I  FM  I  NELT - HOMEP ��. PE����P. ��. MA�.
C    14      I     3        I  LM  I  AMOT - A�P. B MSWALL O��CAH��
C            I              I      I         PE�. ��. MA��H�
C    15      I     4        I  LM  I  APOT - A�PEC B MSWALL ���OB MA��H�
C    16      I     5        I  LM  I  ATOKM - A�P B TOKN/N1 BE����H,
C            I              I      I          PAC��T. ��� ��. MA�.
C    17      I     6        I  FM  I  TBKL - BPEM�, HA��HA� C KOTOPO�O
C            I              I      I         BK���AETC� PE����TOP;
C    18,19   I     7,8      I      I  PE�EPB.
C    20      I     9        I  FM  I  UPOT - �OTO�O�HOE HA�P��EH�E
C    21      I     10       I  FM  I  NSUB - HOMEP �/� �O���OBATE��
C22,23,..,26 I11,12,13,14,15I      I  PA�. MECTO ��� �HT. 1 � 2 B TOKN/1
C  27        I    16        I  FM  I  NPAR - KO�-BO �APAMETPOB,
C            I              I      I         BBE�EHH�X B PSUB<..>
C OT 28 �O   I OT 17 �O     I FM   I  �APAMETP� PSUB, �A�AHH�E
C 28 + NPAR  I 17 + NPAR    I      I  �O���OBATE�EM
C            I              I      I
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 28+NPAR
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 17+NPAR
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 11, 110, 12, 120:
C                ----------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C  +--------------------------------------------------------------+
C  I ���� �O�K���EH�� S1N S2N S3N F1N F2N F3N S1K S2K F1K F2K F3K I
C  I           12-T� C�OB: OT 9 �O 20                             I
C  +--------------------------------------------------------------+
C
C              RMSW T�� # 11, 110, 12, 120 [ ��. MA��HA 3X3X0 ] :
C              ************************************************
C
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PE�EPB I"DOM"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 5-T�   I ��� I      I      I       I       I       I       I     I
C  I  C�OB  IMEX. I 9-T� I 9-T� I 36-T� I 36-T� I 6-T�  I 6-T�  I NMASI
C  I        I�P-� I C�OB I C�OB I C�OB  I C�OB  I C�OB  I C�OB  I C�OBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I        I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - O�PE�E��ETC� PA�MEPOM MAS <...> ==> ...+ NMAS + 1
C
C
C
C
C      DATA D1, D2, D3, D4, D5, D6,  D7,  D8
C    =  /   21, 27, 36, 45, 81, 117, 123, 129 /
C
C--->  D@ -> HOMEPA �O�E� B MSW_ALL MACC�BA;
C
C
C     B MACC�BE TOKN � TOKN1 ��� ��. MA��H� CO�EP��TC�:
C     *************************************************
C    OT 1 �O 6   - �HA�EH�� TOKOB �A� O�MOTOK: S1 S2 S3 F2 F2 F3;
C
C       7        -  �HA�EH�E GAMMA -> ��O� �O�O�EH�� POTOPA;
C
C       8        -  �HA�EH�E OMEGA -> ���OBA� �ACTOTA BPA�EH�� POTOPA
C                                     ( B O�HO�EP�O�HO� MO�E�� );
C       9        -  �HA�EH�E MFT - ��. MA�. MOMEHTA;
C
C      10        -  �HA�EH�E M_BHE�(T) - BHE�H�� HA�P��O�H�� MOMEHT;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL K2D3
         REAL JALL, JNEL, MAXITR
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER, TIP, ZAPROS
         REAL ISQ0N,  ISD0N,  IRQ0N,  IRD0N
         REAL ISQ0N1, ISD0N1, IRQ0N1, IRD0N1
         DIMENSION ULN(6), ULN1(6), STMAS(25), DGMAS(5)
         DATA K2D3 / 0.666667 /
         DATA KXALL, SQ3D2, R120, R240 /6, 0.8660254, 2.0944, 4.1888 /
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       IF ( TIP .EQ. 40 ) GO TO 9900
C
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0014 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C
9900   CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       TBKL = RMSW(17)
       TN1 = TN + H0
       IF ( TN1 .GT. TBKL )  GO TO  6666
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  BCTABKA OT 03.03.87 (�OP�)
       IF ( ZAPROS .EQ. 1 ) RETURN
       ITOKX = MSW(07) - 1
       DO 6667 I=1,20
       TOKN1( ITOKX+I ) = TOKN( ITOKX+I )
6667   CONTINUE
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  PE����TOP BK���EH.....
C--->  B��OP PE��MA �A�POCA..
6666   GO TO( 100, 200), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C-->
C==> C�POC ��A�A       OCHOBHO� PAC�ET B��O�HEH;
       MSW(12) = 0
C
       RETURN
C######################################################################
200    CONTINUE
C-->  H��HO �� PA�OTAT� ?
       IF ( MSW(12) .EQ. 1 ) RETURN
C-->  �P��ETC� .............................
C-->  ZAPROS = 2 --> PAC��TAT� �HA�EH��....
C-->  ����..
       KA = MSW(9)
       KB = MSW(10)
       KC = MSW(11)
C-->  A�P. TOKA B TIP # 40
       ITOK = MSW(7)
C**********************************************************************
       IF ( KA .NE. 0 ) GO TO 105
C-->  OMEG0 -> O�OPHA�  PABHA 314.15927, �A�A 0.0===>
       OMEG0 = 314.15927
C-->  ��O�.....
C$$$$$$COPN = COS(OMEG0*TN   )
C$$$$$$SOPN = SIN(OMEG0*TN   )
C
       COPN1 = COS(OMEG0*TN1 )
       SOPN1 = SIN(OMEG0*TN1 )
C
       GO TO 106
C----------------------------------------------------------------------
105    CONTINUE
C-->  O�OP. �ACTOTA
       VAN = 0.0
       VBN = 0.0
       VCN = 0.0
C
       VAN1 = 0.0
       VBN1 = 0.0
       VCN1 = 0.0
C
       IF ( KA .EQ. 0 ) GO TO 101
       VAN = VALLN(KA)
       VAN1 = VALLN1(KA)
101    CONTINUE
       IF ( KB .EQ. 0 ) GO TO 102
       VBN = VALLN(KB)
       VBN1 = VALLN1(KB)
102    CONTINUE
       IF ( KC .EQ. 0 ) GO TO 103
       VCN = VALLN(KC)
       VCN1 = VALLN1(KC)
103    CONTINUE
C-->  PA��O�EH�E �O OC�M X,Y( CB��AH� CO CTATOPOM ):
       VXN = K2D3*(  VAN - ( VBN + VCN ) / 2.0       )
       VXN1 = K2D3*(   VAN1 - ( VBN1 + VCN1 ) / 2.0  )
C
       VYN = K2D3*(  SQ3D2 * ( VBN - VCN )    )
       VYN1 = K2D3*( SQ3D2 * ( VBN1 - VCN1 )  )
C-->  MO���� B      TN1
       VXYN1 = SQRT(VXN1**2+VYN1**2)
C----------------------------------------------------------------------
C-->  OME�A_O�OPHA�
       OMEG0 = 314.15927
       COPN1 = 1.0
       SOPN1 = 0.0
       IF ( VXYN1 .EQ. 0.0 ) GO TO 106
       XXX = H0 * VXYN1
       IF ( XXX .EQ. 0.0 ) GO TO 104
       D2X = ( VXN1 - VXN ) ** 2
       D2Y = ( VYN1 - VYN ) ** 2
       YYY = SQRT ( D2X + D2Y )
       OMEG0 = YYY / XXX
104    CONTINUE
C-->  COS  &  SIN ���A....
       COPN1 = VXN1 / VXYN1
       SOPN1 = VYN1 / VXYN1
C----------------------------------------------------------------------
106    CONTINUE
       TOKN1( ITOK+3) = OMEG0
C-->  CKO���EH�E, OMEGA - �� MO�E�� ��. MA��H�
       ITOKM = MSW(16)
C-->  CTAP�� & HOB��
       OMEGN = TOKN( ITOKM+7 )
       OMEGN1 = TOKN1( ITOKM+7 )
C--    CKO���EH�E B���C��M, A HE B��EPEM �� TOKN
       SN = ( OMEGN - OMEG0 ) / OMEG0
       SN1 = ( OMEGN1 - OMEG0 ) / OMEG0
C-->  E�O ��KC�P�EM
       TOKN1( ITOK+4) = SN1
C
C-->  �POEK��� HHA C�HXPOHH�E ( OH� OP�EHT�P. �O BEKTOP� U_O�.
C
C-->  A�P. ���OB MA��H�
       IXV1 = MSW(15) - 1
       IXV2 = IXV1 + KXALL
       DO 201 I = 1, KXALL
       K = MSWALL( IXV1 + I )
       M = MSWALL( IXV2 + I )
C$$$$$$VKN = 0.0
C$$$$$$VMN = 0.0
       VKN1 = 0.0
       VMN1 = 0.0
       IF ( K .EQ. 0 ) GO TO 202
C$$$$$$VKN = VALLN(K)
       VKN1 = VALLN1(K)
202    CONTINUE
       IF ( M .EQ. 0 ) GO TO 203
C$$$$$$VMN = VALLN(M)
       VMN1 = VALLN1(M)
203    CONTINUE
C$$$$$$ULN(I) = VKN - VMN
       ULN1(I) = VKN1 - VMN1
201    CONTINUE
C-->  TOK� CTATOPA � POTOPA
C$$$$$$TS1N  = TOKN ( ITOKM )
C$$$$$$TS2N  = TOKN ( ITOKM  + 1 )
C$$$$$$TS3N  = TOKN ( ITOKM  + 2 )
C$$$$$$TR1N  = TOKN ( ITOKM  + 3 )
C$$$$$$TR2N  = TOKN ( ITOKM  + 4 )
C$$$$$$TR3N  = TOKN ( ITOKM  + 5 )
       GAMN  = TOKN ( ITOKM  + 6 )
C->
       TS1N1 = TOKN1( ITOKM )
       TS2N1 = TOKN1( ITOKM  + 1 )
       TS3N1 = TOKN1( ITOKM  + 2 )
       TR1N1 = TOKN1( ITOKM  + 3 )
       TR2N1 = TOKN1( ITOKM  + 4 )
       TR3N1 = TOKN1( ITOKM  + 5 )
       GAMN1 = TOKN1( ITOKM  + 6 )
C->
C-->  �X �POEK���...
C$$$$$$USXXN  = ULN (1) - ( ULN (2) + ULN (3) ) /2.0
C$$$$$$USYYN  = SQ3D2 * ( ULN (2) - ULN (3) )
C-->
C$$$$$$USQ0N  = COPN  * USXXN  + SOPN  * USYYN
C$$$$$$USD0N  = COPN  * USYYN  - SOPN  * USXXN
C
C
       USXXN1 = K2D3*(  ULN1(1) - ( ULN1(2) + ULN1(3) ) /2.0   )
       USYYN1 = K2D3*(  SQ3D2 * ( ULN1(2) - ULN1(3) )          )
C-->
       USQ0N1 = COPN1 * USXXN1 + SOPN1 * USYYN1
       USD0N1 = COPN1 * USYYN1 - SOPN1 * USXXN1
C-->  TOK�..
C$$$$$$TSXXN  = TS1N  - ( TS2N  + TS3N  ) / 2.0
C$$$$$$TSYYN  = SQ3D2 * ( TS2N  - TS3N  )
C*
C$$$$$$ISQ0N  = COPN  *TSXXN  + SOPN  * TSYYN
C$$$$$$ISD0N  = COPN  *TSYYN  - SOPN  * TSXXN
C--> B TN1...
       TSXXN1 = K2D3*(    TS1N1 - ( TS2N1 + TS3N1 ) / 2.0         )
       TSYYN1 = K2D3*(    SQ3D2 * ( TS2N1 - TS3N1 )               )
C*
       ISQ0N1 = COPN1 *TSXXN1 + SOPN1 * TSYYN1
       ISD0N1 = COPN1 *TSYYN1 - SOPN1 * TSXXN1
C
C-->  POTOPH�E BE����H�--->
C$$$$$$CR1N  = COS(GAMN  )
C$$$$$$CR2N  = COS(GAMN  + R120 )
C$$$$$$CR3N  = COS(GAMN  + R240 )
C
C$$$$$$SR1N  = SIN(GAMN  )
C$$$$$$SR2N  = SIN(GAMN  + R120 )
C$$$$$$SR3N  = SIN(GAMN  + R240 )
C-->  B TN1
       CR1N1 = COS(GAMN1 )
       CR2N1 = COS(GAMN1 + R120 )
       CR3N1 = COS(GAMN1 + R240 )
C
       SR1N1 = SIN(GAMN1 )
       SR2N1 = SIN(GAMN1 + R120 )
       SR3N1 = SIN(GAMN1 + R240 )
C***
C->  HA�P��EH��...
C$$$$$$SRXXN  = ULN (4)*CR1N  + ULN (5)*CR2N  + ULN (6)*CR3N
C$$$$$$SRYYN  = ULN (4)*SR1N  + ULN (5)*SR2N  + ULN (6)*SR3N
C
C$$$$$$URQ0N  = COPN  * SRXXN  +  SOPN  * SRYYN
C$$$$$$URD0N  = COPN  * SRYYN  -  SOPN  * SRXXN
C--->  B TO�KE N1..
       SRXXN1 = K2D3*( ULN1(4)*CR1N1 + ULN1(5)*CR2N1 + ULN1(6)*CR3N1 )
       SRYYN1 = K2D3*( ULN1(4)*SR1N1 + ULN1(5)*SR2N1 + ULN1(6)*SR3N1 )
C
       URQ0N1 = COPN1 * SRXXN1 +  SOPN1 * SRYYN1
       URD0N1 = COPN1 * SRYYN1 -  SOPN1 * SRXXN1
C-->
C-->  TOK� POTOPA...
C$$$$$$SRXXN  = TR1N  * CR1N  + TR2N  * CR2N  + TR3N  * CR3N
C$$$$$$SRYYN  = TR1N  * SR1N  + TR2N  * SR2N  + TR3N  * SR3N
C-->
C$$$$$$IRQ0N  = COPN  * SRXXN  + SOPN  * SRYYN
C$$$$$$IRD0N  = COPN  * SRYYN  - SOPN  * SRXXN
C--->  B TN1
       SRXXN1 = K2D3*( TR1N1 * CR1N1 + TR2N1 * CR2N1 + TR3N1 * CR3N1 )
       SRYYN1 = K2D3*( TR1N1 * SR1N1 + TR2N1 * SR2N1 + TR3N1 * SR3N1 )
C-->
       IRQ0N1 = COPN1 * SRXXN1 + SOPN1 * SRYYN1
       IRD0N1 = COPN1 * SRYYN1 - SOPN1 * SRXXN1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  �A��C� B TOKN1(**)
       TOKN1(ITOK + 7 ) = USQ0N1
       TOKN1(ITOK + 8 ) = USD0N1
       TOKN1(ITOK + 9 ) = ISQ0N1
       TOKN1(ITOK + 10) = ISD0N1
       TOKN1(ITOK + 11) = URQ0N1
       TOKN1(ITOK + 12) = URD0N1
       TOKN1(ITOK + 13) = IRQ0N1
       TOKN1(ITOK + 14) = IRD0N1
C----------------------------------------------------------------------
C-->  �HA�EH�� TOK...B TN  ��  TOKN
C
       USQ0N  =  TOKN( ITOK  +  7 )
       USD0N  =  TOKN( ITOK  +  8 )
       ISQ0N  =  TOKN( ITOK  +  9 )
       ISD0N  =  TOKN( ITOK  + 10 )
       URQ0N  =  TOKN( ITOK  + 11 )
       URD0N  =  TOKN( ITOK  + 12 )
       IRQ0N  =  TOKN( ITOK  + 13 )
       IRD0N  =  TOKN( ITOK  + 14 )
C----------------------------------------------------------------------
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C----------------------------------------------------------------------
C
C BCE B �POEK��� HA O�OPH�� C�CTEM� O�PE�E�EHO, MO�HO
C  �OPM�POBAT� ��P. HA�P��EH�� --->  �EPE� PSUB#
C
C  �OP-EM  PSUB _ MACC�B�:
       STMAS(1) = TN
       STMAS(2) = H0
C*********************
       STMAS( 3) = ISQ0N
       STMAS( 4) = ISD0N
       STMAS( 5) = USQ0N
       STMAS( 6) = USD0N
       STMAS( 7) = IRQ0N
       STMAS( 8) = IRD0N
       STMAS( 9) = OMEG0
       STMAS(10) = OMEGN
       STMAS(11) = GAMN
       STMAS(12) = SN
C*********************
       STMAS(13) = ISQ0N1
       STMAS(14) = ISD0N1
       STMAS(15) = USQ0N1
       STMAS(16) = USD0N1
       STMAS(17) = IRQ0N1
       STMAS(18) = IRD0N1
       STMAS(19) = OMEG0
       STMAS(20) = OMEGN1
       STMAS(21) = GAMN1
       STMAS(22) = SN1
C**
C-->  �O����M ��P. HA�P��EH��...
       NSUB = MSW(21)
       NEL = MSW(1)
C-->  URMAS <-> MSW(27),  INTMAS <-> MSW(22)
C                        HO 1 � 2-E �HA�EH�E B TOKN/TOKN1 XPAH�M;
       RMSW(22) = TOKN( ITOK + 17 )
       RMSW(23) = TOKN( ITOK + 18 )
C
       CALL CALLPS( NSUB, NEL, MSW(27), STMAS, DGMAS, MSW(22) )
C-->  �X ��KC�P�EM
       TOKN1( ITOK + 5 ) = DGMAS(1)
       TOKN1(  ITOK + 6 ) = DGMAS(2)
C==> ��KC�P�EM �HTE�PATOP�--> � �A�AHH�E �HA�EH�� TOK � CKO���EH��
       TOKN1( ITOK + 15 ) = DGMAS(4)
       TOKN1( ITOK + 16 ) = DGMAS(5)
       TOKN1( ITOK + 17 ) = RMSW(22)
       TOKN1( ITOK + 18 ) = RMSW(23)
C
       UYQ0 = DGMAS(1)
       UYD0 = DGMAS(2)
C******UYMOD = DGMAS(3)
C**** �POEK��� OT 25.09.86....
C      � A � A     "A" ---->
       UYR1 =        ( UYQ0 * ( CR1N1*COPN1 + SR1N1*SOPN1 )  +
     +                 UYD0 * ( SR1N1*COPN1 - SOPN1*CR1N1 )       )
C      � A � A     "B" ---->
       UYR2 =        ( UYQ0 * ( CR2N1*COPN1 + SR2N1*SOPN1 )  +
     +                 UYD0 * ( SR2N1*COPN1 - SOPN1*CR2N1 )       )
C      � A � A     "C" ---->
       UYR3 =        ( UYQ0 * ( CR3N1*COPN1 + SR3N1*SOPN1 )  +
     +                 UYD0 * ( SR3N1*COPN1 - SOPN1*CR3N1 )       )
C######################################################################
C
C   O�PAH��EH�E �O AM���T��E.....
C
C######################################################################
       UPOT = RMSW(20)
C
       IF ( UYR1 .GT. 0.0  .AND.  UYR1 .GT. UPOT ) UYR1 = UPOT
       IF ( UYR2 .GT. 0.0  .AND.  UYR2 .GT. UPOT ) UYR2 = UPOT
       IF ( UYR3 .GT. 0.0  .AND.  UYR3 .GT. UPOT ) UYR3 = UPOT
C
       IF ( UYR1 .LT. 0.0  .AND . UYR1 .LT. -UPOT ) UYR1 = -UPOT
       IF ( UYR2 .LT. 0.0  .AND . UYR2 .LT. -UPOT ) UYR2 = -UPOT
       IF ( UYR3 .LT. 0.0  .AND . UYR3 .LT. -UPOT ) UYR3 = -UPOT
C -->  �A��C� B TOKN1
       TOKN1( ITOK )  =    UYR1
       TOKN1( ITOK +1 ) =  UYR2
       TOKN1( ITOK +2 ) =  UYR3
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(12) = 1
C----->KOD = 0
       RETURN
       END
