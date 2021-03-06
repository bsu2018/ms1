         SUBROUTINE DM0015 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0015 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    MOH�TOP MO�E�� "BO�����TE��" [ MO�. #50, �.T�� # 6 ]
C   -----------    ****************************************************
C                   *** H��E -3�_0 --> �PEO�PA�OBATE�� �ACTOT�*********
C
C                      �O ��PAB�����M HA�P��EH��M, �O���AEM�M ��
C              MO�E�E� ��EMEHTOB, �X B�PA�AT�BA���X --> #40(PE����TOP
C              BO�����EH��), #13("��MEP�TE��" HA�P��EH��), #63( 3-X
C              �A�HA� �.�.C. ) � T.�. ( B TOKN/TOKN1 -> �EPB�E TP�
C              �HA�EH�� -> �TO �HA�EH�E HA�P��EH�� PE����POBAH�� ),
C             �OPM�P�ET C��HA�� "C��" HA 18-T� BEHT��E� �PEO�PA�OBATE��
C             �ACTOT�; OP�AH���ET B�PA�OTK� C��HA�OB "�A�PET/PA�PE�EH�E"
C             ��� "AHO�H�X" � "KATO�H�X" �P��� BEHT��E� B KA��O� ��
C             �A�;
C
C
C              ===  AM���T��� ��PAB. C��HA�OB O�PAH���BA�TC� �O
C                   EYYV  � EYYI -->  CM. SEQMAX
C              ===
C
C
C######################################################################
C    PAC�PE�E�EH�E HA�P��EH�� ��PAB�EH�� �O �A�AM � AHO�HO-KATO�H�M
C                      � P � � � A M:
C
C                      HA�P��EH�E ��PAB�EH��(�� MSW_PE����TOPA):
C                      -----------------------------------------
C
C     KATO�HA� �P���A:                         AHO�HA� �P���A:
C
C   BEHT. 01, 02, 03 -->"-"EYYA         BEHT. 04, 05, 06 --> "+"EYYA
C   BEHT. 07, 08, 09 -->"-"EYYB         BEHT. 10, 11, 12 --> "+"EYYB
C   BEHT. 13, 14, 15 -->"-"EYYC         BEHT. 16, 17, 18 --> "+"EYYC
C
C  ��E EYYA, EYYB, EYYC - C��HA�� �� TOKN/N1 ��EMEHTA "#��" ��PAB�EH��,
C                         �EPB�E TP� �HA�EH��; ( �A�A A, B, C );
C
C
C        PAC�PE�E�EH�E O�OPH�X HA�P��EH��:
C        ---------------------------------
C BEHT��� �A�� "A":       BEHT��� �A�� "B":       BEHT��� �A�� "C":
C  !!! KATO�H�E !!!        !!! KATO�H�E !!!        !!! KATO�H�E !!!
C -----------------       ----------------        -----------------
C 03     --> "+"V(KXB)    02     --> "+"V(KXC)    01     --> "+"V(KXA)
C 09     --> "+"V(KXB)    08     --> "+"V(KXC)    07     --> "+"V(KXA)
C 15     --> "+"V(KXB)    14     --> "+"V(KXC)    13     --> "+"V(KXA)
C   "+" �A�A "B"              "+" �A�A "C"               "+" �A�A "A"
C ( ��E� KXB )               ( ��E� KXC )            ( ��E� KXA )
C T.E.-120 �P�. OT "A"    T.E.-120 �P�. OT "B"    T.E.-120 �P�. OT "C"
C
C BEHT��� �A�� "A":       BEHT��� �A�� "B":       BEHT��� �A�� "C":
C !!! AHO�H�E !!!          !!! AHO�H�E !!!        !!! AHO�H�E !!!
C -----------------       ----------------        -----------------
C     06 --> "-"V(KXB)        05 --> "-"V(KXC)        04 --> "-"V(KXA)
C     12 --> "-"V(KXB)        11 --> "-"V(KXC)        10 --> "-"V(KXA)
C     18 --> "-"V(KXB)        17 --> "-"V(KXC)        16 --> "-"V(KXA)
C   "-" �A�A "B"              "-" �A�A "C"               "-" �A�A "A"
C ( ��E� KXB )               ( ��E� KXC )            ( ��E� KXA )
C T.E. +60 �P�. OT "A"    T.E. +60 �P�. OT "B"    T.E. +60 �P�. OT "C"
C
C######################################################################
C----------------------------------------------------------------------
C   "CXEMA" �PEO�PA�OBATE��:
C BEHT���:1  2  3   4  5  6  7  8  9  10  11 12  13  14  15  16  17  18
C �A�A:   C  B  A   C  B  A  C  B  A   C   B  A   C   B   A   C   B   A
C �P���A:  KATO�     AHO�     KATO�     AHO�       KATO�        AHO�
C B�XO�:    �A�A   A            �A�A  B               �A�A   C
C B TOKN: 7  8  9  10  11 12 13 14 15  16 17 18   19 20 21   22 23 24
C "C��"  25 26 27   28 29 30 31 32 33  34 35 36   37 38 39   40 41 42
C----------------------------------------------------------------------
C
C              ��EMEHT O��C�BAETC�:
C
C   � 3 T 50 � KA KB KC KRA KRB KRC
C   � T_BK�  DTCIU #��_TA_PE����TOPA_BO�����EH��<�EHEPATOP
C                                                ��PAB����E�O
C                                                HA�P��EH��>;
C
C            KXA  KXB  KXC   EYYV   EYYI;
C     ��E:
C     ***
C              KA, KB, KC  - ���� ��TA��E�O 3-X �A�HO�O HA�P��EH��;
C              KRA, KRB, KRC - ���� POTOPHO� O�MOTK� ��. MA��H� ->
C                              B�XO�HOE HA�P��EH�E �PEO�PA�OBATE��
C                              �ACTOT�;
C
C              �O BPEMEH� T_BK� -> ���� KRA, KRB, KRC -> �AKOPO�EH�
C                 **************    ( MO�E��P��TC� "��EA��H�M" K���EM );
C
C
C              DTCIU - ���TE��HOCT� �M����CA "C��" ->
C                      �M����C� �EHEP�P��TC� CO��ACHO CPABHEH��
C                      HA�P. O�OPHO�O -> B���PAEMO�O �� MACC�BA
C                      TOKN/TOKN1 ��EMEHTA #_��._PE�._BO����.
C                      ( TP� �EPB�E C�OBA ) � HA�P��EH�� ��TAH��
C                      �PEO�PA�OBATE��, T.E. HA ���AX KA, KB, KC;
C                      ( CM. DM0015 )
C              #_��.BO��. - MO�E�� O�PE�E�EH�� HA�P��EH�� ��PAB�EH��:
C                         ( T�� #40, T�� # 63, T�� # 13 );
C                         ###############################
C
C                      KXA, KXB, KXC - ���� "O�OPHO�" C�CTEM�
C                                      HA�P��EH�� �A�A: "A", "B", "C";
C
C   EYYV - O�PAH��EH�E AM���T��� ��P. C��HA�A B B��P�M�TE��HOM PE��ME.
C   EYYI - O�PAH��EH�E AM���T��� ��P. C��HA�A B �HBEPTOPHOM    PE��ME.
C
C
C === B�BO�H�E �HA�EH��: I1, I2, I3, I4, I5, I6, - TOK� BHE�. B�BO�OB,
C                        IV1, IV2, ..., IV18,    - TOK� BEHT��E�,
C                        CIU1, CIU2, ..., CIU18 - "C��" HA BEHT��E;
C
C
C                    O�C����BAH�E BEHT��E� �EPE� �/� DM00B9, K���E� -
C                      "B P��H��"( HE�OCPE�CTBEHHO B �PO�PAMME );
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                 -  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE�
C                   �HA�EH�� "�POBO��MOCT�" �AKP�T�X BEHT��E� ( GVENT6 )
C                   [ B �AHHO� BEPC�� KOM��EKCA ];
C                  - �OPM�POBAH�E BEKTOPA K.�. ���OB "LOGYTR"
C                    ��� �H���EHTH�X ���OB --> �C�O����ETC�
C                     �/�  GETLOG --> OC��ECTB�T� "��EMEHTAPH��" BK�A�;
C
C                  1A) ZAPROS = 10
C                      ------------
C                      �OPM�POBAH�E "TEHEBO�" C�CTEM� �PABHEH��
C                       ( �O��EMEHTH�� BK�A� )
C
C                  B) ZAPROS = 20
C                     ------------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�. �P� �TOM TOK OTKP�T�X BEHT��E�:(���� K � M )
C                  <TOKN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK �AKP�T�X BEHT��E� = 0.0;
C                  �A��C� �T�X TOKOB B MACC�B TOKN1;
C                   !!! B��O�H�T� �O MO����KA��� PE�EH�� VALLN1 !!!
C
C                  C) ZAPROS = 3
C                     -----------
C                      �O O�PE�E�EHH�M TOKAM BEHT���H�X "��E�"
C                      �OPM�P�ET C��HA�� "PA�PE�EH�E/�A�PET" CX � TX
C                      �CXO�� �� HA�P��EH�� ��PAB�EH�� � O�OPH�X
C                      HA�P��EH�� ( ���� KA, KB, KC ) -->
C                      O�PE�E��ET MOMEHT� "ECTECTBEHHO�" KOMM�TA���
C                      BEHT��E� ( CM. �/� MCIU50 ); HA OCHOBE �TO�O
C                     �O��BO��TC� AHA��� HA BO�MO�H�� CMEH� COCTO�H��
C                     BEHT��� [ OTKP�T <==> �AKP�T ]:
C                     �P� �TOM: FKOM = 0 - COCTO�H�E HE ��MEH��OC�;
C                               FKOM = 1 - HA TEK��EM �A�E COCTO�H�E
C                                         �O��HO "���O" ��MEH�T�C� B
C                                         B T = TX MOMEHT BPEMEH�;
C                                         PEA��HO B �PO�PAMME O�PE�E-
C                                         ��ETC� HX - �A� BO�MO�HO�
C                                         KOMM�TA���, �ATEM TX=TN+HX;
C                                         �P� HX < HXMIN [MASPRT(62)]
C                                         �P�H�MAETC� HX = HXMIN;
C                                         COCTO�H�E HX=0.0 B���BAET
C                                         �E�AT� COO��EH��;
C                                         �PO��BE�EH� COTBETCTB����E
C                                         ��MEHEH�� B MASPRT:
C                                          FKOM <== "1"
C                                         HXG = MIN( HX, HXG )
C                                         TXG = TN + HXG
C   !!! OT 01.08.86  !!!
C              ��ET: ICLOSE - MO���� TOKA "�AKP�T��" BEHT���;
C                    UOPEN  - MO���� HA�P��EH�� "OTKP�T��" BEHT���;
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX_BEHT���   -
C                      ��KC�P�ETC� CMEHA COCTO�H�� BEHT��� �APAMETPOM
C                      KD;
C                     PABEHCTBO �POBEP�ETC� C TO�. �O "EPSKOM".
C
C   METO�:         PA�HOB��HOCT� BO�HOBO�O A��OP�TMA ��� �OPM�POBAH��
C   -----          MACC�BA LOGYTR --> �E�E� �/� GETLOG;
C
C                  "TEHEBA�" C�CTEMA �OPM�P�ETC� "MAC�TA��POBAHHA�"
C                   TAK, �TO: V[X] - V[Y] = <TOK    X-Y   BETB�>;
C                   ����, OTME�EHH�E B LOGYTR(*) = 0 - "�A�OB�E"
C                   ��� KA��O� �H���EHTHO� �P���� OTKP�T�X BEHT��E�;
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
C   ------------   2.          SEQCIU, DM00B9, SEQMAX;
C                              -------
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=1, 10,20,3,4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST = 1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C                4. C 3.10.86 --> ��KCA��� CXA+CXK ��� SUQCIU.
C
C              �OPMAT MSW # T��A 50 [ BO�����TE��, H��E ]:
C              -------------------------------------------
C               "�OCTO�HHA�" �ACT�
C               ******************
C
C  +-------+--------+--------+--------+------+------+--------+----+
C  I HOMEP I HOMEP  I HOMEP  I  �P��. I KO�� I KO�. I A�P.   I PE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I �HA�.  I �E I
C  I HTA   I MO�E�� I PAC�ETAI (PE�.) I BO   I �HA� I "TOKN" I PB I
C  I       I        I DM.....I       .I ���OBI EH��.I "TOKN1"I    I
C  +-------+--------+--------+--------+------+------+--------+----+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7    I 8  I
C  +-------+--------+--------+--------+------+------+--------+----+
C
C
C              �EPEMEHHA� �ACT� MSW:
C              *********************
C
C  C�OBO     C�OBO     O�O�HA�EH� � �O���ECKOE �HA�EH�E
C  OT        OT
C  HA�A�A    �EPEMEH.
C  MSW       �ACT�
C
C  9,10,11    -        KA, KB, KC, - HOMEPA ���OB �A� "��TAH��"
C  12,13,14   -        KRA< KRB, KRC - HOMEPA ���OB B�XO�HO�O
C                                      HA�P��EH�� H��E-3�_0
C-- -- - - - - --- - - - - - - - -
C   15         1       [F] NPB - HOMEP ��EMEHTA, �OCTAB����E�O
C                                HA�P��EH�E ��PAB�EH��;
C  16          2       [L] ANPB - A�PEC B MSW_ALL NPB ��EMEHTA;
C  17          3       [L] AUYP - A�PEC B TOKN/N1 NPB ��EMEHTA;
C  18          4       [F] TBKL - BPEM� BK�. H��E;
C  19          5       [F] DTCIU - ���T. �M����CA "C��"
C 20, 21     6, 7      [F] EYYV,  EYYI - O�PAH��EH�� ���OB.
C  22         8             "HX"  K���E� K.�. ���OB;
C 23<->25    9<->11  [F]     HOMEPA "O�OPH�X" ���OB;
C 26<->29    12<->15          PE�EPB;
C
C 30<->39    16<->25      MSWV1  -  C�OBO ��PAB�EH�� 1-M BEHT��EM
C 40<->49    26<->35      MSWV2  -  C�OBO ��PAB�EH�� 2-M BEHT��EM
C .....................................................................
C 200<->209   186<->195   MSWV18 -  C�OBO ��PAB�EH�� 18-M BEHT��EM
C**********************************************************************
C
C   LM O��A� = 209 C�OB, LM_�OK. = 195
C
C       �OPMAT MSWV_EHT��� ��� DM00B9:
C       -------------------------------
C
C      C�OBO                   �HA�EH�E
C
C      1       KD - KO� COCTO�H��: =0 - �AKP�T, =1 - OTKP�T;
C      2       HX - �A� OT TN, BO�MO�HOE BPEM� �EPEK���EH��;
C
C ( B [*] HOMEPA MSW_VEHT��� ��� �/� "MCIU50" )
C
C  [1] 3       TS - BPEM� CTAPTA �M����CA( "E" KOMM�TA��� );
C  [2] 4       TK - BPEM� ��H��A �M����CA( "E" KOMM�TA��� );
C  [3] 5       CX - KO� PA�PE�EH��/�A�PETA "C��":
C                   =0 - C�� �A�PE�EHO �O�ABAT� HA BEHT���,
C                   =1 - C�� PA�PE�EHO �O�ABAT� HA BEHT���;
C [4] 6        TX - BPEM�, C KOTOPO�O �E�CTB�ET KO� "CX";
C
C
C [5] 7        MCIU - KO� �M����CA --->
C                     MCIU   = 00  --> "C��" HET B [TN, TN+H0];
C                                       HXCIU = 0.0;
C                     MCIU   = 01  --> B [TN, TN+H0] -> �EPE�H�� �POHT
C                                   �M����CA "C��", HXCIU = �HTEPBA� OT
C                                      TN �O �EPE�HE�O �POHTA "C��";
C                     MCIU   = 10 -->  B [TN, TN+H0] -> �A�H�� �POHT
C                                   �M����CA "C��", HXCIU = �HTEPBA�
C                                      OT TN �O �A�HE�O �POHTA �M����CA;
C                     MCIU   = 11 -->  B [TN, TN+H0] "C��" BK���EH,
C                                      COCTO�H�E HE MEH��, HXCIU=0.0;
C
C
C [6] 8        HXCIU - �A� OT TN, �O O�EPE�HO�O �POHTA "C��";
C (---)
C     9              - ��A� KD COCTO�H��
C    10              - PE�EPB
C********************************************************************
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL
       DIMENSION MSWALL(*), RMSWAL(*), EYYN(3), EYYN1(3)
       INTEGER ADRMSW
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(59), GVENT6 )
         EQUIVALENCE ( MASPRT(94), DTMCIU )
         EQUIVALENCE ( MASPRT(60), FTEST )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
         INTEGER FTEST, OPEN6
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FKOM,   MASPRT(34) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( HXG,    MASPRT(61) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
C----------------------------------------------------------------------
       DIMENSION TOKXN(2), TOKXN1(2),  THELP(18), VHN(6), VHN1(6)
       DIMENSION KVENT(18), MVENT(18), UYN(18), UYN1(18),
     =           UON(18), UON1(18),                   TNK(3), TN1K(3),
     =           TNA(3), TN1A(3)
       INTEGER AUYP, CXA, CXK
       REAL                          IKN, IKN1, IAN, IAN1
C -->  AHO�H�E ���� ( HA�A�O BEHT��� ):
       DATA KVENT
     = /12,12,12, 11,10,09, 13,13,13, 11,10,09, 14,14,14, 11,10,09/
C--> KATO�H�E ���� ( KOH�� BEHT��E� ):
       DATA MVENT
     = /11,10,09, 12,12,12, 11,10,09, 13,13,13, 11,10,09, 14,14,14/
C
C----------------------------------------------------------------------
       KOD = 0
C--->  KOHTPO�� T��A
         IF ( MSW(2) .NE. 50 ) GO TO 9999
       TBKL = RMSW(18)
       TN1 = TN + H0
C---> B��OP PE��MA:
         IF ( ZAPROS .EQ. 1 )   GO TO 100
         IF ( ZAPROS .EQ. 10 )  GO TO 200
         IF ( ZAPROS .EQ. 20 )  GO TO 300
         IF ( ZAPROS .EQ. 3 )   GO TO 400
         IF ( ZAPROS .EQ. 4 )   GO TO 500
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0015 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
C
100    CONTINUE
       IF ( TN1 .GE. TBKL ) GO TO 101
C-->  MAKET K.�. ���OB KRA-KRB, KRB-KRC ====>
       KRA = MSW(12)
       KRB = MSW(13)
       KRC = MSW(14)
C-->  C�POC HX_K��E�
       RMSW(22) = H0
C-->  KRA<->KRB:
       CALL GETLOG_(KRA, KRB, LOGYTR )
C-->  KRB<->KRC:
       CALL GETLOG_(KRB, KRC, LOGYTR )
       RETURN
C----------------------------------------------------------------------
101    CONTINUE
C-> ZAPROS = 1 -->  PA�OTAET "H��E" -->  �OC�E�. B��OB MOH�TOPA BEH��E�
C-->  B COOTBETCTB�� C MACC�BAM� RMSW<50>, UYN, UYN1, UON, UON1
C
666    CONTINUE
       NEL = MSW(1)
       DO 102 NV = 1,18
C-->  A�PEC RMSW<50> BEHT���-->
       INV = (NV-1) * 10 + 30
C-->  ���� �O�K���EH��-->
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
C-->  HOMEP �O�. BEHT���
       NPEL = NEL*10000+NV
C-->  MOH�TOPHOE O�C����BAH�E-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
102    CONTINUE
C**********************************************************************
       RETURN
C**********************************************************************
C**********************************************************************
C**********************************************************************
200    CONTINUE
C-->  ZAPROS = 10  -->  TEHEBA� C�CTEMA-->
       IF ( TN1 .GE. TBKL ) GO TO 201
C-->  ��� K.�. ���OB--->
       KRA = MSW(12)
       KRB = MSW(13)
       KRC = MSW(14)
C---
       G = 1.0
C---
       IF ( KRA .EQ. 0 ) GO TO 290
       IF ( LOGYTR(KRA) .EQ. -1 ) KRA = 0
290    CONTINUE
       IF ( KRB .EQ. 0 ) GO TO 291
       IF ( LOGYTR(KRB) .EQ. -1 ) KRB = 0
291    CONTINUE
C-->  "BK�A�" ��� K���A KRA<->KRB--->
       CALL DEPOST( KRA, KRB, G, 0.0, YALL, JALL )
C
       IF ( KRC .EQ. 0 ) GO TO 292
       IF ( LOGYTR(KRC) .EQ. -1 ) KRC = 0
292    CONTINUE
       CALL DEPOST( KRB, KRC, G, 0.0, YALL, JALL )
C----------------------------------------------------------------------
       RETURN
201    CONTINUE
C-->  ZAPROS=10-> TO �TO � ��� ZAPROS=1
       GO TO 666
C######################################################################
C######################################################################
C######################################################################
300    CONTINUE
C-->  ZAPROS=20 -->  TOK� BEHT��E� � K���E� O�PE�E��EM...>
       IF ( TN1 .GE. TBKL ) GO TO 301
C-->  K����-->
       KRA = MSW(12)
       KRB = MSW(13)
       KRC = MSW(14)
C-->> KRA-KRB
       IF ( KRA .EQ. 0 ) GO TO 390
       IF ( LOGYTR(KRA) .EQ. -1 ) KRA = 0
390    CONTINUE
       IF ( KRB .EQ. 0 ) GO TO 391
       IF ( LOGYTR(KRB) .EQ. -1 ) KRB = 0
391    CONTINUE
       TA = 0.0
       IF ( KRA .NE. 0 ) TA = VALLN1(KRA)
       TB = 0.0
       IF ( KRB .NE. 0 ) TB = VALLN1(KRB)
       TOKABR = TA - TB
C-->  KRB-KRC
       IF ( KRC .EQ. 0 ) GO TO 392
       IF ( LOGYTR(KRC) .EQ. -1 ) KRC = 0
392    CONTINUE
       TC = 0.0
       IF ( KRC .NE. 0 ) TC = VALLN1(KRC)
       TOKBCR = TB - TC
C-->  �A��C� A TOKN1...
       ITOK = MSW(7)
C
       TOKN1( ITOK + 3 ) =  -TOKABR
       TOKN1( ITOK + 5 ) =  +TOKBCR
       TOKN1( ITOK + 4 ) =   TOKABR - TOKBCR
C-->  OCTA��H�E -> PABH� 0.0
       TOKN1( ITOK ) = 0.0
       TOKN1( ITOK + 1 ) = 0.0
       TOKN1( ITOK + 2 ) = 0.0
C----------------------------------------------------------------------
       RETURN
C**********************************************************************
301    CONTINUE
C--> ZAPROS = 20  -> TOK� BEHT��E� B TOKN/N1-->
C      ITOKNV --> (ITOK-1) + 6 + NV = ITOK+5+NV
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       ITOK = MSW(7)
       NEL = MSW(1)
       DO 302 NV = 1,18
C-->  A�PEC RMSW<50> BEHT���-->
       INV = (NV-1) * 10 + 30
C-->  ���� �O�K���EH��-->
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
C-->  HOMEP �O�. BEHT���
       NPEL = NEL*10000+NV
C-->  MOH�TOPHOE O�C����BAH�E-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
C-->  PACTAB�T� O�PE�. �HA�EH�� TOKOB-->
       ITOKNV = ITOK + 5 + NV
       TOKN1(ITOKNV) = TOKXN1(1)
       THELP(NV) = TOKXN1(1)
302    CONTINUE
C-->  "�TO�OB�E" TOK�-->
C-->  TOK� B�XO�H�X ���OB --> KA, KB, KC-->
C--> HOMEP BEHT��� NV  �O HOMEP� �P���� L-->
C    NV = (L-1)*6 + J, J-> HOMEP BEHT��� B �P���E(B �A�E)
       TKA = 0.0
       TKB = 0.0
       TKC = 0.0
C-->  �O �A�HO-->
       DO 303 L=1,3
C-->  �A�A "A", BEHT��� J = 6 � 3:
       NV6 = (L-1)*6 + 6
       NV3 = (L-1)*6 + 3
       TKA = TKA + ( THELP(NV6) - THELP(NV3) )
C-->  �A�A "B", BEHT��� J = 5 � 2:
       NV5 = (L-1)*6 + 5
       NV2 = (L-1)*6 + 2
       TKB = TKB + ( THELP(NV5) - THELP(NV2) )
C-->  �A�A "C", BEHT��� J = 4, 1
       NV4 = (L-1)*6 + 4
       NV1 = (L-1)*6 + 1
       TKC = TKC + ( THELP(NV4) - THELP(NV1) )
303    CONTINUE
C-->  �X ��KC�P�EM
       TOKN1(ITOK) = TKA
       TOKN1(ITOK+1) = TKB
       TOKN1(ITOK+2) = TKC
C-->  TOK� B�XO�A �EPO�PA�OBATE�� �ACTOT�-->
C-->  C�MM�POBAH�E �O �P���AM..
       TK1 = THELP(1)  + THELP(2)  + THELP(3)
       TK2 = THELP(7)  + THELP(8)  + THELP(9)
       TK3 = THELP(13) + THELP(14) + THELP(15)
C--> ��� AHO�H�X..
       TA1 = THELP(4)   + THELP(5)  + THELP(6)
       TA2 = THELP(10)  + THELP(11) + THELP(12)
       TA3 = THELP(16)  + THELP(17) + THELP(18)
C-->  C�MAPH�E B�X. TOK�-->
       TOKN1( ITOK + 3 ) = TA1 - TK1
       TOKN1( ITOK + 4 ) = TA2 - TK2
       TOKN1( ITOK + 5 ) = TA3 - TK3
C-->  TOK� ��E, A "C��" E�E HE O�PE�E�EH�....
       RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
400    CONTINUE
C-->  ZAPROS=3  ->  AHA��� BO�MO�HO�O COCTO�H��/�EPEK���EH��
       IF ( TN1 .GE. TBKL ) GO TO  401
C-->  K���� B�K���A�TC� TO��KO �P�:
       IF ( TN .LT. TBKL  .AND.  TBKL .LE. TN1 ) GO TO 402
C-->  �HA�E H� �E�O HE MEH�ETC�
       RETURN
402    CONTINUE
C-->  �A� �O BO�MO�HO� KOMM�TA���
       HX = TBKL - TN
C-->  E�O ��KC�P�EM
       IF ( HX .LT. HXMIN ) HX = HXMIN
       RMSW(22) =  HX
       HXG = AMIN1( HX, HXG )
       TXG = TN + HXG
       FKOM = 1
       RETURN
C----------------------------------------------------------------------
401    CONTINUE
C-->  ZAPROS=3 -  TOK� O�PE�E�EH�, HA��EM AHO�H�E � KATO�H�E TOK� B
C                 �A�AX BEHT��E� ��� B��CHEH�� CX, TX
C
       ITOK1 = MSW(7) - 1 + 6
C**********************************************************************
       TNK(1)  = TOKN (ITOK1+01) + TOKN (ITOK1+02) + TOKN (ITOK1+03)
       TN1K(1) = TOKN1(ITOK1+01) + TOKN1(ITOK1+02) + TOKN1(ITOK1+03)
       TNK(2)  = TOKN (ITOK1+07) + TOKN (ITOK1+08) + TOKN (ITOK1+09)
       TN1K(2) = TOKN1(ITOK1+07) + TOKN1(ITOK1+08) + TOKN1(ITOK1+09)
       TNK(3)  = TOKN (ITOK1+13) + TOKN (ITOK1+14) + TOKN (ITOK1+15)
       TN1K(3) = TOKN1(ITOK1+13) + TOKN1(ITOK1+14) + TOKN1(ITOK1+15)
C--
       TNA(1)  = TOKN (ITOK1+04) + TOKN (ITOK1+05) + TOKN (ITOK1+06)
       TN1A(1) = TOKN1(ITOK1+04) + TOKN1(ITOK1+05) + TOKN1(ITOK1+06)
       TNA(2)  = TOKN (ITOK1+10) + TOKN (ITOK1+11) + TOKN (ITOK1+12)
       TN1A(2) = TOKN1(ITOK1+10) + TOKN1(ITOK1+11) + TOKN1(ITOK1+12)
       TNA(3)  = TOKN (ITOK1+16) + TOKN (ITOK1+17) + TOKN (ITOK1+18)
       TN1A(3) = TOKN1(ITOK1+16) + TOKN1(ITOK1+17) + TOKN1(ITOK1+18)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C-->  �CXO�H�E HA�P��EH�� ��PAB�EH��...
C  B TOKN/N1  HAXO��TC� �O A�PEC� AUYP = MSW(17)
C
       AUYP = MSW(17)
C--->
       EYYN (1) = TOKN (AUYP     )
       EYYN1(1) = TOKN1(AUYP     )
       EYYN (2) = TOKN (AUYP + 1 )
       EYYN1(2) = TOKN1(AUYP + 1 )
       EYYN (3) = TOKN (AUYP + 2 )
       EYYN1(3) = TOKN1(AUYP + 2 )
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  O�PAH��EH�� CO��ACHO "PE��MA".......
       EYYV = RMSW(20)
       EYYI = RMSW(21)
       CALL SEQMAX( EYYN (1), TN K(1),  TN A(1),  EYYV,  EYYI )
       CALL SEQMAX( EYYN (2), TN K(2),  TN A(2),  EYYV,  EYYI )
       CALL SEQMAX( EYYN (3), TN K(3),  TN A(3),  EYYV,  EYYI )
C-->  ��� TN1
       CALL SEQMAX( EYYN1(1), TN1K(1),  TN1A(1),  EYYV,  EYYI )
       CALL SEQMAX( EYYN1(2), TN1K(2),  TN1A(2),  EYYV,  EYYI )
       CALL SEQMAX( EYYN1(3), TN1K(3),  TN1A(3),  EYYV,  EYYI )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C-->  UYN/N1  - HA�P. ��PAB�EH�� BEHT���
C-->  UON/N1  - O�OPH�E HA�P.
C     EYYN/N1 - HA�P. ��PAB�EH�� �PEO�PA�OBATE��;
C  VHN / N1 - "�CT�HH�E" HA�P��EH�� HA ���AX O�OPHO�� C�CTEM�
C              KXA, KXB, KXC             B TN/TN+H0
C
       DO 403 I=1,3
C-->  HOMEP ���A:
       K = MSW(I+22)
       VKN = 0.0
       VKN1 = 0.0
       IF ( K .EQ. 0 ) GO TO 404
       VKN  =  VALLN( K )
       VKN1 = VALLN1( K )
404    CONTINUE
       VHN(I)   = VKN
       VHN1(I)  = VKN1
403    CONTINUE
C######################################################################
C    PAC�PE�E�EH�E HA�P��EH�� ��PAB�EH�� �O �A�AM � AHO�HO-KATO�H�M
C                      � P � � � A M:
C
C                      HA�P��EH�E ��PAB�EH��(�� MSW_PE����TOPA):
C                      -----------------------------------------
C
C     KATO�HA� �P���A:                         AHO�HA� �P���A:
C
C   BEHT. 01, 02, 03 -->"-"EYYA         BEHT. 04, 05, 06 --> "+"EYYA
C   BEHT. 07, 08, 09 -->"-"EYYB         BEHT. 10, 11, 12 --> "+"EYYB
C   BEHT. 13, 14, 15 -->"-"EYYC         BEHT. 16, 17, 18 --> "+"EYYC
C
C  ��E EYYA, EYYB, EYYC - C��HA�� �� TOKN/N1 ��EMEHTA "#��" ��PAB�EH��,
C                         �EPB�E TP� �HA�EH��; ( �A�A A, B, C );
C
C
C        PAC�PE�E�EH�E O�OPH�X HA�P��EH��:
C        ---------------------------------
C BEHT��� �A�� "A":       BEHT��� �A�� "B":       BEHT��� �A�� "C":
C  !!! KATO�H�E !!!        !!! KATO�H�E !!!        !!! KATO�H�E !!!
C -----------------       ----------------        -----------------
C 03     --> "+"V(KXB)    02     --> "+"V(KXC)    01     --> "+"V(KXA)
C 09     --> "+"V(KXB)    08     --> "+"V(KXC)    07     --> "+"V(KXA)
C 15     --> "+"V(KXB)    14     --> "+"V(KXC)    13     --> "+"V(KXA)
C   "+" �A�A "B"              "+" �A�A "C"               "+" �A�A "A"
C ( ��E� KXB )               ( ��E� KXC )            ( ��E� KXA )
C T.E.-120 �P�. OT "A"    T.E.-120 �P�. OT "B"    T.E.-120 �P�. OT "C"
C
C BEHT��� �A�� "A":       BEHT��� �A�� "B":       BEHT��� �A�� "C":
C !!! AHO�H�E !!!          !!! AHO�H�E !!!        !!! AHO�H�E !!!
C -----------------       ----------------        -----------------
C     06 --> "-"V(KXB)        05 --> "-"V(KXC)        04 --> "-"V(KXA)
C     12 --> "-"V(KXB)        11 --> "-"V(KXC)        10 --> "-"V(KXA)
C     18 --> "-"V(KXB)        17 --> "-"V(KXC)        16 --> "-"V(KXA)
C   "-" �A�A "B"              "-" �A�A "C"               "-" �A�A "A"
C ( ��E� KXB )               ( ��E� KXC )            ( ��E� KXA )
C T.E. +60 �P�. OT "A"    T.E. +60 �P�. OT "B"    T.E. +60 �P�. OT "C"
C
C**********************************************************************
C-->  HA�P��EH�� ��PAB�EH��*******
C******* TN BPEM� *****************************************************
C   K A T O � H A �   � P � � � A:
       UYN(01) = -EYYN(1)
       UYN(02) = -EYYN(1)
       UYN(03) = -EYYN(1)
       UYN(07) = -EYYN(2)
       UYN(08) = -EYYN(2)
       UYN(09) = -EYYN(2)
       UYN(13) = -EYYN(3)
       UYN(14) = -EYYN(3)
       UYN(15) = -EYYN(3)
C   A H O � H A �   � P � � � A:
       UYN(04) = +EYYN(1)
       UYN(05) = +EYYN(1)
       UYN(06) = +EYYN(1)
       UYN(10) = +EYYN(2)
       UYN(11) = +EYYN(2)
       UYN(12) = +EYYN(2)
       UYN(16) = +EYYN(3)
       UYN(17) = +EYYN(3)
       UYN(18) = +EYYN(3)
C----------------------------------------------------------------------
C******* TN + H0 BPEM� *************************************************
C   K A T O � H A �   � P � � � A:
       UYN1(01) = -EYYN1(1)
       UYN1(02) = -EYYN1(1)
       UYN1(03) = -EYYN1(1)
       UYN1(07) = -EYYN1(2)
       UYN1(08) = -EYYN1(2)
       UYN1(09) = -EYYN1(2)
       UYN1(13) = -EYYN1(3)
       UYN1(14) = -EYYN1(3)
       UYN1(15) = -EYYN1(3)
C   A H O � H A �   � P � � � A:
       UYN1(04) = +EYYN1(1)
       UYN1(05) = +EYYN1(1)
       UYN1(06) = +EYYN1(1)
       UYN1(10) = +EYYN1(2)
       UYN1(11) = +EYYN1(2)
       UYN1(12) = +EYYN1(2)
       UYN1(16) = +EYYN1(3)
       UYN1(17) = +EYYN1(3)
       UYN1(18) = +EYYN1(3)
C----------------------------------------------------------------------
C***** O � O P H � E   H A � P � � E H � �:
C********TN BPEM�: ****************************************************
C-->  �A�A   "A"-> [ 3,9,15 - KATO� "+ B" ], [6,12,18 - AHO� "- B" ]
       UON(03) = +VHN(2)
       UON(06) = -VHN(2)
       UON(09) = +VHN(2)
       UON(12) = -VHN(2)
       UON(15) = +VHN(2)
       UON(18) = -VHN(2)
C--> �A�A "B"-> [ 2,8,14 KATO� "+ C" ], [ 5,11,17 - AHO� "-C" ]
       UON(02) = +VHN(3)
       UON(05) = -VHN(3)
       UON(08) = +VHN(3)
       UON(11) = -VHN(3)
       UON(14) = +VHN(3)
       UON(17) = -VHN(3)
C-->  �A�A "C"-> [ 1,7,13 - KATO� "+ A" ], [ 4,10,16 - AHO� "-A" ]
       UON(01) = +VHN(1)
       UON(04) = -VHN(1)
       UON(07) = +VHN(1)
       UON(10) = -VHN(1)
       UON(13) = +VHN(1)
       UON(16) = -VHN(1)
C----------------------------------------------------------------------
C********TN+H0  BPEM�: *************************************************
C-->  �A�A   "A":
       UON1(03) = +VHN1(2)
       UON1(06) = -VHN1(2)
       UON1(09) = +VHN1(2)
       UON1(12) = -VHN1(2)
       UON1(15) = +VHN1(2)
       UON1(18) = -VHN1(2)
C--> �A�A "B")
       UON1(02) = +VHN1(3)
       UON1(05) = -VHN1(3)
       UON1(08) = +VHN1(3)
       UON1(11) = -VHN1(3)
       UON1(14) = +VHN1(3)
       UON1(17) = -VHN1(3)
C-->  �A�A "C"
       UON1(01) = +VHN1(1)
       UON1(04) = -VHN1(1)
       UON1(07) = +VHN1(1)
       UON1(10) = -VHN1(1)
       UON1(13) = +VHN1(1)
       UON1(16) = -VHN1(1)
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --->  ��� KA��O� �P���� CBOE CX < AHO�HOE>, CX <KATO�HOE> � TX:
C-->
       DO 405 LG=1,3
C-->  LG -> HOMEP �P����, J-> HOMEP B �P���E
C  NV = (L-3)*6 + J  -->"A�CO��TH��" A�PEC...
       IKN  = TNK (LG)
       IKN1 = TN1K(LG)
       IAN  = TNA (LG)
       IAN1 = TN1A(LG)
C----------------------------------------------------------------------
C  ��� "LG" KATO�-AHO� �P����:
C----------------------------------------------------------------------
C   CTAP�E CXA, CXK...
C  1 KATO�,  4 - AHO�..
       JK = 1
       JA = 4
       NVK = ( LG-1)*6 + JK
       NVA = ( LG-1)*6 + JA
       INVK = (NVK-1)*10+30
       INVA = (NVA-1)*10+30
       CXK = MSW(INVK + 4)
       CXA = MSW(INVA + 4)
C----------------------------------------------------------------------
       CALL SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
     =  IFXK,  IFXA,         EYYN(LG), EYYN1(LG)  )
C----------------------------------------------------------------------
440    CONTINUE
C-->  CMEHA CX � TX B MSW<50> (1,2,3 -> KATO�, 4,5,6 -> AHO� ):
       DO 441 J = 1,3
       NV = (LG-1)*6 + J
C-->  A�P. HA�A�A MSW<50> KATO�H�X BEHT��E� B �P���E
       INV = ( NV-1) * 10 + 30
       MSW(INV+4) = CXK
       IF ( IFXK  .EQ. 1 )  RMSW(INV+5) = TXK
441    CONTINUE
C
       DO 442 J=4,6
       NV = (LG-1)*6 + J
C-->  A�P. HA�A�A MSW<50>      AHO�H�X BEHT��E� B �P���E
       INV = ( NV-1) * 10 + 30
       MSW(INV+4) =  CXA
       IF ( IFXA  .EQ. 1 )  RMSW(INV+5) = TXA
442    CONTINUE
C----------------------------------------------------------------------
450    CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
405    CONTINUE
C######################################################################
C######################################################################
C-->  B��O�H�M ZAPROS=3  ��� BEHT�E�***********************************
       NEL = MSW(1)
       ITOK = MSW(7)
       DTCIU = RMSW(19)
       DO 451 NV = 1,18
       INV = (NV-1) * 10 + 30
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
       NPEL = NEL*10000 + NV
C-->  PACTAB�M TOKXN/N1
       ITOKNV = ITOK+5 + NV
       TOKXN(1) =  TOKN ( ITOKNV )
       TOKXN1(1) = TOKN1( ITOKNV )
C-->  BCE O�PE�E�EHO...
C-->  MOH�TOPHOE O�C����BAH�E-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
C-->  �P�MEM C��HA�� "C��"-->
       ICIUNV = ITOK + 5 + 18 + NV
       TOKN1(ICIUNV) = TOKXN1(2)
451    CONTINUE
C-->  �A�POC� ...�
       RETURN
C----------------------------------------------------------------------
C######################################################################
500    CONTINUE
C-->  ZAPROS=4, ��� K���E� �A COCTO�H�EM C�E��T� HE H��HO
       IF ( TN1 .LT. TBKL ) RETURN
C-->  ��� BEHT��E�  -->  AHA�O���HO ZAPROS=1-->
       GO TO 666
C$$$$$$DEBUG           INIT
       END
