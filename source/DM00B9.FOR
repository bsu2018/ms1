         SUBROUTINE DM00B9(KQ,MQ,NPEL,MSWX, RMSWX, YALL, JALL, LOGYTR,
     *                       TOKXN, TOKXN1, VALLN, VALLN1, KOD,
     *         UOPN, UOPN1, UYPN, UYPN1, DTCIU )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM00B9 (KQ,MQ,NPEL, MSWX, RMSWX, YALL, JALL, LOGYTR,
C   -------------            TOKXN, TOKXN1, VALLN, VALLN1, KOD,
C                             UOPN, UOPN1, UYPN, UYPN1, DTCIU )
C
C
C   HA�HA�EH�E:    MOH�TOP MO�E�� ��P. BEHT���  [ MO�. # B9, �.T�� # 6 ]
C   -----------    ****************************************************
C                  [ C�E�. �P�MEHEH�E B MAKPOMO�E��X �PEO�PA�OBATE�E� ]
C                  ----------------------------------------------------
C @ - OT 21.11.86.
C
C                      MACC�B� MSWX ( RMSWX ) - C�E��A��HO�O �OPMATA:
C                      -------------
C**********************************************************************
C
C
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
C     9              - ��A� KD COCTO�H��;
C    10              - PE�EPB
C********************************************************************
C
C
C
C                      MO�E�� O�PE�E��ET HA����E C��. ��PAB�EH�� "C��"
C                      HA BEHT��E �O A��OP�TM� CPABHEH�� O�OPHO�O
C                      � MO����P���E�O HA�P��EH��:
C
C                      UOPN, UOPN1 - �HA�EH�E O�OPHO�O HA�P. B TN, TN+H0
C                      UYPN, UYPN1 - �HA�EH�E MO����P. HA�P. B TN, TN+H0
C
C                     - �T� HA�P��EH��, A TAK �E �O�� "CX" � "TX" B MSWX
C                      HEO�XO��MO O�PE�E��T� TO��KO �P� "ZAPROS=3"
C                      �O�� B TOKXN, TOKXN1 OT (1) - �O��H� K �TOM�
C                      MOMEHT� TAK �E ��T� O�PE�E�EH�;
C
C                     - B OCTA��H�X "ZAPROC=1, 10, 20, 4", �OCTATO�HO
C                       �MET� O�PE�E�EH�M� TO��KO RMWX � VALLN/N1;
C
C
C                      �OPMAT MACC�BOB TOKXN � TOKXN1:
C
C      1 - C�OBO -> TOK BEHT��� ;       ( O�PE�E��ETC� ��� TN+H0 �P�
C                                         ZAPROS = 20 );
C      2 - C�OBO -> �HA�EH�E C��HA�A ��P. "C��" ( O�PE�. �P� ZAPROS=3);
C
C
C              DTCIU - ���TE��HOCT� �M����CA "C��", �P�
C                      DTCIU_PAC�ETHOE < DTMCIU[MASPRT(94)] --->
C                      C��TAETC�, �TO "C��" HA BEHT��� HE �O�ABA�OC�;
C
C              K, M - ���� �O�K�. BEHT���, NPEL - E�O �OP��. HOMEP.
C
C              "ECTECTBEHHOE" ��PAB�EH�E "C��":[ DU** = U**N1 - U**N ]:
C              �P� DUN < 0, A DUN1 >= 0, �EHEP�P�ETC� �EPE�. �POHT "C��"
C              �P� �TOM ���T�BAETC� KO� CX � TX BPEM� -> CX - KO�
C              PA�PE�EH�� (=1) ��� (=0) �A�PETA �O�A�� "C��"( CM. �/�
C                                                          MCIU50 );
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                 -  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE�
C                   �HA�EH�� "�POBO��MOCT�" �AKP�TO�O BEHT��� ( GVENT6 )
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
C                  <TOKXN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK �AKP�T�X BEHT��E� = 0.0;
C                  �A��C� �T�X TOKOB B MACC�B TOKXN1;
C                   !!! B��O�H�T� �O MO����KA��� PE�EH�� VALLN1 !!!
C
C                  C) ZAPROS = 3
C                     -----------
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
C
C              ��ET: ICLOSE - MO���� TOKA "�AKP�T��" BEHT���;
C                    UOPEN  - MO���� HA�P��EH�� "OTKP�T��" BEHT���;
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX_BEHT���   -
C                      ��KC�P�ETC� CMEHA COCTO�H�� BEHT��� �APAMETPOM
C                      KD_BEHT���;
C                     PABEHCTBO �POBEP�ETC� C TO�. �O "EPSKOM".
C
C   METO�:         PA�HOB��HOCT� BO�HOBO�O A��OP�TMA ��� �OPM�POBAH��
C   -----          MACC�BA LOGYTR --> �EPE� �/� GETLOG;
C
C                  "TEHEBA�" C�CTEMA �OPM�P�ETC� "MAC�TA��POBAHHA�"
C                   TAK, �TO: V[X] - V[Y] = <TOK    X-Y   BETB�>;
C                   ����, OTME�EHH�E B LOGYTR(*) = 0 - "�A�OB�E"
C                   ��� KA��O� �H���EHTHO� �P���� OTKP�T�X BEHT��E�;
C
C   �APAMETP�:     MSWX(RMSWX) - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------      ( MACC�B C�E�. HA�HA�EH�� );
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  T  =  TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  TN + H0;
C                  TOKXN  � TOKXN1 - �HA�EH�E �EPEMEHH�X "COCTO�H��"
C                                  MO�E�� �P� TN  �  TN + H0;
C                  ( 1-C�OBO -> TOK, 2-C�OBO "C��" );
C
C                KQ -  "AHO�" - HOMEP ���A;
C                MQ - "KATO�" - HOMEP ���A;
C
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
C   ------------   2. GETLOG,    MCIU50,       DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=1, 10,20,3,4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST = 1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C
C   **** BH�MAH�E *** B 06.01.87   ��OK�POBAHO B�BO� COO��EH���
C                                  O C�POCE B CTAPT. TO�K� �P�
C                                  MMCIU = 11 ***********
C
C
C
C   ****BH�MAH�E *****
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSWX(*), RMSWX(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKXN(*), TOKXN1(*)
         REAL JALL, JRL, L, IRLT, IRLN
* !!!! DIMENSION MSWALL(*), RMSWAL(*)
       INTEGER ADRMSW
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(94), DTMCIU )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(59), GVENT6 )
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
       K = KQ
       M = MQ
C---> B��OP PE��MA:
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997)         ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #00B9', ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "C�POC" �HA�EH�� TX - BO�MO�HO�O BPEMEH� �EPEK���EH��.....
         RMSWX(2) =      H0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSWX(9) = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KD = MSWX(1)
C--->    EC�� "K" � "M" �H���EHTH� --->  B��BAT� GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECT� "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG_( K, M, LOGYTR )
         RETURN
6607     CONTINUE
C===> BEHT��� �AKP�T ==> KD=0 <====== "MAKET" O�EH� MA�O� �POBO��MOCT�
C---> EC�� NVENT6 = 0.0, TO MAKET HE H��EH...
         IF ( GVENT6 .EQ. 0.0 ) RETURN
C--->
         G = GVENT6
C--->
C--->   �O��EMEHTH�� BK�A�:
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
C---->
         RETURN
C======================================================================
C$$$$$$$ ZAPROS = 10 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  �OPM�POBAH�E "TEHEBO�" C�CTEM� ��� �P��� OTKP�T�X BEHT��E�
6670     CONTINUE
         KOD = 0
         KD = MSWX(1)
         IF ( KD .EQ. 0 ) RETURN
C----------------------------------------------------------------------
         G = 1.0
C----------------------------------------------------------------------
C--->  B��POC�T� "�A�OB�E" ����    �H���EHTH�X �P���...
         IF ( K .EQ. 0 ) GO TO 6671
         IF ( LOGYTR(K) .EQ. -1 ) K = 0
6671     CONTINUE
         IF ( M .EQ. 0 ) GO TO 6672
         IF ( LOGYTR(M) .EQ. -1 ) M = 0
6672     CONTINUE
C--->  BK�A��BAEM....
C======================================================================
C--->
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
C--->
         RETURN
C======================================================================
C$$$$$$ZAPROS = 20 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C===> PAC��TAT� TOK� BEHT��E� !!! B��O�H�T� �O MO����KA��� VALLN1 !!!!
6660     CONTINUE
         KOD = 0
         KD = MSWX(1)
         IF ( KD .EQ. 0 ) GO TO 6661
C--->  BEHT��� OTKP�T, B��PAC�T� "�A�OB�E" ���� �O �AHH�M LOGYTR....
         IF ( K .EQ. 0 ) GO TO 6662
         IF ( LOGYTR(K) .EQ. -1 ) K = 0
6662     CONTINUE
         IF ( M .EQ. 0 ) GO TO 6663
         IF ( LOGYTR(M) .EQ. -1 ) M = 0
6663     CONTINUE
         TK = 0.0
         IF ( K .NE. 0 ) TK = VALLN1(K)
         TM = 0.0
         IF ( M .NE. 0 ) TM = VALLN1(M)
         TOKXN1( 1  ) = TK - TM
         RETURN
6661     CONTINUE
C--->  BEHT��� �AKP�T....
         TOKXN1( 1  )  =  0.0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKXN, TOKXN1, VALLN, VALLN1 O�PE�E�EH�------
C-----------------------------------------!!! VALLN1 MO������POBAHO !!!
C----O�PE�E��M TEK. � �PE�. COCTO�H�� (?)------------------------------
C
C**** PA��EPEMC� C "C��" **********************************************
       CALL MCIU50( TN, H0, UOPN, UOPN1, UYPN, UYPN1, DTCIU, DTMCIU,
     =              MSWX(3), RMSWX(3) )
C-->  �O�� MCIU ( MSWX(7) ) � HXCIU ( RMSWX(8) - O�PE�E�EH�
C
C-->  "C��"  B  TOKXN1
C
       TOKXX = 0.0
       MMCIU = MSWX(7)
       IF ( MMCIU .EQ. 01  .OR. MMCIU .EQ. 11 ) TOKXX = 1.0
       TOKXN1(2) = TOKXX
C----------------------------------------------------------------------
C===>    HOBOE HA�P��EH�E � TOK
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         TVT  = TOKXN1(  1   )
C--> A PAH��E ���O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         TVN  = TOKXN(  1   )
C----�A�MEMC� COCTO�H��M�----------------------------------------------
C!!!-------------------------------------------------------------------
C      KD = MSWX(1)
C      CALL SEQSOS(NPEL,    TN, KD, TVN,  UN )
C      MSWX(1) = KD
C!!!-------------------------------------------------------------------
         KD = MSWX(1)
         IF ( KD .EQ. 0 ) GO TO 601
C---> BEHT��� OTKP�T (KD=1), HE XO�ET �� OH �AKP�T�C�?
C        IF (  UN .GT. 0.0  .AND. TVN  .GE. 0.0
C    =                     .AND.  TVT  .LT. 0.0  ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  KOPPEKT�POBKA TVN ********
       IF ( TVN .LT. 0.0  .AND. TVN .LE. -ICLOSE ) TVN = 0.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF (
     =                          TVT .LT. -ICLOSE ) GO TO 602
C                                   @@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C---> �HA�E COCTO�H�E BEHT���A HE ��MEH��OC�....
         KOD = 0
         RETURN
602      CONTINUE
C--->  BEHT��� #�AKP��C�#  B TE�EH�� BPEMEH� OT TN �O TN+H0....
C      KO��A �TO MO��O C����TC�?   ( HX - PEA��HO B��O�HEHH�� �A� )....
C !!!! C�PABE���BO �P� TVN = (-ICLOSE) !!!
         HX =    + H0 * (  (-ICLOSE - TVN) /(TVT - TVN)  )
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 604 ) NPEL,   KD, TN, H0, HX,
     =                                           TVN, TVT, ICLOSE
604   FORMAT(' *** ��EMEHT # ', I8, ' MO�E�� "BEH. #_B9" ==> ???? !?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***'/' *** TVN = ', E12.5, ' TVT = ',E12.5,
     =   ' ICLOSE = ', E12.5,  ' ***' )
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSWX(2) = HX
C--->  EC�� TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMET�T� �TO B "FKOM"....
         FKOM = 1
C@@@@  ��A� KD  ��KC�P�EM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       MSWX(9) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
         KOD = 0
C===> BEHT��� �AKP�T (KD=0), HE �E�AET �� OH OTKP�T�C�?....
         IF ( UT .LE. UOPEN  ) GO TO 6030
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C--->  �TO �E�AET "C��" ???????????????
         MMCIU = MSWX(7)
         IF (                      MMCIU .EQ. 01 ) GO TO 603
C               @@@@@@
C-->  MO�ET ��T� M� �ABHO "XO�EM" ?
         IF (                      MMCIU .EQ. 11 ) GO TO 603
C                @@@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C@@@@@@-->> OT 06.01.87. <<-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  O�PA�OTKA KOH�A....
       IF (                     MMCIU .EQ. 00 ) GO TO 6030
C-->  MMCIU = 10  -->  C�� "KOH�AET"
       HX = 0.0
       IF ( UN .GT. UOPEN ) GO TO 606
       HX = +H0 * ( (UOPEN - UN) / (UT-UN) )
       HXCIU = RMSWX(8)
       IF ( HX .LE. HXCIU ) GO TO 606
6030   CONTINUE
C@@@@@@--> OT 06.01.87. <<-- KOHE�-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===>  HET, HE BO��E�A�, ��� HE �A�� --> COCTO�H�E HE MEH�ETC�...
         KOD = 0
         RETURN
603      CONTINUE
C===>   BEHT��� B TE�EH�� TN  <-->  TN+H0 �O��EH OTKP�T�C� ...
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C       KO��A �O�B��C� "C��" ???????????
         HXCIU = RMSWX(8)
         HX = HXCIU
         IF ( UN .GT. UOPEN ) GO TO 606
C                @@@@@
C--->  UN  �  UT  "PA�HO�O" �HAKA...
C   HX  ->  "�A�" "ECTECTBEHHO�" KOM�TA���
         HX  =    + H0 * (  ( UOPEN -  UN ) /(UT - UN)  )
C-->  KO��A EM� "C��" PA�PE���O...
         IF ( HXCIU .GT. HX ) HX = HXCIU
606      CONTINUE
C--->
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C!!!--------**** BH�MAH�E ****-----------------------------------------
         IF ( MMCIU .NE. 11 .AND. HX .LE. 0.0 ) WRITE (NWTRE, 605 )
     =  NPEL, KD, TN, H0, HX,                    UN, UT, MMCIU
605   FORMAT(' *** ��EMEHT # ', I8, ' MO�E�� "BEH. #_B9" ==> ???? !?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5,  ' ***'/ ' *** UN = ', E12.5, ' UT = ', E12.5,
     =  ' MCIU = ', I2,  ' ***'  )
C!!!-------------------------------------------------------------------
         IF ( HX  .LT. HXMIN ) HX  = HXMIN
         RMSWX(2) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@  ��A� KD  ��KC�P�EM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       MSWX(9) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        KOD = 0
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ��KCA��� CMEH� COCTO�H�� MO�E��.................ZAPROS = 4
C---->  HET �� HEO�XO��MOCT� �PO��BECT� CMEH� COCTO�H�� BEHT���....?
C@@@@  ��A� KD �CTAHOB�EH ?
       IF ( MSWX(9) .NE. -1 ) RETURN
C@@@ MO�HO �POBEP�T� @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC�� TXG   "�P�MEPHO" PABHO TX=MO�E��, TO CMEHA H��HA!
         HX   = RMSWX(2)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> �HA�E CMEH�M COCTO�H�E....
         KD = MSWX(1)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSWX(1) = KD
C---->  EC�� BEHT��� �AKP��C�, �A��KC�P�EM E�O HOMEP.....
         IF ( KD .EQ. 0 ) NTXG = NPEL
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) NPEL,   KD, TN, TXG
501     FORMAT(' ***FTEST_1: B ��.#',I8, ' KD CTA�O =',I2,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
