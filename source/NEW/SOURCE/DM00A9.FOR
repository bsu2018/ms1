         SUBROUTINE DM00A9 (K, M, MSWX, RMSWX, YALL, JALL, LOGYTR,
     *                       TOKNX, TOKN1X, VALLN, VALLN1, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM00A9 (K, M,  MSWX, RMSWX, YALL, JALL, LOGYTR,
C   -------------            TOKNX, TOKN1X, VALLN, VALLN1, KOD )
C   HA�HA�EH�E:    MOH�TOP MO�E�� ��P. BEHT���  [ MO�. # A9, �.T�� # 6 ]
C   -----------    ***   KOM�OHEHT C�O�H�X BEHTE��H�X �PEO�PA�OBATE�E�*
C
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
C                  <TOKN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK �AKP�T�X BEHT��E� = 0.0;
C                  �A��C� �T�X TOKOB B MACC�B TOKN1X;
C                   !!! B��O�H�T� �O MO����KA��� PE�EH�� VALLN1 !!!
C
C=======
C   MACC�B�  TOKNX  �   TOKN1X �ME�T CTP�KT�P�:
C
C   1-E C�OBO := �HA�EH�E TOKA BEHT���;
C
C   2-E C�OBO := �HA�EH�E C��HA�A ��PAB�EH�� ("C��")1
C
C========
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
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX[RMSWX( 5)] -
C                      ��KC�P�ETC� CMEHA COCTO�H�� BEHT��� �APAMETPOM
C                      KD [MSWX( 4)];
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
C   �APAMETP�:   MSWX, RMSWX - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------   !! B # A9 --> C�E�. CTP�KT�P� !!!!!!!!!!!!!!!!!!!!!!!!!
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  T  =  TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  TN + H0;
C                  TOKNX  � TOKN1X - �HA�EH�E �EPEMEHH�X "COCTO�H��"
C                                  MO�E�� �P� TN  �  TN + H0;
C                  ( PA�MEPHOCT� -> ��� A9 -->  �BE �EPEMEHH�E !!!!!!!!
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 6;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C                  K  �  M  ���� �O�K���EH�� BEHT���:
C
C              K --I>--- M
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. GETLOG,  SEQSOS,         DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=1, 10,20,3,4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST = 1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C
C               PA�O��� MACC�B T��A  MSW T��A # A9;
C                ----------------------------------
C
C  +-------+--------+--------+--------+------+
C  I HOMEP I  "KD"  I "HX"   I MMCIU  I  HX  I
C  I �O� - I �P��-  I BPEM�  I KO� ��PI CIU  I
C  I ��EME I HAK CO-I �PE�-O�I �M���� I �M-CAI
C  I HTA   I CTO�H��I KOM-���I  CA    I      I
C  +-------+--------+--------+--------+------+
C  I  1    I   2    I   3    I    4   I   5  I
C  +-------+--------+--------+--------+------+
C
C   MMCIU  �  HXCIU O�PE�E��ET MOH�TOP MAKPOMO�E�� BEHT���......
C
C
C    BEHT��� OTKP�T ==>  KD=1;;;;BEHT��� �AKP�T ==>  KD=0;
C !!!  �O�E MMCIU OT �E�HOCT� �C�O����ETC� KAK ��A� KD  !!!!!
C  MCIU � HXCIU - �AP-TP� "BOCTAHOB�EHHO�O" �M����CA "C��" ( CM. MCIU01)
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSWX(*), RMSWX(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKNX(*), TOKN1X(*)
         REAL JALL
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
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
C--->  KOHTPO�� T��A  HE �POBO��TC�......
C---> B��OP PE��MA:
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997)         ZAPROS
9997     FORMAT( ' *** DM_00A9 --->', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "C�POC" �HA�EH�� TX - BO�MO�HO�O BPEMEH� �EPEK���EH��.....
         RMSWX( 3) =      H0
         KD = MSWX( 2)
C--->    EC�� "K" � "M" �H���EHTH� --->  B��BAT� GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECT� "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG( K, M, LOGYTR )
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
         KD = MSWX( 2)
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
C
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C-->> B��PAT�     C��HA� "C��", �A�OMH�T� MCIU  � HXCIU....
         MMCIU = MSWX(4)
         TOKN1X(2) = 0.0
         IF ( MMCIU .EQ. 01  .OR.  MMCIU .EQ. 11 ) TOKN1X(2) = 1.0
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         KD = MSWX( 2)
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
         TOKN1X( 1  ) = TK - TM
         RETURN
6661     CONTINUE
C--->  BEHT��� �AKP�T....
         TOKN1X( 1  )  =  0.0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 O�PE�E�EH�-------
C-----------------------------------------!!! VALLN1 MO������POBAHO !!!
C----O�PE�E��M TEK. � �PE�. COCTO�H�� (?)------------------------------
C$$$$$$$$
C===>    HOBOE HA�P��EH�E � TOK
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         TVT  = TOKN1X( 1    )
C--> A PAH��E ���O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         TVN  = TOKNX( 1    )
C----�A�MEMC� COCTO�H��M�----------------------------------------------
C!!!-------------------------------------------------------------------
C      KD = MSWX( 2)
C      CALL SEQSOS( MSWX(1), TN, KD, TVN,  UN )
C      MSWX(2 ) = KD
C!!!-------------------------------------------------------------------
         KD = MSWX(2 )
         IF ( KD .EQ. 0 ) GO TO 601
C---> BEHT��� OTKP�T (KD=1), HE XO�ET �� OH �AKP�T�C�?
C        IF (  UN .GT. 0.0  .AND. TVN  .GE. 0.0
C    =                     .AND.  TVT  .LT. 0.0  ) GO TO 602
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  KOPPEKT�POBKA TVN ********
       IF ( TVN .LT. 0.0  .AND. TVN .LE. -ICLOSE ) TVN = 0.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
       IF (
     =                          TVT .LT. -ICLOSE ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ###QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
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
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 604 ) MSWX(1), KD, TN, H0, HX
604   FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "�.BEHT_A9" ==> COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSWX( 3) = HX
C--->  EC�� TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMET�T� �TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSWX(4) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
         KOD = 0
C===> BEHT��� �AKP�T (KD=0), HE �E�AET �� OH OTKP�T�C�?....
         IF ( UT .LE. UOPEN ) GO TO 6030
CQQQQQQQQQQ$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  �TO �E�AET "C��" ???????????????
         MMCIU = MSWX( 4)
         IF (                      MMCIU .EQ. 01 ) GO TO 603
C-->  MO�ET ��T� M� �ABHO "XO�EM" ?
         IF (                      MMCIU .EQ. 11 ) GO TO 603
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C@@@@@@-->> OT 06.01.87. <<-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  O�PA�OTKA KOH�A....
       IF (                     MMCIU .EQ. 00 ) GO TO 6030
C-->  MMCIU = 10  -->  C�� "KOH�AET"
       HX = 0.0
       IF ( UN .GT. UOPEN ) GO TO 606
       HX = +H0 * ( (UOPEN - UN) / (UT-UN) )
       HXCIU = RMSWX(5)
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
         HXCIU = RMSWX( 5)
         HX = HXCIU
         IF ( UN .GT. UOPEN ) GO TO 606
C--->  UN  �  UT  "PA�HO�O" �HAKA...
C   HX  ->  "�A�" "ECTECTBEHHO�" KOM�TA���
         HX  =    + H0 * (  ( UOPEN -  UN ) /(UT - UN)  )
C-->  KO��A EM� "C��" PA�PE���O...
         IF ( HXCIU .GT. HX ) HX = HXCIU
606      CONTINUE
C--->
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 605 ) MSWX(1), KD, TN, H0, HX
605   FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "BEHT_A9  " ==> COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5,  ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX  .LT. HXMIN ) HX  = HXMIN
         RMSWX( 3) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSWX(4) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
           KOD = 0
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ��KCA��� CMEH� COCTO�H�� MO�E��.................ZAPROS = 4
C---->  HET �� HEO�XO��MOCT� �PO��BECT� CMEH� COCTO�H�� BEHT���A...?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( MSWX(4) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC�� TXG   "�P�MEPHO" PABHO TX=MO�E��, TO CMEHA H��HA!
         HX   = RMSWX( 3)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> �HA�E CMEH�M COCTO�H�E....
         KD = MSWX( 2)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSWX( 2) = KD
C---->  EC�� BEHT��� �AKP��C�, �A��KC�P�EM E�O HOMEP.....
         IF ( KD .EQ. 0 ) NTXG = MSWX(1)
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) MSWX(1), KD, TN, TXG
501     FORMAT(' ***FTEST_1: B ��-TE #',I5, ' KD CTA�O =',I2,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
