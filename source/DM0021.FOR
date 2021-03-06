         SUBROUTINE DM0021 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA:  DM0021 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    MOH�TOP�:
C   -----------      MO�E�� BEHT���, ��P. C��� [MO�.# 93,�.T�� #6]
C                    MO�E�� K���A  , ��P. C��� [MO�.#668,�.T�� #6]
C                  ****************************************************
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
C                  �A��C� �T�X TOKOB B MACC�B TOKN1;
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
C                   - �P� TX_G  [MASPRT(35)] .EQ. HX[RMSW(15)] -
C                      ��KC�P�ETC� CMEHA COCTO�H�� BEHT��� �APAMETPOM
C                      KD [MSW(14)];
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
C   ------------   2. GETLOG,  SEQSOS, DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=1, 10,20,3,4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST = 1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 93 � # 668
C                ---------------------------------------
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
C
C
C
C
C
C        RMSW T��A # 93 ��� # 668 [ BEHT���(K���), ��P. C��� ]
C        -----------------------------------------------------
C    SPM   I                SRAB
C +----+---+------+-----+---+------+----+--------+----+
C INELTINSMI ATOKMI KD  IHX I MCIU I HX I        I Q  I
C I    I   I      I     I   I      ICIU I PE�EPB I    I
C I    I   I      I     I   I      I    I        I    I
C +----+---+------+-----+---+------+----+--------+----+
C I  1 I 2 I   3  I  4  I 5 I   6  I  7 I   8,9  I 10 I
C +----+---+------+-----+---+------+----+--------+----+
C I 11 I12 I  13  I 14  I15 I  16  I 17 I  18,19 I 20 I
C +----+---+------+-----+---+------+----+--------+----+
C
C      BEHT��� OTKP�T -> KD=1;    BEHT��� �AKP�T ->  KD=0;
C      HX - �A� �O BPEMEH�, BPEM� BO�MO�HO�O �EPEK���EH��;
C
C        MCIU  �  HXCIU - �APAMETP� "BOCTAHOB�EHHO�O" �MP���CA "C��"
C
C  Q - ��A� KD;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JRL, L, IRLT, IRLN
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADRMSW , ATOKM
         EQUIVALENCE ( RMASPT(1),MASPRT(1) )
         EQUIVALENCE ( HXMIN , MASPRT(62) )
         EQUIVALENCE ( NWTRE , MASPRT(14) )
         EQUIVALENCE ( GVENT6, MASPRT(59) )
         EQUIVALENCE ( FTEST , MASPRT(60) )
         EQUIVALENCE ( OPEN6 , MASPRT(57) )
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
         INTEGER FKOM, ZAPROS,TIP
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
         TIP = MSW(2)
C--------------------------------------------------------------------
C---->   ���� �O�K���EH��:
C--------------------------------------------------------------------
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO�� T��A
         IF ( TIP .NE. 93  .AND. TIP .NE. 668 ) GO TO 9999
C--------------------------------------------------------------------
C---> B��OP PE��MA:
C--------------------------------------------------------------------
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C--------------------------------------------------------------------
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "C�POC" �HA�EH�� TX - BO�MO�HO�O BPEMEH� �EPEK���EH��.....
         RMSW(15) =  H0
         MSW(20) = 0
         KD = MSW(14)
C--->    EC�� "K" � "M" �H���EHTH� --->  B��BAT� GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECT� "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG_( K, M, LOGYTR )
         RETURN
6607     CONTINUE
C--------------------------------------------------------------------
C===> BEHT���(K���) �AKP�T ==> KD=0 <=="MAKET" O�EH� MA�O� �POBO��MOCT�
C---> EC�� NVENT6 = 0.0, TO MAKET HE H��EH...
         IF ( GVENT6 .EQ. 0.0 ) RETURN
         G = GVENT6
C--------------------------------------------------------------------
C--->   �O��EMEHTH�� BK�A�:
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$$ ZAPROS = 10 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->�OPM�POBAH�E "TEHEBO�"C�CTEM� ��� �P��� OTKP�T�X BEHT��E�(K���E�)
6670     CONTINUE
         KOD = 0
         KD = MSW(14)
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
C======================================================================
C--->  BK�A��BAEM....
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$ZAPROS = 20 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C===> PAC��TAT� TOK� BEHT��E� !!! B��O�H�T� �O MO����KA��� VALLN1 !!!!
6660     CONTINUE
         KOD = 0
         ITOK = MSW(7)
         KD = MSW(14)
         IF ( KD .EQ. 0 ) GO TO 6661
C--->  BEHT���(K���) OTKP�T, B��POC�T� "�A�OB�E" ���� �O �AHH�M LOGYTR..
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
         TOKN1(ITOK) = TK - TM
         RETURN
6661     CONTINUE
C--->  BEHT��� (K���)  �AKP�T....
         TOKN1(ITOK)  =  0.0
         RETURN
C======================================================================
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 O�PE�E�EH�-------
C-----------------------------------------!!! VALLN1 MO������POBAHO !!!
C-->>  O�PE�E��T� C��HA� "C��", �A�OMH�T� MCIU  � HXCIU....
         ITOK = MSW(7)
         ATOKM = MSW(13)
         MCIU  = IFIX ( TOKN1(ATOKM) )
         HXCIU = TOKN1( ATOKM + 6 )
         MSW(16) = MCIU
         TOKXX = 0.0
         IF ( MCIU .EQ. 01  .OR. MCIU .EQ. 11 ) TOKXX = 1.0
         TOKN1( ITOK+1 ) = TOKXX
         RMSW(17) = HXCIU
         IF( TIP .EQ. 668 ) GO TO 6015
C--------------------------------------------------------------------
C----O�PE�E��M TEK. � �PE�. COCTO�H�� (?)----------------------------
C===>    HOBOE HA�P��EH�E � TOK
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         TVT  = TOKN1( ITOK )
C--> A PAH��E ���O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         TVN  = TOKN( ITOK )
C======================================================================
C----�A�MEMC� COCTO�H��M�----------------------------------------------
6015     CONTINUE
         KD = MSW(14)
         IF ( KD .EQ. 0 ) GO TO 601
         IF ( TIP .EQ. 668 ) GO TO 6016
C----------------------------------------------------------------------
C---> BEHT��� OTKP�T (KD=1), HE XO�ET �� OH �AKP�T�C�?
C  KOPPEKT�POBKA TVN ��� T�� 93
C----------------------------------------------------------------------
       IF ( TVN .LT. 0.0  .AND. TVN .LE. -ICLOSE ) TVN = 0.0
       IF ( TVT .LT. -ICLOSE ) GO TO 602
C----------------------------------------------------------------------
C---> �HA�E COCTO�H�E BEHT��� HE ��MEH��OC�....
         KOD = 0
         RETURN
6016     CONTINUE
C----------------------------------------------------------------------
C---> K��� OTKP�T (KD=1), HE XO�ET �� OH �AKP�T�C�?
C----------------------------------------------------------------------
       MCIU = MSW(16)
       IF ( MCIU .EQ. 10 .OR. MCIU .EQ. 00 ) GO TO 602
C----------------------------------------------------------------------
C---> �HA�E COCTO�H�E K���A HE ��MEH��OC�....
         KOD = 0
         RETURN
602      CONTINUE
C--->  BEHT���(K���)   #�AKP��C�#  B TE�EH�� BPEMEH� OT TN �O TN+H0....
C      KO��A �TO MO��O C����TC�?   ( HX - PEA��HO B��O�HEHH�� �A� )....
C !!!! C�PABE���BO �P� TVN = (-ICLOSE) !!!
         IF ( TIP .EQ.  93 ) HX = H0 * ( (-ICLOSE - TVN) /(TVT - TVN))
         IF ( TIP .EQ. 668 ) HX = RMSW(17)
C!!!-------------------------------------------------------------------
         IF ( HX .LT. 0.0 ) WRITE (NWTRE, 604 ) MSW(1), KD, TN, H0, HX
604   FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "BEHT���(K���)->COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSW(15) = HX
C--->  EC�� TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMET�T� �TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(20) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
         KOD = 0
         IF ( TIP .EQ. 668 ) GO TO 6017
C===> BEHT��� �AKP�T (KD=0), HE �E�AET �� OH OTKP�T�C�?....
         IF( UT .LE. UOPEN ) GO TO 6030
C======================================================================
C--->  �TO �E�AET "C��" ???????????????
         MCIU = MSW(16)
         IF ( MCIU .EQ. 01 ) GO TO 603
C-->  MO�ET ��T� M� �ABHO "XO�EM" ?
         IF ( MCIU .EQ. 11 ) GO TO 603
C======================================================================
C@@@@@@-->> OT 06.01.87. <<-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  O�PA�OTKA KOH�A....
       IF ( MCIU .EQ. 00 ) GO TO 6030
C-->   MCIU = 10  -->  C�� "KOH�AET"
       HX = 0.0
       IF ( UN .GT. UOPEN ) GO TO 606
       HX = +H0 * ( (UOPEN - UN) / (UT-UN) )
       HXCIU = RMSW(17)
       IF ( HX .LE. HXCIU ) GO TO 606
6030   CONTINUE
C@@@@@@--> OT 06.01.87. <<-- KOHE�-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===>  HET, HE BO��E�A�, ��� HE �A�� --> COCTO�H�E HE MEH�ETC�...
         KOD = 0
         RETURN
6017     CONTINUE
C===> K��� �AKP�T (KD=0), HE �E�AET �� OH OTKP�T�C�?....
C-> OTKPOETC� �P� MCIU => 01  ���  11
         MCIU = MSW(16)
         IF ( MCIU .EQ. 01  .OR.  MCIU .EQ. 11 ) GO TO 603
C===>  HET, HE BO��E�A�, COCTO�H�E HE MEH�ETC�...
         KOD = 0
         RETURN
603      CONTINUE
C======================================================================
C===>   BEHT���(K���) B TE�EH�� TN  <-->  TN+H0 �O��EH OTKP�T�C� ...
C       KO��A �O�B��C� "C��" ???????????
         HXCIU = RMSW(17)
         HX = HXCIU
         IF ( TIP .EQ. 668  ) GO TO 606
         IF ( UN .GT. UOPEN ) GO TO 606
C--->  UN  �  UT  "PA�HO�O" �HAKA...
C   HX  ->  "�A�" "ECTECTBEHHO�" KOM�TA���
         HX  =    + H0 * (  ( UOPEN -  UN ) /(UT - UN)  )
C-->  KO��A EM� "C��" PA�PE���O...
         IF ( HXCIU .GT. HX ) HX = HXCIU
606      CONTINUE
C!!!-------------------------------------------------------------------
         IF ( HX .LT. 0.0 ) WRITE (NWTRE, 605 ) MSW(1), KD, TN, H0, HX,
     =                                 MCIU
605   FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "BEHT���(K���)->COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' MCIU = ', I2, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX  .LT. HXMIN ) HX  = HXMIN
         RMSW(15) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(20) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ��KCA��� CMEH� COCTO�H�� MO�E��.................ZAPROS = 4
C---->  HET �� HEO�XO��MOCT� �PO��BECT� CMEH� COCTO�H�� BEHT���(K���A)?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( MSW(20) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC�� TXG   "�P�MEPHO" PABHO TX=MO�E��, TO CMEHA H��HA!
         HX   = RMSW(15)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> �HA�E CMEH�M COCTO�H�E....
         KD = MSW(14)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSW(14) = KD
C---->  EC�� BEHT��� (K���)  �AKP��C�, �A��KC�P�EM E�O HOMEP.....
         IF ( KD .EQ. 0 ) NTXG = MSW(1)
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) MSW(1), KD, TN, TXG
501     FORMAT(' ***FTEST_1: B ��-TE #',I5, ' KD CTA�O =',I2,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0021 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
          RETURN
         END
