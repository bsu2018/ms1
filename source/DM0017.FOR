         SUBROUTINE DM0017 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSAWL,  ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0017 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSAWL,  ADRMSW )
C   HA�HA�EH�E:    MOH�TOP MO�E�� ��EA�. K���   [ MO�. #667, �.T�� # 6 ]
C   -----------    ****************************************************
C
C
C
C      MO�E�� KO��P�ETC�:
C
C      � # T 667 � "K" "M" � MAS < T(1) T(2) T(3) ... T(KT) >;
C
C
C
C
C                           K���
C                           BK���.
C                            I
C       *********       **********               *********
C       *********       **********               *********
C       *********       **********               *********
C  T0*--*********-------**********---------------*********---------*--->
C       T(1)    T(2)    T(3)     T(4)            T(J)    T(J+1)    T(KT)
C
C
C            K��� �AKP�T ( B�K�. ) -->
C            -----------
C            TN < T(1)
C            TN > T(KT)  --> KT �ETHOE;
C
C            K��� OTKP�T ( BK�. ) -->
C            -----------
C            TN >=T(KT) --> KT HE �ETHOE;
C            T(J) <= TN <= T(J+1) --> J HE �ETHOE;
C
C
C  !!! �P� KT = 0 - K��� BCE��A �AKP�T !!!
C
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                 -  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE�
C                   �HA�EH�� "�POBO��MOCT�" �AKP�TO�O K���A   ( GVENT6 )
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
C                  BPEMEH�. �P� �TOM TOK OTKP�T�X K���E�  :(���� K � M )
C                  <TOKN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK �AKP�T�X BEHT��E� = 0.0;
C                  �A��C� �T�X TOKOB B MACC�B TOKN1;
C                   !!! B��O�H�T� �O MO����KA��� PE�EH�� VALLN1 !!!
C
C                  C) ZAPROS = 3
C                     -----------
C                     �O��BO��TC� AHA��� HA BO�MO�H�� CMEH� COCTO�H��
C                     K���A   [ OTKP�T <==> �AKP�T ]:
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
C
C                  D) ZAPROS = 4
C                     **********
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX[RMSW(12)] -
C                      ��KC�P�ETC� CMEHA COCTO�H�� BEHT��� �APAMETPOM
C                      KD [MSW(11)];
C                     PABEHCTBO �POBEP�ETC� C TO�. �O "EPSKOM".
C
C   METO�:         PA�HOB��HOCT� BO�HOBO�O A��OP�TMA ��� �OPM�POBAH��
C   -----          MACC�BA LOGYTR --> �E�E� �/� GETLOG;
C
C                  "TEHEBA�" C�CTEMA �OPM�P�ETC� "MAC�TA��POBAHHA�"
C                   TAK, �TO: V[X] - V[Y] = <TOK    X-Y   BETB�>;
C                   ����, OTME�EHH�E B LOGYTR(*) = 0 - "�A�OB�E"
C                   ��� KA��O� �H���EHTHO� �P���� OTKP�T�X K���E�;
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
C                  �APAMETP� "C��" XPAH�T�C� A MSW - O��CAH�� ��EMEHTA;
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. GETLOG,  SEQSOS, DEPOST, MCIU01;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=1, 10,20,3,4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST = 1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 667;
C                ----------------------------------
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
C                  RMSW T��A #  667 [ ��EA��H�� K��� ]
C                  -----------------------------------
C
C
C
C                              SRAB
C +----+---++-------+----------------+...>
C I��- I��-II TEK.  I BPEM� �PE�-�   I                  [*] - XPAH�TC�
C IE�  IE� II COCT. I KOMM�TA���     I                        �HA�EH�E
C I"K" I"M"II "KD"  I    "TX"  [*]   I                       HX - PEA��-
C +----+---++-------+----------------+...>                   HO B��O�HE-
C I 9  I 10II  11   I      12        I                       HH�� �A�:
C +----+---++-------+---+---++---+---+...>                   TX=TN+HX;
C
C    K��� OTKP�T ==>  KD=1;;;;K��� �AKP�T ==>  KD=0;
C@@
C
C +------+------+------+-------+------+-------+--------+
C I MMCIUI HXCIUI  KT  I T(1)  I T(2) I ..... I T(KN)  I
C +------+------+------+-------+------+-------+--------+
C I 13   I  14  I  15  I  16   I  17  I ....  I 15+KT  I
C +------+------+------+-------+------+-------+--------+
C  MICU � HXCIU - �AP-P� "BOCTAHOB�EHHO�O" �M����CA PA�PE�EH��
C
C  KT  - KO�-BO TO�EK "KAPT�" K���A
C
C  !!! OT �E�HOCT� -> EC�� BO�MO�HO �EPEK���EH�E K���A, TO B
C                     �O�E MMCIU �CTAHAB��BAETC� = -1
C                     MMCIU �C�O����ETC� KAK ��A� KD !!!!!!!!!
C  !!!
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
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
        DIMENSION MSWALL(*),  RMSAWL(*)
        INTEGER ADRMSW
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
C---->   ���� �O�K���EH��:
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO�� T��A
         IF ( MSW(2) .NE. 667 ) GO TO 9999
C---> B��OP PE��MA:
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "C�POC" �HA�EH�� TX - BO�MO�HO�O BPEMEH� �EPEK���EH��.....
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C######IF ( RMSW(16) .EQ. TN ) MSW(11) = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         RMSW(12) =      H0
         KD = MSW(11)
C--->    EC�� "K" � "M" �H���EHTH� --->  B��BAT� GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECT� "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG_( K, M, LOGYTR )
         RETURN
6607     CONTINUE
C===> K��� �AKP�T ==> KD=0 <====== "MAKET" O�EH� MA�O� �POBO��MOCT�
C---> EC�� NVENT6 = 0.0, TO MAKET HE H��EH...
         IF ( GVENT6 .EQ. 0.0 ) RETURN
C--->
         G = GVENT6
C--->
C--->   �O��EMEHTH�� BK�A�:
C---->
C======================================================================
         CALL DEPOST(K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$$ ZAPROS = 10 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  �OPM�POBAH�E "TEHEBO�" C�CTEM� ��� �P��� OTKP�T�X
6670     CONTINUE
         KOD = 0
         KD = MSW(11)
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
         CALL DEPOST(K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$ZAPROS = 20 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C===> PAC��TAT� TOK� K���A    !!! B��O�H�T� �O MO����KA��� VALLN1 !!!!
6660     CONTINUE
         KOD = 0
         ITOK = MSW(7)
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C-->>  O�PE�E��T� C��HA� "C��", �A�OMH�T� �M����C...
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  B MSW(13)  MMCIU,   B RMSW(14)  HXCIU
       KT = MSW(15)
       MMCIU = 00
       HXCIU = 0.0
       IF ( KT .EQ. 0 ) GO TO 6664
       TN1 = TN + H0
C----------------------------------------------------------------------
       IF ( TN .LE. RMSW(16) ) GO TO 6665
       KT1  = KT - 1
       DO 6666 J=1, KT1
       IF ( RMSW(15+J) .LE. TN
     =         .AND.
     =      TN .LT. RMSW(15+J+1) ) GO TO 6667
6666   CONTINUE
C-->  �A �OC�E�HE� "�EPTO�"
       IF ( MOD(KT,2) .EQ. 0 ) GO TO 6664
       MMCIU = 11
       GO TO 6664
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
6665   CONTINUE
C-->  TO���MC� B HA�A�E
       IF ( TN1 .LT. RMSW(16) ) GO TO 6664
       MMCIU = 01
       HXCIU = RMSW(16) - TN
       GO TO 6664
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
6667   CONTINUE
C--> ME��� (J)  �   (J+1) TO�KO�
       IF ( MOD(J,2)  .EQ.  0 ) GO TO 6668
C-->  �OP� ( HE �ET )
       MMCIU = 11
       IF ( TN1 .LT. RMSW(15+J+1) ) GO TO 6664
       MMCIU = 10
       HXCIU = RMSW(15+J+1 ) - TN
       GO TO 6664
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
6668   CONTINUE
C-->  M�  B  �..(�ME)   -->  �ET
       IF ( TN1 .LT. RMSW(15+J+1) ) GO TO 6664
       MMCIU = 01
       HXCIU = RMSW(15+J+1) - TN
C***** GO TO 6664
C--
6664   CONTINUE
       RMSW(14) = HXCIU
       MSW(13)  = MMCIU
C######################################################################
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         TOKXX = 0.0
         IF ( MMCIU .EQ. 01   .OR.  MMCIU .EQ. 11 ) TOKXX = 1.0
C
       TOKN1( ITOK+1 ) = TOKXX
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         KD = MSW(11)
         IF ( KD .EQ. 0 ) GO TO 6661
C--->  K��� OTKP�T, B��PAC�T� "�A�OB�E" ���� �O �AHH�M LOGYTR....
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
C--->  K��� �AKP�T....
         TOKN1(ITOK)  =  0.0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 O�PE�E�EH�-------
C-----------------------------------------!!! VALLN1 MO������POBAHO !!!
         KD = MSW(11)
         IF ( KD .EQ. 0 ) GO TO 601
C---> K��� OTKP�T (KD=1), HE XO�ET �� OH �AKP�T�C�?
C-->  �AKPOETC� TO��KO �P�  MCIU => 10 ��� 00
         MMCIU = MSW(13)
         IF ( MMCIU .EQ. 10  .OR.  MMCIU .EQ. 00 ) GO TO 602
C---> �HA�E COCTO�H�E K���A HE ��MEH��OC�....
         KOD = 0
         RETURN
602      CONTINUE
C--->  K��� #�AKP��C�#  B TE�EH�� BPEMEH� OT TN �O TN+H0....
C      KO��A �TO MO��O C����TC�?   ( HX - PEA��HO B��O�HEHH�� �A� )....
         HX = RMSW(14)
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 604 ) MSW(1), KD, TN, H0, HX
604   FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "K���_667" ==> COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSW(12) = HX
C--->  EC�� TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMET�T� �TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(13) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
         KOD = 0
C===> K��� �AKP�T (KD=0), HE �E�AET �� OH OTKP�T�C�?....
C-> OTKPOETC� �P� MCIU => 01  ���  11
         MMCIU = MSW(13)
         IF ( MMCIU .EQ. 01  .OR.  MMCIU .EQ. 11 ) GO TO 603
C===>  HET, HE BO��E�A�, COCTO�H�E HE MEH�ETC�...
         KOD = 0
         RETURN
603      CONTINUE
C===>   K��� B TE�EH�� TN  <-->  TN+H0 �O��EH OTKP�T�C� ...
C       KO��A ???
         HX  = RMSW(14)
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 605 ) MSW(1), KD, TN, H0, HX
605   FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "K���_667" ==> COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5,  ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSW(12) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(13) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ��KCA��� CMEH� COCTO�H�� MO�E��.................ZAPROS = 4
C---->  HET �� HEO�XO��MOCT� �PO��BECT� CMEH� COCTO�H�� K���A...?
C@@@ ��A� KD  PA�PE�AET...?    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( MSW(13) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC�� TXG   "�P�MEPHO" PABHO TX=MO�E��, TO CMEHA H��HA!
         HX   = RMSW(12)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> �HA�E CMEH�M COCTO�H�E....
         KD = MSW(11)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSW(11) = KD
C---->  EC�� K��� �AKP��C�, �A��KC�P�EM E�O HOMEP.....
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
9998     FORMAT( ' *** �O�. C�O� B DM0017 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
         END
