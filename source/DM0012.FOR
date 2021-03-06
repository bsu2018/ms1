         SUBROUTINE DM0012 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0012 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTA �E��
C                  T��A # 71, # 72 � 720 [ HE��HE�H�E R  �  L  ]
C                  ********************************************
C
C                   B.A.X     - �A�AETC� C��A�H-PA��O�EH�EM
C                               �EPE� MACC�B MAS < I1 �1 .... >
C                                ( CM. �/� FM0007 );
C                  ��EMEHT O��C�BAETC�:
C                  --------------------
C           � ?? T 71[72]  �  K  M
C
C                   MAS < I1 U1 ...... >  - ��� # 71  BO��T_AM�EP. X-KA
C                   MAS < I1 �1 ...... >; - ��� # 72  BE�EP_AM�EP. X-KA
C                   MAS < I1 L���1.... >; - ��� # 720 -> L_��� OT TOKA;
C
C               *** BO�MO�H� �TEPA��OHH�E �TO�HEH�� ***:
C                   �P� BO�����EH�� FITER [ MASPRT(65) ] --->
C                   �HA�EH�� "TOK"_� - �PO�HO� �BH�M METO�OM -
C                   HE B���C���TC�, A B���PA�TC� �� MACC�BA TOKN1;
C               ***
C              === ��� RN -> KOHTPO�� �O �A�EH�� HA�P��EH��;
C                  ��� RN -> KOHTPO�� �O �PE�CKA�AHHOM� � �TO�HEHHOM�
C                            �HA�EH�� TOKA;
C              ===
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
C                 - �P� �TOM KOHTPO��P��ETC� "PACXO��EH�E" ME���
C                   �HA�EH��M�, �O���EHH�M� �O �BHO� CXEME (�PO�HO�) �
C                   O�PE�E�EHH�M� �CXO�� �� B���C�EHH�X �HA�EH�� VALLN1;
C                   KOHTPO�� BE�ETC� �O HE �PEB��EH�� EITER[MASPRT(67)]
C                   OTHOC�TE��HO� �O�PE�HOCT�. B HE ��A�O�P��THOM C���AE
C                   B "MAXITR" �A��C�BAETC� MAX_�O�PE�HOCT� �
C                   B "NLITER" ��KC�P�ETC� HOMEP ��EMEHTA.....
C                   �APAMETP "KOD" = 1;
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
C   METO�:         ��CKPET��A��� �PABHEH�� COCTO�H�� MO�E�� �AHHO�O
C   -----          T��A HA OCHOBE METO�A ��H���EPA B�O�����:
C                  Y(N+1) = Y(N) + H1*F(N+1) + H2*F(N).
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
C   ------------      DEPOST, SEVAL0
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 71  �  # 72, # 720:
C                ------------------------------------------------
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
C                  RMSW T��A # 71, #72  �  #720 [ HE��HE�H�E R � L ]:
C                  --------------------------------------------------
C                                                        LM=<�EPEMEHHA�>
C                                                          =+1<VAR>
C                                                  (OT KO�-AB TO�EK BAX)
C                    SPM
C+--+---+-----+----+-----------------------+
CI  I   I  G  I  J I  XY_MAS               I
CI KI M I     I    I MACC�B TO�EK BAX.     I    MAC�B� "B", "C", "D" -->
CI  I   I     I    I N,(X,Y).."B","C","D"  I    C��A�H PA��O�EH�E BAX:
C+--+---+-----+----+-----------------------+
CI9 I 10I  11 I 12 I 13 ...12+(5*N+1)      I
C+--+---+-----+----+-----------------------+
C
C   N -> KO���ECTBO TO�EK BAX.
C
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
C-->    HO�� �PO��BO�HO�.......
         DATA EK00/ 1E-9/
C**********************************************************************
C-->  �KA�ATE�� HA TOK
       ITOK = MSW(7)
C-->  ���� �O�K���EH��..
       K = MSW(9)
       M = MSW(10)
       TIP = MSW(2)
C-->
       IF ( TIP .EQ. 71  .OR. TIP .EQ. 72  .OR. TIP .EQ.720) GO TO 9900
       GO TO 9999
9900   CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C--->  B��OP PE��MA �A�POCA..
       GO TO( 100, 200), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C-->  �TO �A MO�E��..?
       IF ( TIP .EQ. 72 ) GO TO 150
       IF ( TIP .EQ. 720 ) GO TO 160
C--- HE��H. PE��CTOP, ZAPROS = 1 ++++++++++++++++++++++++++++++++++++++
C-->  OKPECHOCT� O�PE�E�EH�� TOK_A
       TOK = TOKN(ITOK)
C-->  HO MO�ET ��T� �TO �TEP. �TO�..
       IF ( FITER .EQ. 1 ) TOK = TOKN1( ITOK )
C-->  �HA�EHE �O BAX..
       KODFUN = 1
       FUN = SEVAL0( KODFUN, TOK, RMSW(13) )
       KODFUN = 2
       DFUN = SEVAL0( KODFUN, TOK, RMSW(13) )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       IF ( DFUN .EQ. 0.0 ) WRITE(NWTRE,101) MSW(1), TIP, TN, TOK
       IF ( DFUN .EQ. 0.0 ) DFUN = EK00
101    FORMAT(/' *** ��EMEHT # ', I5, ' T�� # ', I3, ' - 0-�',
     = ' �PO��BO�HA� ***'/
     = ' *** BPEM� ', G12.5, ' �HA�EH�E TOKA ', G12.5, ' �O��TKA',
     = ' KOPPEK��� ***'/)
102    FORMAT(/' *** ��EMEHT # ', I5, ' T�� # ', I3, ' - 0-�',
     = ' ���. �H��KT�BHOCT� ***'/
     = ' *** BPEM� ', G12.5, ' �HA�EH�E TOKA ', G12.5, ' �O��TKA',
     = ' KOPPEK��� ***'/)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C--->  G-J ..
       G = 1.0 / DFUN
       JNEL = TOK - FUN * G
C-->  ��KC�POBAT�
       RMSW(11) = G
       RMSW(12) = JNEL
C-->  B�O��T�
       CALL DEPOST( K, M, G, JNEL, YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
150    CONTINUE
C-->  "LN", ZAPROS=1
C-->  "CTAP��" TOK..
       TOK = TOKN(ITOK)
       IF ( FITER .EQ. 1 ) GO TO 151
C-->  O�PE�. �AP-P� B "N" TO�KE..
       KODFUN = 2
       DFUN = SEVAL0( KODFUN, TOK, RMSW(13) )
       IF ( DFUN .EQ. 0.0 ) WRITE(NWTR,101) MSW(1), TIP, TN, TOK
       IF ( DFUN .EQ. 0.0 ) DFUN = EK00
C-->  CTAPOE �A�EH�E...
       VK = 0.0
       VM = 0.0
       IF ( K .NE. 0 ) VK = VALLN(K)
       IF ( M .NE. 0 ) VM = VALLN(M)
       UL = VK - VM
C-->  �CTO�H�K �O CTAPOM� TOK�..JNEL...
       JNEL = TOK
       IF ( H2 .NE. 0.0 ) JNEL = JNEL + H2*UL/DFUN
       RMSW(12) = JNEL
C--> YTOK --> �PO�HO�..
       YTOK = TOK + H0 * UL/ DFUN
       TOKN1(ITOK) = YTOK
C-->  C��TAT� G � B�O��T�...
151    CONTINUE
       YTOK = TOKN1( ITOK )
       JNEL = RMSW(12)
C-->  �O YTOK   HA��EM G..
       KODFUN = 2
       DFUNX = SEVAL0(KODFUN, YTOK, RMSW(13) )
       IF ( DFUNX .EQ. 0.0 ) WRITE(NWTRE, 101) MSW(1), TIP, TN, YTOK
       IF ( DFUNX .EQ. 0.0 ) DFUNX = EK00
       G = H1 / DFUNX
       RMSW(11) = G
C-- T��A E�O...
       CALL DEPOST(K, M, G, JNEL, YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
160    CONTINUE
C-->  "LN", ZAPROS=1,  L_����EP.
C-->  "CTAP��" TOK..
       TOK = TOKN(ITOK)
       IF ( FITER .EQ. 1 ) GO TO 161
C-->  O�PE�. �AP-P� B "N" TO�KE..
       KODFUN = 1
       DFUN = SEVAL0( KODFUN, TOK, RMSW(13) )
       IF ( DFUN .EQ. 0.0 ) WRITE(NWTR,102) MSW(1), TIP, TN, TOK
       IF ( DFUN .EQ. 0.0 ) DFUN = EK00
C-->  CTAPOE �A�EH�E...
       VK = 0.0
       VM = 0.0
       IF ( K .NE. 0 ) VK = VALLN(K)
       IF ( M .NE. 0 ) VM = VALLN(M)
       UL = VK - VM
C-->  �CTO�H�K �O CTAPOM� TOK�..JNEL...
       JNEL = TOK
       IF ( H2 .NE. 0.0 ) JNEL = JNEL + H2*UL/DFUN
       RMSW(12) = JNEL
C--> YTOK --> �PO�HO�..
       YTOK = TOK + H0 * UL/ DFUN
       TOKN1(ITOK) = YTOK
C-->  C��TAT� G � B�O��T�...
161    CONTINUE
       YTOK = TOKN1( ITOK )
       JNEL = RMSW(12)
C-->  �O YTOK   HA��EM G..
       KODFUN = 1
       DFUNX = SEVAL0(KODFUN, YTOK, RMSW(13) )
       IF ( DFUNX .EQ. 0.0 ) WRITE(NWTRE, 102) MSW(1), TIP, TN, YTOK
       IF ( DFUNX .EQ. 0.0 ) DFUNX = EK00
       G = H1 / DFUNX
       RMSW(11) = G
C-- T��A E�O...
       CALL DEPOST(K, M, G, JNEL, YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
C######################################################################
C######################################################################
200    CONTINUE
C--> ZAPROS = 2
C-->  HOBOE �A�EH�E..
       VK = 0.0
       VM = 0.0
       IF ( K .NE. 0 ) VK = VALLN1(K)
       IF ( M .NE. 0 ) VM = VALLN1(M)
       UL = VK - VM
C-->  TOK..
       TOKX = RMSW(11) * UL  +  RMSW(12)
C--> MO�E�� ???
       IF ( TIP  .EQ. 72  .OR. TIP .EQ. 720    ) GO TO 250
C--->  "RN" ....
C-->  "HOB��" TOK
       TOKN1(ITOK) = TOKX
C-->  "BOCTAHOB�M" P��..
       KODFUN = 1
       URX = SEVAL0( KODFUN, TOKX, RMSW(13) )
C-->  O�EH�M �O�PE�HOCT�..
       X1 = ABS( UL )
       X2 = ABS( URX )
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 201
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1-X2) / XZ
       GO TO 202
201    EPS = ABS( X1 - X2 )
202    CONTINUE
C-->  �O��CT�MO �TO..?
       KOD = 0
       IF ( EPS .LT. EITER ) RETURN
C                            *******
C-->  O���KA..
       KOD = 1
       IF ( EPS .LT. MAXITR ) RETURN
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
       RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
250    CONTINUE
C==> ZAPROS = ",  "LN"...      ���  L_����.  TO �E CAMOE.....
C-->  �PE�CKA�AHH�� TOK ..
       X1 = ABS( TOKN1( ITOK ) )
C-->  �O���EHH�� TOK..
       X2 = ABS( TOKX )
C-->  �A�OMH�M �O���EHH��..
       TOKN1(ITOK) = TOKX
C-->  �O�PE��M...
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 251
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 252
251    EPS = ABS( X1 - X2 )
252    CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) RETURN
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) RETURN
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
       RETURN
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0012 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
