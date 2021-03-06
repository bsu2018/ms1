         SUBROUTINE DM0028 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT �EKA�P� 1987 �.)------------
C   �O��PO�PAMMA:  DM0028 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C    *                         MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTOB �E��:
C                +-----+--------------------------------------------+
C                I T�� I              �M� MO�E��                    I
C                I-----I--------------------------------------------I
C                I 15  I   ��PAB��EM�� HA�P��EH�EM �CTO�H�K TOKA    I
C                I-----I--------------------------------------------I
C                I 16  I   ��PAB��EM�� HA�P��EH�EM �CTO�H�K HA�P�-  I
C                I     I   �EH��                                    I
C                I-----I--------------------------------------------I
C                I 200 I   ��EA��H�� O�EPA��OHH�� �C���TE��         I
C                I-----I--------------------------------------------I
C                  ***************************************************
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
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
C                          �P�  T  =  TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                           �P�  T  =  TN  + H0;
C                  TOKN  �  TOKN1 - BEKTOPA �EPEMEHH�X "COCTO�H��"
C                                   ��EMEHTA �P�  TN  �  TN + H0;
C                                   PA�MEPHOCT� CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 5;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C                   ( �C�O����ETC� ��� MAKPOMO�E�E� "��EA��H�X" K���E�)
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------             DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1  ��� 2 - COO��EH�E � �XO�;
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 15,16,200
C                --------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I  �P��. I KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I  T��A  I �ECT I B�B. I�HA�.  I�E I
C  I HTA   I MO�E�� I PAC�ETAI  MO�E��I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I        I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-----------+
C
C
C
C                  RMSW T�� # 15 [ �T�H ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   �1   I   �2    I    G    II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  11    I   12    I    13   II
C             ++--------+---------+---------++
C
C
C                  RMSW T�� # 16 [ �H�H ]
C                  ----------------------
C
C      ++--------+---------+---------+---------+----------+
C      II        I         I         I         I          I
C      II   �1   I   �2    I   GBH   I    K    I G=GBH*K  I
C      II        I         I         I         I          I
C      ++--------+---------+---------+---------+----------+    LM = 5
C      II   1    I    2    I     3   I    4    I    5     I    ******
C      ++--------+---------+---------+---------+----------+
C      II  11    I   12    I    13   I   14    I   15     I
C      ++--------+---------+---------+---------+----------+
C
C
C                  RMSW T�� # 200 [ ��EA��H�� O�EPA��OHH�� �C���TE�� ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   K    I   GBX   I  GB�X   II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  13    I   14    I    15   II
C             ++--------+---------+---------++
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FKOM,   MASPRT(34) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADRMSW
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS, TIP
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
C===>    BCTPOEHHA� ��HK��� O�C����BAH�� �B�XMEPH�X MACC�BOB:
         ID(I,J) = NDALL * (J-1)  +  I
C--->  KOHTPO�� T��A
         TIP = MSW(2)
         IF ( TIP .NE. 15 .AND. TIP .NE. 16 .AND. TIP.NE.200)GO TO 9999
C---> B��OP PE��MA:
         GO TO ( 667, 666                 ), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C---------ZAPROS = 1---------------------------------------------------
C---> �TO �A T��?
         IF ( TIP .EQ. 16 ) GO TO 20
         IF ( TIP .EQ. 200) GO TO 30
C######################################################################
C---> ��PAB��EM�� HA�P��EH�EM �CTO�H�K TOKA
C--->   �O��EMEHTH�� BK�A�:
C==> CBO�  ����.
         K = MSW(9)
         L = MSW(10)
C==> ���� ��PAB����E�O HA�P��EH��
         M = MSW(11)
         N = MSW(12)
C===> KO������EHT CB���
         G = RMSW(13)
C======================================================================
         KOD = 0
C---->
         CALL DEPOSN( K, L, M, N, G, YALL )
C---->
         RETURN
C######################################################################
20      CONTINUE
C---> ��PAB��EM�� HA�P��EH�EM �CTO�H�K HA�P��EH��
C--->   �O��EMEHTH�� BK�A�:
C==> CBO�  ����.
         K = MSW(9)
         L = MSW(10)
C==> ���� ��PAB����E�O HA�P��EH��
         M = MSW(11)
         N = MSW(12)
C===> BH�TPEHHEE CO�POT�B�EH�E � KO������EHT CB���
         GBH = RMSW(13)
         G   = RMSW(15)
C======================================================================
C-->   BK�A��BAEM...
         KOD = 0
C---->
         CALL DEPOSN( K, L, M, N, G, YALL )
C---->
         CALL DEPOST( K, L, GBH, 0.0, YALL, JALL )
C
         RETURN
C######################################################################
30      CONTINUE
C---> ��EA��H�� O�EPA��OHH�� �C���TE��
C--->   �O��EMEHTH�� BK�A�:
C==> BXO�H�E  ����.
         M = MSW(9)
         N = MSW(10)
C==> B�XO�H�E  ����.
         K = MSW(11)
         L = MSW(12)
C===> KO������EHT �C��EH��, BXO�HA� � B�XO�HA� �POBO��MOCT�.
         AK  = RMSW(13)
         GBX = RMSW(14)
         GBIX= RMSW(15)
C======================================================================
         KOD = 0
C-->  B�O��M BXO�H�� �E��
         CALL DEPOST( M, N, GBX, 0.0, YALL, JALL )
C---> B�O��M B�XO�H�� �E��
         G = AK * GBIX
         CALL DEPOSN( K, L, M, N, G, YALL )
C---->
         CALL DEPOST( K, L, GBIX, 0.0, YALL, JALL )
         RETURN
C======================================================================
666      CONTINUE
C$$$$$$$$$$$$$$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---> �TO �A T��?
         IF ( TIP .EQ. 16 ) GO TO 40
         IF ( TIP .EQ. 200) GO TO 50
C######################################################################
C---> ��PAB��EM�� HA�P��EH�EM �CTO�H�K TOKA
C==> ���� ��PAB����E�O HA�P��EH��
         M = MSW(11)
         N = MSW(12)
C--->  HOB�� TOK:
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         UN1 = VM - VN
         TOKT = RMSW(13) * UN1
C--->     �A��C� TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C######################################################################
40       CONTINUE
C---> ��PAB��EM�� HA�P��EH�EM �CTO�H�K HA�P��EH��
C==> ���� ��PAB����E�O HA�P��EH��
         M = MSW(11)
         N = MSW(12)
C--->CBO� ����.
         K = MSW(9)
         L = MSW(10)
C--->  HOBOE ��PAB����EE HA�P��EH�E
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         UBX = VM - VN
C--->  HOBOE B�XO�HOE HA�P��EH�E
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VL = 0.0
         IF ( L .NE. 0 ) VL = VALLN1(L)
         UBIX = VK - VL
C---> HOB�� TOK
         TOKT = ( RMSW(14) * UBX + UBIX ) * RMSW(13)
C---> �A��C� TOKA
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C######################################################################
50       CONTINUE
C---> ��EA��H�� O�EPA��OHH�� �C���TE��
C==> ���� BXO�HO�O HA�P��EH��
         M = MSW(9)
         N = MSW(10)
C--->���� B�XO�HO�O HA�P��EH��
         K = MSW(11)
         L = MSW(12)
C--->  HOBOE BXO�HOE HA�P��EH�E
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         UBX = VM - VN
C--->  HOBOE B�XO�HOE HA�P��EH�E
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VL = 0.0
         IF ( L .NE. 0 ) VL = VALLN1(L)
         UBIX = VK - VL
C---> HOB�� TOK
         TOKBX = UBX * RMSW(14)
         TOKBIX= ( RMSW(13) * UBX + UBIX ) * RMSW(15)
C---> �A��C� TOKA
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKBX
         TOKN1( ITOK + 1 ) = TOKBIX
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0028 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
