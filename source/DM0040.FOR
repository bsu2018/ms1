         SUBROUTINE DM0040 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT ������  1990 �.)------------
C   �O��PO�PAMMA:  DM0040 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C    *                         MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTA �E��
C
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  66            �.�.C             ESG
C                              �������������
C                              C�H�CO��A��H��
C
C
C                  67          �������������       JSC
C                              C�H�CO��A��H��
C                              ���. ����
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 66 � K0 K1 �    R    L
C                                   MAS < NGARM1  E1  FI1
C                                         NGARM2  E2  FI2
C                                         .....   ..  ...  >  ;
C
C
C
C
C              � # T 67 � K0 K1 �  MAS < NGARM1  J1  FI1
C                                        NGARM2  J2  FI2
C                                        .....   ..  ...  >  ;
C
C
C
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
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
C
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
C               "�OCTO�HHA�" �ACT� MSW T��A # 66 � #67
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
C                  RMSW T��A # 66 [ �CTO�H�K SIN HA�P��EH�� ]
C                  -----------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II     I     I ���-�� I    I    I
C   ����:     II  R  I  L  I ������.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
C
C
C                  RMSW T��A # 67 [ �CTO�H�K SIN TOKA ]
C                  ------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II  ��-I  ��-I ���-�� I    I    I
C   ����:     II ����I ����I ������.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JES
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW, TIP, ZAPROS
C
C==> "HOBOE" BPEM�   ( �A, ��.... )
         TN1 = TN + H0
C---->   ���� �O�K���EH��:
         K0 = MSW(9)
         K1 = MSW(10)
         KOD = 0
C--->  KOHTPO�� T��A
         TIP = MSW(2)
         IF ( TIP .EQ. 66  .OR.  TIP .EQ. 67 ) GO TO 10
C----------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0040 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
10     CONTINUE
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
C
C
         IF( TIP .EQ. 67 ) GO TO 67
C---> �CTO�H�K SIN HA�P��EH��
C--->   �O��EMEHTH�� BK�A�:
         ITOK= MSW(7)
         TOK = TOKN(ITOK)
C----------------------------------------------------------------------
         VK0 = 0.0
         IF ( K0 .NE. 0 ) VK0 = VALLN(K0)
         VK1 = 0.0
         IF ( K1 .NE. 0 ) VK1 = VALLN(K1)
         UN = VK0 - VK1
C----------------------------------------------------------------------
C--> ��������� ���
         KGARM = MSW(13)
         EN = 0.0
         EN1= 0.0
         DO 2000 I=1,KGARM
         NG  =  MSW(13+3*I)
         EG  = RMSW(14+3*I)
         FIG = RMSW(15+3*I)
         EN  = EN  + EG * SIN( NG * 314.16 * TN  + FIG )
         EN1 = EN1 + EG * SIN( NG * 314.16 * TN1 + FIG )
2000     CONTINUE
C----------------------------------------------------------------------
         Z = RMSW(12) + H1 * RMSW(11)
         GES = H1 / Z
         JES =( TOK*(RMSW(12)-H2*RMSW(11)) + H1*EN1 + H2*EN + H2*UN )/Z
         RMSW(14) = GES
         RMSW(15) = JES
C---->
C======================================================================
         KOD = 0
C---->
         CALL DEPOST( K0, K1, GES, JES, YALL, JALL )
C---->
         RETURN
C======================================================================
67       CONTINUE
C---> �CTO�H�K SIN ����
C--->   �O��EMEHTH�� BK�A�:
C--> ��������� ��� ���������
         KGARM = MSW(13)
         TOKJN1= 0.0
         DO 3000 I=1,KGARM
         NG  =  MSW(13+3*I)
         AG  = RMSW(14+3*I)
         FIG = RMSW(15+3*I)
         TOKJN1 = TOKJN1 + AG * SIN( NG * 314.16 * TN1 + FIG )
3000     CONTINUE
C----------------------------------------------------------------------
         JES =  TOKJN1
         RMSW(15) = JES
C---->
C======================================================================
         KOD = 0
C---->
         CALL DEPOST( K0, K1, 0.0, JES, YALL, JALL )
C---->
         RETURN
C======================================================================
666      CONTINUE
C################   ZAPROS = 2   ######################################
C---> �TO �A T��?
         IF ( TIP .EQ. 67 ) GO TO 167
C######################################################################
C---> �CTO�H�K SIN HA�P��EH��.
C--->     HOBE��EE BPEM�...        TN + H0 => ��� HE�O JES O�PE�E�EHO!!!
         GES = RMSW(14)
         JES = RMSW(15)
C--->  HOBOE HA�P��EH�E:
         VK0 = 0.0
         IF ( K0 .NE. 0 ) VK0 = VALLN1(K0)
         VK1 = 0.0
         IF ( K1 .NE. 0 ) VK1 = VALLN1(K1)
         UN1 = VK0 - VK1
C--->  HOB�� TOK:
         TOKT = GES * UN1 + JES
C--->  �A��C� TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C######################################################################
167      CONTINUE
C---> �CTO�H�K SIN ����
         JES = RMSW(15)
C--->  �A��C� TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = JES
         KOD = 0
         RETURN
C######################################################################
         END
