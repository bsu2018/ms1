         SUBROUTINE DM0016 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0016 ( MSW, RMSW, YALL, JALL, LOGYTR,
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
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTA �E��
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  63      3-X �A�HA� �.�.C        ES3
C                              C�H�CO��A��HO�
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 63 � K0 KA KB KC � R EMAX OMEGA FAZ0;
C
C     K0 - O���� ��E�, KA - �A�A A, KB - �A�A B, KC - �A�A C;
C
C---> B�BO�H�E �HA�EH��: TOK�: K ���AM KA, KB, KC, OT ���A K0;
C                              *                   **
C
C
C                  13      "��MEP�TE��"
C                           HA�P��EH��             V3
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 13 � K1 K2 K3;
C
C   B C��COK B��EC�EHH�X �HA�EH�� �OME�AET HA�P��EH�E ���OB K1, K2, K3,
C   �ATEM -> V_MOD, ��O� OTHOC. �A�� "A", V_X HA "A" �POEK���, �
C            V_Y HA "A"+90�PA�. �POEK���, ���OBA� CKOPOCT� BPA�EH��
C                                         BEKTOPA V_MOD;
C                      !!! �P� V_MOD = 0.0 ---> OMEGA = 314.15927 !!!!
C
C                     �EPEXO� OT 3-X K 2-M C ��ETOM KO��. 2/3     !!!!!
C
C                  ***************************************************
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
C
C              ��� MO�E�� # 13 -->  �EPE�A��C� �� VALLN1 B TOKN1
C              -------------------------------------------------
C              + B���C�EH�E PA��O�EH�� 3-X �A�HO� C�CTEM�, C��TA� -->
C              V(K1) - �A�A A; V(K2) - �A�A B; V(K3) - �A�A C;
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
C               "�OCTO�HHA�" �ACT� MSW T��A # 63 � #13
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
C                RMSW T��A # 63 [ �CTO�H�K SIN HA�P��EH�� - 3-� �A�H��
C                -----------------------------------------------------
C
C
C                    ( U1 = EM * SIN( OMEGA * T  +  FAZ0 )     LM = 8
C                    ( U2 = EM * SIN( OMEGA * T  +  FAZ0 - 120 )
C                    ( U3 = EM * SIN( OMEGA * T  +  FAZ0 + 120 )
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II �HA�.  I �HA�.I        I FAZ0 II      IJESIJESI PE  I
C   ����:     II  1/R   I EM/R I OMEGA  I  P   II JESA I   I   I �E  I
C  K0         II        I      I        I PA�. II      I B I C I PB  I
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JESA, JESB, JESC
         REAL K2D3
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
         INTEGER ADRMSW, TIP
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
C
         DATA R120/ 2.0944/, SQ3D2/ 0.8660254 /, PI2/6.283185306/
         DATA RGRAD / 57.29577 /
         DATA K2D3/0.666667 /
C
       KOD = 0
       TIP = MSW(2)
       IF ( TIP .EQ. 13  .OR.  TIP .EQ. 63 ) GO TO 10
C----------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0016 - �A�POC HA O�C����BAH�E ���O�
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
       IF ( TIP .EQ. 13 ) RETURN
C
C--->   �O��EMEHTH�� BK�A�:    -->>  ���  E_SIN_3
C==> "HOBOE" BPEM�   ( �A, ��.... )
         TN1 = TN + H0
         GES = RMSW(13)
         JESA = RMSW(14) * SIN( RMSW(15) * TN1 + RMSW(16)  )
         JESB = RMSW(14) * SIN( RMSW(15) * TN1 + RMSW(16) - R120 )
         JESC = RMSW(14) * SIN( RMSW(15) * TN1 + RMSW(16) + R120 )
         RMSW(17) = JESA
         RMSW(18) = JESB
         RMSW(19) = JESC
C---->
C======================================================================
C-->  ����*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
C---->
         CALL DEPOST( K0, KA, GES, JESA, YALL, JALL )
         CALL DEPOST( K0, KB, GES, JESB, YALL, JALL )
         CALL DEPOST( K0, KC, GES, JESC, YALL, JALL )
C---->
         RETURN
C======================================================================
C$$$$$$$$$$$$$$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C
       IF ( TIP .EQ. 13 ) GO TO 777
C
C-->  ����*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
C---->
C--->     HOBE��EE BPEM�...        TN + H0 => ��� HE�O JES O�PE�E�EHO!!!
         JESA = RMSW(17)
         JESB = RMSW(18)
         JESC = RMSW(19)
C--->  HOB�� TOK:
         V0 = 0.0
         IF ( K0 .NE. 0 ) V0 = VALLN1(K0)
         VA = 0.0
         IF ( KA .NE. 0 ) VA = VALLN1(KA)
         VB = 0.0
         IF ( KB .NE. 0 ) VB = VALLN1(KB)
         VC = 0.0
         IF ( KC .NE. 0 ) VC = VALLN1(KC)
C-->
         TOK1 = RMSW(13) * (V0-VA)  + JESA
         TOK2 = RMSW(13) * (V0-VB)  + JESB
         TOK3 = RMSW(13) * (V0-VC)  + JESC
C--
       TOK0 = TOK1 + TOK2 + TOK3
C-->
C--->     �A��C� TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOK1
         TOKN1( ITOK + 1 ) = TOK2
         TOKN1( ITOK + 2 ) = TOK3
         TOKN1( ITOK + 3 ) = TOK0
C----------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
777    CONTINUE
       VA = 0.0
       VB = 0.0
       VC = 0.0
C
       KA =  MSW(9)
       KB =  MSW(10)
       KC =  MSW(11)
C
       IF ( KA .NE. 0 ) VA = VALLN1( KA )
       IF ( KB .NE. 0 ) VB = VALLN1( KB )
       IF ( KC .NE. 0 ) VC = VALLN1( KC )
C
       ITOK = MSW(7)
C
       TOKN1( ITOK    ) = VA
       TOKN1( ITOK +1 ) = VB
       TOKN1( ITOK +2 ) = VC
C
C-->  PA��O�EH�E BEKTOPOB HA OC� "A" � "A+90�PA�." --> X, Y;
C
       VX = K2D3 *(  VA - ( VB+VC )/2.    )
       VY = K2D3 *(  SQ3D2 * ( VB - VC )  )
C
       VXYMOD = SQRT( VX**2 + VY**2 )
       GVXY = ATAN2( VY, VX )
       IF ( VY .LT. 0.0 ) GVXY = GVXY + PI2
       IF ( GVXY .GE. PI2 ) GVXY = GVXY - INT( GVXY/PI2 ) * PI2
C-->  B TOKN1
       TOKN1( ITOK + 3 ) =  VXYMOD
       TOKN1( ITOK + 4 ) =  GVXY * RGRAD
       TOKN1( ITOK + 5 ) =  VX
       TOKN1( ITOK + 6 ) =  VY
C-->  B���C�EH�E OMEGA BPA�EH��-->
       VXOLD = TOKN( ITOK + 5 )
       VYOLD = TOKN( ITOK + 6 )
C******VXYOLD = TOKN( ITOK + 3 )
       OMEGA = 314.15927
       XXX = H0 * VXYMOD
       IF ( XXX .EQ. 0.0 ) GO TO 778
       YYY = SQRT( (VX - VYOLD)**2  + (VY - VYOLD)**2 )
       OMEGA = YYY / XXX
778    CONTINUE
       TOKN1( ITOK + 7 ) = OMEGA
C-->
       RETURN
       END
