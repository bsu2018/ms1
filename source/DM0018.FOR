         SUBROUTINE DM0018 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0018 ( MSW, RMSW, YALL, JALL, LOGYTR,
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
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  64      3-X �A�HA� �.�.C        E-PE�(1)
C                          PE����P�EMA� OT "PB"
C
C
C                  65      3-X �A�HA� �.�.C.,
C                          ��PAB��EMA� BHE�H�M     E-PE�(2)
C                          C��HA�OM .
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 64 � K0 KA KB KC � T-BK�  R-BH�T #-��-TA  K-�C��;
C
C              � # T 65 � K0 KA KB KC � R-BH�T L-BH�T K-�C��  W  FI
C                                       #-��-TA  #-CME�.
C
C     K0 - O���� ��E�, KA - �A�A A, KB - �A�A B, KC - �A�A C;
C
C---> B�BO�H�E �HA�EH��: TOK�: K ���AM KA, KB, KC, OT ���A K0,
C                              *                   **
C                      �HA�EH�� �.�.C. E1, E2, E3;
C
C
C !!!!   �A�EP�KA HA O��H �A� �HTE�P�POBAH�� !!!
C        -----------------------------------
C
C
C
C                  ***************************************************
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
C
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
C               "�OCTO�HHA�" �ACT� MSW T��A # 64 � #65
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
C                  RMSW T��A # 64 [ �CTO�H�K HA�P��EH�� OT "PE�.BO��" ]
C                  -----------------------------------------
C
C                    TOK(J) = K_�C. * E(J) / R_BH�T. + 1/R * U_N+1
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II �HA�.  I �HA�.I # ��-TAI A�P. II      IJESIJESI K   I
C   ����:     II  1/R   I T_BK�I "PB"   I  B   II JESA I   I   I �C��I
C  K0         II        I      I        I TOKN II      I B I C I EH��I
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
C                         RMSW T��A # 65
C                         --------------
C
C                             SPM                 SRAB
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C            II #   I #   IA�P.I   I   I K I   I   I   I    I    I    I
C  ����:     II��-TAICME�.I B  I R I L I�C�I W I FII G IJESAIJESBIJESCI
C K0         II     I     ITOKNI   I   I�EHI   I   I   I    I    I    I
C   K1       ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C     K2     II  1  I  2  I  3 I 4 I 5 I 6 I 7 I 8 I 9 I 10 I 11 I 12 I
C        K3  ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C 9 10 11 12 II 13  I 14  I 15 I16 I17 I18 I19 I20 I21 I 22 I 23 I 24 I
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
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
         INTEGER ADRMSW, TIP
         INTEGER FKOM, ZAPROS
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
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
C
         DATA R120/ 2.0944/, SQ3D2/ 0.8660254 /, PI2/6.283185306/
         DATA RGRAD / 57.29577 /
C
         KOD = 0
       TIP = MSW(2)
       IF ( TIP .EQ. 64  .OR.  TIP .EQ. 65  ) GO TO 10
C----------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0018 - �A�POC HA O�C����BAH�E ���O�
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
C--->   �O��EMEHTH�� BK�A�:    -->>
C==> "HOBOE" BPEM�   ( �A, ��.... )
         TN1 = TN + H0
C-->  ����*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
       IF( TIP .EQ. 65 ) GO TO 100
C----------------------------------------------------------------------
C      �CTO�H�K 3-X �A�HO� �.�.C. (T�� 64)
C----------------------------------------------------------------------
         GES = RMSW(13)
         JESA = 0.0
         JESB = 0.0
         JESC = 0.0
C-->  PA�OTAEM ?
         TBKL = RMSW(14)
         IF ( TN .LT. TBKL ) GO TO 50
C-->  BO�MEM AM���T���..
         ITRB = MSW(16)
         RKUS = RMSW(20) * GES
         JESA = TOKN( ITRB + 0 ) * RKUS
         JESB = TOKN( ITRB + 1 ) * RKUS
         JESC = TOKN( ITRB + 2 ) * RKUS
50       CONTINUE
C-->  �A�OMH�MC�
         RMSW(17) = JESA
         RMSW(18) = JESB
         RMSW(19) = JESC
C======================================================================
C----> BK�A��BAEM
         CALL DEPOST( K0, KA, GES, JESA, YALL, JALL )
         CALL DEPOST( K0, KB, GES, JESB, YALL, JALL )
         CALL DEPOST( K0, KC, GES, JESC, YALL, JALL )
C---->
         RETURN
C######################################################################
100    CONTINUE
C----------------------------------------------------------------------
C      �CTO�H�K 3-X �A�HO� �.�.C. (T�� 65)
C----------------------------------------------------------------------
CC     Z = RMSW(17) + H0 * RMSW(16)
       Z =                 RMSW(16)
       GES =  1.0 / Z
C--> C��TAEM AM���T���.
       IUPR = MSW(15)
       EM = TOKN(IUPR) * RMSW(18)
C--> HAXO��M �CTO�H�K�.
       W = RMSW(19)
       FI= RMSW(20)
       EA1 = EM * SIN( W * TN1 + FI )
       EB1 = EM * SIN( W * TN1 + FI - 2.0944 )
       EC1 = EM * SIN( W * TN1 + FI + 2.0944 )
C---> CTAP�E TOK�.
       ITOK = MSW(7)
       TOKA = TOKN(ITOK)
       TOKB = TOKN(ITOK+1)
       TOKC = TOKN(ITOK+2)
C--> �CTO�H�K� TOKA
       JESA =(      EA1                   ) / Z
       JESB =(      EB1                   ) / Z
       JESC =(      EC1                   ) / Z
C-->  �A�OMH�MC�
         RMSW(21) = GES
         RMSW(22) = JESA
         RMSW(23) = JESB
         RMSW(24) = JESC
C======================================================================
C----> BK�A��BAEM
         CALL DEPOST( K0, KA, GES, JESA, YALL, JALL )
         CALL DEPOST( K0, KB, GES, JESB, YALL, JALL )
         CALL DEPOST( K0, KC, GES, JESC, YALL, JALL )
C---->
         RETURN
C======================================================================
C$$$$$$$$$$$$$$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C-->  ����*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
       IF( TIP .EQ. 65 ) GO TO 200
C----------------------------------------------------------------------
C      �CTO�H�K 3-X �A�HO� �.�.C. (T�� 64)
C----------------------------------------------------------------------
C---->
       GES = RMSW(13)
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
         TOK1 = GES      * (V0-VA)  + JESA
         TOK2 = GES      * (V0-VB)  + JESB
         TOK3 = GES      * (V0-VC)  + JESC
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
C--> �A��C� �.�.C.  ��PAB�EH��
         ITRB = MSW(16)
         RKUS1 = RMSW(20)
         TOKN1(ITOK+ 4 ) = TOKN( ITRB + 0 ) * RKUS1
         TOKN1(ITOK+ 5 ) = TOKN( ITRB + 1 ) * RKUS1
         TOKN1(ITOK+ 6 ) = TOKN( ITRB + 2 ) * RKUS1
C
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
200    CONTINUE
C----------------------------------------------------------------------
C      �CTO�H�K 3-X �A�HO� �.�.C. (T�� 64)
C----------------------------------------------------------------------
       GES = RMSW(21)
       JESA = RMSW(22)
       JESB = RMSW(23)
       JESC = RMSW(24)
C--->  HOBOE HA�P��EH�E.
         V0 = 0.0
         IF ( K0 .NE. 0 ) V0 = VALLN1(K0)
         VA = 0.0
         IF ( KA .NE. 0 ) VA = VALLN1(KA)
         VB = 0.0
         IF ( KB .NE. 0 ) VB = VALLN1(KB)
         VC = 0.0
         IF ( KC .NE. 0 ) VC = VALLN1(KC)
C-->   HOB�� TOK.
         TOK1 = GES * (V0-VA)  + JESA
         TOK2 = GES * (V0-VB)  + JESB
         TOK3 = GES * (V0-VC)  + JESC
C-->
         TOK0 = TOK1 + TOK2 + TOK3
C-->
C--->     �A��C� TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOK1
         TOKN1( ITOK + 1 ) = TOK2
         TOKN1( ITOK + 2 ) = TOK3
         TOKN1( ITOK + 3 ) = TOK0
       RETURN
C----------------------------------------------------------------------
        END
