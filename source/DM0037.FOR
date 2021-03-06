         SUBROUTINE DM0037 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT CEHT��P� 1988 �.)------------
C   �O��PO�PAMMA:  DM0037 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTA �E��
C                  T��A # 32  33  -->  TPAHC�OPMATOP� ;
C                  *********************
C     KO�-BO O�-K  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C     K = 2        32      2-X O�M. TPAHC.       TP2
C     K = 3        33      3-X O�M. TPAHC.       TP3
C
C!!!!!
C    B H � M A H � E :   K  O�PE�E��ETC� �CXO�� �� HOMEPA T��A:
C    ****************
C                          K  = TIP - 30
C
C!!!!
C                  BBO� �O O��CAH��:
C
C           � # T 3@ � @ @ ..  � R1 R2 .. L1 L2 .. W1 W2 .. RCT  LCT ;
C
C          ���� �O�K���EH��: <HA�A�O O�MOTOK> <KOH�� O�MOTOK>
C
C
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
C
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOSA,
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 32, 33
C                ------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE-I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E-I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW T��OB # 32  33:
C                  --------------------
C
C
C  �OCT.  I �EPEMEHHA� RMSW-->
C +-------+--+--------+--------+--------+------+------+----------+...>
C I ����  I KI R �APA-I L �AP- I W �AP- I      I      I W1 MAT-  I...>
C I       I  I   MET- I   AME- I   AME- I R CT I L CT I    P�-   I...>
C I       I  I    P�  I   TP�  I   TP�  I      I      I    �A    I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I       I 1I K C�OB I K C�OB I K C�OB I  1   I  1   I K*(K+1)/2I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I 9-... I  I        I        I        I      I      I          I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I          I        I        I        I             I
C D0         D1      D2       D3       D4            D5
C DR0        DR1     DR2      DR3      DR4           DR5
C
C
C ...>+---------+--------+------+------+------+
C ...>I G MAT-  I J BEK- I      I      I      I
C ...>I   P��A  I   TOP  I JMU  ITOKMUXI  KY  I
C ...>I         I        I      I      I      I
C ...>+---------+--------+------+------+------+
C ...>IK*(K+1)/2I K C�OB I   1  I   1  I   1  I
C ...>+---------+--------+------+------+------+
C ...>I         I        I      I      I      I
C ...>+---------+--------+------+------+------+
C     I         I        I
C     D6       D7       D8
C     DR6      DR7      DR8
C
C
C
C
C
C
C
C
C
C
C               C M E � E H � � :
C               -----------------
C             T�� #32       T�� # 33
C   -----------------------------------
C   K--->       2              3
C   -----------------------------------
C   LM-->      32             44
C   -----------------------------------
C   D0-->       9             9
C   -----------------------------------
C   D1-->      14             16
C   -----------------------------------
C   D2-->      16             19
C   -----------------------------------
C   D3-->      18             22
C   -----------------------------------
C   D4-->      20             25
C   -----------------------------------
C   D5-->      22             27
C   -----------------------------------
C   D6-->      25             33
C   -----------------------------------
C   D7-->      28             39
C   -----------------------------------
C   D8-->      30             42
C   -----------------------------------
C   LMR->      20             30
C--------------------------------------
C
C   ��� �EPEMEHHO� �ACT� DR<*> = D<*> - D1 + 2
C                        $$$$$$$$$$$$$$$$$$$$$
C
C@@
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION STR(3), TTN(3), ULN(3), Z(9), Z1(9), G0(9)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
          DIMENSION MD0(2), MD1(2), MD2(2), MD3(2), MD4(2)
          DIMENSION MD5(2), MD6(2), MD7(2), MD8(2)
         INTEGER D0, D1, D2, D3, D4, D5, D6, D7, D8
         INTEGER ADRMSW
         INTEGER TIP,  ZAPROS
         REAL JALL, JMU, LCT
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         DATA MD0 /  9,   9  /
         DATA MD1 / 14,  16  /
         DATA MD2 / 16,  19  /
         DATA MD3 / 18,  22  /
         DATA MD4 / 20,  25  /
         DATA MD5 / 22,  27  /
         DATA MD6 / 25,  33  /
         DATA MD7 / 28,  39  /
         DATA MD8 / 30,  42  /
C######################################################################
C  BCTPOEHHA� ��HK���  NIJ(I,J) O�PE�E��ET HOMEP ��EMEHTA B BEKTOPE ,
C  B KOTOPOM XPAH�TC� BEPXHETPE��O��HA� �ACT� C�MMETP��HO� KBA�PATHO�
C  MATP��� (�O CTPOKAM) PA�MEPHOCT��  K .
C
       NIJ(I,J) = K*(I-1) - I*(I-1)/2 + J
C      **********************************
C######################################################################
C  BCTPOEHHA� ��HK���  TDK(I,J) O�PE�E��ET HOMEP ��EMEHTA B BEKTOPE ,
C  B KOTOPOM XPAH�TC� KBA�PATHA� MATP��A (�O CTO���AM) PA�MEPHOCT�� K.
C
       IDK(I,J) = K*(J-1) + I
C      **********************
C######################################################################
C--->  KOHTPO�� T��A
       TIP = MSW(2)
       IF ( TIP .EQ. 32  .OR. TIP .EQ. 33  ) GO TO 9999
C----------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT(/' *** �O�. C�O� B DM0037 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
9999     CONTINUE
C######################################################################
       K = TIP - 30
       K1 = K / 3 + 1
       K2 = K * (K+1) / 2
C-->  CME�EH��
       D0 = MD0(K1)
       D1 = MD1(K1)
       D2 = MD2(K1)
       D3 = MD3(K1)
       D4 = MD4(K1)
       D5 = MD5(K1)
       D6 = MD6(K1)
       D7 = MD7(K1)
       D8 = MD8(K1)
C--> �APAMETP�
       RCT = RMSW(D4)
       LCT = RMSW(D4+1)
C######################################################################
C---> B��OP PE��MA:
         GO TO ( 667, 666                 ), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C--------------------ZAPROS=1------------------------------------------
C--->  B���C�EH�E G - J �APAMETPOB � �X �A��C� A MSW:
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  HAXO��M CTAP�E TOK�.
C----------------------------------------------------------------------
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
       DO 10 I=1,K
       TTN(I) = TOKN(ITOK1+I)
10     CONTINUE
       TOKMU = TOKN(ITOK+K)
C----------------------------------------------------------------------
C-->  HAXO��M CTAP�E HA�P��EH��.
C----------------------------------------------------------------------
       DO 20 I=1,K
       IMSW = D0 + I - 1
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       ULN(I) = VN - VK
20     CONTINUE
C----------------------------------------------------------------------
C--> KOHCTAHT�  ( T1 = H1 + LCT/RCT  )
C----------------------------------------------------------------------
       T1 = H1 + LCT / RCT
C----------------------------------------------------------------------
C--> O�PE�E��M MATP��� "Z" ( Z = L + H1*R + H1*LCT*W1/T1 )
C----------------------------------------------------------------------
       A = H1 * LCT / T1
       DO 30 I=1,K2
       Z(I) = A * RMSW(D5+I-1)
30     CONTINUE
       DO 40 I=1,K
       I1 = I-1
       IZ = NIJ(I,I)
       Z(IZ) = Z(IZ) + RMSW(D2+I1) + H1*RMSW(D1+I1)
40     CONTINUE
C----------------------------------------------------------------------
C--> O�PE�E��M  "JMU" ( JMU = [ (LCT/RCT - H2)*IMUN + H2*WT*I ] / T1 )
C----------------------------------------------------------------------
       JMU = TOKMU * ( LCT / RCT - H2 )
       A = 0
       DO 50 I=1,K
       A = A + TTN(I) * RMSW(D3+I-1)
50     CONTINUE
       JMU = ( JMU + H2 *A ) / T1
C--> �A�OMH�M "JMU"
       RMSW(D8) = JMU
C----------------------------------------------------------------------
C--> O�PA�AEM MATP��� "Z"
C----------------------------------------------------------------------
       IF( K .EQ. 2 ) CALL MMTS2( Z , Z1 )
       IF( K .EQ. 3 ) CALL MMTS3( Z , Z1 )
C----------------------------------------------------------------------
C--> O�PE�E��M BEKTOP "J"
C  J = Z1 * [ H2*UN + (L-H2*R-H2*RCT*W1)*IN + (H1*JMU + H2*IWUN)*RCT*W
C----------------------------------------------------------------------]
       A = -H2 * RCT
       DO 60 I=1,K2
       Z(I) = A * RMSW(D5+I-1)
60     CONTINUE
       DO 70 I=1,K
       I1 = I-1
       IZ = NIJ(I,I)
       Z(IZ) = Z(IZ) + RMSW(D2+I1) - H2 * RMSW(D1+I1)
70     CONTINUE
       IF( K .EQ. 2 ) CALL PMTS2( Z , TTN , STR )
       IF( K .EQ. 3 ) CALL PMTS3( Z , TTN , STR )
       A = RCT * ( H1*JMU + H2*TOKMU )
       DO 80 I=1,K
       TTN(I) = STR(I) + H2 * ULN(I) + A * RMSW(D3+I-1)
80     CONTINUE
       IF( K .EQ. 2 ) CALL PMTS2( Z1 , TTN , RMSW(D7) )
       IF( K .EQ. 3 ) CALL PMTS3( Z1 , TTN , RMSW(D7) )
C----------------------------------------------------------------------
C--> O�PE�E��EM MATP��� "G"  ( G = H1 * Z1 )
C----------------------------------------------------------------------
       I1 = 1
       DO 90 I=1,K
       DO 90 J=I,K
       A = H1 * Z1(I1)
       RMSW(D6+I1-1) = A
       IND0 = IDK(I,J)
       IND1 = IDK(J,I)
       G0(IND0) = A
       G0(IND1) = A
       I1 = I1 + 1
90     CONTINUE
C----------------------------------------------------------------------
C-->  BK�A��BAEM
C----------------------------------------------------------------------
       KKON = 9 + K
C
C
       CALL DEPOSA( MSW(9), MSW(KKON), K, K, G0,
     =              RMSW(D7), YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
C######################################################################
666    CONTINUE
C-->  ZAPROS = 2 ------------------------------------------------------
C######################################################################
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
C----------------------------------------------------------------------
C-->  HAXO��M HOB�E HA�P��EH��.
C----------------------------------------------------------------------
       DO 100 I=1,K
       IMSW = D0 + I - 1
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN1(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN1(KK)
       ULN(I) = VN - VK
100    CONTINUE
C----------------------------------------------------------------------
C-->  �MHO��M  G * ULN1
C----------------------------------------------------------------------
       IF( K .EQ. 2 ) CALL PMTS2( RMSW(D6), ULN, TTN )
       IF( K .EQ. 3 ) CALL PMTS3( RMSW(D6), ULN, TTN )
C----------------------------------------------------------------------
C-->  �A��C� B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       S = 0
       DO 110 I=1,K
       A = TTN(I) + RMSW( D7 - 1 + I )
       TOKN1(ITOK1 + I) = A
       S = S + A * RMSW(D3-1+I)
110    CONTINUE
       TOKN1(ITOK+K) = S * H1 / (H1+LCT/RCT) + RMSW(D8)
       KOD = 0
       KY = MSW(D8+2)
       IF( KY .EQ. 0 ) RETURN
C----------------------------------------------------------------------
C-->   KY = 1 ==> �A�AH PE��M ABTOMAT��ECKO�O B�XO�A HA �CT. PE��M
C--> ��EM MOMEHT �OCT��EH�� KP�BO� HA�P��EH�� U1 H��EBO�O �HA�EH��
C----------------------------------------------------------------------
       KN = MSW(D0)
       KK = MSW(D0+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       U1N = VN - VK
       U1N1= ULN(1)
       IF( U1N1*U1N .GE.  0.0 ) RETURN
       TOKMUX = RMSW(D8+1)
       IF( U1N1 .GT. U1N ) TOKMUX = - TOKMUX
C----------------------------------------------------------------------
C--> �CTAHAB��BAEM PE��M XO�OCTO�O XO�A
C----------------------------------------------------------------------
       TOKN (ITOK)   = 0.99 * TOKMUX
       TOKN1(ITOK)   = 0.99 * TOKMUX
          DO 120 I=2,K
          TOKN (ITOK+I-1) = 0.0
          TOKN1(ITOK+I-1) = 0.0
120    CONTINUE
       TOKN (ITOK+K) = 0.99 * TOKMUX * RMSW(D3)
       TOKN1(ITOK+K) = 0.99 * TOKMUX * RMSW(D3)
       MSW(D8+2) = 0
       RETURN
C****************
       END
