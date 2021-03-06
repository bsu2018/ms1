         SUBROUTINE DM0038 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �HBAP�   1989 �.)------------
C   �O��PO�PAMMA:  DM0038 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C                  T��A # 36  39  --> TPEX�A�H�E  TPAHC�OPMATOP� ;
C                  *********************
C     KO�-BO O�-K  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C     K = 6        36      2-X O�M. TPAHC.       TP2
C     K = 9        39      3-X O�M. TPAHC.       TP3
C
C!!!!!
C    B H � M A H � E :   K  O�PE�E��ETC� �CXO�� �� HOMEPA T��A:
C    ****************
C                          K  = TIP - 30
C
C!!!!
C                  BBO� �O O��CAH��:
C
C           � # T 3@ � @ @ ..  � SH UH PXX IXX PKZ1 PKZ2 ...UKZ1 UKZ2
C           ..R1 R2 .. W1 W2 ...K�
C
C          ���� �O�K���EH��: <HA�A�A O�MOTOK> <KOH�� O�MOTOK>
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
C                  PA�MEPHOCT� MACC�BOB �P���� "ALL" O�PE�E���TC�:
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
C               "�OCTO�HHA�" �ACT� MSW T��A # 36, 39
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
C                  RMSW T��OB # 36  39:
C                  --------------------
C
C
C  �OCT.  I �EPEMEHHA� RMSW-->
C +-------+--+--------+--------+--------+------+------+----------+...>
C I ����  I KI R �APA-I L �AP- I W �AP- I      I      I X  MAT-  I...>
C I       I  I   MET- I   AME- I   AME- I R CT I L CT I    P�-   I...>
C I       I  I    P�  I   TP�  I   TP�  I      I      I    �A    I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I       I 1I K C�OB I K C�OB I K C�OB I  3   I  3   I 9 C�OB   I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I 9-... I  I        I        I        I      I      I          I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I          I        I        I        I             I
C D0         D1      D2       D3       D4            D5
C DR0        DR1     DR2      DR3      DR4           DR5
C
C
C
C ...>+---------+--------+------+------+-------+-------+
C ...>I G MAT-  I J BEK- I      I      I       I       I
C ...>I   P��A  I   TOP  I JMU  I A1   I  IXX  I  K�   I
C ...>I         I        I      I      I       I       I
C ...>+---------+--------+------+------+-------+-------+
C ...>I 36      I K C�OB I  3   I  9   I   1   I   1   I
C ...>+---------+--------+------+--------------I-------I
C ...>I         I        I      I      I       I       I
C ...>+---------+--------+------+------+-------+-------+
C     I         I        I      I      I       I       I
C     D6       D7       D8     D9
C     DR6      DR7      DR8    DR9
C
C
C
C
C
C
C               C M E � E H � � :
C               -----------------
C             T�� #36       T�� # 39
C   -----------------------------------
C   K--->       6              9
C   -----------------------------------
C   LM-->      110           112
C   -----------------------------------
C   D0-->       9             9
C   -----------------------------------
C   D1-->      22             19
C   -----------------------------------
C   D2-->      28             28
C   -----------------------------------
C   D3-->      34             37
C   -----------------------------------
C   D4-->      40             46
C   -----------------------------------
C   D5-->      46             52
C   -----------------------------------
C   D6-->      55             58
C   -----------------------------------
C   D7-->      91            103
C   -----------------------------------
C   D8-->      97            112
C   -----------------------------------
C   D9-->      100           112
C   -----------------------------------
C   LMR->      80             98
C--------------------------------------
C
C   ��� �EPEMEHHO� �ACT� DR<*> = D<*> - D1 + 2
C                        $$$$$$$$$$$$$$$$$$$$$
          DIMENSION MD0(2), MD1(2), MD2(2), MD3(2), MD4(2)
          DIMENSION MD5(2), MD6(2), MD7(2), MD8(2),MD9(2)
         INTEGER D0, D1, D2, D3, D4, D5, D6, D7, D8,D9
         INTEGER ADRMSW
         INTEGER TIP,  ZAPROS
         REAL JALL,LCT,L1,L2
C
C@@
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION TTN1(3),TTN2(3), ULN1(3),ULN2(3),Z1(9),Z2(9)
         DIMENSION X(9),C(9),A(9),G11(9),G12(9),G21(9),G22(9),G0(36)
         DIMENSION TOKMU(3),TOKJM(3),Z11(9),Z21(9)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         DATA MD0 /  9,   9  /
         DATA MD1 / 22,  28  /
         DATA MD2 / 28,  37  /
         DATA MD3 / 34,  46  /
         DATA MD4 / 40,  55  /
         DATA MD5 / 46,  61  /
         DATA MD6 / 55,  67  /
         DATA MD7 / 91, 112  /
         DATA MD8 / 97, 121  /
         DATA MD9 /100, 121  /
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
       IF ( TIP .EQ. 36  .OR. TIP .EQ. 39  ) GO TO 9999
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
       K1 = K / 3 - 1
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
       D9 = MD9(K1)
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
       DO 10 I=1,3
       TTN1(I) = TOKN(ITOK1+I)
10     CONTINUE
       DO 11 I=1,3
       TTN2(I) = TOKN(ITOK1+3+I)
11     CONTINUE
       DO 12 I =1,3
       TOKMU(I) = TOKN(ITOK1+K+I)
12     CONTINUE
C----------------------------------------------------------------------
C-->  HAXO��M CTAP�E HA�P��EH��.
C----------------------------------------------------------------------
       DO 20 I=1,3
       IMSW = D0 + I - 1
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       ULN1(I) = VN - VK
20     CONTINUE
       DO 21 I=1,3
       IMSW = D0 + I + 2
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       ULN2(I) = VN - VK
21     CONTINUE
C----------------------------------------------------------------------
C      �P�CBAEH�E �APAMETPOB TPAHC�OPMATOPA
C----------------------------------------------------------------------
       L1 = RMSW(D2)
       L2 = RMSW(D2+3)
       LCT = RMSW(D4+3)
       RCT = RMSW(D4)
       R1  = RMSW(D1)
       R2 = RMSW(D1+3)
       W1 = RMSW(D3)
       W2 = RMSW(D3+3)
C----------------------------------------------------------------------
C--> O�PE�E��EM MATP��� "A"  A = LCT + H1*X  "�C�O�����C� MATP��A "A"
C----------------------------------------------------------------------
       DO 22 I =1,9
       X(I) = RMSW(D5 + I - 1)
22     CONTINUE
       DO 25 I =1,9
       A(I)=H1*X(I)
25     CONTINUE
       A(1) = A(1) + LCT
       A(5) = A(5) + LCT
       A(9) = A(9) + LCT
C----------------------------------------------------------------------
C  O�PA�AEM MATP��� "A" "
C---------------------------------------------------------------------
       CALL OMTS3( A , A  )
C----------------------------------------------------------------------
C  �A�OM�HAEM MATP��� "A**(-1)"
C---------------------------------------------------------------------
       DO 26 I=1,9
       RMSW(D9 +I-1) = A(I)
 26    CONTINUE
C---------------------------------------------------------------------
C O�PE�E��EM MATP��� "C"  C =H1*( X - H1*X*A**(-1)*X)
C----------------------------------------------------------------------
       CALL YMTS3( A  , X, C  )
       CALL YMTS3( X  , C, C  )
       DO 40 I=1,9
       C(I) =H1*( X(I) - H1*C(I))
40     CONTINUE
C----------------------------------------------------------------------
C  O�PE�E��EM JM = A**(-1)*(LCT*TOKMU+H2*X*(W1*TTN1+W2*TTN2-TOKMU))
C----------------------------------------------------------------------
       DO 50 I=1,3
       TOKJM(I) = H2*(W1*TTN1(I)+W2*TTN2(I) - TOKMU(I))
 50    CONTINUE
       CALL NMTS3(X,TOKJM,TOKJM)
       DO 60 I=1,3
       TOKJM(I) = LCT*TOKMU(I)+TOKJM(I)
 60    CONTINUE
       CALL NMTS3(A,TOKJM,TOKJM)
C===> �A�OMH�M JM...
       DO 70 I=1,3
       RMSW(D8+I-1) = TOKJM(I)
 70    CONTINUE
C----------------------------------------------------------------------
C  O�PE�E��EM  Z = L + H1*R + H1*W*C*W
C----------------------------------------------------------------------
       DO 80 I =1,9
       Z1(I) = W1**2*C(I)
       Z2(I) = W2**2*C(I)
 80    CONTINUE
       DO 85 I=1,3
       Z1(4*I-3) = L1 + H1*R1 + Z1(4*I-3)
       Z2(4*I-3) = L2 + H1*R2 + Z2(4*I-3)
 85    CONTINUE
C----------------------------------------------------------------------
C  O�PA�AEM MATP��� "Z"
C----------------------------------------------------------------------
       CALL OMTS3( Z1, Z11)
       CALL OMTS3( Z2, Z21)
C----------------------------------------------------------------------
C--> O�PE�E��EM MATP��� "G"
C----------------------------------------------------------------------
       CALL YMTS3(C,Z21,Z21)
       CALL YMTS3(Z21,C,G11)
       CALL YMTS3(C,Z11,Z11)
       CALL YMTS3(Z11,C,G22)
       DO 90 I= 1,9
       G11(I) = Z1(I) - (W1*W2)**2*G11(I)
       G22(I) = Z2(I) - (W1*W2)**2*G22(I)
 90    CONTINUE
       CALL OMTS3(G11,G11)
       CALL OMTS3(G22,G22)
       DO 92  I=1,9
       G11(I) = H1*G11(I)
       G22(I) = H1*G22(I)
 92    CONTINUE
       CALL YMTS3(G11,Z21,G12)
       CALL YMTS3(G22,Z11,G21)
       DO 93  I=1,9
       G12(I) = -W1*W2*G12(I)
       G21(I) = -W1*W2*G21(I)
 93    CONTINUE
C-----------------------------------------------------------------------
       DO 95 I=1,3
       DO 95 J=1,3
       G0( J+6*(I-1)) = G11( J+3*(I-1))
       G0( J+6*(I-1)+3 ) = G21( J+3*(I-1))
       G0( J+6*(I-1)+18) = G12( J+3*(I-1))
       G0( J+6*(I-1)+21) = G22( J+3*(I-1))
 95    CONTINUE
       DO 96 I= 1,9
       RMSW(D6+I-1) = G11(I)
       RMSW(D6+I+8) = G21(I)
       RMSW(D6+I+17) = G12(I)
       RMSW(D6+I+26) = G22(I)
 96    CONTINUE
C----------------------------------------------------------------------
C-->  O�PE�E��M BEKTOP "J"
C----------------------------------------------------------------------
C===>  O�PE�E��EM CHA�A�A F1,F2 .XPAH�M B Z1,Z2
       DO 98  I =1,3
       A(I) = W1*TTN1(I)+W2*TTN2(I) - TOKMU(I)
 98    CONTINUE
       CALL NMTS3(X,A,A)
       DO 100 I =1,3
       Z1(I) = L1*TTN1(I)+H2*(ULN1(I)-R1*TTN1(I)-W1*A(I))
       Z2(I) = L2*TTN2(I)+H2*(ULN2(I)-R2*TTN2(I)-W2*A(I))
100    CONTINUE
C-----------------------------------------------------------------------
       CALL NMTS3(X,TOKJM,TOKJM)
       DO 120 I =1,3
       Z1(I) = H1*W1*TOKJM(I)+Z1(I)
       Z2(I) = H1*W2*TOKJM(I)+Z2(I)
120    CONTINUE
       CALL NMTS3(Z21,Z2,RMSW(D7))
       CALL NMTS3(Z11,Z1,RMSW(D7+3))
C-----------------------------------------------------------------------
C   �A�OM�HAEM J1,J2
C-----------------------------------------------------------------------
       DO 130 I = 1,3
       RMSW(D7+I-1)= -W1*W2*RMSW(D7+I-1) + Z1(I)
       RMSW(D7+I+2)= -RMSW(D7+I+2)*W1*W2 + Z2(I)
130    CONTINUE
        DO 150 I = 1,9
       G11(I) = G11(I)/H1
       G22(I) = G22(I)/H1
150    CONTINUE
       CALL NMTS3(G11,RMSW(D7),RMSW(D7))
       CALL NMTS3(G22,RMSW(D7+3),RMSW(D7+3))
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
       DO 200 I=1,3
       IMSW = D0 + I - 1
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN1(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN1(KK)
       ULN1(I) = VN - VK
200    CONTINUE
       DO 201 I=1,3
       IMSW = D0 + I + 2
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN1(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN1(KK)
       ULN2(I) = VN - VK
201    CONTINUE
       DO 205 I= 1,9
        G11(I)=  RMSW(D6+I-1)
        G21(I)=  RMSW(D6+I+8)
        G12(I)=  RMSW(D6+I+17)
        G22(I)=  RMSW(D6+I+26)
 205   CONTINUE
C----------------------------------------------------------------------
C-->  �MHO��M  G * ULN1
C----------------------------------------------------------------------
       CALL NMTS3( G11, ULN1, TTN1 )
       CALL NMTS3( G12, ULN2, TTN2 )
C----------------------------------------------------------------------
C-->  �A��C� B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       DO 210 I=1,3
       A1 = TTN1(I)+TTN2(I) + RMSW( D7 - 1 + I )
       TOKN1(ITOK1 + I) = A1
210    CONTINUE
C----------------------------------------------------------------------
C-->  �MHO��M  G * ULN1
C----------------------------------------------------------------------
       CALL NMTS3( G21, ULN1, TTN1 )
       CALL NMTS3( G22, ULN2, TTN2 )
C----------------------------------------------------------------------
C-->  �A��C� B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       DO 211 I=1,3
       A1= TTN1(I)+TTN2(I) + RMSW( D7 + 2 + I )
       TOKN1(ITOK1 + I+3) = A1
211    CONTINUE
       DO 220 I=1,9
       A(I) = RMSW(D9+I-1)
       X(I) = RMSW(D5+I-1)
220    CONTINUE
       DO 225 I=1,3
       TTN1(I) = W1*TOKN1(ITOK1+I)+W2*TOKN1(ITOK1+I+3)
       TOKJM(I) = RMSW(D8+I-1)
225    CONTINUE
       CALL NMTS3(X,TTN1,TTN1)
       CALL NMTS3(A,TTN1,TTN1)
       DO 230 I =1,3
       TOKN1(ITOK1+K+I) = H1*TTN1(I) + TOKJM(I)
230    CONTINUE
       KOD = 0
       KY = MSW(D9+10)
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
       U1N1= ULN1(1)
       IF( U1N1*U1N .GE.  0.0 ) RETURN
       TOKMUX = RMSW(D9+9)
       IF( U1N1 .GT. U1N ) TOKMUX = - TOKMUX
C----------------------------------------------------------------------
C--> �CTAHAB��BAEM PE��M XO�OCTO�O XO�A
C----------------------------------------------------------------------
       DO 300 I=1,2
       TOKN (ITOK+I)   = - 0.49 * TOKMUX
       TOKN1(ITOK+I)   =  -0.49 * TOKMUX
       TOKN (ITOK+K+I) = - 0.49 * TOKMUX * RMSW(D3)
       TOKN1(ITOK+K+I) = - 0.49 * TOKMUX * RMSW(D3)
300    CONTINUE
       DO 305 I =1,3
          TOKN (ITOK1+I+3) = 0.0
          TOKN1(ITOK1+I+3) = 0.0
305    CONTINUE
       TOKN (ITOK)   = 0.99 * TOKMUX
       TOKN1(ITOK)   = 0.99 * TOKMUX
       TOKN (ITOK+K) = 0.99 * TOKMUX * RMSW(D3)
       TOKN1(ITOK+K) = 0.99 * TOKMUX * RMSW(D3)
       MSW(D9+10) = 0
       RETURN
CCCC      DEBUG INIT
       END
