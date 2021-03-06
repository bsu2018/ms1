         SUBROUTINE DM0041 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT CEHT��P� 1988 �.)------------
C   �O��PO�PAMMA:  DM0041 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C                  T��A # 42- 45  -->  TPAHC�OPMATOP� ;
C                  *********************
C     KO�-BO O�-K  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C     K = 2        42      2-X O�M. TPAHC.       TP2
C     K = 3        43      3-X O�M. TPAHC.       TP3
C     K = 4        44      4-X O�M. TPAHC.       TP4
C     K = 5        45      5� O�M. TPAHC.       TP5
C
C!!!!!
C    B H � M A H � E :   K  O�PE�E��ETC� �CXO�� �� HOMEPA T��A:
C    ****************
C                          K  = TIP - 40
C
C!!!!
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
C             "�OCTO�HHA�" �ACT� MSW T��A # 42, 43, 44, 45
C              -------------------------------------------
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
C                  RMSW T��OB # 42
C                  ---------------
C
C
C  �OCT.     I �EPEMEHHA� RMSW-->
C +----------+--------+--------+--------+------+------+----+-----+----+
C I   ����   I R �APA-I L �AP- I W �AP- I      I      I    I     I    I
C I          I   MET- I   AME- I   AME- I Rsum I Lsum IG21 I G22 I J2 I
C I          I    P�  I   TP�  I   TP�  I      I      I    I     I    I
C +----------+--------+--------+--------+------+------+----+-----+----+
C I          I  1,2   I  3,4   I  5,6   I  7   I  8   I 9  I 10  I 11 I
C +----------+--------+--------+--------+------+------+----+-----+----+
C I9,10,11,12I 13,14  I 15,16  I 17,18  I  19  I  20  I 21 I 22  I 23 I
C +----------+--------+--------+--------+------+------+----+-----+----+
C
C
C
C                  RMSW T��OB # 43
C                  ---------------
C
C +----------------+--------+--------+--------+-------------+---------+
C I      ����      I R �APA-I L �AP- I W �AP- I ��p孥�p��- I         I
C I                I   MET- I   AME- I   AME- I���쭠� ����I    J    I
C I                I    P�  I   TP�  I   TP�  I      G      I         I
C +----------------+--------+--------+--------+-------------+---------+
C I                I 1,2,3  I 4,5,6  I 7,8,9  I   10-15     I  16-18  I
C +----------------+--------+--------+--------+-------------+---------+
C I9,10,11,12,13,14I15,16,17I18,19,20I21,22,23I   24-29     I  30-32  I
C +----------------+--------+--------+--------+-------------+---------+
C
C
C                  RMSW T��OB # 44
C                  ---------------
C +---------+--------+--------+--------+-------------+---------+
C I  ����   I        I        I        I ��p孥�p��- I         I
C I         I R sum  I  L sum I    W   I���쭠� ����I    J    I
C I         I        I        I        I      G      I         I
C +---------+--------+--------+--------+-------------+---------+
C I         I 1 - 6  I 7 - 12 I 13-16  I   17-26     I  27-30  I
C +---------+--------+--------+--------+-------------+---------+
C I 9 - 16  I 17 -22 I 23 - 28I 29 - 32I   33-42     I  43-46  I
C +---------+--------+--------+--------+-------------+---------+
C
C
C                  RMSW T��OB # 45
C                  ---------------
C +---------+--------+--------+--------+-------------+---------+
C I         I        I        I        I ��p孥�p��- I         I
C I  ����   I R sum  I  L sum I    W   I���쭠� ����I    J    I
C I         I        I        I        I      G      I         I
C +---------+--------+--------+--------+-------------+---------+
C I         I 1 - 10 I 11- 20 I 21-25  I   26-40     I  41-45  I
C +---------+--------+--------+--------+-------------+---------+
C I 9 - 18  I 19 -28 I 29- 38 I 39-43  I   44-58     I  59-63  I
C +---------+--------+--------+--------+-------------+---------+
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         DIMENSION STR(5), TTN(5), ULN(5), Z(25), Z1(25), G0(25)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         INTEGER ADRMSW
         INTEGER TIP,  ZAPROS
         REAL JALL, LSUM
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
C######################################################################
C  BCTPOEHHA� ��HK���  IDK(I,J) O�PE�E��ET HOMEP ��EMEHTA B BEKTOPE ,
C  B KOTOPOM XPAH�TC� KBA�PATHA� MATP��A (�O CTO���AM) PA�MEPHOCT�� K.
C
       IDK(I,J) = K*(J-1) + I
C      **********************
C######################################################################
C--->  KOHTPO�� T��A
       TIP = MSW(2)
       K = TIP - 40
       IF ( K .GE. 2  .AND. K .LE. 5  ) GO TO 9999
C----------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT(/' *** �O�. C�O� B DM0041 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
9999     CONTINUE
C######################################################################
       K1 = K - 1
       K2 = K1 * (K1+1) / 2
       NRSUM = 9 + 2 * K
       NLSUM = NRSUM + K2
       NW    = NLSUM + K2
       NGALL = NW    + K
       NJALL = NGALL + K*(K+1)/2
C----------------------------------------------------------------------
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
          DO 10 I=1,K
          TTN(I) = TOKN(ITOK+I-1)
10     CONTINUE
C----------------------------------------------------------------------
C-->  HAXO��M CTAP�E HA�P��EH��.
C----------------------------------------------------------------------
       DO 20 I=1,K
       IMSW = 8 + I
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       ULN(I) = VN - VK
20     CONTINUE
C----------------------------------------------------------------------
       IF( K .GT. 2 ) GO TO 3000
C----------------------------------------------------------------------
C    O�HO�A�H�� �B�XO�MOTO�H�� TPAHC�OPMATOP (T�� # 42)
C----------------------------------------------------------------------
       RSUM = RMSW(19)
       LSUM = RMSW(20)
C--> AK - KO��������� �������������
       AK   = RMSW(18) / RMSW(17)
       ZN   = LSUM + H1 * RSUM
       A    = H1 / ZN
C--> ��������� �������  G
       G0(1)= AK**2 * A
       G0(2)= -AK   * A
       G0(3)= -AK   * A
       G0(4)= A
       RMSW(21) = G0(2)
       RMSW(22) = G0(4)
C--> ��������� ������  J
       A = ( H2*(ULN(2)-AK*ULN(1)) + TTN(2)*(LSUM-H2*RSUM) ) / ZN
       TTN(1) = -AK * A
       TTN(2) =       A
       RMSW(23) =     A
C----------------------------------------------------------------------
C-->  BK�A��BAEM
C----------------------------------------------------------------------
       KKON = 9 + K
C
C
       CALL DEPOSA( MSW(9), MSW(KKON), K, K, G0,
     =              TTN, YALL, JALL )
       KOD = 0
       RETURN
C----------------------------------------------------------------------
C    O�HO�A�H�� TPEX-���� O�MOTO�H�� TPAHC�OPMATOP (T�� # 43,44,45)
C----------------------------------------------------------------------
3000   CONTINUE
C----------------------------------------------------------------------
C--> ��p������ �������⥫�   LSUM + H1 * RSUM
       DO 3010 I=1,K2
       I1 = I - 1
       Z(I) = RMSW(NLSUM+I1) + H1 * RMSW(NRSUM+I1)
3010   CONTINUE
C----------------------------------------------------------------------
C--> ��p�頥� ���p���   (LSUM + H1 * RSUM)
       IF( K .EQ. 3 ) CALL MMTS2( Z , Z1 )
       IF( K .EQ. 4 ) CALL MMTS3( Z , Z1 )
       IF( K .EQ. 5 ) CALL MMTS4( Z , Z1 )
C----------------------------------------------------------------------
C--> ��p��p㥬 ���p���  G
       I1 = 1
       DO 3020 I=2,K
          DO 3020 J=I,K
          A = H1 * Z1(I1)
          RMSW(NGALL+K+I1-1) = A
          IND0 = IDK(I,J)
          IND1 = IDK(J,I)
          G0(IND0) = A
          G0(IND1) = A
          I1 = I1 + 1
3020   CONTINUE
       G0(1) = 0.0
          DO 3040 J=2,K
          S = 0.0
             DO 3030 I=2,K
             IND = IDK(I,J)
             S = S + G0(IND) * RMSW(NW+I-1)
3030      CONTINUE
          S = -S / RMSW(NW)
          I = 1
          IND0 = IDK(I,J)
          IND1 = IDK(J,I)
          G0(IND0) = S
          G0(IND1) = S
          RMSW(NGALL+J -1) = S
          G0(1) = G0(1) + S * RMSW(NW+J-1)
3040   CONTINUE
       G0(1) = -G0(1) / RMSW(NW)
       RMSW(NGALL) = G0(1)
C----------------------------------------------------------------------
C--> ��p��p㥬 ����p  J
C
C--> ��p������   LSUM - H2* RSUM
       DO 3050 I=1,K2
       I1 = I - 1
       Z(I) = RMSW(NLSUM+I1) - H2 * RMSW(NRSUM+I1)
3050   CONTINUE
C
C--> �������  (LSUM - H2* RSUM) * TTN
       IF( K .EQ. 3 ) CALL PMTS2( Z , TTN(2) , STR(2) )
       IF( K .EQ. 4 ) CALL PMTS3( Z , TTN(2) , STR(2) )
       IF( K .EQ. 5 ) CALL PMTS4( Z , TTN(2) , STR(2) )
C
C--> �p������  (LSUM - H2* RSUM) * TTN + H2 * [UN-U1/W1 * W]
       A = ULN(1) / RMSW(NW)
       DO 3060 I=2,K
       TTN(I) = STR(I) + H2 * ( ULN(I) - A * RMSW(NW+I-1) )
3060   CONTINUE
C--> �������  Z1 * [(LSUM - H2* RSUM) * TTN + H2 * (UN-U1/W1 * W) ]
       IF( K .EQ. 3 ) CALL PMTS2( Z1 , TTN(2) , RMSW(NJALL+1) )
       IF( K .EQ. 4 ) CALL PMTS3( Z1 , TTN(2) , RMSW(NJALL+1) )
       IF( K .EQ. 5 ) CALL PMTS4( Z1 , TTN(2) , RMSW(NJALL+1) )
C--> ��p������   J(1)
       TTN(1) = 0.0
       DO 3070 I=2,K
       TTN(1) = TTN(1) + RMSW(NJALL+I-1)*RMSW(NW+I-1)
3070   CONTINUE
       RMSW(NJALL) = -TTN(1)/RMSW(NW)
C----------------------------------------------------------------------
C-->  BK�A��BAEM
C----------------------------------------------------------------------
       KKON = 9 + K
C
C
       CALL DEPOSA( MSW(9), MSW(KKON), K, K, G0,
     =              RMSW(NJALL), YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
C######################################################################
666    CONTINUE
C-->  ZAPROS = 2 ------------------------------------------------------
C######################################################################
       ITOK = MSW(7)
C----------------------------------------------------------------------
C-->  HAXO��M HOB�E HA�P��EH��.
C----------------------------------------------------------------------
       DO 100 I=1,K
       IMSW = 8 + I
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN1(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN1(KK)
       ULN(I) = VN - VK
100    CONTINUE
C----------------------------------------------------------------------
C-->  �A��C� B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       IF( K .GT. 2 ) GO TO 30000
C--> AK - KO��������� �������������
       AK   = RMSW(18) / RMSW(17)
       A = RMSW(21) * ULN(1) + RMSW(22) * ULN(2) + RMSW(23)
       TOKN1(ITOK  ) = -AK * A
       TOKN1(ITOK+1) =       A
       KOD = 0
       RETURN
C*********************************************************************
30000  CONTINUE
C----------------------------------------------------------------------
C-->  �MHO��M  G * ULN1
C----------------------------------------------------------------------
       IF( K .EQ. 3 ) CALL PMTS3( RMSW(NGALL), ULN, TTN )
       IF( K .EQ. 4 ) CALL PMTS4( RMSW(NGALL), ULN, TTN )
       IF( K .EQ. 5 ) CALL PMTS5( RMSW(NGALL), ULN, TTN )
C----------------------------------------------------------------------
C-->  �A��C� B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       DO 110 I=1,K
       TOKN1(ITOK - 1 + I) =  TTN(I) + RMSW( NJALL - 1 + I )
110    CONTINUE
C----------------------------------------------------------------------
       KOD = 0
       RETURN
C*********************************************************************
       END
