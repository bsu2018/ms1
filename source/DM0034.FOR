         SUBROUTINE DM0034 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT MAPTA   1988 �.)------------
C   �O��PO�PAMMA:  DM0034 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C
C               *******************************************************
C               *  T��A # 520 [ �B��ATE�� �OCTO�HHO�O TOKA (�=CONST)]
C               *  T��A # 521 [ �B��ATE�� �OCTO�HHO�O TOKA (�=CONST)]
C               *******************************************************
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
C                   ( �C�O����ETC� ��� MAKPOMO�E�E� "��EA��H�X" K���E�)
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C
C
C            "�OCTO�HHA�" �ACT� MSW T��A # 520,521:
C             -------------------------------------
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
C              �OPMAT RMSW T�� # 520:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I      I HOMEP ���A HA�A�A
C    10      I     0        I      I HOMEP ���A KOH�A
C----------------------------------------------------------------------
C    11      I     1        I  FM  I  R  - CO�POT�B�EH�E �KOP�;
C    12      I     2        I  FM  I  L  - �H��KT�BHOCT� �KOP�;
C    13      I     3        I  FM  I  K� - KOHCTP�KT�BH�� KO������EHT;
C    14      I     4        I  FM  I  JM - MOMEHT �HEP���;
C    15      I     5        I  FM  I        PE�EPB
C    16      I     6        I  FM  I  T� - �OCTO�HHA� �KOPHO� �E��;
C    17      I     7        I  FM  I  TM - �OCTO�HHA� MEXAH��ECKA�;
C    18      I     8        I  DM  I  MBH- �HA�EH�E MBH ��� T=TN;
C    19      I     9        I  DM  I  MBH1-�HA�EH�E MBH ��� T=TN1;
C    20      I    10        I  DM  I  G -  �HA�EH�E �POBO��MOCT�;
C    21      I    11        I  DM  I  J -  �HA�EH�E �CT. TOKA;
C    22      I    12        I  FM  I         PE�EPB
C    23      I    13        I  FM  I         PE�EPB
C    24      I    14        I  FM  I  NT - KO���ECTBO TO�EK MBH(T);
C    25      I    15        I  FM  I  T1 - BPEM�;                      ;
C    26      I    16        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 24 + 2*NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 14 + 2*NT
C
C----------------------------------------------------------------------
C
C
C
C              �OPMAT RMSW T�� # 521:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -3        I      I HOMEP ���A HA�A�A O�MOTK� �KOP�
C    10      I    -2        I      I HOMEP ���A KOH�A  O�MOTK� �KOP�
C    11      I    -1        I      I HOMEP ���A HA�A�A O�MOTK� T�.
C    12      I     0        I      I HOMEP ���A KOH�A  O�MOTK� T�.
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  R  - CO�POT�B�EH�E �KOP�;
C    14      I     2        I  FM  I  L  - �H��KT�BHOCT� �KOP�;
C    15      I     3        I  FM  I  K� - KOHCTP�KT�BH�� KO������EHT;
C    16      I     4        I  FM  I  JM - MOMEHT �HEP���;
C    17      I     5        I  FM  I  KTG- KO�����EHT �EPE�A�� T�.
C    18      I     6        I  FM  I  GTG- �POBO��MOCT� O�MOTK� T�.
C    19      I     7        I  FM  I  T� - �OCTO�HHA� �KOPHO� �E��;
C    20      I     8        I  FM  I  TM - �OCTO�HHA� MEXAH��ECKA�;
C    21      I     9        I  DM  I  MBH- �HA�EH�E MBH ��� T=TN;
C    22      I    10        I  DM  I  MBH1-�HA�EH�E MBH ��� T=TN1;
C    23      I    11        I  DM  I  GR-  �POBO��MOCT� �E�� �KOP�;
C    24      I    12        I  DM  I  JR-  �CT. TOKA    �E�� �KOP�;
C    25      I    13        I  DM  I  GRG- �POBO��MOCT� �E�� �KOP� - T�
C    26      I    14        I  DM  I  JRG- �CT. TOKA    �E�� �KOP� - T�
C    27      I    15        I  FM  I  NT - KO���ECTBO TO�EK MBH(T);
C    28      I    16        I  FM  I  T1 - BPEM�;                      ;
C    29      I    17        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 27 + 2*NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 15 + 2*NT
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         REAL JALL,  JM,  KF,  L, J, MBH, MBH1 , KMAS02, KTG, JR, JRG
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
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS
         TN1 = TN + H0
C--->  KOHTPO�� T��A
         NTIP = MSW(2) - 519
         IF ( NTIP .LT. 1  .OR.  NTIP .GT. 2  ) GO TO  9999
C---> B��OP PE��MA:
         GO TO ( 667, 666                 ), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C######################################################################
C--------------------ZAPROS=1------------------------------------------
C--->  B���C�EH�E G - J �APAMETPOB � �X �A��C� B MSW:
         IF( NTIP .EQ. 2 ) GO TO 700
C######################################################################
C
C   �B��ATE�� �OCTO�HHO�O TOKA ( T�� # 520 )
C
C######################################################################
C---->   ���� �O�K���EH��:
         K = MSW(9)
         M = MSW(10)
C-->   �APAMETP�
         R  = RMSW(11)
         L  = RMSW(12)
         KF = RMSW(13)
         JM = RMSW(14)
         TR = RMSW(16)
         TM = RMSW(17)
C----------------------------------------------------------------------
         ITOK = MSW(7)
         TOKTN = TOKN( ITOK )
         WN    = TOKN( ITOK + 2 )
         MBH   = TOKN( ITOK + 3 )
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
C----------------------------------------------------------------------
         Z1 = TR*TM + TM*H1 + H1**2
         Z2 = TR*TM - TM*H2 - H1*H2
         Z3 = H1 / KF
C----------------------------------------------------------------------
         MBH1 = KMAS02( TN1 , RMSW(24) )
C----------------------------------------------------------------------
         G = TM*H1 / (R*Z1)
C--->
         J = (TOKTN*Z2 + UN*TM*H2/R - WN*JM*H0/KF + MBH1*Z3*H1 +
     +        MBH*Z3*H2 ) / Z1
C----------------------------------------------------------------------
         RMSW(18) = MBH
         RMSW(19) = MBH1
         RMSW(20) = G
         RMSW(21) = J
C--->   �O��EMEHTH�� BK�A�:
C======================================================================
         KOD = 0
C--->
         CALL DEPOST(K, M,  G, J, YALL, JALL )
C---->
         RETURN
C======================================================================
700      CONTINUE
C######################################################################
C
C   �B��ATE�� �OCTO�HHO�O TOKA C TAXO�EHEPATOPOM ( T�� # 521 )
C
C######################################################################
C---->   ���� �O�K���EH��:
         M = MSW(9)
         N = MSW(10)
         K1= MSW(11)
         K2= MSW(12)
C-->   �APAMETP�
         R  = RMSW(13)
         L  = RMSW(14)
         KF = RMSW(15)
         JM = RMSW(16)
         KTG= RMSW(17)
         GTG= RMSW(18)
         TR = RMSW(19)
         TM = RMSW(20)
C-->  TOK� � HA�P��EH�� ��� T=TN
         ITOK = MSW(7)
         TOKTN = TOKN( ITOK )
         WN    = TOKN( ITOK + 2 )
         MBH   = TOKN( ITOK + 3 )
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN(N)
         URN = VM - VN
C----------------------------------------------------------------------
C---> GJ-�APAMETP� �KOPHO� �E��.
         Z1 = TR*TM + TM*H1 + H1**2
         Z2 = TR*TM - TM*H2 - H1*H2
         Z3 = H1 / KF
C----------------------------------------------------------------------
         MBH1 = KMAS02( TN1 , RMSW(27) )
C----------------------------------------------------------------------
         GR = TM*H1 / (R*Z1)
C--->
         JR = (TOKTN*Z2 + URN*TM*H2/R - WN*JM*H0/KF + MBH1*Z3*H1 +
     +        MBH*Z3*H2 ) / Z1
C----------------------------------------------------------------------
C---> �A�OMH�M
         RMSW(21) = MBH
         RMSW(22) = MBH1
         RMSW(23) = GR
         RMSW(24) = JR
C--->   �O��EMEHTH�� BK�A�:
C
         CALL DEPOST(M, N,  GR, JR, YALL, JALL )
C----------------------------------------------------------------------
C---> GJ-�APAMETP� �E�� T�.
         GRG = KTG * GTG * H1 * KF / JM
         JRG = KTG * GTG * ( WN - H1*MBH1/JM + H2*(TOKTN*KF-MBH)/JM )
C---> �A�OMH�M
         RMSW(25) = GRG
         RMSW(26) = JRG
C---> B�O��M...
         J = - GRG * JR - JRG
C
         CALL DEPOST(K1, K2,  GTG, J, YALL, JALL )
C
         G = - GRG * GR
C
         CALL DEPOSN(K1, K2, M, N, G, YALL )
C
         KOD = 0
         RETURN
C======================================================================
666      CONTINUE
C######################################################################
C--->  O�PE�E��EM TOK�:  ZAPROS = 2
         IF( NTIP .EQ. 2 ) GO TO 800
C######################################################################
C
C   �B��ATE�� �OCTO�HHO�O TOKA ( T�� # 520 )
C
C######################################################################
C-->   �APAMETP�
         KF = RMSW(13)
         JM = RMSW(14)
         MBH = RMSW(18)
         MBH1= RMSW(19)
         G   = RMSW(20)
         J   = RMSW(21)
C---->   ���� �O�K���EH��:
         K = MSW(9)
         M = MSW(10)
C--->  HOBOE HA�P��EH�E:
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UN1= VK - VM
C--->  CTAP�E TOK�:
         ITOK = MSW(7)
         TOKTN = TOKN(ITOK)
         YN    = TOKN(ITOK+1)
         WN    = TOKN(ITOK+2)
C--->  HOB�E TOK�:
         TOKTN1 = G * UN1 + J
         WN1 = WN + ( H1*(TOKTN1*KF-MBH1) + H2*(TOKTN*KF-MBH) ) / JM
         YN1 = YN + H1*WN1 + H2*WN
C----------------------------------------------------------------------
C--->     �A��C� TOKOB  B TOKN1
         TOKN1( ITOK    ) = TOKTN1
         TOKN1( ITOK +1 ) = YN1
         TOKN1( ITOK +2 ) = WN1
         TOKN1( ITOK +3 ) = MBH1
         KOD = 0
         RETURN
C----------------------------------------------------------------------
800      CONTINUE
C######################################################################
C
C   �B��ATE�� �OCTO�HHO�O TOKA C TAXO�EHEPATOPOM ( T�� # 521 )
C
C######################################################################
C-->   �APAMETP�
         KF = RMSW(15)
         JM = RMSW(16)
         GTG= RMSW(18)
         MBH = RMSW(21)
         MBH1= RMSW(22)
         GR  = RMSW(23)
         JR  = RMSW(24)
         GRG = RMSW(25)
         JRG = RMSW(26)
C---->   ���� �O�K���EH��:
         M = MSW(9)
         N = MSW(10)
         K1= MSW(11)
         K2= MSW(12)
C--->  HOBOE HA�P��EH�E:
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         URN1= VM - VN
         VK1 = 0.0
         IF ( K1 .NE. 0 ) VK1 = VALLN1(K1)
         VK2 = 0.0
         IF ( K2 .NE. 0 ) VK2 = VALLN1(K2)
         UTGN1= VK1 - VK2
C--->  CTAP�E TOK�:
         ITOK = MSW(7)
         TOKTN = TOKN(ITOK)
         YN    = TOKN(ITOK+1)
         WN    = TOKN(ITOK+2)
C--->  HOB�E TOK�:
         TOKTN1 = GR * URN1 + JR
         WN1 = WN + ( H1*(TOKTN1*KF-MBH1) + H2*(TOKTN*KF-MBH) ) / JM
         YN1 = YN + H1*WN1 + H2*WN
         TOKTG1 = UTGN1 * GTG - GRG * GR * URN1 - GRG * JR - JRG
C----------------------------------------------------------------------
C--->     �A��C� TOKOB  B TOKN1
         TOKN1( ITOK    ) = TOKTN1
         TOKN1( ITOK +1 ) = YN1
         TOKN1( ITOK +2 ) = WN1
         TOKN1( ITOK +3 ) = MBH1
         TOKN1( ITOK +4 ) = TOKTG1
         KOD = 0
         RETURN
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0034 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
C******* DEBUG INIT
         END
