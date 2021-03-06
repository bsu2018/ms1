         SUBROUTINE DM0004 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0004 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTOB �E��
C                  T��A # 4 [ �CTO�H�K �OCTO�HHO�O TOKA ]
C                  T��A # 5 [ �CTO�H�K �OCTO�HHO�O HA�P��EH�� ]
C                  ********************************************
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
C                  D) ZAPROS = 3
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
C                  VALLN- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                         B T = TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          B T = TN + H0;
C                  TOKN  � TOKN1 - BEKTOPA "TOKOB" �EPEMEHH�X COCTO�H��
C                                  MO�E�� ��EMEHTA B TN � TN+H0;
C                                  PA�MEPHOCT� CM. MASPRT(46)=NNTOK;
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
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3,
C   ------------             DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=  1   ��� 2 - COO��EH�E � �XO�;
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 4  �  # 5:
C                ---------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��.  I KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I�HA�.  I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I        I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW T��A # 4 [ �CTO�H�K �OCTO�HHO�O TOKA ]
C                  -------------------------------------------
C
C                SPM
C +-----+-----++------+
C IHOM. I HOM.II �HA�.I
C I���A I���A II   J  I
C I "K" I "M" II      I
C +-----+-----++------+
C I 9   I 10  II  11  I
C +-----------++------+
C
C
C             RMSW T��A # 5 [ �CTO�H�K �OCTO�HHO�O HA�P��EH�� ]
C             ------------------------------------------------
C
C                   SPM
C +-----+-----++--------+------+
C IHOM. I HOM.II �HA�.  I �HA�.I
C I���A I���A II  1/R   I  E/R I
C I "K" I "M" II        I      I
C +-----+-----++--------+------+
C I 9   I 10  II  11    I  12  I
C +-----------++--------+------+
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADRMSW
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
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
         INTEGER FKOM, ZAPROS, TIP
         REAL JALL,  JE
C===>    BCTPOEHHA� ��HK��� O�C����BAH�� �B�XMEPH�X MACC�BOB:
         ID(I,J) = NDALL * (J-1)  +  I
C---->   ���� �O�K���EH��:
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO�� T��A
         TIP = MSW(2)
         IF ( TIP .NE. 4  .AND.  TIP .NE. 5 ) GO TO  9999
C---> B��OP PE��MA:
         GO TO ( 667, 666                 ), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C-----------ZAPROS = 1-------------------------------------------------
C--->  �TO �A T��?
         IF ( TIP .EQ. 5 ) GO TO 5
C######################################################################
C---> �CTO�H�K �OCTO�HHO�O TOKA ( T�� #4 )
C--->  B���C�EH�E G - J �APAMETPOB � �X �A��C� A MSW:
         GE = 0.0
         JE = RMSW(11)
C--->   �O��EMEHTH�� BK�A�:
C======================================================================
         KOD = 0
C--->
         CALL DEPOST( K, M, GE, JE, YALL, JALL )
C--->
         RETURN
C######################################################################
C---> �CTO�H�K �OCTO�HHO�O HA�P��EH�� ( T�� #5 )
5      CONTINUE
C--->  B���C�EH�E G - J �APAMETPOB � �X �A��C� A MSW:
         GE = RMSW(11)
         JE = RMSW(12)
C--->   �O��EMEHTH�� BK�A�:
C======================================================================
         KOD = 0
C--->
         CALL DEPOST( K, M, GE, JE, YALL, JALL )
C--->
         RETURN
C======================================================================
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C--->  �TO �A T��?
         IF ( TIP .EQ. 5 ) GO TO 15
C######################################################################
C---> �CTO�H�K �OCTO�HHO�O TOKA ( T�� #4 )
         TOKT = RMSW(11)
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
       RETURN
C######################################################################
C---> �CTO�H�K �OCTO�HHO�O HA�P��EH�� ( T�� #5 )
15     CONTINUE
C--->  O�PE��E��EM TOK�:   ZAPROS =  1
         GE = RMSW(11)
         JE = RMSW(12)
C--->  HOB�� TOK:
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UE = VK - VM
         TOKT = GE * UE  + JE
C---> �EPE�A��C� TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0004 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
