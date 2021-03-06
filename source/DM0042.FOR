         SUBROUTINE DM0042 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT ﭢ���  1992 �.)------------
C   �O��PO�PAMMA:  DM0042 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C
C                  A) ZAPROS = 1
C                  -------------
C                    C�POC ��A�A FQ - BE����H� ��P. �APAM. O�PE�E�EH�;
C                  === H� �E�O HE PAC��T�BAEM ==
C
C                  B) ZAPROS = 2
C                     ----------
C                      == B C���AE H��EBO�O �HA�EH�� ��A�A FQ -->
C                  B���C�EH�E
C
C
C              FQ - PAC�ET ��P. HA�P��EH�� B��O�HEH -->
C                  MEXAH��M �TO�O ��A�A �O�BO��ET C�HXPOH���POBAT�
C                  B��O�HEH�� PA����H�X DM_MOH�TOPOB C �.T��A # 4;
C
C                  C) ZAPROS = 3
C                     **********
C                     HE �O��EP��BAETC� ( PE�EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE �O��EP��BAETC�;
C
C----------------------------------------------------------------------
C
C              O�PA�AT�BAET MAKPOMO�E�� ��HE�H�X ��HAM��ECK�X �BEH�EB:
C
C   +-----+-------------------------------+---+--------------------+
C   I     I                                   I     �ਬ�砭��     I
C   I T�� I       HA�MEHOBAH�E �BEHA          I                    I
C   I     I                                   I                    I
C   +-----+-------------------------------+---+--------------------+
C   I     I                                   I                    I
C   I     I                                   I ��� ��ᯥਬ���   I
C   I 340 I  ������� ᯥ樠�쭮 ���         I �� ������� �ࠢ��-I
C   I     I  �ࠢ����� ���� ��ਮ���� ��    I ��� .              I
C   I     I   �ந������ ������� ,�������  I                    I
C   I     I   ⠡��筮 .                      I                    I
C   +-----+-----------------------------------+--------------------+
C                ******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C      � # T 320 � 0 �  #��  #CM  P  MAX  H0 T0
C              MAS < NT  X1  Y1  X2  Y2 ... >
C
C      ��E
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          P      -   ��ਭ� ��ਤ�� �ࠢ�����;
C          NT     -   KO���ECTBO  TO�EK �EPE�ATO�HO� ��HK���
C                     �A�AHHO� TA����HO;
C          X1 Y1  -   COOTBETCTBEHHO ������ �६��� � ���祭��
C          � T.�.     �������� �ਢ�� ��室���� ⮪� .
C
C----------------------------------------------------------------------
C
C   METO�:     O�PE�E�EH�E B�XO�H�X C��HA�OB ��HE�H�X ��HAM��ECK�X
C   -----      �BEH�EB B COOTBETCTB�� C �X �EPE�ATO�H�M� ��HK���M�.
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
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C
C
C               "�OCTO�HHA�" �ACT� MSW T��OB # 310,311,312,313:
C                ----------------------------------------------
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
C                      �OPMAT RMSW T�� # 340
C                 ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I  NMIN- �।���� �窠
C            I              I      I
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I  P   - ��ਭ� ��ਤ�� �ࠢ�����
C    16      I     7        I  FM  I  NT  - KO���ECTBO  TO�EK
C            I              I      I        �EPE�ATO�HO� ��HK���
C    17      I     8        I  FM  I  X1  - 1-� ������ �६���
C            I              I      I        �������� �ਢ�� ���. ⮪�
C   ....     I    .....     I      I  .....
C    16+NT   I     7+NT     I  FM  I  X(NT) - ������� �६���
C            I              I      I          �������� �ਢ�� ���. ⮪�
C    16+NT+1 I     7+NT+1   I  FM  I  Y1    -  1-O� TO�K�
C            I              I      I         �������� �ਢ�� ���. ⮪�
C    ......  I    ......    I      I .......
C    16+2NT  I     7+2NT    I  FM  I  Y(NT) - NT-O� TO�K�
C            I              I      I        �������� �ਢ�� ���. ⮪�
C    17+2NT  I     8+2NT    I  FM  I  M   - ���ᨬ���� ⮪
C    18+2NT  I     9+2NT    I  FM  I  DELTA -ࠧ����� ���. ⮪� �� �।. 蠣�
C    19+2NT  I    10+2NT    I  FM  I  SIGOLD ����������� ������
C    19      I     10       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 16 + 2 * NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 7 + 2 * NT
C----------------------------------------------------------------------
C
C----------------------------------------------------------------------
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C     **********************************
C     TOKN/N1(1) - �HA�EH�E B�XO�HO�O C��HA�A;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(100)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION INTMAS(1), STMAS(100), DGMAS(12)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR,INTMAS
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
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       NTIP = TIP - 339
       IF ( NTIP . EQ . 1 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0024 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C--->  B��OP PE��MA �A�POCA..
6666   GO TO( 100, 200), ZAPROS
C----------------------------------------------------------------------
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C==> C�POC ��A�A       OCHOBHO� PAC�ET B��O�HEH;
       MSW(10) = 0
       RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-----ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  H��HO �� PA�OTAT� ?
       IF ( MSW(10) .EQ. 1 ) RETURN
C----------------------------------------------------------------------
C-->  �P��ETC� .............................
C-->  ZAPROS = 2 --> PAC��TAT� �HA�EH��....
C      IF ( NTIP .EQ . 10) GO TO 5000
C      IF ( NTIP .EQ . 41) GO TO 6000
C      GO TO (1000,2000,3000,4000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   ������� ��� �ࠢ����� ���� ��ਮ���� ( ��� #340 )
C----------------------------------------------------------------------
       NT = MSW(16)
       ITOK = MSW(7)
       IF (TN .GT. RMSW(21 + 2*NT )) GO TO 1100
C--> ��砫쭠� ��⠭���� ...
       NMIN = 1
       TOKN1(ITOK) = RMSW(20 + 2 * NT)
        RMSW(19 + 2*NT) = RMSW(20 + 2 * NT)
       DELTA2 = 0.
       GO TO 9990
C-->  ���� ....
1100   CONTINUE
       NMIN = MSW(11)
       DELTA1 = RMSW(18 + 2*NT)
       SIGOLD = RMSW(19 + 2*NT)
       TMAK   = RMSW(17 + 2*NT)
       NTOK   = MSW(14)
       TCUR   = TOKN1(NTOK)
       SIDE   = RMSW(15)
1150   CONTINUE
C--> �� ����� ���ࢠ�� �� ��室���� ...
       IF (NMIN . LT . NT) GOTO 1155
       SIG1 =  RMSW(16 + 2*NT) + SIDE
       SIG2 =  RMSW(16 + 2*NT) - SIDE
       GOTO 1300
1155   CONTINUE
       IF ( TN .LT. RMSW(16 + NMIN +1)) GOTO 1200
       NMIN = NMIN + 1
       GOTO 1150
1200   CONTINUE
C-->  ��� �⠭��ୠ� ���௮���� ...
       TOLD = RMSW(16 +NMIN)
       TNEW = RMSW(16 + NMIN +1)
       SOLD = RMSW(16+ NT + NMIN)
       SNEW = RMSW(16 + NT+NMIN +1)
       signal = SOLD + (SNEW - SOLD)*(TN - TOLD) / (TNEW - TOLD)
       SIG1 = signal + SIDE
       SIG2 = signal -  SIDE
1300   CONTINUE
C--->  �ࠢ������ ���. ⮪ � ������� ��ਤ�஬ .
       IF (TCUR.GT.SIG1) THEN
       DELTA2 = TCUR - SIG1
                 ELSEIF (TCUR.LT.SIG2) THEN
                 DELTA2 = TCUR - SIG2
         ELSE
         DELTA2 = 0.
         ENDIF
        SIGOUT = SIGOLD - (DELTA2 - DELTA1)*14.9/ TMAK
C---> ����襬 �ࠢ���騩 ᨣ���
        RMSW(19 + 2*NT) = SIGOUT
C--> ����襬 � TOKN1
        IF(SIGOUT.GE.15. ) SIGOUT = 14.9
        IF(SIGOUT.LE.(-15.0) ) SIGOUT = -14.9
        TOKN1(ITOK) = SIGOUT
C----------------------------------------------------------------------
9990   CONTINUE
       MSW(11) = NMIN
       RMSW(18 + 2*NT) = DELTA2
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
       END
