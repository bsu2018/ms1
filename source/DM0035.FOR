         SUBROUTINE DM0035 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT  A�PE�� 1988 �.)------------
C   �O��PO�PAMMA:  DM0035 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C  +-----+-------------------------------------------+----------------+
C  I     I                               I  �EPE�A-  I   PAC�ETHA�    I
C  I T�� I       HA�MEHOBAH�E �BEHA      I  TO�HA�   I    �OPM��A     I
C  I     I                               I  ��HK���  I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I  �PO�OP��OHA��HO-C�MM�P���EE  I           I                I
C  I 318 I       ( � - PE����TOP )       I     K     I YN1=SUM(K*UN1) I
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I  �PO�OP��OHA��H�� C�MMATOP    I           I                I
C  I 324 I       KBA�PATOB               I           IYN1=SUM(K*UN1**2)
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C                ******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C      � # T 3XX � 0 �
C              MAS < NNAPR  ��H1 ��K1 K1  ��H2 ��K2 K2  ... >
C              MAS < NGARM  W  A0  A1  FI1  A2  FI2... >
C
C      ��E
C          NNAPR  -   KO���ECTBO HA�P��EH�� CXEM� , KOTOP�E
C                     �C�O�����TC� B KA�ECTBE BXO�H�X C��HA�OB;
C          ��HI,��KI- HOMEPA ���OB, HA�P��EH�E ME��� KOTOP�M�
C                     �C�O����ETC� B KA�ECTBE BXO�HO�O C��HA�A;
C          KI       - KO������EHT �PO�OP��OHA��HOCT� I-TO�O
C                     HA�P��EH��;
C          NGARM  -   KO���ECTBO ��EMEHTOB �APMOH��ECKO�O P��A
C                     O��C�BA��E�O �A�AHH�� C��HA�, �O�ABAEM��
C                     HA BXO� PE����TOPA (C ��ETOM �OCT. COCTAB�.);
C          W    -     �ACTOTA OCHOBHO� �APMOH�K� ( 1/C );
C          A0,A1,...- AM���T��� COOTBETB����X �APMOH�K C��HA�A;
C          FI1,FI2...-HA�. �A�� COOTBETB����X �APMOH�K C��HA�A;
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
C               "�OCTO�HHA�" �ACT� MSW T��OB # 318, # 324:
C                -----------------------------------------
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
C              �OPMAT RMSW T�� # 318, # 324:
C              -----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0030    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NNAPR-KO���ECTBO HA�P., �C�O��-
C            I              I      I        ��EM�X KAK BXO�H�E C��HA��;
C    12      I     3        I  FM  I  ��H1 - ��E� HA�A�A;
C    13      I     4        I  FM  I  ��K1 - ��E� KOH�A;
C    14      I     5        I  FM  I  K1   - KO������EHT �PO�OP��OHA��-
C            I              I      I         HOCT� HA�P��EH��;
C  9+I*3     I     I*3      I  FM  I  ��HI -      -"-
C 10+I*3     I   1+I*3      I  FM  I  ��KI -      -"-
C 11+I*3     I   2+I*3      I  FM  I  KI   -      -"-
C .......    I   .......    I  ... I  ..........................
C12+NNAPR*3  I  3+NBXOD*3   I  FM  I  NGARM -KO���ECTBO �APMOH�K, �C�.
C            I              I      I         ��� O��CAH�� �A�. C��HA�A
C            I              I      I         (C ��ETOM �OCT. COCTAB�.);
C 13+NNAPR*3 I  4+NNAPR*3   I  FM  I  W  -   �ACTOTA OCH. �APMOH�K�;
C 14+NNAPR*3 I  5+NNAPR*3   I  FM  I  A0  -  AM���T��A �OCT. COCTAB��-
C            I              I      I         ��E� �A�. C��HA�A;
C 15+NNAPR*3 I  6+NNAPR*3   I  FM  I  A1  -  AM���T��A 1-O� �APMOH�K�
C            I              I      I         �A�AHHO�O C��HA�A;
C 16+NNAPR*3 I  7+NNAPR*3   I  FM  I  FI1 -  HA�. �A�A 1-O� �APMOH�K�;
C 17+NNAPR*3 I  8+NNAPR*3   I  FM  I  A2  -  AM���T��A 2-O� �APMOH�K�;
C 18+NNAPR*3 I  9+NNAPR*3   I  FM  I  FI2 -  HA�. �A�A 2-O� �APMOH�K�;
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 12 + 3*NNAPR + 2*NGARM
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 3 + 3*NNAPR + 2*NGARM
C
C----------------------------------------------------------------------
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C     **********************************
C     TOKN/N1(1) - �HA�EH�E B�XO�HO�O C��HA�A;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
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
       NTIP = TIP - 317
       IF ( NTIP .EQ. 1 ) GO TO 9900
       IF ( NTIP .EQ. 7 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0035 - �A�POC HA O�C����BAH�E ���O�
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
       GO TO (1000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   �PO�OP��OHA��HO C�MM�P���EE �BEHO ( T�� 318 )
C----------------------------------------------------------------------
       NNAPR = MSW(11)
       NGARM = MSW(12 + 3*NNAPR)
       SIGNAL = 0.
       IF( NGARM .LT. 1 ) GO TO 1110
C----------------------------------------------------------------------
C--> �P��AB�M �HA�EH�E �A�AHHO�O BXO�HO�O C��HA�A
       W = RMSW(13 + 3*NNAPR)
       A0 = RMSW(14 + 3*NNAPR)
       SIGNAL = A0
       I1 = 2*(NGARM - 1)
       IF( I1 .EQ. 0 ) GO TO 1110
          DO 1100 I=1,I1,2
          NRMSW = 14 + I + 3*NNAPR
          AI = RMSW(NRMSW)
          FII= RMSW(NRMSW+1)
          N = I / 2 + 1
          SIGNAL = SIGNAL + AI * SIN(N*W*(TN+H0) + FII)
1100   CONTINUE
1110   CONTINUE
       IF( NTIP .EQ. 7 ) SIGNAL = SIGNAL**2
C----------------------------------------------------------------------
C--> �POC�MM�P�EM BXO�H�E C��HA��
       IF(NNAPR .EQ. 0 ) GO TO 1210
          DO 1200 I=1,NNAPR
          NMSW = 11 + I*3
          AKI  = RMSW(NMSW)
          NYZN = MSW(NMSW-2)
          NYZK = MSW(NMSW-1)
          VN = 0.0
          IF( NYZN .NE. 0 ) VN = VALLN1(NYZN)
          VK = 0.0
          IF( NYZK .NE. 0 ) VK = VALLN1(NYZK)
          IF( NTIP .EQ. 1 ) SIGNAL = SIGNAL + AKI*(VN - VK)
          IF( NTIP .EQ. 7 ) SIGNAL = SIGNAL + AKI*(VN - VK)**2
1200   CONTINUE
1210   CONTINUE
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       ITOK = MSW(7)
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
       END
