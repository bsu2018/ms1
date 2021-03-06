         SUBROUTINE DM0027 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   OKT��P�   1987 �.)------
C   �O��PO�PAMMA:  DM0027 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  MO�E�� # 17--> �CTO�H�K TOKA, ��PAB��EM�� �/�
C                                 �O���OBATE��.
C
C
C              ��EMEHT O��C�BAETC�:
C
C      � # T 17 � K1 K2 �
C              MAS < NTOKOB  #��1 #CM1  #��2 #CM2 ... >
C              MAS < NHA�P   ��E�N1 ��E�K1  ��E�N2 ��E�K2 ... >
C              PSUB #P < .........>;
C
C              �EPE� A��APAT PSUB �A�A�TC� �AKOH� ��PAB�EH�� ;
C
C      ��E
C          NTOKOB -   KO���ECTBO TOKOB ��EMEHTOB CXEM� , KOTOP�E
C                     �C�O�����TC� ��� ��PAB�EH�� �CTO�H�KOM TOKA;
C          #��I , #CMI     - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                            �C�O����EMO�O ��� ��PAB�EH�� �CTO�H�KOM;
C          NHA�P           - KO���ECTBO HA�P��EH�� ME��� ���AM� CXEM� ,
C                            �C�O����EM�X ��� ��PAB�EH�� �CTO�H�KOM;
C          ��E�NI , ��E�KI - HOMEPA ���OB, HA�P��EH�� ME��� KOTOP�M�
C                            �C�O�����TC� ��� ��PAB�EH�� �CTO�H�KOM;
C          #P              - HOMEP �/� �O���OBATE�� ( PSUB#P ), B
C                            KOTOPO� �PO��BO��TC� PAC�ET TOKA �CTO�H�KA;
C
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTA �E��
C
C
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
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
C                   ( =1 �P� �O���O� �OK. �O�PE�HOCT� );
C                 MSWALL( (RMSWAL ) - BEC� C��COK MSW_ALL,
C                 ADRMSW - A�PEC TEK. C�OAB MSW ( RMSW );
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST, CALL_PS --> B��.  PSUB �/�;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 17:
C                ---------------------------------
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
C              �OPMAT RMSW T�� # 17:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0023    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I      I HOMEP ���A �CXO���E�O.
C    10      I     0        I      I HOMEP ���A BXO���E�O.
C----------------------------------------------------------------------
C    11      I     1        I  FM  I  NTOKOB -KO���ECTBO TOKOB, �C�O��-
C            I              I      I          ��EM�X �P� PAC�ETE TOKA
C            I              I      I          �CTO�H�KA;
C    12      I     2        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     3        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     4        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C  9+I*3     I  -1+I*3      I  FM  I  #��I -      -"-
C 10+I*3     I     I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   1+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NTOKOB*3 I  2+NTOKOB*3  I  FM  I  NHA�P -KO���ECTBO HA�P��EH��,
C            I              I      I         �C�. ��� PAC�ETA TOKA
C            I              I      I         �CTO�H��KA;
C13+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  ��E�N1 -��E� HA�A�A HA�P��EH��;
C14+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  ��E�K1 -��E� KOH�A  HA�P��EH��;
C .........  I ............ I .... I  ...........................
C            I              I      I
C13+NTOKOB*3+I 2+NTOKOB*3+  I      I  NPSUB - HOMEP �/� �O���OBATE��
C NHA�P*2    I  NHA�P*2     I  FM  I         ( PSUB) B KOTOPO� �PO��BO-
C            I              I      I         ��TC� PAC�ET TOKA �CTO�H�KA
C (...)+1    I  (...)+1     I  FM  I  NPAR - KO���ECTBO �APAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  �APAMETP� PSUB , �A�AHH�E
C�O(...)+NPARI �O(...)+NPAR I      I  �O���OBATE�EM.
C            I              I      I
C 15+3*NTOKOBI 5 + 3*NTOKOB I  DM  I  TOKT - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P�  T=TN+1
C NPAR       I NPAR         I      I         ( �PO�HO� )
C 16+3*NTOKOBI 6 + 3*NTOKOB I  DM  I   G - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I          �P� T-TH;1
C NPAR       I NPAR         I      I         ( �O C�PO�HO��P. TOK� )
C            I              I      I
C 17+3*NTOKOBI 7 + 3*NTOKOB I  DM  I   J - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I          �P� T=TN+1
C NPAR       I NPAR         I      I
C            I              I      I
C 18+3*NTOKOBI 8 + 3*NTOKOB I  DM  I    NMOD - �P��HAK MO�E��
C +2*NNAPR + I + 2*NNAPR +  I      I           0 - ��HE�HA�
C NPAR       I NPAR         I      I         > 0 - HE��HE�HA�
C            I              I      I
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 18 + 3*NTOKOB + 2*NNAPR + NPAR
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 8 + 3*NTOKOB + 2*NNAPR + NPAR
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C     **********************************
C     1    -     �HA�EH�E TOKA �CTO�H�KA TOKA �P� T=TN/TN1
C     2    -     �HA�EH�E AKT�BHO� MO�HOCT� KOM�EHC�P�EMO� HA�P��K�
C     3    -     �HA�EH�E O�MEHHO� MO�HOCT� KOM�EHCATOPA
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION INTMAS(200), STMAS(50), DGMAS(5)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR, INTMAS, J
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
       IF ( TIP .EQ. 17 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0027 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C----------------------------------------------------------------------
C-->  O�PE�E��M KOHCTAHT�.
       NTOKOB = MSW(11)
       NNAPR = MSW (12 + 3*NTOKOB )
       INPAR = 14 + 3*NTOKOB + 2*NNAPR
       NPAR  = MSW (INPAR)
       ITOKT = INPAR + 1 + NPAR
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
C-->  ZAPROS = 1 --> PAC��TAT� �HA�EH�� �CTO�H�KA TOKA � �POBO��MOCT�
C     �P� T=TN+1 � B�O��T�.
100    CONTINUE
       IF( FITER .EQ. 1 ) GO TO 150
       STMAS( 1) = TN
       STMAS( 2) = H0
       ISTMAS = 2
C----------------------------------------------------------------------
C--> �EPE�AEM TOK�
       IF( NTOKOB .EQ. 0 ) GO TO 16
          DO 15 I= 1,NTOKOB
          ISTMAS = ISTMAS + 1
          ITOKN = MSW( 3*I + 11 )
          STMAS(ISTMAS) = TOKN(ITOKN)
15     CONTINUE
16     CONTINUE
C----------------------------------------------------------------------
C--> �EPE�AEM HA�P��EH��
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 18
       IMSWV = 11 + 3*NTOKOB
          DO 17 I= 1,NNAPR
          ISTMAS = ISTMAS + 1
          IMSWV = IMSWV + 2
          NA = MSW( IMSWV )
          KO = MSW( IMSWV + 1 )
          VNA = 0.0
          IF( NA .GT. 0 ) VNA = VALLN(NA)
          VKO = 0.0
          IF( KO .GT. 0 ) VKO = VALLN(KO)
          STMAS(ISTMAS) = VNA - VKO
17     CONTINUE
18     CONTINUE
C----------------------------------------------------------------------
C-->  URMAS <--> MSW(14 + 3*NTOKOB + 2*NNAPR)
C----------------------------------------------------------------------
       INPSUB = 13 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PAC��T�BAEM �HA�EH�E TOKA �P� T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 1.
       ITOK = MSW(7)
       DGMAS(2) = TOKN( ITOK )
       RMSW(ITOKT)=TOKN(ITOK)
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT = DGMAS(2)
       G=DGMAS(3)
       RMSW( ITOKT+2 ) = TOKT
       RMSW( ITOKT+1 ) = G
       MSW( ITOKT+3 ) = IFIX(DGMAS(1))
       TOKN1( ITOK + 1 ) = DGMAS(4)
       TOKN1( ITOK + 2 ) = DGMAS(5)
       GO TO 170
150    CONTINUE
C----------------------------------------------------------------------
C -->  �TEPA��� HE �EPBA�...
       IF(MSW(ITOKT+3).EQ.0) GO TO 160
       STMAS( 1) = TN
       STMAS( 2) = H0
       ISTMAS = 2
C----------------------------------------------------------------------
C--> �EPE�AEM TOK�
       IF( NTOKOB .EQ. 0 ) GO TO 26
          DO 25 I= 1,NTOKOB
          ISTMAS = ISTMAS + 1
          ITOKN = MSW( 3*I + 11 )
          STMAS(ISTMAS) = TOKN1(ITOKN)
25     CONTINUE
26     CONTINUE
C----------------------------------------------------------------------
C--> �EPE�AEM HA�P��EH��
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 28
       IMSWV = 11 + 3*NTOKOB
          DO 27 I= 1,NNAPR
          ISTMAS = ISTMAS + 1
          IMSWV = IMSWV + 2
          NA = MSW( IMSWV )
          KO = MSW( IMSWV + 1 )
          VNA = 0.0
          IF( NA .GT. 0 ) VNA = VALLN1(NA)
          VKO = 0.0
          IF( KO .GT. 0 ) VKO = VALLN1(KO)
          STMAS(ISTMAS) = VNA - VKO
27     CONTINUE
28     CONTINUE
C----------------------------------------------------------------------
C-->  URMAS <--> MSW(14 + 3*NTOKOB + 2*NNAPR)
C----------------------------------------------------------------------
       INPSUB = 13 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PAC��T�BAEM �HA�EH�E TOKA �P� T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 2.
       DGMAS(2) = RMSW( ITOKT )
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT = DGMAS(2)
       G=DGMAS(3)
       RMSW(ITOKT+2) = TOKT
       RMSW(ITOKT+1) = G
       ITOK = MSW(7)
       TOKN1( ITOK + 1 ) = DGMAS(4)
       TOKN1( ITOK + 2 ) = DGMAS(5)
       GO TO 170
 160   CONTINUE
       G = RMSW( ITOKT+1 )
       TOKT = RMSW(ITOKT+2)
170    CONTINUE
C----------------------------------------------------------------------
C -->  �O��EMEHTH�� BK�A�......
C----------------------------------------------------------------------
       K = MSW(9)
       M = MSW(10)
       KOD = 0
C--->
       CALL DEPOST( K , M , G , TOKT , YALL , JALL )
C--->
       RETURN
C######################################################################
200    CONTINUE
C----------------------------------------------------------------------
C-->  ZAPROS = 2  --> PAC��TAT� TOK ��� T=TN+1 � �A��CAT� B TOKN1
C----------------------------------------------------------------------
C  �PE�CKA�AHH�� TOK.
       X1 = RMSW( ITOKT )
C----------------------------------------------------------------------
C  �O���EHH�� TOK.
       K = MSW(9)
       M = MSW(10)
       VK = 0.0
       IF(K.GT.0) VK = VALLN1(K)
       VM = 0.0
       IF(M.GT.0) VM = VALLN1(M)
       VKM = VK-VM
       ITOK = MSW(7)
       X2 = RMSW(ITOKT+1)*VKM+RMSW(ITOKT+2)
C----------------------------------------------------------------------
C  O�HOB�M CTAPOE �HA�EH�E.
       RMSW( ITOKT ) = X2
       TOKN1(ITOK) =X2
       IF(MSW(ITOKT+3).EQ.0) RETURN
C-->  �O�PE��M...
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 30
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 31
30     EPS = ABS( X1 - X2 )
31     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) RETURN
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) RETURN
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
       RETURN
       END
