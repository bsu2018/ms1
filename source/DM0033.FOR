         SUBROUTINE DM0033 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MAPTA     1988 �.)------
C   �O��PO�PAMMA:  DM0033 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  MO�E�� # 18--> TPEX�A�H�� �CTO�H�K TOKA,
C                                 ��PAB��EM�� �/� �O���OBATE��.
C
C
C              ��EMEHT O��C�BAETC�:
C
C      � # T 18 � KA KB KC K0 �
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
C               "�OCTO�HHA�" �ACT� MSW T��A # 18:
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
C
C              �OPMAT RMSW T�� # 18:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0029    I      I
C------------I--------------I------I-----------------------------------
C     9      I     A        I      I HOMEP ���A A
C    10      I     B        I      I HOMEP ���A B
C    11      I     C        I      I HOMEP ���A C
C    12      I     0        I      I HOMEP ���A 0
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  NTOKOB -KO���ECTBO TOKOB, �C�O��-
C            I              I      I          ��EM�X �P� PAC�ETE TOKA
C            I              I      I          �CTO�H�KA;
C    14      I     2        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    15      I     3        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    16      I     4        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C 11+I*3     I  -1+I*3      I  FM  I  #��I -      -"-
C 12+I*3     I     I*3      I  FM  I  #CMI -      -"-
C 13+I*3     I   1+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C14+NTOKOB*3 I  2+NTOKOB*3  I  FM  I  NHA�P -KO���ECTBO HA�P��EH��,
C            I              I      I         �C�. ��� PAC�ETA TOKA
C            I              I      I         �CTO�H��KA;
C15+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  ��E�N1 -��E� HA�A�A HA�P��EH��;
C16+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  ��E�K1 -��E� KOH�A  HA�P��EH��;
C .........  I ............ I .... I  ...........................
C            I              I      I
C15+NTOKOB*3+I 2+NTOKOB*3+  I      I  NPSUB - HOMEP �/� �O���OBATE��
C NHA�P*2    I  NHA�P*2     I  FM  I         ( PSUB) B KOTOPO� �PO��BO-
C            I              I      I         ��TC� PAC�ET TOKA �CTO�H�KA
C (...)+1    I  (...)+1     I  FM  I  NPAR - KO���ECTBO �APAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  �APAMETP� PSUB , �A�AHH�E
C�O(...)+NPARI �O(...)+NPAR I      I  �O���OBATE�EM.
C            I              I      I
C 17+3*NTOKOBI 5 + 3*NTOKOB I  DM  I  TOKTA - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   A    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 18+3*NTOKOBI 6 + 3*NTOKOB I  DM  I  TOKTB - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   B    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 19+3*NTOKOBI 7 + 3*NTOKOB I  DM  I  TOKTC - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   C    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 20+3*NTOKOBI 8 + 3*NTOKOB I  DM  I  TOKT0 - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   0    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 21+3*NTOKOBI 9 + 3*NTOKOB I  DM  I  G12 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 22+3*NTOKOBI10 + 3*NTOKOB I  DM  I  G13 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 23+3*NTOKOBI11 + 3*NTOKOB I  DM  I  G23 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 24+3*NTOKOBI12 + 3*NTOKOB I  DM  I  G14 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 25+3*NTOKOBI13 + 3*NTOKOB I  DM  I  G24 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 26+3*NTOKOBI14 + 3*NTOKOB I  DM  I  G34 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C
C 27+3*NTOKOBI15 + 3*NTOKOB I  DM  I  J12 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 28+3*NTOKOBI16 + 3*NTOKOB I  DM  I  J13 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 29+3*NTOKOBI17 + 3*NTOKOB I  DM  I  J23 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 30+3*NTOKOBI18 + 3*NTOKOB I  DM  I  J14 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 31+3*NTOKOBI19 + 3*NTOKOB I  DM  I  J24 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 32+3*NTOKOBI20 + 3*NTOKOB I  DM  I  J34 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C
C 33+3*NTOKOBI21 + 3*NTOKOB I  DM  I  NMOD - �P��HAK MO�E��
C +2*NNAPR + I + 2*NNAPR +  I      I     0 - ��HE�HA�
C NPAR       I NPAR         I      I   > 0 - HE��HE�HA�
C            I              I      I
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 33 + 3*NTOKOB + 2*NNAPR + NPAR
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 21 + 3*NTOKOB + 2*NNAPR + NPAR
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C     **********************************
C     1    -     �HA�EH�E TOKA �CTO�H�KA, BXO���E�O B ��E�  KA
C                              �P� T=TN/TN1
C     2    -     �HA�EH�E TOKA �CTO�H�KA, BXO���E�O B ��E�  KB
C                              �P� T=TN/TN1
C     3    -     �HA�EH�E TOKA �CTO�H�KA, BXO���E�O B ��E�  KC
C                              �P� T=TN/TN1
C     4    -     �HA�EH�E TOKA �CTO�H�KA, B�XO���E�O �� ���A  K0
C                              �P� T=TN/TN1----------------------
C     5    -
C     6    -
C     6    -                   P E � E P B
C     8    -
C     9    -
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION INTMAS(200), STMAS(50), DGMAS(22)
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
       IF ( TIP .EQ. 18 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0033 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C----------------------------------------------------------------------
C-->  O�PE�E��M KOHCTAHT�.
       NTOKOB = MSW(13)
       NNAPR = MSW (14 + 3*NTOKOB )
       INPAR = 16 + 3*NTOKOB + 2*NNAPR
       NPAR  = MSW (INPAR)
       ITOKTA = INPAR + 1 + NPAR
       ITOKTB = INPAR + 2 + NPAR
       ITOKTC = INPAR + 3 + NPAR
       ITOKT0 = INPAR + 4 + NPAR
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
          ITOKN = MSW( 3*I + 13 )
          STMAS(ISTMAS) = TOKN(ITOKN)
15     CONTINUE
16     CONTINUE
C----------------------------------------------------------------------
C--> �EPE�AEM HA�P��EH��
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 18
       IMSWV = 13 + 3*NTOKOB
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
       INPSUB = 15 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PAC��T�BAEM �HA�EH�E TOKOB �P� T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 1.
       ITOK = MSW(7)
       DGMAS(2) = TOKN( ITOK )
       DGMAS(3) = TOKN( ITOK+1)
       DGMAS(4) = TOKN( ITOK+2)
       DGMAS(5) = TOKN( ITOK+3)
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT12 = DGMAS(6)
       TOKT13 = DGMAS(7)
       TOKT23 = DGMAS(8)
       TOKT14 = DGMAS(9)
       TOKT24 = DGMAS(10)
       TOKT34 = DGMAS(11)
       G12 = DGMAS(12)
       G13 = DGMAS(13)
       G23 = DGMAS(14)
       G14 = DGMAS(15)
       G24 = DGMAS(16)
       G34 = DGMAS(17)
       RMSW(ITOKT0+1) = G12
       RMSW(ITOKT0+2) = G13
       RMSW(ITOKT0+3) = G23
       RMSW(ITOKT0+4) = G14
       RMSW(ITOKT0+5) = G24
       RMSW(ITOKT0+6) = G34
       RMSW(ITOKT0+7) = TOKT12
       RMSW(ITOKT0+8) = TOKT13
       RMSW(ITOKT0+9) = TOKT23
       RMSW(ITOKT0+10)= TOKT14
       RMSW(ITOKT0+11)= TOKT24
       RMSW(ITOKT0+12)= TOKT34
       MSW( ITOKT0+13 ) = IFIX(DGMAS(1))
       DO 148 I=1,5
       TOKN1( ITOK + 3 +I) = DGMAS(17+I)
148    CONTINUE
       GO TO 170
150    CONTINUE
C----------------------------------------------------------------------
C -->  �TEPA��� HE �EPBA�...
       IF(MSW(ITOKT0+13).EQ.0) GO TO 160
       STMAS( 1) = TN
       STMAS( 2) = H0
       ISTMAS = 2
C----------------------------------------------------------------------
C--> �EPE�AEM TOK�
       IF( NTOKOB .EQ. 0 ) GO TO 26
          DO 25 I= 1,NTOKOB
          ISTMAS = ISTMAS + 1
          ITOKN = MSW( 3*I + 13 )
          STMAS(ISTMAS) = TOKN1(ITOKN)
25     CONTINUE
26     CONTINUE
C----------------------------------------------------------------------
C--> �EPE�AEM HA�P��EH��
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 28
       IMSWV = 13 + 3*NTOKOB
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
       INPSUB = 15 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PAC��T�BAEM �HA�EH�� TOKOB �P� T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 2.
       DGMAS(2) = RMSW( ITOKTA )
       DGMAS(3) = RMSW( ITOKTB )
       DGMAS(4) = RMSW( ITOKTC )
       DGMAS(5) = RMSW( ITOKT0 )
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT12 = DGMAS(6)
       TOKT13 = DGMAS(7)
       TOKT23 = DGMAS(8)
       TOKT14 = DGMAS(9)
       TOKT24 = DGMAS(10)
       TOKT34 = DGMAS(11)
       G12 = DGMAS(12)
       G13 = DGMAS(13)
       G23 = DGMAS(14)
       G14 = DGMAS(15)
       G24 = DGMAS(16)
       G34 = DGMAS(17)
       RMSW(ITOKT0+1) = G12
       RMSW(ITOKT0+2) = G13
       RMSW(ITOKT0+3) = G23
       RMSW(ITOKT0+4) = G14
       RMSW(ITOKT0+5) = G24
       RMSW(ITOKT0+6) = G34
       RMSW(ITOKT0+7) = TOKT12
       RMSW(ITOKT0+8) = TOKT13
       RMSW(ITOKT0+9) = TOKT23
       RMSW(ITOKT0+10)= TOKT14
       RMSW(ITOKT0+11)= TOKT24
       RMSW(ITOKT0+12)= TOKT34
       ITOK = MSW(7)
       DO 158 I=1,5
       TOKN1( ITOK + 3 + I) = DGMAS(17 + I)
 158   CONTINUE
       GO TO 170
 160   CONTINUE
       G12 = RMSW( ITOKT0+1)
       G13 = RMSW( ITOKT0+2)
       G23 = RMSW( ITOKT0+3)
       G14 = RMSW( ITOKT0+4)
       G24 = RMSW( ITOKT0+5)
       G34 = RMSW( ITOKT0+6)
       TOKT12 = RMSW( ITOKT0+7)
       TOKT13 = RMSW( ITOKT0+8)
       TOKT23 = RMSW( ITOKT0+9)
       TOKT14 = RMSW( ITOKT0+10)
       TOKT24 = RMSW( ITOKT0+11)
       TOKT34 = RMSW( ITOKT0+12)
170    CONTINUE
C----------------------------------------------------------------------
C -->  �O��EMEHTH�� BK�A�......
C----------------------------------------------------------------------
       KA = MSW(9)
       KB = MSW(10)
       KC = MSW(11)
       K0 = MSW(12)
       KOD = 0
C--->
       CALL DEPOST( KA , KB , G12 , TOKT12 , YALL , JALL )
       CALL DEPOST( KA , KC , G13 , TOKT13 , YALL , JALL )
       CALL DEPOST( KB , KC , G23 , TOKT23 , YALL , JALL )
       CALL DEPOST( KA , K0 , G14 , TOKT14 , YALL , JALL )
       CALL DEPOST( KB , K0 , G24 , TOKT24 , YALL , JALL )
       CALL DEPOST( KC , K0 , G34 , TOKT34 , YALL , JALL )
C--->
       RETURN
C######################################################################
200    CONTINUE
C----------------------------------------------------------------------
C-->  ZAPROS = 2  --> PAC��TAT� TOK� ��� T=TN+1 � �A��CAT� B TOKN1
C----------------------------------------------------------------------
C  �PE�CKA�AHH�E TOK�.
       XA = RMSW( ITOKTA )
       XB = RMSW( ITOKTB )
       XC = RMSW( ITOKTC )
       X0 = RMSW( ITOKT0 )
C----------------------------------------------------------------------
C  �O���EHH�� TOK.
       KA = MSW(9 )
       KB = MSW(10)
       KC = MSW(11)
       K0 = MSW(12)
       VKA = 0.0
       IF(KA.GT.0)  VKA = VALLN1(KA)
       VKB = 0.0
       IF(KB.GT.0)  VKB = VALLN1(KB)
       VKC = 0.0
       IF(KC.GT.0)  VKC = VALLN1(KC)
       VK0 = 0.0
       IF(K0.GT.0)  VK0 = VALLN1(K0)
       ITOK = MSW(7)
       TOK12 = (VKA - VKB) * RMSW(ITOKT0+1) + RMSW(ITOKT0+7)
       TOK13 = (VKA - VKC) * RMSW(ITOKT0+2) + RMSW(ITOKT0+8)
       TOK23 = (VKB - VKC) * RMSW(ITOKT0+3) + RMSW(ITOKT0+9)
       TOK14 = (VKA - VK0) * RMSW(ITOKT0+4) + RMSW(ITOKT0+10)
       TOK24 = (VKB - VK0) * RMSW(ITOKT0+5) + RMSW(ITOKT0+11)
       TOK34 = (VKC - VK0) * RMSW(ITOKT0+6) + RMSW(ITOKT0+12)
       XANEW = TOK12 + TOK13 + TOK14
       XBNEW = TOK23 - TOK12 + TOK24
       XCNEW = TOK34 - TOK23 - TOK13
       X0NEW = TOK14 + TOK24 + TOK34
C----------------------------------------------------------------------
C  O�HOB�M CTAPOE �HA�EH�E.
       RMSW( ITOKTA ) = XANEW
       RMSW( ITOKTB ) = XBNEW
       RMSW( ITOKTC ) = XCNEW
       RMSW( ITOKT0 ) = X0NEW
       TOKN1(ITOK  ) = XANEW
       TOKN1(ITOK+1) = XBNEW
       TOKN1(ITOK+2) = XCNEW
       TOKN1(ITOK+3) = X0NEW
       IF(MSW(ITOKT0+13).EQ.0) RETURN
C-->  �O�PE��M...
       X1 = XA
       X2 = XANEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 30
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 31
30     EPS = ABS( X1 - X2 )
31     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) GO TO 32
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) GO TO 32
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
32     X1 = XB
       X2 = XBNEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 33
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 34
33     EPS = ABS( X1 - X2 )
34     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) GO TO 35
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) GO TO 35
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
35     X1 = XC
       X2 = XCNEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 36
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 37
36     EPS = ABS( X1 - X2 )
37     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) GO TO 38
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) GO TO 38
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
38     X1 = X0
       X2 = X0NEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 39
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 40
39     EPS = ABS( X1 - X2 )
40     CONTINUE
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