         SUBROUTINE DM0024 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA:  DM0024 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C  I 310 I       ( � - PE����TOP )       I     K     I YN1=SUM(K*XN1) I
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I         �HTE�P�P���EE         I           I YN1 = YN +     I
C  I 311 I       ( � - PE����TOP )       I   1/(TS)  I H0 * XN1 / TAU I
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I                               I           I YN1=YN*EXP(-H0/I
C  I 312 I         �HEP��OHHOE           I 1/(TS+1)  ITAU)+(1-EXP(-H0/I
C  I     I                               I           ITAU))*XN1       I
C  I-----I-------------------------------I-----------I----------------I
C  I     I    �EHEPATOP ���O-O�PA�HO�O   I I*        I I = UMAX -     I
C  I 319 I       ���HA�A                 I I  *      I (UMAX-UMIN)*T/ I
C  I     I                               I I    *    I  TI            I
C  I-----I-------------------------------I-----------I----------------I
C  I     I         ����EPEH��P���EE      I           I YN1=YN*EXP(-H0/I
C  I 313 I       ( � - PE����TOP )       I TS/(TS+1) ITAU) + EXP(-H0/ I
C  I     I                               I           ITAU * (XN1-XN)  I
C  +-----+-------------------------------+-----------+----------------+
C                ******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C      � # T 310 � 0 �
C              MAS < NBXOD  #��1 #CM1 K1  #��2 #CM2 K2  ... >
C              MAS < NGARM  W  A0  A1  FI1  A2  FI2... >
C
C      ��E
C          NBXOD  -   KO���ECTBO TOKOB ��EMEHTOB CXEM� , KOTOP�E
C                     �C�O�����TC� B KA�ECTBE BXO�H�X C��HA�OB;
C          #��I,#CMI- HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          KI       - KO������EHT �PO�OP��OHA��HOCT� TOKA I-TO�O
C                     ��EMEHTA;
C          NGARM  -   KO���ECTBO ��EMEHTOB �APMOH��ECKO�O P��A
C                     O��C�BA��E�O �A�AHH�� C��HA�, �O�ABAEM��
C                     HA BXO� PE����TOPA (C ��ETOM �OCT. COCTAB�.);
C          W    -     �ACTOTA OCHOBHO� �APMOH�K� ( �� );
C          A0,A1,...- AM���T��� COOTBETB����X �APMOH�K C��HA�A;
C          FI1,FI2...-HA�. �A�� COOTBETB����X �APMOH�K C��HA�A;
C      ----------------------------------------------------------------
C
C      � # T 311 � 0 �  #��  #CM  TAU
C      � # T 312 � 0 �  #��  #CM  TAU
C      � # T 313 � 0 �  #��  #CM  TAU
C
C      ��E
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          TAU    -   �OCTO�HHA� BPEMEH�;
C----------------------------------------------------------------------
C       � # T 319 UMAX UMIN TZ TP TI
C       ��E
C            UMAX  - MAKC�M.HA�P��EH�E
C            UMIN  - M�H�MA.HA�P��EH�E
C            TZ    =  C�B��
C             TP   -  �EP�O�
C              TI  -  ���TE��HOCT�
C      ----------------------------------------------------------------
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
C                �OPMAT RMSW T�� # 310:
C                ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NBXOD-KO���ECTBO TOKOB, �C�O��-
C            I              I      I        ��EM�X KAK BXO�H�E C��HA��;
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  FM  I  K1   - KO������EHT �PO�OP��OHA��-
C            I              I      I         HOCT� TOKA #CM1 ��-TA #��1;
C    15      I     6        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C  8+I*4     I  -1+I*4      I  FM  I  #��I -      -"-
C  9+I*4     I     I*4      I  FM  I  #CMI -      -"-
C 10+I*4     I   1+I*4      I  FM  I  KI   -      -"-
C 11+I*4     I   2+I*4      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NBXOD*4  I  3+NBXOD*4   I  FM  I  NGARM -KO���ECTBO �APMOH�K, �C�.
C            I              I      I         ��� O��CAH�� �A�. C��HA�A;
C            I              I      I         (C ��ETOM �OCT. COCTAB�.);
C 13+NBXOD*4 I  4+NBXOD*4   I  FM  I  W  -   �ACTOTA OCH. �APMOH�K�(��);
C 14+NBXOD*4 I  5+NBXOD*4   I  FM  I  A0  -  AM���T��A �OCT. COCTAB��-
C            I              I      I         ��E� �A�. C��HA�A;
C 15+NBXOD*4 I  6+NBXOD*4   I  FM  I  A1  -  AM���T��A 1-O� �APMOH�K�
C            I              I      I         �A�AHHO�O C��HA�A;
C 16+NBXOD*4 I  7+NBXOD*4   I  FM  I  FI1 -  HA�. �A�A 1-O� �APMOH�K�;
C 17+NBXOD*4 I  8+NBXOD*4   I  FM  I  A2  -  AM���T��A 2-O� �APMOH�K�;
C 18+NBXOD*4 I  9+NBXOD*4   I  FM  I  FI2 -  HA�. �A�A 2-O� �APMOH�K�;
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 12 + 4*NBXOD + 2*NGARM
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 3 + 4*NBXOD + 2*NGARM
C
C----------------------------------------------------------------------
C
C              �OPMAT RMSW T�� # 311,312,313:
C              ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I         PE�EPB
C            I              I      I
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I  TAU  - �OCTO�HHA� BPEMEH�;
C    16      I     7        I  FM  I         PE�EPB
C----------------------------------------------------------------------
C
C              �OPMAT RMSW T�� # 319
C              ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I         PE�EPB
C            I              I      I
C    12      I     3        I  FM  I  UMAX - MAK. HA�P��EH�E
C    13      I     4        I  FM  I  UMIN - M�H. HA�P��EH�E
C    14      I     5        I  LM  I  TZ   - C�B�� �O �A�E
C    15      I     6        I  FM  I  TP   - �EP�O� C��HA�A
C    16      I     7        I  FM  I  TI   - ���TE��HOCT� C��HA�A
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 16
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 7
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
       NTIP = TIP - 309
       IF ( NTIP . EQ . 10 ) GO TO 9900
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 9900
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
       IF ( NTIP .EQ . 10) GO TO 5000
       GO TO (1000,2000,3000,4000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   �PO�OP��OHA��HO C�MM�P���EE �BEHO ( T�� 310 )
C----------------------------------------------------------------------
       NBXOD = MSW(11)
       NGARM = MSW(12 + 4*NBXOD)
       SIGNAL = 0.
       IF( NGARM .LT. 1 ) GO TO 1110
C----------------------------------------------------------------------
C--> �P��AB�M �HA�EH�E �A�AHHO�O BXO�HO�O C��HA�A
       W = RMSW(13 + 4*NBXOD)
       A0 = RMSW(14 + 4*NBXOD)
       SIGNAL = A0
       I1 = 2*(NGARM - 1)
       IF( I1 .EQ. 0 ) GO TO 1110
          DO 1100 I=1,I1,2
          NRMSW = 14 + I + 4*NBXOD
          AI = RMSW(NRMSW)
          FII= RMSW(NRMSW+1)
          N = I / 2 + 1
          SIGNAL = SIGNAL + AI * SIN(N*W*(TN+H0) + FII)
1100   CONTINUE
1110   CONTINUE
C----------------------------------------------------------------------
C--> �POC�MM�P�EM BXO�H�E C��HA��
       IF(NBXOD .EQ. 0 ) GO TO 1210
          DO 1200 I=1,NBXOD
          NMSW = 10 + I*4
          AKI  = RMSW(NMSW)
          TOKI = TOKN1( MSW(NMSW + 1) )
          SIGNAL = SIGNAL + AKI*TOKI
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
C######################################################################
2000   CONTINUE
C----------------------------------------------------------------------
C-->   �HTE�P�P���EE �BEHO ( T�� 311 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK=  MSW(14)
       TOKBX= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = TOKN(ITOK) + H0 * TOKBX / TAU
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
3000   CONTINUE
C----------------------------------------------------------------------
C-->   �HEP��OHHOE �BEHO ( T�� 312 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK=  MSW(14)
       TOKBX= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = TOKN(ITOK) * EXP(-H0/TAU) +
     +               ( 1 - EXP(-H0/TAU) ) * TOKBX
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
4000   CONTINUE
C----------------------------------------------------------------------
C-->   ����EPEH��P���EE �BEHO ( T�� 313 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK=  MSW(14)
       TOKBX = TOKN1(NTOK) - TOKN(NTOK)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = TOKN(ITOK) * EXP(-H0/TAU) +
     +               EXP(-H0/TAU) * TOKBX
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
5000   CONTINUE
C----------------------------------------------------------------------
C-->   �EHEPATOP ���O-O�PA�HO�O C��HA�A ( T�� # 319 )
C----------------------------------------------------------------------
       UMAX  = RMSW(12)
       UMIN = RMSW(13)
       TZ = RMSW(14)
       TP = RMSW(15)
       TI = RMSW(16)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = 0.0
       T1 = TN +H0 - TZ
       IF ( T1 .LT.0.) GO TO 5005
       T2 = AMOD(T1,TP)
       IF(T2.LE.TI) TOKN1(ITOK)=UMAX -(UMAX-UMIN)*T2/TI
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
5005   MSW(10) = 1
       RETURN
C######################################################################
       END
