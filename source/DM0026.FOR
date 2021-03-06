          SUBROUTINE DM0026 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA:  DM0026 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C    +-----+-----------------------------------+---------------------+
C    I     I                                   I    �EPE�ATO�HA�     I
C    I T�� I       HA�MEHOBAH�E �BEHA          I                     I
C    I     I                                   I       ��HK���       I
C    I-----I-----------------------------------I---------------------I
C    I     I   �PO�OP��OHA��HO-�HTE�P�P���EE   I    TAU1*S + 1       I
C    I 314 I         ( �� - PE����TOP )        I   -------------     I
C    I     I                                   I      TAU2*S         I
C    I-----I-----------------------------------I---------------------I
C    I     I �PO�OP��OHA��HO-����EPEH��P���EE  I    TAU1*S + 1       I
C    I 315 I         ( �� - PE����TOP )        I   -------------     I
C    I     I                                   I    TAU2*S + 1       I
C    I-----I-----------------------------------I---------------------I
C    I     I                                   I    1 - TAU*S        I
C    I 316 I       �A�OBPA�ATE��               I   -------------     I
C    I     I                                   I    1 + TAU*S        I
C    I-----I-----------------------------------I---------------------I
C    I     I                                   I       W**2          I
C    I 317 I       KO��E�ATE��HOE              I  -----------------  I
C    I     I                                   I  S**2+2*E*W*S+W**2  I
C    +-----+-----------------------------------+---------------------+
C                ******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C      � # T 314 � 0 �  #��  #CM  TAU1  TAU2  UMAX  U0  T0
C      � # T 315 � 0 �  #��  #CM  TAU   TAU2  UMAX 
C      � # T 316 � 0 �  #��  #CM  TAU   UMAX
C      � # T 317 � 0 �  #��  #CM   W    E     UMAX
C
C      ��E
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          TAU,TAU1,
C              TAU2 - �OCTO�HH�E BPEMEH�;
C              W    - �ACTOTA KO��E�AH��;
C              E    - KO������EHT �EM���POBAH��.
C          U0, T0   - ��������� �������� ����������� �� �=�0 
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
C               "�OCTO�HHA�" �ACT� MSW T��OB # 314,315,316,317:
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
C
C              �OPMAT RMSW T�� # 314,315:
C              --------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0022    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  UMAX - MAKC. B�X. C��HA�
C            I              I      I
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I  TAU1 - �OCTO�HHA� BPEMEH�;
C    16      I     7        I  FM  I  TAU2 - �OCTO�HHA� BPEMEH�;
C    17      I     8        I  FM  I  U0   - ��������� ��������;
C    18      I     9        I  FM  I  T0   - ����� ���������   ;
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 16
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 7
C
C----------------------------------------------------------------------
C
C
C                      �OPMAT RMSW T�� # 316:
C                      ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0022    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  UMAX - MAKC. B�X. C��HA�;
C            I              I      I
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I  TAU  - �OCTO�HHA� BPEMEH�;
C    16      I     7        I  FM  I          PE�EPB
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 16
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 7
C
C----------------------------------------------------------------------
C
C
C                    �OPMAT RMSW T�� # 317:
C                    ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0022    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  UMAX - MAKC. B�X. C��HA�;
C            I              I      I
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I   W   - �ACTOTA KO��E�AH�� (��);
C    16      I     7        I  FM  I   E   - KO������EHT �EM���POBAH��;
C    17      I     8        I  DM  I  TN   - BPEM� �P� T=TN
C    18      I     9        I  DM  I  TN-1 - BPEM� �P� T=TN-1
C    19      I    10        I  DM  I  YN   - B�XO�HO� C��HA� �P� T=TN
C    20      I    11        I  DM  I  YN-1 - B�XO�HO� C��HA� �P� T=TN-1
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 20
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 11
C
C----------------------------------------------------------------------
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
         DIMENSION X(3), Y(3), A0(1), A1(1), A2(1)
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
       NTIP = TIP - 313
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0026 - �A�POC HA O�C����BAH�E ���O�
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
       GO TO (1000,2000,3000,4000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   �PO�OP��OHA��HO-�HTE�P�P���EE ( T�� 314 )
C----------------------------------------------------------------------
       ITOK = MSW(7)
       IF( RMSW(18) .LT. TN ) GO TO 10001
       TOKN1(ITOK) = RMSW(17)
       GO TO 10002 
10001  TAU1 = RMSW(15)
       TAU2 = RMSW(16)
       NTOK = MSW(14)
       TOKBX = TOKN(NTOK)
       TOKBX1= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       TOKBIX = TOKN(ITOK) + TOKBX1 * (TAU1 + H0) / TAU2 -
     -               TOKBX * TAU1 / TAU2
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
10002  CONTINUE
       MSW(10) = 1
       RETURN
C######################################################################
2000   CONTINUE
C----------------------------------------------------------------------
C-->   �PO�OP��OHA��HO-����EPEH��P���EE �BEHO  ( T�� 315 )
C----------------------------------------------------------------------
       TAU1 = RMSW(15)
       TAU2 = RMSW(16)
       NTOK =  MSW(14)
       TOKBX = TOKN(NTOK)
       TOKBX1= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       ITOK=  MSW(7)
       EXPHT2 = EXP( - H0 / TAU2 )
       TOKBIX = TOKN(ITOK) * EXPHT2 + EXPHT2 * ( TOKBX1 - TOKBX )*
     *               TAU1 / TAU2 + ( 1.- EXPHT2 ) * TOKBX1
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
3000   CONTINUE
C----------------------------------------------------------------------
C-->   �A�OBPA�ATE�� ( T�� 316 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK =  MSW(14)
       TOKBX = TOKN(NTOK)
       TOKBX1= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       ITOK=  MSW(7)
       EXPHT = EXP( - H0 / TAU )
       TOKBIX = EXPHT * TOKN(ITOK) + ( 1.- 2.* EXPHT ) * TOKBX1 +
     +               EXPHT * TOKBX
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
4000   CONTINUE
C----------------------------------------------------------------------
C-->   KO��E�ATE��HOE �BEHO ( T�� 317 )
C----------------------------------------------------------------------
       W = RMSW(15)
       E = RMSW(16)
       NTOK =  MSW(14)
       ITOK =  MSW(7)
       TOKBX = TOKN1(NTOK)
C----------------------------------------------------------------------
C-->  �POBO��M KBA�PAT��H�� �HTEP�O����� ��� O�PE�E�EH�� �HA�EH��
C-->  B�XO�HO�O C��HA�A �P�  T = TN - H0
C----------------------------------------------------------------------
       TOKBIX = TOKN(ITOK)
       X(1) = RMSW(18)
       X(2) = RMSW(17)
       X(3) = TN
       Y(1) = RMSW(20)
       Y(2) = RMSW(19)
       Y(3) = TOKN(ITOK)
       IF( TN .LE. 1.5*H0 ) GO TO 4050
       XN = TN - H0
       CALL APPROX( 3, 1, X, Y, A2, A1, A0 )
       TOKBIX = A2(1) * XN**2 + A1(1) * XN + A0(1)
4050   CONTINUE
C--> �A���EM B TOKN1
       B = 1.
       IF( E .NE. 0.) B = EXP(-2.*E*W*H0)
       A = 2.* COS(W*H0)
       IF( E .NE. 0.) A = 2.* EXP(-E*W*H0) * COS( W*H0* SQRT(1-E**2) )
       TOKBIX = A * TOKN(ITOK) - B * TOKBIX + (1.- A + B) * TOKBX
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C--->  �A�OMH�M B  MSW  HOBOE �HA�EH�E
C----------------------------------------------------------------------
       IF( FITER. EQ. 1 ) GO TO 4100
       RMSW(18) = X(2)
       RMSW(17) = X(3)
       RMSW(20) = Y(2)
       RMSW(19) = Y(3)
4100   CONTINUE
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
       END
