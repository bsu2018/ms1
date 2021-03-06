         SUBROUTINE DM0025 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA:  DM0025 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C        O�PA�AT�BAET MAKPOMO�E�� HE��HE�H�X ��HAM��ECK�X �BEH�EB:
C
C   +-----+-------------------------------+------------------------+
C   I     I                               I         �EPE�A-        I
C   I T�� I       HA�MEHOBAH�E �BEHA      I         TO�HA�         I
C   I     I                               I         ��HK���        I
C   +-----+-------------------------------+------------------------+
C   I     I                               I             I Y    *   I
C   I     I                               I             I     *    I
C   I     I        HE��HE�HOE �BEHO       I             I*  *      I
C   I 320 I  (�EPE�ATO�HA� ��HK��� E�O    I      ------*+------->  I
C   I     I  �A�AHHO HA�OPOM TO�EK)       I         *   I       X  I
C   I     I                               I    *  *     I          I
C   I     I                               I             I          I
C   +-----+-------------------------------+------------------------+
C   I     I                               I            !Y          I
C   I     I                               I      !--<--*----!----- I
C   I     I                               I      !     !�2  !      I
C   I     I                               I      !     !    !    X I
C   I 321 I       PE�E�HOE �BEHO          I -----*-----I----*----- I
C   I     I                               I      !X1   !    !X2    I
C   I     I                               I      !     !    !      I
C   I     I                               I------!-----*-->-!      I
C   I     I                               I            !�1         I
C   +-----+-----------------------------------+--------------------+
C   I     I                               I !X         *      *    I
C   I     I                               I !    *                 I
C   I     I                               I !*                  T  I
C   I     I                               I *--------------------- I
C   I 322 I  �BEHO "��CTOE �A�A���BAH�E"  I !�                     I
C   I     I                               I !              *      *I
C   I     I                               I !TAU     *             I
C   I     I                               I !<->!*              T  I
C   I     I                               I *---*----------------- I
C   +-----+-------------------------------+------------------------+
C   I     I                               I                        I
C   I 323 I       �MHO��TE��              I � = K * X1 * X2 *...   I
C   I     I                               I                        I
C   +-----+-------------------------------+------------------------+
C   I     I                               I                        I
C   I 325 I    KOPEH� KBA�PATH��          I � = SQRT(X)            I
C   I     I                               I                        I
C   +-----+-------------------------------+------------------------+
C                ******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C      � # T 320 � 0 �  #��  #CM  P
C              MAS < NT  X1  Y1  X2  Y2 ... >
C
C      ��E
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          P      -   �P��HAK B��A A��POKC�MA���:
C                     P = 1 - K�CO�HO-��HE�HA� ;
C                     P = 2 - KBA�PAT��HA� ;
C          NT     -   KO���ECTBO  TO�EK �EPE�ATO�HO� ��HK���
C                     �A�AHHO� TA����HO;
C          X1 Y1  -   COOTBETCTBEHHO A���CCA � OP��HATA TO�K�
C          � T.�.     �EPE�ATO�HO� ��HK���
C
C          ------------------------------------------------------------
C
C         � # T 321 � 0 �  #��  #CM  X1  X2  �1  �2
C
C         ��E:
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          X1,X2  -   A���CC� TO�EK �EPECE�EH�� �EPE�ATO�HO� ��HK���
C                     C OC�� A���CC.
C          �1,�2  -   OP��HAT� TO�EK �EPECE�EH�� �EPE�ATO�HO� ��HK���
C                     C OC�� OP��HAT.
C          ------------------------------------------------------------
C
C         � # T 322 � 0 �  #��  #CM  TAU  HA�.�C�.
C
C         ��E:
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          TAU    -   �OCTO�HHA� BPEMEH� �A�EP�K�;
C          HA�.�C�. - �HA�EH�E B�XO�HO�O C��HA�A �P� T=[0,TAU];
C          ------------------------------------------------------------
C
C         � # T 323 � 0 �  K
C              MAS < NBXOD  #��1 #CM1  #��2 #CM2   ... >;
C
C         ��E
C          K        - KO������EHT �PO�OP��OHA��HOCT� �PO��BE�EH��
C                     BXO�H�X TOKOB;
C          NBXOD  -   KO���ECTBO TOKOB ��EMEHTOB CXEM� , KOTOP�E
C                     �C�O�����TC� B KA�ECTBE BXO�H�X C��HA�OB;
C          #��I,#CMI- HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          ------------------------------------------------------------
C
C         � # T 325 � 0 �  #��  #CM  X1  X2  �1  �2
C
C         ��E:
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
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
C          "�OCTO�HHA�" �ACT� MSW T��OB # 320,321,322,323,325:
C           --------------------------------------------------
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
C                �OPMAT RMSW T�� # 320:
C                ----------------------
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
C    11      I     2        I  DM  I  NMIN - HOMEP ����A��E� MEH��E�
C            I              I      I         TO�K� ��� T=TN+
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I  P   - �P��HAK B��A A�POKC�MA���
C    16      I     7        I  FM  I  NT  - KO���ECTBO  TO�EK
C            I              I      I        �EPE�ATO�HO� ��HK���
C    17      I     8        I  FM  I  X1  - A���CCA 1-� TO�K�
C            I              I      I        �EPE�ATO�HO� ��HK���
C   ....     I    .....     I      I  .....
C    16+NT   I     7+NT     I  FM  I  X(NT) - A���CCA NT-O�  TO�K�
C            I              I      I          �EPE�ATO�HO� ��HK���
C    16+NT+1 I     7+NT+1   I  FM  I  Y1    - OP��HATA 1-O� TO�K�
C            I              I      I          �EPE�ATO�HO� ��HK���
C    ......  I    ......    I      I .......
C    16+2NT  I     7+2NT    I  FM  I  Y(NT) - OP��HATA NT-O� TO�K�
C            I              I      I          �EPE�ATO�HO� ��HK���
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 16 + 2 * NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 7 + 2 * NT
C----------------------------------------------------------------------
C
C                      �OPMAT RMSW T�� # 321
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
C    11      I     2        I  DM  I         PE�EPB
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I  X1  - (CM. P�C.)
C    16      I     7        I  FM  I  X2  - (CM. P�C.)
C    17      I     8        I  FM  I  Y1  - (CM. P�C.)
C    18      I     9        I  FM  I  Y2  - (CM. P�C.)
C    19      I     10       I  DM  I  PAR - �APAMETP, O�PE�E������
C            I              I      I        COCTO�H�E B�X. C��HA�A
C            I              I      I        �P� T=TN.
C            I              I      I        PAR=0 - IB�X=�1
C            I              I      I        PAR=1 - IB�X=�2
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 19
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 10
C----------------------------------------------------------------------
C
C
C                      �OPMAT RMSW T�� # 322
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
C    11      I     2        I  DM  I         PE�EPB
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I  FM  I  TAU - �OCTO�HHA� BPEMEH� �A�EP�K�;
C    16      I     7        I  FM  I  HA�.�C�.- HA�A��H�E �C�OB��;
C    17      I     8        I  FM  I  NT  - KO�-BO TO�EK BXO�HO�O C��H.
C    18      I     9        I  DM  I  IPAR- �KA�ATE��;
C    19      I     10       I  DM  I  T1  - BPEM�;
C    20      I     11       I  DM  I  T2  - BPEM�;
C   ....     I    .....     I .... I .............................
C   18+NT    I    9+NT      I  DM  I  TNT - BPEM�;
C   18+NT+1  I    9+NT+1    I  DM  I  IBX1- BXO�HO� C��HA� �P� T=T1
C   18+NT+2  I    9+NT+2    I  DM  I  IBX2- BXO�HO� C��HA� �P� T=T2
C   ....     I    .....     I .... I .............................
C   18+2*NT  I    9+2*NT    I  DM  I  IBXNT-BXO�HO� C��HA� �P� T=TNT
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 18 + 2*NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 9 + 2*NT
C----------------------------------------------------------------------
C
C
C              �OPMAT RMSW T�� # 323:
C              ----------------------
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
C    11      I     2        I  FM  I  NBXOD-KO���ECTBO TOKOB, �C�O��-
C            I              I      I        ��EM�X KAK BXO�H�E C��HA��;
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C .......    I   .......    I  ... I  ..........................
C  9+I*3     I     I*3      I  FM  I  #��I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C 12+3*NBXOD I  3+3*NBXOD   I  FM  I  K    - KO������EHT �PO�OP��OHA��-
C            I              I      I         HOCT�;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 12 + 3*NBXOD
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 3 + 3*NBXOD
C----------------------------------------------------------------------
C
C
C
C                      �OPMAT RMSW T�� # 325
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
C    11      I     2        I  DM  I         PE�EPB
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C    15      I     6        I      I         PE�EPB
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 15
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 6
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
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR
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
         INTEGER FITER, TIP, ZAPROS, P
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       NTIP = TIP - 319
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 9991
       IF ( NTIP .EQ. 6                   ) GO TO 9991
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0025 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9991   CONTINUE
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
C######################################################################
       ITOK = MSW(7)
       GO TO (1000,2000,3000,4000,6000,6000),NTIP
1000   CONTINUE
C----------------------------------------------------------------------
C-->   HE��HE�HOE �BEHO ( T�� 320 )
C----------------------------------------------------------------------
       NMIN = MSW(11)
       P    = MSW(15)
       NT   = MSW(16)
       NTM1 = NT - 1
       SIGNAL = 0.
C----------------------------------------------------------------------
C--> HE��EM BXO�HO� C��HA�
       SIGBX = TOKN1( MSW(14) )
       IF( SIGBX .LT. RMSW(17) ) GO TO 1800
       IF( SIGBX .GT. RMSW(16+NT) ) GO TO 1700
C--> CPABH�M C �PE������M.
       IF( SIGBX .LE. RMSW(16+NMIN) ) GO TO 1200
C--> C��HA� �BE�����C�.
1100   CONTINUE
       IF( SIGBX .LE. RMSW(17+NMIN) ) GO TO 1500
       NMIN = NMIN + 1
       GO TO 1100
1200   CONTINUE
C--> C��HA� �MEH����C�
       NMIN = NMIN - 1
       IF( SIGBX .GE. RMSW(16+NMIN) ) GO TO 1500
       GO TO 1200
C--> �O�A�� B CEPE��H� TA�����. �HTEP�O��P�EM...
1500   CONTINUE
       X1 = RMSW(16+NMIN)
       X2 = RMSW(17+NMIN)
       Y1 = RMSW(16+NT+NMIN)
       Y2 = RMSW(17+NT+NMIN)
       IF( P .EQ. 2 ) GO TO 1600
1550   CONTINUE
C--> ��HE�HA� �HTEP�O�����.
       SIGNAL = ( (Y1-Y2)*SIGBX + Y2*X1 - Y1*X2 ) / ( X1 - X2 )
       MSW(11) = NMIN
       GO TO 9900
1600   CONTINUE
C--> KBA�PAT��HA� �HTEP�O�����.
       N3 = NMIN - 1
       IF ( N3 .EQ. 0 ) N3 = NMIN + 2
       X3 = RMSW(16+N3)
       Y3 = RMSW(16+NT+N3)
       A = ( (Y3-Y1)/(X3-X1) - (Y2-Y1)/(X2-X1) ) / (X3-X2)
       B = (Y2-Y1)/(X2-X1) - A * (X2+X1)
       C = Y1 - A * X1**2 - B * X1
       SIGNAL = A * SIGBX**2 + B * SIGBX + C
       MSW(11) = NMIN
       GO TO 9900
1700   CONTINUE
C--> B���� �A BEPXH�� �PAH���...
       X1 = RMSW(15+NT)
       X2 = RMSW(16+NT)
       Y1 = RMSW(15+2*NT)
       Y2 = RMSW(16+2*NT)
       NMIN = NT
       GO TO 1550
1800   CONTINUE
C--> B���� �A H��H�� �PAH���...
       X1 = RMSW(17)
       X2 = RMSW(18)
       Y1 = RMSW(17+NT)
       Y2 = RMSW(18+NT)
       NMIN = 1
       GO TO 1550
C######################################################################
2000   CONTINUE
C----------------------------------------------------------------------
C-->   PE�E�HOE �BEHO ( T�� 321 )
C----------------------------------------------------------------------
       NPAR = MSW(19)
       X1  = RMSW(15)
       X2  = RMSW(16)
       Y1  = RMSW(17)
       Y2  = RMSW(18)
C----------------------------------------------------------------------
C--> HE��EM BXO�HO� C��HA�
       SIGBX = TOKN1( MSW(14) )
       IF( SIGBX .LT. X1 ) GO TO 2500
       IF( SIGBX .GT. X2 ) GO TO 2600
C--> �TO ���O HA �PE�����EM �A�E?
       IF( NPAR .EQ. 1 )   GO TO 2600
C--> C��HA� PABEH  Y1 .
2500   CONTINUE
       SIGNAL = Y1
       MSW(19) = 0
       GO TO 9900
C--> C��HA� PABEH  Y2
2600   CONTINUE
       SIGNAL = Y2
       MSW(19) = 1
C======================================================================
9900   CONTINUE
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
3000   CONTINUE
C----------------------------------------------------------------------
C-->  �BEHO T��A "��CTOE �A�A���BAH�E"
C----------------------------------------------------------------------
       TN1 = TN + H0
       TAU = RMSW(15)
       HU  = RMSW(16)
       NT  =  MSW(17)
       IPAR=  MSW(18)
C----------------------------------------------------------------------
C--> HA��EM BXO�HO� C��HA�.
       SIGBX = TOKN (MSW(14))
       SIGBX1= TOKN1(MSW(14))
       IF( FITER .EQ. 1 ) GO TO 3010
C----------------------------------------------------------------------
C--> �A�OMH�M �PE������� BXO�HO� TOK
       RMSW(IPAR) = TN
       RMSW(IPAR+NT) = SIGBX
3010   CONTINUE
C----------------------------------------------------------------------
C--> �TO CO BPEMEHEM?
       IF( TN1 .GE. TAU ) GO TO 3020
C--> TEK��EE BPEM� MEH��E BPEMEH� �A�EP�K�.
       TOKN1(ITOK) = HU
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       IF( FITER .EQ. 1 ) RETURN
       IF( (IPAR-18) .EQ. NT ) IPAR = 18
       MSW(18) = IPAR + 1
       RETURN
C======================================================================
3020   CONTINUE
C----------------------------------------------------------------------
C--> TEK��EE BPEM� �O���E BPEMEH� �A�EP�K�.
       IF( TAU .GT. H0 ) GO TO 3030
C--> �A�EP�KA MEH��E �A�A.
       X1 = TN
       X2 = TN1
       Y1 = SIGBX
       Y2 = SIGBX1
       XN = TN1 - TAU
       GO TO 3100
3030   CONTINUE
C----------------------------------------------------------------------
C--> �A�EP�KA �O���E �A�A.
C--> ��EM BXO�HO� C��HA�.
       XN = TN1 - TAU
       ISIG = IPAR
       NT1 = 0
3040   CONTINUE
       NT1 = NT1 + 1
       IF( NT1 .GT. NT ) GO TO 3200
       ISIG = ISIG - 1
       IF( ISIG .EQ. 18 ) ISIG = 18 + NT
       ISIG1= ISIG - 1
       IF( ISIG1.EQ. 18 ) ISIG1= 18 + NT
       IF( XN .GT. RMSW(ISIG) .OR. XN .LT. RMSW(ISIG1) ) GO TO 3040
       X1 = RMSW(ISIG1)
       X2 = RMSW(ISIG)
       Y1 = RMSW(ISIG1+NT)
       Y2 = RMSW(ISIG+NT)
C----------------------------------------------------------------------
C--> ��HE�HA� �HTEP�O����� B�XO�HO�O C��HA�A.
3100   CONTINUE
       TOKN1(ITOK) = ( (Y1-Y2)*XN + Y2*X1 - Y1*X2 ) / (X1 - X2)
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       IF( FITER .EQ. 1 ) RETURN
       IF( (IPAR-18) .EQ. NT ) IPAR = 18
       MSW(18) = IPAR + 1
       RETURN
C======================================================================
3200   CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,501) MSW(1), MSW(2)
501     FORMAT(' *** KO���ECTBO �A�OM�HAEM�X TO�EK HE �OCTATO�HO ***'/
     =         ' *** ��� �O���EH�� �A�AHHO� �A�EP�K� �O BPEMEH�  ***'/
     =         ' *** ��EMEHT # ',I5,' T�� MO�E�� # ',I5, ' ***'//)
         CALL SUBERR(0)
        RETURN
C######################################################################
4000    CONTINUE
C----------------------------------------------------------------------
C-->  �MHO��TE��
C----------------------------------------------------------------------
        NBXOD = MSW(11)
        AK = RMSW(12+3*NBXOD)
C-->  HAXO��M B�XO�HO� C��HA�, �EPEMHO��B BXO�H�E.
        SIGNAL = AK
           DO 4100 I=1,NBXOD
           SIGNAL = SIGNAL * TOKN1(MSW(11+3*I))
4100       CONTINUE
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
C######################################################################
6000   CONTINUE
C----------------------------------------------------------------------
C-->   KOPEH� KBA�PATH�� (T�� 325 )
C----------------------------------------------------------------------
C--> HE��EM BXO�HO� C��HA�
       SIGBX = TOKN1( MSW(14) )
       SIGNAL = 0.0
       IF( SIGBX .GT. 0.0 ) SIGNAL = SQRT(SIGBX)
C----------------------------------------------------------------------
C--> �A���EM B TOKN1
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  �A��KC�P. KOHE� B���C�EH��       ( FQ ��A� )
       MSW(10) = 1
       RETURN
       END
