       SUBROUTINE FM0021(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   OKT��P� 1987 �.)-----------
C   �O��PO�PAMMA: FM0021(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�E� HE��HE�H�X ��HAM��ECK�X �BEH�EB:
C   ----------
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
C   +-----+-------------------------------+------------------------+
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
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0025, LM0007 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C
C                      ���HA:  T�� 320:==>+1<VAR>
C                      ���HA:  T�� 321:==> 10
C                      ���HA:  T�� 322:==>+1<VAR>
C                      ���HA:  T�� 323:==>+1<VAR>
C                      ���HA:  T�� 325:==> 6
C----------------------------------------------------------------------
C              ��EMEHT O��C�BAETC�:
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
C         � # T 325 � 0 �  #��  #CM
C
C         ��E:
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C----------------------------------------------------------------------
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E��
C   -----
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW �P� FGMSW = 0;
C                  LM - ���HA BCE�O MSW O��CAH��      �P� FGMSW = 1;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C
C                      �OPMAT RMSW T�� # 320
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
C
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
C            I              I      I        PAR=0 - IB�X=Y1
C            I              I      I        PAR=1 - IB�X=Y2
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 319
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
       IF ( NTIP .EQ. 6                   ) GO TO 1
500    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0021 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100,200,300,400,500,600),NTIP
C######################################################################
100   CONTINUE
C----------------------------------------------------------------------
C---> HE��HE�HOE �BEHO ( T��  # 320 )
C----------------------------------------------------------------------
C--> O�PE�E�EH�E KOHCTAHT
       NT = IFIX ( PAR0T(5) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 16 + 2*NT
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 7 + 2*NT
       DO 102 I=1,LM
102    RMSW(I) = 0.0
       N = 1
       RMSW(2) = RN
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NT1 = (IFIX ( PAR0T(4) ) - 1 ) / 2
       IF ( NT .EQ. NT1 ) GO TO 103
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O���KA B FM0021 -->HE�PAB���HO �A�AHO KO���ECTBO TO
     =�EK ***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
103    CONTINUE
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  �A�OMH�M #�� , #CM  �  P
       N = IFIX( PAR0T(1) )
       RMSW(3)=RN
       N = IFIX( PAR0T(2) )
       RMSW(4)=RN
       N = IFIX ( PAR0T(3) )
       RMSW (6) = RN
C---->  �A�OMH�M ��C�O TO�EK �EPE�ATO�HO� XAPAKTEP�CT�K� ---------
       N = NT
       RMSW(7) = RN
       IF( NT.EQ. 0 ) GO TO 105
C---->  �A�OMH�M KOOP��HAT� TO�EK �EPE�ATO�HO� XAPAKTEP�CT�K� ---------
          DO 104 I = 1 , NT
          RMSW(7+I)   = PAR0T( 4 + 2*I )
          RMSW(7+NT+I)= PAR0T( 5 + 2*I )
104    CONTINUE
105    CONTINUE
C----> �POBEP�M,�PAB��HO �� BBE�EH� �AHH�E ?
       DO 106 I=2,NT
       IF ( RMSW(6+I) .LE. RMSW(7+I) ) GO TO 106
C----------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** O���KA B FM0021-->OP��HAT� �A�AH� HE B BO�PACA��EM
     =�OP��KE ***'/' *** HOMEP ��EMEHTA',I5,' HOMEP T��A',I5,' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
106    CONTINUE
       RETURN
C######################################################################
200   CONTINUE
C----------------------------------------------------------------------
C---> PE�E�HOE �BEHO ( T��  # 321 )
C----------------------------------------------------------------------
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 10
       DO 212 I=1,LM
212    RMSW(I) = 0.0
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       X1 =  PAR0T(3)
       X2 =  PAR0T(4)
       Y1 =  PAR0T(5)
       Y2 =  PAR0T(6)
       IF ( X1 .LT. X2  .AND. Y1 .LT. Y2 ) GO TO 213
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 512 ) NEL, TIP
512    FORMAT(/' *** O���KA B FM0021 -->HE�PAB���HO �A�AH� �APAMETP�
     =***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
213    CONTINUE
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  �A�OMH�M #�� , #CM , X1 , X2 , Y1 , Y2.
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = X1
       RMSW(7) = X2
       RMSW(8) = Y1
       RMSW(9) = Y2
       RETURN
C######################################################################
300    CONTINUE
C----------------------------------------------------------------------
C--> �BEHO "��CTOE �A�A���BAH�E" ( T�� # 322 )
C----------------------------------------------------------------------
C-->  �P�H�T� � �POBEP�T� �AHH�E.
       TAU = PAR0T(3)
       IF ( TAU .GE. 0. ) GO TO 314
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 513 ) NEL, TIP
513   FORMAT(/' *** O���KA B FM0021 -->HE�PAB���HO �A�AHA BPEM.�A�EP�KA
     =***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
314    CONTINUE
       NT = 2 * ( IFIX( TAU / H0 ) + 1 )
       IF ( NT .LE. 100 ) GO TO 315
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 514 ) NEL, TIP
514   FORMAT(/' *** O���KA B FM0021 -->C���KOM �O���A� BPEMEH. �A�EP�KA
     =***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
315    CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 3000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 18 + 2 * NT
       RETURN
C######################################################################
3000   CONTINUE
       LM = 9 + 2 * NT
       DO 316 I=1,LM
316    RMSW(I) = 0.0
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  �A�OMH�M #�� , #CM , TAU , HU
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(7) = PAR0T(4)
       N = NT
       RMSW(8) = RN
       N = 19
       RMSW(9) = RN
       RETURN
C######################################################################
400    CONTINUE
C----------------------------------------------------------------------
C-->  �MHO��TE�� ( T�� # 323 )
C----------------------------------------------------------------------
C--> O�PE�E�EH�E KOHCTAHT
       NBXOD = IFIX ( PAR0T(3) )
       IF ( FGMSW .EQ. 0 ) GO TO 4000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 12 + 3*NBXOD
       RETURN
C######################################################################
4000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 3 + 3*NBXOD
       DO 420 I=1,LM
420    RMSW(I) = 0.0
       RMSW(3+3*NBXOD) = PAR0T(1)
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NBXOD1 = ( IFIX( PAR0T(2)) - 1 ) / 2
       IF ( NBXOD .EQ. NBXOD1 ) GO TO 422
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
422    CONTINUE
C---> �A�OMH�M HOMEPA BXO�H�X TOKOB.
       N = NBXOD
       RMSW(2) = RN
       IPAR0T = 3
       IRMSW = 2
          DO 425 I = 1 , NBXOD
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IRMSW = IRMSW + 1
425    CONTINUE
       RETURN
C######################################################################
600   CONTINUE
C----------------------------------------------------------------------
C---> KOPEH� KBA�PATH�� (T��  # 325 )
C----------------------------------------------------------------------
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 6
       DO 612 I=1,LM
612    RMSW(I) = 0.0
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  �A�OMH�M #�� , #CM
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) NEL, TIP,NELBX
555    FORMAT(/' *** O���KA B FM0021 -->HE�PAB���HO �A�AH� BXO�H�E �AHH�
     =E   ***'/' *** HA BXO� ��EMEHTA ', I5, ' HOMEP T��A ', I5,' ***',
     =         ' *** �O�AETC� TOK ��EMEHTA, �ME��E�O �O����� �OP��KOB��
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
