       SUBROUTINE FM0020(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA: FM0020(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�E� ��HE�H�X ��HAM��ECK�X �BEH�EB:
C   ----------
C              +-----+-----------------------------------------------+
C              I     I                                   I  �EPE�A-  I
C              I T�� I       HA�MEHOBAH�E �BEHA          I  TO�HA�   I
C              I     I                                   I  ��HK���  I
C              I-----I-----------------------------------I-----------I
C              I     I  �PO�OP��OHA��HO-C�MM�P���EE      I           I
C              I 310 I       ( � - PE����TOP )           I     K     I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I         �HTE�P�P���EE             I           I
C              I 311 I       ( � - PE����TOP )           I   1/(TS)  I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I                                   I           I
C              I 312 I         �HEP��OHHOE               I 1/(TS+1)  I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I         ����EPEH��P���EE          I           I
C              I 313 I       ( � - PE����TOP )           I TS/(TS+1) I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I                                   I I*        I
C              I 319 I  �EHEPATOP ���OO�PA�HO�O C��HA�A  I I  *      I
C              I     I                                   I I    *    I
C              I-----I-----------------------------------I-----------I
C              *******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0024, LM0007 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C                              T�� 310:==> +1(VAR)
C                      ���HA:  T�� 311:==>  7
C                              T�� 312:==>  7
C                              T�� 313:==>  7
C                              T�� 319:==>  7
C
C----------------------------------------------------------------------
C              ��EMEHT O��C�BAETC�:
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
C          W    -     �ACTOTA OCHOBHO� �APMOH�K� ( 1/C );
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
C      � # T 319 � 0 � UMAX UMIN TZ TP TI
C
C      ��E
C          UMAX  -MAKC�MA��HOE HA�P��EH�E  C��HA�A
C          UMIN  -M�H�MA��HOE  HA�P��EH�E  C��HA�A
C          TZ    - HA�A��H��   C�B�� �O �A�E
C          TP    - �EP�O�  C��HA�A ���OO�PA�HO�O
C          TI    - ���TE��HOCT� C��HA�A
C
C
C----------------------------------------------------------------------
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
C              �OPMAT RMSW T�� # 310:
C              ----------------------
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
C            I              I      I         ��� O��CAH�� �A�. C��HA�A
C            I              I      I         (C ��ETOM �OCT. COCTAB�.);
C 13+NBXOD*4 I  4+NBXOD*4   I  FM  I  W  -   �ACTOTA OCH. �APMOH�K�(1/C)
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
C    16      I     7        I  FM  I          PE�EPB
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 309
       IF ( NTIP .EQ. 10 ) GO TO 300
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0020 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100,200,200,200),NTIP
C----------------------------------------------------------------------
C---> �PO�OP��OHA��HO C�MM�P���EE �BEHO ( T��  # 310 )
C----------------------------------------------------------------------
100   CONTINUE
C--> O�PE�E�EH�E KOHCTAHT
       NBXOD = 0
       IF( PAR0T(2) .GT. 0.0 ) NBXOD = IFIX ( PAR0T(2) )
       INGARM = 4 + 3*NBXOD
       NGARM  = 0
       IF( PAR0T(INGARM) .GT. 0.0 ) NGARM = IFIX ( PAR0T(INGARM) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 12 + 4*NBXOD + 2*NGARM
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 3 + 4*NBXOD + 2*NGARM
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NBXOD1 = (IFIX ( PAR0T(1) ) - 1 ) / 3
       IF ( NBXOD .EQ. NBXOD1 ) GO TO 3
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O���KA B FM0020 -->HE�PAB���HO �A�AHO KO���ECTBO BX
     =O�OB***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
3      CONTINUE
       NGARM1 = (IFIX( PAR0T(INGARM-1) ) - 1) / 2
       IF ( NGARM .EQ. NGARM1 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 503 ) NEL, TIP
503    FORMAT(/' *** O���KA B FM0020 -->HE�PAB���HO �A�AHO KO�-BO �APMOH
     =�K  ***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
4      CONTINUE
C----------------------------------------------------------------------
C---->  �A�OMH�M HOMEPA TOKOB, �B�����XC� BXO�H�M� C��HA�AM�  ---------
       N = NBXOD
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
       IF( NBXOD .EQ. 0 ) GO TO 50000
          DO 5 I = 1 , NBXOD
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          NELBX = IFIX ( PAR0T(IPAR0T) )
          IF( NELBX .GT. NEL ) GO TO 9999
          N = NELBX
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) =  PAR0T(IPAR0T)
          IRMSW = IRMSW + 1
5      CONTINUE
50000  CONTINUE
C----------------------------------------------------------------------
C---->  �A�OMH�M AM���T��� � �A�� �APMOH�K ----------------------------
       IPAR0T = IPAR0T + 2
       IRMSW = IRMSW + 1
       N = NGARM
       RMSW(IRMSW) = RN
       IF( NGARM .EQ. 0 ) GO TO 60000
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       RMSW(IRMSW) = PAR0T(IPAR0T)
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       RMSW(IRMSW) = PAR0T(IPAR0T)
       I1 = NGARM-1
       IF( I1 .LT. 1 ) GO TO 60000
          DO 6 I = 1 , I1
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T)
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T) * 3.1416 / 180.
6      CONTINUE
60000  CONTINUE
       RETURN
C======================================================================
C----------------------------------------------------------------------
C---> �HTE�P�P���EE     �BEHO ( T��  # 311 )
C---> �HEP��OHHOE       �BEHO ( T��  # 312 )
C---> ����EPEH��P���EE  �BEHO ( T��  # 313 )
C----------------------------------------------------------------------
200    CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1001
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 16
       RETURN
C######################################################################
1001   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 7
       DO 7 I=1,LM
7      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> �A�OMH�M �AHH�E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
C----------------------------------------------------------------------
C---> �POBEP�M, �PAB���HO �� BBE�EH� �AHH�E?
       IF( PAR0T(3) .GT. 0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** O���KA B FM0020 -->HE�PAB���HO �A�AHA �OCTO�HHA� BP
     =EMEH� ***'/' ***  ��EMEHT ', I5, ' HOMEP T��A ', I5,' ***'/
     =           ' *** B���C�EH�� �PEKPA�A�TC�              ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
C######################################################################
C----------------------------------------------------------------------
C  �EHEPATOP ���OO�PA�HO�O C��HA�A   ( T�� # 319 )
C----------------------------------------------------------------------
300    CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 2001
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 16
       RETURN
C######################################################################
2001   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 7
       DO 9 I=1,LM
9      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> �A�OMH�M �AHH�E  ------------------------------------------------
       DO 10 I = 1,5
       RMSW(2+I) = PAR0T(I)
 10    CONTINUE
C----------------------------------------------------------------------
C---> �POBEP�M, �PAB���HO �� BBE�EH� �AHH�E?
       IF( PAR0T(4) .GT. 0..AND.PAR0T(5).GT.0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 507 ) NEL, TIP
507    FORMAT(/' *** O���KA B FM0020 -->HE�PAB���HO �A�AHA �OCTO�HHA� BP
     =EMEH� ***'/' ***  ��EMEHT ', I5, ' HOMEP T��A ', I5,' ***'/
     =           ' *** B���C�EH�� �PEKPA�A�TC�              ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 504 ) NEL, TIP,NELBX
504    FORMAT(/' *** O���KA B FM0020 -->HE�PAB���HO �A�AH� BXO�H�E �AHH�
     =E   ***'/' *** HA BXO� ��EMEHTA ', I5, ' HOMEP T��A ', I5,' ***',
     =         ' *** �O�AETC� TOK ��EMEHTA, �ME��E�O �O����� �OP��KOB��
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
