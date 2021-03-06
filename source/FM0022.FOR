        SUBROUTINE FM0022(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   OKT��P� 1987 �.)-----------
C   �O��PO�PAMMA: FM0022(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�E� ��HE�H�X ��HAM��ECK�X �BEH�EB:
C   ----------
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
C              *******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0026, LM0007 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C                              T�� 314:==> 7
C                      ���HA:  T�� 315:==> 7
C                              T�� 316:==> 7
C                              T�� 317:==>11
C
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C      � # T 314 � 0 �  #��  #CM  TAU1  TAU2  UMAX U0 T0
C      � # T 315 � 0 �  #��  #CM  TAU1  TAU2  UMAX
C      � # T 316 � 0 �  #��  #CM  TAU   UMAX
C      � # T 317 � 0 �  #��  #CM   W    E     UMAX
C
C      ��E
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          TAU,TAU1,
C              TAU2 - �OCTO�HH�E BPEMEH�;
C              W    - �ACTOTA KO��E�AH��;
C              E    - KO������EHT �EM���POBAH��;
C              UMAX - MAKC�MA��H�� B�XO�HO� C��HA�.
C              U0, T0 - ������ U0 �� ������� �0   
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
C----------------------------------------------------------------------
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
C    11      I     2        I  FM  I  UMAX - MAKC�M. B�XO�HO� C��HA�.
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
C  O��A� ���HA MSW --> LM<BC�> = 18
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 9
C
C----------------------------------------------------------------------
C
C
C              �OPMAT RMSW T�� # 316:
C              ----------------------
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
C    11      I     2        I  FM  I  UMAX - MAKC. B�XO�HO� C��HA�;
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
C    11      I     2        I  FM  I  UMAX - MAKC. B�XO�HOE HA�P��EH�E;
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 313
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0022 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100,100,200,300),NTIP
C----------------------------------------------------------------------
C---> �PO�OP��OHA��HO-�HTE�P�P���EE    �BEHO ( T��  # 314 )
C---> �PO�OP��OHA��HO-����EPEH��P���EE �BEHO ( T��  # 315 )
C----------------------------------------------------------------------
100   CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1001
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 18
       RETURN
C######################################################################
1001   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 9
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> �A�OMH�M �AHH�E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(7) = PAR0T(4)
       RMSW(2) = PAR0T(5)
       RMSW(8) = PAR0T(6)
       RMSW(9) = PAR0T(7)
C----------------------------------------------------------------------
C---> �POBEP�M, �PAB���HO �� BBE�EH� �AHH�E?
       IF( PAR0T(3) .GT. 0. .AND. PAR0T(4) .GT. 0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** O���KA B FM0022 -->HE�PAB���HO �A�AH� �OCTO�HH�E BP
     =EMEH� ***'/' ***  ��EMEHT ', I5, ' HOMEP T��A ', I5,' ***'/
     =           ' *** B���C�EH�� �PEKPA�A�TC�              ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
C######################################################################
C----------------------------------------------------------------------
C---> �A�OBPA�ATE�� ( T��  # 316 )
C----------------------------------------------------------------------
200   CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 2001
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 16
       RETURN
C######################################################################
2001   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 7
       DO 3 I=1,LM
3      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> �A�OMH�M �AHH�E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(2) = PAR0T(4)
C----------------------------------------------------------------------
C---> �POBEP�M, �PAB���HO �� BBE�EH� �AHH�E?
       IF( PAR0T(3) .GT. 0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
C----------------------------------------------------------------------
       RETURN
C######################################################################
C----------------------------------------------------------------------
C---> KO��E�ATE��HOE   �BEHO ( T��  # 317 )
C----------------------------------------------------------------------
300   CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 3001
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 20
       RETURN
C######################################################################
3001   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 11
       DO 4 I=1,LM
4      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> �A�OMH�M �AHH�E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(7) = PAR0T(4)
       RMSW(2) = PAR0T(5)
C----------------------------------------------------------------------
C---> �POBEP�M, �PAB���HO �� BBE�EH� �AHH�E?
       IF( PAR0T(3) .GT. 0. .AND.
     =     PAR0T(4) .GE. 0. .AND.  PAR0T(4) .LT. 1.) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O���KA B FM0022 -->HE�PAB���HO �A�AH� �AHH�E ***'/
     =         ' ***  ��EMEHT ', I5, ' HOMEP T��A ', I5,' ***'/
     =         ' *** B���C�EH�� �PEKPA�A�TC�              ***')
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 504 ) NEL, TIP,NELBX
504    FORMAT(/' *** O���KA B FM0022 -->HE�PAB���HO �A�AH� BXO�H�E �AHH�
     =E   ***'/' *** HA BXO� ��EMEHTA ', I5, ' HOMEP T��A ', I5,' ***',
     =         ' *** �O�AETC� TOK ��EMEHTA, �ME��E�O �O����� �OP��KOB��
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
