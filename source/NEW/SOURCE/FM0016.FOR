       SUBROUTINE FM0016(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT  �EKA�P�  1987 �.)-----------
C   �O��PO�PAMMA: FM0016(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�E� ���POB�X �O���ECK�X ��EMEHTOB:
C   ----------
C               +-----+--------------------------------------+
C               I     I                                      I
C               I T�� I    HA�MEHOBAH�E ��EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 333 I ���POBO�  RS-TP���EP                 I
C               +-----+--------------------------------------+
C               I 334 I ���POBO�   D-TP���EP                 I
C               +-----+--------------------------------------+
C               I 335 I ���POBO�   T-TP���EP                 I
C               +-----+--------------------------------------+
C               I 360 I KOM�APATOP(1)                        I
C               +-----+--------------------------------------+
C               I 361 I KOM�APATOP(2)                        I
C               +-----+--------------------------------------+
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0020, LM0008 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C
C                      ���HA:  ��� BCEX T��OB :==>+1<VAR>
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C         � # T 33X � 0 �   #��1 #CM1  #��2 #CM2 ;
C
C         ��E
C          #��I,#CMI- HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C         -------------------------------------------------------------
C
C         � # T 360 � 0 �   #��1 #CM1  #��2 #CM2 ;
C
C         ��E
C          #��I,#CMI- HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C         -------------------------------------------------------------
C
C         � # T 361 � 0 �   �1  �2  �3  �4 ;
C
C         ��E
C            �I     - HOMEPA ���OB, HA�P��EH�� ME��� KOTOP�M�
C                     �C�O�����TC� B KA�ECTBE BXO�H�X C��HA�OB;
C         -------------------------------------------------------------
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
C              �OPMAT RMSW T�� # 33X � 360:
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I        PE�EPB
C            I              I      I
C    11      I     2        I  DM  I        PE�EPB
C            I              I      I
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C    15      I     6        I  FM  I  #��2 -      -"-
C    16      I     7        I  FM  I  #CM2 -      -"-
C    17      I     8        I  LM  I  ATOKM2 -    -"-
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 17
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 8
C----------------------------------------------------------------------
C
C
C              �OPMAT RMSW T�� # 335:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I        PE�EPB
C            I              I      I
C    11      I     2        I  DM  I        PE�EPB
C            I              I      I
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 14
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 5
C----------------------------------------------------------------------
C
C
C                   �OPMAT RMSW T�� # 361:
C                   ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I        PE�EPB
C            I              I      I
C    11      I     2        I  DM  I        PE�EPB
C            I              I      I
C    12      I     3        I  FM  I  �1   - HOMEP ���A;
C    13      I     4        I  FM  I  �2   - HOMEP ���A;
C    14      I     5        I  FM  I  �3   - HOMEP ���A;
C    15      I     6        I  FM  I  �4   - HOMEP ���A;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 15
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 6
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 332
       GO TO ( 1 , 1 , 2  ), NTIP
       IF (  TIP .EQ. 360 ) GO TO 3
       IF (  TIP .EQ. 361 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0016 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->           ���POBO� RS-TP���EP
C --->           ���POBO�  D-TP���EP
C----------------------------------------------------------------------
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 8
       DO 10 I=1,LM
10     RMSW(I) = 0.0
C---> BBE�EM HOMEPA BXO�H�X TOKOB.
       NEL1 = IFIX ( PAR0T(1) )
       NEL2 = IFIX ( PAR0T(3) )
       NSM1 = IFIX ( PAR0T(2) )
       NSM2 = IFIX ( PAR0T(4) )
C---> �A�OMH�M HOMEPA BXO�H�X TOKOB.
       N = NEL1
       IF( N .GE. NEL ) GO TO 9999
       RMSW(3) = RN
       N = NEL2
       IF( N .GE. NEL ) GO TO 9999
       RMSW(6) = RN
       N = NSM1
       RMSW(4) = RN
       N = NSM2
       RMSW(7) = RN
       RETURN
C######################################################################
2      CONTINUE
C----------------------------------------------------------------------
C --->           ���POBO�  T-TP���EP
C----------------------------------------------------------------------
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 5
       DO 20 I=1,LM
20     RMSW(I) = 0.0
C---> BBE�EM HOMEPA BXO�H�X TOKOB.
       NEL1 = IFIX ( PAR0T(1) )
       NSM1 = IFIX ( PAR0T(2) )
C---> �A�OMH�M HOMEPA BXO�H�X TOKOB.
       N = NEL1
       IF( N .GE. NEL ) GO TO 9999
       RMSW(3) = RN
       N = NSM1
       RMSW(4) = RN
       RETURN
C######################################################################
3      CONTINUE
C----------------------------------------------------------------------
C --->           KOM�APATOP(1)
C----------------------------------------------------------------------
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 8
       DO 30 I=1,LM
30     RMSW(I) = 0.0
C---> BBE�EM HOMEPA BXO�H�X TOKOB.
       NEL1 = IFIX ( PAR0T(1) )
       NEL2 = IFIX ( PAR0T(3) )
       NSM1 = IFIX ( PAR0T(2) )
       NSM2 = IFIX ( PAR0T(4) )
C---> �A�OMH�M HOMEPA BXO�H�X TOKOB.
       N = NEL1
       RMSW(3) = RN
       N = NSM1
       RMSW(4) = RN
       N = NEL2
       RMSW(6) = RN
       N = NSM2
       RMSW(7) = RN
       RETURN
C######################################################################
4      CONTINUE
C----------------------------------------------------------------------
C --->           KOM�APATOP(2)
C----------------------------------------------------------------------
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 6
       DO 40 I=1,LM
40     RMSW(I) = 0.0
C---> BBE�EM HOMEPA BXO�H�X ���OB.
       NY1 = IFIX ( PAR0T(1) )
       NY2 = IFIX ( PAR0T(2) )
       NY3 = IFIX ( PAR0T(3) )
       NY4 = IFIX ( PAR0T(4) )
C---> �A�OMH�M HOMEPA BXO�H�X ���OB.
       N = NY1
       RMSW(3) = RN
       N = NY2
       RMSW(4) = RN
       N = NY3
       RMSW(5) = RN
       N = NY4
       RMSW(6) = RN
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) NEL, TIP,N
555    FORMAT(/' *** O���KA B FM0016 -->HE�PAB���HO �A�AH� BXO�H�E �AHH�
     =E   ***'/' *** HA BXO� ��EMEHTA ', I5, ' HOMEP T��A ', I5,' ***',
     =         ' *** �O�AETC� TOK ��EMEHTA, �ME��E�O �O����� �OP��KOB��
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
       END
