       SUBROUTINE FM0030(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT  MAPTA  1988 �.)------------
C   �O��PO�PAMMA: FM0030(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�E� ��HE�H�X ��HAM��ECK�X �BEH�EB:
C   ----------
C              +-----+-----------------------------------------------+
C              I     I                                   I  �EPE�A-  I
C              I T�� I       HA�MEHOBAH�E �BEHA          I  TO�HA�   I
C              I     I                                   I  ��HK���  I
C              I-----I-----------------------------------I-----------I
C              I     I  �PO�OP��OHA��HO-C�MM�P���EE      I           I
C              I 318 I       ( � - PE����TOP )           I     K     I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I  �PO�OP��OHA��H�� C�MMATOP        I           I
C              I 324 I       KBA�PATOB                   I           I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              *******************************************************
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0035)
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C                      ���HA:  T�� 318:==> +1(VAR)
C
C----------------------------------------------------------------------
C              ��EMEHT O��C�BAETC�:
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
C          W    -     �ACTOTA OCHOBHO� �APMOH�K� ( �� );
C          A0,A1,...- AM���T��� COOTBETB����X �APMOH�K C��HA�A;
C          FI1,FI2...-HA�. �A�� COOTBETB����X �APMOH�K C��HA�A;
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
C              �OPMAT RMSW T�� # 318, #324:
C              ----------------------------
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
C 13+NNAPR*3 I  4+NNAPR*3   I  FM  I  W  -   �ACTOTA OCH. �APMOH�K�(��);
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( NMKNOT,MASPRT(7) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 317
       IF ( NTIP .EQ. 1 ) GO TO 1
       IF ( NTIP .EQ. 7 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0030 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100),NTIP
C----------------------------------------------------------------------
C---> �PO�OP��OHA��HO C�MM�P���EE �BEHO   ( T��  # 318 )
C---> �PO�OP��OHA��H�� C�MMATOP KBA�PATOB ( T��  # 324 )
C----------------------------------------------------------------------
100   CONTINUE
C--> O�PE�E�EH�E KOHCTAHT
       NNAPR = 0
       IF( PAR0T(2) .GT. 0.0 ) NNAPR = IFIX ( PAR0T(2) )
       INGARM = 4 + 3*NNAPR
       NGARM  = 0
       IF( PAR0T(INGARM) .GT. 0.0 ) NGARM = IFIX ( PAR0T(INGARM) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 12 + 3*NNAPR + 2*NGARM
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 3 + 3*NNAPR + 2*NGARM
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NNAPR1 = (IFIX ( PAR0T(1) ) - 1 ) / 3
       IF ( NNAPR .EQ. NNAPR1 ) GO TO 3
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O���KA B FM0030 -->HE�PAB���HO �A�AHO KO���ECTBO BX
     =O�OB***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
3      CONTINUE
       NGARM1 = (IFIX( PAR0T(INGARM-1) ) - 1) / 2
       IF ( NGARM .EQ. NGARM1 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 503 ) NEL, TIP
503    FORMAT(/' *** O���KA B FM0030 -->HE�PAB���HO �A�AHO KO�-BO �APMOH
     =�K  ***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
4      CONTINUE
C----------------------------------------------------------------------
C---->  �A�OMH�M HOMEPA ���OB.
       N = NNAPR
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
       IF( NNAPR .EQ. 0 ) GO TO 50000
          DO 5 I = 1 , NNAPR
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          NYZN  = IFIX ( PAR0T(IPAR0T) )
          IF( NYZN .LT. 0 .OR. NYZN .GT. NMKNOT ) GO TO 9999
          N = NYZN
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          NYZK = IFIX ( PAR0T(IPAR0T) )
          IF( NYZK .LT. 0 .OR. NYZK .GT. NMKNOT ) GO TO 9999
          N = NYZK
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) =  PAR0T(IPAR0T)
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
       IF( I1 .LT. 1 ) GO TO 7
          DO 6 I = 1 , I1
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T)
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T) * 3.1416 / 180.
6      CONTINUE
60000  CONTINUE
7      CONTINUE
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 504 ) NEL, TIP
504    FORMAT(/' *** O���KA B FM0030 -->HE�PAB���HO �A�AH� BXO�H�E �AHH�
     =E   ***'/' *** HA BXO� ��EMEHTA ', I5, ' HOMEP T��A ', I5,' ***'/
     =         ' *** �O�AETC� HA�P��EH�E C HEC��ECTB����X ���OB
     =    *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
