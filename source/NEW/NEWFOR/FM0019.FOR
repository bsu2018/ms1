       SUBROUTINE FM0019(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA: FM0019(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:   HACTPO�KA MAKPOMO�E�� T��A # 301
C   ----------   C�CTEMA �M����CHO-�A�OBO�O ��PAB�EH�� ( C��� )
C                BEHT���H�M �PEO�PA�OBATE�EM.
C                ***********************************************
C                �.TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0023, LM0006 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C                      ( ���HA :==> +1(VAR) )
C
C
C              ��EMEHT O��C�BAETC�:
C
C      � # T 301 � 0 �
C              MAS < NTOKOB  #��1 #CM1  #��2 #CM2 ... >
C              MAS < NHA�P   ��E�N1 ��E�K1  ��E�N2 ��E�K2 ... >
C              PSUB #P < .........>;
C
C              �EPE� A��APAT PSUB �A�A�TC� �AKOH� ��PAB�EH�� ;
C
C      ��E
C          NTOKOB -   KO���ECTBO TOKOB ��EMEHTOB CXEM� , KOTOP�E
C                     �C�O�����TC� �P� PAC�ETE C��HA�OB ��PAB�EH��(C��);
C          #��I , #CMI     - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                            �C�O����EMO�O �P� PAC�ETE C��;
C          NHA�P           - KO���ECTBO HA�P��EH�� ME��� ���AM� CXEM� ,
C                            �C�O����EM�X �P� PAC�ETE C��;
C          ��E�NI , ��E�KI - HOMEPA ���OB , HA�P��EH�� ME��� KOTOP�M�
C                            �C�O�����TC� �P� PAC�ETE C��;
C          #P              - HOMEP �/� �O���OBATE�� ( PSUB#P ) ,
C                            B KOTOPO� �PO��BO��TC� PAC�ET C��;
C
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
C                  3. KO�-BO �APAMETPOB B �/� PSUB HE KOHTPO��P.
C                  4. H��E �P�BO��TC� �OPMAT� RMSW:
C
C              �OPMAT RMSW T�� # 301:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0019    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NTOKOB -KO���ECTBO TOKOB, �C�O��-
C            I              I      I          ��EM�X �P� PAC�ETE C��;
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C  9+I*3     I     I*3      I  FM  I  #��I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  NHA�P -KO���ECTBO HA�P��EH��,
C            I              I      I         �C�. ��� PAC�ETA C��;
C13+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  ��E�N1 -��E� HA�A�A HA�P��EH��;
C14+NTOKOB*3 I  5+NTOKOB*3  I  FM  I  ��E�K1 -��E� KOH�A  HA�P��EH��;
C .........  I ............ I .... I  ...........................
C            I              I      I
C13+NTOKOB*3+I 3+NTOKOB*3+  I      I  NPSUB - HOMEP �/� �O���OBATE��
C NHA�P*2    I  NHA�P*2     I  FM  I         ( PSUB ) B KOTOPO� �PO��-
C            I              I      I         BO��TC� PAC�ET C��;
C (...)+1    I  (...)+1     I  FM  I  NPAR - KO���ECTBO �APAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  �APAMETP� PSUB , �A�AHH�E
C�O(...)+NPARI �O(...)+NPAR I      I  �O���OBATE�EM ���C BOCEM�
C  + 8       I  + 8         I      I  ��EMEHTOB (HA BC�K�� C���A�)
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 22 + 3*NTOKOB + 2*NNAPR + NPAR 
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 13 + 3*NTOKOB + 2*NNAPR + NPAR 
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 301 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0019 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C--> O�PE�E�EH�E KOHCTAHT
       NTOKOB = 0
       IF( PAR0T(2) .GT. 0.0 ) NTOKOB = IFIX ( PAR0T(2) )
       INNAPR = 4 + 2*NTOKOB
       NNAPR  = 0
       IF( PAR0T(INNAPR) .GT. 0.0 ) NNAPR = IFIX ( PAR0T(INNAPR) )
       INPSUB  = INNAPR + 2*NNAPR + 1
       NPSUB   = -IFIX ( PAR0T(INPSUB) )
       NPAR   = IFIX ( PAR0T(INPSUB + 1) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 22 + 3*NTOKOB + 2*NNAPR + NPAR 
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 13 + 3*NTOKOB + 2*NNAPR + NPAR 
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NTKOB1 = (IFIX ( PAR0T(1) ) - 1 ) / 2
       IF ( NTOKOB .EQ. NTKOB1 ) GO TO 3
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O���KA B FM0019 -->HE�PAB���HO �A�AHO KO���ECTBO TO
     =KOB ***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
3      CONTINUE
       NNAPR1 = (IFIX( PAR0T(INNAPR-1) ) - 1 ) / 2
       IF ( NNAPR .EQ. NNAPR1 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 503 ) NEL, TIP
503    FORMAT(/' *** O���KA B FM0019 -->HE�PAB���HO �A�AHO KO�-BO HA�P��
     =EH��***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
4      CONTINUE
C----------------------------------------------------------------------
C---->  �A�OMH�M HOMEPA TOKOB  ----------------------------------------
       N = NTOKOB
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
       IF( NTOKOB .EQ. 0 ) GO TO 51
          DO 5 I = 1 , NTOKOB
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IRMSW = IRMSW + 1
5      CONTINUE
51     CONTINUE
C----------------------------------------------------------------------
C---->  �A�OMH�M HOMEPA HA�P��EH��  -----------------------------------
       IPAR0T = IPAR0T + 2
       IRMSW = IRMSW + 1
       N = NNAPR
       RMSW(IRMSW) = RN
       IF( NNAPR .EQ. 0 ) GO TO 61
          DO 6 I = 1 , NNAPR
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
6      CONTINUE
61     CONTINUE
C----------------------------------------------------------------------
C---->  �A�OMH�M �APAMETP�, �EPE�ABAEM�E B �/� �O���OBATE��  ----------
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       N = NPSUB
       RMSW(IRMSW) = RN
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       N = NPAR
       RMSW(IRMSW) = RN
          DO 7 I = 1 , NPAR 
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW+7) = PAR0T(IPAR0T)
7      CONTINUE
C######################################################################
       RETURN
       END
