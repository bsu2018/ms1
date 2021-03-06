       SUBROUTINE FM0025(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT  �EKA�P�  1987 �.)-----------
C   �O��PO�PAMMA: FM0025(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�E� ���POB�X �O���ECK�X ��EMEHTOB:
C   ----------
C               +-----+--------------------------------------+
C               I     I                                      I
C               I T�� I    HA�MEHOBAH�E ��EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 330 I ���POBO� �O���ECK�� ��EMEHT   "�"    I
C               +-----+--------------------------------------+
C               I 331 I ���POBO� �O���ECK�� ��EMEHT   "���"  I
C               +-----+--------------------------------------+
C               I 332 I ���POBO� �O���ECK�� ��EMEHT"�CK�.���"I
C               +-----+--------------------------------------+
C
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0029, LM0007 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C
C                      ���HA:  ��� BCEX T��OB :==>+1<VAR>
C----------------------------------------------------------------------
C              ��EMEHT� O��C�BA�TC�:
C
C         � # T 33X � 0 �
C              MAS < NBXOD  #��1 #CM1  #��2 #CM2   ... >;
C
C         ��E
C          NBXOD  -   KO���ECTBO TOKOB ��EMEHTOB CXEM� , KOTOP�E
C                     �C�O�����TC� B KA�ECTBE BXO�H�X C��HA�OB;
C          #��I,#CMI- HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
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
C              �OPMAT RMSW T�� # 33X:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0025    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I         PE�EPB
C            I              I      I
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
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 11 + 3*NBXOD
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 2 + 3*NBXOD
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
       NTIP= TIP - 329
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 3 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0025 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->           ���POBO� �O���ECK�� ��EMEHT   "�"
C --->           ���POBO� �O���ECK�� ��EMEHT   "���"
C --->           ���POBO� �O���ECK�� ��EMEHT"�CK�.���"
C----------------------------------------------------------------------
C--> O�PE�E�EH�E KOHCTAHT
       NBXOD = IFIX ( PAR0T(2) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 11 + 3*NBXOD
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 2 + 3*NBXOD
       DO 20 I=1,LM
20     RMSW(I) = 0.0
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NBXOD1 = ( IFIX( PAR0T(1)) - 1 ) / 2
       IF ( NBXOD .EQ. NBXOD1 ) GO TO 22
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O���KA B FM0025 -->HE�PAB���HO �A�AHO KO���ECTBO TO
     =�EK ***'/' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
22     CONTINUE
C---> �A�OMH�M HOMEPA BXO�H�X TOKOB.
       N = NBXOD
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
          DO 25 I = 1 , NBXOD
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          IF( N .GE. NEL ) GO TO 9999
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IRMSW = IRMSW + 1
25     CONTINUE
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) NEL, TIP,N
555    FORMAT(/' *** O���KA B FM0025 -->HE�PAB���HO �A�AH� BXO�H�E �AHH�
     =E   ***'/' *** HA BXO� ��EMEHTA ', I5, ' HOMEP T��A ', I5,' ***',
     =         ' *** �O�AETC� TOK ��EMEHTA, �ME��E�O �O����� �OP��KOB��
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
