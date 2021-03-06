       SUBROUTINE FM0038(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT  ﭢ���   1992 �.)-----------
C   �O��PO�PAMMA: FM0038(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�E� ॣ���� ��� �ࠢ����� ����:
C   ----------
C   +-----+-------------------------------+---+--------------------+
C   I     I                                   I     �ਬ�砭��     I
C   I T�� I       HA�MEHOBAH�E �BEHA          I                    I
C   I     I                                   I                    I
C   +-----+-------------------------------+---+--------------------+
C   I     I                                   I                    I
C   I     I                                   I ��� ��ᯥਬ���   I
C   I 340 I  ������� ᯥ樠�쭮 ���         I �� ������� �ࠢ��-I
C   I     I  �ࠢ����� ���� ��ਮ���� ��    I ��� .              I
C   I     I   �ந������ ������� ,�������  I                    I
C   I     I   ⠡��筮 .                      I                    I
C   +-----+-----------------------------------+--------------------+
C                �P��HAK TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0042, LM0007 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C
C                      ���HA:  T�� 340:==>+1<VAR>
C----------------------------------------------------------------------
C              ��EMEHT O��C�BAETC�:
C
C      � # T 320 � 0 �  #��  #CM  P   MAX  H0  T0
C              MAS < NT  X1  Y1  X2  Y2 ... >
C
C      ��E
C          #�� ,#CM - HOMEP ��EMEHTA � HOMEP TOKA ��EMEHTA ,
C                     �C�O����EMO�O B KA�ECTBE BXO�HO�O C��HA�A;
C          P      -   ��ਭ� ��ਤ�� �ࠢ�����;
C          MAX    -   ���ᨬ���� �ࠢ�塞� ⮪
C          H0     -   ��������� ���祭�� ᨣ����
C          T0     -   �६� ����祭�� ᨣ����
C          NT     -   KO���ECTBO  TO�EK �EPE�ATO�HO� ��HK���
C                     �A�AHHO� TA����HO;
C          X1 Y1  -   COOTBETCTBEHHO ������ �६��� � ���祭��
C          � T.�.     �������� �ਢ�� ��室���� ⮪� .
C          ------------------------------------------------------------
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
C                      �OPMAT RMSW T�� # 340
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
C    15      I     6        I  FM  I  P   - ��ਭ� ��ਤ�� �ࠢ�����
C    16      I     7        I  FM  I  NT  - KO���ECTBO  TO�EK
C            I              I      I        �EPE�ATO�HO� ��HK���
C    17      I     8        I  FM  I  X1  - 1-� ������ �६���
C            I              I      I        �������� �ਢ�� ���. ⮪�
C   ....     I    .....     I      I  .....
C    16+NT   I     7+NT     I  FM  I  X(NT) - ������� �६���
C            I              I      I          �������� �ਢ�� ���. ⮪�
C    16+NT+1 I     7+NT+1   I  FM  I  Y1    -  1-O� TO�K�
C            I              I      I         �������� �ਢ�� ���. ⮪�
C    ......  I    ......    I      I .......
C    16+2NT  I     7+2NT    I  FM  I  Y(NT) - NT-O� TO�K�
C            I              I      I        �������� �ਢ�� ���. ⮪�
CC   17+2NT  I     8+2NT    I  FM  I    ���ᨬ���� ⮪
C    18+2NT  I     9 + 2NT  I  FM  I    �訡�� ॣ㫨஢����  DELTA
C    19+2NT  I     10 + 2NT I  FM  I    ������ �ࠢ�����    SIGOLD
C    20+2NT  I     11 + 2NT I  FM  I    ��砫쭮� ���祭�� ᨣ����
C    21+2NT  I     12 + 2NT I  FM  I    �६� ����祭��
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 19 + 2 * NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> =10 + 2 * NT
C----------------------------------------------------------------------
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
       NTIP= TIP - 339
C      IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
       IF ( NTIP .EQ. 1                   ) GO TO 1
500    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0021 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C      GO TO (100,200,300,400,500,600),NTIP
C######################################################################
100   CONTINUE
C----------------------------------------------------------------------
C---> ������� �ࠢ����� ���� ��ਮ���� ( ��� #340)
C----------------------------------------------------------------------
C--> O�PE�E�EH�E KOHCTAHT
       NT = IFIX ( PAR0T(8))
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 21 + 2 * NT
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 12+ 2 * NT
       DO 102 I=1,LM
102    RMSW(I) = 0.0
       N = 1
       RMSW(2) = RN
C---> �POBEP�M  �PAB���HO �� BBE�EH� �AHH�E?
       NT1 = (IFIX ( PAR0T(7) ) - 1 ) / 2
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
       RMSW (6) = PAR0T(3)
       RMSW(8 +2*NT) = PAR0T(4)
       RMSW(11+2*NT) = PAR0T(5)
       RMSW(12+2*NT) = PAR0T(6)
C---->  �A�OMH�M ��C�O TO�EK ��室��� XAPAKTEP�CT�K� ---------
       N = NT
       RMSW(7) = RN
       IF( NT.EQ. 0 ) GO TO 105
C---->  �A�OMH�M KOOP��HAT� TO�EK ��室��� XAPAKTEP�CT�K� ---------
          DO 104 I = 1 , NT
          RMSW(7+I)   = PAR0T( 7 + 2*I )
          RMSW(7+NT+I)= PAR0T( 8 + 2*I )
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
C######################################################################
       END
