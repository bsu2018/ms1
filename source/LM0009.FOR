       SUBROUTINE LM0009( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MAPTA 1988 �.)------------
C   �O��PO�PAMMA: LM0009( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�� T��A:
C   ----------  #  18 - TPEX�A�H��  �CTO�H�K TOKA,
C                       ��PAB��EM�� �/� �O���OBATE��;
C               *******************************************************
C
C   METO�:        �O�CK B MSW_ALL O��CAH�� ��EMEHTOB , A�PECOB �A�AHH�X
C   -----         TOKOB ��EMEHTOB B MACC�AX TOKN/TOKN1.
C                 B C���AE HAXO��EH�� -> �O�OPM�POBAH�E MSW_O��CAH��;
C                 B �TOM C���AE KOD = 0, �HA�E = 1;
C
C   �APAMETP�:    MSW(RMSW) - "OTPE�OK" MSW_MO�E�� T�� # 301;
C   ----------    ADR - A�PEC B MSW_ALL MSW_O��CAH�� �TO� MO�E��;
C                 MSWALL(RMSWAL) - BEC� C��COK MSW C�OB;
C                 KOD - = 0 - C�OPM�POBA�� MSW,
C                       = 1 - �O�. O���K�;
C                      ( HA NWTR COO��EH�E B�BO��TC� );
C   �O��PO�PAMM�: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C              �OPMAT RMSW T�� # 18:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0029    I      I
C------------I--------------I------I-----------------------------------
C     9      I     A        I      I HOMEP ���A A
C    10      I     B        I      I HOMEP ���A B
C    11      I     C        I      I HOMEP ���A C
C    12      I     0        I      I HOMEP ���A 0
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  NTOKOB -KO���ECTBO TOKOB, �C�O��-
C            I              I      I          ��EM�X �P� PAC�ETE TOKA
C            I              I      I          �CTO�H�KA;
C    14      I     2        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    15      I     3        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    16      I     4        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C 11+I*3     I  -1+I*3      I  FM  I  #��I -      -"-
C 12+I*3     I     I*3      I  FM  I  #CMI -      -"-
C 13+I*3     I   1+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C14+NTOKOB*3 I  2+NTOKOB*3  I  FM  I  NHA�P -KO���ECTBO HA�P��EH��,
C            I              I      I         �C�. ��� PAC�ETA TOKA
C            I              I      I         �CTO�H��KA;
C15+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  ��E�N1 -��E� HA�A�A HA�P��EH��;
C16+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  ��E�K1 -��E� KOH�A  HA�P��EH��;
C .........  I ............ I .... I  ...........................
C            I              I      I
C15+NTOKOB*3+I 2+NTOKOB*3+  I      I  NPSUB - HOMEP �/� �O���OBATE��
C NHA�P*2    I  NHA�P*2     I  FM  I         ( PSUB) B KOTOPO� �PO��BO-
C            I              I      I         ��TC� PAC�ET TOKA �CTO�H�KA
C (...)+1    I  (...)+1     I  FM  I  NPAR - KO���ECTBO �APAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  �APAMETP� PSUB , �A�AHH�E
C�O(...)+NPARI �O(...)+NPAR I      I  �O���OBATE�EM.
C            I              I      I
C 17+3*NTOKOBI 5 + 3*NTOKOB I  DM  I  TOKTA - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   A    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 18+3*NTOKOBI 6 + 3*NTOKOB I  DM  I  TOKTA - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   B    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 19+3*NTOKOBI 7 + 3*NTOKOB I  DM  I  TOKTA - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   C    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 20+3*NTOKOBI 8 + 3*NTOKOB I  DM  I  TOKTA - �HA�EH�E TOKA �CTO�H�KA
C +2*NNAPR + I + 2*NNAPR +  I      I          �A��   0    �P�  T=TN+1
C NPAR       I NPAR         I      I
C
C 21+3*NTOKOBI 9 + 3*NTOKOB I  DM  I  G12 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 22+3*NTOKOBI10 + 3*NTOKOB I  DM  I  G13 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 23+3*NTOKOBI11 + 3*NTOKOB I  DM  I  G23 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 24+3*NTOKOBI12 + 3*NTOKOB I  DM  I  G14 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 25+3*NTOKOBI13 + 3*NTOKOB I  DM  I  G24 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 26+3*NTOKOBI14 + 3*NTOKOB I  DM  I  G34 - �HA�EH�E �POBO��MOCT�
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C
C 27+3*NTOKOBI15 + 3*NTOKOB I  DM  I  J12 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 28+3*NTOKOBI16 + 3*NTOKOB I  DM  I  J13 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 29+3*NTOKOBI17 + 3*NTOKOB I  DM  I  J23 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 30+3*NTOKOBI18 + 3*NTOKOB I  DM  I  J14 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 31+3*NTOKOBI19 + 3*NTOKOB I  DM  I  J24 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C 32+3*NTOKOBI20 + 3*NTOKOB I  DM  I  J34 - �HA�EH�E �CTO�H�KA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         �P� T=TN+1
C NPAR       I NPAR         I      I
C
C 33+3*NTOKOBI21 + 3*NTOKOB I  DM  I  NMOD - �P��HAK MO�E��
C +2*NNAPR + I + 2*NNAPR +  I      I     0 - ��HE�HA�
C NPAR       I NPAR         I      I   > 0 - HE��HE�HA�
C            I              I      I
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 33 + 3*NTOKOB + 2*NNAPR + NPAR
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 21 + 3*NTOKOB + 2*NNAPR + NPAR
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM
C----------------------------------------------------------------------
C-->  HA�A��...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
C----------------------------------------------------------------------
C--> CKO��KO ��EMEHTOB?
C----------------------------------------------------------------------
       NTOKOB = MSW(13)
       IF( NTOKOB .EQ. 0 ) RETURN
C----------------------------------------------------------------------
C--> ��� KA��O�O ��EMEHTA ��EM...
C----------------------------------------------------------------------
          DO 15 I= 1 , NTOKOB
          NELMOT = MSW(11 + 3*I)
          NSM = MSW(12 + 3*I)
C----------------------------------------------------------------------
C-->  �CKAT� B MSWALL ��EMEHT C #��-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HA��� ��� HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C-->  ��EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 100) NEL, TIP, NELMOT
100    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     =  ' ( T�� # ', I3, ' ) ***'/
     =  ' *** C HE C��ECTB����M B �E�� ��EMEHTOM # ', I5,' ***'/
     =  ' *** O�PA�OTKA �E�� �PEKPA�AETC� *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA���, ECT� �� � ��EMEHTA TPE��EM�� TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 3
2      CONTINUE
C----------------------------------------------------------------------
C-->  CB��� C TOKOM , KOTOPO�O HE C��ECTB�ET � �A�AHHO�O ��EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELMOT, NSM
101    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, ' ( CME�EH�E',I3, ' ) - O���O�EH',
     = ' ��EMEHT HE �MEET TPE��EMO�O TOKA ***'/
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
3      CONTINUE
          MSW(13+3*I) = ATOKM + NSM - 1
15     CONTINUE
C----------------------------------------------------------------------
C--> �AKOH����!
       KOD = 0
       RETURN
       END
