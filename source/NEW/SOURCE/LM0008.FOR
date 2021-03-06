       SUBROUTINE LM0008( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA: LM0008( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�E� T��OB
C   ----------             333,334,335,360:
C              +-----+-----------------------------------------I
C              I     I                                         I
C              I T�� I        HA�MEHOBAH�E ��EMEHTA            I
C              I     I                                         I
C              +-----+-----------------------------------------+
C              I 333 I  ���POBO�  RS-TP���EP                   I
C              +-----+-----------------------------------------+
C              I 334 I  ���POBO�   D-TP���EP                   I
C              +-----+-----------------------------------------+
C              I 335 I  ���POBO�   T-TP���EP                   I
C              +-----+-----------------------------------------+
C              I 360 I  KOM�APATOP(1)                          I
C              +-----+-----------------------------------------+
C
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
C
C              �OPMAT RMSW T�� # 33X � 360:
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0026    I      I
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
C
C
C              �OPMAT RMSW T�� # 335:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0026    I      I
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM, ADRMSW
C----------------------------------------------------------------------
C-->  HA�A��...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
C----------------------------------------------------------------------
100   CONTINUE
C======================================================================
C----------------------------------------------------------------------
C---> ���POBO� RS-TP���EP                     ( T��  # 333 )
C---> ���POBO�  D-TP���EP                     ( T��  # 334 )
C---> ���POBO�  T-TP���EP                     ( T��  # 335 )
C---> KOM�APATOP                              ( T��  # 360 )
C----------------------------------------------------------------------
          NOMTOK = 1
          NELMOT = MSW(12)
          NSM = MSW(13)
          ADRMSW = 14
1         CONTINUE
C----------------------------------------------------------------------
C-->  �CKAT� B MSWALL ��EMEHT C #��-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HA��� ��� HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 4
C----------------------------------------------------------------------
C-->  ��EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 500) NEL, TIP, NELMOT
500    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     =  ' ( T�� # ', I3, ' ) ***'/
     =  ' *** C HE C��ECTB����M B �E�� ��EMEHTOM # ', I5,' ***'/
     =  ' *** O�PA�OTKA �E�� �PEKPA�AETC� *** '//)
       RETURN
C----------------------------------------------------------------------
4      CONTINUE
C-->  MSW - HA���, ECT� �� � ��EMEHTA TPE��EM�� TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 6
C----------------------------------------------------------------------
C-->  CB��� C TOKOM , KOTOPO�O HE C��ECTB�ET � �A�AHHO�O ��EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,501) NEL, TIP, NELMOT, NSM
C----------------------------------------------------------------------
501    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, ' ( CME�EH�E',I3, ' ) - O���O�EH',
     = ' ��EMEHT HE �MEET TPE��EMO�O TOKA ***'/
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
6      CONTINUE
          MSW(ADRMSW) = ATOKM + NSM - 1
C----------------------------------------------------------------------
C--->   ��EM C�E������ TOK
          NOMTOK = NOMTOK + 1
          NELMOT = MSW( 15 )
          NSM    = MSW( 16 )
          ADRMSW   = 17
          IF( NOMTOK .EQ. 2  .AND.  TIP .NE. 335  ) GO TO 1
C----------------------------------------------------------------------
C--> �AKOH����!
C----------------------------------------------------------------------
C
       KOD = 0
       RETURN
       END
