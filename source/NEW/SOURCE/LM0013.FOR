       SUBROUTINE LM0013( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT  ��H�   1989 �.)------------
C   �O��PO�PAMMA: LM0013( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�� T��A
C   ----------             380
C              +-----+-----------------------------------------I
C              I     I                                         I
C              I T�� I        HA�MEHOBAH�E ��EMEHTA            I
C              I     I                                         I
C              +-----+-----------------------------------------+
C              I 380 I  M�KPO�PO�ECOP  K1801BM1(2,3)           I
C              +-----+-----------------------------------------+
C
C
C   METO�:        �O�CK B MSW_ALL O��CAH�� ��EMEHTOB , A�PECOB �A�AHH�X
C   -----         TOKOB ��EMEHTOB B MACC�BAX TOKN/TOKN1.
C                 B C���AE HAXO��EH�� -> �O�OPM�POBAH�E MSW_O��CAH��;
C                 B �TOM C���AE KOD = 0, �HA�E = 1;
C
C   �APAMETP�:    MSW(RMSW) - "OTPE�OK" MSW_MO�E�� T�� # 380;
C   ----------    ADR - A�PEC B MSW_ALL MSW_O��CAH�� �TO� MO�E��;
C                 MSWALL(RMSWAL) - BEC� C��COK MSW C�OB;
C                 KOD - = 0 - C�OPM�POBA�� MSW,
C                       = 1 - �O�. O���K�;
C                      ( HA NWTR COO��EH�E B�BO��TC� );
C   �O��PO�PAMM�: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C                   �OPMAT RMSW T�� # 380:
C                  -----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0032    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I TX - BPEM� BO�MO�HO�O �EPEK���EH��;
C    11      I     2        I  DM  I KX - �KA�ATE�� ��MEHEH�� COCTO�H��
C            I              I      I      �OPTA  B/B HA �A�E ;
C    12      I     3        I  FM  I TTAKT - �EP�O� TAKTOBO� �ACTOT� ;
C    13      I     4        I  FM  I P - ��A� OT�A�O�HO� �E�AT�:
C            I              I      I     P=0 - OT�A�O�HA� �E�AT� OTK�.
C            I              I      I     P=1 - OT�A�O�HA� �E�AT� BK�.
C    14      I     5        I  FM  I           PE�EPB
C    15      I     6        I  FM  I           PE�EPB
C    16      I     7        I  DM  I KX - �KA�ATE�� ��MEHEH�� COCTO�H��
C            I              I      I      �OPTA B/B HA �A�E ;
C    17      I     8        I  DM  I TT - TEK��EE BPEM� ;
C    18      I     9        I  DM  I R0 - CO�EP��MOE POH R0 �P� T=TT ;
C    19      I    10        I  DM  I R1 - CO�EP��MOE POH R1 �P� T=TT ;
C    20      I    11        I  DM  I R2 - CO�EP��MOE POH R2 �P� T=TT ;
C    21      I    12        I  DM  I R3 - CO�EP��MOE POH R3 �P� T=TT ;
C    22      I    13        I  DM  I R4 - CO�EP��MOE POH R4 �P� T=TT ;
C    23      I    14        I  DM  I R5 - CO�EP��MOE POH R5 �P� T=TT ;
C    24      I    15        I  DM  I R6 - CO�EP��MOE POH R6 �P� T=TT ;
C    25      I    16        I  DM  I R7 - CO�EP��MOE POH R7 �P� T=TT ;
C    26      I    17        I  DM  I FLC-�HA�EH�E ��A�A "C" CC� �P� T=TT
C    27      I    18        I  DM  I FLV-�HA�EH�E ��A�A "V" CC� �P� T=TT
C    28      I    19        I  DM  I FLZ-�HA�EH�E ��A�A "Z" CC� �P� T=TT
C    29      I    20        I  DM  I FLN-�HA�EH�E ��A�A "N" CC� �P� T=TT
C    30      I    21        I  DM  I FLT-�HA�EH�E ��A�A "T" CC� �P� T=TT
C    31      I    22        I  DM  I FLIO-�HA�EH�E ��A�A "I/O" CC�
C            I              I      I             �P� T=TT ;
C    32      I    23        I  DM  I IPORT  -CO�EP��MOE �OPTA B�BO�A
C    33      I    24        I  DM  I IPORT1 -CO�EP��MOE �OPTA BBO�A
C            I              I      I             �P� T=TT ;
C    34      I    25        I  FM  I ADRPROG-MAKC�MA��H�� A�PEC O�� ;
C    35      I    26        I  FM  I ADRPRTO-A�PEC �OPTA B�BO�A;
C    36      I    27        I  FM  I ADRPRTI-A�PEC �OPTA BBO�A ;
C    37      I    28        I  DM  I TO - CTAPOE BPEM� ;
C    38      I    29        I  DM  I R0 - CO�EP��MOE POH R0 �P� T=TO ;
C    39      I    30        I  DM  I R1 - CO�EP��MOE POH R1 �P� T=TO ;
C    40      I    31        I  DM  I R2 - CO�EP��MOE POH R2 �P� T=TO ;
C    41      I    32        I  DM  I R3 - CO�EP��MOE POH R3 �P� T=TO ;
C    42      I    33        I  DM  I R4 - CO�EP��MOE POH R4 �P� T=TO ;
C    43      I    34        I  DM  I R5 - CO�EP��MOE POH R5 �P� T=TO ;
C    44      I    35        I  DM  I R6 - CO�EP��MOE POH R6 �P� T=TO ;
C    45      I    36        I  DM  I R7 - CO�EP��MOE POH R7 �P� T=TO ;
C    46      I    37        I  DM  I FLC-�HA�EH�E ��A�A "C" CC� �P� T=TO
C    47      I    38        I  DM  I FLV-�HA�EH�E ��A�A "V" CC� �P� T=TO
C    48      I    39        I  DM  I FLZ-�HA�EH�E ��A�A "Z" CC� �P� T=TO
C    49      I    40        I  DM  I FLN-�HA�EH�E ��A�A "N" CC� �P� T=TO
C    50      I    41        I  DM  I FLT-�HA�EH�E ��A�A "T" CC� �P� T=TO
C    51      I    42        I  DM  I FLIO-�HA�EH�E ��A�A "I/O" CC�
C            I              I      I             �P� T=TO ;
C    52      I    43        I  DM  I IPORT  -CO�EP��MOE �OPTA B�BO�A
C    53      I    44        I  DM  I IPORT1 -CO�EP��MOE �OPTA BBO�A
C            I              I      I             �P� T=TO ;
C    54      I    45        I  FM  I  #�� - HOMEP ��EMEHTA;
C    55      I    46        I  FM  I  #CM - HOMEP TOKA ��EMEHTA #�� ;
C    56      I    47        I  LM  I  ATOKM - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEPA #CM ;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 56
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 47
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
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
C---> M�KPO�PO�ECCOP K1801BM1(2,3)            ( T��  # 380 )
C----------------------------------------------------------------------
          NELMOT = MSW(54)
          NSM = MSW(55)
          ADRMSW = 56
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
C--> �AKOH����!
C----------------------------------------------------------------------
C
       KOD = 0
       RETURN
       END
