       SUBROUTINE LM0004( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: LM0004( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�E� T��A #93 � #668
C   ----------     ***  BEHT���(K���),��PAB��EM�� C���  ***
C   METO�:        �O�CK B MSW_ALL O��CAH�� ��PAB����E�O ��EMEHTA ,
C   -----         B C���AE HAXO��EH�� -> �O�OPM�POBAH�E MSW_O��CAH��;
C                 B �TOM C���AE KOD = 0, �HA�E KOD = 1;
C   �APAMETP�:    MSW(RMSW) - "OTPE�OK" MSW_MO�E�� T�� # 93 ��� # 668
C   ----------    ADR - A�PEC B MSW_ALL MSW_O��CAH�� �TO� MO�E��;
C                 MSWALL(RMSWAL) - BEC� C��COK MSW C�OB;
C                 KOD - = 0 - C�OPM�POBA�� MSW,
C                       = 1 - �O�. O���K�;
C                      ( HA NWTR COO��EH�E B�BO��TC� );
C   �O��PO�PAMM�: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C   �P�ME�AH�E:  �OPMAT� MSW_O��CAH�� ---->
C   ----------
C              �OPMAT RMSW T�� # 93 � # 668
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I       O�O�HA�EH�E - �HA�EH�E
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I
C O��CAH��   I    FM0016    I      I
C            I              I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I  FM  I HOMEP ���A A ( AHO� )
C    10      I     0        I  FM  I HOMEP ���A K ( KATO� )
C----------------------------------------------------------------------
C    11      I     1        I  FM  I NELT  -HOMEP ��EMEHTA, ��PAB�.
C            I              I      I        BEHT��EM ( C��� );
C    12      I     2        I  FM  I NSM   -�OP��KOB�� HOMEP C��HA�A
C            I              I      I        ��PAB�EH�� C���;
C    13      I     3        I  LM  I ATOKM -A�PEC B TOKN/N1 �APAMETPA
C            I              I      I        ��PAB�EH��;
C    14      I     4        I  DM  I KD    -KO� COCTO�H�� BEHT���:
C            I              I      I        KD = 0 -�AKP�T,
C            I              I      I        KD = 1 -OTKP�T;
C    15      I     5        I  DM  I HX    -BPEM� BO�MO�HO� KOMM�TA���;
C    16      I     6        I  DM  I MCIU  -KO� �EPEXO�A �� O�HO�O
C            I              I      I        COCTO�H�� B �P��OE;
C    17      I     7        I  DM  I HXCIU -BPEM� �EPEXO�A;
C  18,19     I    8,9       I      I    PE�EPB
C----------------------------------------------------------------------
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, AELT, ATOKM, TIPX
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ��-TA ,��PAB����E�O BEHT��EM.
       NELT = MSW(11)
C-->
       NEL = MSW(1)
       TIP = MSW(2)
C----------------------------------------------------------------------
C-->  �CKAT� B MSWALL ��EMEHT C #��-TA = NELT:
       CALL SEQMS0(MSWALL, NELT, AELT, IRC, NSM, ATOKM )
C-->  HA��� ?
       IF   ( IRC .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C-->  ��EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 100) NEL, TIP, NELT
100    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     =  ' ( T�� # ', I3, ' ) ***'/
     =  ' *** C HE C��ECTB����M B �E�� ��EMEHTOM # ', I5,' ***'/
     =  ' *** O�PA�OTKA �E�� �PEKPA�AETC� *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA���, �TO �A T�� TAM ��BET ?
       TIPX = MSWALL(AELT + 1 )
       IF( TIPX .EQ. 301 ) GO TO 2
C----------------------------------------------------------------------
C-->  CB��� C HE HA��M T��OM.....
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELT, TIPX
101    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, ' ( T�� # ', I3, ' ) - O���O�EH',
     = ' ��� HE PEA���OBAH ***'/
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
C----------------------------------------------------------------------
2      CONTINUE
C-->  MSW - HA���, A ECT� �� �A�AHHOE CME�EH�E � HA��EHHO�O T��A?
       IF ( MSW(12) .LE. NSM ) GO TO 3
C----------------------------------------------------------------------
C-->  CB��� C �APAMETPOM ,KOTOPO�O HE C��ECTB�ET � �A�AHHO�O ��EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,102) NEL, TIP, NELT, MSW(12) , NSM
102    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, 'CO CME�EH�EM ', I5, ' HE BO�MO�EH ',
     = ' T.K. �A�PO�EHH�� ��EMEHT �MEET MAKC. BO�M. CME�EH�E ', I5 //
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
3      CONTINUE
C----------------------------------------------------------------------
C-->  HA�A�O O��CAH�� BEHT���--->
C-->  A�PEC B TOKN/TOKN1 ��� ��PAB����E� �EPEMEHHO�  -->
       MSW(13) = ATOKM + MSW(12) - 1
       KOD = 0
       RETURN
       END
