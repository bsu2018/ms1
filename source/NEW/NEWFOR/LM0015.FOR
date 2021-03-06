       SUBROUTINE LM0015( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT ������ 1992 �.)------------
C   �O��PO�PAMMA: LM0015( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�E� T��OB
C   ----------        351- ������� ��� ���⨫쭮�� ���� ��ਮ����
C                ॣ㫨����� �� �ந����쭮�� ��������� ������
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
C                      �OPMAT RMSW T�� # 351
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
C    12      I     3        I  DM  I  TIMEU - �६� �뤠� ᨣ���� ��.
C    13      I     4        I  DM  I  SIGOLD 
C    14      I     5        I  DM  I  GAMMA  
C    15      I     6        I  DM  I  TOKOLD 
C    16      I     7        I      I         
C    17      I     8        I      I         
C    18      I     9        I      I         
C    19      I     10       I      I         ������
C    20      I     11       I      I         
C    21      I     12       I      I         
C    22      I     13       I      I         
C    23      I     14       I  FM  I  #��1-        
C    24      I     15       I  FM  I  #��1       
C    25      I     16       I  FM  I  #�����1 - ��ࠢ�塞� ⮪      
C    26      I     17       I  FM  I  OMEG - frequenc;
C    27      I     18       I  FM  I  EDM  - ������㤠 �����. ����.;
C    28      I     19       I  LM  I  E    - ��⨢����
C    29      I     20       I  FM  I  XG  - ����⨢��� ᮯ�⨢.
C    30      I     21       I  FM  I  X   - ����⨢��� ᮯ�⨢.
C    31      I     22       I  FM  I  NT  - ������⢮ ���祭��
C    32      I     23       I  FM  I  T1 
C    33      I     24       I  FM  I  T2 
C    31+2*nt I    22+2*NT   I  FM  I  I(NT/2)
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 31 + 2*NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 22+2*NT
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(880)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM
C----------------------------------------------------------------------
C-->  HA�A��...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
200   CONTINUE
          NELMOT = MSW(23)
          NSM = MSW(24)
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
501    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, ' ( CME�EH�E',I3, ' ) - O���O�EH',
     = ' ��EMEHT HE �MEET TPE��EMO�O TOKA ***'/
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
6      CONTINUE
          MSW(25) = ATOKM + NSM - 1
C----------------------------------------------------------------------
C--> �AKOH����!
       KOD = 0
       RETURN
       END
