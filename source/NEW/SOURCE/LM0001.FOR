       SUBROUTINE LM0001( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: LM0001( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�� T��A # 40
C   ----------     ***  PE����TOP BO�����EH�� 3X3X0 ******
C   METO�:        �O�CK B MSW_ALL O��CAH�� ��EMEHTA *��. MA�.*
C   -----         ( �O�. T�� CO��ACHO BCTPOEHHO� TA�. "LTIP" )
C                 B C���AE HAXO��EH�� -> �O�OPM�POBAH�E MSW_O��CAH��;
C                 B �TOM C���AE KOD = 0, �HA�E = 1;
C   �APAAMETP�:   MSW(RMSW) - "OTPE�OK" MSW_MO�E�� T�� # 40;
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
C              �OPMAT RMSW T�� # 40:
C              ---------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I       O�O�HA�EH�E - �HA�EH�E
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I
C O��CAH��   I    FM0010    I      I
C            I              I      I
C------------I--------------I------I-----------------------------------
C     9      I    -2        I      I HOMEP ���A KA ( �A�A A )
C    10      I    -1        I      I HOMEP ���A KB ( �A�A B )
C    11      I     0        I      I HOMEP ���A KC ( �A�A C )
C----------------------------------------------------------------------
C    12      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4
C    13      I     2        I  FM  I  NELT - HOMEP ��. PE����P. ��. MA�.
C    14      I     3        I  LM  I  AMOT - A�P. B MSWALL O��CAH��
C            I              I      I         PE�. ��. MA��H�
C    15      I     4        I  LM  I  APOT - A�PEC B MSWALL ���OB MA��H�
C    16      I     5        I  LM  I  ATOKM - A�P B TOKN/N1 BE����H,
C            I              I      I          PAC��T. ��� ��. MA�.
C    17      I     6        I  FM  I  TBKL - BPEM�, HA��HA� C KOTOPO�O
C            I              I      I         BK���AETC� PE����TOP;
C    18,19   I     7,8      I      I  PE�EPB.
C    20      I     9        I  FM  I  UPOT - �OTO�O�HOE HA�P��EH�E
C    21      I     10       I  FM  I  NSUB - HOMEP �/� �O���OBATE��
C22,23,24,25 I11,12,13,14,15I      I  PA�. MECTO ��� �HTE�PATOPOB;
C  27        I    16        I  FM  I  NPAR - KO�-BO �APAMETPOB,
C            I              I      I         BBE�EHH�X B PSUB<..>
C OT 28 �O   I OT 17 �O     I FM   I  �APAMETP� PSUB, �A�AHH�E
C 28 + NPAR  I 17 + NPAR    I      I  �O���OBATE�EM
C            I              I      I
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 28+NPAR
C
C
C
C    � � �    ��.  MA��H�:
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 11, 110, 12, 120:
C                ----------------------------------------------
C    + 14  � 140
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C  +--------------------------------------------------------------+
C  I ���� �O�K���EH�� S1N S2N S3N F1N F2N F3N S1K S2K F1K F2K F3K I
C  I           12-T� C�OB: OT 9 �O 20                             I
C  +--------------------------------------------------------------+
C
C              RMSW T�� # 11, 110, 12, 120 [ ��. MA��HA 3X3X0 ] :
C              ************************************************
C
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PE�EPB I"DOM"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 5-T�   I ��� I      I      I       I       I       I       I     I
C  I  C�OB  IMEX. I 9-T� I 9-T� I 36-T� I 36-T� I 6-T�  I 6-T�  I NMASI
C  I        I�P-� I C�OB I C�OB I C�OB  I C�OB  I C�OB  I C�OB  I C�OBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I        I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - O�PE�E��ETC� PA�MEPOM MAS <...> ==> ...+ NMAS + 1
C
C      INTEGER D1,D2,D3,D4,D5,D6,D7,D8
C
C
C      DATA D1, D2, D3, D4, D5, D6,  D7,  D8
C    =  /   21, 27, 36, 45, 81, 117, 123, 129 /
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 11, 110, 12, 120, 14, 140, 4*0 /
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ��-TA ��. MA��H�:
       NELMOT = MSW(13)
C-->
       NEL = MSW(1)
       TIP = MSW(2)
C-->
C-->  �CKAT� B MSWALL ��EMEHT C #��-TA = NELMOT:
       CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, KVIV, ATOKM )
C-->
       IF   ( IRC .EQ. 1 ) GO TO 1
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
C-->  MSW - HA���, �TO �A T�� TAM ��BET ?
       TIPX = MSWALL(AMOT + 1 )
       DO 2 I = 1, K10
       IF ( TIPX .EQ. LTAB(I) ) GO TO 3
2      CONTINUE
C-->  CB��� C HE HA��M T��OM.....
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELMOT, TIPX
101    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, ' ( T�� # ', I3, ' ) - O���O�EH',
     = ' ��� HE PEA���OBAH ***'/
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
3      CONTINUE
C-->  HA�A�O O��CAH�� MA��H�--->
       MSW(14) = AMOT
C-->  HA�A�O C��CKA ���OB MA��H�-->
       MSW(15) = AMOT+8
C-->  A�PEC B TOKN/TOKN1 ��� �EPEMEHH�X COCTO�H�� MA��H�-->
       MSW(16) = ATOKM
       KOD = 0
       RETURN
       END
