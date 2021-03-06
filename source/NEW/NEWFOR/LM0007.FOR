       SUBROUTINE LM0007( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   MA�   1987 �.)------------
C   �O��PO�PAMMA: LM0007( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�E� T��OB
C   ---------- 310,311,312,313,314,315,316,317,320,321,322:
C     +-----+-----------------------------------+--------------------I
C     I     I                                   I     �EPE�ATO�HA�   I
C     I T�� I       HA�MEHOBAH�E �BEHA          I                    I
C     I     I                                   I       ��HK���      I
C     I-----I-----------------------------------I--------------------I
C     I     I  �PO�OP��OHA��HO-C�MM�P���EE      I                    I
C     I 310 I       ( � - PE����TOP )           I          K         I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I         �HTE�P�P���EE             I                    I
C     I 311 I       ( � - PE����TOP )           I        1/(TS)      I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I                                   I                    I
C     I 312 I         �HEP��OHHOE               I        1/(TS+1)    I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I         ����EPEH��P���EE          I                    I
C     I 313 I       ( � - PE����TOP )           I      TS/(TS+1)     I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I   �PO�OP��OHA��HO-�HTE�P�P���EE   I    TAU1*S + 1      I
C     I 314 I         ( �� - PE����TOP )        I   -------------    I
C     I     I                                   I      TAU2*S        I
C     I-----I-----------------------------------I--------------------I
C     I     I �PO�OP��OHA��HO-����EPEH��P���EE  I    TAU1*S + 1      I
C     I 315 I         ( �� - PE����TOP )        I   -------------    I
C     I     I                                   I    TAU2*S + 1      I
C     I-----I-----------------------------------I--------------------I
C     I     I                                   I    1 - TAU*S       I
C     I 316 I       �A�OBPA�ATE��               I   -------------    I
C     I     I                                   I    1 + TAU*S       I
C     I-----I-----------------------------------I--------------------I
C     I     I                                   I       W**2         I
C     I 317 I       KO��E�ATE��HOE              I  ----------------- I
C     I     I                                   I  S**2+2*E*W*S+W**2 I
C     +-----+-----------------------------------+--------------------I
C     I     I                                   I          I Y    *  I
C     I     I                                   I          I     *   I
C     I     I        HE��HE�HOE �BEHO           I          I*  *     I
C     I 320 I  (�EPE�ATO�HA� ��HK��� E�O        I   ------*+-------> I
C     I     I  �A�AHHO HA�OPOM TO�EK)           I      *   I       X I
C     I     I                                   I *  *     I         I
C     I     I                                   I          I         I
C     +-----+-----------------------------------+--------------------+
C     I     I                                   I         !Y         I
C     I     I                                   I   !--<--*----!-----I
C     I     I                                   I   !     !�2  !     I
C     I     I                                   I   !     !    !    XI
C     I 321 I       PE�E�HOE �BEHO              I---*-----I----*-----I
C     I     I                                   I   !X1   !    !X2   I
C     I     I                                   I   !     !    !     I
C     I     I                                   I---!-----*-->-!     I
C     I     I                                   I         !�1        I
C     +-----+-----------------------------------+--------------------+
C     I     I                                   I !X        *   *    I
C     I     I                                   I !    *             I
C     I     I                                   I !*              T  I
C     I     I                                   I *----------------- I
C     I 322 I  �BEHO "��CTOE �A�A���BAH�E"      I !�                 I
C     I     I                                   I !             *   *I
C     I     I                                   I !TAU     *         I
C     I     I                                   I !<->!*          T  I
C     I     I                                   I *---*------------- I
C     +-----+-----------------------------------+--------------------+
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
C              �OPMAT RMSW T�� # 310:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NBXOD-KO���ECTBO TOKOB, �C�O��-
C            I              I      I        ��EM�X KAK BXO�H�E C��HA��;
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  FM  I  K1   - KO������EHT �PO�OP��OHA��-
C            I              I      I         HOCT� TOKA #CM1 ��-TA #��1;
C    15      I     6        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C  8+I*4     I  -1+I*4      I  FM  I  #��I -      -"-
C  9+I*4     I     I*4      I  FM  I  #CMI -      -"-
C 10+I*4     I   1+I*4      I  FM  I  KI   -      -"-
C 11+I*4     I   2+I*4      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NBXOD*4  I  3+NBXOD*4   I  FM  I  NGARM -KO���ECTBO �APMOH�K, �C�.
C            I              I      I         ��� O��CAH�� �A�. C��HA�A;
C 13+NBXOD*4 I  4+NBXOD*4   I  FM  I  W  -   �ACTOTA OCH. �APMOH�K�(��);
C 14+NBXOD*4 I  5+NBXOD*4   I  FM  I  A0  -  AM���T��A �OCT. COCTAB��-
C            I              I      I         ��E� �A�. C��HA�A;
C 15+NBXOD*4 I  6+NBXOD*4   I  FM  I  A1  -  AM���T��A 1-O� �APMOH�K�
C            I              I      I         �A�AHHO�O C��HA�A;
C 16+NBXOD*4 I  7+NBXOD*4   I  FM  I  FI1 -  HA�. �A�A 1-O� �APMOH�K�;
C 17+NBXOD*4 I  8+NBXOD*4   I  FM  I  A2  -  AM���T��A 2-O� �APMOH�K�;
C 18+NBXOD*4 I  9+NBXOD*4   I  FM  I  FI2 -  HA�. �A�A 2-O� �APMOH�K�;
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C
C       �OPMAT �EPB�X ��T� C�OB RMSW, O��HAKOB�� ���  T��OB:
C       T�� # 311,312,313,314,315,316,317,320,321,322,330,331,332:
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0020/21 I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I         PE�EPB
C            I              I      I
C    12      I     3        I  FM  I  #��  - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM  - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ��EMEHTA #��;
C   ......   I     .....    I  ..  I ...............................
C   ......   I     .....    I  ..  I ...............................
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
       IF( TIP .EQ. 310 ) GO TO 100
       GO TO 200
C----------------------------------------------------------------------
C---> �PO�OP��OHA��HO C�MM�P���EE �BEHO ( T��  # 310 )
C----------------------------------------------------------------------
100   CONTINUE
C----------------------------------------------------------------------
C--> CKO��KO ��EMEHTOB?
C----------------------------------------------------------------------
       NBXOD = MSW(11)
       IF( NBXOD .EQ. 0 ) RETURN
C----------------------------------------------------------------------
C--> ��� KA��O�O ��EMEHTA ��EM...
C----------------------------------------------------------------------
          DO 15 I= 1 , NBXOD
          NELMOT = MSW(8 + 4*I)
          NSM = MSW(9 + 4*I)
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
       WRITE(NWTR, 500) NEL, TIP, NELMOT
500    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     =  ' ( T�� # ', I3, ' ) ***'/
     =  ' *** C HE C��ECTB����M B �E�� ��EMEHTOM # ', I5,' ***'/
     =  ' *** O�PA�OTKA �E�� �PEKPA�AETC� *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA���, ECT� �� � ��EMEHTA TPE��EM�� TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 3
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
3      CONTINUE
          MSW(11+4*I) = ATOKM + NSM - 1
15     CONTINUE
C----------------------------------------------------------------------
C--> �AKOH����!
       KOD = 0
       RETURN
C======================================================================
C----------------------------------------------------------------------
C---> �HTE�P�P���EE     �BEHO                 ( T��  # 311 )
C---> �HEP��OHHOE       �BEHO                 ( T��  # 312 )
C---> ����EPEH��P���EE  �BEHO                 ( T��  # 313 )
C---> �PO�OP��OHA��HO-�HTE�P�P���EE     �BEHO ( T��  # 314 )
C---> �PO�OP��OHA��HO-����EPEH��P���EE  �BEHO ( T��  # 315 )
C---> �A�OBPA�ATE��                           ( T��  # 316 )
C---> KO��E�ATE��HOE    �BEHO                 ( T��  # 317 )
C---> HE��HE�HOE        �BEHO                 ( T��  # 320 )
C---> PE�E�HOE          �BEHO                 ( T��  # 321 )
C---> �MHO��TE��                              ( T��  # 322 )
C----------------------------------------------------------------------
200   CONTINUE
          NELMOT = MSW(12)
          NSM = MSW(13)
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
       RETURN
6      CONTINUE
          MSW(14) = ATOKM + NSM - 1
C----------------------------------------------------------------------
C--> �AKOH����!
       KOD = 0
       RETURN
       END
