       SUBROUTINE LM0003( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: LM0003( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�� T��A # 64
C   ----------     ***  �.�.C. PE����TOPA BO�����EH�� 3X3X0 ******
C                  * * * * * * * * * * * * * * * * * * * * * * * *
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  64      3-X �A�HA� �.�.C        E_PE�
C                          PE����P�EMA� OT "PB"
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 64 � K0 KA KB KC � T_BK�  R_BH�T #_��-TA  K_�C��;
C
C     K0 - O���� ��E�, KA - �A�A A, KB - �A�A B, KC - �A�A C;
C
C---> B�BO�H�E �HA�EH��: TOK�: K ���AM KA, KB, KC, OT ���A K0,
C                              *                   **
C                      �HA�EH�� �.�.C. E1, E2, E3;
C
C
C        �� TOKN ��EMEHTA #_��-TA B���PAETC� 3-� �EPB�X
C           �HA�EH��, �MHO�A�TC� HA K_�C��. � �P�H�MA�TC�
C           �A �HA�EH�E �.�.C. 3-X �A�HO� C�CTEM� HA�P��EH��;
C
C  ===  �O��CKAETC� -->  MO�E�� T�� # 40, # 13, # 63
C
C !!!!   �A�EP�KA HA O��H �A� �HTE�P�POBAH�� !!!
C        -----------------------------------
C
C  �P� TN , T_BK� --> ���T�BA�TC� TO��KO O�H� CO�POT�B�EH�� R_BH�T;
C
C
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
C
C
C                  RMSW T��A # 64 [ �CTO�H�K HA�P��EH�� OT "PE�.BO��" ]
C                  -----------------------------------------
C
C                    TOK(J) = K_�C. * E(J) / R_BH�T. + 1/R * U_N+1
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II �HA�.  I �HA�.I # ��-TAI A�P. II      IJESIJESI K   I
C   ����:     II  1/R   I T_BK�I "PB"   I  B   II JESA I   I   I �C��I
C  K0         II        I      I        I TOKN II      I B I C I EH��I
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 40, 13, 63, 7*0/
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ��-TA "PE�. BO�����EH��"
       NELMOT = MSW(15)
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
C-->  HA�A�O O��CAH�� B MSW --->
C              = AMOT
C-->  HA�A�O C��CKA ���OB ��� #�_TA
C              = AMOT+8
C-->  A�PEC B TOKN/TOKN1 ��� �EPEMEHH�X COCTO�H�� PE����TOPA
       MSW(16) = ATOKM
       KOD = 0
       RETURN
       END
