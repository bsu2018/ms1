       SUBROUTINE PSUB10( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �HBAP�  1988 �.)------------
C   �O��PO�PAMMA: PSUB10( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   �AKOHOB ��PAB�EH�� OT�E��H�M BEHT��EM ���
C                �P���O� BEHT��E�.
C                  ( MO�E�� #301 - C��� ).
C               �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C               �HA�EH��M TOKOB � HA�P��EH�� ��EMEHTOB CXEM�
C               O�PE�E���TC� KO� C�� KA��O�O BEHT��� � �A� �O
C               MOMEHTA ��MEHEH�� �HA�EH�� C��.
C
C######################################################################
C
C         �O��PO�PAMMA �OPM�P�ET C��HA�� ��PAB�EH�� K���AM�
C     C��OBO�O ��OKA ��PAB�EH�� �ECT��A�H�M �A�OB�M �B��ATE�EM
C     C �B��O��PH�M ��TAH�EM.
C
C######################################################################
C
C
C   METO�:     O�EB��EH.
C   -----
C  �APAMETP�:   NEL - HOMEP ��EMEHTA, ��� KOTOPO�O PSUB PA�OTAET;
C  ---------    URMAS - MACC�B �AHH�X �O���OBATE��:
C                       1-E C�OBO - KO���ECTBO �APAMETPOB B C��CKE
C                       ( �A��CAHO �E��M ��C�OM B BE�ECTBEHH�� MACC�B )
C                       2-E C�OBO � �A�EE - �APAMETP� PSUB,
C                           �A�AHH�E �O���OBATE�EM;
C
C----------------------------------------------------------------------
C              STMAS - MACC�B �AHH�X, �EPE�ABAEM�X B PSUB:
C
C      C�OBO           O�O�HA�EHE   -   �HA�EH�E
C      1               TN - BPEM� N - �O �A�A PAC�ETA;
C      2               H0 - �A� �HTE�P�POBAH��;
C      3               I1 - �HA�EH�E 1-�O TOKA, �ATPE�OBAHHO�O
C                           �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C      4               I11- TO�E, ��� MOMEHTA TN+H0;
C         �A�EE AHA�O���HO �HA�EH�� OCTA��H�X TOKOB;
C   N=3+2*NTOKOB       U1 - �HA�EH�E 1-�O HA�P��EH��, �ATPE�OBAHHO�O
C                           �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C    N+1               U11- TO�E, ��� MOMEHTA TN+H0;
C         �A�EE AHA�O���HO �HA�EH�� OCTA��H�X HA�P��EH��;
C
C----------------------------------------------------------------------
C              DGMAS  -  MACC�B B��EC�EHH�X �APAMETPOB C��.
C
C   C�OBO        O�O�HA�EHE - �HA�EH�E:
C  OT 1 �O 6     - �HA�EH�� MCIU ( KO� C�� )
C                  MCIU = 00  --> "C��" HET B [TN, TN+H0];
C                                  HX = 0.0;
C                  MCIU = 01  --> B [TN, TN+H0] -> �EPE�H�� �POHT
C                                 �M����CA "C��", HX = �HTEPBA� OT
C                                 TN �O �EPE�HE�O �POHTA "C��";
C                  MCIU = 10 -->  B [TN, TN+H0] -> �A�H�� �POHT
C                                 �M����CA "C��", HX = �HTEPBA�
C                                 OT TN �O �A�HE�O �POHTA �M����CA  ;
C                  MCIU = 11 -->  B [TN, TN+H0] "C��" BK���EH,
C                                 COCTO�H�E HE MEH��, HX=0.0;
C
C  OT 7 �O 12    - �HA�EH�� COOTBETCTB����X HX ( �A�A �O BPEMEH�
C                    �O MOMEHTA ��MEHEH�� KO�A C�� )
C----------------------------------------------------------------------
C              INTMAS - MACC�B �� O�HO�O C�OBA.
C                       ( B �O��PO�PAMME HE �C�O����ETC� )
C----------------------------------------------------------------------
C
C   �O��PO�PAMM�: HET
C   -------------
C
C   �P�ME�AH�E: C�MMA KO����ECTBA TPE��EM�X TOKOB � HA�P��EH��
C   ----------  HE �O��HA �PEB��AT� 49-T�.
C               ( KOHTPO�� HE �PO��BO��TC� )
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       REAL INTMAS
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C BBO��M �AHH�E
C----------------------------------------------------------------------
       X  = STMAS(3)
       X1 = STMAS(4)
       TX = URMAS(2)
       HAG = URMAS(3)
       ALFA= URMAS(4)
       TF  = URMAS(5)
C----------------------------------------------------------------------
C--> �P�BE�EM X � X1 K O�HOM� �EP�O��
C----------------------------------------------------------------------
5      CONTINUE
       IF( X .LE. ( TX - ALFA - HAG )  ) GO TO 7
       X = X - TX
       GO TO 5
7      CONTINUE
       IF( X1 .LE. ( TX - ALFA - HAG )  ) GO TO 8
       X1 = X1 - TX
       GO TO 7
8      CONTINUE
C----------------------------------------------------------------------
C-->  O�H���EM B�XO�H�E �AHH�E
C----------------------------------------------------------------------
       DO 10 I= 1,12
       DGMAS(I) = 0.0
10     CONTINUE
C----------------------------------------------------------------------
C->   BBE�EM HOMEP �PE�����E� �A��
       NPRED = IFIX( URMAS(6) )
C->   ���TE��HOCT� BK���EH�� �PE�����E� �A��
       DIMP = URMAS(7)
C->   BPEM� BK���EH�� TEK��E� �A��
       TIME1 = URMAS(8)
C->   BPEM� BK���EH�� C�E����E� �A��
       TIME2 = URMAS(9)
C----------------------------------------------------------------------
C-->  O�PE�E��M HOMEP �A�� KOTOPA� �.�. BK���EHA B  T=TN  �  T=TN+H0
       NTN  = IFIX( (X + ALFA + HAG)/HAG ) + 1
       NTN1 = IFIX( (X1+ ALFA + HAG)/HAG ) + 1
       IF( NTN .NE. NTN1 ) GO TO 1000
C----------------------------------------------------------------------
C-->  BK���EHA TA�E �A�A
       IF( NPRED .EQ. NTN ) GO TO 100
       IF( TN    .EQ. 0.0 ) GO TO 100
       DIMP  = TIME2 - TIME1
       TIME1 = TIME2
       NPRED = NTN - 1
       IF( NPRED .LT. 1 ) NPRED = 6
100    CONTINUE
       IF( NEL .EQ. 36  .AND.  NTN .GT. 3 ) GO TO 9999
       IF( NEL .EQ. 37  .AND.  NTN .LT. 4 ) GO TO 9999
       IF( NTN .GT. 3 ) NTN = NTN - 3
       DGMAS(NTN) = 11.
       DGMAS(NTN+3) = 11.
C--> KAK �E�A C �OPC�POBKO�?
       TFORS = TIME1 + TF * DIMP
       IF( TN1 .LT. TFORS ) GO TO 9999
       DGMAS(NTN+3) = 10.
       DGMAS(NTN+9) = TFORS - TN
       GO TO 9999
C----------------------------------------------------------------------
C--> HA �A�E �O��HA ��MEH�T�C� �A�A
1000   CONTINUE
C--> HA��EM BPEM� ��MEHEH�� C��HA�A
       XX = (NTN1 - 2) * HAG - ALFA
       TIMEX = TN + H0 * (XX - X)/(X1 - X)
       TIME2 = TIMEX
C--> KOMM�T�P�EM...
C
C--> B�K���AEM
       IF( NEL .EQ. 36  .AND.  NTN .GT. 3 ) GO TO 2000
       IF( NEL .EQ. 37  .AND.  NTN .LT. 4 ) GO TO 2000
       IF( NTN .GT. 3 ) NTN = NTN - 3
       DGMAS(NTN) = 10.
       DGMAS(NTN+6) = TIMEX - TN
       DGMAS(NTN+3) = 10.
       DGMAS(NTN+9) = TIMEX - TN
2000   CONTINUE
C-->  BK���AEM
       IF( NEL .EQ. 36  .AND.  NTN1 .GT. 3 ) GO TO 9999
       IF( NEL .EQ. 37  .AND.  NTN1 .LT. 4 ) GO TO 9999
       IF( NTN1 .GT. 3 ) NTN1 = NTN1 - 3
       DGMAS(NTN1) = 1.
       DGMAS(NTN1+6) = TIMEX - TN
       DGMAS(NTN1+3) = 1.
       DGMAS(NTN1+9) = TIMEX - TN
C----------------------------------------------------------------------
C-->  KOH�AEM...
       URMAS(6) = FLOAT(NPRED)
       URMAS(7) = DIMP
       URMAS(8) = TIME1
       URMAS(9) = TIME2
       RETURN
9999   CALL SUBERR(9999)
C ????????
       END
