       SUBROUTINE PSUB4( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: PSUB4( NEL, URMAS, STMAS, DGMAS, INTMAS )
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
       REAL INTMAS,IZ,IZ1,IN,IN1
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C �OC�E����A� �ACT� �/� PA�PA�AT�BAETC� �O���OBATE�EM
C----------------------------------------------------------------------
       CIU  = STMAS(3)
       CIU1 = STMAS(4)
       IZ   = STMAS(5)
       IZ1  = STMAS(6)
       IN   = STMAS(7)
       IN1  = STMAS(8)
       DELI = URMAS(2)
C----------------------------------------------------------------------
       IF( TN .EQ. 0.0 ) GO TO 1000
       IF( CIU1 .GT. 0.5 ) GO TO 500
C----------------------------------------------------------------------
C PA�OTAET �APA K2 � K3
C----------------------------------------------------------------------
        IZ = IZ - DELI
        IZ1= IZ1- DELI
        DGMAS(1) = 00
        DGMAS(2) = 11
        DGMAS(7) = 0.0
        DGMAS(8) = 0.0
        IF( IN1 .GT. IZ1 ) RETURN
        DGMAS(1) = 01
        DGMAS(2) = 10
        GO TO 800
500     CONTINUE
C----------------------------------------------------------------------
C PA�OTAET �APA K1 � K4
C----------------------------------------------------------------------
        IZ = IZ + DELI
        IZ1= IZ1+ DELI
        DGMAS(1) = 11
        DGMAS(2) = 00
        DGMAS(7) = 0.0
        DGMAS(8) = 0.0
        IF( IN1 .LT. IZ1 ) RETURN
        DGMAS(1) = 10
        DGMAS(2) = 01
800     CONTINUE
        DGMAS(7) = ( (TN+H0)*(IZ-IN) - TN*(IZ1-IN1) ) / (IN1-IZ1+IZ-IN)
        DGMAS(8) = DGMAS(7)
        RETURN
1000    CONTINUE
        DGMAS(1) = 01
        DGMAS(2) = 00
        DGMAS(7) = 0.0
        DGMAS(8) = 0.0
        RETURN
        END
