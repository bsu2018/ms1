       SUBROUTINE PSUB1( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: PSUB1( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   �AKOHOB PE����POBAH�� "PE����TOPA BO�����EH��"
C                ( MAKPOMO�E�� # T��A 40 )
C               �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C               BE����HA�, PAC��TAHH�M B MAKPOMO�E�� # 40
C               ( B �POEK��� HA O�OPH�� C�CTEM� OTC�ETA),
C               O�PE�E���TC� ��PAB�����E HA�P��EH�� PE����POBAH��
C               B TO� �E C�CTEME OTC�ETA.
C   METO�:     B���C�EH�E �O �A�AHH�M �OPM��AM,
C   -----      ��� �HTE�P�POBAH�� - METO� TPA�E���;
C
C              �O���OBATE�� BBO��T �EPE� A��APAT "PSUB" :
C
C   PSUB 1 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD  XAF  >;
C
C
C  �APAMETP�:   NEL - HOMEP ��EMEHTA, ��� KOTOPO�O PSUB PA�OTAET;
C  ---------    URMAS - MACC�B �AHH�X �O���OBATE��:
C               *****
C       C�OBO        �HA�EH�E
C      1               8 - KO�-BO �APAMETPOB B C��CKE URMAS
C      2               KE         �HA�EH�E
C      3               URD00      �HA�EH�E
C      4               KI         �HA�EH�E
C      5               ISDZAD     �HA�EH�E
C      6               URQ00      �HA�EH�E
C      7               KS         �HA�EH�E
C      8               BETA       �HA�EH�E
C      9               SZAD       �HA�EH�E
C      10              XAF        �HA�EH�E
C  - - - - - - -- - - - - - - - - - - - - - - - - - - - - -
C
C
C              STMAS - MACC�B �AHH�X, �EPE�ABAEM�X B PSUB:
C              *****
C      C�OBO           O�O�HA�EHE   -   �HA�EH�E
C      1               TN - BPEM� N - �O �A�A PAC�ETA;
C      2               H0 - �A� �HTE�P�POBAH��;
C-��� TN+H0***��� TN ***
C   I           I
C   Y           Y
C   13         3       ISQ0 - TOK CTATOPA Q OC� O�OPHA�;
C   14         4       ISD0 - TOK CTATOPA D OC� O�OPHA�;
C   15         5       USQ0 - HA�P��EH�E CTATOPA Q OC� O�OPHA�;
C   16         6       USD0 - HA�P��EH�E CTATOPA D OC� O�OPHA�;
C   17         7       IRQ0 - TOK POTOPA Q OC� O�OPHA�;
C   18         8       IRD0 - TOK POTOPA D OC� O�OPHA�;
C   19         9       OMEG0 - O�OPHA� �ACTOTA;
C   20        10       OMEGA - ���OBA� CKOPOCT� POTOPA;
C   21        11       GAMMA - ��O� ME��� CTATOP. � POTOP. �A�A "A";
C   22        12       S - CKO���EH�E;
C------------------------
C   14 *--* 22 - TO �E ��� T = TN+H0;
C   23, 24, 25 -  P E � E P B;
C
C------------------------------------------------------
C
C
C          DGMAS  -  MACC�B B��EC�EHH�X �HA�EH�E HA�P��EH��
C          *****                        ��PAB�EH�� B Q_D O�.
C
C   C�OBO        O�O�HA�EHE - �HA�EH�E:
C   1          UYQ0    - ��P. HA�P��. �O Q OC� O�. C�CTEM�;
C   2          UYD0       - ��P. HA�P��. �O D OC� O�. C�CTEM�;
C   3          UYMOD      - MO���� BEKTOPA ��P. HA�P��EH��;
C   4          I_S_D_�A�. - �A�AHH�� TOK �O D CTATOPA;
C   5          S_�A�.     - �A�AHHOE CKO���EH�E;
C -----------------------------------------------------
C
C
C              INTMAS - MACC�B ��� PEA���A��� �HTE�PATOPOB,
C              ******  ( 5-T� C�OB );
C                �EPB�E 2- �HA�. B TOKN/TOKN1 XPAH�TC�;
C
C  PSUB1 --->  B���C��ET:
C
C  UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* (KE+1) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]*(KE+1) - KE* XAF* IRQ0;
C
C  ��E:  INT - �HTE�PA�;
C
C   �O��PO�PAMM�: HET
C   -------------
C
C
C   �P�ME�AH�E:
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
C
       REAL INTMAS, INTI, INTS, INT, KE, ISDZAD, KI, KS
C
       REAL ISQ0, ISD0, IRQ0, IRD0, INTSUM
C
C  BCTPOEHH�� �HTE�PATOP--->
C
       INT( INTSUM, XN1, XN ) = INTSUM + H0 *(XN1 + XN )/ 2.0
C
       TN = STMAS(1)
       H0 = STMAS(2)
C$$$$$$TN1 = TN + H0
C--> �HTE�PA��
       INTI = INTMAS(1)
       INTS = INTMAS(2)
C-->  HOB. �HA�.
       DIN = STMAS(4) - URMAS(5)
       DIN1 = STMAS(14) - URMAS(5)
C
       INTMAS(1) = INT( INTI, DIN1, DIN )
C
       DSN = STMAS(12) - URMAS(9)
       DSN1 = STMAS(22) - URMAS(9)
C
       INTMAS(2) = INT( INTS, DSN1, DSN )
C
       INTI = INTMAS(1)
C
       INTS = INTMAS(2)
C
       DGMAS(4) = URMAS(5)
C
       DGMAS(5) = URMAS(9)
C
C----------------------------------------------------------------------
C-->  ��P. ��� TN+H0  MOMEHTA BPEMEH�;
C
      UYD0=(URMAS(3)-URMAS(4)*INTI)*(URMAS(2)+1.)-URMAS(2)*STMAS(18)*
     *                                             URMAS(10)
C
      UYQ0=(URMAS(6)-URMAS(7)*STMAS(22)-URMAS(8)*INTS)*(URMAS(2)+1.)-
     =      URMAS(2)*STMAS(17)*URMAS(10)
C----------------------------------------------------------------------
       DGMAS(1) = UYQ0
C
       DGMAS(2) = UYD0
C
       DGMAS(3) = SQRT( UYQ0**2 + UYD0**2 )
C
       RETURN
       END
