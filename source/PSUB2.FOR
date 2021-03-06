       SUBROUTINE PSUB2( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: PSUB2( NEL, URMAS, STMAS, DGMAS, INTMAS )
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
C   PSUB 2 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD   XAF
C
C
C      KTI  TI1 ISZAD_1  TI2 ISZAD_2 .....
C                        I         I
C                        I<---*--->I
C
C      KTS  TS1 S_1      TS2    S_2  ...... >;
C                        I         I
C                        I         I
C                        I<---*--->I
C
C
C    ��E:
C          -*-  �BA �HA�EH��, O��C�BA���E O�H� TO�K� O�PE�E�EH��
C               COOTBETCTB���E� ��HK��OHA��HO� �AB�C�MOCT� -->
C               OHA �A�AETC� KAK K�CO�HO-��HE�HA� ��HK���;
C
C       KTI  - KO�-BO TO�EK ��� O�PE�E�EH�� �AB�C�MOCT� ISZAD OT "TN";
C
C       KTS  - KO�-BO TO�EK ��� O�PE�E�EH�� �AB�C�MOCT� SZAD  OT "TN";
C
C
C  == HA�A�O O��CAH�� ��KC�P�ETC� ���: ISZAD -> URMAS(10)
C                                      SZAD  -> URMAS(11 + "2"*KTI )
C  ==
C !!! BH�MAH�E -->  FM MOH�TOP �HA�EH�� "KTI" � "KTS"  � � B O � �    !!
C                                                      ------------
C
C  �APAMETP�:   NEL - HOMEP ��EMEHTA, ��� KOTOPO�O PSUB PA�OTAET;
C  ---------    URMAS - MACC�B �AHH�X �O���OBATE��:
C               *****
C       C�OBO        �HA�EH�E
C      1               8 - KO�-BO �APAMETPOB B C��CKE URMAS
C      2               KE         �HA�EH�E
C      3               URD00      �HA�EH�E
C      4               KI         �HA�EH�E
C      5               ISDZAD     �HA�EH�E  -->  �P� KTI = 0
C      6               URQ00      �HA�EH�E
C      7               KS         �HA�EH�E
C      8               BETA       �HA�EH�E
C      9               SZAD       �HA�EH�E  -->  �P� KTS = 0
C      10              XAF        �HA�EH�E
C  - - - - - - -- - - - - - - - - - - - - - - - - - - - - -
C      11        KTI      KO�-BO TO�EK ��� O��CAH�� ISDZAD=>�(T) ( *2 )
C
C OT 12 �O 11+"2"*KTI     TO�K� K�CO�. ��HE�HO�O �A�AH�� IZDZAD ->�(T)
C
C 12+"2"*KTI     KTS      KO�-BO TO�EK ��� O��CAH�� SZAD =>�(T) ( *2 )
C
C OT 13+"2"*KTI
C �O 12+"2"*KTI+"2"*KTS   TO�K� O�PE�E�EH�� --> SZAD=>�(T)
C
C
C  --> EC�� KTI = 0 -->  BCE �OPMA��HO TO �E, HO B URMAS(11) <-> KTS
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
C   2          UYD0    - ��P. HA�P��. �O D OC� O�. C�CTEM�;
C   3          UYMOD   - MO���� BEKTOPA ��P. HA�P��EH��;
C   4          ISD_�A�.- �A�AHH�� TOK CTATOPA �O OC� Q;
C   5          S_�A�.  - �A�AHHOE CKO���EH�E;
C -----------------------------------------------------
C
C
C              INTMAS - MACC�B ��� PEA���A��� �HTE�PATOPOB,
C              ******  ( 5-T� C�OB );
C            �EPB�E �BA �HA�. XPAH�TC� B TOKN/TOKN1;
C
C  PSUB2 --->  B���C��ET:
C
C  UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* (KE+1) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]*(KE+1) - KE* XAF* IRQ0;
C
C  ��E:  INT - �HTE�PA�;
C
C   �O��PO�PAMM�:   KMAS02;
C   -------------
C
C   �P�ME�AH�E:
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
C
       REAL INTMAS, INTI, INTS, INT, KE, ISDZAD, KI, KS, KMAS02
C
       REAL ISQ0, ISD0, IRQ0, IRD0, INTSUM
C
C  BCTPOEHH�� �HTE�PATOP--->
C
       INT( INTSUM, XN1, XN ) = INTSUM + H0 *(XN1 + XN )/ 2.0
C
       TN = STMAS(1)
       H0 = STMAS(2)
       TN1 = TN + H0
C######################################################################
C-->  O�PE�E�EH�E ISDZAD  � SZAD �O TA��. ��HK. �AB�C.
C
       ISDZAD = URMAS(5)
       SZAD =   URMAS(9)
C-->  CKO��KO �APAMETPOB B PSUB-->
       KPAR = IFIX( URMAS(1) )
       IF ( KPAR .LE. 9 ) GO TO 1
C-->  KO�-BO TO�EK ��� ISDZAD--> KTI
       KTI = IFIX( URMAS(11) )
       IF ( KTI .LE. 0 ) GO TO 2
C-->  �TO-TO ECT�, HA��EM ..
       ISDZAD = KMAS02(TN1, URMAS(11) )
2      CONTINUE
       IF ( KPAR .LE. (11+KTI) ) GO TO 1
C-->  �TO-TO ECT� � ��� SZAD, HA��EM...
       KTS = IFIX( URMAS(12+KTI) )
       IF ( KTS .LE. 0 ) GO TO 1
       SZAD = KMAS02( TN1, URMAS(12+KTI) )
1      CONTINUE
C######################################################################
       DGMAS(4) = ISDZAD
       DGMAS(5) = SZAD
C
C--> �HTE�PA��
       INTI = INTMAS(1)
       INTS = INTMAS(2)
C-->  HOB. �HA�.
       DIN = STMAS(4) - ISDZAD
       DIN1 = STMAS(14) - ISDZAD
C
       INTMAS(1) = INT( INTI, DIN1, DIN )
C
       DSN = STMAS(12) - SZAD
       DSN1 = STMAS(22) - SZAD
C
       INTMAS(2) = INT( INTS, DSN1, DSN )
C
       INTI = INTMAS(1)
C
       INTS = INTMAS(2)
C----------------------------------------------------------------------
C-->  ��P. ��� TN+H0  MOMEHTA BPEMEH�;
C
      UYD0=(URMAS(3)-URMAS(4)*INTI)*(URMAS(2)+1.)-URMAS(2)*STMAS(18)*
     *                                             URMAS(10)
C
      UYQ0=(URMAS(6)-URMAS(7)*STMAS(22)-URMAS(8)*INTS)*(URMAS(2)+1.)-
     =      URMAS(2)*STMAS(17) * URMAS(10)
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