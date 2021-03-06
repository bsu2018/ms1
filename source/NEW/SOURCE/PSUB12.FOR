       SUBROUTINE PSUB12( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT HO��P� 1988 �.)------------
C   �O��PO�PAMMA: PSUB12( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   �AKOHOB ��PAB�EH�� TPEX�A�H�M KOM�EHCATOPOM
C                MO�HOCT� �CKA�EH�� � HEC�MMETP��
C                C �C�O���OBAH�EM ��HE�H�X HA�P��EH��
C                  ( MO�E�� #18 - J3 ��P.)
C   BAP�AHT�:    BO�MO�H� �BA BAP�AHTA MO�E��:
C   ---------  1.�P��HAK MO�E�� = 0, MO�E�� ��HE�HA�,
C                �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C                �HA�EH��M TOKOB � HA�P��EH�� ��EMEHTOB CXEM�
C                �O MOMEHTA BPEMEH� TN BK����TE��HO B �O��PO�PAMME
C                O�PE�E���TC� �HA�EH�� G,J-�APAMETPOB MO�E�� ��� T=TN+1
C              2.�P��HAK MO�E�� = 1, MO�E�� �TEPA��OHHA�,
C                �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C                �HA�EH��M TOKOB � HA�P��EH�� ��EMEHTOB CXEM�
C                �O MOMEHTA BPEMEH� TN+1 BK����TE��HO B �O��PO�PAMME
C                O�PE�E���TC� �HA�EH�� G,J-�APAMETPOB B MOMEHT TN+1
C                �TEPA��OHH�M C�OCO�OM
C   METO�:     O�EB��EH.
C   -----
C  �APAMETP�:   NEL - HOMEP ��EMEHTA, ��� KOTOPO�O PSUB PA�OTAET;
C  ---------    URMAS - MACC�B �AHH�X �O���OBATE��:
C                       1-E C�OBO - KO���ECTBO �APAMETPOB B C��CKE
C                       ( �A��CAHO �E��M ��C�OM B BE�ECTBEHH�� MACC�B);
C                       2-E C�OBO - �P��HAK MO�E�� ( CM.BAP�AHT� );
C                       3-E C�OBO - MOMEHT BK���EH�� MO�E��  T0;
C                       4-E C�OBO - KO� �AKOHA ��PAB�EH�� ( K );
C                       5-E C�OBO - �EP�O� B���C�EH�� �HTE�PA��H�X
C                                   BE����H;
C                       6-E C�OBO - KO���ECTBO TOKOB CXEM�;
C                       7-E C�OBO - KO���ECTBO HA�P��OK;
C                       8-E C�OBO - TO�HOCT� O�PE�E�EH�� �CTAHOB�B-
C                                   �E�OC� PE��MA;
C                       9-E C�OBO - <0 - PAC�ET BE�ETC� �O �CTAHOB.
C                                   PE��MA, B �P.C���AE - B�BO� �HTE�P.
C                                   BE����H �EPE� KA���� �EP�O� �O
C                                   OKOH�AH�� MO�E��POBAH��;
C                      10-E C�OBO - RA - KO������EHT �P� IA**2
C                      11-E C�OBO - RB - KO������EHT �P� IB**2
C                      12-E C�OBO - RC - KO������EHT �P� IC**2
C
C----------------------------------------------------------------------
C              STMAS - MACC�B �AHH�X, �EPE�ABAEM�X B PSUB:
C
C      C�OBO           O�O�HA�EH�E   -   �HA�EH�E
C      1               TN - BPEM� N - �O �A�A PAC�ETA;
C      2               H0 - �A� �HTE�P�POBAH��;
C      3               IKA - �HA�EH�E TOKA �A��  A  KOM�EHCATOPA,
C                            �ATPE�OBAHHOE �O���OBATE�EM
C                            ��� MOMEHTA BPEMEH� TN;
C      4               IKB - �HA�EH�E TOKA �A��  B  KOM�EHCATOPA,
C      5               IKC - �HA�EH�E TOKA �A��  C  KOM�EHCATOPA,
C      6               IHA - �HA�EH�E TOKA ��HE�HO�O �POBO�A A HA�P��K�
C      7               IHB - �HA�EH�E TOKA ��HE�HO�O �POBO�A B HA�P��K�
C      8               IHC - �HA�EH�E TOKA ��HE�HO�O �POBO�A C HA�P��K�
C      9               UAB - ��HE�HOE HA�P��EH�E  AB
C     10               UBC - ��HE�HOE HA�P��EH�E  BC
C     11               UCA - ��HE�HOE HA�P��EH�E  CA
C     12               UAB - HA�P��EH�E ��� PAC�ETA MO�HOCT� HA�P��K�
C     13               UBB - = 0
C     14               UCB - HA�P��EH�E ��� PAC�ETA MO�HOCT� HA�P��K�
C
C----------------------------------------------------------------------
C         DGMAS(1) - �P�XO��T HOMEP �A�POCA,
C                    EC�� DGMAS(1)=1, TO �TO �EPB�� BXO� B
C                    �O��PO�PAMM� PSUB HA �AHHOM �A�E,
C                    EC�� DGMAS(1)=2, TO �TO HE �EPBA� �TEPA���
C                    B�XO��T �P��HAK MO�E��
C         DGMAS(2) - �P�XO��T �HA�EH�E TOKA  IKA  �A��  A   MO�E�� B
C                    MOMEHT TN+1, EC�� HOMEP �A�POCA = 1 ( DGMAS(1) ),
C                    �TO �HA�EH�E TOKA, B���C�EHHOE HA �PE�����EM �A�E,
C                    EC�� HOMEP �A�POCA = 2 ( DGMAS(1) ), TO  �TO
C                    �HA�EH�E TOKA, B���C�EHHOE HA �PE�����E� �TEPA���
C                    �AHHO�O �A�A, �A�EE AHA�O���HO,
C         DGMAS(3) - AHA�O���HO  IKB,
C         DGMAS(4) - AHA�O���HO  IKC,
C         DGMAS(5) - AHA�O���HO  IK0,
C         DGMAS(6) - TOK �CTO�H�KA C�HTET��ECKO� CXEM� �AME�EH��  J12,
C         DGMAS(7) - TOK �CTO�H�KA C�HTET��ECKO� CXEM� �AME�EH��  J13,
C         DGMAS(8) - TOK �CTO�H�KA C�HTET��ECKO� CXEM� �AME�EH��  J23,
C         DGMAS(9) - TOK �CTO�H�KA C�HTET��ECKO� CXEM� �AME�EH��  J14,
C         DGMAS(10)- TOK �CTO�H�KA C�HTET��ECKO� CXEM� �AME�EH��  J24,
C         DGMAS(11)- TOK �CTO�H�KA C�HTET��ECKO� CXEM� �AME�EH��  J34,
C         DGMAS(12)- �POBO��MOCT�  C�HTET��ECKO� CXEM� �AME�EH��  G12,
C         DGMAS(13)- �POBO��MOCT�  C�HTET��ECKO� CXEM� �AME�EH��  G13,
C         DGMAS(14)- �POBO��MOCT�  C�HTET��ECKO� CXEM� �AME�EH��  G23,
C         DGMAS(15)- �POBO��MOCT�  C�HTET��ECKO� CXEM� �AME�EH��  G14,
C         DGMAS(16)- �POBO��MOCT�  C�HTET��ECKO� CXEM� �AME�EH��  G24,
C         DGMAS(17)- �POBO��MOCT�  C�HTET��ECKO� CXEM� �AME�EH��  G34,
C         DGMAS(18)-
C         DGMAS(19)-
C         DGMAS(20)-           P  E  �  E  P  B
C         DGMAS(21)-
C         DGMAS(22)-
C----------------------------------------------------------------------
C              INTMAS - MACC�B �� �B�XCOT (200) ��EMEHTOB,�C�O����ETC�
C                       B �O��PO�PAMME ��� �A�OM�HAH�� PE����TATOB
C                       �POME��TO�H�X B���C�EH��.
C----------------------------------------------------------------------
C
C   �O��PO�PAMM�: HET
C   -------------
C
C   �P�ME�AH�E: C�MMA KO����ECTBA TPE��EM�X TOKOB � HA�P��EH��
C   ----------  HE �O��HA �PEB��AT� 48.
C               ( KOHTPO�� HE �PO��BO��TC� )
C----------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       REAL INTMAS
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( NWTR ,MASPRT(13) )
       EQUIVALENCE ( NWTRE,MASPRT(14) )
       EQUIVALENCE ( TMAX ,MASPRT(43) )
C
C
C
C
C
C
C
C
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
       T0 = URMAS(3)
       IF(URMAS(2).EQ.0.AND.DGMAS(1).NE.1) RETURN
       DGMAS(1) = URMAS(2)
C----------------------------------------------------------------------
       K=URMAS(4)
       RA=URMAS(10)
       RB=URMAS(11)
       RC=URMAS(12)
C      K - HOMEP �AKOHA ��PAB�EH��
C======================================================================
C--> K=1 IKA = -IHA + PH/(UA**2 + UB**2 + UC**2) * (UAB(T) - UCA(T))/3
C        IKB = -IHB + PH/(UA**2 + UB**2 + UC**2) * (UBC(T) - UAB(T))/3
C        IKC = -IHC + PH/(UA**2 + UB**2 + UC**2) * (UCA(T) - UBC(T))/3,
C    ��E
C                            /T
C        UA**2 = 1/9 * 1/T * ! ( UAB(T) - UCA(T))**2 * DT
C                           0/
C
C                            /T
C        UB**2 = 1/9 * 1/T * ! ( UBC(T) - UAB(T))**2 * DT
C                           0/
C
C                            /T
C        UC**2 = 1/9 * 1/T * ! ( UCA(T) - UBC(T))**2 * DT
C                           0/
C
C======================================================================
C--> K=2 �O CPABHEH�� C BAP�AHTOM K=1 BBE�EHA CTA�����A��� �HEP���
C        HAKO��TE��
C        IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2) * (UAB(T) - UCA(T))/3
C        IKB=-IHB+(PH-PK)/(UA**2+UB**2+UC**2) * (UBC(T) - UAB(T))/3
C        IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2) * (UCA(T) - UBC(T))/3,
C    ��E PK = DW/T , �P��EM DW C MOMEHTA BK���EH��  KOM�EHCATOPA
C
C======================================================================
C--> K=3 ��PAB�EH�E �O SIN U CO CTA�����A��E� �HEP��� �O SIN
C IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2)*UA*SQRT(2)*SIN(OMEGA*T)
C IKB=-IHB+(PH-PK)/(UA**2+UB**2+UC**2)*UB*SQRT(2)*SIN(OMEGA*T-2*PI/3)
C IKC=-IKA-IKB
C
C======================================================================
C--> K=4 ��PAB�EH�E �O SIN U CO CTA�����A��E� �HEP��� �O KP�BO�
C        HA�P��EH��
C IKA = -IHA + PH /(UA**2+UB**2+UC**2)*UA*SQRT(2)*SIN(OMEGA*T) -
C       PK / (UA**2+UB**2+UC**2) * (UAB(T)-UCA(T))/3
C IKB = -IHB + PH /(UA**2+UB**2+UC**2)*UB*SQRT(2)*SIN(OMEGA*T-2*PI/3)-
C       PK / (UA**2+UB**2+UC**2) * (UBC(T)-UAB(T))/3
C IKC = -IKA - IKB
C
C======================================================================
C-->K=5 ��PAB�EH�E �O SIN C �C�O���OBAH�EM CPE�HE�O �HA�EH�� HA�P��EH��
C       ( PABH�E C�MMETP��H�E TOK� ), MO�HOCT� �CT. = CONST
C IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2)*UCP*SQRT(2)*SIN(OMEGA*T)
C IKB=-IHB+(PH-PK)/(UA**2+UB**2+UC**2)*UCP*SQRT(2)*SIN(OMEGA*T-2*PI/3)
C IKC = -IKA - IKB
C
C======================================================================
C-->K=6 ��PAB�EH�E C ��ETOM CO�POT�B�EH�� ��H�� �EPE�A��, �E� O�PATHO�
C       CB���
C       IKA = -IHA - PH*RC/�HAM*UAB - PH*RB/�HAM*UAC
C       IKB = -IHB - PH*RC/�HAM*UAB - PH*RA/�HAM*UBC
C       IKC = -IKA - IKB ,
C       ��E
C       �HAM=RA*(UA**2-2*UB**2-2*UC**2)+RB*(UB**2-2*UA**2-2*UC**2)+
C            RC(UC**2-2*UA**2-2*UB**2)
C       � UA,UB,UC - �E�CTB����E �HA�EH��
C
C
C======================================================================
C-->K=7 ��PAB�EH�E �E� B���C�EH�� �A�H�X HA�P��EH��
C       IKA = -IHA + (IHA**2+IHB**2+IHC**2)/PH * UAB -
C                    (IHA**2+IHB**2+IHC**2)/PH * UCA
C       IKB = -IHB + (IHA**2+IHB**2+IHC**2)/PH * UBC -
C                    (IHA**2+IHB**2+IHC**2)/PH * UAB
C       IKC = - IKA -IKB
C
C======================================================================
C
C
C
C    B C��CKE �APAMETPOB PSUB  �OC�E CTAH�APTH�X �APAMETPOB C�E���T:
C    T - �EP�O� B���C�EH�� �APAMETPOB �AKOHA;
C    NTOKOB - KO���ECTBO BBO��M�X TOKOB;
C    NNAGR - KO���ECTBO HA�P��OK B ��. �E��.
C    EPS - O���KA O�PE�E�EH�� �CTAHOB�B�E�OC� PE��MA
C    M -  BAP�AHT B�BO�A �HTE�PA��H�X �APAMETPOB (< 0  � > = 0 )
C         �P�ME�AH��: 1. KO���ECTBO BBO��M�X HA�P��EH�� �O��HO ��T�
C         -----------    PABHO KO���ECTB� BBO��M�X TOKOB;
C                     2. �OP��OK BBO�A ��� TOKOB ��EMEHTOB � �X HA-
C                        �P��EH�� E��H��;
C                     3. EC�� HA�P��KA COE��HEHA TPE��O��H�KOM,
C                        BBO��TC� ��HE�H�E TOK� � HA�P��EH�� COOTBET-
C                                 ========
C                        CTB����E PAC�ET� PH  METO�OM �B�X BATTMETPOB
C
C         CO�EP�AH�E  STMAS
C         -----------------
C    ------------------------------------------------------------------
C    HOMEP ��EMEHTA                     �HA�EH�E
C    ------------------------------------------------------------------
C    1                                  CTAH�APTH�E �APAMETP� PSUB5
C    2                                         =  "  =  "  =
C    2+I           ( I OT 1 �O NTOKOB)  �HA�EH�� TOKOB ��EMEHTOB
C    2+NTOKOB+I    ( I OT 1 �O NTOKOB)  �HA�EH�� HA�P��EH�� ��EMEHTOB
C    ------------------------------------------------------------------
C
C         CO�EP�AH�E INTMAS
C         -----------------
C    ------------------------------------------------------------------
C    HOMEP ��EMEHTA                     �HA�EH�E
C    1                                  MOMEHT BPEMEH� �PE�����E�O �A�A
C                                       ( TN-1 ) ;
C    2                                  �E�OE KO���ECTBO �EP�O�OB, �PO-
C                                       �E���X C MOMEHTA BK���EH�� KOM-
C                                       �EHCATOPA ( PER ) ;
C    3                                  �HEP���, OT�AHHA� ( �P�H�TA� )
C                                       KOM�EHCATOPOM C HA�A�A �AHHO�O
C                                       �EP�O�A ( TEK��EE DW ) ;
C    4                                  �HEP��� OT�AHHA� ( �P�H�TA� )
C                                       KOM�EHCATOPOM C MOMEHTA BK���E-
C                                       H��  ( T0 ) �A �E�OE ��C�O �PO-
C                                       �E���X �EP�O�OB ;
C    5                                  TEK��EE �HA�EH�E �HTE�PA��HO�
C                                       C�MM�  ��� B���C�EH��  PH
C                                       ( �APAMETPA �AKOHA ��PAB�EH��);
C    6                                  PH - �HA�EH�E �APAMETPA �AKOHA,
C                                       �C�O����EMOE HA �AHHOM �A�E ;
C    7                                  UKA**2 + UKB**2 + UKC**2
C                                       C�MMA KBA�PATOB �E�CTB����X
C                                       �HA�EH�� HA�P��EH�� KOM�EHCATO-
C                                       PA ( �APAMETP �AKOHA ��PAB�EH��
C                                       �C�O����EM�� HA �AHHOM �A�E ) ;
C    8                                  P E � E P B
C    9                                  EC�� INTMAS(9) = 1, TO �PO�ECC
C                                       �CTAHOB�B���C�
C   10                                  UA - �E�CTB.�H HA KOM�EHC.
C   11                                  UB - �E�CTB.�H HA KOM�EHC.
C   12                                  UC - �E�CTB.�H HA KOM�EHC.
C   13                                  CPE�HEE �HA�EH�E �A�H�X HA�P��.
C                                       KOM�EHCATOPA ( UA+UB+UC )/3
C   14                                  C�MMA IHA**2+IHB**2+IHC**2
C   15 - 20                             P E � E P B
C    20 +I          (I OT 1 �O NTOKOB)  TEK���E �HA�EH�� �HTE�PA��H�X
C                                       C�MM ��� B���C�EH�� �E�CTB����X
C                                       �HA�EH�� TOKOB ;
C    20+NTOKOB+I    (I OT 1 �O NTOKOB)  TO �E ��� HA�P��EH�� ;
C    20+2*NTOKOB+I  (I OT 1 �O NTOKOB)  TO �E ��� AKT�BH�X MO�HOCTE� ;
C    ------------------------------------------------------------------
C
C-->  NMOD - �O��CT�MOE KO���ECTBO MO�E�E� �AHHO�O T��A B CXEME
      NMOD = 2
C-->  �PO��BO��TC� PAC�PE�E�EH�E MACC�BA  INTMAS  ���
C     MO�E�E� �AHHO�O T��A  -  PA�H�X ��EMEHTOB CXEM�
C-----------------------------------------------------------------------
 124  DO 122 I=1,NMOD
      II = I
      NEL1 = IFIX(INTMAS(I))
      IF(NEL.EQ.NEL1) GO TO 123
 122  CONTINUE
 127  DO 125 I=1,NMOD
      III=I+1
      NNEL = IFIX(INTMAS(NMOD+1))
      IF(NNEL.EQ.I) GO TO 126
 125  CONTINUE
      INTMAS(NMOD+1) = 1.
      II = 1
      INTMAS(1) = FLOAT(NEL)
      GO TO 123
 126  INTMAS(III) = FLOAT(NEL)
      INTMAS(NMOD+1) = INTMAS(NMOD+1) + 1
      NNEL = IFIX(INTMAS(NMOD+1))
      IF(NNEL.GT.NMOD) GO TO 128
      GO TO 124
 128  WRITE(NWTRE,170) NMOD
      TMAX = TN
      RETURN
 123  NJ = (II - 1) *  98 + NMOD + 1
C----------------------------------------------------------------------
      IF(TN.LE.T0) GO TO 103
 104  IF(TN.LT.T0) RETURN
      NTOKOB = URMAS(6)
      IF(DGMAS(1).EQ.2) GO TO 102
      T=URMAS(5)
      EPS = URMAS(8)
      NNAGR=URMAS(7)
      PI=3.141593
      DT=TN-INTMAS(NJ+1)
      PER=AINT((TN-T0)/T)
      IF(PER.GT.INTMAS(NJ+2)) GO TO 105
C----------------------------------------------------------------------
C-->  B���C�EH�E �HTE�PA��HO� C�MM� ��� PH
 106  INTMAS(NJ+5) = INTMAS(NJ+5) + ( STMAS(6) * STMAS(2+NTOKOB+4) +
     *STMAS(7) * STMAS(2+NTOKOB+5) + STMAS(8) * STMAS(2+NTOKOB+6)) * DT
C-->  B���C�EH�E �HTE�PA��H�X C�MM �A�H�X HA�P��EH�� UA,UB,UC
C     UA ---> ( UAB - UCA )
      INTMAS(NJ+20+NTOKOB+1) = INTMAS(NJ+20+NTOKOB+1) +
     *( STMAS(2+NTOKOB+1) - STMAS(2+NTOKOB+3) )**2 * DT
C     UB ---> ( UBC - UAB )
      INTMAS(NJ+20+NTOKOB+2) = INTMAS(NJ+20+NTOKOB+2) +
     *( STMAS(2+NTOKOB+2) - STMAS(2+NTOKOB+1) )**2 * DT
C     UC ---> ( UCA - UBC )
      INTMAS(NJ+20+NTOKOB+3) = INTMAS(NJ+20+NTOKOB+3) +
     *( STMAS(2+NTOKOB+3) - STMAS(2+NTOKOB+2) )**2 * DT
C-->  B���C�EH�E �HTE�PA��HO� C�MM� ��� �HEP��� KOM�EHCATOPA
      INTMAS(NJ+3) = INTMAS(NJ+3) + ( STMAS(3) * STMAS(2+NTOKOB+1) +
     *STMAS(4) * STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) * DT
C----------------------------------------------------------------------
 116  IF(PER.GT.INTMAS(NJ+2)) GO TO 107
C-->  �A�OMH�M BPEM� �TO�O �A�A HA �����EE
      INTMAS(NJ+1)=INTMAS(NJ+1)+DT
C======================================================================
C     ��ABHA� �OPM��A �TEPA���, B���C�EH�E J,G-�APAMETPOB KOM�EHCATOPA
 102  GO TO (1,2,3,4,5,6),K
C----------------------------------------------------------------------
   1  IF(INTMAS(NJ+7)) 138,131,129
 131  DO 136 I = 6,17
 136  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 132
 129  DGMAS(6) = 0.
      DGMAS(7) = - STMAS(6)
      DGMAS(8) = - STMAS(7)
      DGMAS(9) = 0.
      DGMAS(10)= 0.
      DGMAS(11)= 0.
      DGMAS(12)= INTMAS(NJ+6) / INTMAS(NJ+7) / 3.
      DGMAS(13)= INTMAS(NJ+6) / INTMAS(NJ+7) / 3.
      DGMAS(14)= INTMAS(NJ+6) / INTMAS(NJ+7) / 3.
      DGMAS(15)= 0.
      DGMAS(16)= 0.
      DGMAS(17)= 0.
      GO TO 132
 138  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
 132  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = INTMAS(NJ+4)+ INTMAS(NJ+3)
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
C----------------------------------------------------------------------
   2  IF(INTMAS(NJ+7)) 238,231,229
 231  DO 236 I = 6,17
 236  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 232
 229  DGMAS(6) = 0.
      DGMAS(7) = - STMAS(6)
      DGMAS(8) = - STMAS(7)
      DGMAS(9) = 0.
      DGMAS(10)= 0.
      DGMAS(11)= 0.
      DGMAS(12)= (INTMAS(NJ+6) - INTMAS(NJ+4)/T) / INTMAS(NJ+7) / 3.
      DGMAS(13)= (INTMAS(NJ+6) - INTMAS(NJ+4)/T) / INTMAS(NJ+7) / 3.
      DGMAS(14)= (INTMAS(NJ+6) - INTMAS(NJ+4)/T) / INTMAS(NJ+7) / 3.
      DGMAS(15)= 0.
      DGMAS(16)= 0.
      DGMAS(17)= 0.
      GO TO 232
 238  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
 232  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = INTMAS(NJ+4) + INTMAS(NJ+3)
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
C----------------------------------------------------------------------
   3  IF(INTMAS(NJ+7)) 338,331,329
 331  DO 336 I = 6,17
 336  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 332
 329  DGMAS(6) = 0.
      DGMAS(7) = - STMAS(6)+(INTMAS(NJ+6)-INTMAS(NJ+4)/T)/INTMAS(NJ+7)
     * *INTMAS(NJ+10) * SQRT(2.) * SIN(2.*PI*50.*TN)
      DGMAS(8) = - STMAS(7)+(INTMAS(NJ+6)-INTMAS(NJ+4)/T)/INTMAS(NJ+7)
     * *INTMAS(NJ+11) * SQRT(2.) * SIN(2.*PI*50.*TN-2.*PI/3.)
      DGMAS(9) = 0.
      DGMAS(10)= 0.
      DGMAS(11)= 0.
      DGMAS(12)= 0.
      DGMAS(13)= 0.
      DGMAS(14)= 0.
      DGMAS(15)= 0.
      DGMAS(16)= 0.
      DGMAS(17)= 0.
      GO TO 332
 338  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
 332  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = 0.
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
C----------------------------------------------------------------------
   4  IF(INTMAS(NJ+7)) 438,431,429
 431  DO 436 I = 6,17
 436  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 432
 429  DGMAS(6) = 0.
      DGMAS(7) = - STMAS(6)+ INTMAS(NJ+6) / INTMAS(NJ+7)
     * * INTMAS(NJ+10) * SQRT(2.) * SIN(2.*PI*50.*TN)
      DGMAS(8) = - STMAS(7)+ INTMAS(NJ+6) / INTMAS(NJ+7)
     * * INTMAS(NJ+11) * SQRT(2.) * SIN(2.*PI*50.*TN-2.*PI/3.)
      DGMAS(9) = 0.
      DGMAS(10)= 0.
      DGMAS(11)= 0.
      DGMAS(12)=  - INTMAS(NJ+4)/ T / INTMAS(NJ+7) / 3.
      DGMAS(13)=  - INTMAS(NJ+4)/ T / INTMAS(NJ+7) / 3.
      DGMAS(14)=  - INTMAS(NJ+4)/ T / INTMAS(NJ+7) / 3.
      DGMAS(15)= 0.
      DGMAS(16)= 0.
      DGMAS(17)= 0.
      GO TO 432
 438  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
 432  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = 0.
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
C----------------------------------------------------------------------
   5  IF(INTMAS(NJ+7)) 538,531,529
 531  DO 536 I = 6,17
 536  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 532
 529  DGMAS(6) = 0.
      DGMAS(7) = - STMAS(6)+(INTMAS(NJ+6)-INTMAS(NJ+4)/T)/INTMAS(NJ+7)
     * *INTMAS(NJ+13) * SQRT(2.) * SIN(2.*PI*50.*TN)
      DGMAS(8) = - STMAS(7)+(INTMAS(NJ+6)-INTMAS(NJ+4)/T)/INTMAS(NJ+7)
     * *INTMAS(NJ+13) * SQRT(2.) * SIN(2.*PI*50.*TN-2.*PI/3.)
      DGMAS(9) = 0.
      DGMAS(10)= 0.
      DGMAS(11)= 0.
      DGMAS(12)= 0.
      DGMAS(13)= 0.
      DGMAS(14)= 0.
      DGMAS(15)= 0.
      DGMAS(16)= 0.
      DGMAS(17)= 0.
      GO TO 532
 538  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
 532  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = 0.
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
C----------------------------------------------------------------------
   6  IF(INTMAS(NJ+7)) 638,631,629
 631  DO 636 I = 6,17
 636  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 632
 629  UA = INTMAS(NJ+10)
      UB = INTMAS(NJ+11)
      UC = INTMAS(NJ+12)
      ZNAM=RA*(UA**2-2.*UB**2-2.*UC**2)+RB*(UB**2-2.*UA**2-2.*UC**2)+
     *RC*(UC**2-2.*UA**2-2.*UB**2)
C
      DGMAS(6) = 0.
      DGMAS(7) = - STMAS(6)
C
      DGMAS(8) = - STMAS(7)
C
      DGMAS(9) = 0.
      DGMAS(10)= 0.
      DGMAS(11)= 0.
      DGMAS(12)= -INTMAS(NJ+6)*RC/ZNAM
      DGMAS(13)= -INTMAS(NJ+6)*RB/ZNAM
      DGMAS(14)= -INTMAS(NJ+6)*RA/ZNAM
      DGMAS(15)= 0.
      DGMAS(16)= 0.
      DGMAS(17)= 0.
      GO TO 632
 638  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
 632  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = 0.
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
C----------------------------------------------------------------------
7     CONTINUE
      IF(INTMAS(NJ+6)) 738,731,729
 731  DO 736 I = 6,17
 736  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 732
 729  DGMAS(6) = 0.
      DGMAS(7) = - STMAS(6)
      DGMAS(8) = - STMAS(7)
      DGMAS(9) = 0.
      DGMAS(10)= 0.
      DGMAS(11)= 0.
      DGMAS(12)= INTMAS(NJ+14) / INTMAS(NJ+6)
      DGMAS(13)= INTMAS(NJ+14) / INTMAS(NJ+6)
      DGMAS(14)= INTMAS(NJ+14) / INTMAS(NJ+6)
      DGMAS(15)= 0.
      DGMAS(16)= 0.
      DGMAS(17)= 0.
      GO TO 732
 738  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
 732  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = INTMAS(NJ+4)+ INTMAS(NJ+3)
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
C----------------------------------------------------------------------
 103  IH = NJ + 1
      IB = NJ+98
      DO 114 I=IH,IB
 114  INTMAS(I) = 0.
      INTMAS(NJ+1) =T0
      DO 135 I = 6,22
 135  DGMAS(I) = 0.
      DGMAS(12) = 1.E-6
      DGMAS(13) = 1.E-6
      DGMAS(14) = 1.E-6
C     DGMAS(15) = 1.E-6
C     DGMAS(16) = 1.E-6
C     DGMAS(17) = 1.E-6
      GO TO 104
C-->  �AKOH��M B���C�EH�� ��� �PE�����E�O �EP�O�A
 105  DT1=T0+PER*T-INTMAS(NJ+1)
      DT2=DT-DT1
      DT=DT1
      GO TO 106
 107  INTMAS(NJ+2)=PER
      INTMAS(NJ+4) = INTMAS(NJ+4) + INTMAS(NJ+3)
C----------------------------------------------------------------------
C-->  �POBEPKA HA �CTAHOB�B���C� PE��M
      PNOB = INTMAS(NJ+5)/T
      UNOB = (INTMAS(NJ+20+NTOKOB+1) + INTMAS(NJ+20+NTOKOB+2)
     * + INTMAS(NJ+20+NTOKOB+3)) / T / 9.
      UA = SQRT(INTMAS(NJ+20+NTOKOB +1)/T) / 3.
      UB = SQRT(INTMAS(NJ+20+NTOKOB +2)/T) / 3.
      UC = SQRT(INTMAS(NJ+20+NTOKOB +3)/T) / 3.
      UCP = ( UA+UB+UC ) / 3.
      DIHA = INTMAS(NJ+20+4)/T
      DIHB = INTMAS(NJ+20+5)/T
      DIHC = INTMAS(NJ+20+6)/T
      SUMI = DIHA + DIHB + DIHC
      IF(PNOB.LE.1.E-32.OR.UNOB.LE.1.E-32) GO TO 111
      EPS1 = ABS(INTMAS(NJ+4) - INTMAS(NJ+3))*0.1
      IF(EPS1.GT.EPS) GO TO 108
      EPS2 = ABS((PNOB - INTMAS(NJ+6))/PNOB)
      IF(EPS2.GT.EPS) GO TO 108
      EPS3 = ABS((UNOB - INTMAS(NJ+7))/UNOB)
      IF(EPS3.GT.EPS) GO TO 108
C----------------------------------------------------------------------
C-->  �CTAHOB�B���C� PE��M
      INTMAS(NJ+9) = 1.
 133  WRITE(NWTR,160)
      WRITE(NWTR,130)PER,STMAS(1),INTMAS(NJ+4)
C**********************************************************************
C �E�AT� INTMAS                                O T � A � K A
C     WRITE(NWTR,200)
C     WRITE(NWTR,300)(INTMAS(I),I=1,200)
C200  FORMAT(//20X,'INTMAS       PSUB12'/)
C300  FORMAT(10E12.3)
C**********************************************************************
      IF(URMAS(9).GE.0.) GO TO 134
      TMAX = TN
      RETURN
 111  WRITE(NWTRE,110)
C----------------------------------------------------------------------
C-->  �PO�ECC HE �CTAHOB��C�
C     �A���EM HOB�E �HA�EH�� �APAMETPOB �AKOHA ��PAB�EH��
 108  IF(URMAS(9).GE.0.) GO TO 133
 134  INTMAS(NJ+6) = PNOB
      INTMAS(NJ+7) = UNOB
      INTMAS(NJ+10) = UA
      INTMAS(NJ+11) = UB
      INTMAS(NJ+12) = UC
      INTMAS(NJ+13) = UCP
      INTMAS(NJ+14) = SUMI
C-->  HA�HEM HAKO��EH�E �HTE�PA��H�X C�MM C HA�A�A
C-->  B���C�EH�E �HTE�PA��HO� C�MM� ��� PH
      INTMAS(NJ+5) = ( STMAS(6) * STMAS(2+NTOKOB+4) +
     *STMAS(7) * STMAS(2+NTOKOB+5) + STMAS(8) * STMAS(2+NTOKOB+6)) *DT2
C-->  B���C�EH�E �HTE�PA��H�X C�MM �A�H�X HA�P��EH�� UA,UB,UC
C     UA ---> ( UAB - UCA )
      INTMAS(NJ+20+NTOKOB+1) =
     *( STMAS(2+NTOKOB+1) - STMAS(2+NTOKOB+3) )**2 * DT2
C     UB ---> ( UBC - UAB )
      INTMAS(NJ+20+NTOKOB+2) =
     *( STMAS(2+NTOKOB+2) - STMAS(2+NTOKOB+1) )**2 * DT2
C     UC ---> ( UCA - UBC )
      INTMAS(NJ+20+NTOKOB+3) =
     *( STMAS(2+NTOKOB+3) - STMAS(2+NTOKOB+2) )**2 * DT2
C-->  B���C�EH�E �HTE�PA��HO� C�MM� ��� �HEP��� KOM�EHCATOPA
      INTMAS(NJ+3) = ( STMAS(3) * STMAS(2+NTOKOB+1) +
     *STMAS(4) * STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) *DT2
      GO TO 102
 110  FORMAT(1X,'BO�MO�HO KOPOTKOE �AM�KAH�E HA�P��K�')
 130  FORMAT(1X,E12.5,20X,E12.5,20X,E12.5)
 160  FORMAT(1X,'BPEM� �EPEX. PE��MA KM� (�EP�O��)',5X,'BPEM� �A�A B�BO
     *�A TN',5X,'�HEP���,�P�H�TA� KOM�EHCATOPOM')
 170  FORMAT(1X,'KO���ECTBO MO�E�E� T��A 17 ��� 18 �PEB�C��O �O��CT�MOE
     *( ',I2 ,' )')
 180  FORMAT(1X,'�APAMETP  (UKA**2+UKB**2+UKC**2) < 0  ')
      END
