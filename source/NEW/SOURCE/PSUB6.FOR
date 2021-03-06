       SUBROUTINE PSUB6( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT HO��P�  1988 �.)------------
C   �O��PO�PAMMA: PSUB6( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   �AKOHOB ��PAB�EH��  KOM�EHCATOPOM
C                MO�HOCT� �CKA�EH�� ( AKT�BH�M ����TPOM B� )
C                  ( MO�E�� #17 - J ��P.)
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
C----------------------------------------------------------------------
C              STMAS - MACC�B �AHH�X, �EPE�ABAEM�X B PSUB:
C
C      C�OBO           O�O�HA�EH�E   -   �HA�EH�E
C      1               TN - BPEM� N - �O �A�A PAC�ETA;
C      2               H0 - �A� �HTE�P�POBAH��;
C      3               I1 - �HA�EH�E 1-�O TOKA, �ATPE�OBAHHO�O
C                           �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C         �A�EE AHA�O���HO �HA�EH�� OCTA��H�X TOKOB;
C   N=3+NTOKOB       U1 - �HA�EH�E 1-�O HA�P��EH��, �ATPE�OBAHHO�O
C                           �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C         �A�EE AHA�O���HO �HA�EH�� OCTA��H�X HA�P��EH��;
C
C----------------------------------------------------------------------
C              DGMAS(1) - �P�XO��T HOMEP �A�POCA,
C                         EC�� DGMAS(1)=1, TO �TO �EPB�� BXO� B
C                         �O��PO�PAMM� PSUB HA �AHHOM �A�E,
C                         EC�� DGMAS(1)=2, TO �TO HE �EPBA� �TEPA���
C                         B�XO��T �P��HAK MO�E��
C              DGMAS(2) - �P�XO��T �HA�EH�E TOKA MO�E�� B MOMEHT TN+1,
C                         EC�� HOMEP �A�POCA = 1 ( DGMAS(1) ), TO
C                         �TO �HA�EH�E TOKA MO�E��, B���C�EHHOE HA
C                         �PE�����EM �A�E,
C                         EC�� HOMEP �A�POCA = 2 ( DGMAS(1) ), TO
C                         �TO �HA�EH�E TOKA MO�E��, B���C�EHHOE HA
C                         �PE�����E� �TEPA��� �AHHO�O �A�A
C                         B�XO��T  J-�APAMETP (�CT.TOKA C�HTET��.CXEM�)
C              DGMAS(3) - G-�APAMETP (�POBO��MOCT� C�HTET��ECKO� CXEM�)
C              DGMAS(4) - PH- AKT�BHA� MO�HOCT� KOM�EHC�P�EMO� HA�P��K�
C              DGMAS(5) - QO� - O�MEHHA� MO�HOCT� KOM�EHCATOPA
C
C
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
       REAL INTMAS,KPD,KMH,KMK
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
       T0 = URMAS(3)
       IF(URMAS(2).EQ.0.AND.DGMAS(1).NE.1) RETURN
       DGMAS(1) = URMAS(2)
C----------------------------------------------------------------------
       K=URMAS(4)
C      K - HOMEP �AKOHA ��PAB�EH��
C--> �P� �A��C� �OPM�� �AKOHOB ��PAB�EH�� �C�O���OBAHO COKPA�EH�E "PK"
C    �TO OTHO�EH�E DW(�E��TA W) ��MEHEH�� �HEP��� KOM�EHCATOPA C
C    MOMEHTA BK���EH�� �A �E�OE ��C�O �EP�O�OB K ���TE��HOCT� �EP�O�A T
C======================================================================
C--> K=1        �AKOH FRIZE  IK=-IH+PH/UK**2*UK(T)
C          �E� CTA�����A��� �HEP��� HAKO��TE��
C    K=2        �AKOH FRIZE  IK=-IH+(PH-PK)/UK**2*UK(T)
C          CO CTA�����A��E� �HEP��� �A �EP�O�
C    K=3        IK=-IH+PH/UK**2*UK*SQRT(2.)*SIN(OMEGA*T)
C          �E� CTA�����A��� �HEP��� HAKO��TE��
C    K=4        IK=-IH+PH/UK*SQRT(2.)*SIN(OMEGA*TN)-PK/UK**2*UK(T)
C          CTA�����A��� �O KP�BO� HA�P��EH��
C    K=5        IK=-IH+(PH-PK)/UK*SQRT(2.)*SIN(OMEGA*T)
C          CTA�����A��� �O SIN �AKOH�
C    K=6        IK=-IH+PH*(1.-COS(2.*OMEGA*T)/UK(T)
C          ��P. �O PO�M �E� CTA�����A���
C    K=7        IK=-IH+PH*(1.-COS(2.*OMEGA*T)/UK(T)-PK/UK**2*UK(T)
C          PO�M CO CTA�����A��E� �O KP�BO� HA�P��EH��
C    K=8        IK=-IH+(PH-PK)*(1.-COS(2.*OMEGA*T))/UK(T)
C          PO�M  CO CTA�����A��E� �O KP�BO� MO�HOCT� OT SIN
C    K=9        IK=0.
C          PAC�ET �HTE�PA��H�X �APAMETPOB PE��MA �E� KOM�EHCA���
C    K=10  PAC�ET �O PO�M �E� O��A�OTK� �E�EH�� HA H���
C
C    K=11  PAC�ET TO��KO J-�APAMETPA, G = 0.
C
C    K=12  �O PO�M CO CTA���. �O KP�BO� MO�HOCT�
C                IK = -IH + (PH-PK) * (1-COS(2*WT))/UK(T)
C    K=13  �O PO�M CO CTA���. �O KP�BO� MO�HOCT�
C                TO �E.......PH+PK
C    K=14        IK = -IH + IH**2/(PH+PK)
C
C    B C��CKE �APAMETPOB PSUB5 �OC�E CTAH�APTH�X �APAMETPOB C�E���T:
C    T - �EP�O� B���C�EH�� �APAMETPOB �AKOHA;
C    NTOKOB - KO���ECTBO BBO��M�X TOKOB;
C    NNAGR - KO���ECTBO HA�P��OK B ��. �E��.
C    EPS - O���KA O�PE�E�EH�� �CTAHOB�B�E�OC� PE��MA
C    M -  BAP�AHT B�BO�A �HTE�PA��H�X �APAMETPOB ( 0  � HE 0 )
C         �P�ME�AH��: 1. KO���ECTBO BBO��M�X HA�P��EH�� �O��HO ��T�
C         -----------    PABHO KO���ECTB� BBO��M�X TOKOB;
C                     2. �OP��OK BBO�A ��� TOKOB ��EMEHTOB � �X HA-
C                        �P��EH�� E��H��;
C                     3. �EPB�M� B C��CKAX BBO�A C�E���T TOK � HA�P�-
C                        �EH�E KOM�EHCATOPA, BTOP�M� - TOK � HA�P��E-
C                        H�E KOM�EHC�P�EMO� HA�P��K�, �A�EE - TOK� �
C                        HA�P��EH�� OCTA��H�X HA�P��OK, �A�EE - TOK�
C                        � HA�P��EH�� OCTA��H�X ��EMEHTOB.
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
C    7                                  UH**2 - KBA�PAT �E�CTB���E�O
C                                       �HA�EH�� HA�P��EH�� KOM�EHCATO-
C                                       PA ( �APAMETP �AKOHA ��PAB�EH��,
C                                       �C�O����EM�� HA �AHHOM �A�E ) ;
C    8                                  TEK��EE �HA�EH�E �HTE�PA��HO�
C                                       C�MM� (IK+IH)**2*DT, ��� B���-
C                                       C�EH�� KO������EHTA MO�HOCT�
C                                       HA�P��K� C KOM�EHCATOPOM ;
C    9                                  EC�� INTMAS(9) = 1, TO �PO�ECC
C                                       �CTAHOB�B���C�
C   10                                  T * P+  - COCTAB����A� O�MEHHO�
C                                       MO�HOCT� KOM�EHCATOPA
C   11                                  T * P-  - TO �E
C   12                                  T+  - �HTEPBA� �O�O��TE��H�X
C                                       �HA�EH�� M�H.MO�HOCT� KOM�-PA
C   13                                  T-  - �HTEPBA� OTP��ATE��H�X
C                                       �HA�EH�� M�H.MO�HOCT� KOM�-PA
C   14                                  QO�  - O�MEHHA� MO�HOCT� KOM�.
C   15                                  UK**2 * T - ��� CTA�����A���
C                                       �A�ACA �HEP��� KOM�EHCATOPA
C   16                                  DW �A 1-�� �EP�O� PA�OT� KM�
C   17                                  IH**2 - �APAMETP �AKOHA
C                                       ��PAB�EH��, HA �AHHOM �A�E
C   18                                  �HTE�P.C�MMA P(H+K)
C   19                                  P(H+K) - �APAMETP �AKOHA ��P.
C   20                                  TEK��.�H.�HTE�P.C�MM� TOKA HA�P.
C    20 +I          (I OT 1 �O NTOKOB)  TEK���E �HA�EH�� �HTE�PA��H�X
C                                       C�MM ��� B���C�EH�� �E�CTB����X
C                                       �HA�EH�� TOKOB ;
C    20+NTOKOB+I    (I OT 1 �O NTOKOB)  TO �E ��� HA�P��EH�� ;
C    20+2*NTOKOB+I  (I OT 1 �O NTOKOB)  TO �E ��� AKT�BH�X MO�HOCTE� ;
C    ------------------------------------------------------------------
C-->  NMOD - �O��CT�MOE KO���ECTBO MO�E�E� �AHHO�O T��A B CXEME
      NMOD = 3
C-->  �PO��BO��TC� PAC�PE�E�EH�E MACC�BA  INTMAS  ���
C     MO�E�E� �AHHO�O T��A  -  PA�H�X ��EMEHTOB CXEM�
C-----------------------------------------------------------------------
 124  DO 122 I=1,NMOD
      II = I
      NEL1 = IFIX(INTMAS(I))
      IF(NEL.EQ.NEL1) GO TO 123
 122  CONTINUE
 127  DO 125 I=1,NMOD
      III=I + 1
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
 123  NJ = (II - 1) * 65  + NMOD + 1
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
C-->  B���C�EH�E �HTE�PA��HO� C�MM� ��� PH
 106  INTMAS(NJ+5) = INTMAS(NJ+5)+STMAS(4)*STMAS(2+NTOKOB+1)*DT
      INTMAS(NJ+8) = INTMAS(NJ+8) + (STMAS(3) + STMAS(4))**2*DT
      INTMAS(NJ+18) = INTMAS(NJ+18) + (STMAS(4)+STMAS(3))*
     *STMAS(2+NTOKOB+1)*DT
      INTMAS(NJ+20) = INTMAS(NJ+20) + STMAS(4)**2*DT
C     B���C�EH�E OCTA��H�X �HTE�PA��H�X C�MM
      DO 101 I=1,NTOKOB
      INTMAS(NJ+20+I) = INTMAS(NJ+20+I) + STMAS(2+I)**2*DT
      INTMAS(NJ+20+NTOKOB+I)=INTMAS(NJ+20+NTOKOB+I)+STMAS(2+NTOKOB+I)
     ***2*DT
 101  INTMAS(NJ+20+2*NTOKOB+I)=INTMAS(NJ+20+2*NTOKOB+I)+STMAS(2+I)*
     *STMAS(2+NTOKOB+I)*DT
C-->  B���C�EH�E �HTE�PA��HO� C�MM� ��� �HEP��� KOM�EHCATOPA
      INTMAS(NJ+3) = INTMAS(NJ+20+2*NTOKOB+1)
C----------------------------------------------------------------------
C-->  B���C�EH�E O�MEHHO� MO�HOCT� KOM�EHCATOPA  QO�
      IF(STMAS(2+1)*STMAS(2+NTOKOB+1)) 121,115,115
 115  INTMAS(NJ+10) = INTMAS(NJ+10) + STMAS(2+1)*STMAS(2+NTOKOB+1)*DT
      INTMAS(NJ+12) = INTMAS(NJ+12) + DT
      GO TO 116
 121  INTMAS(NJ+11) = INTMAS(NJ+11) + STMAS(2+1)*STMAS(2+NTOKOB+1) *DT
      INTMAS(NJ+13) = INTMAS(NJ+13) + DT
C----------------------------------------------------------------------
 116  IF(PER.GT.INTMAS(NJ+2)) GO TO 107
C-->  �A�OMH�M BPEM� �TO�O �A�A HA �����EE
      INTMAS(NJ+1)=INTMAS(NJ+1)+DT
C======================================================================
C     ��ABHA� �OPM��A �TEPA���, B���C�EH�E TOKA KOM�EHCATOPA
 102  GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14),K
      WRITE(NWTR,210) K
      TMAX=TN
      RETURN
C----------------------------------------------------------------------
   1  IF(INTMAS(NJ+7)) 129,131,129
 131  DGMAS(2) = 0.
      DGMAS(3) = 0.
      GO TO 132
 129  DGMAS(2) = -STMAS(4)
      DGMAS(3) = INTMAS(NJ+6)/INTMAS(NJ+7)
      GO TO 132
C----------------------------------------------------------------------
   2  IF(INTMAS(NJ+7)) 229,231,229
 231  DGMAS(2) =0.
      DGMAS(3) =0.
      GO TO 132
 229  DGMAS(2) = -STMAS(4)
      DGMAS(3) = INTMAS(NJ+6)/INTMAS(NJ+7)
      IF(INTMAS(NJ+15).NE.0.) DGMAS(3)=DGMAS(3)-INTMAS(NJ+4)/
     *INTMAS(NJ+15)
      GO TO 132
C----------------------------------------------------------------------
   3  DGMAS(3) = 0.
      IF(INTMAS(NJ+7)) 329,331,329
 331  DGMAS(2) = 0.
      GO TO 132
 329  DGMAS(2) = -STMAS(4)+INTMAS(NJ+6)/SQRT(INTMAS(NJ+7))*SQRT(2.)*
     *SIN(2.*PI*50.*TN)
      GO TO 132
C----------------------------------------------------------------------
   4  DGMAS(3) = 0.
      IF(INTMAS(NJ+15).NE.0.) DGMAS(3) = INTMAS(NJ+4)/INTMAS(NJ+15)
      IF(INTMAS(NJ+7)) 429,431,429
 431  DGMAS(2) = 0.
      GO TO 132
 429  DGMAS(2) = -STMAS(4)+INTMAS(NJ+6)/SQRT(INTMAS(NJ+7))*SQRT(2.)*
     *SIN(2.*PI*50.*TN)
      GO TO 132
C----------------------------------------------------------------------
   5  DGMAS(3) = 0.
      IF(INTMAS(NJ+7)) 529,531,529
 531  DGMAS(2) = 0.
      GO TO 132
 529  DGMAS(2) = -STMAS(4)+(INTMAS(NJ+6)/SQRT(INTMAS(NJ+7))-
     *INTMAS(NJ+4)/INTMAS(NJ+15))*SQRT(2.)*SIN(2.*PI*50.*TN)
      GO TO 132
C----------------------------------------------------------------------
   6  DGMAS(2) = -STMAS(4)
      DG1 = 1. - COS(2.*2.*PI*50.*TN)
      DG2 = STMAS(2+NTOKOB+1)**2
      IF(DG1.GT.DG2.AND.DG2.LT.1.E-10) GO TO 132
      DG1 = DG1*INTMAS(NJ+6)
      IF(INTMAS(NJ+6).EQ.0.) DGMAS(2) = 0.
      DGMAS(3) = DG1/DG2
      GO TO 132
C----------------------------------------------------------------------
   7  DGMAS(2) = -STMAS(4)
      DG1 = 1. - COS(2.*2.*PI*50.*TN)
      DG2 = STMAS(2+NTOKOB+1)**2
      IF(DG1.GT.DG2.AND.DG2.LT.1.E-10) GO TO 132
      DG1 = DG1*INTMAS(NJ+6)
      IF(INTMAS(NJ+6).EQ.0.) DGMAS(2) = 0.
      DGMAS(3) = DG1/DG2
      IF(INTMAS(NJ+15).NE.0.) DGMAS(3) = DGMAS(3) - INTMAS(NJ+4)/
     *INTMAS(NJ+15)
      GO TO 132
C----------------------------------------------------------------------
   8  DGMAS(2) = -STMAS(4)
      DG1 = 1. - COS(2.*2.*PI*50.*TN)
      DG2 = STMAS(2+NTOKOB+1)**2
      IF(DG1.GT.DG2.AND.DG2.LT.1.E-10) GO TO 132
      DG3 = INTMAS(NJ+6)
      IF(INTMAS(NJ+6).EQ.0.) DGMAS(2) = 0.
      IF(INTMAS(NJ+15).NE.0.) DG3 = DG3 - INTMAS(NJ+4)/INTMAS(NJ+15)
      DG1 = DG1*DG3
      DGMAS(3) = DG1/DG2
      GO TO 132
   9  DGMAS(2) = 0.
      DGMAS(3) = 0.
      GO TO 132
C----------------------------------------------------------------------
  10  DGMAS(2) = -STMAS(4)
      DG1 = 1. - COS(2.*2.*PI*50.*TN)
      DG2 = STMAS(2+NTOKOB+1)**2
      DG1 = DG1*INTMAS(NJ+6)
      IF(INTMAS(NJ+6).EQ.0.) DGMAS(2) = 0.
      DGMAS(3) = DG1/DG2
      GO TO 132
C----------------------------------------------------------------------
  11  DGMAS(3) = 0.
      DGMAS(2) = -STMAS(4) + INTMAS(NJ+6) * (1.- COS(2.*2.*PI*50.*TN))
     */STMAS(2+NTOKOB+1)
      GO TO 132
C----------------------------------------------------------------------
  12  DGMAS(2) = -STMAS(4)
      DG1 = 1. - COS(2.*2.*PI*50.*TN)
      DG2 = STMAS(2+NTOKOB+1)**2
      IF(INTMAS(NJ+15).NE.0) DG1 = DG1 - INTMAS(NJ+4)/INTMAS(NJ+15)
      DG1 = DG1*INTMAS(NJ+6)
      IF(INTMAS(NJ+6).EQ.0.) DGMAS(2) = 0.
      DGMAS(3) = DG1/DG2
      GO TO 132
C----------------------------------------------------------------------
  13  DGMAS(2) = -STMAS(4)
      DG1 = 1. - COS(2.*2.*PI*50.*TN)
      DG2 = STMAS(2+NTOKOB+1)**2
      IF(INTMAS(NJ+15).NE.0) DG1 = DG1 + INTMAS(NJ+4)/INTMAS(NJ+15)
      DG1 = DG1*INTMAS(NJ+6)
      IF(INTMAS(NJ+6).EQ.0.) DGMAS(2) = 0.
      DGMAS(3) = DG1/DG2
      GO TO 132
C----------------------------------------------------------------------
 14   IF(INTMAS(NJ+19)) 1429,1431,1429
 1431 DGMAS(2) = 0.
      DGMAS(3) = 0.
      GO TO 132
 1429 DGMAS(2) = -STMAS(4)
      DGMAS(3) = INTMAS(NJ+17)/INTMAS(NJ+19)
      GO TO 132
C----------------------------------------------------------------------
C
 132  DGMAS(4) = INTMAS(NJ+3) + INTMAS(NJ+4)
      DGMAS(5) = INTMAS(NJ+14)
      RETURN
 103  IH = NJ + 1
      IB = NJ+65
      DO 114 I=IH,IB
 114  INTMAS(I) = 0.
      INTMAS(NJ+1) =T0
      DGMAS(2) = 0.
      DGMAS(3) = 0.
      GO TO 104
C-->  �AKOH��M B���C�EH�� ��� �PE�����E�O �EP�O�A
 105  DT1=T0+PER*T-INTMAS(NJ+1)
      DT2=DT-DT1
      DT=DT1
      GO TO 106
 107  INTMAS(NJ+2)=PER
      INTMAS(NJ+1) = T0+PER*T+DT2
      INTMAS(NJ+4) = INTMAS(NJ+4) + INTMAS(NJ+3)
      INTMAS(NJ+14) = PI/T * (INTMAS(NJ+10) * INTMAS(NJ+13) / T -
     *INTMAS(NJ+11) * INTMAS(NJ+12) / T)
      IF(INTMAS(NJ+2).EQ.2.) INTMAS(NJ+16)=INTMAS(NJ+4)
C
C----------------------------------------------------------------------
C-->  �POBEPKA HA �CTAHOB�B���C� PE��M
      PNOB = INTMAS(NJ+5)/T
      PNOB1= INTMAS(NJ+18)/T
      INTMAS(NJ+15) = INTMAS(NJ+20+NTOKOB+1)
      UNOB = INTMAS(NJ+20+NTOKOB+1)/T
      IF(INTMAS(NJ+20+2*NTOKOB+2).LE.1.E-32.OR.PNOB.LE.1.E-32.
     *OR.UNOB.LE.1.E-32) GO TO 111
      EPS1 = ABS(INTMAS(NJ+3)/INTMAS(NJ+20+2*NTOKOB+2))
      IF(EPS1.GT.EPS) GO TO 108
      EPS2 = ABS((PNOB - INTMAS(NJ+6))/PNOB)
      IF(EPS2.GT.EPS) GO TO 108
      EPS3 = ABS((UNOB - INTMAS(NJ+7))/UNOB)
      IF(EPS3.GT.EPS) GO TO 108
C-->  �CTAHOB�B���C� PE��M
      INTMAS(NJ+9) = 1.
 133  PAKT = 0.
      INTMAS(NJ+17) = INTMAS(NJ+20)/T
      DO 112 I=1,NTOKOB
      INTMAS(NJ+20+I) = SQRT(INTMAS(NJ+20+I)/T)
      INTMAS(NJ+20+NTOKOB+I) = SQRT(INTMAS(NJ+20+NTOKOB+I)/T)
      INTMAS(NJ+20+2*NTOKOB+I) = INTMAS(NJ+20+2*NTOKOB+I)/T
 112  PAKT = PAKT + INTMAS(NJ+20+2*NTOKOB+I)
      WRITE(NWTR,120)
      WRITE(NWTR,130) (INTMAS(NJ+20+I),INTMAS(NJ+20+NTOKOB+I),
     *INTMAS(NJ+20+2*NTOKOB+I),I=1,NTOKOB)
      PNAGR = 0.
      DO 113 I=1,NNAGR
 113  PNAGR = PNAGR + INTMAS(NJ+21+2*NTOKOB+I)
      PPOT = PAKT - PNAGR
      KPD = PNAGR/PAKT
      WRITE(NWTR,140)
      WRITE(NWTR,130) PNAGR,PPOT,KPD
      DZIL = SQRT(INTMAS(NJ+8)/T)
      KMH = INTMAS(NJ+20+2*NTOKOB+2)/(INTMAS(NJ+20+2)*
     *INTMAS(NJ+20+NTOKOB+2))
      KMK = INTMAS(NJ+20+2*NTOKOB+2)/(DZIL*
     *INTMAS(NJ+20+NTOKOB+2))
      SK = INTMAS(NJ+20+1)*INTMAS(NJ+20+NTOKOB+1)
      WRITE(NWTR,150)
      WRITE(NWTR,130) KMH,KMK,SK
C     PER = PER - 1.                   BO�MO�HA� �P���HA C�O�
      WK=INTMAS(NJ+16)
      WRITE(NWTR,160)
      WRITE(NWTR,130) PER,STMAS(1),INTMAS(NJ+4)
C        O T � A � K A       B�BO� INTMAS      ************************
C     WRITE(NWTR,200)
C200  FORMAT(//20X,'INTMAS       PSUB6 '/)
C
C191  WRITE(NWTR,190)(INTMAS(I),I=1,200)
C        K O H E �    OT�A�O�H�X O�EPATOPOB
      WRITE(7,180) KMH,KMK,SK,PPOT,KPD,PER,WK
      IF(URMAS(9).GE.0.) GO TO 134
C --> �OC�E������ O�EPATOP  - OKOH�AH�E MO�E��POBAH�� �O �OCT��EH��
C     �CTAHOB�B�E�OC� �PO�ECCA B �E�� C ��PAB��EM�M KOM�EHCAATOPOM
      TMAX = TN
      RETURN
 111  WRITE(NWTRE,110)
C-->  �PO�ECC HE �CTAHOB��C�
C     �A���EM HOB�E �HA�EH�� �APAMETPOB �AKOHA ��PAB�EH��
 108  IF(URMAS(9).GE.0.) GO TO 133
 134  INTMAS(NJ+6) = PNOB
      INTMAS(NJ+7) = UNOB
      INTMAS(NJ+17) = INTMAS(NJ+20)/T
      INTMAS(NJ+19) = PNOB1
C-->  HA�HEM HAKO��EH�E �HTE�PA��H�X C�MM C HA�A�A
      INTMAS(NJ+5) = STMAS(4)*STMAS(2+NTOKOB+1)*DT2
      INTMAS(NJ+18) = (STMAS(3)+STMAS(4))*STMAS(2+NTOKOB+1)*DT2
      INTMAS(NJ+20) = STMAS(4)**2*DT2
      IF(STMAS(2+1)*STMAS(2+NTOKOB+1)) 117,118,118
 118  INTMAS(NJ+10) = STMAS(2+1) * STMAS(2+NTOKOB+1) * DT2
      INTMAS(NJ+12) = DT2
      INTMAS(NJ+11) = 0.
      INTMAS(NJ+13) = 0.
      GO TO 119
 117  INTMAS(NJ+11) = STMAS(2+1) * STMAS(2+NTOKOB+1) * DT2
      INTMAS(NJ+13) = DT2
      INTMAS(NJ+10) = 0.
      INTMAS(NJ+12) = 0.
 119  DO 109 I=1,NTOKOB
      INTMAS(NJ+20+I) = STMAS(2+I)**2*DT2
      INTMAS(NJ+20+NTOKOB+I) = STMAS(2+NTOKOB+I)**2*DT2
 109  INTMAS(NJ+20+2*NTOKOB+I) = STMAS(2+I)*STMAS(2+NTOKOB+I)*DT2
      INTMAS(NJ+8) = (STMAS(3) + STMAS(4))**2*DT2
      INTMAS(NJ+3) = INTMAS(NJ+20+2*NTOKOB+1)
      GO TO 102
 110  FORMAT(1X,'�O�O�P�TE��HA� HA�P��KA, �PO�O��A� C��TAT�')
 120  FORMAT(1X,'�E�CTB���. �H. TOKOB.',5X,'�E�CTB���. �H. HA�P��.',5X,
     *'AKT�BH�E MO�HOCT�')
 130  FORMAT(1X,E12.5,20X,E12.5,20X,E12.5)
 140  FORMAT(1X,'AKT.MO�HOCT� HA�P��OK',5X,'MO�HOCT� �OTEP� �HEP���',5X
     *,' K.�.�. BCE� �E��')
 150  FORMAT(1X,'KO��. MO�HOCT� HA�P��K�',5X,'KO��.MO�. C KOM�EHCATOPOM
     *','�O�HA� MO�HOCT� KOM�EHCATOPA')
 160  FORMAT(1X,'BPEM� �EPEX. PE��MA KM� (�EP�O��)',5X,'BPEM� �A�A B�BO
     *�A TN',5X,'�HEP���,�P�H�TA� KOM�EHCATOPOM')
 170  FORMAT(1X,'KO���ECTBO MO�E�E� T��A 17 �PEB�C��O �O��CT�MOE ( ',I2
     *,' )')
 180  FORMAT(7E11.4)
C190  FORMAT(10E12.3)
 210  FORMAT(///'HOMEP A��OP�TMA K = ',I3,'HE �KA�AH B KATA�O�E'/)
      END
