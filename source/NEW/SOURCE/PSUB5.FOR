       SUBROUTINE PSUB5( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT A�PE��  1988 �.)------------
C   �O��PO�PAMMA: PSUB5( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   �AKOHOB ��PAB�EH�� TPEX�A�H�M KOM�EHCATOPOM
C                MO�HOCT� �CKA�EH�� � HEC�MMETP��
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
C      6               IHA - �HA�EH�E TOKA �A��  A  HA�P��K�,
C      7               IHB - �HA�EH�E TOKA �A��  B  HA�P��K�,
C      8               IHC - �HA�EH�E TOKA �A��  C  HA�P��K�,
C      9               UKA - HA�P��EH�E �A�� A KOM�EHCATOPA,
C     10               UKB - HA�P��EH�E �A�� B KOM�EHCATOPA,
C     11               UKC - HA�P��EH�E �A�� C KOM�EHCATOPA,
C     12               UHA - HA�P��EH�E �A�� A HA�P��K�,
C     13               UHB - HA�P��EH�E �A�� B HA�P��K�,
C     14               UHC - HA�P��EH�E �A�� C HA�P��K�,
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
       REAL INTMAS
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
       T0 = URMAS(3)
       IF(URMAS(2).EQ.0.AND.DGMAS(1).NE.1) RETURN
       DGMAS(1) = URMAS(2)
C----------------------------------------------------------------------
       K=URMAS(4)
C      K - HOMEP �AKOHA ��PAB�EH��
C======================================================================
C--> K=1    IKA = - IHA + PH / ( UKA**2 + UKB**2 + UKC**2 ) * UKA(T)
C           IKB = - IHB + PH / ( UKA**2 + UKB**2 + UKC**2 ) * UKB(T)
C           IKC = - IHC + PH / ( UKA**2 + UKB**2 + UKC**2 ) * UKC(T)
C======================================================================
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
C
C
C    9                                  EC�� INTMAS(9) = 1, TO �PO�ECC
C                                       �CTAHOB�B���C�
C   10 - 20                             P E � E P B
C
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
C-->  B���C�EH�E �HTE�PA��H�X C�MM �A�H�X HA�P��EH�� UKA,UKB,UKC
      DO 101 I=1,3
 101  INTMAS(NJ+20+NTOKOB+I) = INTMAS(NJ+20+NTOKOB+I) +
     * STMAS(2+NTOKOB+I)**2 * DT
C-->  B���C�EH�E �HTE�PA��HO� C�MM� ��� �HEP��� KOM�EHCATOPA
      INTMAS(NJ+3) = INTMAS(NJ+3) + ( STMAS(3) * STMAS(2+NTOKOB+1) +
     *STMAS(4) * STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) * DT
C----------------------------------------------------------------------
 116  IF(PER.GT.INTMAS(NJ+2)) GO TO 107
C-->  �A�OMH�M BPEM� �TO�O �A�A HA �����EE
      INTMAS(NJ+1)=INTMAS(NJ+1)+DT
C======================================================================
C     ��ABHA� �OPM��A �TEPA���, B���C�EH�E J,G-�APAMETPOB KOM�EHCATOPA
 102  GO TO (1),K
C----------------------------------------------------------------------
   1  IF(INTMAS(NJ+7)) 138,131,129
 131  DO 136 I = 6,17
 136  DGMAS(I) = 0.
C     DGMAS(12) = 1.E-3
C     DGMAS(13) = 1.E-3
C     DGMAS(14) = 1.E-3                O  T  �  A  �  K  A
      DGMAS(15) = 1.E-6
      DGMAS(16) = 1.E-6
      DGMAS(17) = 1.E-6
      GO TO 132
 129  DGMAS(6) = 0.
      DGMAS(7) = 0.
      DGMAS(8) = 0.
      DGMAS(9) = - STMAS(6)
      DGMAS(10)= - STMAS(7)
      DGMAS(11)= - STMAS(8)
      DGMAS(12)= 0.
      DGMAS(13)= 0.
      DGMAS(14)= 0.
      DGMAS(15)= INTMAS(NJ+6) / INTMAS(NJ+7)
      DGMAS(16)= INTMAS(NJ+6) / INTMAS(NJ+7)
      DGMAS(17)= INTMAS(NJ+6) / INTMAS(NJ+7)
      GO TO 132
 138  WRITE(NWTRE,180)
      TMAX = TN
      RETURN
C----------------------------------------------------------------------
 132  DGMAS(18) = INTMAS(NJ+6)
      DGMAS(19) = 0.
      DGMAS(20) = 0.
      DGMAS(21) = 0.
      DGMAS(22) = 0.
      RETURN
 103  IH = NJ + 1
      IB = NJ+98
      DO 114 I=IH,IB
 114  INTMAS(I) = 0.
      INTMAS(NJ+1) =T0
      DO 135 I = 6,22
 135  DGMAS(I) = 0.
C     DGMAS(12) = 1.E-3
C     DGMAS(13) = 1.E-3
C     DGMAS(14) = 1.E-3         O  T  �  A  �  K  A
      DGMAS(15) = 1.E-6
      DGMAS(16) = 1.E-6
      DGMAS(17) = 1.E-6
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
     * + INTMAS(NJ+20+NTOKOB+3)) / T
      IF(PNOB.LE.1.E-32.OR.UNOB.LE.1.E-32) GO TO 111
      EPS1 = ABS(INTMAS(NJ+4) - INTMAS(NJ+3))
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
C-->  HA�HEM HAKO��EH�E �HTE�PA��H�X C�MM C HA�A�A
      INTMAS(NJ+5) = ( STMAS(6) * STMAS(2+NTOKOB+4) + STMAS(7) *
     *STMAS(2+NTOKOB+5) + STMAS(8) * STMAS(2+NTOKOB+6)) * DT2
      DO 137 I = 1,3
 137  INTMAS(NJ+20+NTOKOB+I) = STMAS(2+NTOKOB+I)**2 * DT2
      INTMAS(NJ+3) = ( STMAS(3) * STMAS(2+NTOKOB+1) + STMAS(4) *
     *STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) * DT2
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