      SUBROUTINE PKROSS(KOD,TAB,RAB,MASPRT,R,KTAB,KST)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:    PKROSS(KOD,TAB,RAB,MASPRT,R)
C   ------------
C
C   HA�HA�EH�E:    �EPEC��KA CO�EP��MO�O MACC�BOB
C   ----------
C                  �P�  KOD=0  TAB=>MASPRT
C                              RAB=>R
C
C                       KOD=1  MASPRT=>TAB
C                              R=>RAB
C
C
C   METO�:    B TAB(ITAB+12) XPAH�TC� HOMEP ��EMEHTA B MASPRT,
C   ------                   ��E ITAB - HOMEP CTPOK� TAB (O�HA CTPOKA
C                            COOTBETCTB�ET O�HOM� ��EMEHT�,CO�EP��T
C                            HEO�XO��M�� �H�OPMA��� O HEM)
C             B TAB(ITAB+10) XPAH�TC� �HA�EH�E ��EMEHTA
C
C   �APAMETP�:     KTAB - KO�-BO CTPOK TAB,
C   ----------     KST - KO� - BO �A�T B O�HO� CTPOKE TAB
C
C  �O��PO�PAMM�:   HET
C  ------------
C
C  �P�ME�AH�E:     BC�OMO�ATE��HA� �/� ��� DRMET0
C  -----------
C
CZZZ-------------------------------------------------------------------
      DIMENSION MASPRT(1)
      DIMENSION TAB(1),RAB(1)
      DIMENSION R(1)
C----------------------------------------------------------------------
      INTEGER TAB
      K=KTAB*KST
      IF (KOD.EQ.1) GO TO 10
C--> KOD=0 �EPEC��KA TAB=>MASPRT,RAB=>R
      DO 5 ITAB=1,K,KST
      IF (TAB(ITAB+12).EQ.0) GO TO 8
      IF (TAB(ITAB).EQ.1) GO TO 6
      MASPRT(TAB(ITAB+12))=TAB(ITAB+10)
      GO TO 5
    6 R(TAB(ITAB+12))=RAB(ITAB+10)
    5 CONTINUE
    8 RETURN
C--> KOD=1 �EPEC��KA MASPRT=>TAB,R=>RAB
   10 DO 15 ITAB=1,K,KST
      IF (TAB(ITAB+12).EQ.0) GO TO 18
      IF (TAB(ITAB).EQ.1) GO TO 16
      TAB(ITAB+10)=MASPRT(TAB(ITAB+12))
      GO TO 15
   16 RAB(ITAB+10)=R(TAB(ITAB+12))
   15 CONTINUE
   18 RETURN
      END
