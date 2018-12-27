      SUBROUTINE PKROSS(KOD,TAB,RAB,MASPRT,R,KTAB,KST)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:    PKROSS(KOD,TAB,RAB,MASPRT,R)
C   ------------
C
C   HA‡HA—EHˆE:    EPEC›‹KA CO„EP†ˆMOƒO MACCˆBOB
C   ----------
C                  Pˆ  KOD=0  TAB=>MASPRT
C                              RAB=>R
C
C                       KOD=1  MASPRT=>TAB
C                              R=>RAB
C
C
C   METO„:    B TAB(ITAB+12) XPAHˆTCŸ HOMEP ‹EMEHTA B MASPRT,
C   ------                   ƒ„E ITAB - HOMEP CTPOKˆ TAB (O„HA CTPOKA
C                            COOTBETCTB“ET O„HOM“ ‹EMEHT“,CO„EP†ˆT
C                            HEOXO„ˆM“ž ˆH”OPMA–ˆž O HEM)
C             B TAB(ITAB+10) XPAHˆTCŸ ‡HA—EHˆE ‹EMEHTA
C
C   APAMETP›:     KTAB - KO‹-BO CTPOK TAB,
C   ----------     KST - KO‹ - BO A‰T B O„HO‰ CTPOKE TAB
C
C  O„POƒPAMM›:   HET
C  ------------
C
C  PˆME—AHˆE:     BCOMOƒATE‹œHAŸ / „‹Ÿ DRMET0
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
C--> KOD=0 EPEC›‹KA TAB=>MASPRT,RAB=>R
      DO 5 ITAB=1,K,KST
      IF (TAB(ITAB+12).EQ.0) GO TO 8
      IF (TAB(ITAB).EQ.1) GO TO 6
      MASPRT(TAB(ITAB+12))=TAB(ITAB+10)
      GO TO 5
    6 R(TAB(ITAB+12))=RAB(ITAB+10)
    5 CONTINUE
    8 RETURN
C--> KOD=1 EPEC›‹KA MASPRT=>TAB,R=>RAB
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