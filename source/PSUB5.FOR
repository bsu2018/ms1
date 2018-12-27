       SUBROUTINE PSUB5( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT APE‹Ÿ  1988 ƒ.)------------
C   O„POƒPAMMA: PSUB5( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA‡HA—EHˆE:  / O‹œ‡OBATE‹Ÿ „‹Ÿ MO„E‹ˆPOBAHˆŸ
C   ----------   ‡AKOHOB “PAB‹EHˆŸ TPEX”A‡H›M KOMEHCATOPOM
C                MO™HOCTˆ ˆCKA†EHˆŸ ˆ HECˆMMETPˆˆ
C                  ( MO„E‹œ #18 - J3 “P.)
C   BAPˆAHT›:    BO‡MO†H› „BA BAPˆAHTA MO„E‹ˆ:
C   ---------  1.Pˆ‡HAK MO„E‹ˆ = 0, MO„E‹œ ‹ˆHE‰HAŸ,
C                O BE‹ˆ—ˆHAM, EPE„ABAEM›M O‹œ‡OBATE‹EM ˆ
C                ‡HA—EHˆŸM TOKOB ˆ HAPŸ†EHˆ‰ ‹EMEHTOB CXEM›
C                „O MOMEHTA BPEMEHˆ TN BK‹ž—ˆTE‹œHO B O„POƒPAMME
C                OPE„E‹ŸžTCŸ ‡HA—EHˆŸ G,J-APAMETPOB MO„E‹ˆ „‹Ÿ T=TN+1
C              2.Pˆ‡HAK MO„E‹ˆ = 1, MO„E‹œ ˆTEPA–ˆOHHAŸ,
C                O BE‹ˆ—ˆHAM, EPE„ABAEM›M O‹œ‡OBATE‹EM ˆ
C                ‡HA—EHˆŸM TOKOB ˆ HAPŸ†EHˆ‰ ‹EMEHTOB CXEM›
C                „O MOMEHTA BPEMEHˆ TN+1 BK‹ž—ˆTE‹œHO B O„POƒPAMME
C                OPE„E‹ŸžTCŸ ‡HA—EHˆŸ G,J-APAMETPOB B MOMEHT TN+1
C                ˆTEPA–ˆOHH›M COCOOM
C   METO„:     O—EBˆ„EH.
C   -----
C  APAMETP›:   NEL - HOMEP ‹EMEHTA, „‹Ÿ KOTOPOƒO PSUB PAOTAET;
C  ---------    URMAS - MACCˆB „AHH›X O‹œ‡OBATE‹Ÿ:
C                       1-E C‹OBO - KO‹ˆ—ECTBO APAMETPOB B CˆCKE
C                       ( ‡AˆCAHO –E‹›M —ˆC‹OM B BE™ECTBEHH›‰ MACCˆB);
C                       2-E C‹OBO - Pˆ‡HAK MO„E‹ˆ ( CM.BAPˆAHT› );
C                       3-E C‹OBO - MOMEHT BK‹ž—EHˆŸ MO„E‹ˆ  T0;
C                       4-E C‹OBO - KO„ ‡AKOHA “PAB‹EHˆŸ ( K );
C                       5-E C‹OBO - EPˆO„ B›—ˆC‹EHˆŸ ˆHTEƒPA‹œH›X
C                                   BE‹ˆ—ˆH;
C                       6-E C‹OBO - KO‹ˆ—ECTBO TOKOB CXEM›;
C                       7-E C‹OBO - KO‹ˆ—ECTBO HAƒP“‡OK;
C                       8-E C‹OBO - TO—HOCTœ OPE„E‹EHˆŸ “CTAHOBˆB-
C                                   ˜EƒOCŸ PE†ˆMA;
C                       9-E C‹OBO - <0 - PAC—ET BE„ETCŸ „O “CTAHOB.
C                                   PE†ˆMA, B „P.C‹“—AE - B›BO„ ˆHTEƒP.
C                                   BE‹ˆ—ˆH —EPE‡ KA†„›‰ EPˆO„ „O
C                                   OKOH—AHˆŸ MO„E‹ˆPOBAHˆŸ;
C----------------------------------------------------------------------
C              STMAS - MACCˆB „AHH›X, EPE„ABAEM›X B PSUB:
C
C      C‹OBO           OO‡HA—EHˆE   -   ‡HA—EHˆE
C      1               TN - BPEMŸ N - ƒO ˜AƒA PAC—ETA;
C      2               H0 - ˜Aƒ ˆHTEƒPˆPOBAHˆŸ;
C      3               IKA - ‡HA—EHˆE TOKA ”A‡›  A  KOMEHCATOPA,
C                            ‡ATPEOBAHHOE O‹œ‡OBATE‹EM
C                            „‹Ÿ MOMEHTA BPEMEHˆ TN;
C      4               IKB - ‡HA—EHˆE TOKA ”A‡›  B  KOMEHCATOPA,
C      5               IKC - ‡HA—EHˆE TOKA ”A‡›  C  KOMEHCATOPA,
C      6               IHA - ‡HA—EHˆE TOKA ”A‡›  A  HAƒP“‡Kˆ,
C      7               IHB - ‡HA—EHˆE TOKA ”A‡›  B  HAƒP“‡Kˆ,
C      8               IHC - ‡HA—EHˆE TOKA ”A‡›  C  HAƒP“‡Kˆ,
C      9               UKA - HAPŸ†EHˆE ”A‡› A KOMEHCATOPA,
C     10               UKB - HAPŸ†EHˆE ”A‡› B KOMEHCATOPA,
C     11               UKC - HAPŸ†EHˆE ”A‡› C KOMEHCATOPA,
C     12               UHA - HAPŸ†EHˆE ”A‡› A HAƒP“‡Kˆ,
C     13               UHB - HAPŸ†EHˆE ”A‡› B HAƒP“‡Kˆ,
C     14               UHC - HAPŸ†EHˆE ”A‡› C HAƒP“‡Kˆ,
C
C----------------------------------------------------------------------
C         DGMAS(1) - PˆXO„ˆT HOMEP ‡APOCA,
C                    EC‹ˆ DGMAS(1)=1, TO TO EPB›‰ BXO„ B
C                    O„POƒPAMM“ PSUB HA „AHHOM ˜AƒE,
C                    EC‹ˆ DGMAS(1)=2, TO TO HE EPBAŸ ˆTEPA–ˆŸ
C                    B›XO„ˆT Pˆ‡HAK MO„E‹ˆ
C         DGMAS(2) - PˆXO„ˆT ‡HA—EHˆE TOKA  IKA  ”A‡›  A   MO„E‹ˆ B
C                    MOMEHT TN+1, EC‹ˆ HOMEP ‡APOCA = 1 ( DGMAS(1) ),
C                    TO ‡HA—EHˆE TOKA, B›—ˆC‹EHHOE HA PE„›„“™EM ˜AƒE,
C                    EC‹ˆ HOMEP ‡APOCA = 2 ( DGMAS(1) ), TO  TO
C                    ‡HA—EHˆE TOKA, B›—ˆC‹EHHOE HA PE„›„“™E‰ ˆTEPA–ˆˆ
C                    „AHHOƒO ˜AƒA, „A‹EE AHA‹Oƒˆ—HO,
C         DGMAS(3) - AHA‹Oƒˆ—HO  IKB,
C         DGMAS(4) - AHA‹Oƒˆ—HO  IKC,
C         DGMAS(5) - AHA‹Oƒˆ—HO  IK0,
C         DGMAS(6) - TOK ˆCTO—HˆKA CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  J12,
C         DGMAS(7) - TOK ˆCTO—HˆKA CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  J13,
C         DGMAS(8) - TOK ˆCTO—HˆKA CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  J23,
C         DGMAS(9) - TOK ˆCTO—HˆKA CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  J14,
C         DGMAS(10)- TOK ˆCTO—HˆKA CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  J24,
C         DGMAS(11)- TOK ˆCTO—HˆKA CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  J34,
C         DGMAS(12)- POBO„ˆMOCTœ  CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  G12,
C         DGMAS(13)- POBO„ˆMOCTœ  CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  G13,
C         DGMAS(14)- POBO„ˆMOCTœ  CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  G23,
C         DGMAS(15)- POBO„ˆMOCTœ  CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  G14,
C         DGMAS(16)- POBO„ˆMOCTœ  CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  G24,
C         DGMAS(17)- POBO„ˆMOCTœ  CˆHTETˆ—ECKO‰ CXEM› ‡AME™EHˆŸ  G34,
C         DGMAS(18)-
C         DGMAS(19)-
C         DGMAS(20)-           P  E  ‡  E  P  B
C         DGMAS(21)-
C         DGMAS(22)-
C----------------------------------------------------------------------
C              INTMAS - MACCˆB ˆ‡ „B“XCOT (200) ‹EMEHTOB,ˆCO‹œ‡“ETCŸ
C                       B O„POƒPAMME „‹Ÿ ‡AOMˆHAHˆŸ PE‡“‹œTATOB
C                       POME†“TO—H›X B›—ˆC‹EHˆ‰.
C----------------------------------------------------------------------
C
C   O„POƒPAMM›: HET
C   -------------
C
C   PˆME—AHˆE: C“MMA KO‹‹ˆ—ECTBA TPE“EM›X TOKOB ˆ HAPŸ†EHˆ‰
C   ----------  HE „O‹†HA PEB›˜ATœ 48.
C               ( KOHTPO‹œ HE POˆ‡BO„ˆTCŸ )
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
C      K - HOMEP ‡AKOHA “PAB‹EHˆŸ
C======================================================================
C--> K=1    IKA = - IHA + PH / ( UKA**2 + UKB**2 + UKC**2 ) * UKA(T)
C           IKB = - IHB + PH / ( UKA**2 + UKB**2 + UKC**2 ) * UKB(T)
C           IKC = - IHC + PH / ( UKA**2 + UKB**2 + UKC**2 ) * UKC(T)
C======================================================================
C
C    B CˆCKE APAMETPOB PSUB  OC‹E CTAH„APTH›X APAMETPOB C‹E„“žT:
C    T - EPˆO„ B›—ˆC‹EHˆŸ APAMETPOB ‡AKOHA;
C    NTOKOB - KO‹ˆ—ECTBO BBO„ˆM›X TOKOB;
C    NNAGR - KO‹ˆ—ECTBO HAƒP“‡OK B ‹. –Eˆ.
C    EPS - O˜ˆKA OPE„E‹EHˆŸ “CTAHOBˆB˜EƒOCŸ PE†ˆMA
C    M -  BAPˆAHT B›BO„A ˆHTEƒPA‹œH›X APAMETPOB (< 0  ˆ > = 0 )
C         PˆME—AHˆŸ: 1. KO‹ˆ—ECTBO BBO„ˆM›X HAPŸ†EHˆ‰ „O‹†HO ›Tœ
C         -----------    PABHO KO‹ˆ—ECTB“ BBO„ˆM›X TOKOB;
C                     2. OPŸ„OK BBO„A „‹Ÿ TOKOB ‹EMEHTOB ˆ ˆX HA-
C                        PŸ†EHˆ‰ E„ˆH›‰;
C                     3. EC‹ˆ HAƒP“‡KA COE„ˆHEHA TPE“ƒO‹œHˆKOM,
C                        BBO„ŸTCŸ ‹ˆHE‰H›E TOKˆ ˆ HAPŸ†EHˆŸ COOTBET-
C                                 ========
C                        CTB“ž™ˆE PAC—ET“ PH  METO„OM „B“X BATTMETPOB
C
C         CO„EP†AHˆE  STMAS
C         -----------------
C    ------------------------------------------------------------------
C    HOMEP ‹EMEHTA                     ‡HA—EHˆE
C    ------------------------------------------------------------------
C    1                                  CTAH„APTH›E APAMETP› PSUB5
C    2                                         =  "  =  "  =
C    2+I           ( I OT 1 „O NTOKOB)  ‡HA—EHˆŸ TOKOB ‹EMEHTOB
C    2+NTOKOB+I    ( I OT 1 „O NTOKOB)  ‡HA—EHˆŸ HAPŸ†EHˆ‰ ‹EMEHTOB
C    ------------------------------------------------------------------
C
C         CO„EP†AHˆE INTMAS
C         -----------------
C    ------------------------------------------------------------------
C    HOMEP ‹EMEHTA                     ‡HA—EHˆE
C    1                                  MOMEHT BPEMEHˆ PE„›„“™EƒO ˜AƒA
C                                       ( TN-1 ) ;
C    2                                  –E‹OE KO‹ˆ—ECTBO EPˆO„OB, PO-
C                                       ˜E„˜ˆX C MOMEHTA BK‹ž—EHˆŸ KOM-
C                                       EHCATOPA ( PER ) ;
C    3                                  HEPƒˆŸ, OT„AHHAŸ ( PˆHŸTAŸ )
C                                       KOMEHCATOPOM C HA—A‹A „AHHOƒO
C                                       EPˆO„A ( TEK“™EE DW ) ;
C    4                                  HEPƒˆŸ OT„AHHAŸ ( PˆHŸTAŸ )
C                                       KOMEHCATOPOM C MOMEHTA BK‹ž—E-
C                                       HˆŸ  ( T0 ) ‡A –E‹OE —ˆC‹O PO-
C                                       ˜E„˜ˆX EPˆO„OB ;
C    5                                  TEK“™EE ‡HA—EHˆE ˆHTEƒPA‹œHO‰
C                                       C“MM›  „‹Ÿ B›—ˆC‹EHˆŸ  PH
C                                       ( APAMETPA ‡AKOHA “PAB‹EHˆŸ);
C    6                                  PH - ‡HA—EHˆE APAMETPA ‡AKOHA,
C                                       ˆCO‹œ‡“EMOE HA „AHHOM ˜AƒE ;
C    7                                  UKA**2 + UKB**2 + UKC**2
C                                       C“MMA KBA„PATOB „E‰CTB“ž™ˆX
C                                       ‡HA—EHˆ‰ HAPŸ†EHˆ‰ KOMEHCATO-
C                                       PA ( APAMETP ‡AKOHA “PAB‹EHˆŸ
C                                       ˆCO‹œ‡“EM›‰ HA „AHHOM ˜AƒE ) ;
C    8                                  P E ‡ E P B
C
C
C    9                                  EC‹ˆ INTMAS(9) = 1, TO PO–ECC
C                                       “CTAHOBˆB˜ˆ‰CŸ
C   10 - 20                             P E ‡ E P B
C
C    20 +I          (I OT 1 „O NTOKOB)  TEK“™ˆE ‡HA—EHˆŸ ˆHTEƒPA‹œH›X
C                                       C“MM „‹Ÿ B›—ˆC‹EHˆŸ „E‰CTB“ž™ˆX
C                                       ‡HA—EHˆ‰ TOKOB ;
C    20+NTOKOB+I    (I OT 1 „O NTOKOB)  TO †E „‹Ÿ HAPŸ†EHˆ‰ ;
C    20+2*NTOKOB+I  (I OT 1 „O NTOKOB)  TO †E „‹Ÿ AKTˆBH›X MO˜HOCTE‰ ;
C    ------------------------------------------------------------------
C
C-->  NMOD - „O“CTˆMOE KO‹ˆ—ECTBO MO„E‹E‰ „AHHOƒO TˆA B CXEME
      NMOD = 2
C-->  POˆ‡BO„ˆTCŸ PACPE„E‹EHˆE MACCˆBA  INTMAS  „‹Ÿ
C     MO„E‹E‰ „AHHOƒO TˆA  -  PA‡H›X ‹EMEHTOB CXEM›
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
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œHO‰ C“MM› „‹Ÿ PH
 106  INTMAS(NJ+5) = INTMAS(NJ+5) + ( STMAS(6) * STMAS(2+NTOKOB+4) +
     *STMAS(7) * STMAS(2+NTOKOB+5) + STMAS(8) * STMAS(2+NTOKOB+6)) * DT
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œH›X C“MM ”A‡H›X HAPŸ†EHˆ‰ UKA,UKB,UKC
      DO 101 I=1,3
 101  INTMAS(NJ+20+NTOKOB+I) = INTMAS(NJ+20+NTOKOB+I) +
     * STMAS(2+NTOKOB+I)**2 * DT
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œHO‰ C“MM› „‹Ÿ HEPƒˆˆ KOMEHCATOPA
      INTMAS(NJ+3) = INTMAS(NJ+3) + ( STMAS(3) * STMAS(2+NTOKOB+1) +
     *STMAS(4) * STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) * DT
C----------------------------------------------------------------------
 116  IF(PER.GT.INTMAS(NJ+2)) GO TO 107
C-->  ‡AOMHˆM BPEMŸ TOƒO ˜AƒA HA “„“™EE
      INTMAS(NJ+1)=INTMAS(NJ+1)+DT
C======================================================================
C     ƒ‹ABHAŸ ”OPM“‹A ˆTEPA–ˆˆ, B›—ˆC‹EHˆE J,G-APAMETPOB KOMEHCATOPA
 102  GO TO (1),K
C----------------------------------------------------------------------
   1  IF(INTMAS(NJ+7)) 138,131,129
 131  DO 136 I = 6,17
 136  DGMAS(I) = 0.
C     DGMAS(12) = 1.E-3
C     DGMAS(13) = 1.E-3
C     DGMAS(14) = 1.E-3                O  T  ‹  A  „  K  A
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
C     DGMAS(14) = 1.E-3         O  T  ‹  A  „  K  A
      DGMAS(15) = 1.E-6
      DGMAS(16) = 1.E-6
      DGMAS(17) = 1.E-6
      GO TO 104
C-->  ‡AKOH—ˆM B›—ˆC‹EHˆŸ „‹Ÿ PE„›„“™EƒO EPˆO„A
 105  DT1=T0+PER*T-INTMAS(NJ+1)
      DT2=DT-DT1
      DT=DT1
      GO TO 106
 107  INTMAS(NJ+2)=PER
      INTMAS(NJ+4) = INTMAS(NJ+4) + INTMAS(NJ+3)
C----------------------------------------------------------------------
C-->  POBEPKA HA “CTAHOBˆB˜ˆ‰CŸ PE†ˆM
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
C-->  “CTAHOBˆB˜ˆ‰CŸ PE†ˆM
      INTMAS(NJ+9) = 1.
 133  WRITE(NWTR,160)
      WRITE(NWTR,130)PER,STMAS(1),INTMAS(NJ+4)
      IF(URMAS(9).GE.0.) GO TO 134
      TMAX = TN
      RETURN
 111  WRITE(NWTRE,110)
C----------------------------------------------------------------------
C-->  PO–ECC HE “CTAHOBˆ‹CŸ
C     ‡Aˆ˜EM HOB›E ‡HA—EHˆŸ APAMETPOB ‡AKOHA “PAB‹EHˆŸ
 108  IF(URMAS(9).GE.0.) GO TO 133
 134  INTMAS(NJ+6) = PNOB
      INTMAS(NJ+7) = UNOB
C-->  HA—HEM HAKO‹EHˆE ˆHTEƒPA‹œH›X C“MM C HA—A‹A
      INTMAS(NJ+5) = ( STMAS(6) * STMAS(2+NTOKOB+4) + STMAS(7) *
     *STMAS(2+NTOKOB+5) + STMAS(8) * STMAS(2+NTOKOB+6)) * DT2
      DO 137 I = 1,3
 137  INTMAS(NJ+20+NTOKOB+I) = STMAS(2+NTOKOB+I)**2 * DT2
      INTMAS(NJ+3) = ( STMAS(3) * STMAS(2+NTOKOB+1) + STMAS(4) *
     *STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) * DT2
      GO TO 102
 110  FORMAT(1X,'BO‡MO†HO KOPOTKOE ‡AM›KAHˆE HAƒP“‡Kˆ')
 130  FORMAT(1X,E12.5,20X,E12.5,20X,E12.5)
 160  FORMAT(1X,'BPEMŸ EPEX. PE†ˆMA KMˆ (EPˆO„›)',5X,'BPEMŸ ˜AƒA B›BO
     *„A TN',5X,'HEPƒˆŸ,PˆHŸTAŸ KOMEHCATOPOM')
 170  FORMAT(1X,'KO‹ˆ—ECTBO MO„E‹E‰ TˆA 17 ˆ‹ˆ 18 PEB›Cˆ‹O „O“CTˆMOE
     *( ',I2 ,' )')
 180  FORMAT(1X,'APAMETP  (UKA**2+UKB**2+UKC**2) < 0  ')
      END
