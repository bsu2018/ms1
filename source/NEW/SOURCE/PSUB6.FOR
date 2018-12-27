       SUBROUTINE PSUB6( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT HOŸPŸ  1988 ƒ.)------------
C   O„POƒPAMMA: PSUB6( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA‡HA—EHˆE:  / O‹œ‡OBATE‹Ÿ „‹Ÿ MO„E‹ˆPOBAHˆŸ
C   ----------   ‡AKOHOB “PAB‹EHˆŸ  KOMEHCATOPOM
C                MO™HOCTˆ ˆCKA†EHˆŸ ( AKTˆBH›M ”ˆ‹œTPOM Bƒ )
C                  ( MO„E‹œ #17 - J “P.)
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
C      3               I1 - ‡HA—EHˆE 1-ƒO TOKA, ‡ATPEOBAHHOƒO
C                           O‹œ‡OBATE‹EM, „‹Ÿ MOMEHTA BPEMEHˆ TN;
C         „A‹EE AHA‹Oƒˆ—HO ‡HA—EHˆŸ OCTA‹œH›X TOKOB;
C   N=3+NTOKOB       U1 - ‡HA—EHˆE 1-ƒO HAPŸ†EHˆŸ, ‡ATPEOBAHHOƒO
C                           O‹œ‡OBATE‹EM, „‹Ÿ MOMEHTA BPEMEHˆ TN;
C         „A‹EE AHA‹Oƒˆ—HO ‡HA—EHˆŸ OCTA‹œH›X HAPŸ†EHˆ‰;
C
C----------------------------------------------------------------------
C              DGMAS(1) - PˆXO„ˆT HOMEP ‡APOCA,
C                         EC‹ˆ DGMAS(1)=1, TO TO EPB›‰ BXO„ B
C                         O„POƒPAMM“ PSUB HA „AHHOM ˜AƒE,
C                         EC‹ˆ DGMAS(1)=2, TO TO HE EPBAŸ ˆTEPA–ˆŸ
C                         B›XO„ˆT Pˆ‡HAK MO„E‹ˆ
C              DGMAS(2) - PˆXO„ˆT ‡HA—EHˆE TOKA MO„E‹ˆ B MOMEHT TN+1,
C                         EC‹ˆ HOMEP ‡APOCA = 1 ( DGMAS(1) ), TO
C                         TO ‡HA—EHˆE TOKA MO„E‹ˆ, B›—ˆC‹EHHOE HA
C                         PE„›„“™EM ˜AƒE,
C                         EC‹ˆ HOMEP ‡APOCA = 2 ( DGMAS(1) ), TO
C                         TO ‡HA—EHˆE TOKA MO„E‹ˆ, B›—ˆC‹EHHOE HA
C                         PE„›„“™E‰ ˆTEPA–ˆˆ „AHHOƒO ˜AƒA
C                         B›XO„ˆT  J-APAMETP (ˆCT.TOKA CˆHTETˆ—.CXEM›)
C              DGMAS(3) - G-APAMETP (POBO„ˆMOCTœ CˆHTETˆ—ECKO‰ CXEM›)
C              DGMAS(4) - PH- AKTˆBHAŸ MO™HOCTœ KOMEHCˆP“EMO‰ HAƒP“‡Kˆ
C              DGMAS(5) - QO - OMEHHAŸ MO™HOCTœ KOMEHCATOPA
C
C
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
       REAL INTMAS,KPD,KMH,KMK
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
       T0 = URMAS(3)
       IF(URMAS(2).EQ.0.AND.DGMAS(1).NE.1) RETURN
       DGMAS(1) = URMAS(2)
C----------------------------------------------------------------------
       K=URMAS(4)
C      K - HOMEP ‡AKOHA “PAB‹EHˆŸ
C--> Pˆ ‡AˆCˆ ”OPM“‹ ‡AKOHOB “PAB‹EHˆŸ ˆCO‹œ‡OBAHO COKPA™EHˆE "PK"
C    TO OTHO˜EHˆE DW(„E‹œTA W) ˆ‡MEHEHˆŸ HEPƒˆˆ KOMEHCATOPA C
C    MOMEHTA BK‹ž—EHˆŸ ‡A –E‹OE —ˆC‹O EPˆO„OB K „‹ˆTE‹œHOCTˆ EPˆO„A T
C======================================================================
C--> K=1        ‡AKOH FRIZE  IK=-IH+PH/UK**2*UK(T)
C          E‡ CTAˆ‹ˆ‡A–ˆˆ HEPƒˆˆ HAKOˆTE‹Ÿ
C    K=2        ‡AKOH FRIZE  IK=-IH+(PH-PK)/UK**2*UK(T)
C          CO CTAˆ‹ˆ‡A–ˆE‰ HEPƒˆˆ ‡A EPˆO„
C    K=3        IK=-IH+PH/UK**2*UK*SQRT(2.)*SIN(OMEGA*T)
C          E‡ CTAˆ‹ˆ‡A–ˆˆ HEPƒˆˆ HAKOˆTE‹Ÿ
C    K=4        IK=-IH+PH/UK*SQRT(2.)*SIN(OMEGA*TN)-PK/UK**2*UK(T)
C          CTAˆ‹ˆ‡A–ˆŸ O KPˆBO‰ HAPŸ†EHˆŸ
C    K=5        IK=-IH+(PH-PK)/UK*SQRT(2.)*SIN(OMEGA*T)
C          CTAˆ‹ˆ‡A–ˆŸ O SIN ‡AKOH“
C    K=6        IK=-IH+PH*(1.-COS(2.*OMEGA*T)/UK(T)
C          “P. O POM E‡ CTAˆ‹ˆ‡A–ˆˆ
C    K=7        IK=-IH+PH*(1.-COS(2.*OMEGA*T)/UK(T)-PK/UK**2*UK(T)
C          POM CO CTAˆ‹ˆ‡A–ˆE‰ O KPˆBO‰ HAPŸ†EHˆŸ
C    K=8        IK=-IH+(PH-PK)*(1.-COS(2.*OMEGA*T))/UK(T)
C          POM  CO CTAˆ‹ˆ‡A–ˆE‰ O KPˆBO‰ MO™HOCTˆ OT SIN
C    K=9        IK=0.
C          PAC—ET ˆHTEƒPA‹œH›X APAMETPOB PE†ˆMA E‡ KOMEHCA–ˆˆ
C    K=10  PAC—ET O POM E‡ OAOTKˆ „E‹EHˆŸ HA H“‹œ
C
C    K=11  PAC—ET TO‹œKO J-APAMETPA, G = 0.
C
C    K=12  O POM CO CTAˆ‹. O KPˆBO‰ MO™HOCTˆ
C                IK = -IH + (PH-PK) * (1-COS(2*WT))/UK(T)
C    K=13  O POM CO CTAˆ‹. O KPˆBO‰ MO™HOCTˆ
C                TO †E.......PH+PK
C    K=14        IK = -IH + IH**2/(PH+PK)
C
C    B CˆCKE APAMETPOB PSUB5 OC‹E CTAH„APTH›X APAMETPOB C‹E„“žT:
C    T - EPˆO„ B›—ˆC‹EHˆŸ APAMETPOB ‡AKOHA;
C    NTOKOB - KO‹ˆ—ECTBO BBO„ˆM›X TOKOB;
C    NNAGR - KO‹ˆ—ECTBO HAƒP“‡OK B ‹. –Eˆ.
C    EPS - O˜ˆKA OPE„E‹EHˆŸ “CTAHOBˆB˜EƒOCŸ PE†ˆMA
C    M -  BAPˆAHT B›BO„A ˆHTEƒPA‹œH›X APAMETPOB ( 0  ˆ HE 0 )
C         PˆME—AHˆŸ: 1. KO‹ˆ—ECTBO BBO„ˆM›X HAPŸ†EHˆ‰ „O‹†HO ›Tœ
C         -----------    PABHO KO‹ˆ—ECTB“ BBO„ˆM›X TOKOB;
C                     2. OPŸ„OK BBO„A „‹Ÿ TOKOB ‹EMEHTOB ˆ ˆX HA-
C                        PŸ†EHˆ‰ E„ˆH›‰;
C                     3. EPB›Mˆ B CˆCKAX BBO„A C‹E„“žT TOK ˆ HAPŸ-
C                        †EHˆE KOMEHCATOPA, BTOP›Mˆ - TOK ˆ HAPŸ†E-
C                        HˆE KOMEHCˆP“EMO‰ HAƒP“‡Kˆ, „A‹EE - TOKˆ ˆ
C                        HAPŸ†EHˆŸ OCTA‹œH›X HAƒP“‡OK, „A‹EE - TOKˆ
C                        ˆ HAPŸ†EHˆŸ OCTA‹œH›X ‹EMEHTOB.
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
C    7                                  UH**2 - KBA„PAT „E‰CTB“ž™EƒO
C                                       ‡HA—EHˆŸ HAPŸ†EHˆŸ KOMEHCATO-
C                                       PA ( APAMETP ‡AKOHA “PAB‹EHˆŸ,
C                                       ˆCO‹œ‡“EM›‰ HA „AHHOM ˜AƒE ) ;
C    8                                  TEK“™EE ‡HA—EHˆE ˆHTEƒPA‹œHO‰
C                                       C“MM› (IK+IH)**2*DT, „‹Ÿ B›—ˆ-
C                                       C‹EHˆŸ KO””ˆ–ˆEHTA MO™HOCTˆ
C                                       HAƒP“‡Kˆ C KOMEHCATOPOM ;
C    9                                  EC‹ˆ INTMAS(9) = 1, TO PO–ECC
C                                       “CTAHOBˆB˜ˆ‰CŸ
C   10                                  T * P+  - COCTAB‹Ÿž™AŸ OMEHHO‰
C                                       MO™HOCTˆ KOMEHCATOPA
C   11                                  T * P-  - TO †E
C   12                                  T+  - ˆHTEPBA‹ O‹O†ˆTE‹œH›X
C                                       ‡HA—EHˆ‰ MƒH.MO™HOCTˆ KOM-PA
C   13                                  T-  - ˆHTEPBA‹ OTPˆ–ATE‹œH›X
C                                       ‡HA—EHˆ‰ MƒH.MO™HOCTˆ KOM-PA
C   14                                  QO  - OMEHHAŸ MO™HOCTœ KOM.
C   15                                  UK**2 * T - „‹Ÿ CTAˆ‹ˆ‡A–ˆˆ
C                                       ‡AACA HEPƒˆˆ KOMEHCATOPA
C   16                                  DW ‡A 1-›‰ EPˆO„ PAOT› KMˆ
C   17                                  IH**2 - APAMETP ‡AKOHA
C                                       “PAB‹EHˆŸ, HA „AHHOM ˜AƒE
C   18                                  ˆHTEƒP.C“MMA P(H+K)
C   19                                  P(H+K) - APAMETP ‡AKOHA “P.
C   20                                  TEK“™.‡H.ˆHTEƒP.C“MM› TOKA HAƒP.
C    20 +I          (I OT 1 „O NTOKOB)  TEK“™ˆE ‡HA—EHˆŸ ˆHTEƒPA‹œH›X
C                                       C“MM „‹Ÿ B›—ˆC‹EHˆŸ „E‰CTB“ž™ˆX
C                                       ‡HA—EHˆ‰ TOKOB ;
C    20+NTOKOB+I    (I OT 1 „O NTOKOB)  TO †E „‹Ÿ HAPŸ†EHˆ‰ ;
C    20+2*NTOKOB+I  (I OT 1 „O NTOKOB)  TO †E „‹Ÿ AKTˆBH›X MO™HOCTE‰ ;
C    ------------------------------------------------------------------
C-->  NMOD - „O“CTˆMOE KO‹ˆ—ECTBO MO„E‹E‰ „AHHOƒO TˆA B CXEME
      NMOD = 3
C-->  POˆ‡BO„ˆTCŸ PACPE„E‹EHˆE MACCˆBA  INTMAS  „‹Ÿ
C     MO„E‹E‰ „AHHOƒO TˆA  -  PA‡H›X ‹EMEHTOB CXEM›
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
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œHO‰ C“MM› „‹Ÿ PH
 106  INTMAS(NJ+5) = INTMAS(NJ+5)+STMAS(4)*STMAS(2+NTOKOB+1)*DT
      INTMAS(NJ+8) = INTMAS(NJ+8) + (STMAS(3) + STMAS(4))**2*DT
      INTMAS(NJ+18) = INTMAS(NJ+18) + (STMAS(4)+STMAS(3))*
     *STMAS(2+NTOKOB+1)*DT
      INTMAS(NJ+20) = INTMAS(NJ+20) + STMAS(4)**2*DT
C     B›—ˆC‹EHˆE OCTA‹œH›X ˆHTEƒPA‹œH›X C“MM
      DO 101 I=1,NTOKOB
      INTMAS(NJ+20+I) = INTMAS(NJ+20+I) + STMAS(2+I)**2*DT
      INTMAS(NJ+20+NTOKOB+I)=INTMAS(NJ+20+NTOKOB+I)+STMAS(2+NTOKOB+I)
     ***2*DT
 101  INTMAS(NJ+20+2*NTOKOB+I)=INTMAS(NJ+20+2*NTOKOB+I)+STMAS(2+I)*
     *STMAS(2+NTOKOB+I)*DT
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œHO‰ C“MM› „‹Ÿ HEPƒˆˆ KOMEHCATOPA
      INTMAS(NJ+3) = INTMAS(NJ+20+2*NTOKOB+1)
C----------------------------------------------------------------------
C-->  B›—ˆC‹EHˆE OMEHHO‰ MO™HOCTˆ KOMEHCATOPA  QO
      IF(STMAS(2+1)*STMAS(2+NTOKOB+1)) 121,115,115
 115  INTMAS(NJ+10) = INTMAS(NJ+10) + STMAS(2+1)*STMAS(2+NTOKOB+1)*DT
      INTMAS(NJ+12) = INTMAS(NJ+12) + DT
      GO TO 116
 121  INTMAS(NJ+11) = INTMAS(NJ+11) + STMAS(2+1)*STMAS(2+NTOKOB+1) *DT
      INTMAS(NJ+13) = INTMAS(NJ+13) + DT
C----------------------------------------------------------------------
 116  IF(PER.GT.INTMAS(NJ+2)) GO TO 107
C-->  ‡AOMHˆM BPEMŸ TOƒO ˜AƒA HA “„“™EE
      INTMAS(NJ+1)=INTMAS(NJ+1)+DT
C======================================================================
C     ƒ‹ABHAŸ ”OPM“‹A ˆTEPA–ˆˆ, B›—ˆC‹EHˆE TOKA KOMEHCATOPA
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
C-->  ‡AKOH—ˆM B›—ˆC‹EHˆŸ „‹Ÿ PE„›„“™EƒO EPˆO„A
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
C-->  POBEPKA HA “CTAHOBˆB˜ˆ‰CŸ PE†ˆM
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
C-->  “CTAHOBˆB˜ˆ‰CŸ PE†ˆM
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
C     PER = PER - 1.                   BO‡MO†HAŸ Pˆ—ˆHA COŸ
      WK=INTMAS(NJ+16)
      WRITE(NWTR,160)
      WRITE(NWTR,130) PER,STMAS(1),INTMAS(NJ+4)
C        O T ‹ A „ K A       B›BO„ INTMAS      ************************
C     WRITE(NWTR,200)
C200  FORMAT(//20X,'INTMAS       PSUB6 '/)
C
C191  WRITE(NWTR,190)(INTMAS(I),I=1,200)
C        K O H E –    OT‹A„O—H›X OEPATOPOB
      WRITE(7,180) KMH,KMK,SK,PPOT,KPD,PER,WK
      IF(URMAS(9).GE.0.) GO TO 134
C --> OC‹E„“ž™ˆ‰ OEPATOP  - OKOH—AHˆE MO„E‹ˆPOBAHˆŸ O „OCTˆ†EHˆˆ
C     “CTAHOBˆB˜EƒOCŸ PO–ECCA B –Eˆ C “PAB‹ŸEM›M KOMEHCAATOPOM
      TMAX = TN
      RETURN
 111  WRITE(NWTRE,110)
C-->  PO–ECC HE “CTAHOBˆ‹CŸ
C     ‡Aˆ˜EM HOB›E ‡HA—EHˆŸ APAMETPOB ‡AKOHA “PAB‹EHˆŸ
 108  IF(URMAS(9).GE.0.) GO TO 133
 134  INTMAS(NJ+6) = PNOB
      INTMAS(NJ+7) = UNOB
      INTMAS(NJ+17) = INTMAS(NJ+20)/T
      INTMAS(NJ+19) = PNOB1
C-->  HA—HEM HAKO‹EHˆE ˆHTEƒPA‹œH›X C“MM C HA—A‹A
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
 110  FORMAT(1X,'O„O‡PˆTE‹œHAŸ HAƒP“‡KA, PO„O‹†Až C—ˆTATœ')
 120  FORMAT(1X,'„E‰CTB“ž™. ‡H. TOKOB.',5X,'„E‰CTB“ž™. ‡H. HAPŸ†.',5X,
     *'AKTˆBH›E MO™HOCTˆ')
 130  FORMAT(1X,E12.5,20X,E12.5,20X,E12.5)
 140  FORMAT(1X,'AKT.MO™HOCTœ HAƒP“‡OK',5X,'MO™HOCTœ OTEPœ HEPƒˆˆ',5X
     *,' K..„. BCE‰ –Eˆ')
 150  FORMAT(1X,'KO”. MO™HOCTˆ HAƒP“‡Kˆ',5X,'KO”.MO™. C KOMEHCATOPOM
     *','O‹HAŸ MO™HOCTœ KOMEHCATOPA')
 160  FORMAT(1X,'BPEMŸ EPEX. PE†ˆMA KMˆ (EPˆO„›)',5X,'BPEMŸ ˜AƒA B›BO
     *„A TN',5X,'HEPƒˆŸ,PˆHŸTAŸ KOMEHCATOPOM')
 170  FORMAT(1X,'KO‹ˆ—ECTBO MO„E‹E‰ TˆA 17 PEB›Cˆ‹O „O“CTˆMOE ( ',I2
     *,' )')
 180  FORMAT(7E11.4)
C190  FORMAT(10E12.3)
 210  FORMAT(///'HOMEP A‹ƒOPˆTMA K = ',I3,'HE “KA‡AH B KATA‹OƒE'/)
      END
