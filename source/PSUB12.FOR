       SUBROUTINE PSUB12( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT HOŸPŸ 1988 ƒ.)------------
C   O„POƒPAMMA: PSUB12( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA‡HA—EHˆE:  / O‹œ‡OBATE‹Ÿ „‹Ÿ MO„E‹ˆPOBAHˆŸ
C   ----------   ‡AKOHOB “PAB‹EHˆŸ TPEX”A‡H›M KOMEHCATOPOM
C                MO™HOCTˆ ˆCKA†EHˆŸ ˆ HECˆMMETPˆˆ
C                C ˆCO‹œ‡OBAHˆEM ‹ˆHE‰H›X HAPŸ†EHˆ‰
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
C                      10-E C‹OBO - RA - KO””ˆ–ˆEHT Pˆ IA**2
C                      11-E C‹OBO - RB - KO””ˆ–ˆEHT Pˆ IB**2
C                      12-E C‹OBO - RC - KO””ˆ–ˆEHT Pˆ IC**2
C
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
C      6               IHA - ‡HA—EHˆE TOKA ‹ˆHE‰HOƒO POBO„A A HAƒP“‡Kˆ
C      7               IHB - ‡HA—EHˆE TOKA ‹ˆHE‰HOƒO POBO„A B HAƒP“‡Kˆ
C      8               IHC - ‡HA—EHˆE TOKA ‹ˆHE‰HOƒO POBO„A C HAƒP“‡Kˆ
C      9               UAB - ‹ˆHE‰HOE HAPŸ†EHˆE  AB
C     10               UBC - ‹ˆHE‰HOE HAPŸ†EHˆE  BC
C     11               UCA - ‹ˆHE‰HOE HAPŸ†EHˆE  CA
C     12               UAB - HAPŸ†EHˆE „‹Ÿ PAC—ETA MO™HOCTˆ HAƒP“‡Kˆ
C     13               UBB - = 0
C     14               UCB - HAPŸ†EHˆE „‹Ÿ PAC—ETA MO™HOCTˆ HAƒP“‡Kˆ
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
C      K - HOMEP ‡AKOHA “PAB‹EHˆŸ
C======================================================================
C--> K=1 IKA = -IHA + PH/(UA**2 + UB**2 + UC**2) * (UAB(T) - UCA(T))/3
C        IKB = -IHB + PH/(UA**2 + UB**2 + UC**2) * (UBC(T) - UAB(T))/3
C        IKC = -IHC + PH/(UA**2 + UB**2 + UC**2) * (UCA(T) - UBC(T))/3,
C    ƒ„E
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
C--> K=2 O CPABHEHˆž C BAPˆAHTOM K=1 BBE„EHA CTAˆ‹ˆ‡A–ˆŸ HEPƒˆˆ
C        HAKOˆTE‹Ÿ
C        IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2) * (UAB(T) - UCA(T))/3
C        IKB=-IHB+(PH-PK)/(UA**2+UB**2+UC**2) * (UBC(T) - UAB(T))/3
C        IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2) * (UCA(T) - UBC(T))/3,
C    ƒ„E PK = DW/T , Pˆ—EM DW C MOMEHTA BK‹ž—EHˆŸ  KOMEHCATOPA
C
C======================================================================
C--> K=3 “PAB‹EHˆE O SIN U CO CTAˆ‹ˆ‡A–ˆE‰ HEPƒˆˆ O SIN
C IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2)*UA*SQRT(2)*SIN(OMEGA*T)
C IKB=-IHB+(PH-PK)/(UA**2+UB**2+UC**2)*UB*SQRT(2)*SIN(OMEGA*T-2*PI/3)
C IKC=-IKA-IKB
C
C======================================================================
C--> K=4 “PAB‹EHˆE O SIN U CO CTAˆ‹ˆ‡A–ˆE‰ HEPƒˆˆ O KPˆBO‰
C        HAPŸ†EHˆŸ
C IKA = -IHA + PH /(UA**2+UB**2+UC**2)*UA*SQRT(2)*SIN(OMEGA*T) -
C       PK / (UA**2+UB**2+UC**2) * (UAB(T)-UCA(T))/3
C IKB = -IHB + PH /(UA**2+UB**2+UC**2)*UB*SQRT(2)*SIN(OMEGA*T-2*PI/3)-
C       PK / (UA**2+UB**2+UC**2) * (UBC(T)-UAB(T))/3
C IKC = -IKA - IKB
C
C======================================================================
C-->K=5 “PAB‹EHˆE O SIN C ˆCO‹œ‡OBAHˆEM CPE„HEƒO ‡HA—EHˆŸ HAPŸ†EHˆŸ
C       ( PABH›E CˆMMETPˆ—H›E TOKˆ ), MO™HOCTœ ˆCT. = CONST
C IKA=-IHA+(PH-PK)/(UA**2+UB**2+UC**2)*UCP*SQRT(2)*SIN(OMEGA*T)
C IKB=-IHB+(PH-PK)/(UA**2+UB**2+UC**2)*UCP*SQRT(2)*SIN(OMEGA*T-2*PI/3)
C IKC = -IKA - IKB
C
C======================================================================
C-->K=6 “PAB‹EHˆE C “—ETOM COPOTˆB‹EHˆ‰ ‹ˆHˆˆ EPE„A—ˆ, E‡ OPATHO‰
C       CBŸ‡ˆ
C       IKA = -IHA - PH*RC/‡HAM*UAB - PH*RB/‡HAM*UAC
C       IKB = -IHB - PH*RC/‡HAM*UAB - PH*RA/‡HAM*UBC
C       IKC = -IKA - IKB ,
C       ƒ„E
C       ‡HAM=RA*(UA**2-2*UB**2-2*UC**2)+RB*(UB**2-2*UA**2-2*UC**2)+
C            RC(UC**2-2*UA**2-2*UB**2)
C       ˆ UA,UB,UC - „E‰CTB“ž™ˆE ‡HA—EHˆŸ
C
C
C======================================================================
C-->K=7 “PAB‹EHˆE E‡ B›—ˆC‹EHˆŸ ”A‡H›X HAPŸ†EHˆ‰
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
C    9                                  EC‹ˆ INTMAS(9) = 1, TO PO–ECC
C                                       “CTAHOBˆB˜ˆ‰CŸ
C   10                                  UA - „E‰CTB.‡H HA KOMEHC.
C   11                                  UB - „E‰CTB.‡H HA KOMEHC.
C   12                                  UC - „E‰CTB.‡H HA KOMEHC.
C   13                                  CPE„HEE ‡HA—EHˆE ”A‡H›X HAPŸ†.
C                                       KOMEHCATOPA ( UA+UB+UC )/3
C   14                                  C“MMA IHA**2+IHB**2+IHC**2
C   15 - 20                             P E ‡ E P B
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
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œH›X C“MM ”A‡H›X HAPŸ†EHˆ‰ UA,UB,UC
C     UA ---> ( UAB - UCA )
      INTMAS(NJ+20+NTOKOB+1) = INTMAS(NJ+20+NTOKOB+1) +
     *( STMAS(2+NTOKOB+1) - STMAS(2+NTOKOB+3) )**2 * DT
C     UB ---> ( UBC - UAB )
      INTMAS(NJ+20+NTOKOB+2) = INTMAS(NJ+20+NTOKOB+2) +
     *( STMAS(2+NTOKOB+2) - STMAS(2+NTOKOB+1) )**2 * DT
C     UC ---> ( UCA - UBC )
      INTMAS(NJ+20+NTOKOB+3) = INTMAS(NJ+20+NTOKOB+3) +
     *( STMAS(2+NTOKOB+3) - STMAS(2+NTOKOB+2) )**2 * DT
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œHO‰ C“MM› „‹Ÿ HEPƒˆˆ KOMEHCATOPA
      INTMAS(NJ+3) = INTMAS(NJ+3) + ( STMAS(3) * STMAS(2+NTOKOB+1) +
     *STMAS(4) * STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) * DT
C----------------------------------------------------------------------
 116  IF(PER.GT.INTMAS(NJ+2)) GO TO 107
C-->  ‡AOMHˆM BPEMŸ TOƒO ˜AƒA HA “„“™EE
      INTMAS(NJ+1)=INTMAS(NJ+1)+DT
C======================================================================
C     ƒ‹ABHAŸ ”OPM“‹A ˆTEPA–ˆˆ, B›—ˆC‹EHˆE J,G-APAMETPOB KOMEHCATOPA
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
C-->  “CTAHOBˆB˜ˆ‰CŸ PE†ˆM
      INTMAS(NJ+9) = 1.
 133  WRITE(NWTR,160)
      WRITE(NWTR,130)PER,STMAS(1),INTMAS(NJ+4)
C**********************************************************************
C E—ATœ INTMAS                                O T ‹ A „ K A
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
C-->  PO–ECC HE “CTAHOBˆ‹CŸ
C     ‡Aˆ˜EM HOB›E ‡HA—EHˆŸ APAMETPOB ‡AKOHA “PAB‹EHˆŸ
 108  IF(URMAS(9).GE.0.) GO TO 133
 134  INTMAS(NJ+6) = PNOB
      INTMAS(NJ+7) = UNOB
      INTMAS(NJ+10) = UA
      INTMAS(NJ+11) = UB
      INTMAS(NJ+12) = UC
      INTMAS(NJ+13) = UCP
      INTMAS(NJ+14) = SUMI
C-->  HA—HEM HAKO‹EHˆE ˆHTEƒPA‹œH›X C“MM C HA—A‹A
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œHO‰ C“MM› „‹Ÿ PH
      INTMAS(NJ+5) = ( STMAS(6) * STMAS(2+NTOKOB+4) +
     *STMAS(7) * STMAS(2+NTOKOB+5) + STMAS(8) * STMAS(2+NTOKOB+6)) *DT2
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œH›X C“MM ”A‡H›X HAPŸ†EHˆ‰ UA,UB,UC
C     UA ---> ( UAB - UCA )
      INTMAS(NJ+20+NTOKOB+1) =
     *( STMAS(2+NTOKOB+1) - STMAS(2+NTOKOB+3) )**2 * DT2
C     UB ---> ( UBC - UAB )
      INTMAS(NJ+20+NTOKOB+2) =
     *( STMAS(2+NTOKOB+2) - STMAS(2+NTOKOB+1) )**2 * DT2
C     UC ---> ( UCA - UBC )
      INTMAS(NJ+20+NTOKOB+3) =
     *( STMAS(2+NTOKOB+3) - STMAS(2+NTOKOB+2) )**2 * DT2
C-->  B›—ˆC‹EHˆE ˆHTEƒPA‹œHO‰ C“MM› „‹Ÿ HEPƒˆˆ KOMEHCATOPA
      INTMAS(NJ+3) = ( STMAS(3) * STMAS(2+NTOKOB+1) +
     *STMAS(4) * STMAS(2+NTOKOB+2) + STMAS(5) * STMAS(2+NTOKOB+3)) *DT2
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