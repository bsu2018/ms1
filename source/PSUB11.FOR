        SUBROUTINE PSUB11( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT APE‹Ÿ  1988 ƒ.)------------
C   O„POƒPAMMA: PSUB11( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA‡HA—EHˆE:  / O‹œ‡OBATE‹Ÿ „‹Ÿ MO„E‹ˆPOBAHˆŸ
C   ----------   HE‹ˆHE‰H›X ‹EMEHTOB
C
C                  ( MO„E‹œ #17 )
C   BAPˆAHT›:    BO‡MO†H› „BA BAPˆAHTA MO„E‹ˆ:
C   ---------  1.Pˆ‡HAK MO„E‹ˆ = 0, MO„E‹œ ‹ˆHE‰HAŸ,
C                O BE‹ˆ—ˆHAM, EPE„ABAEM›M O‹œ‡OBATE‹EM ˆ
C                ‡HA—EHˆŸM TOKA  ˆ HAPŸ†EHˆŸ HE‹ˆHE‰HOƒO ‹EMEHTA
C                „O MOMEHTA BPEMEHˆ TN BK‹ž—ˆTE‹œHO B O„POƒPAMME
C                OPE„E‹ŸžTCŸ ‡HA—EHˆŸ G,J-APAMETPOB MO„E‹ˆ „‹Ÿ T=TN+1
C              2.Pˆ‡HAK MO„E‹ˆ = 1, MO„E‹œ ˆTEPA–ˆOHHAŸ,
C                O BE‹ˆ—ˆHAM, EPE„ABAEM›M O‹œ‡OBATE‹EM ˆ
C                ‡HA—EHˆŸM TOKA  ˆ HAPŸ†EHˆŸ HE‹ˆHE‰HOƒO ‹EMEHTA
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
C                       4-E C‹OBO - KO„ Bˆ„A HE‹ˆHE‰HOCTˆ ( K )
C                       5-E C‹OBO ˆ „A‹EE  - KOHCTAHT› AHA‹ˆTˆ—ECKˆX
C                           B›PA†EHˆ‰, OPE„E‹ŸEM›X KO„OM ( K )
C----------------------------------------------------------------------
C              STMAS - MACCˆB „AHH›X, EPE„ABAEM›X B PSUB:
C
C      C‹OBO           OO‡HA—EHˆE   -   ‡HA—EHˆE
C      1          TN - BPEMŸ N - ƒO ˜AƒA PAC—ETA;
C      2          H0 - ˜Aƒ ˆHTEƒPˆPOBAHˆŸ;
C      3          I  - ‡HA—EHˆE TOKA HE‹ˆHE‰HOƒO ‹EMEHTA ‡ATPEO-
C                      BAHHOƒO  O‹œ‡OBATE‹EM, „‹Ÿ MOMEHTA BPEMEHˆ TN;
C      4          U  - ‡HA—EHˆE HAPŸ†EHˆŸ HE‹ˆHE‰HOƒO ‹EMEHTA, ‡ATPE-
C                      OBAHHOƒO O‹œ‡OBATE‹EM, „‹Ÿ MOMEHTA BPEMEHˆ TN;
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
C              DGMAS(4) - = 0.
C              DGMAS(5) - = 0.       P  E  ‡  E  P  B
C
C----------------------------------------------------------------------
C              INTMAS - MACCˆB ˆ‡ „B“XCOT (200) ‹EMEHTOB,
C                       B O„POƒPAMME  H E   ˆ C  O ‹ œ ‡ “ E T C Ÿ
C                                       ===============================
C----------------------------------------------------------------------
C
C   O„POƒPAMM›: HET
C   -------------
C
C----------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( NWTR ,MASPRT(13) )
       EQUIVALENCE ( NWTRE,MASPRT(14) )
       EQUIVALENCE ( TMAX ,MASPRT(43) )
C
C
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
       I = STMAS(3)
       U = STMAS(4)
       T0 = URMAS(3)
       IF(URMAS(2).EQ.0.AND.DGMAS(1).NE.1) RETURN
       DGMAS(1) = URMAS(2)
       DGMAS(4) = 0.
       DGMAS(5) = 0.
       IF(TN.LT.T0) GO TO 102
C----------------------------------------------------------------------
       K=URMAS(4)
C--->  K - HOMEP Bˆ„A HE‹ˆHE‰HOCTˆ
C======================================================================
C--> K=1        I = A * U**3
C          A - KOHCTAHTA, BBO„ˆTCŸ URMAS(5)
C
C
C    B CˆCKE APAMETPOB PSUB  OC‹E CTAH„APTH›X APAMETPOB C‹E„“žT:
C    KOHCTAHT› AHA‹ˆTˆ—ECKˆX B›PA†EHˆ‰ „‹Ÿ HE‹ˆHE‰H›X ‹EMEHTOB
C
C----------------------------------------------------------------------
      PI=3.141593
C======================================================================
C     ‡AˆCœ AHA‹ˆTˆ—ECKˆX B›PA†EHˆ‰ „‹Ÿ HE‹ˆHE‰H›X ‹EMEHTOB
 101  GO TO (1),K
C----------------------------------------------------------------------
   1  A = URMAS(5)
      DGMAS(2) = 0.
      DGMAS(3) = A * U**2
C----------------------------------------------------------------------
      RETURN
 102  DGMAS(2) = 1.E-6
      DGMAS(3) = 1.E-6
      RETURN
      END
