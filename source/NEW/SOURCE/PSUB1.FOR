       SUBROUTINE PSUB1( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: PSUB1( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA‡HA—EHˆE:  / O‹œ‡OBATE‹Ÿ „‹Ÿ MO„E‹ˆPOBAHˆŸ
C   ----------   ‡AKOHOB PEƒ“‹ˆPOBAHˆŸ "PEƒ“‹ŸTOPA BO‡“†„EHˆŸ"
C                ( MAKPOMO„E‹œ # TˆA 40 )
C               O BE‹ˆ—ˆHAM, EPE„ABAEM›M O‹œ‡OBATE‹EM ˆ
C               BE‹ˆ—ˆHA—, PAC—ˆTAHH›M B MAKPOMO„E‹ˆ # 40
C               ( B POEK–ˆˆ HA OOPH“ž CˆCTEM“ OTC—ETA),
C               OPE„E‹ŸžTCŸ “PAB‹Ÿž™ˆE HAPŸ†EHˆŸ PEƒ“‹ˆPOBAHˆŸ
C               B TO‰ †E CˆCTEME OTC—ETA.
C   METO„:     B›—ˆC‹EHˆE O ‡A„AHH›M ”OPM“‹AM,
C   -----      „‹Ÿ ˆHTEƒPˆPOBAHˆŸ - METO„ TPAE–ˆˆ;
C
C              O‹œ‡OBATE‹œ BBO„ˆT —EPE‡ AAPAT "PSUB" :
C
C   PSUB 1 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD  XAF  >;
C
C
C  APAMETP›:   NEL - HOMEP ‹EMEHTA, „‹Ÿ KOTOPOƒO PSUB PAOTAET;
C  ---------    URMAS - MACCˆB „AHH›X O‹œ‡OBATE‹Ÿ:
C               *****
C       C‹OBO        ‡HA—EHˆE
C      1               8 - KO‹-BO APAMETPOB B CˆCKE URMAS
C      2               KE         ‡HA—EHˆE
C      3               URD00      ‡HA—EHˆE
C      4               KI         ‡HA—EHˆE
C      5               ISDZAD     ‡HA—EHˆE
C      6               URQ00      ‡HA—EHˆE
C      7               KS         ‡HA—EHˆE
C      8               BETA       ‡HA—EHˆE
C      9               SZAD       ‡HA—EHˆE
C      10              XAF        ‡HA—EHˆE
C  - - - - - - -- - - - - - - - - - - - - - - - - - - - - -
C
C
C              STMAS - MACCˆB „AHH›X, EPE„ABAEM›X B PSUB:
C              *****
C      C‹OBO           OO‡HA—EHE   -   ‡HA—EHˆE
C      1               TN - BPEMŸ N - ƒO ˜AƒA PAC—ETA;
C      2               H0 - ˜Aƒ ˆHTEƒPˆPOBAHˆŸ;
C-„‹Ÿ TN+H0***„‹Ÿ TN ***
C   I           I
C   Y           Y
C   13         3       ISQ0 - TOK CTATOPA Q OCœ OOPHAŸ;
C   14         4       ISD0 - TOK CTATOPA D OCœ OOPHAŸ;
C   15         5       USQ0 - HAPŸ†EHˆE CTATOPA Q OCœ OOPHAŸ;
C   16         6       USD0 - HAPŸ†EHˆE CTATOPA D OCœ OOPHAŸ;
C   17         7       IRQ0 - TOK POTOPA Q OCœ OOPHAŸ;
C   18         8       IRD0 - TOK POTOPA D OCœ OOPHAŸ;
C   19         9       OMEG0 - OOPHAŸ —ACTOTA;
C   20        10       OMEGA - “ƒ‹OBAŸ CKOPOCTœ POTOPA;
C   21        11       GAMMA - “ƒO‹ ME†„“ CTATOP. ˆ POTOP. ”A‡A "A";
C   22        12       S - CKO‹œ†EHˆE;
C------------------------
C   14 *--* 22 - TO †E „‹Ÿ T = TN+H0;
C   23, 24, 25 -  P E ‡ E P B;
C
C------------------------------------------------------
C
C
C          DGMAS  -  MACCˆB B›—EC‹EHH›X ‡HA—EHˆE HAPŸ†EHˆ‰
C          *****                        “PAB‹EHˆŸ B Q_D O.
C
C   C‹OBO        OO‡HA—EHE - ‡HA—EHˆE:
C   1          UYQ0    - “P. HAPŸ†. O Q OCˆ O. CˆCTEM›;
C   2          UYD0       - “P. HAPŸ†. O D OCˆ O. CˆCTEM›;
C   3          UYMOD      - MO„“‹œ BEKTOPA “P. HAPŸ†EHˆŸ;
C   4          I_S_D_‡A„. - ‡A„AHH›‰ TOK O D CTATOPA;
C   5          S_‡A„.     - ‡A„AHHOE CKO‹œ†EHˆE;
C -----------------------------------------------------
C
C
C              INTMAS - MACCˆB „‹Ÿ PEA‹ˆ‡A–ˆˆ ˆHTEƒPATOPOB,
C              ******  ( 5-Tœ C‹OB );
C                EPB›E 2- ‡HA—. B TOKN/TOKN1 XPAHŸTCŸ;
C
C  PSUB1 --->  B›—ˆC‹ŸET:
C
C  UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* (KE+1) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]*(KE+1) - KE* XAF* IRQ0;
C
C  ƒ„E:  INT - ˆHTEƒPA‹;
C
C   O„POƒPAMM›: HET
C   -------------
C
C
C   PˆME—AHˆE:
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
C
       REAL INTMAS, INTI, INTS, INT, KE, ISDZAD, KI, KS
C
       REAL ISQ0, ISD0, IRQ0, IRD0, INTSUM
C
C  BCTPOEHH›‰ ˆHTEƒPATOP--->
C
       INT( INTSUM, XN1, XN ) = INTSUM + H0 *(XN1 + XN )/ 2.0
C
       TN = STMAS(1)
       H0 = STMAS(2)
C$$$$$$TN1 = TN + H0
C--> ˆHTEƒPA‹›
       INTI = INTMAS(1)
       INTS = INTMAS(2)
C-->  HOB. ‡HA—.
       DIN = STMAS(4) - URMAS(5)
       DIN1 = STMAS(14) - URMAS(5)
C
       INTMAS(1) = INT( INTI, DIN1, DIN )
C
       DSN = STMAS(12) - URMAS(9)
       DSN1 = STMAS(22) - URMAS(9)
C
       INTMAS(2) = INT( INTS, DSN1, DSN )
C
       INTI = INTMAS(1)
C
       INTS = INTMAS(2)
C
       DGMAS(4) = URMAS(5)
C
       DGMAS(5) = URMAS(9)
C
C----------------------------------------------------------------------
C-->  “P. „‹Ÿ TN+H0  MOMEHTA BPEMEHˆ;
C
      UYD0=(URMAS(3)-URMAS(4)*INTI)*(URMAS(2)+1.)-URMAS(2)*STMAS(18)*
     *                                             URMAS(10)
C
      UYQ0=(URMAS(6)-URMAS(7)*STMAS(22)-URMAS(8)*INTS)*(URMAS(2)+1.)-
     =      URMAS(2)*STMAS(17)*URMAS(10)
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