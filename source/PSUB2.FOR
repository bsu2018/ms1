       SUBROUTINE PSUB2( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: PSUB2( NEL, URMAS, STMAS, DGMAS, INTMAS )
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
C   PSUB 2 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD   XAF
C
C
C      KTI  TI1 ISZAD_1  TI2 ISZAD_2 .....
C                        I         I
C                        I<---*--->I
C
C      KTS  TS1 S_1      TS2    S_2  ...... >;
C                        I         I
C                        I         I
C                        I<---*--->I
C
C
C    ƒ„E:
C          -*-  „BA ‡HA—EHˆŸ, OˆC›BAž™ˆE O„H“ TO—K“ OPE„E‹EHˆŸ
C               COOTBETCTB“ž™E‰ ”“HK–ˆOHA‹œHO‰ ‡ABˆCˆMOCTˆ -->
C               OHA ‡A„AETCŸ KAK K“CO—HO-‹ˆHE‰HAŸ ”“HK–ˆŸ;
C
C       KTI  - KO‹-BO TO—EK „‹Ÿ OPE„E‹EHˆŸ ‡ABˆCˆMOCTˆ ISZAD OT "TN";
C
C       KTS  - KO‹-BO TO—EK „‹Ÿ OPE„E‹EHˆŸ ‡ABˆCˆMOCTˆ SZAD  OT "TN";
C
C
C  == HA—A‹O OˆCAHˆŸ ”ˆKCˆP“ETCŸ „‹Ÿ: ISZAD -> URMAS(10)
C                                      SZAD  -> URMAS(11 + "2"*KTI )
C  ==
C !!! BHˆMAHˆE -->  FM MOHˆTOP ‡HA—EHˆŸ "KTI" ˆ "KTS"  “ „ B O ˆ ‹    !!
C                                                      ------------
C
C  APAMETP›:   NEL - HOMEP ‹EMEHTA, „‹Ÿ KOTOPOƒO PSUB PAOTAET;
C  ---------    URMAS - MACCˆB „AHH›X O‹œ‡OBATE‹Ÿ:
C               *****
C       C‹OBO        ‡HA—EHˆE
C      1               8 - KO‹-BO APAMETPOB B CˆCKE URMAS
C      2               KE         ‡HA—EHˆE
C      3               URD00      ‡HA—EHˆE
C      4               KI         ‡HA—EHˆE
C      5               ISDZAD     ‡HA—EHˆE  -->  Pˆ KTI = 0
C      6               URQ00      ‡HA—EHˆE
C      7               KS         ‡HA—EHˆE
C      8               BETA       ‡HA—EHˆE
C      9               SZAD       ‡HA—EHˆE  -->  Pˆ KTS = 0
C      10              XAF        ‡HA—EHˆE
C  - - - - - - -- - - - - - - - - - - - - - - - - - - - - -
C      11        KTI      KO‹-BO TO—EK „‹Ÿ OˆCAHˆŸ ISDZAD=>”(T) ( *2 )
C
C OT 12 „O 11+"2"*KTI     TO—Kˆ K“CO—. ‹ˆHE‰HOƒO ‡A„AHˆŸ IZDZAD ->”(T)
C
C 12+"2"*KTI     KTS      KO‹-BO TO—EK „‹Ÿ OˆCAHˆŸ SZAD =>”(T) ( *2 )
C
C OT 13+"2"*KTI
C „O 12+"2"*KTI+"2"*KTS   TO—Kˆ OPE„E‹EHˆŸ --> SZAD=>”(T)
C
C
C  --> EC‹ˆ KTI = 0 -->  BCE ”OPMA‹œHO TO †E, HO B URMAS(11) <-> KTS
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
C   2          UYD0    - “P. HAPŸ†. O D OCˆ O. CˆCTEM›;
C   3          UYMOD   - MO„“‹œ BEKTOPA “P. HAPŸ†EHˆŸ;
C   4          ISD_‡A„.- ‡A„AHH›‰ TOK CTATOPA O OCˆ Q;
C   5          S_‡A„.  - ‡A„AHHOE CKO‹œ†EHˆE;
C -----------------------------------------------------
C
C
C              INTMAS - MACCˆB „‹Ÿ PEA‹ˆ‡A–ˆˆ ˆHTEƒPATOPOB,
C              ******  ( 5-Tœ C‹OB );
C            EPB›E „BA ‡HA—. XPAHŸTCŸ B TOKN/TOKN1;
C
C  PSUB2 --->  B›—ˆC‹ŸET:
C
C  UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* (KE+1) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]*(KE+1) - KE* XAF* IRQ0;
C
C  ƒ„E:  INT - ˆHTEƒPA‹;
C
C   O„POƒPAMM›:   KMAS02;
C   -------------
C
C   PˆME—AHˆE:
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
C
       REAL INTMAS, INTI, INTS, INT, KE, ISDZAD, KI, KS, KMAS02
C
       REAL ISQ0, ISD0, IRQ0, IRD0, INTSUM
C
C  BCTPOEHH›‰ ˆHTEƒPATOP--->
C
       INT( INTSUM, XN1, XN ) = INTSUM + H0 *(XN1 + XN )/ 2.0
C
       TN = STMAS(1)
       H0 = STMAS(2)
       TN1 = TN + H0
C######################################################################
C-->  OPE„E‹EHˆE ISDZAD  ˆ SZAD O TA‹. ”“HK. ‡ABˆC.
C
       ISDZAD = URMAS(5)
       SZAD =   URMAS(9)
C-->  CKO‹œKO APAMETPOB B PSUB-->
       KPAR = IFIX( URMAS(1) )
       IF ( KPAR .LE. 9 ) GO TO 1
C-->  KO‹-BO TO—EK „‹Ÿ ISDZAD--> KTI
       KTI = IFIX( URMAS(11) )
       IF ( KTI .LE. 0 ) GO TO 2
C-->  —TO-TO ECTœ, HA‰„EM ..
       ISDZAD = KMAS02(TN1, URMAS(11) )
2      CONTINUE
       IF ( KPAR .LE. (11+KTI) ) GO TO 1
C-->  —TO-TO ECTœ ˆ „‹Ÿ SZAD, HA‰„EM...
       KTS = IFIX( URMAS(12+KTI) )
       IF ( KTS .LE. 0 ) GO TO 1
       SZAD = KMAS02( TN1, URMAS(12+KTI) )
1      CONTINUE
C######################################################################
       DGMAS(4) = ISDZAD
       DGMAS(5) = SZAD
C
C--> ˆHTEƒPA‹›
       INTI = INTMAS(1)
       INTS = INTMAS(2)
C-->  HOB. ‡HA—.
       DIN = STMAS(4) - ISDZAD
       DIN1 = STMAS(14) - ISDZAD
C
       INTMAS(1) = INT( INTI, DIN1, DIN )
C
       DSN = STMAS(12) - SZAD
       DSN1 = STMAS(22) - SZAD
C
       INTMAS(2) = INT( INTS, DSN1, DSN )
C
       INTI = INTMAS(1)
C
       INTS = INTMAS(2)
C----------------------------------------------------------------------
C-->  “P. „‹Ÿ TN+H0  MOMEHTA BPEMEHˆ;
C
      UYD0=(URMAS(3)-URMAS(4)*INTI)*(URMAS(2)+1.)-URMAS(2)*STMAS(18)*
     *                                             URMAS(10)
C
      UYQ0=(URMAS(6)-URMAS(7)*STMAS(22)-URMAS(8)*INTS)*(URMAS(2)+1.)-
     =      URMAS(2)*STMAS(17) * URMAS(10)
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