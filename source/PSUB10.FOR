       SUBROUTINE PSUB10( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT ŸHBAPŸ  1988 ƒ.)------------
C   O„POƒPAMMA: PSUB10( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA‡HA—EHˆE:  / O‹œ‡OBATE‹Ÿ „‹Ÿ MO„E‹ˆPOBAHˆŸ
C   ----------   ‡AKOHOB “PAB‹EHˆŸ OT„E‹œH›M BEHTˆ‹EM ˆ‹ˆ
C                ƒP“O‰ BEHTˆ‹E‰.
C                  ( MO„E‹œ #301 - Cˆ”“ ).
C               O BE‹ˆ—ˆHAM, EPE„ABAEM›M O‹œ‡OBATE‹EM ˆ
C               ‡HA—EHˆŸM TOKOB ˆ HAPŸ†EHˆ‰ ‹EMEHTOB CXEM›
C               OPE„E‹ŸžTCŸ KO„ Cˆ“ KA†„OƒO BEHTˆ‹Ÿ ˆ ˜Aƒ „O
C               MOMEHTA ˆ‡MEHEHˆŸ ‡HA—EHˆŸ Cˆ“.
C
C######################################################################
C
C         O„POƒPAMMA ”OPMˆP“ET CˆƒHA‹› “PAB‹EHˆŸ K‹ž—AMˆ
C     Cˆ‹OBOƒO ‹OKA “PAB‹EHˆŸ ˜ECTˆ”A‡H›M ˜AƒOB›M „BˆƒATE‹EM
C     C „B“O‹ŸPH›M ˆTAHˆEM.
C
C######################################################################
C
C
C   METO„:     O—EBˆ„EH.
C   -----
C  APAMETP›:   NEL - HOMEP ‹EMEHTA, „‹Ÿ KOTOPOƒO PSUB PAOTAET;
C  ---------    URMAS - MACCˆB „AHH›X O‹œ‡OBATE‹Ÿ:
C                       1-E C‹OBO - KO‹ˆ—ECTBO APAMETPOB B CˆCKE
C                       ( ‡AˆCAHO –E‹›M —ˆC‹OM B BE™ECTBEHH›‰ MACCˆB )
C                       2-E C‹OBO ˆ „A‹EE - APAMETP› PSUB,
C                           ‡A„AHH›E O‹œ‡OBATE‹EM;
C
C----------------------------------------------------------------------
C              STMAS - MACCˆB „AHH›X, EPE„ABAEM›X B PSUB:
C
C      C‹OBO           OO‡HA—EHE   -   ‡HA—EHˆE
C      1               TN - BPEMŸ N - ƒO ˜AƒA PAC—ETA;
C      2               H0 - ˜Aƒ ˆHTEƒPˆPOBAHˆŸ;
C      3               I1 - ‡HA—EHˆE 1-ƒO TOKA, ‡ATPEOBAHHOƒO
C                           O‹œ‡OBATE‹EM, „‹Ÿ MOMEHTA BPEMEHˆ TN;
C      4               I11- TO†E, „‹Ÿ MOMEHTA TN+H0;
C         „A‹EE AHA‹Oƒˆ—HO ‡HA—EHˆŸ OCTA‹œH›X TOKOB;
C   N=3+2*NTOKOB       U1 - ‡HA—EHˆE 1-ƒO HAPŸ†EHˆŸ, ‡ATPEOBAHHOƒO
C                           O‹œ‡OBATE‹EM, „‹Ÿ MOMEHTA BPEMEHˆ TN;
C    N+1               U11- TO†E, „‹Ÿ MOMEHTA TN+H0;
C         „A‹EE AHA‹Oƒˆ—HO ‡HA—EHˆŸ OCTA‹œH›X HAPŸ†EHˆ‰;
C
C----------------------------------------------------------------------
C              DGMAS  -  MACCˆB B›—EC‹EHH›X APAMETPOB Cˆ“.
C
C   C‹OBO        OO‡HA—EHE - ‡HA—EHˆE:
C  OT 1 „O 6     - ‡HA—EHˆŸ MCIU ( KO„ Cˆ“ )
C                  MCIU = 00  --> "Cˆ“" HET B [TN, TN+H0];
C                                  HX = 0.0;
C                  MCIU = 01  --> B [TN, TN+H0] -> EPE„Hˆ‰ ”POHT
C                                 ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹ OT
C                                 TN „O EPE„HEƒO ”POHTA "Cˆ“";
C                  MCIU = 10 -->  B [TN, TN+H0] -> ‡A„Hˆ‰ ”POHT
C                                 ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹
C                                 OT TN „O ‡A„HEƒO ”POHTA ˆM“‹œCA  ;
C                  MCIU = 11 -->  B [TN, TN+H0] "Cˆ“" BK‹ž—EH,
C                                 COCTOŸHˆE HE MEHŸ‹, HX=0.0;
C
C  OT 7 „O 12    - ‡HA—EHˆŸ COOTBETCTB“ž™ˆX HX ( ˜AƒA O BPEMEHˆ
C                    „O MOMEHTA ˆ‡MEHEHˆŸ KO„A Cˆ“ )
C----------------------------------------------------------------------
C              INTMAS - MACCˆB ˆ‡ O„HOƒO C‹OBA.
C                       ( B O„POƒPAMME HE ˆCO‹œ‡“ETCŸ )
C----------------------------------------------------------------------
C
C   O„POƒPAMM›: HET
C   -------------
C
C   PˆME—AHˆE: C“MMA KO‹‹ˆ—ECTBA TPE“EM›X TOKOB ˆ HAPŸ†EHˆ‰
C   ----------  HE „O‹†HA PEB›˜ATœ 49-Tˆ.
C               ( KOHTPO‹œ HE POˆ‡BO„ˆTCŸ )
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       REAL INTMAS
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C BBO„ˆM „AHH›E
C----------------------------------------------------------------------
       X  = STMAS(3)
       X1 = STMAS(4)
       TX = URMAS(2)
       HAG = URMAS(3)
       ALFA= URMAS(4)
       TF  = URMAS(5)
C----------------------------------------------------------------------
C--> PˆBE„EM X ˆ X1 K O„HOM“ EPˆO„“
C----------------------------------------------------------------------
5      CONTINUE
       IF( X .LE. ( TX - ALFA - HAG )  ) GO TO 7
       X = X - TX
       GO TO 5
7      CONTINUE
       IF( X1 .LE. ( TX - ALFA - HAG )  ) GO TO 8
       X1 = X1 - TX
       GO TO 7
8      CONTINUE
C----------------------------------------------------------------------
C-->  OH“‹ŸEM B›XO„H›E „AHH›E
C----------------------------------------------------------------------
       DO 10 I= 1,12
       DGMAS(I) = 0.0
10     CONTINUE
C----------------------------------------------------------------------
C->   BBE„EM HOMEP PE„›„“™E‰ ”A‡›
       NPRED = IFIX( URMAS(6) )
C->   „‹ˆTE‹œHOCTœ BK‹ž—EHˆŸ PE„›„“™E‰ ”A‡›
       DIMP = URMAS(7)
C->   BPEMŸ BK‹ž—EHˆŸ TEK“™E‰ ”A‡›
       TIME1 = URMAS(8)
C->   BPEMŸ BK‹ž—EHˆŸ C‹E„“ž™E‰ ”A‡›
       TIME2 = URMAS(9)
C----------------------------------------------------------------------
C-->  OPE„E‹ˆM HOMEP ”A‡› KOTOPAŸ „.. BK‹ž—EHA B  T=TN  ˆ  T=TN+H0
       NTN  = IFIX( (X + ALFA + HAG)/HAG ) + 1
       NTN1 = IFIX( (X1+ ALFA + HAG)/HAG ) + 1
       IF( NTN .NE. NTN1 ) GO TO 1000
C----------------------------------------------------------------------
C-->  BK‹ž—EHA TA†E ”A‡A
       IF( NPRED .EQ. NTN ) GO TO 100
       IF( TN    .EQ. 0.0 ) GO TO 100
       DIMP  = TIME2 - TIME1
       TIME1 = TIME2
       NPRED = NTN - 1
       IF( NPRED .LT. 1 ) NPRED = 6
100    CONTINUE
       IF( NEL .EQ. 36  .AND.  NTN .GT. 3 ) GO TO 9999
       IF( NEL .EQ. 37  .AND.  NTN .LT. 4 ) GO TO 9999
       IF( NTN .GT. 3 ) NTN = NTN - 3
       DGMAS(NTN) = 11.
       DGMAS(NTN+3) = 11.
C--> KAK „E‹A C ”OPCˆPOBKO‰?
       TFORS = TIME1 + TF * DIMP
       IF( TN1 .LT. TFORS ) GO TO 9999
       DGMAS(NTN+3) = 10.
       DGMAS(NTN+9) = TFORS - TN
       GO TO 9999
C----------------------------------------------------------------------
C--> HA ˜AƒE „O‹†HA ˆ‡MEHˆTœCŸ ”A‡A
1000   CONTINUE
C--> HA‰„EM BPEMŸ ˆ‡MEHEHˆŸ CˆƒHA‹A
       XX = (NTN1 - 2) * HAG - ALFA
       TIMEX = TN + H0 * (XX - X)/(X1 - X)
       TIME2 = TIMEX
C--> KOMM“TˆP“EM...
C
C--> B›K‹ž—AEM
       IF( NEL .EQ. 36  .AND.  NTN .GT. 3 ) GO TO 2000
       IF( NEL .EQ. 37  .AND.  NTN .LT. 4 ) GO TO 2000
       IF( NTN .GT. 3 ) NTN = NTN - 3
       DGMAS(NTN) = 10.
       DGMAS(NTN+6) = TIMEX - TN
       DGMAS(NTN+3) = 10.
       DGMAS(NTN+9) = TIMEX - TN
2000   CONTINUE
C-->  BK‹ž—AEM
       IF( NEL .EQ. 36  .AND.  NTN1 .GT. 3 ) GO TO 9999
       IF( NEL .EQ. 37  .AND.  NTN1 .LT. 4 ) GO TO 9999
       IF( NTN1 .GT. 3 ) NTN1 = NTN1 - 3
       DGMAS(NTN1) = 1.
       DGMAS(NTN1+6) = TIMEX - TN
       DGMAS(NTN1+3) = 1.
       DGMAS(NTN1+9) = TIMEX - TN
C----------------------------------------------------------------------
C-->  KOH—AEM...
       URMAS(6) = FLOAT(NPRED)
       URMAS(7) = DIMP
       URMAS(8) = TIME1
       URMAS(9) = TIME2
       RETURN
9999   CALL SUBERR(9999)
C ????????
       END
