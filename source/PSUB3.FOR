       SUBROUTINE PSUB3( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: PSUB3( NEL, URMAS, STMAS, DGMAS, INTMAS )
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
       REAL INTMAS, ITN, ITN1, IMAX, UTN, UTN1, UMAX
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C OC‹E„“ž™AŸ —ACTœ / PA‡PAAT›BAETCŸ O‹œ‡OBATE‹EM
C----------------------------------------------------------------------
       ITN  = STMAS(3)
       ITN1 = STMAS(4)
       UTN  = STMAS(5)
       UTN1 = STMAS(6)
       IMAX = URMAS(2)
       UMAX = URMAS(3)
C----------------------------------------------------------------------
       DGMAS(1) = 0.0
       DGMAS(7) = 0.0
       IF( UTN1.GT. UMAX ) GO TO 100
       IF( UTN .GT. UMAX ) GO TO 100
       IF( ITN .GT. IMAX ) GO TO 200
       IF( ITN1.GT. IMAX ) GO TO 200
       RETURN
C----------------------------------------------------------------------
100    CONTINUE
       IF( UTN .GT. UMAX ) GO TO 120
C--->  EPE„Hˆ‰ ”POHT
       DGMAS(1) = 1.0
       DGMAS(7) = H0 * ( UMAX - UTN ) / ( UTN1 - UTN )
       RETURN
120    CONTINUE
       IF( UTN1.GT. UMAX ) GO TO 140
C--->  ‡A„Hˆ‰ ”POHT
       DGMAS(1) = 10.0
       DGMAS(7) = H0 * ( UTN1 - UMAX ) / ( UTN1 - UTN )
       RETURN
140    CONTINUE
C--->  Cˆ“ ECTœ
       DGMAS(1) = 11.0
       DGMAS(7) = 0.0
       RETURN
C----------------------------------------------------------------------
200    CONTINUE
       IF( ITN .GT. IMAX ) GO TO 220
C--->  EPE„Hˆ‰ ”POHT
       DGMAS(1) = 1.0
       DGMAS(7) = H0 * ( IMAX - ITN ) / ( ITN1 - ITN )
       RETURN
220    CONTINUE
       IF( ITN1.GT. IMAX ) GO TO 240
C--->  ‡A„Hˆ‰ ”POHT
       DGMAS(1) = 10.0
       DGMAS(7) = H0 * ( ITN1 - IMAX ) / ( ITN1 - ITN )
       RETURN
240    CONTINUE
C--->  Cˆ“ ECTœ
       DGMAS(1) = 11.0
       DGMAS(7) = 0.0
       RETURN
       END
