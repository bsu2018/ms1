       SUBROUTINE PSUB9( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT ŸHBAPŸ  1988 ƒ.)------------
C   O„POƒPAMMA: PSUB9( NEL, URMAS, STMAS, DGMAS, INTMAS )
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
C         O„POƒPAMMA ”OPMˆP“ET CˆƒHA‹› “PAB‹EHˆŸ TˆPˆCTOPAMˆ
C     TPEX”A‡HO-O„HO”A‡HOƒO PEOPA‡OBATE‹Ÿ —ACTOT› C HEOCPE„CTBEH-
C     HO‰ CBŸ‡œž ˆ ECTECTBEHHO‰ KOMM“TA–ˆE‰ Pˆ PA‡„E‹œHOM “PAB‹E-
C     Hˆˆ PAOTO‰ AHO„HO‰ ˆ KOTO„HO‰ ƒP“AMˆ.
C         BXO„H›Mˆ BE‹ˆ—ˆHAMˆ ŸB‹ŸžTCŸ CˆƒHA‹ “PAB‹EHˆŸ ( UYPR )
C     ˆ TPˆ OOPH›X CˆƒHA‹A „‹Ÿ KA†„O‰ ”A‡› ˆTAž™E‰ CETˆ  UOPA, UOPB,
C     UOPC, KOTOP›E „O‹†H› OEPE†ATœ "CBOˆ" ”A‡H›E HAPŸ†EHˆŸ HA
C     O„ˆHAKOB›‰ “ƒO‹ ( =< 60 ‹. ƒPA„ ).
C         CˆƒHA‹› “PAB‹EHˆŸ HA TˆPˆCTOP› KATO„HO‰ ƒP“› ”OPMˆP“žTCŸ
C     Pˆ O‹O†ˆTE‹œHOM HAPŸ†EHˆˆ “PAB‹EHˆŸ Pˆ COBA„EHˆˆ EƒO C
C     HˆCA„Až™ˆM “—ACTKOM COOTBETCTB“ž™EƒO OOPHOƒO HAPŸ†EHˆŸ.
C     „‹Ÿ AHO„HO‰ ƒP“› - TO†E Pˆ OTPˆ–ATE‹œHOM HAPŸ†EHˆˆ “PAB‹EHˆŸ.
C         „‹ˆTE‹œHOCTœ ˆM“‹œCA “PAB‹EHˆŸ ‡A„AETCŸ O‹œ‡OBATE‹EM
C     —EPE‡ APAMETP / PSUB 9.
C         Pˆ EPEXO„E TOKA HAƒP“‡Kˆ —EPE‡ HO‹œ O„„EP†ˆBAETCŸ
C     E‡TOKOBAŸ A“‡A, „‹ˆTE‹œHOCTœ KOTOPO‰ ‡A„AETCŸ O‹œ‡OBATE‹EM
C     —EPE‡ APAMETP / PSUB 9.
C
C
C     OˆCAHˆE Cˆ”“, “PAB‹Ÿž™E‰ TPEX”A‡HO-O„HO”A‡H›M PEOPA‡OBATE‹EM
C     C‹E„“ž™EE :
C
C       #  T 301  “ 0  
C           MAS < 5  #‹1  #CM1  #‹2  #CM2  #‹3  #CM3  #‹4  #CM4
C                    #‹5  #CM5  >
C           MAS < 0 >
C           PSUB  9  <  DIMP   TPAUSE  >  ;
C
C    ƒ„E
C      #‹1,#CM1 - HOMEP ‹EMEHTA, TOK KOTOPOƒO ˆCO‹œ‡“ETCŸ B KA—ECTBE
C                  CˆƒHA‹A “PAB‹EHˆŸ ;
C      #‹2,#CM2 - HOMEPA ‹EMEHTOB, TOK KOTOPOƒO ˆCO‹œ‡“ETCŸ B
C      #‹3,#CM3   KA—ECTBE OOPH›X CˆƒHA‹OB COOTBETCTBEHHO „‹Ÿ
C      #‹4,#CM4   ”A‡ "A", "B", "C";
C      #‹5,#CM5 - HOMEP ‹EMEHTA, TOK KOTOPOƒO ŸB‹ŸETCŸ TOKOM
C                  HAƒP“‡Kˆ ;
C      DIMP      - „‹ˆTE‹œHOCTœ ˆM“‹œCA “PAB‹EHˆŸ ( CEK.);
C      TPAUSE    - „‹ˆTE‹œHOCTœ E‡TOKOBO‰ A“‡›    ( CEK.).
C
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
       UYPR = STMAS(3)
       UYPR1= STMAS(4)
       UOPA = STMAS(5)
       UOPA1= STMAS(6)
       UOPB = STMAS(7)
       UOPB1= STMAS(8)
       UOPC = STMAS(9)
       UOPC1= STMAS(10)
       TOKNG= STMAS(11)
       TOKNG1=STMAS(12)
       IF( ABS(TOKNG) .LT. 1.E-4 ) TOKNG = 0.0
       IF( ABS(TOKNG1).LT. 1.E-4 ) TOKNG1= 0.0
       DIMP = URMAS(2)
       TIMEP = URMAS(3)
       TIME1 = URMAS(4)
       TIME2 = URMAS(5)
       TIME3 = URMAS(6)
       TIME4 = URMAS(7)
       TIME5 = URMAS(8)
       TIME6 = URMAS(9)
       TIME0 = URMAS(10)
       IF( (TIME1 + DIMP) .LT. TN ) TIME1 = 0.0
       IF( (TIME2 + DIMP) .LT. TN ) TIME2 = 0.0
       IF( (TIME3 + DIMP) .LT. TN ) TIME3 = 0.0
       IF( (TIME4 + DIMP) .LT. TN ) TIME4 = 0.0
       IF( (TIME5 + DIMP) .LT. TN ) TIME5 = 0.0
       IF( (TIME6 + DIMP) .LT. TN ) TIME6 = 0.0
       IF( (TIME0 + TIMEP).LT. TN ) TIME0 = 0.0
C----------------------------------------------------------------------
C-->  OH“‹ŸEM B›XO„H›E „AHH›E
C----------------------------------------------------------------------
       DO 10 I= 1,12
       DGMAS(I) = 0.0
10     CONTINUE
C----------------------------------------------------------------------
C->   ˆ™EM EPE„Hˆ‰ ”POHT
C----------------------------------------------------------------------
C--->  KAKO‰ BEHTˆ‹œ PAOTAET?
C--->  MO†ET PAOTAET BEHTˆ‹œ B ”A‡E A?
       HXA = -1.
       A = UOPA1 - UOPA
       C = UYPR1 - UYPR
       IF( A .EQ. C ) GO TO 100
       HXA = H0 * (UYPR - UOPA)/(A-C)
       IF( HXA .LT. 0.0  .OR.  HXA .GT. H0 ) HXA = -1.
100    CONTINUE
C--->  MO†ET PAOTAET BEHTˆ‹œ B ”A‡E B?
       HXB = -1.
       A = UOPB1 - UOPB
       C = UYPR1 - UYPR
       IF( A .EQ. C ) GO TO 200
       HXB = H0 * (UYPR - UOPB)/(A-C)
       IF( HXB .LT. 0.0  .OR.  HXB .GT. H0 ) HXB = -1.
200    CONTINUE
C--->  MO†ET PAOTAET BEHTˆ‹œ B ”A‡E C?
       HXC = -1.
       A = UOPC1 - UOPC
       C = UYPR1 - UYPR
       IF( A .EQ. C ) GO TO 300
       HXC = H0 * (UYPR - UOPC)/(A-C)
       IF( HXC .LT. 0.0  .OR.  HXC .GT. H0 ) HXC = -1.
300    CONTINUE
C----------------------------------------------------------------------
C-->  BEHTˆ‹œ KAKO‰ ƒP“› PAOTAET?
C----------------------------------------------------------------------
       PRZV = TOKNG * TOKNG1
       IA =1
       IK =1
       IF( PRZV .LE. 0.0 ) GO TO 500
       IF( TOKNG .GT. 0.0 ) IA = 0
       IF( TOKNG .LT. 0.0 ) IK = 0
       GO TO 1000
C----------------------------------------------------------------------
500    CONTINUE
       HXTOK = 0.0
       IF( TOKNG .NE. TOKNG1 ) GO TO 600
       IF( UYPR1 .GE. 0.0    ) IA = 0
       IF( UYPR1 .LT. 0.0    ) IK = 0
       GO TO 1000
C----------------------------------------------------------------------
600    CONTINUE
       HXTOK = H0 * TOKNG / (TOKNG - TOKNG1)
       IF( UYPR1 .GE. 0.0    ) IA = 0
       IF( UYPR1 .LT. 0.0    ) IK = 0
       IF( TIME0 .GT. 0.0    ) GO TO 1000
       TIME0 = TN + HXTOK
1000   CONTINUE
C----------------------------------------------------------------------
C-->  ”OPMˆP“EM B›XO„HO‰ CˆƒHA‹!
C----------------------------------------------------------------------
C
C           BEHTˆ‹ˆ ”A‡›  "A"
C
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ KATO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       IF( IK .EQ. 1  .AND.  HXA .GE.0.0  .AND.  UOPA .GE. UOPA1 )
     * GO TO 2004
C->  HA ˜AƒE EPE„HEƒO ”POHTA HA „AHHOM BEHTˆ‹E HET.
       IF( TIME1 .GT. 0.0 ) GO TO 2001
C->  CˆƒHA‹A HE ›‹O ˆ HET
       DGMAS(1) = 0.0
       GO TO 2005
2001   CONTINUE
C->  CˆƒHA‹ ›‹ ˆ “„ET
       IF( (TIME1 + DIMP) .LE. (TN+H0) ) GO TO 2002
       DGMAS(1) = 11.
       GO TO 2005
2002   CONTINUE
C->  CˆƒHA‹ ›‹, TEEPœ HE “„ET
       DGMAS(1) = 10.
       DGMAS(7) = TIME1 + DIMP - TN
       GO TO 2005
2004   CONTINUE
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(1) = 1.
       DGMAS(7) = HXA
       TIME1 = TN + HXA
2005   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ AHO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       IF( IA .EQ. 1  .AND.  HXA .GE.0.0  .AND.  UOPA .LE. UOPA1 )
     * GO TO 2009
C->  HA ˜AƒE EPE„HEƒO ”POHTA HA „AHHOM BEHTˆ‹E HET.
       IF( TIME4 .GT. 0.0 ) GO TO 2006
C->  CˆƒHA‹A HE ›‹O ˆ HET
       DGMAS(4) = 0.0
       GO TO 2010
2006   CONTINUE
C->  CˆƒHA‹ ›‹ ˆ “„ET
       IF( (TIME4 + DIMP) .LE. (TN+H0) ) GO TO 2007
       DGMAS(4) = 11.
       GO TO 2010
2007   CONTINUE
C->  CˆƒHA‹ ›‹, TEEPœ HE “„ET
       DGMAS(4) = 10.
       DGMAS(10) = TIME4 + DIMP - TN
       GO TO 2010
2009   CONTINUE
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(4) = 1.
       DGMAS(10) = HXA
       TIME4 = TN + HXA
2010   CONTINUE
C----------------------------------------------------------------------
C
C           BEHTˆ‹ˆ ”A‡›  "B"
C
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ KATO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       IF( IK .EQ. 1  .AND.  HXB .GE. 0.0  .AND.  UOPB .GE. UOPB1 )
     * GO TO 2014
C->  HA ˜AƒE EPE„HEƒO ”POHTA HA „AHHOM BEHTˆ‹E HET.
       IF( TIME2 .GT. 0.0 ) GO TO 2011
C->  CˆƒHA‹A HE ›‹O ˆ HET
       DGMAS(2) = 0.0
       GO TO 2015
2011   CONTINUE
C->  CˆƒHA‹ ›‹ ˆ “„ET
       IF( (TIME2 + DIMP) .LE. (TN+H0) ) GO TO 2012
       DGMAS(2) = 11.
       GO TO 2015
2012   CONTINUE
C->  CˆƒHA‹ ›‹, TEEPœ HE “„ET
       DGMAS(2) = 10.
       DGMAS(8) = TIME2 + DIMP - TN
       GO TO 2015
2014   CONTINUE
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(2) = 1.
       DGMAS(8) = HXB
       TIME2 = TN + HXB
2015   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ AHO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       IF( IA .EQ. 1  .AND.  HXB .GE. 0.0  .AND.  UOPB .LE. UOPB1 )
     * GO TO 2019
C->  HA ˜AƒE EPE„HEƒO ”POHTA HA „AHHOM BEHTˆ‹E HET.
       IF( TIME5 .GT. 0.0 ) GO TO 2016
C->  CˆƒHA‹A HE ›‹O ˆ HET
       DGMAS(5) = 0.0
       GO TO 2020
2016   CONTINUE
C->  CˆƒHA‹ ›‹ ˆ “„ET
       IF( (TIME5 + DIMP) .LE. (TN+H0) ) GO TO 2017
       DGMAS(5) = 11.
       GO TO 2020
2017   CONTINUE
C->  CˆƒHA‹ ›‹, TEEPœ HE “„ET
       DGMAS(5) = 10.
       DGMAS(11) = TIME5 + DIMP - TN
       GO TO 2020
2019   CONTINUE
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(5) = 1.
       DGMAS(11) = HXB
       TIME5 = TN + HXB
2020   CONTINUE
C----------------------------------------------------------------------
C
C           BEHTˆ‹ˆ ”A‡›  "C"
C
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ KATO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       IF( IK .EQ. 1  .AND.  HXC .GE. 0.0  .AND.  UOPC .GE. UOPC1 )
     * GO TO 2024
C->  HA ˜AƒE EPE„HEƒO ”POHTA HA „AHHOM BEHTˆ‹E HET.
       IF( TIME3 .GT. 0.0 ) GO TO 2021
C->  CˆƒHA‹A HE ›‹O ˆ HET
       DGMAS(3) = 0.0
       GO TO 2025
2021   CONTINUE
C->  CˆƒHA‹ ›‹ ˆ “„ET
       IF( (TIME3 + DIMP) .LE. (TN+H0) ) GO TO 2022
       DGMAS(3) = 11.
       GO TO 2025
2022   CONTINUE
C->  CˆƒHA‹ ›‹, TEEPœ HE “„ET
       DGMAS(3) = 10.
       DGMAS(9) = TIME3 + DIMP - TN
       GO TO 2025
2024   CONTINUE
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(3) = 1.
       DGMAS(9) = HXC
       TIME3 = TN + HXC
2025   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ AHO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       IF( IA .EQ. 1  .AND.  HXC .GE. 0.0  .AND.  UOPC .LE. UOPC1 )
     * GO TO 2029
C->  HA ˜AƒE EPE„HEƒO ”POHTA HA „AHHOM BEHTˆ‹E HET.
       IF( TIME6 .GT. 0.0 ) GO TO 2026
C->  CˆƒHA‹A HE ›‹O ˆ HET
       DGMAS(6) = 0.0
       GO TO 2030
2026   CONTINUE
C->  CˆƒHA‹ ›‹ ˆ “„ET
       IF( (TIME6 + DIMP) .LE. (TN+H0) ) GO TO 2027
       DGMAS(6) = 11.
       GO TO 2030
2027   CONTINUE
C->  CˆƒHA‹ ›‹, TEEPœ HE “„ET
       DGMAS(6) = 10.
       DGMAS(12) = TIME6 + DIMP - TN
       GO TO 2030
2029   CONTINUE
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(6) = 1.
       DGMAS(12) = HXC
       TIME6 = TN + HXC
2030   CONTINUE
C----------------------------------------------------------------------
C-->  A KAK HA C—ET "E‡TOKOBO‰ A“‡›"?
C----------------------------------------------------------------------
       IF( TN .GE. (TIME0+TIMEP) ) GO TO 9999
       DO 2050 I=1,12
       DGMAS(I) = 0.0
2050   CONTINUE
9999   CONTINUE
       URMAS(4) = TIME1
       URMAS(5) = TIME2
       URMAS(6) = TIME3
       URMAS(7) = TIME4
       URMAS(8) = TIME5
       URMAS(9) = TIME6
       URMAS(10)= TIME0
       RETURN
C----------------------------------------------------------------------
       END
