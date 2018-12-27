       SUBROUTINE PSUB13( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT ¤¥ª ¡àï 1991 ƒ.)------------
C   O„POƒPAMMA: PSUB13( NEL, URMAS, STMAS, DGMAS, INTMAS )
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
C     „‹Ÿ “PAB‹ŸEMOƒO B›PŸMˆTE‹Ÿ, COPAHHOƒO O CXEME ‹APˆOHOBA.
C         BXO„H›Mˆ BE‹ˆ—ˆHAMˆ ŸB‹ŸžTCŸ CˆƒHA‹ “PAB‹EHˆŸ ( UYPR )
C     ˆ TPˆ OOPH›X CˆƒHA‹A „‹Ÿ KA†„OƒO HAPŸ†EHˆŸ ˆTAž™E‰ CETˆ
C     UOPAB, UOPBC, UOPCA, KOTOP›E „O‹†H› OEPE†ATœ "CBOˆ" ‹ˆHE‰H›E
C     HAPŸ†EHˆŸ HA O„ˆHAKOB›‰ “ƒO‹ ( =< 30 ‹. ƒPA„ ).
C         CˆƒHA‹› “PAB‹EHˆŸ HA TˆPˆCTOP› B›PŸMˆTE‹Ÿ ”OPMˆP“žTCŸ
C     Pˆ O‹O†ˆTE‹œHOM HAPŸ†EHˆˆ “PAB‹EHˆŸ Pˆ COBA„EHˆˆ EƒO C
C     HˆCA„Až™ˆM “—ACTKOM COOTBETCTB“ž™EƒO OOPHOƒO HAPŸ†EHˆŸ.
C         „‹ˆTE‹œHOCTœ ˆM“‹œCA “PAB‹EHˆŸ ‡A„AETCŸ O‹œ‡OBATE‹EM
C     —EPE‡ APAMETP / PSUB 13.
C         Pˆ EPEXO„E TOKA HAƒP“‡Kˆ —EPE‡ HO‹œ O„„EP†ˆBAETCŸ
C     E‡TOKOBAŸ A“‡A, „‹ˆTE‹œHOCTœ KOTOPO‰ ‡A„AETCŸ O‹œ‡OBATE‹EM
C     —EPE‡ APAMETP / PSUB 13.
C
C
C     OˆCAHˆE Cˆ”“, “PAB‹Ÿž™E‰ B›PŸMˆTE‹EM
C     C‹E„“ž™EE :
C
C       #  T 301  “ 0  
C           MAS < 5  #‹1  #CM1  #‹2  #CM2  #‹3  #CM3  #‹4  #CM4
C                    #‹5  #CM5  >
C           MAS < 0 >
C           PSUB  13 <  DIMP   TPAUSE  >  ;
C
C    ƒ„E
C      #‹1,#CM1 - HOMEP ‹EMEHTA, TOK KOTOPOƒO ˆCO‹œ‡“ETCŸ B KA—ECTBE
C                  CˆƒHA‹A “PAB‹EHˆŸ ;
C      #‹2,#CM2 - HOMEPA ‹EMEHTOB, TOK KOTOPOƒO ˆCO‹œ‡“ETCŸ B
C      #‹3,#CM3   KA—ECTBE OOPH›X CˆƒHA‹OB COOTBETCTBEHHO „‹Ÿ
C      #‹4,#CM4   ‹ˆHE‰H›X HAPŸ†EHˆ‰  "AB", "BC", "CA";
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
C                ¯à®£à ¬¬  ¨§¬¥­¥­  8.12.1991 ¤«ï ã¯à ¢«¥­¨ï ¨
C                ¨­¢¥àâ®à  ¨ ¢ë¯àï¬¨â¥«ï ....
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       REAL INTMAS
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C BBO„ˆM „AHH›E
C----------------------------------------------------------------------
       UYPR  = STMAS(3)
       UYPR1 = STMAS(4)
       UOPAB = STMAS(5)
       UOPAB1= STMAS(6)
       UOPBC = STMAS(7)
       UOPBC1= STMAS(8)
       UOPCA = STMAS(9)
       UOPCA1= STMAS(10)
       TOKNG = STMAS(11)
       TOKNG1= STMAS(12)
       IF( ABS(TOKNG) .LT. 1.E-4 ) TOKNG = 0.0
       IF( ABS(TOKNG1).LT. 1.E-4 ) TOKNG1= 0.0
       DIMP  = URMAS(9)
       TIMEP = URMAS(10)
       TIME1 = URMAS(2)
       TIME2 = URMAS(3)
       TIME3 = URMAS(4)
       TIME4 = URMAS(5)
       TIME5 = URMAS(6)
       TIME6 = URMAS(7)
       TIME0 = URMAS(8)
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
C--> “„EM PAOTATœ ?
C----------------------------------------------------------------------
C      á«¥¤ãîé ï ª®¬ ­¤  â®«ìª® ¤«ï ¢ë¯àï¬¨â¥«ï !!!!!
C      IF( UYPR .LE. 0.0  .AND.  UYPR1 .LE. 0.0 ) RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C--->  KAKO‰ BEHTˆ‹œ PAOTAET?
C----------------------------------------------------------------------
       C = UYPR1 - UYPR
C----------------------------------------------------------------------
C
C           BEHTˆ‹ˆ ”A‡›  "A"
C
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ KATO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       HXAK = -1.
       A = UOPAB1 - UOPAB
       IF( A .GT. 0 ) GO TO 100
       IF( A .EQ. C ) GO TO 100
       HXAK = H0 * (UYPR - UOPAB)/(A-C)
       IF( HXAK .LT. 0.0  .OR.  HXAK .GT. H0 ) HXAK = -1.
C----------------------------------------------------------------------
C-->  ”OPMˆP“EM B›XO„HO‰ CˆƒHA‹!
100    CONTINUE
       IF( HXAK .GE. 0.0 ) GO TO 2004
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
       if( time1 .GT. 0.0 ) go to 20005
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(1) = 1.
       DGMAS(7) = HXAK
       TIME1 = TN + HXAK
       DGMAS(5) = 1.
       DGMAS(11) = HXAK
       TIME5 = TN + HXAK
       go to 2005
20005  continue
C->  ‘¨£­ « ¡ë« ¨ á­®¢  ¯¥p¥¤­¨© äp®­â
       DGMAS(1) = 11.
       DGMAS(7) = 0.0
       TIME1 = TN + HXAK
       DGMAS(5) = 11.
       DGMAS(11) = 0.0
       TIME5 = TN + HXAK
2005   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ AHO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       HXAA = -1.
       A = UOPAB - UOPAB1
       IF( A .GT. 0 ) GO TO 200
       IF( A .EQ. C ) GO TO 200
       HXAA = H0 * (UYPR + UOPAB)/(A-C)
       IF( HXAA .LT. 0.0  .OR.  HXAA .GT. H0 ) HXAA = -1.
C----------------------------------------------------------------------
C-->  ”OPMˆP“EM B›XO„HO‰ CˆƒHA‹!
200    CONTINUE
       IF( HXAA .GE. 0.0 ) GO TO 2009
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
       if( time1 .GT. 0.0 ) go to 20010
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(4) = 1.
       DGMAS(10) = HXAA
       TIME4 = TN + HXAA
       DGMAS(2) = 1.
       DGMAS(8) = HXAA
       TIME2 = TN + HXAA
       go to 2010
20010  continue
C->  ‘¨£­ « ¡ë« ¨ á­®¢  ¯¥p¥¤­¨© äp®­â
       DGMAS(4) = 11.
       DGMAS(10) = 0.0
       TIME4 = TN + HXAA
       DGMAS(2) = 11.
       DGMAS(8) = 0.0
       TIME2 = TN + HXAA
2010   CONTINUE
C----------------------------------------------------------------------
C
C           BEHTˆ‹ˆ ”A‡›  "B"
C
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ KATO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       HXBK = -1.
       A = UOPBC1 - UOPBC
       IF( A .GT. 0 ) GO TO 300
       IF( A .EQ. C ) GO TO 300
       HXBK = H0 * (UYPR - UOPBC)/(A-C)
       IF( HXBK .LT. 0.0  .OR.  HXBK .GT. H0 ) HXBK = -1.
C----------------------------------------------------------------------
C-->  ”OPMˆP“EM B›XO„HO‰ CˆƒHA‹!
300    CONTINUE
       IF( HXBK .GE. 0.0 ) GO TO 2014
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
       if( time1 .GT. 0.0 ) go to 20015
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(2) = 1.
       DGMAS(8) = HXBK
       TIME2 = TN + HXBK
       DGMAS(6) = 1.
       DGMAS(12) = HXBK
       TIME6 = TN + HXBK
       go to 2015
20015  continue
C->  ‘¨£­ « ¡ë« ¨ á­®¢  ¯¥p¥¤­¨© äp®­â
       DGMAS(2) = 11.
       DGMAS(8) = 0.0
       TIME2 = TN + HXBK
       DGMAS(6) = 11.
       DGMAS(12) = 0.0
       TIME6 = TN + HXBK
2015   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ AHO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       HXBA = -1.
       A = UOPBC - UOPBC1
       IF( A .GT. 0 ) GO TO 400
       IF( A .EQ. C ) GO TO 400
       HXBA = H0 * (UYPR + UOPBC)/(A-C)
       IF( HXBA .LT. 0.0  .OR.  HXBA .GT. H0 ) HXBA = -1.
C----------------------------------------------------------------------
C-->  ”OPMˆP“EM B›XO„HO‰ CˆƒHA‹!
400    CONTINUE
       IF( HXBA .GE. 0.0 ) GO TO 2019
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
       if( time1 .GT. 0.0 ) go to 20020
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(5) = 1.
       DGMAS(11) = HXBA
       TIME5 = TN + HXBA
       DGMAS(3) = 1.
       DGMAS(9) = HXBA
       TIME3 = TN + HXBA
       go to 2020
20020  continue
C->  ‘¨£­ « ¡ë« ¨ á­®¢  ¯¥p¥¤­¨© äp®­â
       DGMAS(5) = 11.
       DGMAS(11) = 0.0
       TIME5 = TN + HXBA
       DGMAS(3) = 11.
       DGMAS(9) = 0.0
       TIME3 = TN + HXBA
2020   CONTINUE
C----------------------------------------------------------------------
C
C           BEHTˆ‹ˆ ”A‡›  "C"
C
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ KATO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       HXCK = -1.
       A = UOPCA1 - UOPCA
       IF( A .GT. 0 ) GO TO 500
       IF( A .EQ. C ) GO TO 500
       HXCK = H0 * (UYPR - UOPCA)/(A-C)
       IF( HXCK .LT. 0.0  .OR.  HXCK .GT. H0 ) HXCK = -1.
C----------------------------------------------------------------------
C-->  ”OPMˆP“EM B›XO„HO‰ CˆƒHA‹!
500    CONTINUE
       IF( HXCK .GE. 0.0 ) GO TO 2024
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
       if( time1 .GT. 0.0 ) go to 20025
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(3) = 1.
       DGMAS(9) = HXCK
       TIME3 = TN + HXCK
       DGMAS(4) = 1.
       DGMAS(10) = HXCK
       TIME4 = TN + HXCK
       go to 2025
20025  continue
C->  ‘¨£­ « ¡ë« ¨ á­®¢  ¯¥p¥¤­¨© äp®­â
       DGMAS(3) = 11.
       DGMAS(9) = 0.0
       TIME3 = TN + HXCK
       DGMAS(4) = 11.
       DGMAS(10) = 0.0
       TIME4 = TN + HXCK
2025   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTˆ‹œ AHO„HO‰ ƒP“›.
C----------------------------------------------------------------------
       HXCA = -1.
       A = UOPCA - UOPCA1
       IF( A .GT. 0 ) GO TO 600
       IF( A .EQ. C ) GO TO 600
       HXCA = H0 * (UYPR + UOPCA)/(A-C)
       IF( HXCA .LT. 0.0  .OR.  HXCA .GT. H0 ) HXCA = -1.
C----------------------------------------------------------------------
C-->  ”OPMˆP“EM B›XO„HO‰ CˆƒHA‹!
600    CONTINUE
       IF( HXCA .GE. 0.0 ) GO TO 2029
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
       if( time1 .GT. 0.0 ) go to 20030
C->  CˆƒHA‹A HE ›‹O, TEEPœ “„ET
       DGMAS(6) = 1.
       DGMAS(12) = HXCA
       TIME6 = TN + HXCA
       DGMAS(1) = 1.
       DGMAS(7) = HXCA
       TIME1 = TN + HXCA
       go to 2030
20030  continue
C->  ‘¨£­ « ¡ë« ¨ á­®¢  ¯¥p¥¤­¨© äp®­â
       DGMAS(6) = 11.
       DGMAS(12) = 0.0
       TIME6 = TN + HXCA
       DGMAS(1) = 11.
       DGMAS(7) = 0.0
       TIME1 = TN + HXCA
2030   CONTINUE
C----------------------------------------------------------------------
C-->  A KAK HA C—ET "E‡TOKOBO‰ A“‡›"?
C----------------------------------------------------------------------
C      PRZV = TOKNG * TOKNG1
C      HXTOK = 0.0
C      IF( PRZV .GE. 0.0 ) GO TO 3000
C---> A“‡A.
C      HXTOK = H0 * TOKNG / (TOKNG - TOKNG1)
C      IF( TIME0 .GT. 0.0  ) GO TO 3000
C      TIME0 = TN + HXTOK
C3000   CONTINUE
C       IF( TN .GE. (TIME0+TIMEP) ) GO TO 9999
C--> A“‡A => OH“‹ˆMCŸ
C       DO 4000 I=1,12
C       DGMAS(I) = 0.0
C4000   CONTINUE
C--> ‡AOMHˆMCŸ.
C9999   CONTINUE
       URMAS(2) = TIME1
       URMAS(3) = TIME2
       URMAS(4) = TIME3
       URMAS(5) = TIME4
       URMAS(6) = TIME5
       URMAS(7) = TIME6
       URMAS(8)= TIME0
       if ( TN .gt. 0.0 ) return
       DO 10123 I= 1,6
       DGMAS(I)  = 11.
       DGMAS(I+6)= 0.0
10123  CONTINUE
       time1 = h0/10.
       time2 = time1
       time3 = time1
       time4 = time1
       time5 = time1
       time6 = time1
       RETURN
C----------------------------------------------------------------------
       END
