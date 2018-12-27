         SUBROUTINE DM0015 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0015 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    MOHˆTOP MO„E‹ˆ "BO‡“„ˆTE‹œ" [ MO„. #50, .Tˆ # 6 ]
C   -----------    ****************************************************
C                   *** H—E -3”_0 --> PEOPA‡OBATE‹œ —ACTOT›*********
C
C                      O “PAB‹Ÿž™ˆM HAPŸ†EHˆŸM, O‹“—AEM›M ˆ‡
C              MO„E‹E‰ ‹EMEHTOB, ˆX B›PAAT›BAž™ˆX --> #40(PEƒ“‹ŸTOP
C              BO†“†„EHˆŸ), #13("ˆ‡MEPˆTE‹œ" HAPŸ†EHˆŸ), #63( 3-X
C              ”A‡HAŸ .„.C. ) ˆ T.. ( B TOKN/TOKN1 -> EPB›E TPˆ
C              ‡HA—EHˆŸ -> TO ‡HA—EHˆE HAPŸ†EHˆŸ PEƒ“‹ˆPOBAHˆŸ ),
C             ”OPMˆP“ET CˆƒHA‹› "Cˆ“" HA 18-Tœ BEHTˆ‹E‰ PEOPA‡OBATE‹Ÿ
C             —ACTOT›; OPƒAHˆ‡“ET B›PAOTK“ CˆƒHA‹OB "‡APET/PA‡PE˜EHˆE"
C             „‹Ÿ "AHO„H›X" ˆ "KATO„H›X" ƒP“ BEHTˆ‹E‰ B KA†„O‰ ˆ‡
C             ”A‡;
C
C
C              ===  AM‹ˆT“„› “PAB. CˆƒHA‹OB OƒPAHˆ—ˆBAžTCŸ O
C                   EYYV  ˆ EYYI -->  CM. SEQMAX
C              ===
C
C
C######################################################################
C    PACPE„E‹EHˆE HAPŸ†EHˆ‰ “PAB‹EHˆŸ O ”A‡AM ˆ AHO„HO-KATO„H›M
C                      ƒ P “   A M:
C
C                      HAPŸ†EHˆE “PAB‹EHˆŸ(ˆ‡ MSW_PEƒ“‹ŸTOPA):
C                      -----------------------------------------
C
C     KATO„HAŸ ƒP“A:                         AHO„HAŸ ƒP“A:
C
C   BEHT. 01, 02, 03 -->"-"EYYA         BEHT. 04, 05, 06 --> "+"EYYA
C   BEHT. 07, 08, 09 -->"-"EYYB         BEHT. 10, 11, 12 --> "+"EYYB
C   BEHT. 13, 14, 15 -->"-"EYYC         BEHT. 16, 17, 18 --> "+"EYYC
C
C  ƒ„E EYYA, EYYB, EYYC - CˆƒHA‹› ˆ‡ TOKN/N1 ‹EMEHTA "#‹" “PAB‹EHˆŸ,
C                         EPB›E TPˆ ‡HA—EHˆŸ; ( ”A‡A A, B, C );
C
C
C        PACPE„E‹EHˆE OOPH›X HAPŸ†EHˆ‰:
C        ---------------------------------
C BEHTˆ‹ˆ ”A‡› "A":       BEHTˆ‹ˆ ”A‡› "B":       BEHTˆ‹ˆ ”A‡› "C":
C  !!! KATO„H›E !!!        !!! KATO„H›E !!!        !!! KATO„H›E !!!
C -----------------       ----------------        -----------------
C 03     --> "+"V(KXB)    02     --> "+"V(KXC)    01     --> "+"V(KXA)
C 09     --> "+"V(KXB)    08     --> "+"V(KXC)    07     --> "+"V(KXA)
C 15     --> "+"V(KXB)    14     --> "+"V(KXC)    13     --> "+"V(KXA)
C   "+" ”A‡A "B"              "+" ”A‡A "C"               "+" ”A‡A "A"
C ( “‡E‹ KXB )               ( “‡E‹ KXC )            ( “‡E‹ KXA )
C T.E.-120 ƒP„. OT "A"    T.E.-120 ƒP„. OT "B"    T.E.-120 ƒP„. OT "C"
C
C BEHTˆ‹ˆ ”A‡› "A":       BEHTˆ‹ˆ ”A‡› "B":       BEHTˆ‹ˆ ”A‡› "C":
C !!! AHO„H›E !!!          !!! AHO„H›E !!!        !!! AHO„H›E !!!
C -----------------       ----------------        -----------------
C     06 --> "-"V(KXB)        05 --> "-"V(KXC)        04 --> "-"V(KXA)
C     12 --> "-"V(KXB)        11 --> "-"V(KXC)        10 --> "-"V(KXA)
C     18 --> "-"V(KXB)        17 --> "-"V(KXC)        16 --> "-"V(KXA)
C   "-" ”A‡A "B"              "-" ”A‡A "C"               "-" ”A‡A "A"
C ( “‡E‹ KXB )               ( “‡E‹ KXC )            ( “‡E‹ KXA )
C T.E. +60 ƒP„. OT "A"    T.E. +60 ƒP„. OT "B"    T.E. +60 ƒP„. OT "C"
C
C######################################################################
C----------------------------------------------------------------------
C   "CXEMA" PEOPA‡OBATE‹Ÿ:
C BEHTˆ‹œ:1  2  3   4  5  6  7  8  9  10  11 12  13  14  15  16  17  18
C ”A‡A:   C  B  A   C  B  A  C  B  A   C   B  A   C   B   A   C   B   A
C ƒP“A:  KATO„     AHO„     KATO„     AHO„       KATO„        AHO„
C B›XO„:    ”A‡A   A            ”A‡A  B               ”A‡A   C
C B TOKN: 7  8  9  10  11 12 13 14 15  16 17 18   19 20 21   22 23 24
C "Cˆ“"  25 26 27   28 29 30 31 32 33  34 35 36   37 38 39   40 41 42
C----------------------------------------------------------------------
C
C              ‹EMEHT OˆC›BAETCŸ:
C
C    3 T 50 “ KA KB KC KRA KRB KRC
C    T_BK‹  DTCIU #‹_TA_PEƒ“‹ŸTOPA_BO‡“†„EHˆŸ<ƒEHEPATOP
C                                                “PAB‹Ÿž™EƒO
C                                                HAPŸ†EHˆŸ>;
C
C            KXA  KXB  KXC   EYYV   EYYI;
C     ƒ„E:
C     ***
C              KA, KB, KC  - “‡‹› ˆTAž™EƒO 3-X ”A‡HOƒO HAPŸ†EHˆŸ;
C              KRA, KRB, KRC - “‡‹› POTOPHO‰ OMOTKˆ ‹. MA˜ˆH› ->
C                              B›XO„HOE HAPŸ†EHˆE PEOPA‡OBATE‹Ÿ
C                              —ACTOT›;
C
C              „O BPEMEHˆ T_BK‹ -> “‡‹› KRA, KRB, KRC -> ‡AKOPO—EH›
C                 **************    ( MO„E‹ˆP“žTCŸ "ˆ„EA‹œH›M" K‹ž—EM );
C
C
C              DTCIU - „‹ˆTE‹œHOCTœ ˆM“‹œCA "Cˆ“" ->
C                      ˆM“‹œC› ƒEHEPˆP“žTCŸ COƒ‹ACHO CPABHEHˆž
C                      HAP. OOPHOƒO -> B›ˆPAEMOƒO ˆ‡ MACCˆBA
C                      TOKN/TOKN1 ‹EMEHTA #_‹._PEƒ._BO‡“†.
C                      ( TPˆ EPB›E C‹OBA ) ˆ HAPŸ†EHˆŸ ˆTAHˆŸ
C                      PEOPA‡OBATE‹Ÿ, T.E. HA “‡‹AX KA, KB, KC;
C                      ( CM. DM0015 )
C              #_‹.BO‡. - MO„E‹œ OPE„E‹EHˆŸ HAPŸ†EHˆŸ “PAB‹EHˆŸ:
C                         ( Tˆ #40, Tˆ # 63, Tˆ # 13 );
C                         ###############################
C
C                      KXA, KXB, KXC - “‡‹› "OOPHO‰" CˆCTEM›
C                                      HAPŸ†EHˆ‰ ”A‡A: "A", "B", "C";
C
C   EYYV - OƒPAHˆ—EHˆE AM‹ˆT“„› “P. CˆƒHA‹A B B›PŸMˆTE‹œHOM PE†ˆME.
C   EYYI - OƒPAHˆ—EHˆE AM‹ˆT“„› “P. CˆƒHA‹A B ˆHBEPTOPHOM    PE†ˆME.
C
C
C === B›BO„H›E ‡HA—EHˆŸ: I1, I2, I3, I4, I5, I6, - TOKˆ BHE˜. B›BO„OB,
C                        IV1, IV2, ..., IV18,    - TOKˆ BEHTˆ‹E‰,
C                        CIU1, CIU2, ..., CIU18 - "Cˆ“" HA BEHTˆ‹E;
C
C
C                    OC‹“†ˆBAHˆE BEHTˆ‹E‰ —EPE‡ / DM00B9, K‹ž—E‰ -
C                      "B P“—H“ž"( HEOCPE„CTBEHHO B POƒPAMME );
C
C                  B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C
C                  B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                 -  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰
C                   ‡HA—EHˆ‰ "POBO„ˆMOCTœ" ‡AKP›T›X BEHTˆ‹E‰ ( GVENT6 )
C                   [ B „AHHO‰ BEPCˆˆ KOM‹EKCA ];
C                  - ”OPMˆPOBAHˆE BEKTOPA K.‡. “‡‹OB "LOGYTR"
C                    „‹Ÿ ˆH–ˆ„EHTH›X “‡‹OB --> ˆCO‹œ‡“ETCŸ
C                     /  GETLOG --> OC“™ECTBˆTœ "‹EMEHTAPH›‰" BK‹A„;
C
C                  1A) ZAPROS = 10
C                      ------------
C                      ”OPMˆPOBAHˆE "TEHEBO‰" CˆCTEM› “PABHEHˆ‰
C                       ( O‹EMEHTH›‰ BK‹A„ )
C
C                  B) ZAPROS = 20
C                     ------------
C                  B›—ˆC‹EHˆE ‡HA—EHˆ‰ TOKOB B BETBŸX B T(N+1) MOMEHT
C                  BPEMEHˆ. Pˆ TOM TOK OTKP›T›X BEHTˆ‹E‰:(“‡‹› K ˆ M )
C                  <TOKN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK ‡AKP›T›X BEHTˆ‹E‰ = 0.0;
C                  ‡AˆCœ TˆX TOKOB B MACCˆB TOKN1;
C                   !!! B›O‹HŸTœ „O MO„ˆ”ˆKA–ˆˆ PE˜EHˆŸ VALLN1 !!!
C
C                  C) ZAPROS = 3
C                     -----------
C                      O OPE„E‹EHH›M TOKAM BEHTˆ‹œH›X "‹E—"
C                      ”OPMˆP“ET CˆƒHA‹› "PA‡PE˜EHˆE/‡APET" CX ˆ TX
C                      ˆCXO„Ÿ ˆ‡ HAPŸ†EHˆ‰ “PAB‹EHˆŸ ˆ OOPH›X
C                      HAPŸ†EHˆ‰ ( “‡‹› KA, KB, KC ) -->
C                      OPE„E‹ŸET MOMEHT› "ECTECTBEHHO‰" KOMM“TA–ˆˆ
C                      BEHTˆ‹E‰ ( CM. / MCIU50 ); HA OCHOBE TOƒO
C                     Oˆ‡BO„ˆTCŸ AHA‹ˆ‡ HA BO‡MO†H“ž CMEH“ COCTOŸHˆŸ
C                     BEHTˆ‹Ÿ [ OTKP›T <==> ‡AKP›T ]:
C                     Pˆ TOM: FKOM = 0 - COCTOŸHˆE HE ˆ‡MEHˆ‹OCœ;
C                               FKOM = 1 - HA TEK“™EM ˜AƒE COCTOŸHˆE
C                                         „O‹†HO "›‹O" ˆ‡MEHˆTœCŸ B
C                                         B T = TX MOMEHT BPEMEHˆ;
C                                         PEA‹œHO B POƒPAMME OPE„E-
C                                         ‹ŸETCŸ HX - ˜Aƒ BO‡MO†HO‰
C                                         KOMM“TA–ˆˆ, ‡ATEM TX=TN+HX;
C                                         Pˆ HX < HXMIN [MASPRT(62)]
C                                         PˆHˆMAETCŸ HX = HXMIN;
C                                         COCTOŸHˆE HX=0.0 B›‡›BAET
C                                         E—ATœ COO™EHˆŸ;
C                                         POˆ‡BE„EH› COTBETCTB“ž™ˆE
C                                         ˆ‡MEHEHˆŸ B MASPRT:
C                                          FKOM <== "1"
C                                         HXG = MIN( HX, HXG )
C                                         TXG = TN + HXG
C   !!! OT 01.08.86  !!!
C              “—ET: ICLOSE - MO„“‹œ TOKA "‡AKP›TˆŸ" BEHTˆ‹Ÿ;
C                    UOPEN  - MO„“‹œ HAPŸ†EHˆŸ "OTKP›TˆŸ" BEHTˆ‹Ÿ;
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - Pˆ TX_G  [MASPRT(35)] .EQ. TX_BEHTˆ‹Ÿ   -
C                      ”ˆKCˆP“ETCŸ CMEHA COCTOŸHˆŸ BEHTˆ‹Ÿ APAMETPOM
C                      KD;
C                     PABEHCTBO POBEPŸETCŸ C TO—. „O "EPSKOM".
C
C   METO„:         PA‡HOBˆ„HOCTœ BO‹HOBOƒO A‹ƒOPˆTMA „‹Ÿ ”OPMˆPOBAHˆŸ
C   -----          MACCˆBA LOGYTR --> —EE‡ / GETLOG;
C
C                  "TEHEBAŸ" CˆCTEMA ”OPMˆP“ETCŸ "MAC˜TAˆPOBAHHAŸ"
C                   TAK, —TO: V[X] - V[Y] = <TOK    X-Y   BETBˆ>;
C                   “‡‹›, OTME—EHH›E B LOGYTR(*) = 0 - "A‡OB›E"
C                   „‹Ÿ KA†„O‰ ˆH–ˆ„EHTHO‰ ƒP“› OTKP›T›X BEHTˆ‹E‰;
C
C   APAMETP›:     MSW, RMSW - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------      ( "OTPE‡OK" MACCˆB› MSWALL -> C.M. / GETMSW );
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  T  =  TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  TN + H0;
C                  TOKN  ˆ TOKN1 - ‡HA—EHˆE EPEMEHH›X "COCTOŸHˆŸ"
C                                  MO„E‹ˆ Pˆ TN  ˆ  TN + H0;
C                          ( PA‡MEPHOCTˆ -> CM. MASPRT(46) = NNTOK )
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 6;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C                  KOD - APAMETP OPATHO‰ CBŸ‡ˆ C MOHˆTOPOM DM....
C
C
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2.          SEQCIU, DM00B9, SEQMAX;
C                              -------
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=1, 10,20,3,4 - B›„A—A COO™EHˆŸ ˆ “XO„
C                3. Pˆ FTEST = 1 -> E—ATœ HA WTRE AKTA CMEH› COCTOŸHˆŸ
C                4. C 3.10.86 --> ”ˆKCA–ˆŸ CXA+CXK „‹Ÿ SUQCIU.
C
C              ”OPMAT MSW # TˆA 50 [ BO‡“„ˆTE‹œ, H—E ]:
C              -------------------------------------------
C               "OCTOŸHHAŸ" —ACTœ
C               ******************
C
C  +-------+--------+--------+--------+------+------+--------+----+
C  I HOMEP I HOMEP  I HOMEP  I  Pˆ‡. I KO‹ˆ I KO‹. I A„P.   I PE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I ‡HA—.  I ‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI (PE‡.) I BO   I ‡HA— I "TOKN" I PB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I "TOKN1"I    I
C  +-------+--------+--------+--------+------+------+--------+----+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7    I 8  I
C  +-------+--------+--------+--------+------+------+--------+----+
C
C
C              EPEMEHHAŸ —ACTœ MSW:
C              *********************
C
C  C‹OBO     C‹OBO     OO‡HA—EHˆ ˆ ‹Oƒˆ—ECKOE ‡HA—EHˆE
C  OT        OT
C  HA—A‹A    EPEMEH.
C  MSW       —ACTˆ
C
C  9,10,11    -        KA, KB, KC, - HOMEPA “‡‹OB ”A‡ "ˆTAHˆŸ"
C  12,13,14   -        KRA< KRB, KRC - HOMEPA “‡‹OB B›XO„HOƒO
C                                      HAPŸ†EHˆŸ H—E-3”_0
C-- -- - - - - --- - - - - - - - -
C   15         1       [F] NPB - HOMEP ‹EMEHTA, OCTAB‹Ÿž™EƒO
C                                HAPŸ†EHˆE “PAB‹EHˆŸ;
C  16          2       [L] ANPB - A„PEC B MSW_ALL NPB ‹EMEHTA;
C  17          3       [L] AUYP - A„PEC B TOKN/N1 NPB ‹EMEHTA;
C  18          4       [F] TBKL - BPEMŸ BK‹. H—E;
C  19          5       [F] DTCIU - „‹ˆT. ˆM“‹œCA "Cˆ“"
C 20, 21     6, 7      [F] EYYV,  EYYI - OƒPAHˆ—EHˆŸ “ƒ‹OB.
C  22         8             "HX"  K‹ž—E‰ K.‡. “‡‹OB;
C 23<->25    9<->11  [F]     HOMEPA "OOPH›X" “‡‹OB;
C 26<->29    12<->15          PE‡EPB;
C
C 30<->39    16<->25      MSWV1  -  C‹OBO “PAB‹EHˆŸ 1-M BEHTˆ‹EM
C 40<->49    26<->35      MSWV2  -  C‹OBO “PAB‹EHˆŸ 2-M BEHTˆ‹EM
C .....................................................................
C 200<->209   186<->195   MSWV18 -  C‹OBO “PAB‹EHˆŸ 18-M BEHTˆ‹EM
C**********************************************************************
C
C   LM O™AŸ = 209 C‹OB, LM_‹OK. = 195
C
C       ”OPMAT MSWV_EHTˆ‹Ÿ „‹Ÿ DM00B9:
C       -------------------------------
C
C      C‹OBO                   ‡HA—EHˆE
C
C      1       KD - KO„ COCTOŸHˆŸ: =0 - ‡AKP›T, =1 - OTKP›T;
C      2       HX - ˜Aƒ OT TN, BO‡MO†HOE BPEMŸ EPEK‹ž—EHˆŸ;
C
C ( B [*] HOMEPA MSW_VEHTˆ‹Ÿ „‹Ÿ / "MCIU50" )
C
C  [1] 3       TS - BPEMŸ CTAPTA ˆM“‹œCA( "E" KOMM“TA–ˆŸ );
C  [2] 4       TK - BPEMŸ ”ˆHˆ˜A ˆM“‹œCA( "E" KOMM“TA–ˆŸ );
C  [3] 5       CX - KO„ PA‡PE˜EHˆŸ/‡APETA "Cˆ“":
C                   =0 - Cˆ“ ‡APE™EHO O„ABATœ HA BEHTˆ‹œ,
C                   =1 - Cˆ“ PA‡PE˜EHO O„ABATœ HA BEHTˆ‹œ;
C [4] 6        TX - BPEMŸ, C KOTOPOƒO „E‰CTB“ET KO„ "CX";
C
C
C [5] 7        MCIU - KO„ ˆM“‹œCA --->
C                     MCIU   = 00  --> "Cˆ“" HET B [TN, TN+H0];
C                                       HXCIU = 0.0;
C                     MCIU   = 01  --> B [TN, TN+H0] -> EPE„Hˆ‰ ”POHT
C                                   ˆM“‹œCA "Cˆ“", HXCIU = ˆHTEPBA‹ OT
C                                      TN „O EPE„HEƒO ”POHTA "Cˆ“";
C                     MCIU   = 10 -->  B [TN, TN+H0] -> ‡A„Hˆ‰ ”POHT
C                                   ˆM“‹œCA "Cˆ“", HXCIU = ˆHTEPBA‹
C                                      OT TN „O ‡A„HEƒO ”POHTA ˆM“‹œCA;
C                     MCIU   = 11 -->  B [TN, TN+H0] "Cˆ“" BK‹ž—EH,
C                                      COCTOŸHˆE HE MEHŸ‹, HXCIU=0.0;
C
C
C [6] 8        HXCIU - ˜Aƒ OT TN, „O O—EPE„HOƒO ”POHTA "Cˆ“";
C (---)
C     9              - ”‹Aƒ KD COCTOŸHˆŸ
C    10              - PE‡EPB
C********************************************************************
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL
       DIMENSION MSWALL(*), RMSWAL(*), EYYN(3), EYYN1(3)
       INTEGER ADRMSW
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(59), GVENT6 )
         EQUIVALENCE ( MASPRT(94), DTMCIU )
         EQUIVALENCE ( MASPRT(60), FTEST )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
         INTEGER FTEST, OPEN6
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FKOM,   MASPRT(34) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( HXG,    MASPRT(61) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
C----------------------------------------------------------------------
       DIMENSION TOKXN(2), TOKXN1(2),  THELP(18), VHN(6), VHN1(6)
       DIMENSION KVENT(18), MVENT(18), UYN(18), UYN1(18),
     =           UON(18), UON1(18),                   TNK(3), TN1K(3),
     =           TNA(3), TN1A(3)
       INTEGER AUYP, CXA, CXK
       REAL                          IKN, IKN1, IAN, IAN1
C -->  AHO„H›E “‡‹› ( HA—A‹O BEHTˆ‹Ÿ ):
       DATA KVENT
     = /12,12,12, 11,10,09, 13,13,13, 11,10,09, 14,14,14, 11,10,09/
C--> KATO„H›E “‡‹› ( KOH–› BEHTˆ‹E‰ ):
       DATA MVENT
     = /11,10,09, 12,12,12, 11,10,09, 13,13,13, 11,10,09, 14,14,14/
C
C----------------------------------------------------------------------
       KOD = 0
C--->  KOHTPO‹œ TˆA
         IF ( MSW(2) .NE. 50 ) GO TO 9999
       TBKL = RMSW(18)
       TN1 = TN + H0
C---> B›OP PE†ˆMA:
         IF ( ZAPROS .EQ. 1 )   GO TO 100
         IF ( ZAPROS .EQ. 10 )  GO TO 200
         IF ( ZAPROS .EQ. 20 )  GO TO 300
         IF ( ZAPROS .EQ. 3 )   GO TO 400
         IF ( ZAPROS .EQ. 4 )   GO TO 500
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0015 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
C
100    CONTINUE
       IF ( TN1 .GE. TBKL ) GO TO 101
C-->  MAKET K.‡. “‡‹OB KRA-KRB, KRB-KRC ====>
       KRA = MSW(12)
       KRB = MSW(13)
       KRC = MSW(14)
C-->  CPOC HX_Kž—E‰
       RMSW(22) = H0
C-->  KRA<->KRB:
       CALL GETLOG_(KRA, KRB, LOGYTR )
C-->  KRB<->KRC:
       CALL GETLOG_(KRB, KRC, LOGYTR )
       RETURN
C----------------------------------------------------------------------
101    CONTINUE
C-> ZAPROS = 1 -->  PAOTAET "H—E" -->  OC‹E„. B›‡OB MOHˆTOPA BEHˆ‹E‰
C-->  B COOTBETCTBˆˆ C MACCˆBAMˆ RMSW<50>, UYN, UYN1, UON, UON1
C
666    CONTINUE
       NEL = MSW(1)
       DO 102 NV = 1,18
C-->  A„PEC RMSW<50> BEHTˆ‹Ÿ-->
       INV = (NV-1) * 10 + 30
C-->  “‡‹› O„K‹ž—EHˆŸ-->
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
C-->  HOMEP O„. BEHTˆ‹Ÿ
       NPEL = NEL*10000+NV
C-->  MOHˆTOPHOE OC‹“†ˆBAHˆE-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
102    CONTINUE
C**********************************************************************
       RETURN
C**********************************************************************
C**********************************************************************
C**********************************************************************
200    CONTINUE
C-->  ZAPROS = 10  -->  TEHEBAŸ CˆCTEMA-->
       IF ( TN1 .GE. TBKL ) GO TO 201
C-->  „‹Ÿ K.‡. “‡‹OB--->
       KRA = MSW(12)
       KRB = MSW(13)
       KRC = MSW(14)
C---
       G = 1.0
C---
       IF ( KRA .EQ. 0 ) GO TO 290
       IF ( LOGYTR(KRA) .EQ. -1 ) KRA = 0
290    CONTINUE
       IF ( KRB .EQ. 0 ) GO TO 291
       IF ( LOGYTR(KRB) .EQ. -1 ) KRB = 0
291    CONTINUE
C-->  "BK‹A„" „‹Ÿ K‹ž—A KRA<->KRB--->
       CALL DEPOST( KRA, KRB, G, 0.0, YALL, JALL )
C
       IF ( KRC .EQ. 0 ) GO TO 292
       IF ( LOGYTR(KRC) .EQ. -1 ) KRC = 0
292    CONTINUE
       CALL DEPOST( KRB, KRC, G, 0.0, YALL, JALL )
C----------------------------------------------------------------------
       RETURN
201    CONTINUE
C-->  ZAPROS=10-> TO —TO ˆ „‹Ÿ ZAPROS=1
       GO TO 666
C######################################################################
C######################################################################
C######################################################################
300    CONTINUE
C-->  ZAPROS=20 -->  TOKˆ BEHTˆ‹E‰ ˆ K‹ž—E‰ OPE„E‹ŸEM...>
       IF ( TN1 .GE. TBKL ) GO TO 301
C-->  K‹ž—ˆ-->
       KRA = MSW(12)
       KRB = MSW(13)
       KRC = MSW(14)
C-->> KRA-KRB
       IF ( KRA .EQ. 0 ) GO TO 390
       IF ( LOGYTR(KRA) .EQ. -1 ) KRA = 0
390    CONTINUE
       IF ( KRB .EQ. 0 ) GO TO 391
       IF ( LOGYTR(KRB) .EQ. -1 ) KRB = 0
391    CONTINUE
       TA = 0.0
       IF ( KRA .NE. 0 ) TA = VALLN1(KRA)
       TB = 0.0
       IF ( KRB .NE. 0 ) TB = VALLN1(KRB)
       TOKABR = TA - TB
C-->  KRB-KRC
       IF ( KRC .EQ. 0 ) GO TO 392
       IF ( LOGYTR(KRC) .EQ. -1 ) KRC = 0
392    CONTINUE
       TC = 0.0
       IF ( KRC .NE. 0 ) TC = VALLN1(KRC)
       TOKBCR = TB - TC
C-->  ‡AˆCœ A TOKN1...
       ITOK = MSW(7)
C
       TOKN1( ITOK + 3 ) =  -TOKABR
       TOKN1( ITOK + 5 ) =  +TOKBCR
       TOKN1( ITOK + 4 ) =   TOKABR - TOKBCR
C-->  OCTA‹œH›E -> PABH› 0.0
       TOKN1( ITOK ) = 0.0
       TOKN1( ITOK + 1 ) = 0.0
       TOKN1( ITOK + 2 ) = 0.0
C----------------------------------------------------------------------
       RETURN
C**********************************************************************
301    CONTINUE
C--> ZAPROS = 20  -> TOKˆ BEHTˆ‹E‰ B TOKN/N1-->
C      ITOKNV --> (ITOK-1) + 6 + NV = ITOK+5+NV
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       ITOK = MSW(7)
       NEL = MSW(1)
       DO 302 NV = 1,18
C-->  A„PEC RMSW<50> BEHTˆ‹Ÿ-->
       INV = (NV-1) * 10 + 30
C-->  “‡‹› O„K‹ž—EHˆŸ-->
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
C-->  HOMEP O„. BEHTˆ‹Ÿ
       NPEL = NEL*10000+NV
C-->  MOHˆTOPHOE OC‹“†ˆBAHˆE-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
C-->  PACTABˆTœ OPE„. ‡HA—EHˆŸ TOKOB-->
       ITOKNV = ITOK + 5 + NV
       TOKN1(ITOKNV) = TOKXN1(1)
       THELP(NV) = TOKXN1(1)
302    CONTINUE
C-->  "ˆTOƒOB›E" TOKˆ-->
C-->  TOKˆ B›XO„H›X “‡‹OB --> KA, KB, KC-->
C--> HOMEP BEHTˆ‹Ÿ NV  O HOMEP“ ƒP“› L-->
C    NV = (L-1)*6 + J, J-> HOMEP BEHTˆ‹Ÿ B ƒP“E(B ”A‡E)
       TKA = 0.0
       TKB = 0.0
       TKC = 0.0
C-->  O ”A‡HO-->
       DO 303 L=1,3
C-->  ”A‡A "A", BEHTˆ‹ˆ J = 6 ˆ 3:
       NV6 = (L-1)*6 + 6
       NV3 = (L-1)*6 + 3
       TKA = TKA + ( THELP(NV6) - THELP(NV3) )
C-->  ”A‡A "B", BEHTˆ‹ˆ J = 5 ˆ 2:
       NV5 = (L-1)*6 + 5
       NV2 = (L-1)*6 + 2
       TKB = TKB + ( THELP(NV5) - THELP(NV2) )
C-->  ”A‡A "C", BEHTˆ‹ˆ J = 4, 1
       NV4 = (L-1)*6 + 4
       NV1 = (L-1)*6 + 1
       TKC = TKC + ( THELP(NV4) - THELP(NV1) )
303    CONTINUE
C-->  ˆX ”ˆKCˆP“EM
       TOKN1(ITOK) = TKA
       TOKN1(ITOK+1) = TKB
       TOKN1(ITOK+2) = TKC
C-->  TOKˆ B›XO„A EPOPA‡OBATE‹Ÿ —ACTOT›-->
C-->  C“MMˆPOBAHˆE O ƒP“AM..
       TK1 = THELP(1)  + THELP(2)  + THELP(3)
       TK2 = THELP(7)  + THELP(8)  + THELP(9)
       TK3 = THELP(13) + THELP(14) + THELP(15)
C--> „‹Ÿ AHO„H›X..
       TA1 = THELP(4)   + THELP(5)  + THELP(6)
       TA2 = THELP(10)  + THELP(11) + THELP(12)
       TA3 = THELP(16)  + THELP(17) + THELP(18)
C-->  C“MAPH›E B›X. TOKˆ-->
       TOKN1( ITOK + 3 ) = TA1 - TK1
       TOKN1( ITOK + 4 ) = TA2 - TK2
       TOKN1( ITOK + 5 ) = TA3 - TK3
C-->  TOKˆ “†E, A "Cˆ“" E™E HE OPE„E‹EH›....
       RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
400    CONTINUE
C-->  ZAPROS=3  ->  AHA‹ˆ‡ BO‡MO†HOƒO COCTOŸHˆŸ/EPEK‹ž—EHˆŸ
       IF ( TN1 .GE. TBKL ) GO TO  401
C-->  K‹ž—ˆ B›K‹ž—AžTCŸ TO‹œKO Pˆ:
       IF ( TN .LT. TBKL  .AND.  TBKL .LE. TN1 ) GO TO 402
C-->  ˆHA—E Hˆ —EƒO HE MEHŸETCŸ
       RETURN
402    CONTINUE
C-->  ˜Aƒ „O BO‡MO†HO‰ KOMM“TA–ˆˆ
       HX = TBKL - TN
C-->  EƒO ”ˆKCˆP“EM
       IF ( HX .LT. HXMIN ) HX = HXMIN
       RMSW(22) =  HX
       HXG = AMIN1( HX, HXG )
       TXG = TN + HXG
       FKOM = 1
       RETURN
C----------------------------------------------------------------------
401    CONTINUE
C-->  ZAPROS=3 -  TOKˆ OPE„E‹EH›, HA‰„EM AHO„H›E ˆ KATO„H›E TOKˆ B
C                 ”A‡AX BEHTˆ‹E‰ „‹Ÿ B›ŸCHEHˆŸ CX, TX
C
       ITOK1 = MSW(7) - 1 + 6
C**********************************************************************
       TNK(1)  = TOKN (ITOK1+01) + TOKN (ITOK1+02) + TOKN (ITOK1+03)
       TN1K(1) = TOKN1(ITOK1+01) + TOKN1(ITOK1+02) + TOKN1(ITOK1+03)
       TNK(2)  = TOKN (ITOK1+07) + TOKN (ITOK1+08) + TOKN (ITOK1+09)
       TN1K(2) = TOKN1(ITOK1+07) + TOKN1(ITOK1+08) + TOKN1(ITOK1+09)
       TNK(3)  = TOKN (ITOK1+13) + TOKN (ITOK1+14) + TOKN (ITOK1+15)
       TN1K(3) = TOKN1(ITOK1+13) + TOKN1(ITOK1+14) + TOKN1(ITOK1+15)
C--
       TNA(1)  = TOKN (ITOK1+04) + TOKN (ITOK1+05) + TOKN (ITOK1+06)
       TN1A(1) = TOKN1(ITOK1+04) + TOKN1(ITOK1+05) + TOKN1(ITOK1+06)
       TNA(2)  = TOKN (ITOK1+10) + TOKN (ITOK1+11) + TOKN (ITOK1+12)
       TN1A(2) = TOKN1(ITOK1+10) + TOKN1(ITOK1+11) + TOKN1(ITOK1+12)
       TNA(3)  = TOKN (ITOK1+16) + TOKN (ITOK1+17) + TOKN (ITOK1+18)
       TN1A(3) = TOKN1(ITOK1+16) + TOKN1(ITOK1+17) + TOKN1(ITOK1+18)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C-->  ˆCXO„H›E HAPŸ†EHˆŸ “PAB‹EHˆŸ...
C  B TOKN/N1  HAXO„ŸTCŸ O A„PEC“ AUYP = MSW(17)
C
       AUYP = MSW(17)
C--->
       EYYN (1) = TOKN (AUYP     )
       EYYN1(1) = TOKN1(AUYP     )
       EYYN (2) = TOKN (AUYP + 1 )
       EYYN1(2) = TOKN1(AUYP + 1 )
       EYYN (3) = TOKN (AUYP + 2 )
       EYYN1(3) = TOKN1(AUYP + 2 )
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  OƒPAHˆ—EHˆŸ COƒ‹ACHO "PE†ˆMA".......
       EYYV = RMSW(20)
       EYYI = RMSW(21)
       CALL SEQMAX( EYYN (1), TN K(1),  TN A(1),  EYYV,  EYYI )
       CALL SEQMAX( EYYN (2), TN K(2),  TN A(2),  EYYV,  EYYI )
       CALL SEQMAX( EYYN (3), TN K(3),  TN A(3),  EYYV,  EYYI )
C-->  „‹Ÿ TN1
       CALL SEQMAX( EYYN1(1), TN1K(1),  TN1A(1),  EYYV,  EYYI )
       CALL SEQMAX( EYYN1(2), TN1K(2),  TN1A(2),  EYYV,  EYYI )
       CALL SEQMAX( EYYN1(3), TN1K(3),  TN1A(3),  EYYV,  EYYI )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C-->  UYN/N1  - HAP. “PAB‹EHˆŸ BEHTˆ‹Ÿ
C-->  UON/N1  - OOPH›E HAP.
C     EYYN/N1 - HAP. “PAB‹EHˆŸ PEOPA‡OBATE‹Ÿ;
C  VHN / N1 - "ˆCTˆHH›E" HAPŸ†EHˆŸ HA “‡‹AX OOPHO‰‰ CˆCTEM›
C              KXA, KXB, KXC             B TN/TN+H0
C
       DO 403 I=1,3
C-->  HOMEP “‡‹A:
       K = MSW(I+22)
       VKN = 0.0
       VKN1 = 0.0
       IF ( K .EQ. 0 ) GO TO 404
       VKN  =  VALLN( K )
       VKN1 = VALLN1( K )
404    CONTINUE
       VHN(I)   = VKN
       VHN1(I)  = VKN1
403    CONTINUE
C######################################################################
C    PACPE„E‹EHˆE HAPŸ†EHˆ‰ “PAB‹EHˆŸ O ”A‡AM ˆ AHO„HO-KATO„H›M
C                      ƒ P “   A M:
C
C                      HAPŸ†EHˆE “PAB‹EHˆŸ(ˆ‡ MSW_PEƒ“‹ŸTOPA):
C                      -----------------------------------------
C
C     KATO„HAŸ ƒP“A:                         AHO„HAŸ ƒP“A:
C
C   BEHT. 01, 02, 03 -->"-"EYYA         BEHT. 04, 05, 06 --> "+"EYYA
C   BEHT. 07, 08, 09 -->"-"EYYB         BEHT. 10, 11, 12 --> "+"EYYB
C   BEHT. 13, 14, 15 -->"-"EYYC         BEHT. 16, 17, 18 --> "+"EYYC
C
C  ƒ„E EYYA, EYYB, EYYC - CˆƒHA‹› ˆ‡ TOKN/N1 ‹EMEHTA "#‹" “PAB‹EHˆŸ,
C                         EPB›E TPˆ ‡HA—EHˆŸ; ( ”A‡A A, B, C );
C
C
C        PACPE„E‹EHˆE OOPH›X HAPŸ†EHˆ‰:
C        ---------------------------------
C BEHTˆ‹ˆ ”A‡› "A":       BEHTˆ‹ˆ ”A‡› "B":       BEHTˆ‹ˆ ”A‡› "C":
C  !!! KATO„H›E !!!        !!! KATO„H›E !!!        !!! KATO„H›E !!!
C -----------------       ----------------        -----------------
C 03     --> "+"V(KXB)    02     --> "+"V(KXC)    01     --> "+"V(KXA)
C 09     --> "+"V(KXB)    08     --> "+"V(KXC)    07     --> "+"V(KXA)
C 15     --> "+"V(KXB)    14     --> "+"V(KXC)    13     --> "+"V(KXA)
C   "+" ”A‡A "B"              "+" ”A‡A "C"               "+" ”A‡A "A"
C ( “‡E‹ KXB )               ( “‡E‹ KXC )            ( “‡E‹ KXA )
C T.E.-120 ƒP„. OT "A"    T.E.-120 ƒP„. OT "B"    T.E.-120 ƒP„. OT "C"
C
C BEHTˆ‹ˆ ”A‡› "A":       BEHTˆ‹ˆ ”A‡› "B":       BEHTˆ‹ˆ ”A‡› "C":
C !!! AHO„H›E !!!          !!! AHO„H›E !!!        !!! AHO„H›E !!!
C -----------------       ----------------        -----------------
C     06 --> "-"V(KXB)        05 --> "-"V(KXC)        04 --> "-"V(KXA)
C     12 --> "-"V(KXB)        11 --> "-"V(KXC)        10 --> "-"V(KXA)
C     18 --> "-"V(KXB)        17 --> "-"V(KXC)        16 --> "-"V(KXA)
C   "-" ”A‡A "B"              "-" ”A‡A "C"               "-" ”A‡A "A"
C ( “‡E‹ KXB )               ( “‡E‹ KXC )            ( “‡E‹ KXA )
C T.E. +60 ƒP„. OT "A"    T.E. +60 ƒP„. OT "B"    T.E. +60 ƒP„. OT "C"
C
C**********************************************************************
C-->  HAPŸ†EHˆŸ “PAB‹EHˆŸ*******
C******* TN BPEMŸ *****************************************************
C   K A T O „ H A Ÿ   ƒ P “   A:
       UYN(01) = -EYYN(1)
       UYN(02) = -EYYN(1)
       UYN(03) = -EYYN(1)
       UYN(07) = -EYYN(2)
       UYN(08) = -EYYN(2)
       UYN(09) = -EYYN(2)
       UYN(13) = -EYYN(3)
       UYN(14) = -EYYN(3)
       UYN(15) = -EYYN(3)
C   A H O „ H A Ÿ   ƒ P “   A:
       UYN(04) = +EYYN(1)
       UYN(05) = +EYYN(1)
       UYN(06) = +EYYN(1)
       UYN(10) = +EYYN(2)
       UYN(11) = +EYYN(2)
       UYN(12) = +EYYN(2)
       UYN(16) = +EYYN(3)
       UYN(17) = +EYYN(3)
       UYN(18) = +EYYN(3)
C----------------------------------------------------------------------
C******* TN + H0 BPEMŸ *************************************************
C   K A T O „ H A Ÿ   ƒ P “   A:
       UYN1(01) = -EYYN1(1)
       UYN1(02) = -EYYN1(1)
       UYN1(03) = -EYYN1(1)
       UYN1(07) = -EYYN1(2)
       UYN1(08) = -EYYN1(2)
       UYN1(09) = -EYYN1(2)
       UYN1(13) = -EYYN1(3)
       UYN1(14) = -EYYN1(3)
       UYN1(15) = -EYYN1(3)
C   A H O „ H A Ÿ   ƒ P “   A:
       UYN1(04) = +EYYN1(1)
       UYN1(05) = +EYYN1(1)
       UYN1(06) = +EYYN1(1)
       UYN1(10) = +EYYN1(2)
       UYN1(11) = +EYYN1(2)
       UYN1(12) = +EYYN1(2)
       UYN1(16) = +EYYN1(3)
       UYN1(17) = +EYYN1(3)
       UYN1(18) = +EYYN1(3)
C----------------------------------------------------------------------
C***** O  O P H › E   H A  P Ÿ † E H ˆ Ÿ:
C********TN BPEMŸ: ****************************************************
C-->  ”A‡A   "A"-> [ 3,9,15 - KATO„ "+ B" ], [6,12,18 - AHO„ "- B" ]
       UON(03) = +VHN(2)
       UON(06) = -VHN(2)
       UON(09) = +VHN(2)
       UON(12) = -VHN(2)
       UON(15) = +VHN(2)
       UON(18) = -VHN(2)
C--> ”A‡A "B"-> [ 2,8,14 KATO„ "+ C" ], [ 5,11,17 - AHO„ "-C" ]
       UON(02) = +VHN(3)
       UON(05) = -VHN(3)
       UON(08) = +VHN(3)
       UON(11) = -VHN(3)
       UON(14) = +VHN(3)
       UON(17) = -VHN(3)
C-->  ”A‡A "C"-> [ 1,7,13 - KATO„ "+ A" ], [ 4,10,16 - AHO„ "-A" ]
       UON(01) = +VHN(1)
       UON(04) = -VHN(1)
       UON(07) = +VHN(1)
       UON(10) = -VHN(1)
       UON(13) = +VHN(1)
       UON(16) = -VHN(1)
C----------------------------------------------------------------------
C********TN+H0  BPEMŸ: *************************************************
C-->  ”A‡A   "A":
       UON1(03) = +VHN1(2)
       UON1(06) = -VHN1(2)
       UON1(09) = +VHN1(2)
       UON1(12) = -VHN1(2)
       UON1(15) = +VHN1(2)
       UON1(18) = -VHN1(2)
C--> ”A‡A "B")
       UON1(02) = +VHN1(3)
       UON1(05) = -VHN1(3)
       UON1(08) = +VHN1(3)
       UON1(11) = -VHN1(3)
       UON1(14) = +VHN1(3)
       UON1(17) = -VHN1(3)
C-->  ”A‡A "C"
       UON1(01) = +VHN1(1)
       UON1(04) = -VHN1(1)
       UON1(07) = +VHN1(1)
       UON1(10) = -VHN1(1)
       UON1(13) = +VHN1(1)
       UON1(16) = -VHN1(1)
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --->  „‹Ÿ KA†„O‰ ƒP“› CBOE CX < AHO„HOE>, CX <KATO„HOE> ˆ TX:
C-->
       DO 405 LG=1,3
C-->  LG -> HOMEP ƒP“›, J-> HOMEP B ƒP“E
C  NV = (L-3)*6 + J  -->"ACO‹žTH›‰" A„PEC...
       IKN  = TNK (LG)
       IKN1 = TN1K(LG)
       IAN  = TNA (LG)
       IAN1 = TN1A(LG)
C----------------------------------------------------------------------
C  „‹Ÿ "LG" KATO„-AHO„ ƒP“›:
C----------------------------------------------------------------------
C   CTAP›E CXA, CXK...
C  1 KATO„,  4 - AHO„..
       JK = 1
       JA = 4
       NVK = ( LG-1)*6 + JK
       NVA = ( LG-1)*6 + JA
       INVK = (NVK-1)*10+30
       INVA = (NVA-1)*10+30
       CXK = MSW(INVK + 4)
       CXA = MSW(INVA + 4)
C----------------------------------------------------------------------
       CALL SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
     =  IFXK,  IFXA,         EYYN(LG), EYYN1(LG)  )
C----------------------------------------------------------------------
440    CONTINUE
C-->  CMEHA CX ˆ TX B MSW<50> (1,2,3 -> KATO„, 4,5,6 -> AHO„ ):
       DO 441 J = 1,3
       NV = (LG-1)*6 + J
C-->  A„P. HA—A‹A MSW<50> KATO„H›X BEHTˆ‹E‰ B ƒP“E
       INV = ( NV-1) * 10 + 30
       MSW(INV+4) = CXK
       IF ( IFXK  .EQ. 1 )  RMSW(INV+5) = TXK
441    CONTINUE
C
       DO 442 J=4,6
       NV = (LG-1)*6 + J
C-->  A„P. HA—A‹A MSW<50>      AHO„H›X BEHTˆ‹E‰ B ƒP“E
       INV = ( NV-1) * 10 + 30
       MSW(INV+4) =  CXA
       IF ( IFXA  .EQ. 1 )  RMSW(INV+5) = TXA
442    CONTINUE
C----------------------------------------------------------------------
450    CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
405    CONTINUE
C######################################################################
C######################################################################
C-->  B›O‹HˆM ZAPROS=3  „‹Ÿ BEHT‹E‰***********************************
       NEL = MSW(1)
       ITOK = MSW(7)
       DTCIU = RMSW(19)
       DO 451 NV = 1,18
       INV = (NV-1) * 10 + 30
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
       NPEL = NEL*10000 + NV
C-->  PACTABˆM TOKXN/N1
       ITOKNV = ITOK+5 + NV
       TOKXN(1) =  TOKN ( ITOKNV )
       TOKXN1(1) = TOKN1( ITOKNV )
C-->  BCE OPE„E‹EHO...
C-->  MOHˆTOPHOE OC‹“†ˆBAHˆE-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
C-->  PˆMEM CˆƒHA‹› "Cˆ“"-->
       ICIUNV = ITOK + 5 + 18 + NV
       TOKN1(ICIUNV) = TOKXN1(2)
451    CONTINUE
C-->  ‡APOC“ ...–
       RETURN
C----------------------------------------------------------------------
C######################################################################
500    CONTINUE
C-->  ZAPROS=4, „‹Ÿ K‹ž—E‰ ‡A COCTOŸHˆEM C‹E„ˆTœ HE H“†HO
       IF ( TN1 .LT. TBKL ) RETURN
C-->  „‹Ÿ BEHTˆ‹E‰  -->  AHA‹Oƒˆ—HO ZAPROS=1-->
       GO TO 666
C$$$$$$DEBUG           INIT
       END
