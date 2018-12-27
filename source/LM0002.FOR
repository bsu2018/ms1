       SUBROUTINE LM0002( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: LM0002( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹ˆ TˆA # 50
C   ----------     *** BO‡“„ˆTE‹œ H—E 3”-0 („‹Ÿ ‹. MA˜. 3X3X0) ****
C
C                  ‹EMEHT MO†ET ›Tœ ‹Oƒˆ—ECKˆ CBŸ‡AH C MAKPOMO„E‹ŸMˆ:
C                       Tˆ # 40,  # 63 ( 3-X ”A‡HAŸ .„.C.),
C                           # 13 ( "ˆ‡MEPˆTE‹œ HAPŸ†EHˆŸ" );
C
C ===                  TPˆ EPB›E ‡HA—EHˆŸ ˆ‡ TOKN/TOKN1 - PˆHˆMAžTCŸ
C                      ‡A ‡HA—EHˆŸ HAPŸ†EHˆ‰ “PAB‹EHˆŸ;
C ===
C
C   METO„:        OˆCK B MSW_ALL OˆCAHˆˆ ‹EMEHTA *‹. MA˜.*
C   -----         ( „O. Tˆ COƒ‹ACHO BCTPOEHHO‰ TA. "LTIP" )
C                 B C‹“—AE HAXO†„EHˆŸ -> „O”OPMˆPOBAHˆE MSW_OˆCAHˆŸ;
C                 B TOM C‹“—AE KOD = 0, ˆHA—E = 1;
C   APAAMETP›:   MSW(RMSW) - "OTPE‡OK" MSW_MO„E‹ˆ Tˆ # 50;
C   ----------    ADR - A„PEC B MSW_ALL MSW_OˆCAHˆŸ TO‰ MO„E‹ˆ;
C                 MSWALL(RMSWAL) - BECœ CˆCOK MSW C‹OB;
C                 KOD - = 0 - C”OPMˆPOBA‹ˆ MSW,
C                       = 1 - ‹Oƒ. O˜ˆKˆ;
C                      ( HA NWTR COO™EHˆE B›BO„ˆTCŸ );
C   O„POƒPAMM›: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C   PˆME—AHˆE:  ”OPMAT› MSW_OˆCAHˆ‰ ---->
C   ----------
C
C                      MAKPOMO„E‹œ # TˆA 50:
C                BO‡“„ˆTE‹œ H—E-3”_0 ( „‹Ÿ ‹. MA˜. 3X3X0 )
C                ********************************************
C                POˆ‡HAK TˆA #6, BEHTˆ‹œH›‰ PEOPA‡. —ACTOT›;
C
C              ‹EMEHT OˆC›BAETCŸ:
C
C    3 T 50 “ KA KB KC KRA KRB KRC
C    T_BK‹  DTCIU #‹_TA_PEƒ“‹ŸTOPA_BO‡“†„EHˆŸ<ƒEHEPATOP
C                                                “PAB‹Ÿž™EƒO
C                                                HAPŸ†EHˆŸ
C
C            KXA  KXB  KXC;
C     ƒ„E:
C
C              KA, KB, KC  - “‡‹› ˆTAž™EƒO 3-X ”A‡HOƒO HAPŸ†EHˆŸ;
C              KRA, KRB, KRC - “‡‹› POTOPHO‰ OMOTKˆ ‹. MA˜ˆH› ->
C                              B›XO„HOE HAPŸ†EHˆE PEOPA‡OBATE‹Ÿ
C                              —ACTOT›;
C              DTCIU - „‹ˆTE‹œHOCTœ ˆM“‹œCA "Cˆ“" ->
C                      ˆM“‹œC› ƒEHEPˆP“žTCŸ COƒ‹ACHO CPABHEHˆž
C                      HAP. OOPHOƒO -> B›ˆPAEMOƒO ˆ‡ MACCˆBA
C                      TOKN/TOKN1 ‹EMEHTA #_‹._PEƒ._BO‡“†.
C                      ( TPˆ EPB›E C‹OBA ) ˆ HAPŸ†EHˆŸ ˆTAHˆŸ
C                      PEOPA‡OBATE‹Ÿ, T.E. HA “‡‹AX KA, KB, KC;
C                      ( CM. DM0015 )
C                      KXA, KXB, KXC - “‡‹› "OOPHO‰" CˆCTEM›
C                                      HAPŸ†EHˆ‰ ”A‡A: "A", "B", "C";
C
C              #_‹.BO‡. - MO„E‹œ OPE„E‹EHˆŸ HAPŸ†EHˆŸ “PAB‹EHˆŸ:
C                         ( Tˆ #40, Tˆ # 63, Tˆ # 13 );
C                         ###############################
C
C === B›BO„H›E ‡HA—EHˆŸ: I1, I2, I3, I4, I5, I6, - TOKˆ BHE˜. B›BO„OB,
C                        IV1, IV2, ..., IV18,    - TOKˆ BEHTˆ‹E‰,
C                        CIU1, CIU2, ..., CIU18 - "Cˆ“" HA BEHTˆ‹E;
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
C 20, 21     6, 7           PE‡EPB;
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
C     9              - PE‡EPB
C    10              - PE‡EPB
C********************************************************************
C
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 40, 63, 13, 7*0 /
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ‹-TA C U. “P
       NELMOT = MSW(15)
C-->
       NEL = MSW(1)
       TIP = MSW(2)
C-->
C-->  ˆCKATœ B MSWALL ‹EMEHT C #‹-TA = NELMOT:
       CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, KVIV, ATOKM )
C-->
       IF   ( IRC .EQ. 1 ) GO TO 1
C-->  ‹EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 100) NEL, TIP, NELMOT
100    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     =  ' ( Tˆ # ', I3, ' ) ***'/
     =  ' *** C HE C“™ECTB“ž™ˆM B –Eˆ ‹EMEHTOM # ', I5,' ***'/
     =  ' *** OPAOTKA –Eˆ PEKPA™AETCŸ *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA˜‹ˆ, —TO ‡A Tˆ TAM †ˆBET ?
       TIPX = MSWALL(AMOT + 1 )
       DO 2 I = 1, K10
       IF ( TIPX .EQ. LTAB(I) ) GO TO 3
2      CONTINUE
C-->  CBŸ‡œ C HE HA˜ˆM TˆOM.....
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELMOT, TIPX
101    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     = ' ( Tˆ # ', I3, ' ) ***'//
     = ' *** C ‹EMEHTOM # ', I5, ' ( Tˆ # ', I3, ' ) - O˜ˆO—EH',
     = ' ˆ‹ˆ HE PEA‹ˆ‡OBAH ***'/
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
3      CONTINUE
C-->  AMOT - A„P MSW ‹EMEHTA, C KOTOP›M B CBŸ‡ˆ..
       MSW(16) = AMOT
C-->    ATOKM -->  A„PEC B›B. ‡HA—. TOƒO ‹EMEHTA
       MSW(17) = ATOKM
       KOD = 0
       RETURN
       END
