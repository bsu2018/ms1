       SUBROUTINE FM0011( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: FM0011( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA‡HA—EHˆE:        HACTPO‰KA MAKPOMO„E‹ˆ # TˆA 50:
C  ------------  BO‡“„ˆTE‹œ H—E-3”_0 ( „‹Ÿ ‹. MA˜. 3X3X0 )
C                ********************************************
C                POˆ‡HAK TˆA #6, BEHTˆ‹œH›‰ PEOPA‡. —ACTOT›;
C
C              ‹EMEHT OˆC›BAETCŸ:
C
C    3 T 50 “ KA KB KC KRA KRB KRC
C    T_BK‹  DTCIU #‹_TA_PEƒ“‹ŸTOPA_BO‡“†„EHˆŸ<ƒEHEPATOP
C                                                “PAB‹Ÿž™EƒO
C                                                HAPŸ†EHˆŸ>
C            KXA  KXB  KXC  EYYV  EYYI;
C     ƒ„E:
C
C              KA, KB, KC  - “‡‹› ˆTAž™EƒO 3-X ”A‡HOƒO HAPŸ†EHˆŸ;
C              KRA, KRB, KRC - “‡‹› POTOPHO‰ OMOTKˆ ‹. MA˜ˆH› ->
C                              B›XO„HOE HAPŸ†EHˆE PEOPA‡OBATE‹Ÿ
C                              —ACTOT›;
C              DTCIU - „‹ˆTE‹œHOCTœ ˆM“‹œCA "Cˆ“" -> [ B ‹. ƒPA„“CAX ]
C                      ˆM“‹œC› ƒEHEPˆP“žTCŸ COƒ‹ACHO CPABHEHˆž
C                      HAP. OOPHOƒO -> B›ˆPAEMOƒO ˆ‡ MACCˆBA
C                      TOKN/TOKN1 ‹EMEHTA #_‹._PEƒ._BO‡“†.
C                      ( TPˆ EPB›E C‹OBA ) ˆ HAPŸ†EHˆŸ ˆTAHˆŸ
C                      PEOPA‡OBATE‹Ÿ, T.E. HA “‡‹AX KA, KB, KC;
C                      ( CM. DM0015 )
C              #_‹.BO‡. - MO„E‹œ OPE„E‹EHˆŸ HAPŸ†EHˆŸ “PAB‹EHˆŸ:
C                         ( Tˆ #40, Tˆ # 63, Tˆ # 13 );
C                         ###############################
C                      KXA, KXB, KXC - “‡‹› "OOPHO‰" CˆCTEM›
C                                      HAPŸ†EHˆ‰ ”A‡A: "A", "B", "C";
C         EYYV - OƒPAHˆ—EHˆE “P. CˆƒHA‹A Pˆ B›PŸMˆTE‹œHOM PE†ˆME,
C         EYYI - OƒPAHˆ—EHˆE “P. CˆƒHA‹A Pˆ ˆHBEPCHOM      PE†ˆME;
C
C === B›BO„H›E ‡HA—EHˆŸ: I1, I2, I3, I4, I5, I6, - TOKˆ BHE˜. B›BO„OB,
C                        IV1, IV2, ..., IV18,    - TOKˆ BEHTˆ‹E‰,
C                        CIU1, CIU2, ..., CIU18 - "Cˆ“" HA BEHTˆ‹E;
C
C
C   METO„:       PA‡METKA O‹E‰, ‡A„AHˆE CX_BEHTˆ‹Ÿ = 0( ‡APET    );
C   -----
C   APAMETP›:    BA0T, KNOTT, PAR0T - "OTPE‡KA" MACCˆBOB ˆCX. ˆH”.
C   ---------
C                RMSW - "EPEMEHAŸ" —ACTœ MSW_‹EMEHTA,
C                LM - EE „‹ˆHA B C‹OBAX;
C   O„POƒPAMM›:   HET
C   ------------
C   PˆME—AHˆE:  1. KOHTPO‹œ HE PEA‹ˆ‡OBAH;
C  -----------      !!!  KOHTPO‹ˆP“ETCŸ TO‹œKO "OA„AHˆE" HOMEPOB
C                        “‡‹OB KXA, KXB, KXC B „ˆAA‡OH OT 0 „O
C                        MAKCˆMA‹œHO ‡A„AHHOƒO ( NMKNOT )
C                   !!!               ˆ H A — E  --->  STOP 20
C                                                      -------
C                2. „‹Ÿ OKOH—ATE‹œHO‰ HACTPO‰Kˆ ->
C                    C‹“†ˆT LM_MOHˆTOP "LM0002";
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
C 20, 21     6, 7      [F] EYYV,  EYYI;
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
C     9              - ”‹Aƒ KD COCTOŸHˆŸ;
C    10              - PE‡EPB
C********************************************************************
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( NMKNOT, MASPRT(7) )
       EQUIVALENCE( RK,  K )
       EQUIVALENCE ( NPB, RNPB   )
       EQUIVALENCE ( ID1, D1        )
       INTEGER NPB, TIP, BA0T
C
       DATA ID1 / 1 /
C
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 50 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0011 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
C-->
C  PAR0T -> TBKL, TCIU, NPB, KXA, KXB, KXC, EYYV, EYYI
C            1     2     3    4    5    6    7     8
C######################################################################
1      CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 195
C      *********
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C
       TBKL = PAR0T(1)
       RMSW(4) = TBKL
C
       NPB = IFIX( PAR0T(3) )
       RMSW(1) = RNPB
C
       DTCIU = PAR0T(2) / 1.8E+4
       RMSW(5) = DTCIU
C
       RMSW(6) = PAR0T(7)
       RMSW(7) = PAR0T(8)
C
C-->  PACTABˆTœ B MSW_BEHTˆ‹E‰ CX=1 - PA‡PE˜EHˆE  !! ‹OKˆPOBAHO !!
C
C      DO 3 NV = 1,18
C      ICX = (NV-1)*10 + 16 + 4
C      RMSW(ICX) = D1
C3      CONTINUE
C----------------------------------------------------------------------
C-->  "OOPH›E" “‡‹›
       K = IFIX( PAR0T(4) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(9) = RK
       K = IFIX( PAR0T(5) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(10) = RK
       K = IFIX( PAR0T(6) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(11) = RK
C-
       RETURN
C-
5      CONTINUE
       WRITE(NWTR,  502 ) NEL, K
       WRITE(NWTRE, 502 ) NEL, K
502    FORMAT(/' *** B ‹EMEHTE # ', I6, ' HE „O“CTˆM›‰ ',
     = '“‡E‹ # ', I5, ' ***'/
     = ' ***  PAC—ET PEKPA™EH   [ FM0011 ] ***'//)
       STOP 20
       END
