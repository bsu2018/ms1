       SUBROUTINE FM0015( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: FM0015( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA‡HA—EHˆE:        HACTPO‰KA MAKPOMO„E‹ˆ # TˆA 92:
C  ------------  BEHTˆ‹œH›‰ K‹ž— ( „BA BEHTˆ‹Ÿ 1- OT "K" K "M" );
C                ************************************************
C                POˆ‡HAK TˆA #6
C
C                   +----I<I-----+   *EPB›‰ BEHTˆ‹œ_KATO„HAŸ ƒP“A*
C                   I            I
C  *K_“‡E‹*---------+            +------*M_“‡E‹*
C                   I            I
C                   +----I>I-----+   *BTOPO‰ BEHTˆ‹œ_AHO„HAŸ ƒP“A*
C
C
C     HAPŸ†EHˆE OOPHOE „‹Ÿ 1-ƒO BEHTˆ‹EŸ -->   V(“O1)
C     HAPŸ†EHˆE OOPHOE „‹Ÿ 2-ƒO BEHTˆ‹EŸ -->   V(“O2)
C     HAPŸ†EHˆE “PAB‹EHˆŸ „‹Ÿ 1-ƒO BEHTˆ‹Ÿ --> V(““1) - V(““2)
C     HAPŸ†EHˆE “PAB‹EHˆŸ „‹Ÿ 2-ƒO BEHTˆ‹Ÿ --> V(““2) - V(““1)
C     ( T.E. TO —TO „‹Ÿ BEHTˆ‹Ÿ 1, HO C OPATH›M ‡HAKOM );
C
C
C       PEA‹ˆ‡“ETCŸ "PA‡„E‹œHOE" “PAB‹EHˆE BEHTˆ‹ŸMˆ:
C                    Pˆ OƒACAHˆˆ O„HOƒO -> ‡APET O„A—ˆ HA HEƒO
C                                             ˆM“‹œCOB "Cˆ“" ˆ
C                                             PA‡PE˜EHˆE O„A—ˆ "Cˆ“"
C                                             HA „P“ƒO‰;
C                    Pˆ OƒACAHˆˆ OOˆX -> PA‡PE˜EHˆE "Cˆ“" HA TOT,
C                                           „‹Ÿ KOTOPOƒO HAPŸ†EHˆE
C                                           “PAB‹EHˆŸ O‹O†ˆTE‹œHOE;
C
C
C              ‹EMEHT OˆC›BAETCŸ:
C
C    # T 92 “ K  M    “O1  “O2   ““1  ““2  DTCIU  EYYV  EYYI;
C
C     ƒ„E:     K, M  -  “‡‹› O„K‹ž—EHˆŸ,
C
C              “O1, “O2  -  “‡‹› "OOPHOƒO" HAPŸ†EHˆŸ-> V(“01),V(“O2)
C
C              ““1, ““2  -  “‡‹› "“PAB‹Ÿž™EƒO" HA-Ÿ ->[V(““1)-V(““2)]
C
C              DTCIU - „‹ˆTE‹œHOCTœ ˆM“‹œCA "Cˆ“" -> [ B ‹. ƒPA„“CAX ]
C                      ˆM“‹œC› ƒEHEPˆP“žTCŸ COƒ‹ACHO CPABHEHˆž
C                      HAP. OOPHOƒO -> B›ˆPAEMOƒO ˆ‡ MACCˆBA
C                      VALLN/VALLN1 ˆ  HAPŸ†EHˆŸ "“PAB‹Ÿž™EƒO" ->
C                      B›ˆPAEMOƒO ˆ‡ VALLN/VALLN1;
C
C  EYYV - OƒPAHˆ—EHˆE "CBEPX“" “P. CˆƒHA‹A;
C  EYYI - OƒPAHˆ—EHˆE "CHˆ‡“"  “P. CˆƒHA‹A;
C
C
C === B›BO„H›E ‡HA—EHˆŸ: I1         - TOK  BHE˜. B›BO„OB, OT "K" K "M";
C                        I2, I3     - TOKˆ BEHTˆ‹E‰,
C                        I3, I4     - "Cˆ“" HA BEHTˆ‹ŸX;
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
C                        BCEX “‡‹OB          B „ˆAA‡OH OT 0 „O
C                        MAKCˆMA‹œHO ‡A„AHHOƒO ( NMKNOT )
C                   !!!               ˆ H A — E  --->  STOP 20
C                                                      -------
C              ”OPMAT MSW # TˆA 92 [ BEHTˆ‹œH›‰ K‹ž— ]:
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
C  9          -        HOMEP “‡‹A : 1 - BEHTˆ‹œ --I>I--, 2 -‰ --I<I--
C  10         -        HOMEP “‡‹A : 1 - BEHTˆ‹œ --I<I--, 2 -‰ --I>I--
C
C-- -- - - - - --- - - - - - - - -
C  11, 12     1, 2     [F] HOMEPA “‡‹OB “O1, “O2;
C
C  13, 14     3, 4     [F] HOMEPA “‡‹OB ““1, ““2;
C
C  15          5       [F] DTCIU - „‹ˆT. ˆM“‹œCA "Cˆ“"
C
C 16<->25     6<->15      MSWV1  -  C‹OBO “PAB‹EHˆŸ 1-M BEHTˆ‹EM
C      $$                 ( B 25 - XPAHˆTœCŸ EYYV )
C 26<->35    16<->25      MSWV2  -  C‹OBO “PAB‹EHˆŸ 2-M BEHTˆ‹EM
C      $$                 ( B 35 - XPAHˆTCŸ EYYI )
C**********************************************************************
C
C   LM O™AŸ = 35  C‹OB, LM_‹OK. =  25
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
C    10              - „‹Ÿ 1-ƒO BEHTˆ‹Ÿ -> EYYV, „‹Ÿ 2-ƒO --> EYYI;
C
C********************************************************************
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
       DATA ID1 / 1 /
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 92 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0015 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
C-->
C  PAR0T -> “O1,  “O2,  ““1,  ““2, DTCIU  EYYV  EYYI;
C            1     2     3    4    5        6    7
C######################################################################
1      CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 25
C      *********
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C
       DTCIU = PAR0T(5) / 1.8E+4
       RMSW(5) = DTCIU
C
C-->  PACTABˆTœ B MSW_BEHTˆ‹E‰ CX=1 - PA‡PE˜EHˆE     !! ‹OKˆPOBAHO !!
C
C      DO 3 NV = 1,2
C      ICX = (NV-1)*10 +  6 + 4
C      RMSW(ICX) = D1
C3      CONTINUE
C----------------------------------------------------------------------
C-->  "OOPH›E"  ˆ "“P" “‡‹›
       DO 4 I = 1, 4
       K = IFIX( PAR0T(I) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(I) = RK
4      CONTINUE
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       RMSW(15) =  PAR0T(6)
       RMSW(25) =  PAR0T(7)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       RETURN
C-
5      CONTINUE
       WRITE(NWTR,  502 ) NEL, K
       WRITE(NWTRE, 502 ) NEL, K
502    FORMAT(/' *** B í«¥¬¥­â¥ # ', I6, ' ­¥ ¤®¯ãáâ¨¬ë© ',
     = 'ã§¥« # ', I5, ' ***'/
     = ' ***  à áç¥â ¯à¥ªà é¥­   [ FM0015 ] ***'//)
       call STOPP(28)
       END
