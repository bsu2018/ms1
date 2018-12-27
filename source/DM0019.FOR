         SUBROUTINE DM0019 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0019 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:  MOHˆTOP         MAKPOMO„E‹ˆ # TˆA 92:
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
C     HAPŸ†EHˆE OOPHOE „‹Ÿ 1-ƒO BEHTˆ‹ˆ‹Ÿ -->   V(“O1);
C     HAPŸ†EHˆE OOPHOE „‹Ÿ 2-ƒO BEHTˆ‹ˆ‹Ÿ -->   V(“O2);
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
C                                           “PAB‹EHˆŸ COOTB. H“†HO‰
C                                           ƒP“E POBO„ˆMOCTˆ;
C
C              ‹EMEHT OˆC›BAETCŸ:
C
C    # T 92 “ K  M    “O1  “O2   ““1  ““2  DTCIU  EYYV   EYYI;
C
C     ƒ„E:     K, M  -  “‡‹› O„K‹ž—EHˆŸ,
C
C              “O1, “O2  -  “‡‹› "OOPHOƒO" HAPŸ†EHˆŸ-> V(“01), V(“O2)
C
C              ““1, ““2  -  “‡‹› "“PAB‹Ÿž™EƒO" HA-Ÿ -> [(““1)-V(““2)]
C
C              DTCIU - „‹ˆTE‹œHOCTœ ˆM“‹œCA "Cˆ“" -> [ B ‹. ƒPA„“CAX ]
C                      ˆM“‹œC› ƒEHEPˆP“žTCŸ COƒ‹ACHO CPABHEHˆž
C                      HAP. OOPHOƒO -> B›ˆPAEMOƒO ˆ‡ MACCˆBA
C                      VALLN/VALLN1 ˆ  HAPŸ†EHˆŸ "“PAB‹Ÿž™EƒO" ->
C                      B›ˆPAEMOƒO ˆ‡ VALLN/VALLN1;
C
C  EYYV - MAX. „O. AM‹ˆT“„A Cˆ“ Pˆ B›PŸMˆTE‹œHM PE†ˆME PAOT›;     ‹
C  EYYI - MAX. „O. AM‹ˆT“„A Cˆ“ Pˆ ˆHBEPTOPHOM   PE†ˆME PAOT›;
C
C
C   !!! CˆƒHA‹ "Cˆ“" ƒEHEPˆP“ETCŸ B MOMEHT EPECE—EHˆŸ KPˆBO‰
C       “PAB‹Ÿž™EƒO HAPŸ†EHˆŸ BEHTˆ‹Ÿ C KPˆBO‰ OOPHOƒO
C       HAPŸ†EHˆŸ - HˆCA„Až™ˆ‰ “—ACTOK.
C   !!!
C   !!!  „‹Ÿ "ECTECTBEHHO‰" PAOT› K‹ž—A_2 B ”A‡E –Eˆ PEKOMEH„“ETCŸ:
C
C        U_O_1<KATO„> := - E_”A‡›;
C
C        U_O_2<AHO„>  := + E_”A‡›;
C   !!!
C
C
C === B›BO„H›E ‡HA—EHˆŸ: I1         - TOK  BHE˜. B›BO„OB, OT "K" K "M";
C                        I2, I3     - TOKˆ BEHTˆ‹E‰,
C                        I3, I4     - "Cˆ“" HA BEHTˆ‹ŸX;
C
C######################################################################
C    PACPE„E‹EHˆE HAPŸ†EHˆ‰ “PAB‹EHˆŸ O         AHO„HO-KATO„H›M
C                      BEHTˆ‹ŸM
C
C                 EYY:=<HAPŸ†EHˆE “PAB‹EHˆŸ>:= [V(““1) - V(““2)]
C                      --------------------------------------------
C
C    "KATO„H›‰" BEHTˆ‹œ                      "AHO„H›‰" BEHTˆ‹œ
C
C   BEHT. 01         -->  "-" EYY       BEHT. 02         --> "+" EYY
C
C                      OOPH›E HAPŸ†EHˆŸ:
C
C   BEHT. 01   -->  V(“O1)              BEHT. 02          --> V(“O2)
C
C######################################################################
C-->  UYN/N1  - HAP. “PAB‹EHˆŸ
C
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
C
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
C   ------------   2. DM00B9, SEQCIU, SEQMAX
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=1, 10,20,3,4 - B›„A—A COO™EHˆŸ ˆ “XO„
C                3. Pˆ FTEST = 1 -> E—ATœ HA WTRE AKTA CMEH› COCTOŸHˆŸ
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
C      $$                 ( B 25 --->  EYYV )
C 26<->35    16<->25      MSWV2  -  C‹OBO “PAB‹EHˆŸ 2-M BEHTˆ‹EM
C      $$                 ( B 35 --->  EYYI )
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
C    10              - B 1-M --> EYYV,  BO 2-M --> EYYI
C
C********************************************************************
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL
         DIMENSION MSWALL(*), RMSWAL(*)
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
       DIMENSION TOKXN(2), TOKXN1(2),  THELP( 2), VHN(4), VHN1(4)
       DIMENSION KVENT( 2), MVENT( 2), UYN( 2), UYN1( 2),
     =           UON( 2), UON1( 2)
C
       INTEGER       CXA, CXK
       REAL                          IKN, IKN1, IAN, IAN1
C -->  AHO„H›E “‡‹› ( HA—A‹O BEHTˆ‹Ÿ ):
       DATA KVENT
     = /10,09/
C--> KATO„H›E “‡‹› ( KOH–› BEHTˆ‹E‰ ):
       DATA MVENT
     = /09,10/
C
C----------------------------------------------------------------------
       KOD = 0
C--->  KOHTPO‹œ TˆA
         IF ( MSW(2) .NE. 92 ) GO TO 9999
C
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
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0019 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
C
100    CONTINUE
101    CONTINUE
C-> ZAPROS = 1 -->                  -->  OC‹E„. B›‡OB MOHˆTOPA BEHˆ‹E‰
C-->  B COOTBETCTBˆˆ C MACCˆBAMˆ RMSW<50>, UYN, UYN1, UON, UON1
C
666    CONTINUE
       NEL = MSW(1)
       DO 102 NV = 1,2
C-->  A„PEC RMSW<50> BEHTˆ‹Ÿ-->
       INV = (NV-1) * 10 + 16
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
201    CONTINUE
C-->  ZAPROS=10-> TO —TO ˆ „‹Ÿ ZAPROS=1
       GO TO 666
C######################################################################
C######################################################################
C######################################################################
300    CONTINUE
C-->  ZAPROS=20 -->  TOKˆ BEHTˆ‹E‰          OPE„E‹ŸEM...>
C**********************************************************************
301    CONTINUE
C--> ZAPROS = 20  -> TOKˆ BEHTˆ‹E‰ B TOKN/N1-->
C      ITOKNV --> (ITOK-1) + 1 + NV = ITOK+0+NV
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       ITOK = MSW(7)
       NEL = MSW(1)
       DO 302 NV = 1,2
C-->  A„PEC RMSW<50> BEHTˆ‹Ÿ-->
       INV = (NV-1) * 10 + 16
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
       ITOKNV = ITOK     + NV
       TOKN1(ITOKNV) = TOKXN1(1)
       THELP(NV) = TOKXN1(1)
302    CONTINUE
C-->  "ˆTOƒOB›E" TOKˆ-->
C-->  TOK OT  "K"  K  "M" -->
C
       TOKN1( ITOK ) = THELP(2) - THELP(1)
C
C-->  TOKˆ “†E, A "Cˆ“" E™E HE OPE„E‹EH›....
       RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
400    CONTINUE
C-->  ZAPROS=3  ->  AHA‹ˆ‡ BO‡MO†HOƒO COCTOŸHˆŸ/EPEK‹ž—EHˆŸ
401    CONTINUE
C-->  ZAPROS=3 -  TOKˆ OPE„E‹EH›, HA‰„EM AHO„H›E ˆ KATO„H›E TOKˆ B
C                 ”A‡AX BEHTˆ‹E‰ „‹Ÿ B›ŸCHEHˆŸ CX, TX
C
C**********************************************************************
C----------------------------------------------------------------------
C
C-->  UON/N1  - OOPH›E HAP.
C-->  UYN/N1  - HAP. “PAB‹EHˆŸ
C
C  VHN / N1 - "ˆCTˆHH›E" HAPŸ†EHˆŸ HA “‡‹AX
C              “O1, “O2, ““1, ““2        B TN/TN+H0
C
C######################################################################
C    PACPE„E‹EHˆE HAPŸ†EHˆ‰ “PAB‹EHˆŸ O         AHO„HO-KATO„H›M
C                      BEHTˆ‹ŸM
C
C                 EYY:=<HAPŸ†EHˆE “PAB‹EHˆŸ>:= [V(““1) - V(““2)]
C                      --------------------------------------------
C
C    "KATO„H›‰" BEHTˆ‹œ                      "AHO„H›‰" BEHTˆ‹œ
C
C   BEHT. 01    -->  "-" EYY            BEHT. 02         --> "+" EYY
C
C                      OOPH›E HAPŸ†EHˆŸ:
C
C   BEHT. 01   -->  V(“O1)              BEHT. 02          --> V(“O2)
C
C######################################################################
       DO 403 I=1,4
C-->  HOMEP “‡‹A:
       K = MSW(I+10)
       VKN = 0.0
       VKN1 = 0.0
       IF ( K .EQ. 0 ) GO TO 404
       VKN  =  VALLN( K )
       VKN1 = VALLN1( K )
404    CONTINUE
       VHN(I)   = VKN
       VHN1(I)  = VKN1
403    CONTINUE
C-->  HAPŸ†EHˆŸ “PAB‹EHˆŸ*******
C******* TN BPEMŸ *****************************************************
C   K A T O „ H A Ÿ   ƒ P “   A:
       UYN(01) = - ( VHN(3) - VHN(4) )
C---            !!!!
C   A H O „ H A Ÿ   ƒ P “   A:
       UYN(02) = + ( VHN(3) - VHN(4) )
C--
C----------------------------------------------------------------------
C******* TN + H0 BPEMŸ *************************************************
C   K A T O „ H A Ÿ   ƒ P “   A:
       UYN1(01) =  - ( VHN1(3) - VHN1(4)  )
C---              !!!!
C   A H O „ H A Ÿ   ƒ P “   A:
       UYN1(02) = + ( VHN1(3) - VHN1(4) )
C--
C----------------------------------------------------------------------
C***** O  O P H › E   H A  P Ÿ † E H ˆ Ÿ:
C********TN BPEMŸ: ****************************************************
C-->
       UON(01) = VHN(1)
       UON(02) = VHN(2)
C----------------------------------------------------------------------
C********TN+H0  BPEMŸ: *************************************************
C-->
       UON1(01) =  VHN1(1)
       UON1(02) =  VHN1(2)
C-->
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --->  „‹Ÿ KA†„OƒO BEHTˆ‹Ÿ CBOE CX < AHO„HOE>, CX <KATO„HOE> ˆ TX:
C
       ITOK = MSW(7)
       IKN     = TOKN (ITOK + 01)
       IKN1    = TOKN1(ITOK + 01)
       IAN     = TOKN (ITOK + 02)
       IAN1    = TOKN1(ITOK + 02)
C----------------------------------------------------------------------
C--> HAPŸ†EHˆE “PAB‹EHˆE B TN ˆ B   TN+H0 -->
       EYYN1 = VHN1( 3) -  VHN1( 4 )
       EYYN  = VHN ( 3) -  VHN ( 4 )
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  OƒPAHˆ—EHˆE.....
       EYYV = RMSW(25)
       EYYI = RMSW(35)
       CALL SEQMAX( EYYN, IKN, IAN,  EYYV, EYYI )
       CALL SEQMAX( EYYN1, IKN1, IAN1,  EYYV, EYYI )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C----------------------------------------------------------------------
       INVK = (1-1)*10 + 16
       INVA = (2-1)*10 + 16
       CXK = MSW(INVK+4)
       CXA = MSW(INVA+4)
C----------------------------------------------------------------------
       CALL SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
     =  IFXK, IFXA,    EYYN, EYYN1 )
C
C$$$$$$GO TO 440
C
C-->  CMEHA CX ˆ TX B MSW<50>  1     -> KATO„,  2    -> AHO„  :
C
       NV = 1
C-->  A„P. HA—A‹A MSW<50>   KATO„H›‰ BEHTˆ‹œ
       INV = ( NV-1) * 10 + 16
       MSW(INV+4) = CXK
       IF ( IFXK  .EQ. 1 )    RMSW(INV+5) = TXK
C
       NV = 2
C-->  A„P. HA—A‹A MSW<50>  AHO„H›‰ BEHTˆ‹œ
       INV = ( NV-1) * 10 + 16
       MSW(INV+4) =  CXA
       IF ( IFXA  .EQ. 1 )    RMSW(INV+5) = TXA
C----------------------------------------------------------------------
450    CONTINUE
C######################################################################
C######################################################################
C-->  B›O‹HˆM ZAPROS=3  „‹Ÿ BEHT‹E‰***********************************
       NEL = MSW(1)
       ITOK = MSW(7)
       DTCIU = RMSW(15)
       DO 451 NV = 1,2
       INV = (NV-1) * 10 + 16
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
       NPEL = NEL*10000 + NV
C-->  PACTABˆM TOKXN/N1
       ITOKNV = ITOK  +  NV
       TOKXN(1) =  TOKN ( ITOKNV )
       TOKXN1(1) = TOKN1( ITOKNV )
C-->  BCE OPE„E‹EHO...
C-->  MOHˆTOPHOE OC‹“†ˆBAHˆE-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
C-->  PˆMEM CˆƒHA‹› "Cˆ“"-->
       ICIUNV = ITOK + 2 +  NV
       TOKN1(ICIUNV) = TOKXN1(2)
451    CONTINUE
C-->  ‡APOC“ ...–
       RETURN
C----------------------------------------------------------------------
C######################################################################
500    CONTINUE
C-->  ZAPROS=4
C
C-->  „‹Ÿ BEHTˆ‹E‰  -->  AHA‹Oƒˆ—HO ZAPROS=1-->
       GO TO 666
C
       END
