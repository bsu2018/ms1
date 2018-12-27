         SUBROUTINE DM00B9(KQ,MQ,NPEL,MSWX, RMSWX, YALL, JALL, LOGYTR,
     *                       TOKXN, TOKXN1, VALLN, VALLN1, KOD,
     *         UOPN, UOPN1, UYPN, UYPN1, DTCIU )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM00B9 (KQ,MQ,NPEL, MSWX, RMSWX, YALL, JALL, LOGYTR,
C   -------------            TOKXN, TOKXN1, VALLN, VALLN1, KOD,
C                             UOPN, UOPN1, UYPN, UYPN1, DTCIU )
C
C
C   HA‡HA—EHˆE:    MOHˆTOP MO„E‹ˆ “P. BEHTˆ‹Ÿ  [ MO„. # B9, .Tˆ # 6 ]
C   -----------    ****************************************************
C                  [ CE–. PˆMEHEHˆE B MAKPOMO„E‹ŸX PEOPA‡OBATE‹E‰ ]
C                  ----------------------------------------------------
C @ - OT 21.11.86.
C
C                      MACCˆB› MSWX ( RMSWX ) - CE–ˆA‹œHOƒO ”OPMATA:
C                      -------------
C**********************************************************************
C
C
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
C
C                      MO„E‹œ OPE„E‹ŸET HA‹ˆ—ˆE Cˆƒ. “PAB‹EHˆŸ "Cˆ“"
C                      HA BEHTˆ‹E O A‹ƒOPˆTM“ CPABHEHˆŸ OOPHOƒO
C                      ˆ MO„“‹ˆP“ž™EƒO HAPŸ†EHˆ‰:
C
C                      UOPN, UOPN1 - ‡HA—EHˆE OOPHOƒO HAP. B TN, TN+H0
C                      UYPN, UYPN1 - ‡HA—EHˆE MO„“‹ˆP. HAP. B TN, TN+H0
C
C                     - Tˆ HAPŸ†EHˆŸ, A TAK †E O‹Ÿ "CX" ˆ "TX" B MSWX
C                      HEOXO„ˆMO OPE„E‹ˆTœ TO‹œKO Pˆ "ZAPROS=3"
C                      O‹Ÿ B TOKXN, TOKXN1 OT (1) - „O‹†H› K TOM“
C                      MOMEHT“ TAK †E ›Tœ OPE„E‹EH›;
C
C                     - B OCTA‹œH›X "ZAPROC=1, 10, 20, 4", „OCTATO—HO
C                       ˆMETœ OPE„E‹EH›Mˆ TO‹œKO RMWX ˆ VALLN/N1;
C
C
C                      ”OPMAT MACCˆBOB TOKXN ˆ TOKXN1:
C
C      1 - C‹OBO -> TOK BEHTˆ‹Ÿ ;       ( OPE„E‹ŸETCŸ „‹Ÿ TN+H0 Pˆ
C                                         ZAPROS = 20 );
C      2 - C‹OBO -> ‡HA—EHˆE CˆƒHA‹A “P. "Cˆ“" ( OPE„. Pˆ ZAPROS=3);
C
C
C              DTCIU - „‹ˆTE‹œHOCTœ ˆM“‹œCA "Cˆ“", Pˆ
C                      DTCIU_PAC—ETHOE < DTMCIU[MASPRT(94)] --->
C                      C—ˆTAETCŸ, —TO "Cˆ“" HA BEHTˆ‹œ HE O„ABA‹OCœ;
C
C              K, M - “‡‹› O„K‹. BEHTˆ‹Ÿ, NPEL - EƒO OPŸ„. HOMEP.
C
C              "ECTECTBEHHOE" “PAB‹EHˆE "Cˆ“":[ DU** = U**N1 - U**N ]:
C              Pˆ DUN < 0, A DUN1 >= 0, ƒEHEPˆP“ETCŸ EPE„. ”POHT "Cˆ“"
C              Pˆ TOM “—ˆT›BAETCŸ KO„ CX ˆ TX BPEMŸ -> CX - KO„
C              PA‡PE˜EHˆŸ (=1) ˆ‹ˆ (=0) ‡APETA O„A—ˆ "Cˆ“"( CM. /
C                                                          MCIU50 );
C                  B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C                  B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                 -  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰
C                   ‡HA—EHˆ‰ "POBO„ˆMOCTœ" ‡AKP›TOƒO BEHTˆ‹Ÿ ( GVENT6 )
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
C                  <TOKXN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK ‡AKP›T›X BEHTˆ‹E‰ = 0.0;
C                  ‡AˆCœ TˆX TOKOB B MACCˆB TOKXN1;
C                   !!! B›O‹HŸTœ „O MO„ˆ”ˆKA–ˆˆ PE˜EHˆŸ VALLN1 !!!
C
C                  C) ZAPROS = 3
C                     -----------
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
C                      KD_BEHTˆ‹Ÿ;
C                     PABEHCTBO POBEPŸETCŸ C TO—. „O "EPSKOM".
C
C   METO„:         PA‡HOBˆ„HOCTœ BO‹HOBOƒO A‹ƒOPˆTMA „‹Ÿ ”OPMˆPOBAHˆŸ
C   -----          MACCˆBA LOGYTR --> —EPE‡ / GETLOG;
C
C                  "TEHEBAŸ" CˆCTEMA ”OPMˆP“ETCŸ "MAC˜TAˆPOBAHHAŸ"
C                   TAK, —TO: V[X] - V[Y] = <TOK    X-Y   BETBˆ>;
C                   “‡‹›, OTME—EHH›E B LOGYTR(*) = 0 - "A‡OB›E"
C                   „‹Ÿ KA†„O‰ ˆH–ˆ„EHTHO‰ ƒP“› OTKP›T›X BEHTˆ‹E‰;
C
C   APAMETP›:     MSWX(RMSWX) - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------      ( MACCˆB CE–. HA‡HA—EHˆŸ );
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  T  =  TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  TN + H0;
C                  TOKXN  ˆ TOKXN1 - ‡HA—EHˆE EPEMEHH›X "COCTOŸHˆŸ"
C                                  MO„E‹ˆ Pˆ TN  ˆ  TN + H0;
C                  ( 1-C‹OBO -> TOK, 2-C‹OBO "Cˆ“" );
C
C                KQ -  "AHO„" - HOMEP “‡‹A;
C                MQ - "KATO„" - HOMEP “‡‹A;
C
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
C   ------------   2. GETLOG,    MCIU50,       DEPOST;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=1, 10,20,3,4 - B›„A—A COO™EHˆŸ ˆ “XO„
C                3. Pˆ FTEST = 1 -> E—ATœ HA WTRE AKTA CMEH› COCTOŸHˆŸ
C
C   **** BHˆMAHˆE *** B 06.01.87   ‹OKˆPOBAHO B›BO„ COO™EHˆˆŸ
C                                  O CPOCE B CTAPT. TO—K“ Pˆ
C                                  MMCIU = 11 ***********
C
C
C
C   ****BHˆMAHˆE *****
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSWX(*), RMSWX(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKXN(*), TOKXN1(*)
         REAL JALL, JRL, L, IRLT, IRLN
       DIMENSION MSWALL(*), RMSWAL(*)
       INTEGER ADRMSW
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(94), DTMCIU )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(59), GVENT6 )
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
       K = KQ
       M = MQ
C---> B›OP PE†ˆMA:
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997)         ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #00B9', ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "CPOC" ‡HA—EHˆŸ TX - BO‡MO†HOƒO BPEMEHˆ EPEK‹ž—EHˆŸ.....
         RMSWX(2) =      H0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSWX(9) = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KD = MSWX(1)
C--->    EC‹ˆ "K" ˆ "M" ˆH–ˆ„EHTH› --->  B›‡BATœ GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECTœ "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG( K, M, LOGYTR )
         RETURN
6607     CONTINUE
C===> BEHTˆ‹œ ‡AKP›T ==> KD=0 <====== "MAKET" O—EHœ MA‹O‰ POBO„ˆMOCTˆ
C---> EC‹ˆ NVENT6 = 0.0, TO MAKET HE H“†EH...
         IF ( GVENT6 .EQ. 0.0 ) RETURN
C--->
         G = GVENT6
C--->
C--->   O‹EMEHTH›‰ BK‹A„:
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
C---->
         RETURN
C======================================================================
C$$$$$$$ ZAPROS = 10 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  ”OPMˆPOBAHˆE "TEHEBO‰" CˆCTEM› „‹Ÿ ƒP“ OTKP›T›X BEHTˆ‹E‰
6670     CONTINUE
         KOD = 0
         KD = MSWX(1)
         IF ( KD .EQ. 0 ) RETURN
C----------------------------------------------------------------------
         G = 1.0
C----------------------------------------------------------------------
C--->  B›POCˆTœ "A‡OB›E" “‡‹›    ˆH–ˆ„EHTH›X ƒP“...
         IF ( K .EQ. 0 ) GO TO 6671
         IF ( LOGYTR(K) .EQ. -1 ) K = 0
6671     CONTINUE
         IF ( M .EQ. 0 ) GO TO 6672
         IF ( LOGYTR(M) .EQ. -1 ) M = 0
6672     CONTINUE
C--->  BK‹A„›BAEM....
C======================================================================
C--->
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
C--->
         RETURN
C======================================================================
C$$$$$$ZAPROS = 20 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C===> PAC—ˆTATœ TOKˆ BEHTˆ‹E‰ !!! B›O‹HŸTœ „O MO„ˆ”ˆKA–ˆˆ VALLN1 !!!!
6660     CONTINUE
         KOD = 0
         KD = MSWX(1)
         IF ( KD .EQ. 0 ) GO TO 6661
C--->  BEHTˆ‹œ OTKP›T, B›PACˆTœ "A‡OB›E" “‡‹› O „AHH›M LOGYTR....
         IF ( K .EQ. 0 ) GO TO 6662
         IF ( LOGYTR(K) .EQ. -1 ) K = 0
6662     CONTINUE
         IF ( M .EQ. 0 ) GO TO 6663
         IF ( LOGYTR(M) .EQ. -1 ) M = 0
6663     CONTINUE
         TK = 0.0
         IF ( K .NE. 0 ) TK = VALLN1(K)
         TM = 0.0
         IF ( M .NE. 0 ) TM = VALLN1(M)
         TOKXN1( 1  ) = TK - TM
         RETURN
6661     CONTINUE
C--->  BEHTˆ‹œ ‡AKP›T....
         TOKXN1( 1  )  =  0.0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKXN, TOKXN1, VALLN, VALLN1 OPE„E‹EH›------
C-----------------------------------------!!! VALLN1 MO„ˆ”ˆ–ˆPOBAHO !!!
C----OPE„E‹ˆM TEK. ˆ PE„. COCTOŸHˆŸ (?)------------------------------
C
C**** PA‡EPEMCŸ C "Cˆ“" **********************************************
       CALL MCIU50( TN, H0, UOPN, UOPN1, UYPN, UYPN1, DTCIU, DTMCIU,
     =              MSWX(3), RMSWX(3) )
C-->  O‹Ÿ MCIU ( MSWX(7) ) ˆ HXCIU ( RMSWX(8) - OPE„E‹EH›
C
C-->  "Cˆ“"  B  TOKXN1
C
       TOKXX = 0.0
       MMCIU = MSWX(7)
       IF ( MMCIU .EQ. 01  .OR. MMCIU .EQ. 11 ) TOKXX = 1.0
       TOKXN1(2) = TOKXX
C----------------------------------------------------------------------
C===>    HOBOE HAPŸ†EHˆE ˆ TOK
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         TVT  = TOKXN1(  1   )
C--> A PAHœ˜E ›‹O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         TVN  = TOKXN(  1   )
C----‡A‰MEMCŸ COCTOŸHˆŸMˆ----------------------------------------------
C!!!-------------------------------------------------------------------
C      KD = MSWX(1)
C      CALL SEQSOS(NPEL,    TN, KD, TVN,  UN )
C      MSWX(1) = KD
C!!!-------------------------------------------------------------------
         KD = MSWX(1)
         IF ( KD .EQ. 0 ) GO TO 601
C---> BEHTˆ‹œ OTKP›T (KD=1), HE XO—ET ‹ˆ OH ‡AKP›TœCŸ?
C        IF (  UN .GT. 0.0  .AND. TVN  .GE. 0.0
C    =                     .AND.  TVT  .LT. 0.0  ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  KOPPEKTˆPOBKA TVN ********
       IF ( TVN .LT. 0.0  .AND. TVN .LE. -ICLOSE ) TVN = 0.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF (
     =                          TVT .LT. -ICLOSE ) GO TO 602
C                                   @@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C---> ˆHA—E COCTOŸHˆE BEHTˆ‹œA HE ˆ‡MEHˆ‹OCœ....
         KOD = 0
         RETURN
602      CONTINUE
C--->  BEHTˆ‹œ #‡AKP›‹CŸ#  B TE—EHˆˆ BPEMEHˆ OT TN „O TN+H0....
C      KOƒ„A TO MOƒ‹O C‹“—ˆTCŸ?   ( HX - PEA‹œHO B›O‹HEHH›‰ ˜Aƒ )....
C !!!! CPABE„‹ˆBO Pˆ TVN = (-ICLOSE) !!!
         HX =    + H0 * (  (-ICLOSE - TVN) /(TVT - TVN)  )
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 604 ) NPEL,   KD, TN, H0, HX,
     =                                           TVN, TVT, ICLOSE
604   FORMAT(' *** ‹EMEHT # ', I8, ' MO„E‹œ "BEH. #_B9" ==> ???? !?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***'/' *** TVN = ', E12.5, ' TVT = ',E12.5,
     =   ' ICLOSE = ', E12.5,  ' ***' )
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSWX(2) = HX
C--->  EC‹ˆ TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMETˆTœ TO B "FKOM"....
         FKOM = 1
C@@@@  ”‹Aƒ KD  ”ˆKCˆP“EM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       MSWX(9) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
         KOD = 0
C===> BEHTˆ‹œ ‡AKP›T (KD=0), HE †E‹AET ‹ˆ OH OTKP›TœCŸ?....
         IF ( UT .LE. UOPEN  ) GO TO 6030
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C--->  —TO „E‹AET "Cˆ“" ???????????????
         MMCIU = MSWX(7)
         IF (                      MMCIU .EQ. 01 ) GO TO 603
C               @@@@@@
C-->  MO†ET ›Tœ M› „ABHO "XO—EM" ?
         IF (                      MMCIU .EQ. 11 ) GO TO 603
C                @@@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C@@@@@@-->> OT 06.01.87. <<-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  OPAOTKA KOH–A....
       IF (                     MMCIU .EQ. 00 ) GO TO 6030
C-->  MMCIU = 10  -->  Cˆ“ "KOH—AET"
       HX = 0.0
       IF ( UN .GT. UOPEN ) GO TO 606
       HX = +H0 * ( (UOPEN - UN) / (UT-UN) )
       HXCIU = RMSWX(8)
       IF ( HX .LE. HXCIU ) GO TO 606
6030   CONTINUE
C@@@@@@--> OT 06.01.87. <<-- KOHE–-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===>  HET, HE BO‡†E‹A‹, ˆ‹ˆ HE „A‹ˆ --> COCTOŸHˆE HE MEHŸETCŸ...
         KOD = 0
         RETURN
603      CONTINUE
C===>   BEHTˆ‹œ B TE—EHˆˆ TN  <-->  TN+H0 „O‹†EH OTKP›TœCŸ ...
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C       KOƒ„A OŸBˆ‹CŸ "Cˆ“" ???????????
         HXCIU = RMSWX(8)
         HX = HXCIU
         IF ( UN .GT. UOPEN ) GO TO 606
C                @@@@@
C--->  UN  ˆ  UT  "PA‡HOƒO" ‡HAKA...
C   HX  ->  "˜Aƒ" "ECTECTBEHHO‰" KOM“TA–ˆˆ
         HX  =    + H0 * (  ( UOPEN -  UN ) /(UT - UN)  )
C-->  KOƒ„A EM“ "Cˆ“" PA‡PE˜ˆ‹O...
         IF ( HXCIU .GT. HX ) HX = HXCIU
606      CONTINUE
C--->
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C!!!--------**** BHˆMAHˆE ****-----------------------------------------
         IF ( MMCIU .NE. 11 .AND. HX .LE. 0.0 ) WRITE (NWTRE, 605 )
     =  NPEL, KD, TN, H0, HX,                    UN, UT, MMCIU
605   FORMAT(' *** ‹EMEHT # ', I8, ' MO„E‹œ "BEH. #_B9" ==> ???? !?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5,  ' ***'/ ' *** UN = ', E12.5, ' UT = ', E12.5,
     =  ' MCIU = ', I2,  ' ***'  )
C!!!-------------------------------------------------------------------
         IF ( HX  .LT. HXMIN ) HX  = HXMIN
         RMSWX(2) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@  ”‹Aƒ KD  ”ˆKCˆP“EM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       MSWX(9) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        KOD = 0
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ MO„E‹ˆ.................ZAPROS = 4
C---->  HET ‹ˆ HEOXO„ˆMOCTˆ POˆ‡BECTˆ CMEH“ COCTOŸHˆŸ BEHTˆ‹Ÿ....?
C@@@@  ”‹Aƒ KD “CTAHOB‹EH ?
       IF ( MSWX(9) .NE. -1 ) RETURN
C@@@ MO†HO POBEPŸTœ @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC‹ˆ TXG   "PˆMEPHO" PABHO TX=MO„E‹ˆ, TO CMEHA H“†HA!
         HX   = RMSWX(2)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> ˆHA—E CMEHˆM COCTOŸHˆE....
         KD = MSWX(1)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSWX(1) = KD
C---->  EC‹ˆ BEHTˆ‹œ ‡AKP›‹CŸ, ‡A”ˆKCˆP“EM EƒO HOMEP.....
         IF ( KD .EQ. 0 ) NTXG = NPEL
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) NPEL,   KD, TN, TXG
501     FORMAT(' ***FTEST_1: B ‹.#',I8, ' KD CTA‹O =',I2,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
