         SUBROUTINE GETNEW( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     *          YALL, JALL, LOGYTR, WORKX, IRC, YALLHP, WORKY )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  GETNEW( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
C   ------------    YALL, JALL, LOGYTR, WORKX, IRC, YALLHP, WORKY )
C
C   HA‡HA—EHˆE:    - B›—ˆC‹EHˆE O—EPE„HO‰ TO—Kˆ "COCTOŸHˆŸ" –Eˆ:
C   -----------    *** „‹Ÿ T = TN + HXG OPE„E‹ŸžTCŸ TOKN1 ˆ VALLN1 ***
C
C              == Pˆ IPOL = 0 --> O›—HAŸ ˆHTEPO‹Ÿ–ˆŸ, ‹ˆHE‰HAŸ;
C                 Pˆ IPOL = 1 --> CE–ˆA‹œHAŸ, „‹Ÿ METO„A ‹ˆH.-BˆO‹.;
C ***‹ETO 1987********** BO‹EBOE PE˜EHˆE: Pˆ DX<ZEROCP, DX=0.0 *****
C
C                      - Pˆ ‡APOCE (FRUN0 = 1) ---> B›O‹HŸETCŸ
C                        TECT P“HƒE, Pˆ TOM „O“CKAETCŸ
C                        HE O‹EE NRUNG "„PO‹EHˆ‰" H0DATA;
C                         HXMIN "„POˆTCŸ" CˆHXPOHHO;
C                      - B C‹“—AE, H0 > H0_OLD -> TECT POBO„ˆTCŸ BHE
C                        ‡ABˆCˆMOCTˆ OT FRUN0;
C
C                      - Pˆ PONRUN = 1 (CM. MASPRT) B›BO„ˆTCŸ C‹E„“ž™EE
C                        TN  <CTAPTOBAŸ TO—KA>
C                        TN1 <"CTAPAŸ" TO—KA Pˆ TN+H0>
C                        TNX1 <HOBAŸ TO—KA  Pˆ TN+H0/2>
C                        TNX2 <HOBAŸ TO—KA  Pˆ TN+H0/2+H0/2>
C
C
C                      *** Pˆ FRUNGE = 0 TECT Hˆ KOƒ„A HE POBO„ˆTCŸ **
C                      *** Pˆ FRUNGE = 2 TECT POBO„ˆTCŸ HA KA†„OM   **
C                                               ˜AƒE                ****
C
C
C              == Pˆ KODSYS = 1 --> PE˜EHˆE C‹A“ E‡ ˆTEPA–ˆ‰;
C
C              == Pˆ KODSYS = 2 --> PE˜EHˆE C‹A“ C   ˆTEPA–ˆŸMˆ;
C                                    EPSSYS--> TO—HOCTœ;
C                      ( "K“COK" -->  "C@C‹A“@" );
C
C
C                  - CMEHA COCTOŸHˆŸ BEHTˆ‹œH›X ‹EMEHTOB, BO‡“†„EHˆE
C                    ”‹AƒA FKOM B C‹“—AE HA‹ˆ—ˆŸ KOMM“TA–ˆˆ BEHTˆ‹E‰...
C                    [ Pˆ OBTOPE PAC—ETA C TO—Kˆ TN, HEOXO„ˆMO HE
C                      ˆ‡MEHŸŸ ‡HA—EHˆ‰ FKOM, HXG ˆ HX B MSWALL MO„E‹E‰
C                      "BEPH“Tœ" BEHTˆ‹ˆ B COCTOŸHˆE Pˆ T = TN,
C                      „‹Ÿ —EƒO BOCO‹œ‡OBATœCŸ / RESETX..].
C
C                  - Pˆ HEOXO„ˆMOCTˆ B TN = T0 ( CM. MASPRT )
C                    POBE„EHˆE POCECCA "OPE„E‹EHˆŸ ˆCXO„HOƒO
C                    COCTOŸHˆŸ BEHTˆ‹œH›X ‹EMEHTOB", A TAK†E B C‹“—AE
C                    TXG == TN, T.E. Pˆ HXG = 0.0 ==> POˆ‡BO„ˆTCŸ
C                    SEQTIM BO‡BPATOB B "CTAPTOBYž" TO—KY TN PAC—ETA:
C                    Pˆ TOM -> B›BO„ ˆH”OPMA–ˆOHHOƒO COO™EHˆŸ;
C                   ( C.—.K. SEQTIM - KOHTPO‹œ "‡A–ˆK‹ˆBAHˆŸ" );
C                    Tˆ CˆT“A–ˆˆ BO‡MO†H› Pˆ HXMIN = 0.0;
C
C                  - Pˆ B›PO†„EHˆˆ CˆCTEM› - POˆCXO„ˆT 10-ˆ
C                    KPATHAŸ O›TKA ˆ‡ABˆTCŸ OT TO‰ CˆT“A–ˆˆ;
C                    TO BO‡MO†HO Pˆ BK‹ž—EHˆˆ ”‹AƒA SEQTRX ˆ Pˆ
C                    “C‹OBˆˆ HE PABEHCTBA 0.0 POBO„ˆMOCTˆ GVENT6;
C                    BHE ‡ABˆCˆMOCTˆ OT ”‹AƒA SEQTRX, Pˆ PAC—ETE TO—Kˆ
C                    T = T0, KOHTPO‹œ B›PO†„EHHOCTˆ CˆCTEM› BKž—AETCŸ;
C
C                  - Pˆ ‡APOCAX OT DM... MOHˆTOPOB ˆTEPA–ˆOHH›X
C                    “TO—HEHˆ‰ EPEMEHH›X COCTOŸHˆŸ [ TOKN1 ],
C                    B›O‹HŸETCŸ HE O‹EE MITER ˆTEPA–ˆ‰;
C
C                - KA†„›‰ DM MOHˆTOP .TˆA 2 & 3 Pˆ ZAPROS=2 B›O‹HŸET
C                  CPABHEHˆE PE„CKA‡AHHOƒO ‡HA—EHˆŸ BEKTOPA "TOK"
C                  (XPAHˆTCŸ B "TOKN1") C B›—ˆC‹EHH›M ˆCXO„Ÿ ˆ‡
C                  ‡HA—EHˆ‰ VALLN1;
C                B C‹“—AE, KOƒ„A EPS=ABS(TOK_Ÿ - TOKN1)/MAX(TOK_Ÿ,TOKN1)
C                  > EITER --> KOD=1 ˆ Pˆ MAXITR<EPS -->
C                 ”ˆKCˆP“ETCŸ HOBOE ‡HA—EHˆE MAXITR=EPS ˆ NLITER=N_‹-TA
C
C                - **** “TO—HEHˆE COCTOŸHˆŸ ***: TO-->
C                  BO‡“†„EHˆE ”‹AƒA FITER, —TO TPE“ET OT DM MOHˆTOPOB
C                  .TˆA 2 & 3 Pˆ B›—ˆC‹EHˆˆ G-J APAMETPOB ˆCPO‹œ-
C                  ‡OBATœ HE PE„CKA‡AHH›E ‡HA—EHˆŸ "TOK", A ‡HA—EHˆŸ
C                  "TOK", XPAHŸ™ˆECŸ B MACCˆBE "TOKN1";
C
C                - Pˆ MITER = 0  -->  KOHTPO‹Ÿ HA KOD ^=0 HET !!!!!!!!!
C
C
C   METO„:         - „ˆCKPETˆ‡A–ˆŸ “PABHEHˆ‰ COCTOŸHˆŸ MAKPOMO„E‹E‰
C   -----            HA OCHOBE METO„A ‹ˆHˆ„†EPA-BˆO‹“ƒˆ (H2^=0) ˆ‹ˆ
C                    HE ŸBHOƒO METO„A ‰‹EPA (H2=0), COCTAB‹EHˆE CˆCTEM›
C                    ‹ˆHE‰H›X A‹ƒE. “P-ˆ‰ ˆ ˆX PE˜EHˆE;
C                    “PAB‹EHˆE COCTOŸHˆEM BEHTˆ‹œH›X ‹EMEHTOB HA
C                    OCHOBE ˆH”OPMA–ˆˆ „‹Ÿ TN ˆ TN+H0 MOMEHTA BPEMEHˆ;
C                    ˆHTEPO‹Ÿ–ˆŸ Pˆ HEOXO„ˆMOCTˆ TOKOB ˆ HAPŸ†EHˆ‰;
C
C                 - Pˆ POBE„EHˆˆ TECTA P“HƒE -> ˆCO‹œ‡“ETCŸ
C                   PAO—ˆ‰ ”A‰‹ HA KAHA‹E NFILER; CTEK „‹Ÿ
C                   „B“X TO—EK PE˜EHˆŸ Pˆ T = TN  ˆ T = TN1;
C                   KOHTPO‹œ ‹OK. O˜ˆKˆ - O BEKTOPAM “‡‹. HAPŸ†EHˆ‰;
C
C
C                  - "B›BO„" BEHTˆ‹E‰ B HA—A‹œHOE COCTOŸHˆE
C                    OC“™ECTB‹ŸETCŸ B TOM C‹“—AE, KOƒ„A PE†ˆM AHA‹ˆ-
C                    ‡ˆP“EMO‰ –Eˆ, ‡A„AHH›‰ O‹œ‡OBATE‹EM, HE COOTBET-
C                    CTB“ET "HA—A‹œHOM“" COCTOŸHˆž BEHTˆ‹E‰.
C                    ( T.E. EC‹ˆ HA ˜AƒE OT "TN=T0" „O "TN+H0"
C                      HEKOTOP›E BEHTˆ‹ˆ ˆ‡MEHŸžT CBOE COCTOŸHˆE
C                      TAK, —TO TX (BPEMŸ BO‡MO†HO‰ KOMM“TA–ˆˆ) = T0,
C                      TO B›O‹HŸETCŸ "HACTPO‰KA" TˆX BEHTˆ‹œH›X
C                      ‹EMEHTOB, KOTOPAŸ ‡AK‹ž—AETCŸ B CMEHE ˆX
C                      COCTOŸHˆŸ; ‡ATEM TO—KA TN+H0 PAC—ˆT›BAETCŸ
C                      ‡AHOBO. TA PO–E„“PA B›O‹HŸETCŸ HE     O„ˆH
C                      PA‡, HO TO‹œKO B TO—KE T = T0 ).
C
C          ==     B ‡ABˆCˆMOCTˆ OT METVET[MASPRT(107)] HXG OPE„E‹ŸETCŸ:
C              =0 - ˆHTEPO‹Ÿ–ˆŸ K TXG ME†„“ "TN"  ˆ  "TN1 =TN+H0";
C              =1 - EPEC—ET K TXG OT TN C H0=HXG;
C                   Pˆ TOM HXMIN - MIN BO‡MO†H›‰ ˜Aƒ;
C              =2 - "C‹ˆAHˆE" "TN1" C "TXG",
C                   Pˆ TOM CMEHA COCTOŸHˆ‰ BEHTˆ‹E‰ C
C                    EPSKOM = TN1 - TXG
C              >2 - TO †E, —TO ˆ =2, HO EPSKOM HE MO„ˆ”ˆ–ˆP“ETCŸ;
C
C          ==  Pˆ METVET=1 ˆ HXMIN=0 --> PAC—ET KAK „‹Ÿ METVET=0  ===
C
C
C   APAMETP›:     MSWALL - C”OPMˆPOBAHH›‰ CˆCOK C‹OB COCTOŸHˆŸ
C   ---------               MO„E‹E‰ ‹EMEHTOB –Eˆ;
C                  VALLN, TOKN - HAPŸ†EHˆŸ ˆ "TOKˆ" –Eˆ Pˆ
C                                T = T0, T.E. B HA—A‹œH›‰ MOMEHT BPEMEHˆ
C                                B OC‹E„CTBˆˆ - B TEK“™ˆ‰ "CTAPTOB›‰"
C                                MOMEHT BPEMEHˆ T.E. B T = TN;
C                  VALLN1 ˆ TOKN1 - TO †E „‹Ÿ  TN = TN + HXG  ( H0 );
C                  YALL - MATPˆ–› “‡‹OB›X POBO„ˆMOCTE‰;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA;
C                  LOGYTR - MACCˆB Pˆ‡HAKOB K.‡. “‡‹OB;
C                           ( „‹Ÿ MAKPOMO„E‹E‰ ˆ„EA‹œH›X BEHTˆ‹E‰ );
C                 WORKX - PA. MACCˆB, PA‡MEPHOCTœž HE MEHEE NSYS;
C                  IRC = 1 TO—KA PAC—ˆTAHHA, Pˆ FKOM = 1 COCTOŸHˆŸ
C                          BEHTˆ‹E‰ ˆ‡MEHEH›;
C                      = 2 „‹Ÿ MO„E‹E‰ C .TˆOM #2 ˆ/ˆ‹ˆ #3 ‡A MITER
C                          ˆTEPA–ˆ‰ HE „OCTˆƒH“TA TO—HOCTœ EITER;
C                          ( TA CˆT“A–ˆŸ B V.M=2.0 ˆƒHOPˆP“ETCŸ )
C                      = 3 - TECT P“HƒE HE B›O‹HEH, TO—HOCTœ HE
C                            „OCTˆƒH“TA „A†E OC‹E NRUNGE
C                            PA‡ "„PO‹EHˆŸ" H0DATA;
C
C                      Pˆ KODSYS = 2 HEOXO„ˆMO:
C                      WORKY --> PA‡MEPHOCTœ  <= NSYS;
C                      YALLHP --> PA‡MEPHOCTœ NDALL * NDALL;
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   -------------  2. GETSYS,   MODVAL,  DECOM0+SOLVE0
C                  3. PRINT4, RSLMC, FACTR;
C   PˆME—AHˆE:
C   ----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(1)
         COMMON /PROF3/ FUNTAB(1)
         DIMENSION MSWALL(1), TOKN(1), TOKN1(1), VALLN(1), VALLN1(1)
         DIMENSION YALL(1), JALL(1), LOGYTR(1)
         REAL JALL, MU
         INTEGER FEND, FKOM, ZAPROS, T5MSW, T6MSW, PON, FLOG
         EQUIVALENCE ( MASPRT(29), TN ),
     =               ( MASPRT(30), H0 ),
     =               ( MASPRT(31), MU ),
     =               ( MASPRT(32), H1 ),
     =               ( MASPRT(33), H2 ),
     =               ( MASPRT(50), ZEROCP ),
     =               ( MASPRT(13), NWTR ),
     =               ( MASPRT(14), NWTRE ),
     =               ( MASPRT(35), TXG ),
     =               ( MASPRT(23), NDALL ),
     =               ( MASPRT(37), FLOG ),
     =               ( MASPRT(26), NSYS ),
     =               ( MASPRT(34), FKOM ),
     =               ( MASPRT(41), T5MSW ),
     =               ( MASPRT(49), T6MSW ),
     =               ( MASPRT(46), NNTOK ),
     =               ( MASPRT(56), PON   ),
     =               ( MASPRT(42), T0    ),
     =               ( MASPRT(28), ZAPROS )
         EQUIVALENCE ( HXG, MASPRT(61) )
         EQUIVALENCE ( GVENT6, MASPRT(59) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( MITER,  MASPRT(66) )
         INTEGER FITER, SEQTRX
         EQUIVALENCE ( SEQTRX, MASPRT(64) )
         EQUIVALENCE ( SEQTIM, MASPRT(71) )
         EQUIVALENCE ( NDIVH0, MASPRT(84) )
         EQUIVALENCE ( NFILER, MASPRT(83) )
         EQUIVALENCE ( FRUN0,  MASPRT(82) )
         EQUIVALENCE ( NRUNGE, MASPRT(70) )
         EQUIVALENCE ( ERUNGE, MASPRT(69) )
         EQUIVALENCE ( H0OLD,  MASPRT(81) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( FRUNGE, MASPRT(68) )
         EQUIVALENCE ( H0DATA, MASPRT(40) )
         EQUIVALENCE ( PONRUN, MASPRT(86) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( PPVENT, MASPRT(106) )
         EQUIVALENCE ( EPSKOM, MASPRT(39)  )
         EQUIVALENCE ( HXMIN,  MASPRT(62)  )
         EQUIVALENCE ( METVET, MASPRT(107)  )
         EQUIVALENCE ( IPOL,   MASPRT(108)  )
         EQUIVALENCE ( KODSYS, MASPRT(109)  )
         EQUIVALENCE ( EPSSYS, MASPRT(110)  )
         EQUIVALENCE ( KODMU,  MASPRT(113)  )
         EQUIVALENCE ( KODMUM,  MASPRT(114)  )
         INTEGER PPVENT
         DIMENSION WORKX(1), WORKY(1), YALLHP(1)
         REAL MAXITR
         INTEGER CASE, FRUN0, FRUNGE, PONRUN, CASE1
         INTEGER SEQTIM
C
C--->  Pˆ SEQTRX = 1  -  BK‹ž—AETCŸ KOHTPO‹œ HA B›PO†„EHHOCTœ;
C
         ID(I,J) = NDALL * (J-1) + I
C====>
         DATA IHELPV/ 0 /, IHELP6/0/, K10/10/
         MAXITR = 0.0
         NLITER = 0
        CASE = 0
        CASE1 = 0
        FKOM = 0
C       ********
100    CONTINUE
         IHELP6 = 0
         IHELPV = 0
C--->  KOHTPO‹œ BKž—ˆM:
         COND = 0.0
C-->  Pˆ SEQTRX = 0, B›K‹ž—ˆM:
         IF ( SEQTRX .EQ. 0 ) COND = -1.0
C--> Pˆ TN = T0, KOHTPO‹œ BK‹ž—ˆM PˆH“„ˆTE‹œHO...
         IF ( T0 .EQ. TN ) COND = 0.0
C---> NITER - C.—.K. ˆTEPA–ˆ‰, FITER - ”‹Aƒ ˆTEPA–ˆ‰....
         NITER = 0
         FITER = 0
         IRC = 1
C======================================================================
C--->   ‡A”ˆKCˆP“EM BBE„EHHOE ‡HA—EHˆE GVENT6.......
         HELP6 = GVENT6
         GVENT6 = 0.0
52       CONTINUE
C----------------------------------------------------------------------
C==>  C”OPMˆPOBATœ CˆCTEM“
         CALL GETSYS( MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1 )
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C--->>>   C ‹ A “  <<<-------------------------------------------------
C --  METKA  201  --> ˆTEP. METO„.
       GO TO ( 200, 201 ), KODSYS
       CALL SUBERR(KODSYS)
200    CONTINUE
C
C   PE˜ˆTœ CˆCTEM“ < DECOM0 + SOLVE0 >
C===> IPVT  <==> VALLN1
C===> WORK  <==> WORKX
         CALL DECOM0( NDALL, NSYS, YALL, COND, VALLN1, WORKX  )
C
         IF ( COND .NE. COND+1. ) GO TO 1
C--->>  ABAPˆŸ....
202    CONTINUE
C---> EC‹ˆ MO†EM - TO „E‹O ›TAEMCŸ ˆCPABˆTœ....
         IF ( HELP6 .EQ. 0.0 ) GO TO 15
C---->  EC‹ˆ ˆ„EA‹œH›X BEHTˆ‹E‰ HET, TO ˆ ˆCPAB‹ŸTœ HE—EƒO.....
         IF ( FLOG .EQ. 0 ) GO TO 15
C---> "BŸKHEM"
         T1HELP = TN + H0
         WRITE(NWTRE, 16 )         TN, H0,  T1HELP
16       FORMAT(' *** B TO—KE: TN = ', G12.5, ' H0 = ', G12.5,
     =           ' TN1 = ', G12.5, '     ***'/
     =' ***  CˆCTEMA [Y] * [V] = [J] B_›_P_O_†_„_E_H_A ',24X,  ' ***')
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( IHELP6 .GT. K10 ) GO TO 15
         IHELP6 = IHELP6 + 1
         RHELP6 = IHELP6
         GVENT6 = RHELP6 * HELP6
         WRITE(NWTRE,14) GVENT6
14       FORMAT(' *** POBO„ˆMOCTˆ ‡AKP›T›X BEHTˆ‹E‰ PˆHˆMAžTCŸ PABH›Mˆ
     =', E12.5, ' [CˆM]***' )
         GO TO 52
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
15       CONTINUE
C!!!-------------------------------------------------------------------
C  CˆCTEMA B›PO†„EHA ”ATA‹œHO...............
         WRITE(NWTRE,10) TN, H0
         WRITE(NWTR ,10) TN, H0
10       FORMAT(//' *** CˆCTEMA B›PO†„EHA  ”_A_T_A_‹_œ_H_O   ***'/
     =          ' *** TN = ', E12.5, '  H0 = ', G12.5, ' ***')
C-->
C----------------------------------------------------------------------
C==>  C”OPMˆPOBATœ CˆCTEM“  „‹Ÿ E—ATˆ....
         CALL GETSYS( MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1 )
C----------------------------------------------------------------------
C-->
       CALL PRINT4( YALL, JALL, LOGYTR )
C-->
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
1        CONTINUE
C===> PE˜ˆTœ CˆCTEM“
         CALL SOLVE0( NDALL, NSYS, YALL, JALL, VALLN1, IRC )
C---> IRC - OKA HE AHA‹ˆ‡ˆP“EM.....
C===> PE˜EHˆE B VALLN1.....
         DO 20 I=1,NSYS
           VALLN1(I) = JALL(I)
20       CONTINUE
C
C--->> C‹A“ PE˜EH›.........
203    CONTINUE
C--->  BOCTAHOBˆTœ CTAPOE GVENT6
         GVENT6 = HELP6
C===> B‹ž—EH ‹ˆ AAPAT FLOG ?
         IF ( FLOG .EQ. 0 ) GO TO 60
C---> OPE„E‹ˆTœ TOKˆ OTKP›T›X BEHTˆ‹E‰... ( B TOKN1 )
         KF = 3
         ZAPROS = 20
         CALL CALLDM(KF, MSWALL, YALL, JALL, LOGYTR,
     =               TOKN, TOKN1, VALLN, VALLN1, KODMD )
C--->
C===> MO„ˆ”ˆ–ˆP“EM PE˜EHˆE COƒ‹ACHO LOGYTR.......
         CALL MODVAL( VALLN1, LOGYTR )
60       CONTINUE
C==> OPE„E‹ˆM HOB›E TOKˆ HA ‹EMEHTAX "OCTOŸHHO‰" CTP“KT“P›.....
         ZAPROS = 2
C===> OPOC BCEX DM...., KPOME ˆ„EA‹œH›X BEHTˆ‹E‰
         KF = 5
         CALL CALLDM(KF, MSWALL, YALL, JALL, LOGYTR,
     =               TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
C--->  Pˆ MITER = 0, KOHTPO‹Ÿ HET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       IF ( MITER .EQ. 0 ) GO TO 61
C--->
C---> ECTœ ‹ˆ ‡APOC› HA "ˆTEPA–ˆˆ"
         IF ( KODMD . EQ. 0 ) GO TO 61
C---> POCŸT ˆTEPA–ˆ‰.....
C======================================================================
C    MAKPOMO„E‹ˆ .TˆA #2 ˆ/ˆ‹ˆ #3   ‡APOCˆ‹ˆ „OABO—H›E
C              ˆTEPA–ˆˆ „‹Ÿ “TO—HEHˆŸ PE„CKA‡AHH›X ‡HA—EHˆ‰ "TOK"
C              B TOM C‹“—AE KODMD ^= 0
C
C        HEOXO„ˆMO:
C OPOCˆTœ DM... MOHˆTOP› C ZAPROS = 1 Pˆ BO‡“†„EHˆˆ FITER = 1 ”‹AƒA;
C DM... MOHˆTOP› B TOM C‹“—AE „O‹†H› ˆCO‹œ‡OBATœ HE PE„CKA‡AHH›E
C ˆMˆ ‡HA—EHˆŸ "TOK", A ‡HA—EHˆŸ ˆ‡ MACCˆBA "TOKN1"
C
C --->  BK‹ž—AEM FITER
         FITER = 1
C--->  ”ˆKCˆP“EM ˆTEPA–ˆž
         NITER = NITER + 1
C--->  OBTOPŸEM, EC‹ˆ TO BO‡MO†HO....
         IF ( NITER .LE.  MITER ) GO TO 52
C--> PˆEXA‹ˆ.....
         WRITE(NWTRE, 62 ) MITER, EITER, TN, H0
62     FORMAT(' *** B MO„E‹ŸX .TˆA 2 ˆ/ˆ‹ˆ 3 ‡A ', I3,' ˆTEPA–ˆ‰ ',
     =     25X, '***'/
     =' *** TO—HOCTœ ', G12.5, ' HE „OCTˆƒH“TA ( TN=', G10.5,
     = ' H0=',G12.5, ' ***')
       WRITE(NWTRE,63) MAXITR, NLITER, NSITER
63     FORMAT(' *** MAX. OƒPE˜HOCTœ ', G12.5,' BHOCˆT ‹EMEHT # ',
     =  I5, ' (CM.#', I3, ') ', 5X, '***' )
C--->  TO OKA ˆƒHOPˆP“EM....!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IRC = 2
C---> DIGITA - MO†ET OBTOPˆTœ PAC—ET Pˆ MEHœ˜EM H0..........
C
C======================================================================
C
61       CONTINUE
C--->  ”‹A†EK, TO CPOCˆM...
         FITER = 0
C
C===> TOKN  ˆ TOKN1 OPE„E‹EH› „‹Ÿ BCEX ‹EMEHTOB...Pˆ T => TN I TN1
C      VALLN  ˆ  VALLN1 ˆ‡BECTH›.....
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C   T_E_C_T_____P_“_H_ƒ_E
C
700    CONTINUE
       GO TO ( 701, 704 ), CASE
C-->  H“†EH ‹ˆ TECT?
C
C***>  Pˆ FRUNGE = 2 --->  TECTˆPOBAHˆE HA KA†„OM ˜AƒE !!!!!!!!!!!!!!!
C
C
       IF ( FRUNGE .EQ. 0 ) GO TO 799
       IF ( FRUNGE .EQ. 2 ) GO TO 710
C-->  Pˆ FRUN0 = 1 ˆ‹ˆ Pˆ H0 > H0OLD  TECTˆP“EM....
       IF ( FRUN0 .EQ. 0  .AND.  H0 .LE. H0OLD ) GO TO 799
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
710    CONTINUE
C-->  ”ˆKC. EPEMEHH›E
       H0OLD = H0
       H1OLD = H1
       H2OLD = H2
       TNOLD = TN
       TN1OLD = TN + H0
C----------------------------------------------------------------------
        KAKA5=5
       IF ( PONRUN .EQ. 1  .AND.  PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5,  TNOLD, TOKN, VALLN )
       IF ( PONRUN .EQ. 1  .AND.  PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5,  TN1OLD, TOKN1, VALLN1 )
C----------------------------------------------------------------------
C-->  ”ˆKC. TO—Kˆ B CTEKE HA KAHA‹E NFILER...
       REWIND NFILER
       WRITE(NFILER) TN1OLD, ( VALLN1(I), I=1,NSYS)
       WRITE(NFILER) TN1OLD, ( TOKN1(I), I=1,NNTOK)
       WRITE(NFILER) TNOLD, ( VALLN(I), I=1,NSYS)
       WRITE(NFILER) TNOLD, (  TOKN(I), I=1,NNTOK)
       END FILE NFILER
C-->  ˆ‡MEHŸEM ˜Aƒ
C-->
       H0 = H0OLD / 2.0
       H1 = H1OLD / 2.0
       H2 = H2OLD / 2.0
C-->  C—ET HOBO‰ TO—Kˆ  „‹Ÿ TN + H0/2, ”ˆKCA–ˆŸ B VALLN1 ˆ TOKN1...
       CASE = 1
       GO TO 100
701    CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  TO—KA „‹Ÿ T = TN + H0/2  ---> B VALLN1 ˆ TOKN1
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  ”ˆKCˆP“EM
       TN = TN + H0
       DO 702 I = 1, NSYS
702    VALLN(I) = VALLN1(I)
       DO 703 I = 1, NNTOK
703    TOKN(I) = TOKN1(I)
C----------------------------------------------------------------------
       IF ( PONRUN .EQ. 1  .AND. PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5, TN,      TOKN, VALLN )
C----------------------------------------------------------------------
C-->  PAC—ET „‹Ÿ T = TN + H0/2 + H0/2  "HOB›‰" B›PˆAHT..
       CASE = 2
       GO TO 100
704    CONTINUE
        CASE = 0
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C   TO—KA  „‹Ÿ  T = TN +H0/2 +H0/2  -->  B VALLN1 ˆ TOKN1
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        TN1X = TN + H0
C----------------------------------------------------------------------
       IF ( PONRUN .EQ. 1  .AND. PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5,   TN1X,  TOKN1, VALLN1 )
C----------------------------------------------------------------------
C--->  B›—ˆC‹ˆM OƒPE˜HOCTœ...
        REWIND NFILER
C-->  —ˆTAEM B JALL  CTAPOE VALLN1...
       READ(NFILER) TXX, ( JALL(I), I=1,NSYS)
C-->  CPABHEHˆŸ
       EPSMAX = 0.0
       IMAX = 0
       DO 714 I = 1, NSYS
       X1 = ABS( VALLN1(I) )
       X2 = ABS( JALL  (I) )
       IF ( X1 .LE. ZEPOCP  .OR.  X2 .LE. ZEROCP ) GO TO 780
       XZ = AMAX1( X1, X2 )
       EPS = ABS(X1 - X2) / XZ
       GO TO 781
780    CONTINUE
C-->  O„HO ˆ‡ —ˆCE‹ ‹ˆ‡KO K H“‹ž.....
       EPS = ABS( X1 - X2 )
781    CONTINUE
       IF ( EPS .LT. EPSMAX ) GO TO 714
C-->  ”ˆKCˆP“EM...
       IMAX = I
       EPSMAX = EPS
714    CONTINUE
C-->           ˆTOƒOB›‰ KOHTPO‹œ....
       IF (  EPSMAX .LE. ERUNGE ) GO TO 705
C-->  TECT HE PO˜E‹.....
       WRITE( NWTRE, 706 ) TNOLD, TN1OLD, IMAX, EPSMAX, ERUNGE
706    FORMAT(' *** HA [', G12.5, ' -:- ', G12.5, ' ]  „‹Ÿ “‡‹A # ',
     =  I5, ' ***'/
     = ' *** ‹OK. OƒPE˜HOCTœ ', G10.5, ' O‹œ˜E ‡A„AHHO‰ ', G10.5,
     =  ' ***' )
C-->CMEHA ˜AƒA ˆ TO—Kˆ, MAKET HOBO‰ TO—Kˆ „‹Ÿ T = TN(CTAPOE) + H0OLD/2
C-->
       DO 707 I=1,NSYS
707    VALLN1(I) = VALLN(I)
       DO 708 I=1, NNTOK
708    TOKN1(I) = TOKN(I)
C-->  BOCTAHOBˆM "CTAP›E" TOKN ˆ VALLN ˆ‡ NFILER_A
C-->  PO“CTˆM ‡AˆCœ O TOKN1
       READ(NFILER)
C-->  BOCTAHAB‹ˆBAEM...
       READ(NFILER) TXX, (VALLN(I), I=1, NSYS )
       READ(NFILER) TXX, (TOKN(I), I=1, NNTOK )
       TN = TNOLD
C--> O„„PEP†ˆM "C‹“†“" BPEMEHˆ....
       HXG = H0
       TXG = TN + H0
C-->  “MEHœ˜ˆM H0DATA....
       H0DATA = H0DATA / 2.0
       HXMIN = HXMIN / 2.0
C--> H1, H0, H2 --> “†E ˆ‡MEHEH›..
       XX = H0DATA * 1.8E+4
       WRITE(NWTRE, 709 ) H0DATA, XX
709    FORMAT(' *** ‡A„AHH›‰ ˜Aƒ H0DATA ˆ‡MEHEH HA ', G12.5,
     = ' (',G10.5,' ) ***')
C-->  KOHTPO‹œ ‡A–ˆK‹.
       NDIVH0 = NDIVH0 + 1
       IF ( NDIVH0 .LE. NRUNGE ) GO TO 710
C-->  PˆEXA‹ˆ.....
       WRITE(NWTRE, 711) NRUNGE, H0DATA
711    FORMAT( /' *** PEB›˜EHO „O. KO‹-BO "„PO‹EHˆ‰" ˜AƒA H0DATA ',
     = I5, ' ***'/
     = ' *** "TECT P“HƒE" OTK‹ž—AETCŸ, PAC—ET BE„ETCŸ C H0DATA = ',
     = G12.5, ' ***'/)
       WRITE(NWTR, 711) NRUNGE, H0DATA
       FRUNGE = 0
       IRC = 3
C-->  MAKET -->  TECT PO˜E‹....
       GO TO 705
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
705    CONTINUE
C-->  TECT PO˜E‹....
C-->  BOCTAHOB‹EHˆE "ƒ‹OA‹œHOE"
       REWIND NFILER
       READ(NFILER) XX, ( VALLN1(I), I=1,NSYS)
       READ(NFILER) XX, ( TOKN1 (I), I=1,NNTOK)
       READ(NFILER) XX, ( VALLN (I), I=1,NSYS)
       READ(NFILER) XX, ( TOKN  (I), I=1,NNTOK)
C-->
       TN = TNOLD
       H0 = H0OLD
       H1 = H1 OLD
       H2 = H2 OLD
       HXG = H0
       TXG = TN + H0
C-->  BCE....
799    CONTINUE
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C==> ›‹A ‹ˆ KOMM“TA–ˆŸ?( EC‹ˆ T5MSW ˆ T6MSW = 0, TO HET HEOXO„ˆMOCTˆ )
C    EPEOPE„E‹ŸTœ TX_G BPEMŸ PEA‹œHOƒO ˜AƒA...)
C
         IF ( T5MSW .EQ. 0  .AND. T6MSW .EQ. 0 ) GO TO 3
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       FKOM = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===> TX_G  HA‰Tˆ...
C--->  †„EM KOM-ž...
         ZAPROS = 3
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
3        CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       GO TO ( 851 ), CASE1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C====>  ›‹A  ‹ˆ  K O M “ T A – ˆ Ÿ ?????
         IF ( FKOM .EQ. 0 ) GO TO 40
C######################################################################
C  O  P A  O T K A   K O M “ T A – ˆ ˆ ..............................
C######################################################################
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  OT 21.10.86.
       IF ( METVET .EQ. 0 ) GO TO 820
       IF ( METVET .EQ. 1 ) GO TO 850
       IF ( METVET .EQ. 2 ) GO TO 880
C@@@ ˆHA—E KAK  = 2, HO E‡ MO„. EPSKOM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       GO TO 890
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
820    CONTINUE
C***** ˆ H T E P  O ‹ Ÿ – ˆ Ÿ   **************************************
C===>  ˆ‡MEHˆTœ COCTOŸHˆE BEHTˆ‹E‰...
         ZAPROS = 4
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
C******> “CTAHOBˆTœ "HEŸBH›‰ ‰‹EP", T.K. B TOM C‹“—AE FKOM = 1
C  Pˆ “C‹OBˆˆ KODMU=0 ...........
         IF ( KODMU .EQ. 1 ) GO TO 54321
         H1 = H0
         H2 = 0.0
54321    CONTINUE
C
C
C
C
C**********************************************************************
C**********************************************************************
C====> "HACTPO‰KA" BEHTˆ‹E‰====>  TO‹œKO „‹Ÿ ˜AƒA T0 <===> T0+H0
C====>  EC‹ˆ TX = T0, TO O›TKA PAC—ETA –Eˆ  OT CTAPO‰
C====> "CTAPTOBO‰" TO—Kˆ, Pˆ TOM COCTOŸHˆŸ BEHTˆ‹E‰ “†E ˆ‡MEHEH›;
C====>  TA O–E„“PA B›O‹HŸETCŸ #HE CTPOƒO# O„ˆH PA‡ „‹Ÿ TN = T0 ˆ
C====>  BCEƒO SEQTIM PA‡ „‹Ÿ TN ^= T0:
C######################################################################
C######################################################################
         IF ( ABS(TXG-T0) .GT. ZEROCP ) GO TO 50
C===>   #OMOƒAEM#  ˆ OBTOPŸEM, EC‹ˆ TO Pˆ T  = TN
         TXG = T0 + H0
         HXG = H0
         GO TO 52
50       CONTINUE
C!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( HXG .NE. 0.0 ) GO TO 55
C---> "P“KCOB›BAHˆE" !!!COO™EHˆE ‹OKˆPOBAHO (13.01.87)!!!
         TN1 = TN + H0
C        WRITE(NWTRE, 56 ) TN1,     TN
56       FORMAT(            ' *** Pˆ TN1=', G12.5, ' "CPOC" B "CTAPTOB
     =“ž" TO—K“ TN=',G12.5,' ***')
         IHELPV = IHELPV + 1
         TXG = TN + H0
         HXG = H0
         IF ( IHELPV .LE. SEQTIM ) GO TO 52
C-@@@@@@@
         WRITE(NWTRE, 57) SEQTIM, TN, METVET
         WRITE(NWTR,  57) SEQTIM, TN, METVET
57    FORMAT(//' *** B›O‹HEHO ', I5,' "CPOCOB" PAC—ETA B "CTAPTOB“ž"',
     =  ' TO—K“ TN=', G12.5, ' ***'/
     = ' *** PAC—ET PEP›BAETCŸ -> METVET=', I3, '     ***'///)
         CALL SUBERR(0)
55       CONTINUE
C!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C**********************************************************************
C@@@@
       IF ( METVET .GE. 2 ) GO TO 856
C          @@@@@@@@@@@@@@@
C----------------------------------------------------------------------
C**********************************************************************
C===>   ˆHTEPO‹Ÿ–ˆŸ [TOKN]  <=> [TOKN1]  B  [TOKN1]
C===>   ˆHTEPO‹Ÿ–ˆŸ [VALLN] <=> [VALLN1] B  [VALLN1]
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( IPOL .EQ. 0 ) GO TO 900
C-->> CE–. ˆHTEPO‹. ˆ‡ TN <-> TN1 B TXG ( XPAHˆTœ B "N1" )
       D1X = (H1 - HXG) / H0
       D2X = (H2 - HXG) / H0
       DO 901 I = 1, NSYS
       VN1 = VALLN1(I)
       VN  = VALLN (I)
       VALLN1(I) = D1X * VN1 + D2X * VN  - HXG * ( VN1 - VN )
901    CONTINUE
C---
       DO 902 I=1, NNTOK
       TTN1 = TOKN1(I)
       TTN  = TOKN (I)
       TOKN1(I) = D1X * TTN1 + D2X * TTN - HXG * ( TTN1 - TTN )
902    CONTINUE
       GO TO 903
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
900    CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  KOHTPO‹œ ˆHTEPO‹Ÿ–ˆˆ ...
       IF ( HXG .GT. H0 ) GO TO 800
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C*******>DTX = (TX - TN ) / H0
         DTX = HXG / H0
         DO 4 I = 1, NSYS
         VN = VALLN(I)
         DVN1 = VALLN1(I) - VN
C******** BO‹EBOE PE˜EHˆE:
         IF ( ABS(DVN1) .LT. ZEROCP ) DVN1 = 0.0
         VALLN1(I) = VN + DVN1 * DTX
4        CONTINUE
C----------------------------------------------------------------------
         DO 5 I=1,NNTOK
         TTN = TOKN(I)
         DTN1 = TOKN1(I) - TTN
C******** BO‹EBOE PE˜EHˆE:
         IF ( ABS(DTN1) .LT. ZEROCP ) DTN1 = 0.0
         TOKN1(I) = TTN + DTN1 * DTX
5        CONTINUE
C*******BCE********
903    CONTINUE
802    CONTINUE
C!!!-------------------------------------------------------------------
         TN1 = TN + HXG
         IF ( PON .EQ. 1  .AND. PPVENT .EQ. 1 )
     =                     CALL DRPR01( 4, TN1, TOKN1, VALLN1 )
C!!!-------------------------------------------------------------------
         RETURN
40       CONTINUE
C===> KOM“TA–ˆˆ HE ›‹O....
C     TOKN1  ˆ   VALLN1  „‹Ÿ  T  =  TN + HXG  OPE„E‹EH›
         RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
800    CONTINUE
       TXG = TN + HXG
       WRITE(NWTRE,801)  TN, H0, TXG, HXG, METVET
801    FORMAT( ' *** ABAPˆŸ B ‹OKE ˆHTEPO‹Ÿ–ˆˆ...[GETNEW]:',
     =  5X, '(METVET=', I2, 10X, ' ***'/
     = ' *** TN=',E12.5,' H0=',E12.5,' TXG=',E12.5,' HXG=',E12.5,
     = ' ***'  )
       GO TO 802
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
850    CONTINUE
C@@@ EPEC—ET....
C@@  COCTOŸHˆE HE MEHŸEM, A C—ˆTAEM C H0 = HXG.....
       IF ( HXG .EQ. 0.0 ) GO TO 820
       CASE1 = 1
       H0M1 = H0
       H0 = HXG
       TXG = TN + H0
       GO TO 52
C-->  O CASE1 ....
851    CONTINUE
       H0 = H0M1
C-->  EPEC—ˆTA‹ˆ K TO—KE TN+HXG ....
       IF ( FKOM .EQ. 1 ) GO TO 852
C***
       WRITE(NWTRE, 85344 ) TN, HXG, METVET, HXMIN, EPSKOM
85344  FORMAT(' *** PO“CK KOMM“TA–ˆˆ TN=', G12.5, ' HXG=',G12.5,
     = ' ***'/
     = ' *** METVET=', I3, ' HXMIN=',G12.5, ' EPSKOM=', G12.5, ' ***')
       GO TO 853
C-->  ˆ‡MEHˆM COCTOŸHˆE BEHTˆ‹E‰...
852    CONTINUE
C=
         ZAPROS = 4
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
853    CONTINUE
C-->  E—ATœ "KOMM“TA–ˆˆ" :::
       TN1 = TN + HXG
       IF ( PON .EQ. 1 .AND. PPVENT .EQ. 1 )
     =                 CALL DRPR01( 4, TN1, TOKN1, VALLN1 )
       CASE1 = 0
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
880    CONTINUE
C-->  "C‹ˆAHˆE"
C-->  HOB›‰ EPSKOM....
       EPSOLD = EPSKOM
C-->   EPSKOM = (TN+H0) - (TN+HXG)
       EPSKOM = H0 - HXG
       IF ( EPSKOM .LT. 0.0 ) WRITE(NWTRE,8805) TN, H0, HXG, EPSKOM
8805   FORMAT(' *** TN=',G12.5,' H0=',G12.5,' HXG=',G12.5,
     =         ' EPSKOM=', G12.5, ' ***')
885    CONTINUE
C===>  ˆ‡MEHˆTœ COCTOŸHˆE BEHTˆ‹E‰...
         ZAPROS = 4
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
       EPSKOM = EPSOLD
C-->  „A‹EE TAK †E KAK „‹Ÿ METVET=0    KPOME ˆHTEPO‹Ÿ–ˆˆ..
       GO TO 50
856    CONTINUE
C-->  B "N1" PE‡“‹œTAT ... E—ATAEM...
       TN1 = TN + HXG
       IF ( PON .EQ. 1 .AND. PPVENT .EQ. 1 )
     =      CALL DRPR01( 4, TN1, TOKN1, VALLN1 )
C@@@@@ C ‹ ˆ  A H ˆ E  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       HXG = H0
       TXG = TN + HXG
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
890    CONTINUE
C-->  E‡ MO„. EPSKOM_A...
       EPSOLD = EPSKOM
       GO TO 885
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C@C‹A“@ ---------------------------------------------------------------
201    CONTINUE
C-->  C‹A“ C ˆTEP. “TO—.
C  YALLHP  --> ˆCXO„HAŸ, YALL - „EKOMO‡ˆPOBAHAŸ...
C
C-->  XPAHˆM...
       DO 205 I=1,NSYS
       DO 206 J=1,NSYS
       IDHH = ID(I,J)
       YALLHP(IDHH) = YALL(IDHH)
206    CONTINUE
205    CONTINUE
C-->  YALL = L * U,  PER -> WORKY
       CALL FACTR( YALL, WORKY, NSYS, NDALL, IERFAC )
C
       IF ( IERFAC .EQ. 0 ) GO TO 207
C-->  TO—HOE B›PA†„EHˆE
       WRITE(NWTRE, 2000) TN
2000   FORMAT(' *** YALL HE ”AKTOPˆ‡“ETCŸ TN=',G12.5, ' ***' )
C
       GO TO 202
207    CONTINUE
C-->  PO“EM PE˜ATœ..
       EPSXXX = EPSSYS
       CALL RSLMC(YALLHP, YALL, JALL, VALLN1, NSYS, EPSXXX,
     =            IERRSL, NDALL, WORKX, WORKY )
C
C-->  ???
       IF ( IERRSL .EQ. 0 ) GO TO 203
C
       GO TO (211, 212, 213, 214 ), IERRSL
       CALL SUBERR(IERRSL )
211    CONTINUE
C-->  TO—HA TO‹œKO HOPMA..
C@@@@@@WRITE(NWTRE,2001)     TN
2001   FORMAT(' *** B C‹A“ TO—HOCTœ -> TO‹œKO „‹Ÿ',
     =  ' HOPM› PE˜EHˆŸ TN=',G12.5,  ' ***' )
C-->  ˆƒHOPˆP“EM..
       GO TO 203
212    CONTINUE
C-->  TO—HOCTœ HOPM› Hˆ†E ‡A„.
       WRITE(NWTRE,2002)         EPSXXX, TN
2002   FORMAT(' *** B C‹A“',
     =  ' PE˜EHˆE C TO—HOCTœž =',G12.5, ' TN=',G12.5,   ' ***' )
C-->  IGNORE
       GO TO 203
213    CONTINUE
C-->  HET CM›C‹A..
       WRITE(NWTRE,2003) TN
2003   FORMAT(' *** PE˜EHˆE C‹A“ HE ˆMEET CM›C‹A TN=', G12.5, ' ***' )
       GO TO 202
C
214    CONTINUE
C-->  YALL = L*U  -->  B U --> 0-‹œ B „ˆAƒOHA‹E...
       WRITE(NWTRE,2004) TN
2004   FORMAT(' *** C‹A“ -> H“‹ˆ B „ˆAƒOHA‹ˆ [U] TN=',G12.5, ' ***')
       GO TO 202
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         END
