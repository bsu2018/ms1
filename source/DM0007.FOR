         SUBROUTINE DM0007 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0007 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰ ˆ
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA ‡HA—EHˆ‰ APAMETPOB
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTA –Eˆ
C
C                      Pˆ KODMUM = 1 --> BCEƒ„A H1 ˆ H2 B›ˆPAžTCŸ
C                                         COƒ‹ACHO MU APAMETP“, A HE
C                                         O TEK“™ˆM H1 ˆ H2
C
C
C                   ‹EKTPˆ—ECKAŸ  MA˜ˆHA 3X3X0:
C                   ****************************
C                  Tˆ # 11  --> "P“—. BAP.", MB(_T_)
C                  Tˆ # 110 --> "P“—. BAP.", MB(_OMEƒA_)
C                  Tˆ # 14  --> "P“—. BAP.", MB(_T_) - M_FT(#)
C                  Tˆ # 140 --> "P“—. BAP.", MB(_OMEƒA_) -  M_FT(#)
C                  Tˆ # 12  --> "REDUCE."  , MB(_T_)
C                  Tˆ # 120 --> "REDUCE."  , MB(_OMEƒA_)
C                  ****************************************************
C                            „‹Ÿ TˆOB # 11 ˆ # 12 ˆ 14:
C                            ----------------------
C                   M_BHE˜(T) - ‡A„AETCŸ K“CO—HO-‹ˆHE‰HO‰ ”“HK–ˆE‰
C                               —EPE‡ MACCˆB MAS < T1 Y1 .... >
C                                ( CM. / KMAS02 );
C
C                      „‹Ÿ TˆOB # 110 ˆ # 120 ˆ 140:
C                     -------------------------
C              M_BHE˜(_OMEGA_) ‡A„AETCŸ KO”. O‹ˆHOMA:
C              MAS < A0 A1 A2 ...>;
C
C                      MB := A0 + A1 * OMEGA + A2 * OMEGA**2 + ....
C
C
C                            „‹Ÿ TˆOB # 11 ˆ 110:
C                            ---------------------
C                  ‹EMEHT OˆC›BAETCŸ:
C            ?? T 11 “ #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                      RS RF LS  MS  LR  MR  MSR  PTAU GD**2
C                       MAS < T1 Y1 ....   > ;
C
C                     [ "P“—HO‰" B A P ˆ A H T ]
C
C                            „‹Ÿ TˆOB # 14 ˆ 140:
C                            ---------------------
C                  ‹EMEHT OˆC›BAETCŸ:
C            ?? T 11 “ #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                      RS RF LS  MS  LR  MR  MSR  PTAU GD**2  #
C                       MAS < T1 Y1 ....   > ;
C    # - ‡HA—EHˆE  MF_T TOƒO ‹-TA B›—ˆTAETCŸ ˆ‡ M_BHE˜.
C
C                     [ "P“—HO‰" B A P ˆ A H T ]
C
C                            „‹Ÿ TˆOB # 12 ˆ 120:
C                            ---------------------
C                  ‹EMEHT OˆC›BAETCŸ:
C            ?? T 11 “ #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                      RS RF LVS LVF MSS MFF     PTAU GD**2
C                       MAS < T1 Y1 ....   > ;
C
C@
C
C               *** BO‡MO†H› ˆTEPA–ˆOHH›E “TO—HEHˆŸ ***:
C                   Pˆ BO‡“†„EHˆˆ FITER [ MASPRT(65) ] --->
C                   ‡HA—EHˆŸ "TOK"_Ÿ - POƒHO‡ ŸBH›M METO„OM -
C                   HE B›—ˆC‹ŸžTCŸ, A B›ˆPAžTCŸ ˆ‡ MACCˆBA TOKN1;
C               ***
C                  B) ZAPROS = 2
C                     ----------
C                  B›—ˆC‹EHˆE ‡HA—EHˆ‰ TOKOB B BETBŸX B T(N+1) MOMEHT
C                  BPEMEHˆ   ˆCXO„Ÿ ˆ‡ G ˆ J APAMETPOB ˆ ‡HA—EHˆŸ
C                  “‡‹OB›X HAPŸ†EHˆ‰ HA ‹EMEHTE PAC—ˆTAHH›X „‹Ÿ T(N+1)
C                    ˆ ‡AˆCœ (”ˆKCA–ˆŸ) TˆX ‡HA—EHˆ‰ B "TOK T(N+1)";
C                 - Pˆ TOM KOHTPO‹ˆP“žETCŸ "PACXO†„EHˆE" ME†„“
C                   ‡HA—EHˆŸMˆ, O‹“—EHH›Mˆ O ŸBHO‰ CXEME (POƒHO‡) ˆ
C                   OPE„E‹EHH›Mˆ ˆCXO„Ÿ ˆ‡ B›—ˆC‹EHH›X ‡HA—EHˆ‰ VALLN1;
C                   KOHTPO‹œ BE„ETCŸ O HE PEB›˜EHˆž EITER[MASPRT(67)]
C                   OTHOCˆTE‹œHO‰ OƒPE˜HOCTˆ. B HE ‹AƒOPˆŸTHOM C‹“—AE
C                   B "MAXITR" ‡AˆC›BAETCŸ MAX_OƒPE˜HOCTœ ˆ
C                   B "NLITER" ”ˆKCˆP“ETCŸ HOMEP ‹EMEHTA.....
C                   APAMETP "KOD" = 1;
C
C
C                  C) ZAPROS = 3
C                     **********
C                     HE O„„EP†ˆBAETCŸ ( PE‡EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE O„„EP†ˆBAETCŸ;
C
C   METO„:         „ˆCKPETˆ‡A–ˆŸ “PABHEHˆ‰ COCTOŸHˆŸ MO„E‹ˆ „AHHOƒO
C   -----          TˆA HA OCHOBE METO„A ‹ˆHˆ„†EPA BˆO‹“ƒˆ:
C                  Y(N+1) = Y(N) + H1*F(N+1) + H2*F(N).
C
C   APAMETP›:     MSW, RMSW - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------      ( "OTPE‡OK" MACCˆB› MSWALL -> C.M. / GETMSW );
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          B T = TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          B T = TN + H0;
C                  TOKN  ˆ  TOKN1  -  ‡HA—EHˆŸ "TOKOB" Pˆ T :=
C                                     TN  ˆ  TN + H0;
C                                   PA‡MEPHOCTˆ CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 5;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C                  KOD - APAMETP OPATHO‰ CBŸ‡ˆ C MOHˆTOPOM DM....
C                   ( =1 Pˆ O‹œ˜O‰ ‹OK. OƒPE˜HOCTˆ );
C
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;  + PRINT5 - OT‹A„KA;
C   ------------      DEPOSA, ME1N13, ME2N13, ME3N13,
C                     KMAS01, KMAS02,  MA1N13, MA2N13, MA3N13;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=   1 ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C                3. Pˆ FTEST=-1 E—ATœ OT‹. ˆH”. —EPE‡ PRINT5;
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 11, 110, 12, 120:
C                ----------------------------------------------
C                      +  14  ˆ 140:
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. IHA—.   I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C  +--------------------------------------------------------------+
C  I “‡‹› O„K‹ž—EHˆŸ S1N S2N S3N F1N F2N F3N S1K S2K F1K F2K F3K I
C  I           12-Tœ C‹OB: OT 9 „O 20                             I
C  +--------------------------------------------------------------+
C
C              RMSW Tˆ # 11, 110, 12, 120 [ ‹. MA˜ˆHA 3X3X0 ] :
C              ************************************************
C         + 14  ˆ 140
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PE‡EPB I"DOM"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 3,4,5  I „‹Ÿ I      I      I       I       I       I       I     I
C  I        IMEX. I 9-Tœ I 9-Tœ I 36-Tœ I 36-Tœ I 6-Tœ  I 6-Tœ  I NMASI
C  I        I“P-Ÿ I C‹OB I C‹OB I C‹OB  I C‹OB  I C‹OB  I C‹OB  I C‹OBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I ****   I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - OPE„E‹ŸETCŸ PA‡MEPOM MAS <...> ==> ...+ NMAS + 1
C            *** - 22 - A„PEC B TOKN/N1 ‡HA—EHˆŸ MFT „‹Ÿ # (Tˆ 14,140)
C
C
C
C--->  D@ -> HOMEPA O‹E‰ B MSW_ALL MACCˆBA;
C
C
C
C
C
C      „‹ˆHA MSW_ALL ==> D8 + NMAS + 1; (+1 - HA „‹ˆH“ TMAS );
C
C      „‹ˆHA "EPEMEHHO‰" —ACTˆ ==> DR8 + NMAS + 1;
C
C      PAC˜ˆ”POBKA MACCˆBOB PARD ˆ T.„. CM. B / ME@N13 ... ˆ FM0008;
C
C     B MACCˆBE TOKN ˆ TOKN1 CO„EP†ˆTCŸ:
C
C    OT 1 „O 6   - ‡HA—EHˆŸ TOKOB ”A‡ OMOTOK: S1 S2 S3 F2 F2 F3;
C
C       7        -  ‡HA—EHˆE GAMMA -> “ƒO‹ O‹O†EHˆŸ POTOPA;
C
C       8        -  ‡HA—EHˆE OMEGA -> “ƒ‹OBAŸ —ACTOTA BPA™EHˆŸ POTOPA
C                                     ( B O„HOEPˆO„HO‰ MO„E‹ˆ );
C       9        -  ‡HA—EHˆE MFT - ‹. MAƒ. MOMEHTA;
C
C      10        -  ‡HA—EHˆE M_BHE˜(T) - BHE˜Hˆ‰ HAƒP“‡O—H›‰ MOMEHT;
C
C  ”OPM“‹›:
C  ********
C   “HKT A) ( E‡ ”‹AƒA ):
C            O „AHH›M ˆ‡ TOKN ˆ VALLN:
C            -1                          D L(N)
C   J0 = L(N)  *[ U(N) - ( R + OMEGA(N) *-------  ) * TOKN ]     (B MSW)
C                                        D GAMMA
C   MFTN = ˆ‡ TOKN
C
C   MBHN = ˆ‡ TOKN
C
C   DOMN = [PTAU/JMOM] * ( MBHN - MFTN )    (B MSW)
C
C   TOKN_Y(ŸBH›‰) = TOKN + H0 * J0    (B TOKN1)
C
C   GAM_Y(ŸBH›‰)  = GAM_N + H0 * OMEG_N  (B TOKN1)
C
C   OMEG_Y(ŸBH›‰) = OMEG_N + H0 * DOMN   (B TOKN1)
C
C
C   “HKT A) ( PO„O‹†EHˆE, ˆ‹ˆ Pˆ ”‹AƒE, TO‹œKO TO):
C   O „AHH›M ˆ‡ TOKN1, MSW ˆ TOKN:
C
C   MFTN_1 = F(GAMN_1, TOKN_1) ˆ ‡AˆCœ B TOKN1
C
C   MBHN_1 = HE C—ˆTAETCŸ*** SUB( VALLN, TOKN_1, (TN+H0), ...)
C
C   DOMN_1 = HET **** [PTAU/JMOM] * ( MBHN_1 - MFTN_1 )
C
C   GAMN_1 = ˆ‡ TOKN1   GAMN + H1 * OMEGN_1 + H2 * OMEGN
C
C   OMEGN_1 = ˆ‡ TOKN1   OMEGN + H1 * DOMN_1 + H2 * DOMN
C
C   G0 = [ L(N+1) + H1*R + OMEGN_1 * D(L(N+1))/D(GAM) ] ** (-1)
C
C   GY = H1 * G0
C
C   G1 = G0 * L(N+1)
C
C   JY = G1 * [ TOKN + H2 * J0 ]
C
C------
C
C   “HKT B) -->  PAC—ET TOKN1 ˆ KOHTPO‹œ:
C   O „AHH›M ˆ‡ TOKN1, MSW, VALLN1:
C
C   TOKX = GY * UL(N+1) + JY;  UL--> VALLN1(*) - VALLN1(*);
C
C   MFTX = F( GAMN_1, TOKX )
C
C   MBHX = SUB( VALLN1, TOKX, (TN+H0), ...) „‹Ÿ TˆOB 13 ˆ 130
C                                           "-" MFT_„‹Ÿ #.
C   DOMX = [PTAU/JMOM] * ( MBHX - MFTX )
C
C   OMEGX = OMEGN + H1 * DOMX + H2 * DOMN
C
C   GAMX =  GAMN +  H1 * OMEGX + H2 * GAMN
C
C   OˆCK  EPS:= [MAX] -> ABS(TOKX - TOKN1)/MAX(TOKX,TOKN1)
C
C   Pˆ  EPS > MAXITR --> ”ˆKCA–ˆŸ CˆT“A–ˆˆ: NLITER, NSITER,...
C                         KOD = 1
C                         *******
C
C   ”ˆKCA–ˆŸ B TOKN1 ‡HA—EHˆ‰ TOKX, .....
C
C
CZZZ-------------------------------------------------------------------
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      DIMENSION G12(12,12), J12(12)
C      REAL J12
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         REAL JALL, ME3N13, MA3N13
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION UL(6),  TOKX(6)
         INTEGER ADRMSW
         INTEGER D1,D2,D3,D4,D5,D6,D7,D8
         INTEGER FTEST
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( FTEST,  MASPRT(60) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( MU,     MASPRT(31) )
         EQUIVALENCE ( H1OLD,  MASPRT(32) )
         EQUIVALENCE ( H2OLD,  MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( KODMUM, MASPRT(114) )
         INTEGER FITER,  ZAPROS, TIP
         REAL MAXITR, MFTN, MBHN, KMAS02, MFTX, MBHX, KMAS01, MU, MFTN1
         DATA KAKA9 / 9 /
C**********************************************************************
       DATA D1, D2, D3, D4, D5, D6,  D7,  D8
     =  /   21, 27, 36, 45, 81, 117, 123, 129 /
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C   BE‹ˆ—ˆH›, ‡ABˆCŸ™ˆE OT KOHKPETHO‰ MO„E‹ˆ:
C 1) BCE D1,....  CM. B›˜E;
C 2) PA‡MEPHOCTˆ PA. MACCˆBOB ( H“†H› KXALL, A HE KALL):
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C 3) PA‡MEPHOCTœ —ACTˆ "GY", "JY" KXALL - KOTOP“ž BK‹A„›BAEM:
       DATA KXALL/6/
C 4) "O‹H›E" PA‡MEPHOCTˆ GY ˆ JY:
       DATA KALL/6/
C 5) BCOM. “KA‡ATE‹ˆ-> CME™EHˆE B MSW „‹Ÿ COOTB. MACCˆBOB
C    ( HA "1" MEHœ˜E COOTB. D1, D2,....):
       DATA IPARD, IPARM, IPARGY, IPARG1, IPARJY, IPARJ0, ITMAS
     =    /  26,    35,    44,     80,      116,    122,   128 /
C 6) “KA‡ATE‹œ HA "DOM" C‹OBO:
       DATA IDOM/26/
C 7) CME™EHˆE B TOKN/TOKN1 HA "MEXAHˆ—ECKˆE" EPEMEHH›E:
       DATA IGAM, IOMEG, IMFT, IMBH
     =    /  +6,   +7,    +8,   +9  /
C 8) “KA‡ATE‹œ HA [PTAU/JMOM] OTHOCˆTE‹œHO PARM( OT CME™EHˆŸ D3 ):
       DATA IPTJ/+8/
C--->  ”“HK–ˆˆ "„B“XMEPH›X" MACCˆBOB:
       IDGY(I,J) = IPARGY + (J-1)*KALL + I
       IDG1(I,J) = IPARG1 + (J-1)*KALL + I
       IDYALL(I,J) = (J-1) * NDALL + I
C      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       TIP = MSW(2)
C--
       IF ( TIP .EQ. 11  .OR. TIP .EQ.110.OR.TIP .EQ.14.OR.TIP.EQ.140
     =  .OR.  TIP .EQ. 12  .OR. TIP .EQ. 120       ) GO TO  99990
       GO TO 9999
99990  CONTINUE
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C--->  B›OP H1  ˆ H2 --->
       H1 = H1OLD
       H2 = H2OLD
       IF ( KODMUM .EQ. 0 )  GO TO 3000
       H1 = H0 * ( 1.0 - MU )
       H2 = H0 * MU
3000   CONTINUE
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C-->  “KA‡-‹œ HA TOK...
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
C--->  B›OP PE†ˆMA ‡APOCA..
       GO TO( 100, 200), ZAPROS
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C-->  A„EHˆE HAPŸ†EHˆŸ:
       DO 102 I=1,KALL
       IF ( I .GT. KXALL ) GO TO 1020
C-->  “‡‹› PEA‹œH›E, HE „EMP”EPH›E...
       K = MSW( I + 8 )
       M = MSW( 8 + KXALL + I )
       VK = 0.0
       IF ( K .NE. 0 ) VK = VALLN(K)
       VM = 0.0
       IF ( M .NE. 0 ) VM = VALLN(M)
       UL(I) = VK - VM
       GO TO 102
1020   CONTINUE
C-->  „EM”EPH›E “‡‹›...
       UL(I) = 0.0
102    CONTINUE
C--->
C-->  HE POCŸT ‹ˆ ˆTEPA–ˆˆ ?
       IF ( FITER .EQ. 1 ) GO TO 101
C-->  “HKT A) --- HA—A‹O -----(E‡ ”‹AƒA)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66666) ( IQ, RMSW(IQ), IQ=D1,D8 )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C66666  FORMAT(1X,5(1X,'R(',I3,')=', G12.5 ) )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  PAC—ET J0:
       GAMMA = TOKN(ITOK+IGAM)
       OMEGA = TOKN(ITOK+IOMEG)
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     = CALL ME2N13( RMSW(D2), RMSW(D3), OMEGA, GAMMA,
     =              TOKN(ITOK), UL, RMSW(D7) )
       IF ( TIP.EQ.11.OR.TIP.EQ.110.OR.TIP.EQ.14.OR.TIP.EQ.140 )
     = CALL MA2N13( RMSW(D2), RMSW(D3), OMEGA, GAMMA,
     =              TOKN(ITOK), UL, RMSW(D7) )
C??????????????????????????????????????????????????????????????????????
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66666) ( IQ, RMSW(IQ), IQ=D1,D8 )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  MFT_N  ˆ  MBH_N
C*****
C      ˆMEET CM›C‹ EPEC—ˆTATœ, T.K. B C‹. ˆHTEP. TOK ˆ‡MEH.
C     MFT -->  MEH. O—EHœ Cˆ‹.
         MFTN = TOKN(ITOK+IMFT)
C      #########################
C@@    IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 )
C@@  =     MFTN = ME3N13( RMSW(D2), GAMMA, TOKN(ITOK) )
C@@    IF ( TIP.EQ.11.OR.TIP.EQ.110 .OR. TIP.EQ.14.OR.TIP.EQ.140)
C@@  =     MFTN = MA3N13( RMSW(D2), GAMMA, TOKN(ITOK) )
C-->
C66667  FORMAT('  MFTN=',G12.5,' MBHN=',G12.5,' G=',G12.5,' OM=',G12.5)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C   MBHN -->  MO†HO B‡ŸŸTœ ˆ‡ TOKN,
C   MO†HO EPEC—ˆTATœ „‹Ÿ “—ETA F(OMEGA)
C***
C##    IF ( TIP .EQ. 11  .OR.   TIP .EQ. 12  .OR. TIP  .EQ. 14 )
C##  = MBHN = KMAS02( TN, RMSW(D8) )
C##
C##    IF ( TIP .EQ. 110  .OR.   TIP .EQ. 120  .OR. TIP .EQ. 140 )
C##  = MBHN = KMAS01(OMEGA,  RMSW(D8) )
C##
C@@@@@ TOKN(ITOK+IMFT) = MFTN
C###   TOKN(ITOK+IMBH) = MBHN
       MBHN = TOKN(ITOK + IMBH )
C      #########################
C
C??????????????????????????????????????????????????????????????????????
C-->  DOM_N -> MEX. XAP-KA..
       DOMN = RMSW(D3+IPTJ) * ( MBHN - MFTN )
       RMSW ( IDOM ) = DOMN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66667) MFTN, MBHN, GAMMA, OMEGA
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C--->  POƒHO‡ "ŸBH›X" ‡HA—EHˆ‰
       DO 103 I=1,KALL
       TOKN1( ITOK1 + I ) = TOKN(ITOK1+I) + H0*RMSW(IPARJ0+I)
103    CONTINUE
       TOKN1(ITOK + IGAM ) = GAMMA + H0 * OMEGA
       TOKN1(ITOK + IOMEG ) = OMEGA + H0 * DOMN
C----------------------------------------------------------------------
C-->  “HKT A) C ”‹AƒOM....
101    CONTINUE
C**********************************************************************
C
C  -->  "ŸBH›E" ˆ‹ˆ  "ˆTEPA–ˆOHH›E" GAMMA ˆ OMEGA-->  OMEG1 ˆ GAM1
       OMEG1 = TOKN1( ITOK + IOMEG )
       GAM1  = TOKN1( ITOK + IGAM  )
C**** EPEC—ET MFT „‹Ÿ O„„„EP†Kˆ TˆOB 14 ˆ 140:::>
C   „‹Ÿ ‡HA—EHˆ‰ GAM1 ˆ TOKN1  PAC—ET MFTN1 ˆ ‡AˆCœ EƒO B TOKN1:
C
C      #########################
       IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 )
     =     MFTN1 = ME3N13( RMSW(D2), GAM1,  TOKN1(ITOK) )
       IF ( TIP.EQ.11.OR.TIP.EQ.110 .OR. TIP.EQ.14.OR.TIP.EQ.140)
     =     MFTN1 = MA3N13( RMSW(D2), GAM1,  TOKN1(ITOK) )
       TOKN1(ITOK + IMFT ) = MFTN1
C    ############
C-->  GAMMA  ˆ  OMEGA  B TO—EK  TN--->
       OMEGA = TOKN( ITOK + IOMEG )
       GAMMA = TOKN( ITOK + IGAM  )
C--> O  „AHH›M POƒHO‡A  HAXO„ˆM  GY  ˆ  G1 --->
C
C-->  PAC—ET "O‹H›X" ( T.E KALL HA KALL ) GY ˆ G1....
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     = CALL ME1N13( RMSW(D2), RMSW(D3), H1, OMEG1, GAM1, RMSW(D4),
     =                 RMSW(D5) )
       IF ( TIP .EQ. 11  .OR. TIP .EQ. 110 .OR.TIP.EQ.14.OR.TIP.EQ.140)
     = CALL MA1N13( RMSW(D2), RMSW(D3), H1, OMEG1, GAM1, RMSW(D4),
     =                 RMSW(D5) )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66666) ( IQ, RMSW(IQ), IQ=D1,D8 )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  PAC—ET JY...
       IF ( H2 .EQ. 0.0 ) GO TO  104
C-->  H2 HE 0.0,  “—ET J0...
       DO 105 I=1,KALL
       TOKX(I) = TOKN(ITOK1+I) + H2 * RMSW(IPARJ0+I)
105    CONTINUE
C-->  JY = G1 * TOKX
       DO 106 I=1,KALL
       SUM = 0.0
       DO 107 J=1,KALL
       INDEX = IDG1(I,J)
       SUM = SUM + RMSW(INDEX) * TOKX(J)
107    CONTINUE
       RMSW(IPARJY+I) = SUM
106    CONTINUE
       GO TO 1070
C**********************************************************************
104    CONTINUE
C-->  H2 = 0.0,  „OABKˆ HET
       DO 108 I=1,KALL
       SUM = 0.0
       DO 109 J = 1,KALL
       INDEX = IDG1(I,J)
       SUM = SUM + RMSW(INDEX) * TOKN(ITOK1 + J)
109    CONTINUE
       RMSW(IPARJY + I ) = SUM
108    CONTINUE
C----------------------------------------------------------------------
1070   CONTINUE
C
C   GY   ˆ  JY   OPE„E‹EH›....
C---  BK‹A„›BAEM ----
       IIX = KAKA9 + KXALL
       CALL DEPOSA( MSW(KAKA9), MSW(IIX), KXALL, KALL,RMSW(D4),RMSW(D6),
     =              YALL,  JALL )
C
C--->   ˆ‡ GY ˆ JY  O‹“—ˆM "PA‡BEPH“T›E" MATPˆ–›--->
C$$$$$$CALL MOT005( RMSW(D4), G12,  RMSW(D6), J12 )
C-->  B‹O†ˆM B P“—H“ž--->******
C
C   BK‹A„›
C        DO  552 I=1,12
C        DO  553 J1=1,12
C        IJ1XX = IDYALL( MSW(8+I), MSW(8+J1) )
C****
C553      YALL(        IJ1XX       )=YALL( IJ1XX )
C    =                              + G12( I, J1 )
C552      JALL(MSW(8+I))=JALL(MSW(8+I)) - J12(I)
C                                      $  -------- !!!
C@@@@
       KOD = 0
       IF ( FTEST .NE. -1 ) RETURN
C--->  OT‹A„O—HAŸ E—ATœ:
       CALL PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
       RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C---- ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  A„EHˆE HAPŸ†EHˆŸ-->  HOBOE
       DO 201 I=1,KALL
       IF ( I .GT. KXALL ) GO TO 2010
C-->  “‡‹› PEA‹œH›E, HE „EMP”EPH›E...
       K = MSW( I + 8 )
       M = MSW( 8 + KXALL + I )
       VK = 0.0
       IF ( K .NE. 0 ) VK = VALLN1(K)
       VM = 0.0
       IF ( M .NE. 0 ) VM = VALLN1(M)
       UL(I) = VK - VM
       GO TO 201
2010   CONTINUE
C-->  „EM”EPH›E “‡‹›...
       UL(I) = 0.0
201    CONTINUE
C-->  HA‰„EM TOK  O  VALLN1...
       DO 202 I=1,KALL
       SUM = RMSW(IPARJY + I )
       DO 203 J=1,KALL
       INDEX = IDGY(I,J)
       SUM = SUM + RMSW(INDEX)*UL(J)
203    CONTINUE
       TOKX(I) = SUM
202    CONTINUE
C--->  MFT  ˆ  MBH ...
C  -->  "ŸBH›E" ˆ‹ˆ ˆTEP. ‡HA—EHˆŸ „‹Ÿ TN1 ( PE„CKA‡AHˆE) -->
       GAM1 = TOKN1(ITOK + IGAM)
       OMEG1 = TOKN1(ITOK + IOMEG)
C######################################################################
C   „‹Ÿ TˆA 14/140 B›OP MFTN1  ˆ‡ TOKN1(#) „‹Ÿ “—ETA B MBH(TN1)
       IF ( TIP.EQ.14.OR.TIP.EQ.140) MFTN1=TOKN1( MSW(D1+1) )
C######################################################################
C--->   ‡HA—EHˆŸ  GAMMA ˆ OMEGA  B TN  TO—EK -->
       GAMMA = TOKN(ITOK+IGAM)
       OMEGA = TOKN(ITOK+IOMEG)
C-->  # HE ŸBH›‰ ˜Aƒ „‹Ÿ MEXAHˆKˆ #####--->  ( MFT  O HOB›M TOKAM )::>
       IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 )
     =          MFTX = ME3N13( RMSW(D2), GAM1, TOKX )
C
       IF ( TIP .EQ. 11  .OR.  TIP .EQ.110.OR.TIP.EQ.14.OR.TIP.EQ.140)
     =          MFTX = MA3N13( RMSW(D2), GAM1, TOKX )
C-->   M_BH  „‹Ÿ  TN+H0   :-->
C****
C  MBHX-->  C “—ETOM TOKX, GAM1, OMEG1---> „‹Ÿ 14/140 “—ECTœ MFTN1(#)
C****
       TN1 = TN + H0
       IF ( TIP .EQ. 11  .OR.   TIP .EQ. 12  .OR. TIP .EQ. 14 )
     = MBHX = KMAS02( TN1, RMSW(D8) )
C
       IF ( TIP .EQ. 110  .OR.   TIP .EQ. 120  .OR. TIP .EQ. 140 )
     = MBHX = KMAS01( OMEG1,   RMSW(D8) )
C--
C######################################################################
       IF ( TIP .EQ. 14 .OR. TIP .EQ. 140 ) MBHX = MBHX - MFTN1
C######################################################################
       DOMX = RMSW(D3+IPTJ) * ( MBHX - MFTX )
       DOMN = RMSW(IDOM)
C--->  ####  B›O‹HEHˆE  HEŸBHOƒO  ˜AƒA ( TA KOPPEK–ˆˆ )::-->
       OMEGX = OMEGA + H1*DOMX + H2*DOMN
       GAMX = GAMMA + H1*OMEG1 + H2*OMEGA
C
C
C----------------------------------------------------------------------
C-->  O–EHKA OƒPE˜HOCTˆ...
       EPSX = 0.0
       NSM = 0
       DO 204 I=1,KALL
       X1 = ABS( TOKN1(ITOK1+I) )
       X2 = ABS( TOKX(I) )
C++++
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP )  GO TO 2040
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1 - X2 ) / XZ
       GO TO 2041
2040   EPS = ABS( X1 - X2 )
2041   CONTINUE
C+++++
       IF ( EPS .LT. EPSX ) GO TO 204
C-->  ”ˆKCˆP“EM  B MO„E‹ˆ...
       EPSX = EPS
       NSM = I
204    CONTINUE
C-->  „‹Ÿ MEXAHˆKˆ....
C----------------------------------------------------------------------
       X1 = ABS(GAM1)
       X2 = ABS(GAMX)
C++++
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP )  GO TO 2050
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1 - X2 ) / XZ
       GO TO 2051
2050   EPS = ABS( X1 - X2 )
2051   CONTINUE
C+++++
       IF ( EPS .LT. EPSX ) GO TO 205
C-->  ”ˆKCˆP“EM..
       EPSX = EPS
       NSM = IGAM + 1
205    CONTINUE
C----------------------------------------------------------------------
       X1 = ABS(OMEG1)
       X2 = ABS(OMEGX)
C++++
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP )  GO TO 2060
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1 - X2 ) / XZ
       GO TO 2061
2060   EPS = ABS( X1 - X2 )
2061   CONTINUE
C+++++
       IF ( EPS .LT. EPSX ) GO TO 206
C-->  ”ˆKCˆP“EM..
       EPSX = EPS
       NSM = IOMEG + 1
206    CONTINUE
C--->  EPE‡AˆCœ TOKOB  B   TOKN1...
       DO 207 I=1,KALL
       TOKN1(ITOK1+I) = TOKX(I)
207    CONTINUE
       TOKN1(ITOK+ IGAM) = GAMX
       TOKN1(ITOK+ IOMEG) = OMEGX
       TOKN1(ITOK+ IMFT ) = MFTX
       TOKN1(ITOK+ IMBH ) = MBHX
C--->  ECTœ ‹ˆ "O‹œ˜AŸ" O˜ˆKA  ????
       KOD = 0
       IF ( EPSX .LT. EITER ) RETURN
       KOD = 1
C-->  O˜ˆKA ECTœ, HE H“†HO ‹ˆ EE ”ˆKCˆPOBATœ....
       IF ( EPSX .LT. MAXITR ) RETURN
       MAXITR = EPSX
       NLITER = MSW(1)
       NSITER = NSM
       RETURN
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0007 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
         END
