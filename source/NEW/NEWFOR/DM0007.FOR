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
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 11:
C                ----------------------------------------------
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
C  I “‡‹› O„K‹ž—EHˆŸ S1N S2N S3N FN S1K S2K S3K FK               I
C  I           8-Tœ C‹OB: OT 9 „O 16                              I
C  +--------------------------------------------------------------+
C
C              RMSW Tˆ # 11 [ ‹. MA˜ˆHA 3X3X0 ] :
C              ************************************************
C  +--------+-----+------+------+-------+-------+-------+-------+-----+-----+
C  I OMEGA  IMATR I MATR I MATR I MATR  I MATR  I MATR  I MATR  I MATRI TOK I
C  I        I A1  I  A2  I  A3  I  A4-1 I  A5   I  A6   I  G    I  J  I DEMFI
C  I   15   I 7   I  7   I  2   I 16    I 16    I  8    I 16    I 4   I  2  I
C  I  ‘‹Ž‚  I ‘‹Ž‚I ‘‹Ž‚ I C‹OB I C‹OB  I C‹OB  I C‹OB  I C‹OB  I C‹OBI ‘‹Ž‚I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+-----+
C  I 17-31  I32-38I39-45 I46-47 I48-63  I64-79  I 80-87 I88 -103I104- I 108 I
C  I        I     I      I      I       I       I       I       I 107 I- 109I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+-----+
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
         REAL JALL
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION UL(6),  TOKX(6)
         INTEGER ADRMSW
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
C**********************************************************************
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       TIP = MSW(2)
C--
       IF ( TIP .EQ. 11   ) GO TO  99990
       GO TO 9999
99990  CONTINUE
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C-->  “KA‡-‹œ HA TOK...
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
cc ˆ§¬¥­ï¥¬ è £ ¨­â¥£à¨à®¢ ­¨ï
cá
cá
cá
c       if( (TN .LE. 0.3) .OR. (RMSW(110) .EQ. 1.) ) goto 10 
c        RMASPT(40) = RMASPT(40) / 10.
c        rmsw(110) = 1.
c        RMSW(28) = RMSW(28) / 100.
c10      continue
c        write (*,*) 'H0=',H0,'rmsw(110)',rmsw(110)
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
C-->  PAC—ET J0:
       GAMMA = TOKN1(ITOK+ 4)
       OMEGA = RMSW(31)
       GAMMA = GAMMA + H0 * OMEGA
       CALL MOTOR1( H0, GAMMA, RMSW(17),RMSW(32),RMSW(39),
     = RMSW(46),RMSW(48),RMSW(88) )
C---> €•Ž„ˆŒ G
c       CALL MOTOR2( H0,GAMMA,RMSW(48),RMSW(88))
c        write(*,*) rmsw(88),rmsw(89),rmsw(90),rmsw(91)
c        write(*,*) rmsw(92),rmsw(93),rmsw(94),rmsw(95)
c        write(*,*) rmsw(96),rmsw(97),rmsw(98),rmsw(99)
c        write(*,*) rmsw(100),rmsw(101),rmsw(102),rmsw(103)
c        pause
C----> ¨é¥¬ A5
       CALL MOTOR3( H0,GAMMA,RMSW(17),RMSW(48),RMSW(64) )
c        write(*,*) 'rmsw'
c        write(*,*) rmsw(64),rmsw(65),rmsw(66),rmsw(67)
c        write(*,*) rmsw(68),rmsw(69),rmsw(70),rmsw(71)
c        write(*,*) rmsw(72),rmsw(73),rmsw(74),rmsw(75)
c        write(*,*) rmsw(76),rmsw(77),rmsw(78),rmsw(79)
C----> ¨é¥¬ A6
       CALL MOTOR4( H0,RMSW(17),RMSW(48),RMSW(80) )
C----> €•Ž„ˆŒ J
       DO 105 I = 1,4
       RMSW(103+I) = RMSW(63+4*(I-1) +1)*TOKN1(ITOK) +
     =   RMSW(63+4*(I-1) +2)*TOKN1(ITOK+1) +
     =   RMSW(63+4*(I-1) +3)*TOKN1(ITOK+2) +
     =   RMSW(63+4*(I-1) +4)*TOKN1(ITOK+3)
105     CONTINUE
       DO 110 I = 1,4
       RMSW(103 + I ) = RMSW(103 + I) + RMSW(80 +2*(I-1))*RMSW(108)
     = + RMSW(81 +2*(I-1))*RMSW(109)
110    CONTINUE
c        write(*,*) 'TOKN'
c        write(*,*) TOKN(ITOK),TOKN(ITOK+1),TOKN(ITOK+2),
c     = TOKN(ITOK+3)
c         write(*,*) 'RMSW(104)'
c         write(*,*) rmsw(104),rmsw(105),rmsw(106),rmsw(107)
c         write(*,*) 'RMSW(80)'
c         write(*,*) rmsw(80),rmsw(81),rmsw(82),rmsw(83)
c         write(*,*) rmsw(84),rmsw(85),rmsw(86),rmsw(87)
c         write(*,*) 'RMSW(108)'
c         write(*,*) rmsw(108),rmsw(109)
c       pause
C   GY   ˆ  JY   OPE„E‹EH›....
C---  BK‹A„›BAEM ----
       CALL DEPOSA( MSW(9), MSW(13), 4, 4,RMSW(88),RMSW(104),
     =              YALL,  JALL )
C@@@@
       KOD = 0
       IF ( FTEST .NE. -1 ) RETURN
C--->  OT‹A„O—HAŸ E—ATœ:
       CALL PRINT5( TN,  RMSW, TOKN, TOKN1, ITOK )
       RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C---- ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  A„EHˆE HAPŸ†EHˆŸ-->  HOBOE
       DO 201 I=1,4
C-->  “‡‹› PEA‹œH›E, HE „EMP”EPH›E...
       K = MSW( I + 8 )
       M = MSW( 8 + 4 + I )
       VK = 0.0
       IF ( K .NE. 0 ) VK = VALLN1(K)
       VM = 0.0
       IF ( M .NE. 0 ) VM = VALLN1(M)
       UL(I) = VK - VM
201    CONTINUE
c       write(*,*)'Ul'
c       write(*,*) ul(1),ul(2),ul(3),ul(4)
c        if( TN .LT. 0.015)  goto 205
c        do 205 i = 1,3         
c         ul(i) = 0.01
c205     continue 
C-->  HA‰„EM TOKˆ ‚ ‘’€’Ž›• ˆ Ž’Ž›• ŽŒŽ’€• O  VALLN1...
       DO 210 I = 1,4
       TOKX(I) = RMSW(87+4*(I-1) +1)*UL(1) +
     =   RMSW(87+4*(I-1) +2)*UL(2) +
     =   RMSW(87+4*(I-1) +3)*UL(3) + RMSW(87+4*(I-1) +4)*UL(4)
     =  + RMSW(103+I)
210     CONTINUE
c        write(*,*)'tokn'
c        write(*,*) tokx(1),tokx(2),tokx(3),tokx(4)
c        pause
C---> €‰„…Œ ’ŽŠˆ ‚ „…Œ”…›• ŽŒŽ’Š€•
       TOKDD = -RMSW(32)*TOKX(1) - RMSW(33)*TOKX(2) -RMSW(34)*
     = TOKX(3) - RMSW(35)*TOKX(4) + RMSW(39)*TOKN(ITOK) +
     = RMSW(40)*TOKN(ITOK+1) + RMSW(41)*TOKN(ITOK +2) + RMSW(42)*
     = TOKN(ITOK+3) + RMSW(46)*RMSW(108)
C----------------------------------
       TOKDQ = -RMSW(36)*TOKX(1) - RMSW(37)*TOKX(2) -RMSW(38)*
     = TOKX(3)  + RMSW(43)*TOKN(ITOK) +
     = RMSW(44)*TOKN(ITOK+1) + RMSW(45)*TOKN(ITOK +2) +
     =   RMSW(47)*RMSW(109)
C ”ˆŠ‘ˆ“…Œ
      RMSW(108) = TOKDD
      RMSW(109) = TOKDQ
C--->  EPE‡AˆCœ TOKOB  B   TOKN1...
       DO 217 I=1,4
       TOKN1(ITOK1+I) = TOKX(I)
217    CONTINUE
      TOKN1(ITOK1+5)  =  TOKN(ITOK1+5) + H0*RMSW(31)
C “•Ž„ˆŒ
       KOD = 0
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