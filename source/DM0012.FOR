         SUBROUTINE DM0012 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0012 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰ ˆ
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA ‡HA—EHˆ‰ APAMETPOB
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTA –Eˆ
C                  TˆA # 71, # 72 ˆ 720 [ HE‹ˆHE‰H›E R  ˆ  L  ]
C                  ********************************************
C
C                   B.A.X     - ‡A„AETCŸ C‹A‰H-PA‡‹O†EHˆEM
C                               —EPE‡ MACCˆB MAS < I1 ”1 .... >
C                                ( CM. / FM0007 );
C                  ‹EMEHT OˆC›BAETCŸ:
C                  --------------------
C            ?? T 71[72]  “  K  M
C
C                   MAS < I1 U1 ...... >  - „‹Ÿ # 71  BO‹œT_AMEP. X-KA
C                   MAS < I1 ”1 ...... >; - „‹Ÿ # 72  BEEP_AMEP. X-KA
C                   MAS < I1 L„ˆ”1.... >; - „‹Ÿ # 720 -> L_„ˆ” OT TOKA;
C
C               *** BO‡MO†H› ˆTEPA–ˆOHH›E “TO—HEHˆŸ ***:
C                   Pˆ BO‡“†„EHˆˆ FITER [ MASPRT(65) ] --->
C                   ‡HA—EHˆŸ "TOK"_Ÿ - POƒHO‡ ŸBH›M METO„OM -
C                   HE B›—ˆC‹ŸžTCŸ, A B›ˆPAžTCŸ ˆ‡ MACCˆBA TOKN1;
C               ***
C              === „‹Ÿ RN -> KOHTPO‹œ O A„EHˆž HAPŸ†EHˆŸ;
C                  „‹Ÿ RN -> KOHTPO‹œ O PE„CKA‡AHHOM“ ˆ “TO—HEHHOM“
C                            ‡HA—EHˆž TOKA;
C              ===
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
C                 MSWALL( (RMSWAL ) - BECœ CˆCOK MSW_ALL,
C                 ADRMSW - A„PEC TEK. C‹OAB MSW ( RMSW );
C
C
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST, SEVAL0
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=   1 ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 71  ˆ  # 72, # 720:
C                ------------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. IHA—.   I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C                  RMSW TˆA # 71, #72  ˆ  #720 [ HE‹ˆHE‰H›E R ˆ L ]:
C                  --------------------------------------------------
C                                                        LM=<EPEMEHHAŸ>
C                                                          =+1<VAR>
C                                                  (OT KO‹-AB TO—EK BAX)
C                    SPM
C+--+---+-----+----+-----------------------+
CI  I   I  G  I  J I  XY_MAS               I
CI KI M I     I    I MACCˆB TO—EK BAX.     I    MACˆB› "B", "C", "D" -->
CI  I   I     I    I N,(X,Y).."B","C","D"  I    C‹A‰H PA‡‹O†EHˆE BAX:
C+--+---+-----+----+-----------------------+
CI9 I 10I  11 I 12 I 13 ...12+(5*N+1)      I
C+--+---+-----+----+-----------------------+
C
C   N -> KO‹ˆ—ECTBO TO—EK BAX.
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER, TIP, ZAPROS
C-->    HO‹œ POˆ‡BO„HO‰.......
         DATA EK00/ 1E-9/
C**********************************************************************
C-->  “KA‡ATE‹œ HA TOK
       ITOK = MSW(7)
C-->  “‡‹› O„K‹ž—EHˆŸ..
       K = MSW(9)
       M = MSW(10)
       TIP = MSW(2)
C-->
       IF ( TIP .EQ. 71  .OR. TIP .EQ. 72  .OR. TIP .EQ.720) GO TO 9900
       GO TO 9999
9900   CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C-->  —TO ‡A MO„E‹œ..?
       IF ( TIP .EQ. 72 ) GO TO 150
       IF ( TIP .EQ. 720 ) GO TO 160
C--- HE‹ˆH. PE‡ˆCTOP, ZAPROS = 1 ++++++++++++++++++++++++++++++++++++++
C-->  OKPECHOCTœ OPE„E‹EHˆŸ TOK_A
       TOK = TOKN(ITOK)
C-->  HO MO†ET ›Tœ TO ˆTEP. “TO—..
       IF ( FITER .EQ. 1 ) TOK = TOKN1( ITOK )
C-->  ‡HA—EHE O BAX..
       KODFUN = 1
       FUN = SEVAL0( KODFUN, TOK, RMSW(13) )
       KODFUN = 2
       DFUN = SEVAL0( KODFUN, TOK, RMSW(13) )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       IF ( DFUN .EQ. 0.0 ) WRITE(NWTRE,101) MSW(1), TIP, TN, TOK
       IF ( DFUN .EQ. 0.0 ) DFUN = EK00
101    FORMAT(/' *** ‹EMEHT # ', I5, ' Tˆ # ', I3, ' - 0-Ÿ',
     = ' POˆ‡BO„HAŸ ***'/
     = ' *** BPEMŸ ', G12.5, ' ‡HA—EHˆE TOKA ', G12.5, ' O›TKA',
     = ' KOPPEK–ˆˆ ***'/)
102    FORMAT(/' *** ‹EMEHT # ', I5, ' Tˆ # ', I3, ' - 0-Ÿ',
     = ' „ˆ”. ˆH„“KTˆBHOCTœ ***'/
     = ' *** BPEMŸ ', G12.5, ' ‡HA—EHˆE TOKA ', G12.5, ' O›TKA',
     = ' KOPPEK–ˆˆ ***'/)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C--->  G-J ..
       G = 1.0 / DFUN
       JNEL = TOK - FUN * G
C-->  ”ˆKCˆPOBATœ
       RMSW(11) = G
       RMSW(12) = JNEL
C-->  B‹O†ˆTœ
       CALL DEPOST( K, M, G, JNEL, YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
150    CONTINUE
C-->  "LN", ZAPROS=1
C-->  "CTAP›‰" TOK..
       TOK = TOKN(ITOK)
       IF ( FITER .EQ. 1 ) GO TO 151
C-->  OPE„. AP-P› B "N" TO—KE..
       KODFUN = 2
       DFUN = SEVAL0( KODFUN, TOK, RMSW(13) )
       IF ( DFUN .EQ. 0.0 ) WRITE(NWTR,101) MSW(1), TIP, TN, TOK
       IF ( DFUN .EQ. 0.0 ) DFUN = EK00
C-->  CTAPOE A„EHˆE...
       VK = 0.0
       VM = 0.0
       IF ( K .NE. 0 ) VK = VALLN(K)
       IF ( M .NE. 0 ) VM = VALLN(M)
       UL = VK - VM
C-->  ˆCTO—HˆK O CTAPOM“ TOK“..JNEL...
       JNEL = TOK
       IF ( H2 .NE. 0.0 ) JNEL = JNEL + H2*UL/DFUN
       RMSW(12) = JNEL
C--> YTOK --> POƒHO‡..
       YTOK = TOK + H0 * UL/ DFUN
       TOKN1(ITOK) = YTOK
C-->  C—ˆTATœ G ˆ B‹O†ˆTœ...
151    CONTINUE
       YTOK = TOKN1( ITOK )
       JNEL = RMSW(12)
C-->  O YTOK   HA‰„EM G..
       KODFUN = 2
       DFUNX = SEVAL0(KODFUN, YTOK, RMSW(13) )
       IF ( DFUNX .EQ. 0.0 ) WRITE(NWTRE, 101) MSW(1), TIP, TN, YTOK
       IF ( DFUNX .EQ. 0.0 ) DFUNX = EK00
       G = H1 / DFUNX
       RMSW(11) = G
C-- T“„A EƒO...
       CALL DEPOST(K, M, G, JNEL, YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
160    CONTINUE
C-->  "LN", ZAPROS=1,  L_„ˆ””EP.
C-->  "CTAP›‰" TOK..
       TOK = TOKN(ITOK)
       IF ( FITER .EQ. 1 ) GO TO 161
C-->  OPE„. AP-P› B "N" TO—KE..
       KODFUN = 1
       DFUN = SEVAL0( KODFUN, TOK, RMSW(13) )
       IF ( DFUN .EQ. 0.0 ) WRITE(NWTR,102) MSW(1), TIP, TN, TOK
       IF ( DFUN .EQ. 0.0 ) DFUN = EK00
C-->  CTAPOE A„EHˆE...
       VK = 0.0
       VM = 0.0
       IF ( K .NE. 0 ) VK = VALLN(K)
       IF ( M .NE. 0 ) VM = VALLN(M)
       UL = VK - VM
C-->  ˆCTO—HˆK O CTAPOM“ TOK“..JNEL...
       JNEL = TOK
       IF ( H2 .NE. 0.0 ) JNEL = JNEL + H2*UL/DFUN
       RMSW(12) = JNEL
C--> YTOK --> POƒHO‡..
       YTOK = TOK + H0 * UL/ DFUN
       TOKN1(ITOK) = YTOK
C-->  C—ˆTATœ G ˆ B‹O†ˆTœ...
161    CONTINUE
       YTOK = TOKN1( ITOK )
       JNEL = RMSW(12)
C-->  O YTOK   HA‰„EM G..
       KODFUN = 1
       DFUNX = SEVAL0(KODFUN, YTOK, RMSW(13) )
       IF ( DFUNX .EQ. 0.0 ) WRITE(NWTRE, 102) MSW(1), TIP, TN, YTOK
       IF ( DFUNX .EQ. 0.0 ) DFUNX = EK00
       G = H1 / DFUNX
       RMSW(11) = G
C-- T“„A EƒO...
       CALL DEPOST(K, M, G, JNEL, YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
C######################################################################
C######################################################################
200    CONTINUE
C--> ZAPROS = 2
C-->  HOBOE A„EHˆE..
       VK = 0.0
       VM = 0.0
       IF ( K .NE. 0 ) VK = VALLN1(K)
       IF ( M .NE. 0 ) VM = VALLN1(M)
       UL = VK - VM
C-->  TOK..
       TOKX = RMSW(11) * UL  +  RMSW(12)
C--> MO„E‹œ ???
       IF ( TIP  .EQ. 72  .OR. TIP .EQ. 720    ) GO TO 250
C--->  "RN" ....
C-->  "HOB›‰" TOK
       TOKN1(ITOK) = TOKX
C-->  "BOCTAHOBˆM" PŸ„..
       KODFUN = 1
       URX = SEVAL0( KODFUN, TOKX, RMSW(13) )
C-->  O–EHˆM OƒPE˜HOCTœ..
       X1 = ABS( UL )
       X2 = ABS( URX )
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 201
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1-X2) / XZ
       GO TO 202
201    EPS = ABS( X1 - X2 )
202    CONTINUE
C-->  „O“CTˆMO TO..?
       KOD = 0
       IF ( EPS .LT. EITER ) RETURN
C                            *******
C-->  O˜ˆKA..
       KOD = 1
       IF ( EPS .LT. MAXITR ) RETURN
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
       RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
250    CONTINUE
C==> ZAPROS = ",  "LN"...      „‹Ÿ  L_„ˆ””.  TO †E CAMOE.....
C-->  PE„CKA‡AHH›‰ TOK ..
       X1 = ABS( TOKN1( ITOK ) )
C-->  O‹“—EHH›‰ TOK..
       X2 = ABS( TOKX )
C-->  ‡AOMHˆM O‹“—EHH›‰..
       TOKN1(ITOK) = TOKX
C-->  OƒPE˜ˆM...
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 251
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 252
251    EPS = ABS( X1 - X2 )
252    CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) RETURN
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) RETURN
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
       RETURN
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0012 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
