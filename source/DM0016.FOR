         SUBROUTINE DM0016 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0016 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C    *                         MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰ ˆ
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA ‡HA—EHˆ‰ APAMETPOB
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTA –Eˆ
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  63      3-X ”A‡HAŸ .„.C        ES3
C                              CˆH“COˆ„A‹œHOŸ
C
C
C              ‹EMEHT KO„ˆP“ETCŸ:
C              -------------------
C
C               # T 63 “ K0 KA KB KC  R EMAX OMEGA FAZ0;
C
C     K0 - O™ˆ‰ “‡E‹, KA - ”A‡A A, KB - ”A‡A B, KC - ”A‡A C;
C
C---> B›BO„H›E ‡HA—EHˆŸ: TOKˆ: K “‡‹AM KA, KB, KC, OT “‡‹A K0;
C                              *                   **
C
C
C                  13      "ˆ‡MEPˆTE‹œ"
C                           HAPŸ†EHˆŸ             V3
C
C
C              ‹EMEHT KO„ˆP“ETCŸ:
C              -------------------
C
C               # T 13 “ K1 K2 K3;
C
C   B CˆCOK B›—EC‹EHH›X ‡HA—EHˆ‰ OME™AET HAPŸ†EHˆE “‡‹OB K1, K2, K3,
C   ‡ATEM -> V_MOD, “ƒO‹ OTHOC. ”A‡› "A", V_X HA "A" POEK–ˆŸ, ˆ
C            V_Y HA "A"+90ƒPA„. POEK–ˆŸ, “ƒ‹OBAŸ CKOPOCTœ BPA™EHˆŸ
C                                         BEKTOPA V_MOD;
C                      !!! Pˆ V_MOD = 0.0 ---> OMEGA = 314.15927 !!!!
C
C                     EPEXO„ OT 3-X K 2-M C “—ETOM KO”. 2/3     !!!!!
C
C                  ***************************************************
C                  B) ZAPROS = 2
C                     ----------
C                  B›—ˆC‹EHˆE ‡HA—EHˆ‰ TOKOB B BETBŸX B T(N+1) MOMEHT
C                  BPEMEHˆ   ˆCXO„Ÿ ˆ‡ G ˆ J APAMETPOB ˆ ‡HA—EHˆŸ
C                  “‡‹OB›X HAPŸ†EHˆ‰ HA ‹EMEHTE PAC—ˆTAHH›X „‹Ÿ T(N+1)
C                    ˆ ‡AˆCœ (”ˆKCA–ˆŸ) TˆX ‡HA—EHˆ‰ B "TOK T(N+1)";
C
C              „‹Ÿ MO„E‹ˆ # 13 -->  EPE‡AˆCœ ˆ‡ VALLN1 B TOKN1
C              -------------------------------------------------
C              + B›—ˆC‹EHˆE PA‡‹O†EHˆŸ 3-X ”A‡HO‰ CˆCTEM›, C—ˆTAŸ -->
C              V(K1) - ”A‡A A; V(K2) - ”A‡A B; V(K3) - ”A‡A C;
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
C                          Pˆ  T  =  TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                           Pˆ  T  =  TN  + H0;
C                  TOKN  ˆ  TOKN1 - BEKTOPA EPEMEHH›X "COCTOŸHˆŸ"
C                                   ‹EMEHTA Pˆ  TN  ˆ  TN + H0;
C                                   PA‡MEPHOCTˆ CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 5;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C                  KOD - APAMETP OPATHO‰ CBŸ‡ˆ C MOHˆTOPOM DM....
C
C
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------             DEPOST;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=   1  ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 63 ˆ #13
C                --------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I  Pˆ‡. I KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I  TˆA  I —ECT I B›B. I‡HA—.  I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI  MO„E‹ˆI BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I        I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-----------+
C
C
C
C                RMSW TˆA # 63 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ - 3-œ ”A‡H›‰
C                -----------------------------------------------------
C
C
C                    ( U1 = EM * SIN( OMEGA * T  +  FAZ0 )     LM = 8
C                    ( U2 = EM * SIN( OMEGA * T  +  FAZ0 - 120 )
C                    ( U3 = EM * SIN( OMEGA * T  +  FAZ0 + 120 )
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II ‡HA—.  I ‡HA—.I        I FAZ0 II      IJESIJESI PE  I
C   “‡‹›:     II  1/R   I EM/R I OMEGA  I  P   II JESA I   I   I ‡E  I
C  K0         II        I      I        I PA„. II      I B I C I PB  I
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JESA, JESB, JESC
         REAL K2D3
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FKOM,   MASPRT(34) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADRMSW, TIP
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
C
         DATA R120/ 2.0944/, SQ3D2/ 0.8660254 /, PI2/6.283185306/
         DATA RGRAD / 57.29577 /
         DATA K2D3/0.666667 /
C
       KOD = 0
       TIP = MSW(2)
       IF ( TIP .EQ. 13  .OR.  TIP .EQ. 63 ) GO TO 10
C----------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0016 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
10     CONTINUE
C---> B›OP PE†ˆMA:
         GO TO ( 667, 666                 ), ZAPROS
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C---------ZAPROS = 1---------------------------------------------------
C
       IF ( TIP .EQ. 13 ) RETURN
C
C--->   O‹EMEHTH›‰ BK‹A„:    -->>  „‹Ÿ  E_SIN_3
C==> "HOBOE" BPEMŸ   ( „A, “†.... )
         TN1 = TN + H0
         GES = RMSW(13)
         JESA = RMSW(14) * SIN( RMSW(15) * TN1 + RMSW(16)  )
         JESB = RMSW(14) * SIN( RMSW(15) * TN1 + RMSW(16) - R120 )
         JESC = RMSW(14) * SIN( RMSW(15) * TN1 + RMSW(16) + R120 )
         RMSW(17) = JESA
         RMSW(18) = JESB
         RMSW(19) = JESC
C---->
C======================================================================
C-->  “‡‹›*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
C---->
         CALL DEPOST( K0, KA, GES, JESA, YALL, JALL )
         CALL DEPOST( K0, KB, GES, JESB, YALL, JALL )
         CALL DEPOST( K0, KC, GES, JESC, YALL, JALL )
C---->
         RETURN
C======================================================================
C$$$$$$$$$$$$$$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C
       IF ( TIP .EQ. 13 ) GO TO 777
C
C-->  “‡‹›*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
C---->
C--->     HOBE‰˜EE BPEMŸ...        TN + H0 => „‹Ÿ HEƒO JES OPE„E‹EHO!!!
         JESA = RMSW(17)
         JESB = RMSW(18)
         JESC = RMSW(19)
C--->  HOB›‰ TOK:
         V0 = 0.0
         IF ( K0 .NE. 0 ) V0 = VALLN1(K0)
         VA = 0.0
         IF ( KA .NE. 0 ) VA = VALLN1(KA)
         VB = 0.0
         IF ( KB .NE. 0 ) VB = VALLN1(KB)
         VC = 0.0
         IF ( KC .NE. 0 ) VC = VALLN1(KC)
C-->
         TOK1 = RMSW(13) * (V0-VA)  + JESA
         TOK2 = RMSW(13) * (V0-VB)  + JESB
         TOK3 = RMSW(13) * (V0-VC)  + JESC
C--
       TOK0 = TOK1 + TOK2 + TOK3
C-->
C--->     ‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOK1
         TOKN1( ITOK + 1 ) = TOK2
         TOKN1( ITOK + 2 ) = TOK3
         TOKN1( ITOK + 3 ) = TOK0
C----------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
777    CONTINUE
       VA = 0.0
       VB = 0.0
       VC = 0.0
C
       KA =  MSW(9)
       KB =  MSW(10)
       KC =  MSW(11)
C
       IF ( KA .NE. 0 ) VA = VALLN1( KA )
       IF ( KB .NE. 0 ) VB = VALLN1( KB )
       IF ( KC .NE. 0 ) VC = VALLN1( KC )
C
       ITOK = MSW(7)
C
       TOKN1( ITOK    ) = VA
       TOKN1( ITOK +1 ) = VB
       TOKN1( ITOK +2 ) = VC
C
C-->  PA‡‹O†EHˆE BEKTOPOB HA OCˆ "A" ˆ "A+90ƒPA„." --> X, Y;
C
       VX = K2D3 *(  VA - ( VB+VC )/2.    )
       VY = K2D3 *(  SQ3D2 * ( VB - VC )  )
C
       VXYMOD = SQRT( VX**2 + VY**2 )
       GVXY = ATAN2( VY, VX )
       IF ( VY .LT. 0.0 ) GVXY = GVXY + PI2
       IF ( GVXY .GE. PI2 ) GVXY = GVXY - INT( GVXY/PI2 ) * PI2
C-->  B TOKN1
       TOKN1( ITOK + 3 ) =  VXYMOD
       TOKN1( ITOK + 4 ) =  GVXY * RGRAD
       TOKN1( ITOK + 5 ) =  VX
       TOKN1( ITOK + 6 ) =  VY
C-->  B›—ˆC‹EHˆE OMEGA BPA™EHˆŸ-->
       VXOLD = TOKN( ITOK + 5 )
       VYOLD = TOKN( ITOK + 6 )
C******VXYOLD = TOKN( ITOK + 3 )
       OMEGA = 314.15927
       XXX = H0 * VXYMOD
       IF ( XXX .EQ. 0.0 ) GO TO 778
       YYY = SQRT( (VX - VYOLD)**2  + (VY - VYOLD)**2 )
       OMEGA = YYY / XXX
778    CONTINUE
       TOKN1( ITOK + 7 ) = OMEGA
C-->
       RETURN
       END
