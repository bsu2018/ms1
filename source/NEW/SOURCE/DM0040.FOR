         SUBROUTINE DM0040 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT €…‹Ÿ  1990 ƒ.)------------
C   O„POƒPAMMA:  DM0040 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C    *                         MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C
C                  A) ZAPROS = 1
C                     ----------
C                  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰ ˆ
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA ‡HA—EHˆ‰ APAMETPOB
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTA –Eˆ
C
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  66            .„.C             ESG
C                              ƒ€ŒŽˆ—…‘Š€Ÿ
C                              CˆH“COˆ„A‹œH€Ÿ
C
C
C                  67          ƒ€ŒŽˆ—…‘Šˆ‰       JSC
C                              CˆH“COˆ„A‹œH›‰
C                              ˆ‘’. ’ŽŠ€
C
C
C              ‹EMEHT KO„ˆP“ETCŸ:
C              -------------------
C
C               # T 66 “ K0 K1     R    L
C                                   MAS < NGARM1  E1  FI1
C                                         NGARM2  E2  FI2
C                                         .....   ..  ...  >  ;
C
C
C
C
C               # T 67 “ K0 K1   MAS < NGARM1  J1  FI1
C                                        NGARM2  J2  FI2
C                                        .....   ..  ...  >  ;
C
C
C
C                  B) ZAPROS = 2
C                     ----------
C                  B›—ˆC‹EHˆE ‡HA—EHˆ‰ TOKOB B BETBŸX B T(N+1) MOMEHT
C                  BPEMEHˆ   ˆCXO„Ÿ ˆ‡ G ˆ J APAMETPOB ˆ ‡HA—EHˆŸ
C                  “‡‹OB›X HAPŸ†EHˆ‰ HA ‹EMEHTE PAC—ˆTAHH›X „‹Ÿ T(N+1)
C                    ˆ ‡AˆCœ (”ˆKCA–ˆŸ) TˆX ‡HA—EHˆ‰ B "TOK T(N+1)";
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
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 66 ˆ #67
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
C                  RMSW TˆA # 66 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ ]
C                  -----------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II     I     I ŠŽ‹-‚Ž I    I    I
C   “‡‹›:     II  R  I  L  I ƒ€ŒŽ.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
C
C
C                  RMSW TˆA # 67 [ ˆCTO—HˆK SIN TOKA ]
C                  ------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II  …-I  …-I ŠŽ‹-‚Ž I    I    I
C   “‡‹›:     II ‡…‚I ‡…‚I ƒ€ŒŽ.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JES
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW, TIP, ZAPROS
C
C==> "HOBOE" BPEMŸ   ( „A, “†.... )
         TN1 = TN + H0
C---->   “‡‹› O„K‹ž—EHˆŸ:
         K0 = MSW(9)
         K1 = MSW(10)
         KOD = 0
C--->  KOHTPO‹œ TˆA
         TIP = MSW(2)
         IF ( TIP .EQ. 66  .OR.  TIP .EQ. 67 ) GO TO 10
C----------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0040 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
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
C
         IF( TIP .EQ. 67 ) GO TO 67
C---> ˆCTO—HˆK SIN HAPŸ†EHˆŸ
C--->   O‹EMEHTH›‰ BK‹A„:
         ITOK= MSW(7)
         TOK = TOKN(ITOK)
C----------------------------------------------------------------------
         VK0 = 0.0
         IF ( K0 .NE. 0 ) VK0 = VALLN(K0)
         VK1 = 0.0
         IF ( K1 .NE. 0 ) VK1 = VALLN(K1)
         UN = VK0 - VK1
C----------------------------------------------------------------------
C--> Ž…„…‹ˆŒ …„‘
         KGARM = MSW(13)
         EN = 0.0
         EN1= 0.0
         DO 2000 I=1,KGARM
         NG  =  MSW(13+3*I)
         EG  = RMSW(14+3*I)
         FIG = RMSW(15+3*I)
         EN  = EN  + EG * SIN( NG * 314.16 * TN  + FIG )
         EN1 = EN1 + EG * SIN( NG * 314.16 * TN1 + FIG )
2000     CONTINUE
C----------------------------------------------------------------------
         Z = RMSW(12) + H1 * RMSW(11)
         GES = H1 / Z
         JES =( TOK*(RMSW(12)-H2*RMSW(11)) + H1*EN1 + H2*EN + H2*UN )/Z
         RMSW(14) = GES
         RMSW(15) = JES
C---->
C======================================================================
         KOD = 0
C---->
         CALL DEPOST( K0, K1, GES, JES, YALL, JALL )
C---->
         RETURN
C======================================================================
67       CONTINUE
C---> ˆCTO—HˆK SIN ’ŽŠ€
C--->   O‹EMEHTH›‰ BK‹A„:
C--> Ž…„…‹ˆŒ ’ŽŠ ˆ‘’Ž—ˆŠ€
         KGARM = MSW(13)
         TOKJN1= 0.0
         DO 3000 I=1,KGARM
         NG  =  MSW(13+3*I)
         AG  = RMSW(14+3*I)
         FIG = RMSW(15+3*I)
         TOKJN1 = TOKJN1 + AG * SIN( NG * 314.16 * TN1 + FIG )
3000     CONTINUE
C----------------------------------------------------------------------
         JES =  TOKJN1
         RMSW(15) = JES
C---->
C======================================================================
         KOD = 0
C---->
         CALL DEPOST( K0, K1, 0.0, JES, YALL, JALL )
C---->
         RETURN
C======================================================================
666      CONTINUE
C################   ZAPROS = 2   ######################################
C---> —TO ‡A Tˆ?
         IF ( TIP .EQ. 67 ) GO TO 167
C######################################################################
C---> ˆCTO—HˆK SIN HAPŸ†EHˆŸ.
C--->     HOBE‰˜EE BPEMŸ...        TN + H0 => „‹Ÿ HEƒO JES OPE„E‹EHO!!!
         GES = RMSW(14)
         JES = RMSW(15)
C--->  HOBOE HAPŸ†EHˆE:
         VK0 = 0.0
         IF ( K0 .NE. 0 ) VK0 = VALLN1(K0)
         VK1 = 0.0
         IF ( K1 .NE. 0 ) VK1 = VALLN1(K1)
         UN1 = VK0 - VK1
C--->  HOB›‰ TOK:
         TOKT = GES * UN1 + JES
C--->  ‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C######################################################################
167      CONTINUE
C---> ˆCTO—HˆK SIN ’ŽŠ€
         JES = RMSW(15)
C--->  ‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = JES
         KOD = 0
         RETURN
C######################################################################
         END
