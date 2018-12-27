         SUBROUTINE DM0005 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0005 ( MSW, RMSW, YALL, JALL, LOGYTR,
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
C                  TˆA # 6 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ(1) ];
C                  TˆA # 7 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ(2) ];
C                  ***************************************************
C                  B) ZAPROS = 2
C                     ----------
C                  B›—ˆC‹EHˆE ‡HA—EHˆ‰ TOKOB B BETBŸX B T(N+1) MOMEHT
C                  BPEMEHˆ   ˆCXO„Ÿ ˆ‡ G ˆ J APAMETPOB ˆ ‡HA—EHˆŸ
C                  “‡‹OB›X HAPŸ†EHˆ‰ HA ‹EMEHTE PAC—ˆTAHH›X „‹Ÿ T(N+1)
C                    ˆ ‡AˆCœ (”ˆKCA–ˆŸ) TˆX ‡HA—EHˆ‰ B "TOK T(N+1)";
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
C                   ( ˆCO‹œ‡“ETCŸ „‹Ÿ MAKPOMO„E‹E‰ "ˆ„EA‹œH›X" K‹ž—E‰)
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
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 6 ˆ  # 7:
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
C                  RMSW TˆA # 6 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ(1) ]
C                  --------------------------------------------
C                    ( U = EM * SIN( OMEGA * T  +  FAZA )
C
C
C                             SPM                 SRAB
C +-----+-----++--------+------+--------+------++------+
C IHOM. I HOM.II ‡HA—.  I ‡HA—.I        I FAZA II      I
C I“‡‹A I“‡‹A II  1/R   I EM/R I OMEGA  I  P   II JES  I
C I "K" I "M" II        I      I        I PA„. II      I
C +-----+-----++--------+------+--------+------++------+
C I  9  I  10 II  11    I  12  I  13    I  14  II  15  I
C +-----+-----++--------+------+--------+------++------+
C
C
C
C                  RMSW TˆA # 7 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ(2) ]
C                  --------------------------------------------
C                                                             LM = 7
C                                                             ******
C
C                      SPM                        SRAB
C     ++------+------+------+--------+------++------+------+
C     II      I      I ‡HA—.I        I FAZA II      I      I
C     II  R   I  L   I  EM  I OMEGA  I  P   II GES  I JES  I
C     II      I      I      I        I PA„. II      I      I
C     ++------+------+------+--------+------++------+------+
C     II  1   I  2   I  3   I   4    I  5   II  6   I   7  I
C     ++------+------+------+--------+------++------+------+
C     II  11  I  12  I  13  I  14    I  15  II  16  I  17  I
C     ++------+------+------+--------+------++------+------+
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JES
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
       INTEGER ADRMSW
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS, TIP
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
C===>    BCTPOEHHAŸ ”“HK–ˆŸ OC‹“†ˆBAHˆŸ „B“XMEPH›X MACCˆBOB:
         ID(I,J) = NDALL * (J-1)  +  I
C==> "HOBOE" BPEMŸ   ( „A, “†.... )
         TN1 = TN + H0
C---->   “‡‹› O„K‹ž—EHˆŸ:
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO‹œ TˆA
         TIP = MSW(2)
         IF ( TIP .LT. 6 .OR. TIP .GT. 7 ) GO TO 9999
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
C---> —TO ‡A Tˆ?
         IF ( TIP .EQ. 7 ) GO TO 7
C######################################################################
C---> ˆCTO—HˆK SIN HAPŸ†EHˆŸ(1).
C--->   O‹EMEHTH›‰ BK‹A„:
         GES = RMSW(11)
         JES = RMSW(12) * SIN( RMSW(13) * TN1 + RMSW(14)  )
         RMSW(15) = JES
C---->
C======================================================================
         KOD = 0
C---->
         CALL DEPOST( K, M, GES, JES, YALL, JALL )
C---->
         RETURN
C######################################################################
C---> ˆCTO—HˆK SIN HAPŸ†EHˆŸ(2)
7        CONTINUE
C--->   O‹EMEHTH›‰ BK‹A„:
         ITOK= MSW(7)
         TOK = TOKN(ITOK)
C----------------------------------------------------------------------
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
C----------------------------------------------------------------------
         EN  = RMSW(13) * SIN( RMSW(14) * TN  + RMSW(15)  )
         EN1 = RMSW(13) * SIN( RMSW(14) * TN1 + RMSW(15)  )
C----------------------------------------------------------------------
         Z = RMSW(12) + H1 * RMSW(11)
         GES = H1 / Z
         JES =( TOK*(RMSW(12)-H2*RMSW(11)) + H1*EN1 + H2*EN + H2*UN )/Z
         RMSW(16) = GES
         RMSW(17) = JES
C---->
C======================================================================
         KOD = 0
C---->
         CALL DEPOST( K, M, GES, JES, YALL, JALL )
C---->
         RETURN
C======================================================================
666      CONTINUE
C$$$$$$$$$$$$$$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---> —TO ‡A Tˆ?
         IF ( TIP .EQ. 7 ) GO TO 17
C######################################################################
C---> ˆCTO—HˆK SIN HAPŸ†EHˆŸ.
C--->     HOBE‰˜EE BPEMŸ...        TN + H0 => „‹Ÿ HEƒO JES OPE„E‹EHO!!!
         JES = RMSW(15)
C---> HOBOE HAPŸ†EHˆE:
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UN1 = VK - VM
C--->  HOB›‰ TOK:
         TOKT = RMSW(11) * UN1 + JES
C--->  ‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C######################################################################
C---> ˆCTO—HˆK SIN HAPŸ†EHˆŸ(2)
17       CONTINUE
C--->     HOBE‰˜EE BPEMŸ...        TN + H0 => „‹Ÿ HEƒO JES OPE„E‹EHO!!!
         GES = RMSW(16)
         JES = RMSW(17)
C---> HOBOE HAPŸ†EHˆE:
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UN1 = VK - VM
C--->  HOB›‰ TOK:
         TOKT = GES * UN1 + JES
C--->     ‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0005 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
