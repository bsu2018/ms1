         SUBROUTINE DM0004 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0004 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰ ˆ
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA ‡HA—EHˆ‰ APAMETPOB
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTOB –Eˆ
C                  TˆA # 4 [ ˆCTO—HˆK OCTOŸHHOƒO TOKA ]
C                  TˆA # 5 [ ˆCTO—HˆK OCTOŸHHOƒO HAPŸ†EHˆŸ ]
C                  ********************************************
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
C                  D) ZAPROS = 3
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
C                  VALLN- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                         B T = TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          B T = TN + H0;
C                  TOKN  ˆ TOKN1 - BEKTOPA "TOKOB" EPEMEHH›X COCTOŸHˆŸ
C                                  MO„E‹ˆ ‹EMEHTA B TN ˆ TN+H0;
C                                  PA‡MEPHOCTœ CM. MASPRT(46)=NNTOK;
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
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3,
C   ------------             DEPOST;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=  1   ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 4  ˆ  # 5:
C                ---------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡.  I KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I‡HA—.  I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I        I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW TˆA # 4 [ ˆCTO—HˆK OCTOŸHHOƒO TOKA ]
C                  -------------------------------------------
C
C                SPM
C +-----+-----++------+
C IHOM. I HOM.II ‡HA—.I
C I“‡‹A I“‡‹A II   J  I
C I "K" I "M" II      I
C +-----+-----++------+
C I 9   I 10  II  11  I
C +-----------++------+
C
C
C             RMSW TˆA # 5 [ ˆCTO—HˆK OCTOŸHHOƒO HAPŸ†EHˆŸ ]
C             ------------------------------------------------
C
C                   SPM
C +-----+-----++--------+------+
C IHOM. I HOM.II ‡HA—.  I ‡HA—.I
C I“‡‹A I“‡‹A II  1/R   I  E/R I
C I "K" I "M" II        I      I
C +-----+-----++--------+------+
C I 9   I 10  II  11    I  12  I
C +-----------++--------+------+
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADRMSW
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
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
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS, TIP
         REAL JALL,  JE
C===>    BCTPOEHHAŸ ”“HK–ˆŸ OC‹“†ˆBAHˆŸ „B“XMEPH›X MACCˆBOB:
         ID(I,J) = NDALL * (J-1)  +  I
C---->   “‡‹› O„K‹ž—EHˆŸ:
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO‹œ TˆA
         TIP = MSW(2)
         IF ( TIP .NE. 4  .AND.  TIP .NE. 5 ) GO TO  9999
C---> B›OP PE†ˆMA:
         GO TO ( 667, 666                 ), ZAPROS
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C-----------ZAPROS = 1-------------------------------------------------
C--->  —TO ‡A Tˆ?
         IF ( TIP .EQ. 5 ) GO TO 5
C######################################################################
C---> ˆCTO—HˆK OCTOŸHHOƒO TOKA ( Tˆ #4 )
C--->  B›—ˆC‹EHˆE G - J APAMETPOB ˆ ˆX ‡AˆCœ A MSW:
         GE = 0.0
         JE = RMSW(11)
C--->   O‹EMEHTH›‰ BK‹A„:
C======================================================================
         KOD = 0
C--->
         CALL DEPOST( K, M, GE, JE, YALL, JALL )
C--->
         RETURN
C######################################################################
C---> ˆCTO—HˆK OCTOŸHHOƒO HAPŸ†EHˆŸ ( Tˆ #5 )
5      CONTINUE
C--->  B›—ˆC‹EHˆE G - J APAMETPOB ˆ ˆX ‡AˆCœ A MSW:
         GE = RMSW(11)
         JE = RMSW(12)
C--->   O‹EMEHTH›‰ BK‹A„:
C======================================================================
         KOD = 0
C--->
         CALL DEPOST( K, M, GE, JE, YALL, JALL )
C--->
         RETURN
C======================================================================
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C--->  —TO ‡A Tˆ?
         IF ( TIP .EQ. 5 ) GO TO 15
C######################################################################
C---> ˆCTO—HˆK OCTOŸHHOƒO TOKA ( Tˆ #4 )
         TOKT = RMSW(11)
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
       RETURN
C######################################################################
C---> ˆCTO—HˆK OCTOŸHHOƒO HAPŸ†EHˆŸ ( Tˆ #5 )
15     CONTINUE
C--->  OPE„„E‹ŸEM TOKˆ:   ZAPROS =  1
         GE = RMSW(11)
         JE = RMSW(12)
C--->  HOB›‰ TOK:
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UE = VK - VM
         TOKT = GE * UE  + JE
C---> EPE‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0004 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
