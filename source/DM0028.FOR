         SUBROUTINE DM0028 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT „EKAPŸ 1987 ƒ.)------------
C   O„POƒPAMMA:  DM0028 ( MSW, RMSW, YALL, JALL, LOGYTR,
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
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTOB –Eˆ:
C                +-----+--------------------------------------------+
C                I Tˆ I              ˆMŸ MO„E‹ˆ                    I
C                I-----I--------------------------------------------I
C                I 15  I   “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK TOKA    I
C                I-----I--------------------------------------------I
C                I 16  I   “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK HAPŸ-  I
C                I     I   †EHˆŸ                                    I
C                I-----I--------------------------------------------I
C                I 200 I   ˆ„EA‹œH›‰ OEPA–ˆOHH›‰ “Cˆ‹ˆTE‹œ         I
C                I-----I--------------------------------------------I
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
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 15,16,200
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
C                  RMSW Tˆ # 15 [ ˆT“H ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   “1   I   “2    I    G    II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  11    I   12    I    13   II
C             ++--------+---------+---------++
C
C
C                  RMSW Tˆ # 16 [ ˆH“H ]
C                  ----------------------
C
C      ++--------+---------+---------+---------+----------+
C      II        I         I         I         I          I
C      II   “1   I   “2    I   GBH   I    K    I G=GBH*K  I
C      II        I         I         I         I          I
C      ++--------+---------+---------+---------+----------+    LM = 5
C      II   1    I    2    I     3   I    4    I    5     I    ******
C      ++--------+---------+---------+---------+----------+
C      II  11    I   12    I    13   I   14    I   15     I
C      ++--------+---------+---------+---------+----------+
C
C
C                  RMSW Tˆ # 200 [ ˆ„EA‹œH›‰ OEPA–ˆOHH›‰ “Cˆ‹ˆTE‹œ ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   K    I   GBX   I  GB›X   II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  13    I   14    I    15   II
C             ++--------+---------+---------++
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL
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
C--->  KOHTPO‹œ TˆA
         TIP = MSW(2)
         IF ( TIP .NE. 15 .AND. TIP .NE. 16 .AND. TIP.NE.200)GO TO 9999
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
         IF ( TIP .EQ. 16 ) GO TO 20
         IF ( TIP .EQ. 200) GO TO 30
C######################################################################
C---> “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK TOKA
C--->   O‹EMEHTH›‰ BK‹A„:
C==> CBOˆ  “‡‹›.
         K = MSW(9)
         L = MSW(10)
C==> “‡‹› “PAB‹Ÿž™EƒO HAPŸ†EHˆŸ
         M = MSW(11)
         N = MSW(12)
C===> KO””ˆ–ˆEHT CBŸ‡ˆ
         G = RMSW(13)
C======================================================================
         KOD = 0
C---->
         CALL DEPOSN( K, L, M, N, G, YALL )
C---->
         RETURN
C######################################################################
20      CONTINUE
C---> “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK HAPŸ†EHˆŸ
C--->   O‹EMEHTH›‰ BK‹A„:
C==> CBOˆ  “‡‹›.
         K = MSW(9)
         L = MSW(10)
C==> “‡‹› “PAB‹Ÿž™EƒO HAPŸ†EHˆŸ
         M = MSW(11)
         N = MSW(12)
C===> BH“TPEHHEE COPOTˆB‹EHˆE ˆ KO””ˆ–ˆEHT CBŸ‡ˆ
         GBH = RMSW(13)
         G   = RMSW(15)
C======================================================================
C-->   BK‹A„›BAEM...
         KOD = 0
C---->
         CALL DEPOSN( K, L, M, N, G, YALL )
C---->
         CALL DEPOST( K, L, GBH, 0.0, YALL, JALL )
C
         RETURN
C######################################################################
30      CONTINUE
C---> ˆ„EA‹œH›‰ OEPA–ˆOHH›‰ “Cˆ‹ˆTE‹œ
C--->   O‹EMEHTH›‰ BK‹A„:
C==> BXO„H›E  “‡‹›.
         M = MSW(9)
         N = MSW(10)
C==> B›XO„H›E  “‡‹›.
         K = MSW(11)
         L = MSW(12)
C===> KO””ˆ–ˆEHT “Cˆ‹EHˆŸ, BXO„HAŸ ˆ B›XO„HAŸ POBO„ˆMOCTœ.
         AK  = RMSW(13)
         GBX = RMSW(14)
         GBIX= RMSW(15)
C======================================================================
         KOD = 0
C-->  B‹O†ˆM BXO„H“ž –Eœ
         CALL DEPOST( M, N, GBX, 0.0, YALL, JALL )
C---> B‹O†ˆM B›XO„H“ž –Eœ
         G = AK * GBIX
         CALL DEPOSN( K, L, M, N, G, YALL )
C---->
         CALL DEPOST( K, L, GBIX, 0.0, YALL, JALL )
         RETURN
C======================================================================
666      CONTINUE
C$$$$$$$$$$$$$$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---> —TO ‡A Tˆ?
         IF ( TIP .EQ. 16 ) GO TO 40
         IF ( TIP .EQ. 200) GO TO 50
C######################################################################
C---> “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK TOKA
C==> “‡‹› “PAB‹Ÿž™EƒO HAPŸ†EHˆŸ
         M = MSW(11)
         N = MSW(12)
C--->  HOB›‰ TOK:
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         UN1 = VM - VN
         TOKT = RMSW(13) * UN1
C--->     ‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C######################################################################
40       CONTINUE
C---> “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK HAPŸ†EHˆŸ
C==> “‡‹› “PAB‹Ÿž™EƒO HAPŸ†EHˆŸ
         M = MSW(11)
         N = MSW(12)
C--->CBOˆ “‡‹›.
         K = MSW(9)
         L = MSW(10)
C--->  HOBOE “PAB‹Ÿž™EE HAPŸ†EHˆE
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         UBX = VM - VN
C--->  HOBOE B›XO„HOE HAPŸ†EHˆE
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VL = 0.0
         IF ( L .NE. 0 ) VL = VALLN1(L)
         UBIX = VK - VL
C---> HOB›‰ TOK
         TOKT = ( RMSW(14) * UBX + UBIX ) * RMSW(13)
C---> ‡AˆCœ TOKA
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKT
         KOD = 0
         RETURN
C######################################################################
50       CONTINUE
C---> ˆ„EA‹œH›‰ OEPA–ˆOHH›‰ “Cˆ‹ˆTE‹œ
C==> “‡‹› BXO„HOƒO HAPŸ†EHˆŸ
         M = MSW(9)
         N = MSW(10)
C--->“‡‹› B›XO„HOƒO HAPŸ†EHˆŸ
         K = MSW(11)
         L = MSW(12)
C--->  HOBOE BXO„HOE HAPŸ†EHˆE
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         UBX = VM - VN
C--->  HOBOE B›XO„HOE HAPŸ†EHˆE
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VL = 0.0
         IF ( L .NE. 0 ) VL = VALLN1(L)
         UBIX = VK - VL
C---> HOB›‰ TOK
         TOKBX = UBX * RMSW(14)
         TOKBIX= ( RMSW(13) * UBX + UBIX ) * RMSW(15)
C---> ‡AˆCœ TOKA
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOKBX
         TOKN1( ITOK + 1 ) = TOKBIX
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0028 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
