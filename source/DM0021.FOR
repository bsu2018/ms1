         SUBROUTINE DM0021 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT   MAŸ   1987 ƒ.)------------
C   O„POƒPAMMA:  DM0021 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    MOHˆTOP›:
C   -----------      MO„E‹ˆ BEHTˆ‹Ÿ, “P. Cˆ”“ [MO„.# 93,.Tˆ #6]
C                    MO„E‹ˆ K‹ž—A  , “P. Cˆ”“ [MO„.#668,.Tˆ #6]
C                  ****************************************************
C
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
C                  <TOKN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK ‡AKP›T›X BEHTˆ‹E‰ = 0.0;
C                  ‡AˆCœ TˆX TOKOB B MACCˆB TOKN1;
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
C                   - Pˆ TX_G  [MASPRT(35)] .EQ. HX[RMSW(15)] -
C                      ”ˆKCˆP“ETCŸ CMEHA COCTOŸHˆŸ BEHTˆ‹Ÿ APAMETPOM
C                      KD [MSW(14)];
C                     PABEHCTBO POBEPŸETCŸ C TO—. „O "EPSKOM".
C
C   METO„:         PA‡HOBˆ„HOCTœ BO‹HOBOƒO A‹ƒOPˆTMA „‹Ÿ ”OPMˆPOBAHˆŸ
C   -----          MACCˆBA LOGYTR --> —EE‡ / GETLOG;
C
C                  "TEHEBAŸ" CˆCTEMA ”OPMˆP“ETCŸ "MAC˜TAˆPOBAHHAŸ"
C                   TAK, —TO: V[X] - V[Y] = <TOK    X-Y   BETBˆ>;
C                   “‡‹›, OTME—EHH›E B LOGYTR(*) = 0 - "A‡OB›E"
C                   „‹Ÿ KA†„O‰ ˆH–ˆ„EHTHO‰ ƒP“› OTKP›T›X BEHTˆ‹E‰;
C
C   APAMETP›:     MSW, RMSW - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------      ( "OTPE‡OK" MACCˆB› MSWALL -> C.M. / GETMSW );
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  T  =  TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  TN + H0;
C                  TOKN  ˆ TOKN1 - ‡HA—EHˆE EPEMEHH›X "COCTOŸHˆŸ"
C                                  MO„E‹ˆ Pˆ TN  ˆ  TN + H0;
C                          ( PA‡MEPHOCTˆ -> CM. MASPRT(46) = NNTOK )
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
C   ------------   2. GETLOG,  SEQSOS, DEPOST;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=1, 10,20,3,4 - B›„A—A COO™EHˆŸ ˆ “XO„
C                3. Pˆ FTEST = 1 -> E—ATœ HA WTRE AKTA CMEH› COCTOŸHˆŸ
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 93 ˆ # 668
C                ---------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I‡HA—.  I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I        I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C
C
C
C
C
C        RMSW TˆA # 93 ˆ‹ˆ # 668 [ BEHTˆ‹œ(K‹ž—), “P. Cˆ”“ ]
C        -----------------------------------------------------
C    SPM   I                SRAB
C +----+---+------+-----+---+------+----+--------+----+
C INELTINSMI ATOKMI KD  IHX I MCIU I HX I        I Q  I
C I    I   I      I     I   I      ICIU I PE‡EPB I    I
C I    I   I      I     I   I      I    I        I    I
C +----+---+------+-----+---+------+----+--------+----+
C I  1 I 2 I   3  I  4  I 5 I   6  I  7 I   8,9  I 10 I
C +----+---+------+-----+---+------+----+--------+----+
C I 11 I12 I  13  I 14  I15 I  16  I 17 I  18,19 I 20 I
C +----+---+------+-----+---+------+----+--------+----+
C
C      BEHTˆ‹œ OTKP›T -> KD=1;    BEHTˆ‹œ ‡AKP›T ->  KD=0;
C      HX - ˜Aƒ O BPEMEHˆ, BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ;
C
C        MCIU  ˆ  HXCIU - APAMETP› "BOCTAHOB‹EHHOƒO" ˆMP“‹œCA "Cˆ“"
C
C  Q - ”‹Aƒ KD;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JRL, L, IRLT, IRLN
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADRMSW , ATOKM
         EQUIVALENCE ( RMASPT(1),MASPRT(1) )
         EQUIVALENCE ( HXMIN , MASPRT(62) )
         EQUIVALENCE ( NWTRE , MASPRT(14) )
         EQUIVALENCE ( GVENT6, MASPRT(59) )
         EQUIVALENCE ( FTEST , MASPRT(60) )
         EQUIVALENCE ( OPEN6 , MASPRT(57) )
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
         INTEGER FKOM, ZAPROS,TIP
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
         TIP = MSW(2)
C--------------------------------------------------------------------
C---->   “‡‹› O„K‹ž—EHˆŸ:
C--------------------------------------------------------------------
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO‹œ TˆA
         IF ( TIP .NE. 93  .AND. TIP .NE. 668 ) GO TO 9999
C--------------------------------------------------------------------
C---> B›OP PE†ˆMA:
C--------------------------------------------------------------------
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C--------------------------------------------------------------------
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "CPOC" ‡HA—EHˆŸ TX - BO‡MO†HOƒO BPEMEHˆ EPEK‹ž—EHˆŸ.....
         RMSW(15) =  H0
         MSW(20) = 0
         KD = MSW(14)
C--->    EC‹ˆ "K" ˆ "M" ˆH–ˆ„EHTH› --->  B›‡BATœ GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECTœ "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG_( K, M, LOGYTR )
         RETURN
6607     CONTINUE
C--------------------------------------------------------------------
C===> BEHTˆ‹œ(K‹ž—) ‡AKP›T ==> KD=0 <=="MAKET" O—EHœ MA‹O‰ POBO„ˆMOCTˆ
C---> EC‹ˆ NVENT6 = 0.0, TO MAKET HE H“†EH...
         IF ( GVENT6 .EQ. 0.0 ) RETURN
         G = GVENT6
C--------------------------------------------------------------------
C--->   O‹EMEHTH›‰ BK‹A„:
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$$ ZAPROS = 10 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->”OPMˆPOBAHˆE "TEHEBO‰"CˆCTEM› „‹Ÿ ƒP“ OTKP›T›X BEHTˆ‹E‰(K‹ž—E‰)
6670     CONTINUE
         KOD = 0
         KD = MSW(14)
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
C======================================================================
C--->  BK‹A„›BAEM....
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$ZAPROS = 20 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C===> PAC—ˆTATœ TOKˆ BEHTˆ‹E‰ !!! B›O‹HŸTœ „O MO„ˆ”ˆKA–ˆˆ VALLN1 !!!!
6660     CONTINUE
         KOD = 0
         ITOK = MSW(7)
         KD = MSW(14)
         IF ( KD .EQ. 0 ) GO TO 6661
C--->  BEHTˆ‹œ(K‹ž—) OTKP›T, B›POCˆTœ "A‡OB›E" “‡‹› O „AHH›M LOGYTR..
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
         TOKN1(ITOK) = TK - TM
         RETURN
6661     CONTINUE
C--->  BEHTˆ‹œ (K‹ž—)  ‡AKP›T....
         TOKN1(ITOK)  =  0.0
         RETURN
C======================================================================
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 OPE„E‹EH›-------
C-----------------------------------------!!! VALLN1 MO„ˆ”ˆ–ˆPOBAHO !!!
C-->>  OPE„E‹ˆTœ CˆƒHA‹ "Cˆ“", ‡AOMHˆTœ MCIU  ˆ HXCIU....
         ITOK = MSW(7)
         ATOKM = MSW(13)
         MCIU  = IFIX ( TOKN1(ATOKM) )
         HXCIU = TOKN1( ATOKM + 6 )
         MSW(16) = MCIU
         TOKXX = 0.0
         IF ( MCIU .EQ. 01  .OR. MCIU .EQ. 11 ) TOKXX = 1.0
         TOKN1( ITOK+1 ) = TOKXX
         RMSW(17) = HXCIU
         IF( TIP .EQ. 668 ) GO TO 6015
C--------------------------------------------------------------------
C----OPE„E‹ˆM TEK. ˆ PE„. COCTOŸHˆŸ (?)----------------------------
C===>    HOBOE HAPŸ†EHˆE ˆ TOK
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         TVT  = TOKN1( ITOK )
C--> A PAHœ˜E ›‹O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         TVN  = TOKN( ITOK )
C======================================================================
C----‡A‰MEMCŸ COCTOŸHˆŸMˆ----------------------------------------------
6015     CONTINUE
         KD = MSW(14)
         IF ( KD .EQ. 0 ) GO TO 601
         IF ( TIP .EQ. 668 ) GO TO 6016
C----------------------------------------------------------------------
C---> BEHTˆ‹œ OTKP›T (KD=1), HE XO—ET ‹ˆ OH ‡AKP›TœCŸ?
C  KOPPEKTˆPOBKA TVN „‹Ÿ Tˆ 93
C----------------------------------------------------------------------
       IF ( TVN .LT. 0.0  .AND. TVN .LE. -ICLOSE ) TVN = 0.0
       IF ( TVT .LT. -ICLOSE ) GO TO 602
C----------------------------------------------------------------------
C---> ˆHA—E COCTOŸHˆE BEHTˆ‹Ÿ HE ˆ‡MEHˆ‹OCœ....
         KOD = 0
         RETURN
6016     CONTINUE
C----------------------------------------------------------------------
C---> K‹ž— OTKP›T (KD=1), HE XO—ET ‹ˆ OH ‡AKP›TœCŸ?
C----------------------------------------------------------------------
       MCIU = MSW(16)
       IF ( MCIU .EQ. 10 .OR. MCIU .EQ. 00 ) GO TO 602
C----------------------------------------------------------------------
C---> ˆHA—E COCTOŸHˆE K‹ž—A HE ˆ‡MEHˆ‹OCœ....
         KOD = 0
         RETURN
602      CONTINUE
C--->  BEHTˆ‹œ(K‹ž—)   #‡AKP›‹CŸ#  B TE—EHˆˆ BPEMEHˆ OT TN „O TN+H0....
C      KOƒ„A TO MOƒ‹O C‹“—ˆTCŸ?   ( HX - PEA‹œHO B›O‹HEHH›‰ ˜Aƒ )....
C !!!! CPABE„‹ˆBO Pˆ TVN = (-ICLOSE) !!!
         IF ( TIP .EQ.  93 ) HX = H0 * ( (-ICLOSE - TVN) /(TVT - TVN))
         IF ( TIP .EQ. 668 ) HX = RMSW(17)
C!!!-------------------------------------------------------------------
         IF ( HX .LT. 0.0 ) WRITE (NWTRE, 604 ) MSW(1), KD, TN, H0, HX
604   FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ "BEHTˆ‹œ(K‹ž—)->COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSW(15) = HX
C--->  EC‹ˆ TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMETˆTœ TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(20) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
         KOD = 0
         IF ( TIP .EQ. 668 ) GO TO 6017
C===> BEHTˆ‹œ ‡AKP›T (KD=0), HE †E‹AET ‹ˆ OH OTKP›TœCŸ?....
         IF( UT .LE. UOPEN ) GO TO 6030
C======================================================================
C--->  —TO „E‹AET "Cˆ“" ???????????????
         MCIU = MSW(16)
         IF ( MCIU .EQ. 01 ) GO TO 603
C-->  MO†ET ›Tœ M› „ABHO "XO—EM" ?
         IF ( MCIU .EQ. 11 ) GO TO 603
C======================================================================
C@@@@@@-->> OT 06.01.87. <<-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  OPAOTKA KOH–A....
       IF ( MCIU .EQ. 00 ) GO TO 6030
C-->   MCIU = 10  -->  Cˆ“ "KOH—AET"
       HX = 0.0
       IF ( UN .GT. UOPEN ) GO TO 606
       HX = +H0 * ( (UOPEN - UN) / (UT-UN) )
       HXCIU = RMSW(17)
       IF ( HX .LE. HXCIU ) GO TO 606
6030   CONTINUE
C@@@@@@--> OT 06.01.87. <<-- KOHE–-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===>  HET, HE BO‡†E‹A‹, ˆ‹ˆ HE „A‹ˆ --> COCTOŸHˆE HE MEHŸETCŸ...
         KOD = 0
         RETURN
6017     CONTINUE
C===> K‹ž— ‡AKP›T (KD=0), HE †E‹AET ‹ˆ OH OTKP›TœCŸ?....
C-> OTKPOETCŸ Pˆ MCIU => 01  ˆ‹ˆ  11
         MCIU = MSW(16)
         IF ( MCIU .EQ. 01  .OR.  MCIU .EQ. 11 ) GO TO 603
C===>  HET, HE BO‡†E‹A‹, COCTOŸHˆE HE MEHŸETCŸ...
         KOD = 0
         RETURN
603      CONTINUE
C======================================================================
C===>   BEHTˆ‹œ(K‹ž—) B TE—EHˆˆ TN  <-->  TN+H0 „O‹†EH OTKP›TœCŸ ...
C       KOƒ„A OŸBˆ‹CŸ "Cˆ“" ???????????
         HXCIU = RMSW(17)
         HX = HXCIU
         IF ( TIP .EQ. 668  ) GO TO 606
         IF ( UN .GT. UOPEN ) GO TO 606
C--->  UN  ˆ  UT  "PA‡HOƒO" ‡HAKA...
C   HX  ->  "˜Aƒ" "ECTECTBEHHO‰" KOM“TA–ˆˆ
         HX  =    + H0 * (  ( UOPEN -  UN ) /(UT - UN)  )
C-->  KOƒ„A EM“ "Cˆ“" PA‡PE˜ˆ‹O...
         IF ( HXCIU .GT. HX ) HX = HXCIU
606      CONTINUE
C!!!-------------------------------------------------------------------
         IF ( HX .LT. 0.0 ) WRITE (NWTRE, 605 ) MSW(1), KD, TN, H0, HX,
     =                                 MCIU
605   FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ "BEHTˆ‹œ(K‹ž—)->COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' MCIU = ', I2, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX  .LT. HXMIN ) HX  = HXMIN
         RMSW(15) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(20) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ MO„E‹ˆ.................ZAPROS = 4
C---->  HET ‹ˆ HEOXO„ˆMOCTˆ POˆ‡BECTˆ CMEH“ COCTOŸHˆŸ BEHTˆ‹Ÿ(K‹ž—A)?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( MSW(20) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC‹ˆ TXG   "PˆMEPHO" PABHO TX=MO„E‹ˆ, TO CMEHA H“†HA!
         HX   = RMSW(15)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> ˆHA—E CMEHˆM COCTOŸHˆE....
         KD = MSW(14)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSW(14) = KD
C---->  EC‹ˆ BEHTˆ‹œ (K‹ž—)  ‡AKP›‹CŸ, ‡A”ˆKCˆP“EM EƒO HOMEP.....
         IF ( KD .EQ. 0 ) NTXG = MSW(1)
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) MSW(1), KD, TN, TXG
501     FORMAT(' ***FTEST_1: B ‹-TE #',I5, ' KD CTA‹O =',I2,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0021 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
          RETURN
         END
