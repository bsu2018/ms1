         SUBROUTINE DM0008 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0008 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    MOHˆTOP MO„E‹ˆ BEHTˆ‹Ÿ  [ MO„E‹œ # 8, .Tˆ # 6 ]
C   -----------    **************************************************
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
C                    ˆ Pˆ EPEK‹ž—EHˆˆ ˆ Pˆ HAXO†„EHˆˆ HX;
C
C                  D) ZAPROS = 4
C                     **********
C                   - Pˆ TX_G  [MASPRT(35)] .EQ. TX[RMSW(12)] -
C                      ”ˆKCˆP“ETCŸ CMEHA COCTOŸHˆŸ BEHTˆ‹Ÿ APAMETPOM
C                      KD [MSW(11)];
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
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 8;
C                -------------------------------
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
C                  RMSW TˆA # 8  [ ˆ„EA‹œH›‰ BEHTˆ‹œ ]
C                  --------------------------------
C
C
C
C                              SRAB
C +----+---++-------+----------------+-------+------+
C I“‡- I“‡-II TEK.  I BPEMŸ PE„-‰   I ”‹Aƒ  I PE‡E I   [*] - XPAHˆTCŸ
C IE‹  IE‹ II COCT. I KOMM“TA–ˆˆ     I  KD   I  PB  I         ‡HA—EHˆE
C I"K" I"M"II "KD"  I    "TX"  [*]   I       I      I        HX - PEA‹œ-
C +----+---++-------+----------------+-------+------+        HO B›O‹HE-
C I 9  I 10II  11   I      12        I 13    I  14  I        HH›‰ ˜Aƒ:
C +----+---++-------+---+---++---+---+-------+------+        TX=TN+HX;
C
C      „ˆO„ OTKP›T ==>  KD=1       „ˆO„ ‡AKP›T ==>  KD=0;
C  ”‹Aƒ KD - PA‡PE˜EHˆE CMEH› COCTOŸHˆŸ;
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JRL, L, IRLT, IRLN
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(59), GVENT6 )
         EQUIVALENCE ( MASPRT(60), FTEST )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
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
         INTEGER FKOM, ZAPROS
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
         DIMENSION MSWALL(*), RMSWAL(*)
       INTEGER ADRMSW
C===>    BCTPOEHHAŸ ”“HK–ˆŸ OC‹“†ˆBAHˆŸ „B“XMEPH›X MACCˆBOB:
         ID(I,J) = NDALL * (J-1)  +  I
C---->   “‡‹› O„K‹ž—EHˆŸ:
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO‹œ TˆA
         IF ( MSW(2) .NE. 8 ) GO TO 9999
C---> B›OP PE†ˆMA:
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "CPOC" ‡HA—EHˆŸ TX - BO‡MO†HOƒO BPEMEHˆ EPEK‹ž—EHˆŸ.....
         RMSW(12) =      H0
         MSW(13) = 0
         KD = MSW(11)
C--->    EC‹ˆ "K" ˆ "M" ˆH–ˆ„EHTH› --->  B›‡BATœ GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECTœ "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG_( K, M, LOGYTR )
         RETURN
6607     CONTINUE
C===> BEHTˆ‹œ ‡AKP›T ==> KD=0 <====== "MAKET" O—EHœ MA‹O‰ POBO„ˆMOCTˆ
C---> EC‹ˆ NVENT6 = 0.0, TO MAKET HE H“†EH...
         IF ( GVENT6 .EQ. 0.0 ) RETURN
         G = GVENT6
C--->
C--->   O‹EMEHTH›‰ BK‹A„:
         CALL DEPOST(K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$$ ZAPROS = 10 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  ”OPMˆPOBAHˆE "TEHEBO‰" CˆCTEM› „‹Ÿ ƒP“ OTKP›T›X BEHTˆ‹E‰
6670     CONTINUE
         KOD = 0
         KD = MSW(11)
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
C--->  BK‹A„›BAEM....
         CALL  DEPOST(K, M, G, 0.0, YALL, JALL )
         RETURN
C======================================================================
C$$$$$$ZAPROS = 20 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C===> PAC—ˆTATœ TOKˆ BEHTˆ‹E‰ !!! B›O‹HŸTœ „O MO„ˆ”ˆKA–ˆˆ VALLN1 !!!!
6660     CONTINUE
         KOD = 0
         ITOK = MSW(7)
         KD = MSW(11)
         IF ( KD .EQ. 0 ) GO TO 6661
C--->  BEHTˆ‹œ OTKP›T, B›PACˆTœ "A‡OB›E" “‡‹› O „AHH›M LOGYTR....
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
C--->  BEHTˆ‹œ ‡AKP›T....
         TOKN1(ITOK)  =  0.0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 OPE„E‹EH›-------
C-----------------------------------------!!! VALLN1 MO„ˆ”ˆ–ˆPOBAHO !!!
C----OPE„E‹ˆM TEK. ˆ PE„. COCTOŸHˆŸ (?)------------------------------
         ITOK = MSW(7)
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
C----‡A‰MEMCŸ COCTOŸHˆŸMˆ----------------------------------------------
C!!!-------------------------------------------------------------------
C      KD = MSW(11)
C      CALL SEQSOS( MSW(1), TN, KD, TVN,  UN )
C      MSW(11) = KD
C!!!-------------------------------------------------------------------
         KD = MSW(11)
         IF ( KD .EQ. 0 ) GO TO 601
C---> „ˆO„ OTKP›T (KD=1), HE XO—ET ‹ˆ OH ‡AKP›TœCŸ?
C        IF (  UN .GT. 0.0  .AND. TVN  .GE. 0.0
C    =                     .AND.  TVT  .LT. 0.0  ) GO TO 602
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  KOPPEKTˆPOBKA TVN ********
       IF ( TVN .LT. 0.0  .AND. TVN .LE. -ICLOSE ) TVN = 0.0
       IF ( TVT .LT. -ICLOSE ) GO TO 602
C---------------------------------------------------------------------
C---> ˆHA—E COCTOŸHˆE „ˆO„A HE ˆ‡MEHˆ‹OCœ....
         KOD = 0
         RETURN
602      CONTINUE
C--->  „ˆO„ #‡AKP›‹CŸ#  B TE—EHˆˆ BPEMEHˆ OT TN „O TN+H0....
C      KOƒ„A TO MOƒ‹O C‹“—ˆTCŸ?   ( HX - PEA‹œHO B›O‹HEHH›‰ ˜Aƒ )....
C !!!  ”OPM“‹A BEPHA Pˆ TVN > (-ICLOSE)
         HX =    + H0 * ( (-ICLOSE - TVN ) /(TVT - TVN)  )
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 604 ) MSW(1), KD, TN, H0, HX
604   FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ "BEHTˆ‹œ_8" ==> COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSW(12) = HX
C--->  EC‹ˆ TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMETˆTœ TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          MSW(13) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
C===> „ˆO„ ‡AKP›T (KD=0), HE †E‹AET ‹ˆ OH OTKP›TœCŸ?....
C        IF ( UT .GT. 0.0 ) GO TO 603
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
       IF ( UT .GT. UOPEN ) GO TO 603
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C===>  HET, HE BO‡†E‹A‹, COCTOŸHˆE HE MEHŸETCŸ...
         KOD = 0
         RETURN
603      CONTINUE
C===>   „ˆO„ B TE—EHˆˆ TN  <-->  TN+H0 „O‹†EH OTKP›TœCŸ ...
C       KOƒ„A ??? !!! „‹Ÿ  UN < UOPEN !!!!!
         HX =    + H0 * ( ( UOPEN -  UN ) /(UT - UN)  )
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 605 ) MSW(1), KD, TN, H0, HX
605   FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ "BEHTˆ‹œ_8" ==> COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5,  ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSW(12) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
          MSW(13) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ MO„E‹ˆ.................ZAPROS = 4
C---->  HET ‹ˆ HEOXO„ˆMOCTˆ POˆ‡BECTˆ CMEH“ COCTOŸHˆŸ „ˆO„A...?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( MSW(13) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC‹ˆ TXG   "PˆMEPHO" PABHO TX=MO„E‹ˆ, TO CMEHA H“†HA!
         HX   = RMSW(12)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> ˆHA—E CMEHˆM COCTOŸHˆE....
         KD = MSW(11)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSW(11) = KD
C---->  EC‹ˆ BEHTˆ‹œ ‡AKP›‹CŸ, ‡A”ˆKCˆP“EM EƒO HOMEP.....
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
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0008 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
