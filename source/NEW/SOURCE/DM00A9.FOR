         SUBROUTINE DM00A9 (K, M, MSWX, RMSWX, YALL, JALL, LOGYTR,
     *                       TOKNX, TOKN1X, VALLN, VALLN1, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM00A9 (K, M,  MSWX, RMSWX, YALL, JALL, LOGYTR,
C   -------------            TOKNX, TOKN1X, VALLN, VALLN1, KOD )
C   HA‡HA—EHˆE:    MOHˆTOP MO„E‹ˆ “P. BEHTˆ‹Ÿ  [ MO„. # A9, .Tˆ # 6 ]
C   -----------    ***   KOMOHEHT C‹O†H›X BEHTE‹œH›X PEOPA‡OBATE‹E‰*
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
C                  ‡AˆCœ TˆX TOKOB B MACCˆB TOKN1X;
C                   !!! B›O‹HŸTœ „O MO„ˆ”ˆKA–ˆˆ PE˜EHˆŸ VALLN1 !!!
C
C=======
C   MACCˆB›  TOKNX  ˆ   TOKN1X ˆMEžT CTP“KT“P“:
C
C   1-E C‹OBO := ‡HA—EHˆE TOKA BEHTˆ‹Ÿ;
C
C   2-E C‹OBO := ‡HA—EHˆE CˆƒHA‹A “PAB‹EHˆŸ ("Cˆ“")1
C
C========
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
C                   - Pˆ TX_G  [MASPRT(35)] .EQ. TX[RMSWX( 5)] -
C                      ”ˆKCˆP“ETCŸ CMEHA COCTOŸHˆŸ BEHTˆ‹Ÿ APAMETPOM
C                      KD [MSWX( 4)];
C                     PABEHCTBO POBEPŸETCŸ C TO—. „O "EPSKOM".
C
C   METO„:         PA‡HOBˆ„HOCTœ BO‹HOBOƒO A‹ƒOPˆTMA „‹Ÿ ”OPMˆPOBAHˆŸ
C   -----          MACCˆBA LOGYTR --> —EPE‡ / GETLOG;
C
C                  "TEHEBAŸ" CˆCTEMA ”OPMˆP“ETCŸ "MAC˜TAˆPOBAHHAŸ"
C                   TAK, —TO: V[X] - V[Y] = <TOK    X-Y   BETBˆ>;
C                   “‡‹›, OTME—EHH›E B LOGYTR(*) = 0 - "A‡OB›E"
C                   „‹Ÿ KA†„O‰ ˆH–ˆ„EHTHO‰ ƒP“› OTKP›T›X BEHTˆ‹E‰;
C
C   APAMETP›:   MSWX, RMSWX - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------   !! B # A9 --> CE–. CTP“KT“P› !!!!!!!!!!!!!!!!!!!!!!!!!
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  T  =  TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  TN + H0;
C                  TOKNX  ˆ TOKN1X - ‡HA—EHˆE EPEMEHH›X "COCTOŸHˆŸ"
C                                  MO„E‹ˆ Pˆ TN  ˆ  TN + H0;
C                  ( PA‡MEPHOCTˆ -> „‹Ÿ A9 -->  „BE EPEMEHH›E !!!!!!!!
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 6;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C                  KOD - APAMETP OPATHO‰ CBŸ‡ˆ C MOHˆTOPOM DM....
C                  K  ˆ  M  “‡‹› O„K‹ž—EHˆŸ BEHTˆ‹Ÿ:
C
C              K --I>--- M
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. GETLOG,  SEQSOS,         DEPOST;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=1, 10,20,3,4 - B›„A—A COO™EHˆŸ ˆ “XO„
C                3. Pˆ FTEST = 1 -> E—ATœ HA WTRE AKTA CMEH› COCTOŸHˆŸ
C
C               PAO—ˆ‰ MACCˆB TˆA  MSW TˆA # A9;
C                ----------------------------------
C
C  +-------+--------+--------+--------+------+
C  I HOMEP I  "KD"  I "HX"   I MMCIU  I  HX  I
C  I O„ - I Pˆ‡-  I BPEMŸ  I KO„ “PI CIU  I
C  I ‹EME I HAK CO-I PE„-O‰I ˆM“‹œ I ˆM-CAI
C  I HTA   I CTOŸHˆŸI KOM-–ˆˆI  CA    I      I
C  +-------+--------+--------+--------+------+
C  I  1    I   2    I   3    I    4   I   5  I
C  +-------+--------+--------+--------+------+
C
C   MMCIU  ˆ  HXCIU OPE„E‹ŸET MOHˆTOP MAKPOMO„E‹ˆ BEHTˆ‹Ÿ......
C
C
C    BEHTˆ‹œ OTKP›T ==>  KD=1;;;;BEHTˆ‹œ ‡AKP›T ==>  KD=0;
C !!!  O‹E MMCIU OT E„HOCTˆ ˆCO‹œ‡“ETCŸ KAK ”‹Aƒ KD  !!!!!
C  MCIU ˆ HXCIU - AP-TP› "BOCTAHOB‹EHHOƒO" ˆM“‹œCA "Cˆ“" ( CM. MCIU01)
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSWX(*), RMSWX(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKNX(*), TOKN1X(*)
         REAL JALL
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
C--->  KOHTPO‹œ TˆA  HE POBO„ˆTCŸ......
C---> B›OP PE†ˆMA:
         IF ( ZAPROS .EQ. 1 ) GO TO 667
         IF ( ZAPROS .EQ. 10 ) GO TO 6670
         IF ( ZAPROS .EQ. 20 ) GO TO 6660
         IF ( ZAPROS .EQ. 3 ) GO TO 600
         IF ( ZAPROS .EQ. 4 ) GO TO 500
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997)         ZAPROS
9997     FORMAT( ' *** DM_00A9 --->', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
         KOD = 0
C---> "CPOC" ‡HA—EHˆŸ TX - BO‡MO†HOƒO BPEMEHˆ EPEK‹ž—EHˆŸ.....
         RMSWX( 3) =      H0
         KD = MSWX( 2)
C--->    EC‹ˆ "K" ˆ "M" ˆH–ˆ„EHTH› --->  B›‡BATœ GETLOG.....
         IF ( KD .EQ. 0 ) GO TO 6607
C--->    ECTœ "KOHTAKT"  [ KD = 1 ]
         CALL GETLOG( K, M, LOGYTR )
         RETURN
6607     CONTINUE
C===> BEHTˆ‹œ ‡AKP›T ==> KD=0 <====== "MAKET" O—EHœ MA‹O‰ POBO„ˆMOCTˆ
C---> EC‹ˆ NVENT6 = 0.0, TO MAKET HE H“†EH...
         IF ( GVENT6 .EQ. 0.0 ) RETURN
C--->
         G = GVENT6
C--->
C--->   O‹EMEHTH›‰ BK‹A„:
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
C---->
         RETURN
C======================================================================
C$$$$$$$ ZAPROS = 10 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  ”OPMˆPOBAHˆE "TEHEBO‰" CˆCTEM› „‹Ÿ ƒP“ OTKP›T›X BEHTˆ‹E‰
6670     CONTINUE
         KOD = 0
         KD = MSWX( 2)
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
C======================================================================
C--->
         CALL DEPOST( K, M, G, 0.0, YALL, JALL )
C--->
         RETURN
C======================================================================
C$$$$$$ZAPROS = 20 $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C===> PAC—ˆTATœ TOKˆ BEHTˆ‹E‰ !!! B›O‹HŸTœ „O MO„ˆ”ˆKA–ˆˆ VALLN1 !!!!
6660     CONTINUE
         KOD = 0
C
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C-->> B›PATœ     CˆƒHA‹ "Cˆ“", ‡AOMHˆTœ MCIU  ˆ HXCIU....
         MMCIU = MSWX(4)
         TOKN1X(2) = 0.0
         IF ( MMCIU .EQ. 01  .OR.  MMCIU .EQ. 11 ) TOKN1X(2) = 1.0
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         KD = MSWX( 2)
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
         TOKN1X( 1  ) = TK - TM
         RETURN
6661     CONTINUE
C--->  BEHTˆ‹œ ‡AKP›T....
         TOKN1X( 1  )  =  0.0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 OPE„E‹EH›-------
C-----------------------------------------!!! VALLN1 MO„ˆ”ˆ–ˆPOBAHO !!!
C----OPE„E‹ˆM TEK. ˆ PE„. COCTOŸHˆŸ (?)------------------------------
C$$$$$$$$
C===>    HOBOE HAPŸ†EHˆE ˆ TOK
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         TVT  = TOKN1X( 1    )
C--> A PAHœ˜E ›‹O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         TVN  = TOKNX( 1    )
C----‡A‰MEMCŸ COCTOŸHˆŸMˆ----------------------------------------------
C!!!-------------------------------------------------------------------
C      KD = MSWX( 2)
C      CALL SEQSOS( MSWX(1), TN, KD, TVN,  UN )
C      MSWX(2 ) = KD
C!!!-------------------------------------------------------------------
         KD = MSWX(2 )
         IF ( KD .EQ. 0 ) GO TO 601
C---> BEHTˆ‹œ OTKP›T (KD=1), HE XO—ET ‹ˆ OH ‡AKP›TœCŸ?
C        IF (  UN .GT. 0.0  .AND. TVN  .GE. 0.0
C    =                     .AND.  TVT  .LT. 0.0  ) GO TO 602
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  KOPPEKTˆPOBKA TVN ********
       IF ( TVN .LT. 0.0  .AND. TVN .LE. -ICLOSE ) TVN = 0.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@$$$@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
       IF (
     =                          TVT .LT. -ICLOSE ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ###QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C---> ˆHA—E COCTOŸHˆE BEHTˆ‹œA HE ˆ‡MEHˆ‹OCœ....
         KOD = 0
         RETURN
602      CONTINUE
C--->  BEHTˆ‹œ #‡AKP›‹CŸ#  B TE—EHˆˆ BPEMEHˆ OT TN „O TN+H0....
C      KOƒ„A TO MOƒ‹O C‹“—ˆTCŸ?   ( HX - PEA‹œHO B›O‹HEHH›‰ ˜Aƒ )....
C !!!! CPABE„‹ˆBO Pˆ TVN = (-ICLOSE) !!!
         HX =    + H0 * (  (-ICLOSE - TVN) /(TVT - TVN)  )
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 604 ) MSWX(1), KD, TN, H0, HX
604   FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ ".BEHT_A9" ==> COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX .LT. HXMIN ) HX = HXMIN
         RMSWX( 3) = HX
C--->  EC‹ˆ TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMETˆTœ TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSWX(4) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
         KOD = 0
C===> BEHTˆ‹œ ‡AKP›T (KD=0), HE †E‹AET ‹ˆ OH OTKP›TœCŸ?....
         IF ( UT .LE. UOPEN ) GO TO 6030
CQQQQQQQQQQ$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  —TO „E‹AET "Cˆ“" ???????????????
         MMCIU = MSWX( 4)
         IF (                      MMCIU .EQ. 01 ) GO TO 603
C-->  MO†ET ›Tœ M› „ABHO "XO—EM" ?
         IF (                      MMCIU .EQ. 11 ) GO TO 603
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C@@@@@@-->> OT 06.01.87. <<-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  OPAOTKA KOH–A....
       IF (                     MMCIU .EQ. 00 ) GO TO 6030
C-->  MMCIU = 10  -->  Cˆ“ "KOH—AET"
       HX = 0.0
       IF ( UN .GT. UOPEN ) GO TO 606
       HX = +H0 * ( (UOPEN - UN) / (UT-UN) )
       HXCIU = RMSWX(5)
       IF ( HX .LE. HXCIU ) GO TO 606
6030   CONTINUE
C@@@@@@--> OT 06.01.87. <<-- KOHE–-@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===>  HET, HE BO‡†E‹A‹, ˆ‹ˆ HE „A‹ˆ --> COCTOŸHˆE HE MEHŸETCŸ...
         KOD = 0
         RETURN
603      CONTINUE
C===>   BEHTˆ‹œ B TE—EHˆˆ TN  <-->  TN+H0 „O‹†EH OTKP›TœCŸ ...
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C       KOƒ„A OŸBˆ‹CŸ "Cˆ“" ???????????
         HXCIU = RMSWX( 5)
         HX = HXCIU
         IF ( UN .GT. UOPEN ) GO TO 606
C--->  UN  ˆ  UT  "PA‡HOƒO" ‡HAKA...
C   HX  ->  "˜Aƒ" "ECTECTBEHHO‰" KOM“TA–ˆˆ
         HX  =    + H0 * (  ( UOPEN -  UN ) /(UT - UN)  )
C-->  KOƒ„A EM“ "Cˆ“" PA‡PE˜ˆ‹O...
         IF ( HXCIU .GT. HX ) HX = HXCIU
606      CONTINUE
C--->
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C!!!-------------------------------------------------------------------
         IF ( HX .LE. 0.0 ) WRITE (NWTRE, 605 ) MSWX(1), KD, TN, H0, HX
605   FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ "BEHT_A9  " ==> COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =  ' HX = ', E12.5,  ' ***')
C!!!-------------------------------------------------------------------
         IF ( HX  .LT. HXMIN ) HX  = HXMIN
         RMSWX( 3) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSWX(4) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
           KOD = 0
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ MO„E‹ˆ.................ZAPROS = 4
C---->  HET ‹ˆ HEOXO„ˆMOCTˆ POˆ‡BECTˆ CMEH“ COCTOŸHˆŸ BEHTˆ‹œA...?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( MSWX(4) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC‹ˆ TXG   "PˆMEPHO" PABHO TX=MO„E‹ˆ, TO CMEHA H“†HA!
         HX   = RMSWX( 3)
C        TX - TXG == HX - HXG
         EPST = ABS( HX   - HXG )
         IF ( EPST .GT. EPSKOM ) RETURN
C===> ˆHA—E CMEHˆM COCTOŸHˆE....
         KD = MSWX( 2)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSWX( 2) = KD
C---->  EC‹ˆ BEHTˆ‹œ ‡AKP›‹CŸ, ‡A”ˆKCˆP“EM EƒO HOMEP.....
         IF ( KD .EQ. 0 ) NTXG = MSWX(1)
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) MSWX(1), KD, TN, TXG
501     FORMAT(' ***FTEST_1: B ‹-TE #',I5, ' KD CTA‹O =',I2,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
