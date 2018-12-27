         SUBROUTINE DM0006 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0006 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA‡HA—EHˆE:    MOHˆTOP MO„E‹ˆ R_L „ˆO„A [ MO„E‹œ # 80, .Tˆ # 5 ]
C   -----------    ***************************************************
C
C                  B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C                  B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰ ˆ
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA ‡HA—EHˆ‰ APAMETPOB
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTA –Eˆ
C                           TˆA # 80 [ RL _ „ˆO„ ];
C                           ***********************
C                  B) ZAPROS = 2
C                     ----------
C                  B›—ˆC‹EHˆE ‡HA—EHˆ‰ TOKOB B BETBŸX B T(N+1) MOMEHT
C                  BPEMEHˆ   ˆCXO„Ÿ ˆ‡ G ˆ J APAMETPOB ˆ ‡HA—EHˆŸ
C                  “‡‹OB›X HAPŸ†EHˆ‰ HA ‹EMEHTE PAC—ˆTAHH›X „‹Ÿ T(N+1)
C                    ˆ ‡AˆCœ (”ˆKCA–ˆŸ) TˆX ‡HA—EHˆ‰ B "TOK T(N+1)";
C                    ( E‡ "AAPATA" LOGYTR )
C
C                  C) ZAPROS = 3
C                     -----------
C                     Oˆ‡BO„ˆTCŸ AHA‹ˆ‡ HA BO‡MO†H“ž CMEH“ COCTOŸHˆŸ
C                     „ˆO„A [ OTKP›T <==> ‡AKP›T ]:
C                     Pˆ TOM: FKOM = 0 - COCTOŸHˆE HE ˆ‡MEHˆ‹OCœ;
C                               FCOM = 1 - HA TEK“™EM ˜AƒE COCTOŸHˆE
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
C                             Tˆ ‡HA—EHˆŸ “—ˆT›BAžTCŸ KAK ˆ Pˆ
C                             CMEHE COCTOŸHˆŸ ‹EMEHTA, TAK ˆ Pˆ
C                            OPE„E‹EHˆˆ BO‡MO†H›X BPEMEH EPEK‹ž—EHˆŸ;
C
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - Pˆ TX_G  [MASPRT(35)] .EQ. TX[RMSW(19)] -
C                      ”ˆKCˆP“ETCŸ CMEHA COCTOŸHˆŸ "„ˆO„A" APAMETPOM
C                      KD [MSW(18)];
C                     PABEHCTBO POBEPŸETCŸ C TO—. „O "EPSKOM".
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
C   ------------   2. SEQSOS
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=  1, 2, 3, 4 - B›„A—A COO™EHˆŸ ˆ “XO„
C                3. Pˆ FTEST >=1 -> E—ATœ HA WTRE AKTA CMEH› COCTOŸHˆŸ
C                4. „O“CKAETCŸ ‡A„AHˆE L[0] ˆ L[@] = 0.0;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 80:
C                --------------------------------
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
C                  RMSW TˆA # 80 [ RL _ „ˆO„ ]
C                  --------------------------------
C
C
C
C                    SPM                      SRAB
C +----+---++---+---+---+--++----+---+-------+------+--------+--------+
C I“‡- I“‡-II R I L I R I L II G I J I ”‹Aƒ  I TEK .I BPEMŸ  I PE‡EPB I
C IE‹  IE‹ II 0 I 0 I @ I @ II R I R I  KD   I COCT.I EPEK‹.I        I
C I"K" I"M"II   I   I   I   II L I L I       I "KD" I "TX"[*]I        I
C +----+---++---+---+---+---++---+---+-------+------+--------+--------+
C I 9  I 10II 11I 12I 13I 14II 15I 16I   17  I  18  I   19   I   20   I
C +----+---++---+---+---+---++---+---+-------+------+--------+--------+
C
C  0 - „ˆO„ OTKP›T (KD=1); @ - „ˆO„ ‡AKP›T (KD=0);
C [*] - XPAHˆTCŸ ‡HA—EHˆE PEA‹œHO B›O‹HEHHOƒO ˜AƒA HX ==> TX = TN + HX
C ”‹Aƒ KD - PA‡PE˜EHˆE HA CMEH“ COCTOŸHˆŸ;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JRL, L, IRLT, IRLN
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
               DIMENSION MSWALL(*), RMSWAL(*)
               INTEGER ADRMSW
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(60), FTEST )
         INTEGER FTEST
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( HXG,    MASPRT(61) )
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
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
         INTEGER FKOM, ZAPROS
C===>    BCTPOEHHAŸ ”“HK–ˆŸ OC‹“†ˆBAHˆŸ „B“XMEPH›X MACCˆBOB:
         ID(I,J) = NDALL * (J-1)  +  I
C---->   “‡‹› O„K‹ž—EHˆŸ:
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO‹œ TˆA
         IF ( MSW(2) .NE. 80 ) GO TO 9999
C---> B›OP PE†ˆMA:
         GO TO ( 667, 666, 600, 500       ), ZAPROS
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
C---> "CPOC" ‡HA—EHˆŸ HX - BO‡MO†HOƒO BPEMEHˆ EPEK‹ž—EHˆŸ.....
         RMSW(19) =      H0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(17) = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===> —TO ‡A COCTOŸHˆE ? (KD=0 -- ‡AKP›T, KD=1 -- OTKP›T)?
         IF (  MSW(18) .EQ. 1 ) GO TO 698
C===> „ˆO„ ‡AKP›T==> KD=0 <======
         R = RMSW(13)
         L = RMSW(14)
         GO TO 555
698      CONTINUE
C===> „ˆO„ OTKP›T ==> KD=1 <======
         R = RMSW(11)
         L = RMSW(12)
555      CONTINUE
C---> TEEPœ BCE E„ˆHO....
C====> PAC—ET GRL  ˆ JRL     ”OPM“‹› ˆ‡MEHEH›:= MO†HO L = 0.0
         GRL = H1 / ( L + H1 * R )
C--->   TOK HA N - OM  ˜AƒE
         ITOK = MSW(7)
         TOK  = TOKN( ITOK )
C--->
         JRL = (L*TOK) / (L+H1*R)
         IF ( H2 .EQ. 0.0 ) GO TO 668
C===>  HAPŸ†EHˆE HA N-OM ˜AƒE.
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
C--->
         JRL = JRL + H2*( (UN-R*TOK)/(L+H1*R) )
668      CONTINUE
C===> ‡AOMHˆTœ G ˆ J APAMETP› „ˆO„A;
         RMSW(15) = GRL
         RMSW(16) = JRL
C--->
C--->   O‹EMEHTH›‰ BK‹A„:
C---->
C======================================================================
         KOD = 0
C--->
         CALL DEPOST( K, M, GRL, JRL, YALL, JALL )
C--->
         RETURN
C======================================================================
C$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C===> PAC—ˆTATœ TOKˆ
         GRL = RMSW(15)
         JRL = RMSW(16)
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
C---> TOK B T(N+1) MOMEHT BPEMEHˆ:
         IRLT = GRL * UT  + JRL
C===>        ‡AˆCœ TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = IRLT
C********
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 OPE„E‹EH›-------
C----OPE„E‹ˆM TEK. ˆ PE„. COCTOŸHˆŸ (?)------------------------------
         ITOK = MSW(7)
C===>
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         IRLT = TOKN1( ITOK )
C--> A PAHœ˜E ›‹O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         IRLN = TOKN( ITOK )
C----‡A‰MEMCŸ COCTOŸHˆŸMˆ----------------------------------------------
C!!!-------------------------------------------------------------------
C      KD = MSW(18)
C      CALL SEQSOS( MSW(1), TN, KD, IRLN, UN )
C      MSW(18) = KD
C!!!-------------------------------------------------------------------
         KD = MSW(18)
         IF ( KD .EQ. 0 ) GO TO 601
C---> „ˆO„ OTKP›T (KD=1), HE XO—ET ‹ˆ OH ‡AKP›TœCŸ?
C        IF (  UN .GE 0.0  .AND.  IRLN .GE. 0.0
C    =                     .AND.  IRLT .LT. 0.0  ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
       IF (
     =                          IRLT .LE. -ICLOSE ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C---> ˆHA—E COCTOŸHˆE „ˆO„A HE ˆ‡MEHˆ‹OCœ....
         KOD = 0
         RETURN
602      CONTINUE
C--->  „ˆO„ #‡AKP›‹CŸ#  B TE—EHˆˆ BPEMEHˆ OT TN „O TN+H0....
C      KOƒ„A TO MOƒ‹O C‹“—ˆTCŸ?
C !!! ”OPM“‹A CPABE„‹ˆBA Pˆ  IRLN > (-ICLOSE) !!!
         HX =    + H0 * ( (-ICLOSE - IRLN ) /(IRLT-IRLN)  )
         IF ( HX .LT. HXMIN ) HX = HXMIN
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF (  HX .LE. 0.0 ) WRITE (NWTRE, 604 ) MSW(1), KD, TN, H0, HX
604     FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ "RL-„ˆO„" ==> COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =   ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RMSW(19) = HX
C--->  EC‹ˆ TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMETˆTœ TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(17) = -1
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
C       KOƒ„A ??? +!!!!!!!  CPABE„‹ˆBO „‹Ÿ  UN < UOPEN !!!!!!!!
         HX =    + H0 * ( (UOPEN - UN) /(UT - UN)  )
         IF ( HX .LT. HXMIN ) HX = HXMIN
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
        IF ( HX   .LE. 0.0 ) WRITE (NWTRE, 605 ) MSW(1), KD, TN, H0, HX
605     FORMAT(' *** ‹EMEHT # ', I5, ' MO„E‹œ "RL-„ˆO„" ==> COMHEHˆŸ ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     = ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RMSW(19) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
         KOD = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(17) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ MO„E‹ˆ.................ZAPROS = 4
C---->  HET ‹ˆ HEOXO„ˆMOCTˆ POˆ‡BECTˆ CMEH“ COCTOŸHˆŸ „ˆO„A...?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( MSW(17) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC‹ˆ TXG   "PˆMEPHO" PABHO TX=MO„E‹ˆ, TO CMEHA H“†HA!
         HX   = RMSW(19)
         EPST = ABS( HX   - HXG )
C----->   (TX - TXG) = [ (TN+HX) - (TN+HXG) ]
         IF ( EPST .GT. EPSKOM ) RETURN
C===> ˆHA—E CMEHˆM COCTOŸHˆE....
         KD = MSW(18)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSW(18) = KD
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
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0006 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
         END
