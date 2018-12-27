         SUBROUTINE DIGITA( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     *            YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.5  (OT ABÉKCTA 1989 É.)------------
C   èOÑèPOÉPAMMA:  DIGITA( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
C   ------------      YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
C
C   HAáHAóEHàE:    OÅôEE ìèPABãEHàE èPOñECCOM PACóETA ùãEKTPàóECKOâ
C   ----------     ñEèà; àCXOÑHOE OèàCAHàE ñEèà COÑEPÜàTCü B CèàCKE
C                   MSW_ALL;
C
C                  == èPà FRUNGE = 1 BõèOãHEHàE "TECTA" PìHÉE ;
C                  == TECT áAèPAòàBAETCü èOCãE KAÜÑOâ KOMMìTAñàà ===
C                     èPà HEXBATKE MITER àTEPAñàâ Ñãü EITER "ãOKAãúHOâ"
C                     OòàÅKà - CMEHA H0DATA HE PEAãàáOBAHA
C                  == B GETNEW ùTO PEAãàáOBAHO;
C
C              == èPà HXMIN > H0DATA -> ìCTAHOBKA HXMIN=H0DATA/10;
C              ==
C              == èPà METVET=1 à HXMIN=0 -> ìCTAHOBKA HXMIN=H0DATA/100.0
C
C
C                  == àáMEHEHO ìèPABãEHàE CãìÜÅOâ BPEMEHà ---->
C                     BCEÉÑA CóET C H0DATA, èPà "èPOCKOKE" óEPEá
C!!! C TOóHOCTúû !!!  TOóKì BõBOÑA -> ãàHEâHAü àHTEPèOãüñàü K T=TOUT;
C!!! ÑO H0MIN !!!    == èPà IPOL = 1 --> CèEñ. àHTEPèOãüñàü K T=TOUT;
C                  == K.T. COáÑAETCü B CETKE T0+HOUTT à èPà RESTARTE
C                     (FREST=1) èìCK C àHTEPèOãàPOBAHHOÉO áHAóEHàü,
C                      óTO MOÜET BHECTà OòàÅKì (?????) -> ùTO HE üCHO;
C              === èPà BOáÅìÜÑEHàà îãAÉA FREST [MASPRT(45)] -->
C                  áAèPOC HA èPOÑOãÜEHàE PEòEHàü àá TOóKà TN....
C                  èPà áTOM èPEÑèOãOÉAETCü, óTO BCE ÑAHHõE àá MASPRT
C                  BOCTAHOBãEHHõ;
C              ===
C
C              == èPà FCHEK [MASPRT(91)] è/è WRIT00 COáÑAET
C                K.T. ABTOMATàóECKà HA îAâã NFACHK[MASPRT(97)];
C
C                 DIGITA Ñãü ùTOÉO OèPEÑEãüET èOãE TACHEK -->
C                 BPEMü COáÑAHàü OóEPEÑHOâ K.T. à
C                 KOPPEKTàPìET èOãE DTCHEK -> ÑEãAET EÉO
C                 áHAóEHàE KPATHõM "HOUTT"
C                      *** èPà FCHEK = 1 ---->
C                 B KOHñE PACóETA COáÑAETCü K.T. HA NFACHK
C                 BHE áABàCàMOCTà OT DTCHEK;
C              ==
C
C              ==  èEPBOE CãOBO èEPBOâ áAèàCà HA NSPOOL -->
C                  áHAóEHàE HOUTT;
C              ==
C
C    @@@@@@@@@@@@@--> "KOHTPOãú" CãìÜÅõ BPEMEHà ÅãOKàPOBAH;
C
C    $$$ OT 8.02.89 èPà  T0OUT(MASPRT(115)->REAL*4) > 0.0 BõBOÑ B
C    $$$$$$$$$$ îAâã HAKOèãEHàü (SPOOL) BõèOãHüETCü èPà  TN >= T0OUT
C    $$$ OT 3.08.89 èPà T0OUT ^=0 à H0OUTT(MASPRT(116) B TOóKE
C    $$$$$$$$$$ T0OUT ìCTAHABãàBAETCü òAÉ BõBOÑA HOUTT = H0OUTT
C    $$$$$$$$$$ èPà HEOÅXOÑàMOCTà KOPPEKTàPìETCü H0DATA.......
C    $$$$$$$$$$  èPà áAèPOCE #ACOX COXPAHEHàE àCXOÑü àá CTAPOÉO
C    $$$$$$$$$$  áHAóEHàü HOUTT
C    $$$=> T0OUT=T0OUT-H0DATA(CTPAXOBKA)
C    $$$=> OUTFLG=0(HET),=1(TOãúKO T0HOUT),=2(MEHüEM H0DATA...)
C
C   METOÑ:         - èOCãEÑOBATEãúHOE àCèOãúáOBAHàE è/è "BõóàCãàTú
C   -----            COCTOüHàE ñEèà Ñãü T = TN + H0" Ñãü BCEÉO àHTEPBAãA
C                    PACóETA;  ìèPABãEHàE METOÑOM óàCEHHOÉO àHTEÉPàPO-
C                    BAHàü èPOBOÑàTCü HA OCHOBAHàà îãAÉA HAãàóàü
C                    KOMMìTAñàà HA OóEPEÑHOM òAÉE àHTEÉPàPOBAHàü;
C
C   !!!!!!!!!!== èPà KODMU = 1 --->  "PAáÉOH" HE èPOBOÑàTCü ===
C
C                  - BõèOãHüETCü ìèPABãEHàE BõBOÑOM àHîOPMAñàà B
C                    îAâãõ HAKOèãEHàü;
C
C
C              === èPà NSYS = 1 ---> BCEÉÑA KODSYS=1; ====
C
C
C   èAPAMETPõ:     MSWALL - CîOPMàPOBAHHõâ CèàCOK CãOB COCTOüHàü
C   ---------               MOÑEãEâ ùãEMEHTOB ñEèà;
C                  VALLN, TOKN - HAèPüÜEHàü à "TOKà" ñEèà èPà
C                                T = T0, T.E. B HAóAãúHõâ MOMEHT BPEMEHà
C                                B èOCãEÑCTBàà - B TEKìôàâ "CTAPTOBõâ"
C                                MOMEHT BPEMEHà T.E. B T = TN;
C                  VALLN1 à TOKN1 - TO ÜE Ñãü  TN = TN + H0;
C                  YALL - MATPàñõ ìáãOBõX èPOBOÑàMOCTEâ;
C                  JALL - BEKTOP àCTOóHàKOB TOKA;
C                  LOGYTR - MACCàB èPàáHAKOB K.á. ìáãOB;
C                           ( Ñãü MAKPOMOÑEãEâ àÑEAãúHõX BEHTàãEâ );
C                   WORKX - PAÅ. MACCàB PAáMEPHOCTúû NSYS;
C                   Ñãü METOÑA PEòEHàü CãAì KODSYS=2 HEOÅXOÑàMO:
C                   YALLHP(NDALL,NDALL) - KOèàü YALL à
C                   WORKY( >= NSYS ) - PAÅ. MACCàB.
C
C
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, PROF2, PROF3;
C   -------------  2. GETNEW, WRIT00, SVCHEK
C
C   èPàMEóAHàE:    1. èPà áABEPòEHàà PACóETA îãAÉ FEND [MASPRT(55)] = 1;
C   ----------     2. áAèOãHüET èOãü: TN, H0, H1, H2, NSYS, ZEROCP, FEND
C                     KKCHEK      B "MASPRT";
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(1)
         COMMON /PROF3/ FUNTAB(1)
         DIMENSION RMASPT(1)
         DIMENSION MSWALL(1), TOKN(1), TOKN1(1), VALLN(1), VALLN1(1)
         DIMENSION YALL(1), JALL(1), LOGYTR(1), WORKX(1), YALLHP(1)
         DIMENSION WORKY(1)
         REAL JALL, MU
         INTEGER FEND, FKOM, PON, FRUN0
         EQUIVALENCE ( NNTOK, MASPRT(46) )
         EQUIVALENCE ( FRUN0, MASPRT(82) )
         EQUIVALENCE ( H0OLD, MASPRT(81) )
         EQUIVALENCE ( NDIVH0, MASPRT(84) )
         EQUIVALENCE ( SEQTIM, MASPRT(71) )
         EQUIVALENCE ( H0MIN,  MASPRT(72) )
         EQUIVALENCE ( NRUNGE, MASPRT(70) )
         EQUIVALENCE ( FRUNGE, MASPRT(68) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( MITER,  MASPRT(66) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( FREST,   MASPRT(45) )
         EQUIVALENCE ( TOUT,    MASPRT(44) )
         EQUIVALENCE ( FCHEK,   MASPRT(91) )
         EQUIVALENCE ( NFACHK,  MASPRT(97) )
         EQUIVALENCE ( KKCHEK,  MASPRT(96) )
         EQUIVALENCE ( DTCHEK,  MASPRT(90) )
         EQUIVALENCE ( METVET,  MASPRT(107) )
         EQUIVALENCE ( HXMIN,   MASPRT(62) )
         EQUIVALENCE ( EPSKOM,  MASPRT(39) )
         EQUIVALENCE ( GVENT6,  MASPRT(59) )
         EQUIVALENCE ( IPOL,    MASPRT(108) )
         EQUIVALENCE ( KODSYS,  MASPRT(109) )
         EQUIVALENCE ( EPSSYS,  MASPRT(110) )
         EQUIVALENCE ( KODMU,   MASPRT(113) )
         EQUIVALENCE ( KODMUM,  MASPRT(114) )
         EQUIVALENCE ( T0OUT ,  MASPRT(115) )
         EQUIVALENCE ( H0OUTT,  MASPRT(116) )
         EQUIVALENCE ( H0DATX,  MASPRT(117) )
         EQUIVALENCE ( OUTFLG,  MASPRT(118) )
C ----->
         INTEGER FITER, FRUNGE, FRUNDG, SEQTIM, FREST, FCHEK, OUTFLG
C--->  FRUGDG - îãAÉ ìèPABãEHàü....
         EQUIVALENCE ( MASPRT(29), TN ),
     =               ( MASPRT(30), H0 ),
     =               ( MASPRT(31), MU ),
     =               ( MASPRT(32), H1 ),
     =               ( MASPRT(33), H2 ),
     =               ( MASPRT(40), H0DATA ),
     =               ( MASPRT(42), T0 ),
     =               ( MASPRT(43), TMAX ),
     =               ( MASPRT(50), ZEROCP ),
     =               ( MASPRT(55), FEND ),
     =               ( MASPRT(48), HOUTT ),
     =               ( MASPRT(13), NWTR ),
     =               ( MASPRT(14), NWTRE ),
     =               ( MASPRT(35), TXG ),
     =               ( MASPRT(61), HXG ),
     =               ( MASPRT(23), NDALL ),
     =               ( MASPRT(56), PON   ),
     =               ( MASPRT(34), FKOM )
C======================================================================
C**********************************************************************
C@@@@@@  CÅPOC "PACóET OKOHóEH":
       FEND = 0
C-->  BõÅOP KKCHEK èPà FCHEK = 1
       IF ( FCHEK .EQ. 0 ) GO TO 1
       KKCHEK = INT( DTCHEK/HOUTT )
       IF ( KKCHEK .LE. 0 ) KKCHEK = 1
       DTCHEK = KKCHEK * HOUTT
       WRITE( NWTR , 100) DTCHEK, KKCHEK
       WRITE( NWTRE, 100) DTCHEK, KKCHEK
100    FORMAT(/' *** òAÉ Ñãü "#ACOXP" ìCTAHOBãEH PABHõM ', G12.5,
     = ' C. (óEPEá ', I5, ' TOóEK) ***'/)
1      CONTINUE
C@@@@@ KOHTPOãú @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( HXMIN .GT. H0DATA ) WRITE(NWTRE,3456) HXMIN
       IF ( HXMIN .GT. H0DATA ) HXMIN = H0DATA / 10.0
3456   FORMAT(/' *** áHAóEHàE HXMIN =',G12.5,' OóEHú ÅOãúòOE ***'/)
C@@@
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) WRITE(NWTRE,3457)
3457   FORMAT(/' *** èPà METVET=1 áAÑAHO HXMIN=0 --> èEPEOèPEÑEãüETCü',
     = ' HA 1% OT H0DATA ***'/)
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) HXMIN = H0DATA / 100.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  ECãà BXOÑ èO K.T., TO HAó. ìCTAHOBKà HE ÑEãATú !!!
        IF ( FREST .EQ. 1 ) GO TO 2
C???????????????????????????????????????????????????????????????????????
C-->  HAóAãúHõE ìCTAHOBKà....
       OUTFLG=0
       HOUTTX = HOUTT
       H0DATX = H0DATA
C óTO áA CàTìAñàü....
C  OUTFLG = 1 -  TOãúKO OTãOÜEHHõâ BõBOÑ B SPOOL....
C  OUTFLG = 2 -  OTãOÜEHHõâ BõBOÑ B SPOOL + CMEHE HOUTT ( óEPEá T0OUT)
C  OUTFLG = 2 -  TOÜE, óTO à "2" + CMEHA H0DATA.....
       IF ( T0OUT .LE. T0  ) GO TO 11110
C$$ óTOÅõ HE èPCKOóàTú T0OUT.......
       T0OUT = T0OUT - H0DATA
C --> ÑPìÉOE HAóAãO BPEMEHà BõBOÑA, EÉO îàKCàPìEM....
       OUTFLG=1
C  ÅìÑEM ãà MEHüTú òAÉ BõBOÑA.....
       IF ( H0OUTT .EQ. 0.0 ) GO TO 11110
C ÑA, èPOCüT HOBõâ òAÉ BõBOÑA, HAÑO ãà CMEHàTú H0DATA ???????
       OUTFLG = 2
       IF ( H0OUTT .LT. H0DATA ) H0DATX = H0OUTT
       IF ( H0OUTT .LT. H0DATA ) OUTFLG = 3
C  Ñãü OPEN SPOOL.....
       HOUTTX = H0OUTT
11110  CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C===>      MAòàHHõâ HOãú [ MAX 16777215 MANTISA ]
          H0000 = H0OUTT * 1.8E+4
          H0001 = H0DATX * 1.8E+4
        WRITE(NWTRE,2345) ZEROCP, H0DATA, H0MIN, METVET, HXMIN, EPSKOM,
     =  IPOL, KODSYS, EPSSYS, MU, KODMU, KODMUM, T0OUT, H0000,  H0001
2345  FORMAT(/' *** "MAòàHHõâ HOãú" ìCTAHOBãEH PABHõM  ', E20.10, ' ***'
     =  /    ' *** HAóAãúHõâ òAÉ àHTEÉPàPOBAHàü H0DATA', E20.10,' ***'/
     =    ' *** "èOÉPEòHOCTú" CETKà BõBOÑA H0MIN...', E20.10,' ***'/
     =    ' *** METOÑ OèPEÑEãEHàü KOMMìTAñàà METVET', I20,' ***'/
     =    ' *** MIN BPEMü ÑOè. KOMMìTAñàà     HXMIN', E20.10,' ***'/
     =    ' *** àHTEPBAã "PABHõE" KOMMìTAñàà EPSKOM', E20.10,' ***'/
     =    ' *** METOÑ àHTEPèOãüñàà HA [TN,TN1] IPOL', I20, ' ***'/
     =    ' *** KOÑ METOÑA PEòEHàü "CãAì"    KODSYS', I20, ' ***'/
     =    ' *** TOóHOCTú PEòEHàü "CãAì"      EPSSYS', E20.10, ' ***'/
     =    ' *** KOHCTAHTA METOÑA àHTEÉPàPOBAHàü MU ', E20.10, ' ***'/
     =    ' *** KOÑ METOÑA "PAáÉOHA"         KODMU ', I20,  ' ***'/
     =    ' *** KOÑ METOÑA "PAáÉOHA" Ñãü ù-M KODMUM', I20,  ' ***'/
     =    ' *** BPEMü HAóAãA BõBOÑA B SPOOL .......', E20.10, ' ***'/
     =    ' *** HOBõâ òAÉ BõBOÑA (ùã.ÉPAÑ) ........', E20.10, ' ***'/
     =    ' *** HOBõâ òAÉ PACóETA (ùã.ÉPAÑ) .......', E20.10, ' ***'//)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C???????????????????????????????????????????????????????????????????????
C      OTKPõTú SPOOLX ....
       KOD = 1
       CALL WRIT00( KOD, HOUTTX, MSWALL, TOKN1, VALLN1 )
       MASPRT(26) = MASPRT(7)
C-->  áAHìãàTú îãAÉà....
       IF ( NSYS .EQ. 1 ) KODSYS=1
       TN = T0
       FEND = 0
       FRUNDG = 0
C-->  ECãà FRUN0 = 1 -> TECT PìHÉE BCEÉÑA HA èEPBOM òAÉE....
       FRUN0 = 0
       H0OLD = 0.0
       NDIVH0 = 0
       FKOM = 0
       TOUT = T0 + HOUTT
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C??????IF( T0OUT .GT. T0 ) TOUT = T0OUT
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
       H0 = H0DATA
       H1 = H0
       H2 = 0.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( KODMU .EQ. 1 ) H1 = ( 1 - MU ) * H0
       IF ( KODMU .EQ. 1 ) H2 =  MU  * H0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  BõBOÑ èEPBOâ TOóKà B SPOOLX
       KOD = 2
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
       IF( T0OUT .LE. T0 ) CALL WRIT00( KOD, TN, MSWALL, TOKN, VALLN )
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--> KOHEñ HAó. ìCT.
C
2      CONTINUE
C----------------------------------------------------------------------
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C--> OTKPõTàE èEóATà à BõBOÑ èEPBOâ TOóKà àá "N", TN -->
       KOD = 1
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN1, VALLN1 )
       KOD = 2
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN,  VALLN  )
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C----------------------------------------------------------------------
3      CONTINUE
C--> HE KOHEñ ãà ùTO,  TOUT OÜàÑAEMOE BPEMü BõBOÑA OóEPEÑHOâ TOóKà
       IF ( TOUT .GT. TMAX + HOUTT ) GO TO 99
C .......... METOÑ..
       H0 = H0DATA
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( KODMU .EQ. 1 ) GO TO 33333
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       H1 = H0
       H2 = 0.0
       IF ( FKOM .EQ. 1 ) GO TO 30
C--> "TPAèEñàà"
33333  H1 = H0 * (1.0 - MU )
       H2 = MU * H0
30     CONTINUE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C-->  "CóET"
       HXG = H0
       TXG = TN + H0
C-->  áAèPOC TECTA PìHÉE èPà KOMMìTAñàà
       FRUN0 = FKOM
C----------------------------------------------------------------------
       CALL GETNEW(MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =      YALL, JALL, LOGYTR, WORKX, IRC, YALLHP, WORKY )
C      CALL SUBERR(3)
C----------------------------------------------------------------------
C  ****  òAÉ HXG C TN BõèOãHEH *******
C   áHAóEHàü VALLN1  à  TOKN1  Ñãü  T = TN + TXG  OèPEÑEãEHõ:
C
C   H E O Å X O Ñ à M O   "ÅÑàTú" ....
C  =====================
C
C  A) èPà IRC = 2 ìMEHúòàTú H0DATA, CÅPOCàTú TOóKì à èOBTOPàTú..
C        (èOKA àÉHOPàPìETCü )
C  B) èPà IRC = 3 -  HE èPOòEã TECT PìHÉE,
C      ( òAÉ ìMEHúòEH, CàTìAñàû èOKA àÉHOPàPìEM )
C
C----------------------------------------------------------------------
C
C  PAáÅEPEMCü B BõBOÑE à èOÑÉOTOBàM HOBìû CTAPTOBìû TOóKì
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C-->  BPEMü PACóàTAHHOâ TOóKà
       TXG = TN + HXG
C-----------------------------------------------------------------------
C èEPEXOÑ ECãà "OTãOÜEHHõâ" BõBOÑ....
        IF ( OUTFLG .GT. 0  .AND. TXG .GE. T0OUT ) GO TO 11111
C???????
C--> "áAáOP" K BPEMEHà BõBOÑA...
       HXOUT = ( TN - TOUT )  +  HXG
C-->  BõBOÑàTú ãà HAM?
       IF ( ABS(HXOUT) .LE. H0MIN  ) GO TO 31
C-->  HET, HìÜHO ãà  àHTEPèOãàPOBATú?
       IF ( TOUT .LT. TXG ) GO TO 32
C-->  TOóKA èPOMEÜìTOóHAü....
       TN = TXG
C-->  èEPEèàCATú àá "N1" B "N"  à èPOÑOãÜàTú
       DO 40 I = 1, NSYS
40     VALLN(I) = VALLN1(I)
C
       DO 41 I = 1, NNTOK
41     TOKN(I) = TOKN1(I)
C
       GO TO 3
C----------------------------------------------------------------------
31     CONTINUE
C-->  Mõ B PAâOHE BõBOÑA, BõBEÑEM...
       KOD = 2
       TN = TXG
       DO 310 I = 1, NSYS
310    VALLN(I) = VALLN1(I)
       DO 311 I = 1, NNTOK
311    TOKN(I) = TOKN1(I)
C-->  èEóATú
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN, VALLN )
C-->  B SPOOLX, HO CMEHàM TOUT..
       TOUT = TOUT + HOUTT
       IF ( OUTFLG .EQ. 0 )
     +       CALL WRIT00( KOD, TN, MSWALL, TOKN, VALLN )
C-->  HA HOBOÉO..
       GO TO 3
C----------------------------------------------------------------------
32     CONTINUE
C-->  TOóKì BõBOÑA èPOCKOóàãà, àHTEPèOãàPìEM..
C-->  KOHTPOãú
       IF ( TN .LT. TOUT ) GO TO 320
C--> CÅOâ
       WRITE(NWTRE, 3210) TN, H0, TOUT, HOUTT, TXG, HXG, HXOUT,OUTFLG
3210   FORMAT(/' *** CÅOâ CãìÜÅõ BPEMEHà B DIGITA.................***'/
     = ' *** TN=', E15.8, ' H0=',E15.8, ' TOUT=',E15.8, ' ***'/
     = ' *** HOUTT=',E15.8, ' TXG=',E15.8, ' HXG=',E15.8, ' ***'/
     = ' *** HXOUT=',E15.8, '--->  BõáõBAâTE ÅOPû CEPÉEü..***'/
     = ' *** OUTFLG=',I14,  '--->  BõáõBAâTE ÅOPû CEPÉEü..***'//)
       WRITE(NWTR, 3210) TN, H0, TOUT, HOUTT, TXG, HXG, HXOUT,OUTFLG
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       STOP 20
C----------------------------------------------------------------------
320    CONTINUE
C-->  àHTEPèOãàPìEM B "N"  K  TOóKE  TOUT   HA àHTEPBAãE HXG....
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( IPOL .EQ. 0 ) GO TO 900
C-->> CèEñ. àHTEPèOã. àá TN <-> TN1 B TOUT ( XPAHàTú B "N" )
       DTX = TOUT - TN
       D1X = (H1 - DTX) / HXG
       D2X = (H2 - DTX) / HXG
       DO 901 I = 1, NSYS
       VN1 = VALLN1(I)
       VN  = VALLN (I)
       VALLN (I) = D1X * VN1 + D2X * VN  - DTX * ( VN1 - VN )
901    CONTINUE
C---
       DO 902 I=1, NNTOK
       TTN1 = TOKN1(I)
       TTN  = TOKN (I)
       TOKN (I) = D1X * TTN1 + D2X * TTN - DTX * ( TTN1 - TTN )
902    CONTINUE
       GO TO 903
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
900    CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C
       DTX = ( TOUT - TN ) / HXG
C
       DO 321 I = 1,NSYS
       VN = VALLN(I)
       VN1 = VALLN1(I)
       DVNN1 = VN1 - VN
       VALLN(I) = VN + DVNN1 * DTX
321    CONTINUE
C
       DO 322 I = 1, NNTOK
       TTN = TOKN(I)
       TTN1 = TOKN1(I)
       DTTNN1 = TTN1 - TTN
       TOKN(I) = TTN + DTTNN1 * DTX
322    CONTINUE
C----------------------------------------------------------------------
903    CONTINUE
C-->  ùTO BCE COOTBETCTBìET BPEMEHà TOUT...
       TN = TOUT
       KOD = 7
       IF ( PON .EQ. 1 )   CALL DRPR01( KOD,  TN, TOKN, VALLN )
C-->  àáMEHàM  TOUT..
       TOUT = TOUT + HOUTT
        KOD = 2
       IF ( OUTFLG .EQ. 0 )
     +     CALL WRIT00(KOD, TN, MSWALL, TOKN, VALLN )
C-->  BOCTAHOBàM CTAPTOBìû TOóKì B "N"..
       TN = TXG
C-->  àá N1  B N......
       DO 323 I = 1,  NSYS
323    VALLN(I) = VALLN1(I)
C
       DO 324 I = 1, NNTOK
324    TOKN(I) = TOKN1(I)
C-->  HA HOBàHúKOÉO...
       GO TO 3
C**********************************************************************
C**********************************************************************
C**********************************************************************
99     CONTINUE
C-->  PACóET OKOHóEH..... B TN à "N" èOCãEÑHüü TOóKA..
       FEND = 1
C-->  ECãà "#ACOXP" HE ÅõãO, TO èPOCTO áAKPOEMCü..
       IF ( FCHEK .EQ. 0 ) GO TO 990
C-->  èEóATAEM O K.T.
       KOD = 6
       IF ( PON .EQ. 1 ) CALL DRPR01(KOD, TN, TOKN, VALLN )
C-->  K.T. COáÑAÑàM..
       CALL SVCHEK( NFACHK, TN, MSWALL, TOKN, VALLN )
990    CONTINUE
C-->  CLOSE>>>
       KOD = 3
       CALL WRIT00(KOD, TN1,  MSWALL, TOKN1, VALLN1 )
       IF ( PON .EQ. 1 ) CALL  DRPR01(KOD, TN1, TOKN1, VALLN1 )
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
11111    CONTINUE
C--->  CMEHA BõB. XAPAKTEPàCTàK....
         TOUT = TXG
         WRITE(NWTRE,11114) TOUT
         WRITE(NWTR ,11114) TOUT
11114    FORMAT(/' *** B MOMEHT BPEMEHà "MOÑEãà" ', E12.5, ' (CEK.)'/
     +           ' *** CTAPTOBAã èPOñECC HAKOèãEHàü áHAóEHàâ PEòEHàü'/)
         IF ( OUTFLG .GE. 2 ) HOUTT  = H0OUTT
         IF ( OUTFLG .GE. 2 ) H0002 = H0OUTT * 1.8E+4
         IF ( OUTFLG .GE. 2 ) WRITE(NWTRE,11113) HOUTT,  H0002,  TXG
         IF ( OUTFLG .GE. 2 ) WRITE(NWTR ,11113) HOUTT,  H0002,  TXG
11113    FORMAT(/' *** BHàMAHàE: òAÉ BõBOÑA áHAóEHàâ èEPEOèPEÑEãEH:'/
     *        ' *** HOUTT=',E15.5,' (CEK.), ',E15.5,' (ùã. ÉPAÑ.)'/
     *        ' *** BPEMü "MOÑEãà" TN = ',E15.5,' (CEK.)'/)
         IF ( OUTFLG .EQ. 3 ) H0DATA = H0DATX
         IF ( OUTFLG .EQ. 3 ) H0DATQ=H0DATX*1.8E+4
         IF ( OUTFLG .EQ. 3 ) WRITE(NWTRE,11112) H0DATX, H0DATQ, TXG
         IF ( OUTFLG .EQ. 3 ) WRITE(NWTR ,11112) H0DATX, H0DATQ, TXG
11112    FORMAT(/' *** BHàMAHàE: òAÉ àHTEÉPàPOBAHàü èEPEOèPEÑEãEH:'/
     *        ' *** H0DATA=',E15.5,' (CEK.), ',E15.5,' (ùã. ÉPAÑ.)'/
     *        ' *** BPEMü "MOÑEãà" TN = ',E15.5,' (CEK.)'/)
C@@@@@ KOHTPOãú @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( HXMIN .GT. H0DATA ) WRITE(NWTRE,3456) HXMIN
       IF ( HXMIN .GT. H0DATA ) HXMIN = H0DATA / 10.0
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) WRITE(NWTRE,3457)
       IF ( HXMIN .EQ. 0.0 .AND. METVET .EQ. 1 ) HXMIN = H0DATA / 100.0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         OUTFLG = 0
         GO TO 31
C***** DEBUG INIT(TN,H0,TOUT, HOUTT,TXG,HXG, HXOUT)
       END
