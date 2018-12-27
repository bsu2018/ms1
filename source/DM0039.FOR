         SUBROUTINE DM0039 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (AT OCTOBER 1989 É.)------------
C   èOÑèPOÉPAMMA:  DM0039 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HAáHAóEHàE:    MOHàTOP MAKPOMOÑEãà èúEáOKOMèEHCATOPA    (Tàè 800)
C   -----------     ( FM0035, LM0014 )
C
C                  ñEèú C PACèPEÑEãEHHõMà èAPAMETPAMà
C                  èPECTABãüETCü B CàTETàóECKOâ CXEME
C
C@
C
C                  B áABàCàMOCTà OT áHAóEHàü îãAÉA ZAPROS [MASPRT(28)]:
C                  BõèOãHüET CãEÑìûôàE îìHKñàà:
C
C                  A) ZAPROS = 1
C                     ----------
C
C               *** BOáMOÜHõ àTEPAñàOHHõE ìTOóHEHàü ***:
C                   èPà BOáÅìÜÑEHàà FITER [ MASPRT(65) ] --->
C                   áHAóEHàü "TOK"_ü - èPOÉHOá üBHõM METOÑOM -
C                   HE BõóàCãüûTCü, A BõÅàPAûTCü àá MACCàBA TOKN1;
C               ***
C                  B) ZAPROS = 2
C                     ----------
C                  BõóàCãEHàE áHAóEHàâ TOKOB B BETBüX B T(N+1) MOMEHT
C                  BPEMEHà   àCXOÑü àá G à J èAPAMETPOB à áHAóEHàü
C                  ìáãOBõX HAèPüÜEHàâ HA ùãEMEHTE PACóàTAHHõX Ñãü T(N+1)
C                    à áAèàCú (îàKCAñàü) ùTàX áHAóEHàâ B "TOK T(N+1)";
C                 - èPà ùTOM KOHTPOãàPìûETCü "PACXOÜÑEHàE" MEÜÑì
C                   áHAóEHàüMà, èOãìóEHHõMà èO üBHOâ CXEME (èPOÉHOá) à
C                   OèPEÑEãEHHõMà àCXOÑü àá BõóàCãEHHõX áHAóEHàâ VALLN1;
C                   KOHTPOãú BEÑETCü èO HE èPEBõòEHàû EITER[MASPRT(67)]
C                   OTHOCàTEãúHOâ èOÉPEòHOCTà. B HE ÅãAÉOèPàüTHOM CãìóAE
C                   B "MAXITR" áAèàCõBAETCü MAX_èOÉPEòHOCTú à
C                   B "NLITER" îàKCàPìETCü HOMEP ùãEMEHTA.....
C                   èAPAMETP "KOD" = 1;
C
C
C                  C) ZAPROS = 3
C                     **********
C                     HE èOÑÑEPÜàBAETCü ( PEáEPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE èOÑÑEPÜàBAETCü;
C
C   METOÑ:
C   -----
C
C
C   èAPAMETPõ:     MSW, RMSW - ìèPABãüûôEE CãOBO COCTOüHàü MAKPOMOÑEãà
C   ---------      ( "OTPEáOK" MACCàBõ MSWALL -> C.M. è/è GETMSW );
C                  YALL - MATPàñA èP0BOÑàMOCTà AHAãàáàPìEMOâ ñEèà;
C                  JALL - BEKTOP àCTOóHàKOB TOKA AHAãàáàPìEMOâ ñEèà;
C                  VALLN - BEKTOP ìáãOBõX HAèPüÜEHàâ AHAãàáàPìEMOâ ñEèà:
C                          B T = TN;
C                  VALLN1- BEKTOP ìáãOBõX HAèPüÜEHàâ AHAãàáàPìEMOâ ñEèà:
C                          B T = TN + H0;
C                  TOKN  à  TOKN1  -  áHAóEHàü "TOKOB" èPà T :=
C                                     TN  à  TN + H0;
C                                   PAáMEPHOCTà CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP èPàáHAKOB K.á. ìáãOB:
C                  àCèOãúáìETCü B MAKPOMOÑEãüX C èPàáHAKOM TàèA = 5;
C                  PAáMEPHOCà MACCàBOB ÉPìèèõ "ALL" OèPEÑEãüûTCü:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEKìôAü_PAáMEPHOCTú ---> NSYS = MASPRT(26)
C                  KOD - èAPAMETP OÅPATHOâ CBüáà C MOHàTOPOM DM....
C                   ( =1 èPà ÅOãúòOâ ãOK. èOÉPEòHOCTà );
C
C
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, PROF2, PROF3;SUPPL1, SUPPL2
C   ------------
C
C   èPàMEóAHàE:  1.CTPìKTìPì MSW CãOBA MAKPOMOÑEãà ÑAHHOÉO TàèA CM. HàÜE
C   ----------   2. èPà ZAPROS ^=  1, 2, 3, 4 - BõÑAóA COOÅôEHàü à ìXOÑ
C                3. èPà FTEST >=1 -> èEóATú HA WTRE AKTA CMEHõ COCTOüHàü
C
C
C                    "èOCTOüHHAü" óACTú MSW
C                     ----------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I èPàáHAKI KOãà I KOã. IAÑP.   IPE I
C  I ùãEME I TàèA   I è/è    I TàèA   I óECT I BõB. IáHAó.  IáE I
C  I HTA   I MOÑEãà I PACóETAI MOÑEãà I BO   I áHAó I"TOKN" IPB I
C  I       I        I DM.....I        I ìáãOBI EHàâ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW TàèA  # 800
C                  --------------------
C
C
C  èOCT.  I èEPEMEHHAü RMSW-->
C +----------+----+---+----+---+----+---+------+------+---+---+---+...>
C I ìáãõI    I    I   I RI I   I    I   I      I      I   I   I   I...>
C I     I G1 I #ùãI#CMI    I GLI GC I KOI  KJP I G(20)IG11IG12IG22I...>
C I     I    I    I   I    I   I    I   I      I      I   I   I   I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I   3 I 1  I 1  I 1 I  1 I 1 I  1 I 1 I  1   I  20  I 1 I 1 I 1 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I 9-11I 12 I 13 I 14I 15 I16 I 17 I18 I 19   I 20-39I 40I41 I42 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I     I 1  I 2  I 3 I 4  I 5 I 6  I 7 I 8    I 9-28 I29 I30 I 31I
C
C
C
C ...>+---------+--------+------+
C ...>I G MAT-  I PEáEPB I      I
C ...>I   PàñA  I        I ATOKMI
C ...>I BCèOMOÉ.I        I      I
C ...>+---------+--------+------+
C ...>I   16    I   1    I   1  I
C ...>+---------+--------+------+
C ...>I 43 - 58 I  59    I  60  I
C ...>+---------+--------+------+
C     I 32 - 47 I  48    I  49  I
C----------------------------------------------------------------------
C  OÅôAü ÑãàHA MSW --> LM<BCü> = 60
C
C  ÑãàHA èEPEMEH. óACTà --> LM<RMSW> = 49
C
C   BHàMAHàE           óàCãO èúEáO-òAâÅ CTPOÉO PABHO  198
C   ----------
C               Ñãü KAÜÑOâ èúEáO-òAâÅõ  óàCãO áBEHúEB PABHO 1 (M)
C
C   ÅãOK RIEZO : TNL - TOKà B àHÑìKTàBHOCTüX   ( KH )
C                TNC - TOKà B EMKOCTüX          (KH )
C                UN0 - CTAPOE HAèPüÜEHàE       ( M/C)
C                UN1 - HOBOE  HAèPüÜEHàE       ( M/C)
C                TJNL - àCTOóHàKà TOKA B àHÑìKTàBHOCTà  ( KH)
C                TJNC - àCTOóHàKà TOKA B EMKOCTà        ( KH)
C                D11  - ÑàAÉOHAãúHõE ùãEMEHTõ MATPàñ.PAáãOÜEHàü
C                            XOãEñKOÉO
C                D21  - BHE-ÑàAÉOHAãúHõE ùãEMEHTõ PAá. XOãEñKOÉO
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
C----------------------------------------------------------------------
       COMMON /PIEZO/ TNL(200),TNC(200),UN0(200),UN1(200),
     * TJNL(200),TJNC(200),D11(200),D21(200)
C----------------------------------------------------------------------
C!!!!!!!!!!!!!!!!!!#########!!!!!!!!!!!!!!!!!--------------------------
         DIMENSION  G(20), TJN1(200), TJN2(200)
C----------------------------------------------------------------------
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER,  ZAPROS, TIP
         INTEGER ADRMSW
         REAL   KJP ,KO ,LI
C####################################################################
C      H0 = 0.13889E-5
C####################################################################
       TIP = MSW(2)
       IF ( TIP .NE. 800 ) GO TO 9999
C--------------------------------------------------------------------
       G1 = RMSW(12)
       GL = RMSW(16)
       GC = RMSW(17)
C      WRITE (6,*) G1,GL,GC
C--------------------------------------------------------------------
C--->  BõÅOP PEÜàMA áAèPOCA..
       GO TO( 100, 200), ZAPROS
C===> àHAóE OòàÅKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHàTOP #', I4, ' áAèPOC OÅCãìÜàBAHàü OòàÅOóEH
     = ***'/ ' *** ZAPROS = ', I5, ' - àÉHOPàPìETCü ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  èAPAMETPõ CC
      RI = RMSW(15)
      KO = RMSW(18)
      KJP = RMSW(19)
      G11 = RMSW(40)
      G12 = RMSW(41)
      G22 = RMSW(42)
C--> KAKOE BXOÑHOE HAèPüÜEHàE ?
      UBX = TOKN1(MSW(60))
CCC   WRITE (6,*) G11,G12,G22
      DO 10 I = 1,20
      G(I) = RMSW(19 + I)
10    CONTINUE
C--------------------------------------------------------------------
C--> àCTOóHàKà TOKA B àHÑìKTàBHOCTà à EMKOCTà
      DO 20 I =1,198
      X1 =  G1*(UN1(I) - UN1(I+1))
      X2 =  UN0(I) + UN1(I) - UN0(I+1) - UN1(I+1)
      TJNL(I) = TNL(I) + X1 + KO*UBX + X2*KJP*H0/2.
      TJNC(I) = TNC(I) + GC*UN1(I+1)
20    CONTINUE
CCC   WRITE(6,*) TJNL(198),TJNC(198)
C--> àCTOóHàKà TOKA B è-OÅPAáHOM áBEHE
      TJN1(1) = - TJNL(1)
      TJN2(1) = TJNC(1)/2. + TJNL(1)
      DO 30 I =2,198
      TJN1(I) = TJNC(I-1)/2. - TJNL(I)
      TJN2(I) = TJNC(I)/2. + TJNL(I)
30    CONTINUE
      TJN2(198) = TJNC(198)/2. + TJN2(198)
C--------------------------------------------------------------------
C--> ÉPìèàPìEM J èO CTEèEHüM ÑBOâKà
      CALL SUPPL1(7 ,TJN1(1),TJN2(1),TJ1,TJ2,G )
      CALL SUPPL1(6 ,TJN1(129),TJN2(129),TJ3,TJ4,G )
      CALL SUPPL1(2 ,TJN1(193),TJN2(193),TJ5,TJ6,G )
      CALL SUPPL1(1 ,TJN1(197),TJN2(197),TJ7,TJ8,G )
C--> CKãAÑõBAEM ...
      CALL SUPPL2(TJ1,TJ2,TJ3,TJ4,RMSW(43),RMSW(44) )
      CALL SUPPL2(TJ1,TJ2,TJ5,TJ6,RMSW(45),RMSW(46) )
      CALL SUPPL2(TJ1,TJ2,TJ7,TJ8,RMSW(47),RMSW(48) )
CCC   WRITE (6,*) TJ1,TJ2
C----------------------------------------------------------------------
C  BKãAÑõBAEM B OÅôìû MATPàñì G-J
C----------------------------------------------------------------------
      KOD = 0
      G12 = - G12
      G11 = G11 + G12
      G22 = G22 + G12
      CALL DEPOST (MSW(9),MSW(10), G12,0.,YALL,JALL)
      CALL DEPOST (0 ,MSW(10), G22,TJ2,YALL,JALL)
      CALL DEPOST (0 ,MSW(9), G11,TJ1,YALL,JALL)
        RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C---- ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C--> BXOÑHOE à BõXOÑHOE HAèPüÜEHàü CàHTETàóECKOâ CXEMõ
       KN = MSW(9)
       KK = MSW(10)
       VN = VALLN1(KN)
       VK = VALLN1(KK)
C--> OÅHOBãüEM BHìTPEHàE HAèPüÜEHàü
       DO 210 I = 1,199
       UN0(I) = UN1(I)
210     CONTINUE
C----------------------------------------------------------------------
C  àôEM HOBõE BHìTPEHàE HAèPüÜEHàü CC
C----------------------------------------------------------------------
       UN1(1) = VN
       UN1(199) = VK
C--> HAâÑEM J
       DO 220 I =2,198
       TJN1(I) = TJNL(I-1) + TJNC(I-1) - TJNL(I)
220     CONTINUE
       TJN1(2) = TJN1(2) + GL*VN
       TJN1(198) = TJN1(198) + GL*VK
C--> àCèOãúáìETCü PAáãOÜEHàE XOãEñKOÉO
C--> D*TJN2 = J
       TJN2(2) = TJN1(2)/D11(2)
       DO 230 I =3,198
       TJN2(I) = (TJN1(I) - D21(I-1)*TJN2(I-1))/D11(I)
230     CONTINUE
C--> DT*UN1 = TJN2
       UN1(198) = TJN2(198)/ D11(198)
       DO 240 I =1,196
       UN1(198-I) = (TJN2(198-I)-D21(198-I)*UN1(199-I))/D11(198-I)
240     CONTINUE
C--> HOBõE BHìTPEHàE TOKà
       DO 250 I =1,198
       TNL(I) = G1*(UN1(I)-UN1(I+1)+UN0(I)-UN0(I+1))+ TNL(I)
       TNC(I) = GC*(UN1(I+1) - UN0(I+1))- TNC(I)
250     CONTINUE
       TOKN1(MSW(7)) = VK
       KOD = 0
       RETURN
C######################################################################
C######################################################################
9999     CONTINUE
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ãOÉ. CÅOâ B DM0039 - áAèPOC HA OÅCãìÜàBAHàE óìÜOÉ
     =O TàèA ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
C######################################################################
C        DEBUG INIT
         END
