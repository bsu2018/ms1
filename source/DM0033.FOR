         SUBROUTINE DM0033 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT   MAPTA     1988 É.)------
C   èOÑèPOÉPAMMA:  DM0033 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HAáHAóEHàE:    B áABàCàMOCTà OT áHAóEHàü îãAÉA ZAPROS [MASPRT(28)]:
C   -----------    BõèOãHüET CãEÑìûôàE îìHKñàà:
C
C                  MOÑEãú # 18--> TPEXîAáHõâ àCTOóHàK TOKA,
C                                 ìèPABãüEMõâ è/è èOãúáOBATEãü.
C
C
C              ùãEMEHT OèàCõBAETCü:
C
C      ù # T 18 ì KA KB KC K0 è
C              MAS < NTOKOB  #ùã1 #CM1  #ùã2 #CM2 ... >
C              MAS < NHAèP   ìáEãN1 ìáEãK1  ìáEãN2 ìáEãK2 ... >
C              PSUB #P < .........>;
C
C              óEPEá AèèAPAT PSUB áAÑAûTCü áAKOHõ ìèPABãEHàü ;
C
C      ÉÑE
C          NTOKOB -   KOãàóECTBO TOKOB ùãEMEHTOB CXEMõ , KOTOPõE
C                     àCèOãúáìûTCü Ñãü ìèPABãEHàü àCTOóHàKOM TOKA;
C          #ùãI , #CMI     - HOMEP ùãEMEHTA à HOMEP TOKA ùãEMEHTA ,
C                            àCèOãúáìEMOÉO Ñãü ìèPABãEHàü àCTOóHàKOM;
C          NHAèP           - KOãàóECTBO HAèPüÜEHàâ MEÜÑì ìáãAMà CXEMõ ,
C                            àCèOãúáìEMõX Ñãü ìèPABãEHàü àCTOóHàKOM;
C          ìáEãNI , ìáEãKI - HOMEPA ìáãOB, HAèPüÜEHàü MEÜÑì KOTOPõMà
C                            àCèOãúáìûTCü Ñãü ìèPABãEHàü àCTOóHàKOM;
C          #P              - HOMEP è/è èOãúáOBATEãü ( PSUB#P ), B
C                            KOTOPOâ èPOàáBOÑàTCü PACóET TOKA àCTOóHàKA;
C
C
C                  A) ZAPROS = 1
C                     ----------
C                  îOPMàPOBAHàE BKãAÑOB B YALL MATPàñì èPOBOÑàMOCTEâ à
C                  JALL - BEKTOP àCTOóHàKOB TOKA áHAóEHàâ èAPAMETPOB
C                  ùKBàBAãEHTHõX  G - J BETBEâ  MOÑEãà ùãEMEHTA ñEèà
C
C
C                  B) ZAPROS = 2
C                     ----------
C                  BõóàCãEHàE áHAóEHàâ TOKOB B BETBüX B T(N+1) MOMEHT
C                  BPEMEHà   àCXOÑü àá G à J èAPAMETPOB à áHAóEHàü
C                  ìáãOBõX HAèPüÜEHàâ HA ùãEMEHTE PACóàTAHHõX Ñãü T(N+1)
C                    à áAèàCú (îàKCAñàü) ùTàX áHAóEHàâ B "TOK T(N+1)";
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
C
C   METOÑ:         ÑàCKPETàáAñàü ìPABHEHàâ COCTOüHàü MOÑEãà ÑAHHOÉO
C   -----          TàèA HA OCHOBE METOÑA ãàHàÑÜEPA BàOãìÉÅà:
C                  Y(N+1) = Y(N) + H1*F(N+1) + H2*F(N).
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
C                 MSWALL( (RMSWAL ) - BECú CèàCOK MSW_ALL,
C                 ADRMSW - AÑPEC TEK. CãOAB MSW ( RMSW );
C
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST, CALL_PS --> Bõá.  PSUB è/è;
C
C   èPàMEóAHàE:  1.CTPìKTìPì MSW CãOBA MAKPOMOÑEãà ÑAHHOÉO TàèA CM. HàÜE
C   ----------   2. èPà ZAPROS ^=   1 àãà 2 - COOÅôEHàE à ìXOÑ;
C
C
C               "èOCTOüHHAü" óACTú MSW TàèA # 18:
C                ---------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I èPàáHAKI KOãà I KOã. IAÑP.   IPE I
C  I ùãEME I TàèA   I è/è    I TàèA   I óECT I BõB. IHAó.   IáE I
C  I HTA   I MOÑEãà I PACóETAI MOÑEãà I BO   I áHAó I"TOKN" IPB I
C  I       I        I DM.....I       .I ìáãOBI EHàâ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C              îOPMAT RMSW Tàè # 18:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # CãOBA   I    # CãOBA B I KTO  I
C OT HAóAãA  I    OTPEáKE   IOèPEÑ.I
C BCEÉO MSW  I    RMSW B    I      I       OÅOáHAóEHàE - áHAóEHàE
C OèàCAHàü   I    FM0029    I      I
C------------I--------------I------I-----------------------------------
C     9      I     A        I      I HOMEP ìáãA A
C    10      I     B        I      I HOMEP ìáãA B
C    11      I     C        I      I HOMEP ìáãA C
C    12      I     0        I      I HOMEP ìáãA 0
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  NTOKOB -KOãàóECTBO TOKOB, àCèOãú-
C            I              I      I          áìEMõX èPà PACóETE TOKA
C            I              I      I          àCTOóHàKA;
C    14      I     2        I  FM  I  #ùã1 - HOMEP ùãEMEHTA;
C    15      I     3        I  FM  I  #CM1 - HOMEP TOKA ùãEMEHTA #ùã1;
C    16      I     4        I  LM  I  ATOKM1 - AÑP B TOKN/N1 áHAóEHàü
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ùãEMEHTA #ùã1;
C 11+I*3     I  -1+I*3      I  FM  I  #ùãI -      -"-
C 12+I*3     I     I*3      I  FM  I  #CMI -      -"-
C 13+I*3     I   1+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C14+NTOKOB*3 I  2+NTOKOB*3  I  FM  I  NHAèP -KOãàóECTBO HAèPüÜEHàâ,
C            I              I      I         àCè. Ñãü PACóETA TOKA
C            I              I      I         àCTOóHààKA;
C15+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  ìáEãN1 -ìáEã HAóAãA HAèPüÜEHàü;
C16+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  ìáEãK1 -ìáEã KOHñA  HAèPüÜEHàü;
C .........  I ............ I .... I  ...........................
C            I              I      I
C15+NTOKOB*3+I 2+NTOKOB*3+  I      I  NPSUB - HOMEP è/è èOãúáOBATEãü
C NHAèP*2    I  NHAèP*2     I  FM  I         ( PSUB) B KOTOPOâ èPOàáBO-
C            I              I      I         ÑàTCü PACóET TOKA àCTOóHàKA
C (...)+1    I  (...)+1     I  FM  I  NPAR - KOãàóECTBO èAPAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  èAPAMETPõ PSUB , áAÑAHHõE
CÑO(...)+NPARI ÑO(...)+NPAR I      I  èOãúáOBATEãEM.
C            I              I      I
C 17+3*NTOKOBI 5 + 3*NTOKOB I  DM  I  TOKTA - áHAóEHàE TOKA àCTOóHàKA
C +2*NNAPR + I + 2*NNAPR +  I      I          îAáõ   A    èPà  T=TN+1
C NPAR       I NPAR         I      I
C
C 18+3*NTOKOBI 6 + 3*NTOKOB I  DM  I  TOKTB - áHAóEHàE TOKA àCTOóHàKA
C +2*NNAPR + I + 2*NNAPR +  I      I          îAáõ   B    èPà  T=TN+1
C NPAR       I NPAR         I      I
C
C 19+3*NTOKOBI 7 + 3*NTOKOB I  DM  I  TOKTC - áHAóEHàE TOKA àCTOóHàKA
C +2*NNAPR + I + 2*NNAPR +  I      I          îAáõ   C    èPà  T=TN+1
C NPAR       I NPAR         I      I
C
C 20+3*NTOKOBI 8 + 3*NTOKOB I  DM  I  TOKT0 - áHAóEHàE TOKA àCTOóHàKA
C +2*NNAPR + I + 2*NNAPR +  I      I          îAáõ   0    èPà  T=TN+1
C NPAR       I NPAR         I      I
C
C 21+3*NTOKOBI 9 + 3*NTOKOB I  DM  I  G12 - áHAóEHàE èPOBOÑàMOCTà
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 22+3*NTOKOBI10 + 3*NTOKOB I  DM  I  G13 - áHAóEHàE èPOBOÑàMOCTà
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 23+3*NTOKOBI11 + 3*NTOKOB I  DM  I  G23 - áHAóEHàE èPOBOÑàMOCTà
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 24+3*NTOKOBI12 + 3*NTOKOB I  DM  I  G14 - áHAóEHàE èPOBOÑàMOCTà
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 25+3*NTOKOBI13 + 3*NTOKOB I  DM  I  G24 - áHAóEHàE èPOBOÑàMOCTà
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 26+3*NTOKOBI14 + 3*NTOKOB I  DM  I  G34 - áHAóEHàE èPOBOÑàMOCTà
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C
C 27+3*NTOKOBI15 + 3*NTOKOB I  DM  I  J12 - áHAóEHàE àCTOóHàKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 28+3*NTOKOBI16 + 3*NTOKOB I  DM  I  J13 - áHAóEHàE àCTOóHàKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 29+3*NTOKOBI17 + 3*NTOKOB I  DM  I  J23 - áHAóEHàE àCTOóHàKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 30+3*NTOKOBI18 + 3*NTOKOB I  DM  I  J14 - áHAóEHàE àCTOóHàKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 31+3*NTOKOBI19 + 3*NTOKOB I  DM  I  J24 - áHAóEHàE àCTOóHàKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C 32+3*NTOKOBI20 + 3*NTOKOB I  DM  I  J34 - áHAóEHàE àCTOóHàKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà T=TN+1
C NPAR       I NPAR         I      I
C
C 33+3*NTOKOBI21 + 3*NTOKOB I  DM  I  NMOD - èPàáHAK MOÑEãà
C +2*NNAPR + I + 2*NNAPR +  I      I     0 - ãàHEâHAü
C NPAR       I NPAR         I      I   > 0 - HEãàHEâHAü
C            I              I      I
C----------------------------------------------------------------------
C
C  OÅôAü ÑãàHA MSW --> LM<BCü> = 33 + 3*NTOKOB + 2*NNAPR + NPAR
C
C  ÑãàHA èEPEMEH. óACTà --> LM<RMSW> = 21 + 3*NTOKOB + 2*NNAPR + NPAR
C
C     B MACCàBE TOKN à TOKN1 COÑEPÜàTCü:
C     **********************************
C     1    -     áHAóEHàE TOKA àCTOóHàKA, BXOÑüôEÉO B ìáEã  KA
C                              èPà T=TN/TN1
C     2    -     áHAóEHàE TOKA àCTOóHàKA, BXOÑüôEÉO B ìáEã  KB
C                              èPà T=TN/TN1
C     3    -     áHAóEHàE TOKA àCTOóHàKA, BXOÑüôEÉO B ìáEã  KC
C                              èPà T=TN/TN1
C     4    -     áHAóEHàE TOKA àCTOóHàKA, BõXOÑüôEÉO àá ìáãA  K0
C                              èPà T=TN/TN1----------------------
C     5    -
C     6    -
C     6    -                   P E á E P B
C     8    -
C     9    -
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION INTMAS(200), STMAS(50), DGMAS(22)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR, INTMAS, J
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER, TIP, ZAPROS
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       IF ( TIP .EQ. 18 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ãOÉ. CÅOâ B DM0033 - áAèPOC HA OÅCãìÜàBAHàE óìÜOÉ
     =O TàèA ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C----------------------------------------------------------------------
C-->  OèPEÑEãàM KOHCTAHTõ.
       NTOKOB = MSW(13)
       NNAPR = MSW (14 + 3*NTOKOB )
       INPAR = 16 + 3*NTOKOB + 2*NNAPR
       NPAR  = MSW (INPAR)
       ITOKTA = INPAR + 1 + NPAR
       ITOKTB = INPAR + 2 + NPAR
       ITOKTC = INPAR + 3 + NPAR
       ITOKT0 = INPAR + 4 + NPAR
C--->  BõÅOP PEÜàMA áAèPOCA..
6666   GO TO( 100, 200), ZAPROS
C----------------------------------------------------------------------
C===> àHAóE OòàÅKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHàTOP #', I4, ' áAèPOC OÅCãìÜàBAHàü OòàÅOóEH
     = ***'/ ' *** ZAPROS = ', I5, ' - àÉHOPàPìETCü ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
C-->  ZAPROS = 1 --> PACóàTATú áHAóEHàü àCTOóHàKA TOKA à èPOBOÑàMOCTà
C     èPà T=TN+1 à BãOÜàTú.
100    CONTINUE
       IF( FITER .EQ. 1 ) GO TO 150
       STMAS( 1) = TN
       STMAS( 2) = H0
       ISTMAS = 2
C----------------------------------------------------------------------
C--> èEPEÑAEM TOKà
       IF( NTOKOB .EQ. 0 ) GO TO 16
          DO 15 I= 1,NTOKOB
          ISTMAS = ISTMAS + 1
          ITOKN = MSW( 3*I + 13 )
          STMAS(ISTMAS) = TOKN(ITOKN)
15     CONTINUE
16     CONTINUE
C----------------------------------------------------------------------
C--> èEPEÑAEM HAèPüÜEHàü
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 18
       IMSWV = 13 + 3*NTOKOB
          DO 17 I= 1,NNAPR
          ISTMAS = ISTMAS + 1
          IMSWV = IMSWV + 2
          NA = MSW( IMSWV )
          KO = MSW( IMSWV + 1 )
          VNA = 0.0
          IF( NA .GT. 0 ) VNA = VALLN(NA)
          VKO = 0.0
          IF( KO .GT. 0 ) VKO = VALLN(KO)
          STMAS(ISTMAS) = VNA - VKO
17     CONTINUE
18     CONTINUE
C----------------------------------------------------------------------
C-->  URMAS <--> MSW(14 + 3*NTOKOB + 2*NNAPR)
C----------------------------------------------------------------------
       INPSUB = 15 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PACóàTõBAEM áHAóEHàE TOKOB èPà T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 1.
       ITOK = MSW(7)
       DGMAS(2) = TOKN( ITOK )
       DGMAS(3) = TOKN( ITOK+1)
       DGMAS(4) = TOKN( ITOK+2)
       DGMAS(5) = TOKN( ITOK+3)
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT12 = DGMAS(6)
       TOKT13 = DGMAS(7)
       TOKT23 = DGMAS(8)
       TOKT14 = DGMAS(9)
       TOKT24 = DGMAS(10)
       TOKT34 = DGMAS(11)
       G12 = DGMAS(12)
       G13 = DGMAS(13)
       G23 = DGMAS(14)
       G14 = DGMAS(15)
       G24 = DGMAS(16)
       G34 = DGMAS(17)
       RMSW(ITOKT0+1) = G12
       RMSW(ITOKT0+2) = G13
       RMSW(ITOKT0+3) = G23
       RMSW(ITOKT0+4) = G14
       RMSW(ITOKT0+5) = G24
       RMSW(ITOKT0+6) = G34
       RMSW(ITOKT0+7) = TOKT12
       RMSW(ITOKT0+8) = TOKT13
       RMSW(ITOKT0+9) = TOKT23
       RMSW(ITOKT0+10)= TOKT14
       RMSW(ITOKT0+11)= TOKT24
       RMSW(ITOKT0+12)= TOKT34
       MSW( ITOKT0+13 ) = IFIX(DGMAS(1))
       DO 148 I=1,5
       TOKN1( ITOK + 3 +I) = DGMAS(17+I)
148    CONTINUE
       GO TO 170
150    CONTINUE
C----------------------------------------------------------------------
C -->  àTEPAñàü HE èEPBAü...
       IF(MSW(ITOKT0+13).EQ.0) GO TO 160
       STMAS( 1) = TN
       STMAS( 2) = H0
       ISTMAS = 2
C----------------------------------------------------------------------
C--> èEPEÑAEM TOKà
       IF( NTOKOB .EQ. 0 ) GO TO 26
          DO 25 I= 1,NTOKOB
          ISTMAS = ISTMAS + 1
          ITOKN = MSW( 3*I + 13 )
          STMAS(ISTMAS) = TOKN1(ITOKN)
25     CONTINUE
26     CONTINUE
C----------------------------------------------------------------------
C--> èEPEÑAEM HAèPüÜEHàü
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 28
       IMSWV = 13 + 3*NTOKOB
          DO 27 I= 1,NNAPR
          ISTMAS = ISTMAS + 1
          IMSWV = IMSWV + 2
          NA = MSW( IMSWV )
          KO = MSW( IMSWV + 1 )
          VNA = 0.0
          IF( NA .GT. 0 ) VNA = VALLN1(NA)
          VKO = 0.0
          IF( KO .GT. 0 ) VKO = VALLN1(KO)
          STMAS(ISTMAS) = VNA - VKO
27     CONTINUE
28     CONTINUE
C----------------------------------------------------------------------
C-->  URMAS <--> MSW(14 + 3*NTOKOB + 2*NNAPR)
C----------------------------------------------------------------------
       INPSUB = 15 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PACóàTõBAEM áHAóEHàü TOKOB èPà T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 2.
       DGMAS(2) = RMSW( ITOKTA )
       DGMAS(3) = RMSW( ITOKTB )
       DGMAS(4) = RMSW( ITOKTC )
       DGMAS(5) = RMSW( ITOKT0 )
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT12 = DGMAS(6)
       TOKT13 = DGMAS(7)
       TOKT23 = DGMAS(8)
       TOKT14 = DGMAS(9)
       TOKT24 = DGMAS(10)
       TOKT34 = DGMAS(11)
       G12 = DGMAS(12)
       G13 = DGMAS(13)
       G23 = DGMAS(14)
       G14 = DGMAS(15)
       G24 = DGMAS(16)
       G34 = DGMAS(17)
       RMSW(ITOKT0+1) = G12
       RMSW(ITOKT0+2) = G13
       RMSW(ITOKT0+3) = G23
       RMSW(ITOKT0+4) = G14
       RMSW(ITOKT0+5) = G24
       RMSW(ITOKT0+6) = G34
       RMSW(ITOKT0+7) = TOKT12
       RMSW(ITOKT0+8) = TOKT13
       RMSW(ITOKT0+9) = TOKT23
       RMSW(ITOKT0+10)= TOKT14
       RMSW(ITOKT0+11)= TOKT24
       RMSW(ITOKT0+12)= TOKT34
       ITOK = MSW(7)
       DO 158 I=1,5
       TOKN1( ITOK + 3 + I) = DGMAS(17 + I)
 158   CONTINUE
       GO TO 170
 160   CONTINUE
       G12 = RMSW( ITOKT0+1)
       G13 = RMSW( ITOKT0+2)
       G23 = RMSW( ITOKT0+3)
       G14 = RMSW( ITOKT0+4)
       G24 = RMSW( ITOKT0+5)
       G34 = RMSW( ITOKT0+6)
       TOKT12 = RMSW( ITOKT0+7)
       TOKT13 = RMSW( ITOKT0+8)
       TOKT23 = RMSW( ITOKT0+9)
       TOKT14 = RMSW( ITOKT0+10)
       TOKT24 = RMSW( ITOKT0+11)
       TOKT34 = RMSW( ITOKT0+12)
170    CONTINUE
C----------------------------------------------------------------------
C -->  èOùãEMEHTHõâ BKãAÑ......
C----------------------------------------------------------------------
       KA = MSW(9)
       KB = MSW(10)
       KC = MSW(11)
       K0 = MSW(12)
       KOD = 0
C--->
       CALL DEPOST( KA , KB , G12 , TOKT12 , YALL , JALL )
       CALL DEPOST( KA , KC , G13 , TOKT13 , YALL , JALL )
       CALL DEPOST( KB , KC , G23 , TOKT23 , YALL , JALL )
       CALL DEPOST( KA , K0 , G14 , TOKT14 , YALL , JALL )
       CALL DEPOST( KB , K0 , G24 , TOKT24 , YALL , JALL )
       CALL DEPOST( KC , K0 , G34 , TOKT34 , YALL , JALL )
C--->
       RETURN
C######################################################################
200    CONTINUE
C----------------------------------------------------------------------
C-->  ZAPROS = 2  --> PACóàTATú TOKà Ñãü T=TN+1 à áAèàCATú B TOKN1
C----------------------------------------------------------------------
C  èPEÑCKAáAHHõE TOKà.
       XA = RMSW( ITOKTA )
       XB = RMSW( ITOKTB )
       XC = RMSW( ITOKTC )
       X0 = RMSW( ITOKT0 )
C----------------------------------------------------------------------
C  èOãìóEHHõâ TOK.
       KA = MSW(9 )
       KB = MSW(10)
       KC = MSW(11)
       K0 = MSW(12)
       VKA = 0.0
       IF(KA.GT.0)  VKA = VALLN1(KA)
       VKB = 0.0
       IF(KB.GT.0)  VKB = VALLN1(KB)
       VKC = 0.0
       IF(KC.GT.0)  VKC = VALLN1(KC)
       VK0 = 0.0
       IF(K0.GT.0)  VK0 = VALLN1(K0)
       ITOK = MSW(7)
       TOK12 = (VKA - VKB) * RMSW(ITOKT0+1) + RMSW(ITOKT0+7)
       TOK13 = (VKA - VKC) * RMSW(ITOKT0+2) + RMSW(ITOKT0+8)
       TOK23 = (VKB - VKC) * RMSW(ITOKT0+3) + RMSW(ITOKT0+9)
       TOK14 = (VKA - VK0) * RMSW(ITOKT0+4) + RMSW(ITOKT0+10)
       TOK24 = (VKB - VK0) * RMSW(ITOKT0+5) + RMSW(ITOKT0+11)
       TOK34 = (VKC - VK0) * RMSW(ITOKT0+6) + RMSW(ITOKT0+12)
       XANEW = TOK12 + TOK13 + TOK14
       XBNEW = TOK23 - TOK12 + TOK24
       XCNEW = TOK34 - TOK23 - TOK13
       X0NEW = TOK14 + TOK24 + TOK34
C----------------------------------------------------------------------
C  OÅHOBàM CTAPOE áHAóEHàE.
       RMSW( ITOKTA ) = XANEW
       RMSW( ITOKTB ) = XBNEW
       RMSW( ITOKTC ) = XCNEW
       RMSW( ITOKT0 ) = X0NEW
       TOKN1(ITOK  ) = XANEW
       TOKN1(ITOK+1) = XBNEW
       TOKN1(ITOK+2) = XCNEW
       TOKN1(ITOK+3) = X0NEW
       IF(MSW(ITOKT0+13).EQ.0) RETURN
C-->  èOÉPEòàM...
       X1 = XA
       X2 = XANEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 30
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 31
30     EPS = ABS( X1 - X2 )
31     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) GO TO 32
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) GO TO 32
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
32     X1 = XB
       X2 = XBNEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 33
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 34
33     EPS = ABS( X1 - X2 )
34     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) GO TO 35
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) GO TO 35
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
35     X1 = XC
       X2 = XCNEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 36
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 37
36     EPS = ABS( X1 - X2 )
37     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) GO TO 38
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) GO TO 38
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
38     X1 = X0
       X2 = X0NEW
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 39
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 40
39     EPS = ABS( X1 - X2 )
40     CONTINUE
       KOD = 0
       IF ( EPS .LT. EITER ) RETURN
C                            *******
       KOD = 1
       IF ( EPS .LT. MAXITR ) RETURN
C                             ******
       MAXITR = EPS
       NLITER = MSW(1)
       NSITER = 1
       RETURN
       END
