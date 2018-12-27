         SUBROUTINE DM0027 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT   OKTüÅPü   1987 É.)------
C   èOÑèPOÉPAMMA:  DM0027 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HAáHAóEHàE:    B áABàCàMOCTà OT áHAóEHàü îãAÉA ZAPROS [MASPRT(28)]:
C   -----------    BõèOãHüET CãEÑìûôàE îìHKñàà:
C
C                  MOÑEãú # 17--> àCTOóHàK TOKA, ìèPABãüEMõâ è/è
C                                 èOãúáOBATEãü.
C
C
C              ùãEMEHT OèàCõBAETCü:
C
C      ù # T 17 ì K1 K2 è
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
C               "èOCTOüHHAü" óACTú MSW TàèA # 17:
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
C              îOPMAT RMSW Tàè # 17:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # CãOBA   I    # CãOBA B I KTO  I
C OT HAóAãA  I    OTPEáKE   IOèPEÑ.I
C BCEÉO MSW  I    RMSW B    I      I       OÅOáHAóEHàE - áHAóEHàE
C OèàCAHàü   I    FM0023    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I      I HOMEP ìáãA àCXOÑüôEÉO.
C    10      I     0        I      I HOMEP ìáãA BXOÑüôEÉO.
C----------------------------------------------------------------------
C    11      I     1        I  FM  I  NTOKOB -KOãàóECTBO TOKOB, àCèOãú-
C            I              I      I          áìEMõX èPà PACóETE TOKA
C            I              I      I          àCTOóHàKA;
C    12      I     2        I  FM  I  #ùã1 - HOMEP ùãEMEHTA;
C    13      I     3        I  FM  I  #CM1 - HOMEP TOKA ùãEMEHTA #ùã1;
C    14      I     4        I  LM  I  ATOKM1 - AÑP B TOKN/N1 áHAóEHàü
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ùãEMEHTA #ùã1;
C  9+I*3     I  -1+I*3      I  FM  I  #ùãI -      -"-
C 10+I*3     I     I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   1+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NTOKOB*3 I  2+NTOKOB*3  I  FM  I  NHAèP -KOãàóECTBO HAèPüÜEHàâ,
C            I              I      I         àCè. Ñãü PACóETA TOKA
C            I              I      I         àCTOóHààKA;
C13+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  ìáEãN1 -ìáEã HAóAãA HAèPüÜEHàü;
C14+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  ìáEãK1 -ìáEã KOHñA  HAèPüÜEHàü;
C .........  I ............ I .... I  ...........................
C            I              I      I
C13+NTOKOB*3+I 2+NTOKOB*3+  I      I  NPSUB - HOMEP è/è èOãúáOBATEãü
C NHAèP*2    I  NHAèP*2     I  FM  I         ( PSUB) B KOTOPOâ èPOàáBO-
C            I              I      I         ÑàTCü PACóET TOKA àCTOóHàKA
C (...)+1    I  (...)+1     I  FM  I  NPAR - KOãàóECTBO èAPAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  èAPAMETPõ PSUB , áAÑAHHõE
CÑO(...)+NPARI ÑO(...)+NPAR I      I  èOãúáOBATEãEM.
C            I              I      I
C 15+3*NTOKOBI 5 + 3*NTOKOB I  DM  I  TOKT - áHAóEHàE TOKA àCTOóHàKA
C +2*NNAPR + I + 2*NNAPR +  I      I         èPà  T=TN+1
C NPAR       I NPAR         I      I         ( èPOÉHOá )
C 16+3*NTOKOBI 6 + 3*NTOKOB I  DM  I   G - áHAóEHàE èPOBOÑàMOCTà
C +2*NNAPR + I + 2*NNAPR +  I      I          èPà T-TH;1
C NPAR       I NPAR         I      I         ( èO CèPOÉHOáàP. TOKì )
C            I              I      I
C 17+3*NTOKOBI 7 + 3*NTOKOB I  DM  I   J - áHAóEHàE àCTOóHàKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I          èPà T=TN+1
C NPAR       I NPAR         I      I
C            I              I      I
C 18+3*NTOKOBI 8 + 3*NTOKOB I  DM  I    NMOD - èPàáHAK MOÑEãà
C +2*NNAPR + I + 2*NNAPR +  I      I           0 - ãàHEâHAü
C NPAR       I NPAR         I      I         > 0 - HEãàHEâHAü
C            I              I      I
C----------------------------------------------------------------------
C
C  OÅôAü ÑãàHA MSW --> LM<BCü> = 18 + 3*NTOKOB + 2*NNAPR + NPAR
C
C  ÑãàHA èEPEMEH. óACTà --> LM<RMSW> = 8 + 3*NTOKOB + 2*NNAPR + NPAR
C
C     B MACCàBE TOKN à TOKN1 COÑEPÜàTCü:
C     **********************************
C     1    -     áHAóEHàE TOKA àCTOóHàKA TOKA èPà T=TN/TN1
C     2    -     áHAóEHàE AKTàBHOâ MOôHOCTà KOMèEHCàPìEMOâ HAÉPìáKà
C     3    -     áHAóEHàE OÅMEHHOâ MOôHOCTà KOMèEHCATOPA
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION INTMAS(200), STMAS(50), DGMAS(5)
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
       IF ( TIP .EQ. 17 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ãOÉ. CÅOâ B DM0027 - áAèPOC HA OÅCãìÜàBAHàE óìÜOÉ
     =O TàèA ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C----------------------------------------------------------------------
C-->  OèPEÑEãàM KOHCTAHTõ.
       NTOKOB = MSW(11)
       NNAPR = MSW (12 + 3*NTOKOB )
       INPAR = 14 + 3*NTOKOB + 2*NNAPR
       NPAR  = MSW (INPAR)
       ITOKT = INPAR + 1 + NPAR
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
          ITOKN = MSW( 3*I + 11 )
          STMAS(ISTMAS) = TOKN(ITOKN)
15     CONTINUE
16     CONTINUE
C----------------------------------------------------------------------
C--> èEPEÑAEM HAèPüÜEHàü
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 18
       IMSWV = 11 + 3*NTOKOB
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
       INPSUB = 13 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PACóàTõBAEM áHAóEHàE TOKA èPà T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 1.
       ITOK = MSW(7)
       DGMAS(2) = TOKN( ITOK )
       RMSW(ITOKT)=TOKN(ITOK)
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT = DGMAS(2)
       G=DGMAS(3)
       RMSW( ITOKT+2 ) = TOKT
       RMSW( ITOKT+1 ) = G
       MSW( ITOKT+3 ) = IFIX(DGMAS(1))
       TOKN1( ITOK + 1 ) = DGMAS(4)
       TOKN1( ITOK + 2 ) = DGMAS(5)
       GO TO 170
150    CONTINUE
C----------------------------------------------------------------------
C -->  àTEPAñàü HE èEPBAü...
       IF(MSW(ITOKT+3).EQ.0) GO TO 160
       STMAS( 1) = TN
       STMAS( 2) = H0
       ISTMAS = 2
C----------------------------------------------------------------------
C--> èEPEÑAEM TOKà
       IF( NTOKOB .EQ. 0 ) GO TO 26
          DO 25 I= 1,NTOKOB
          ISTMAS = ISTMAS + 1
          ITOKN = MSW( 3*I + 11 )
          STMAS(ISTMAS) = TOKN1(ITOKN)
25     CONTINUE
26     CONTINUE
C----------------------------------------------------------------------
C--> èEPEÑAEM HAèPüÜEHàü
C----------------------------------------------------------------------
       IF( NNAPR .EQ. 0 ) GO TO 28
       IMSWV = 11 + 3*NTOKOB
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
       INPSUB = 13 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PACóàTõBAEM áHAóEHàE TOKA èPà T=TN+1
C----------------------------------------------------------------------
       DGMAS(1) = 2.
       DGMAS(2) = RMSW( ITOKT )
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
       TOKT = DGMAS(2)
       G=DGMAS(3)
       RMSW(ITOKT+2) = TOKT
       RMSW(ITOKT+1) = G
       ITOK = MSW(7)
       TOKN1( ITOK + 1 ) = DGMAS(4)
       TOKN1( ITOK + 2 ) = DGMAS(5)
       GO TO 170
 160   CONTINUE
       G = RMSW( ITOKT+1 )
       TOKT = RMSW(ITOKT+2)
170    CONTINUE
C----------------------------------------------------------------------
C -->  èOùãEMEHTHõâ BKãAÑ......
C----------------------------------------------------------------------
       K = MSW(9)
       M = MSW(10)
       KOD = 0
C--->
       CALL DEPOST( K , M , G , TOKT , YALL , JALL )
C--->
       RETURN
C######################################################################
200    CONTINUE
C----------------------------------------------------------------------
C-->  ZAPROS = 2  --> PACóàTATú TOK Ñãü T=TN+1 à áAèàCATú B TOKN1
C----------------------------------------------------------------------
C  èPEÑCKAáAHHõâ TOK.
       X1 = RMSW( ITOKT )
C----------------------------------------------------------------------
C  èOãìóEHHõâ TOK.
       K = MSW(9)
       M = MSW(10)
       VK = 0.0
       IF(K.GT.0) VK = VALLN1(K)
       VM = 0.0
       IF(M.GT.0) VM = VALLN1(M)
       VKM = VK-VM
       ITOK = MSW(7)
       X2 = RMSW(ITOKT+1)*VKM+RMSW(ITOKT+2)
C----------------------------------------------------------------------
C  OÅHOBàM CTAPOE áHAóEHàE.
       RMSW( ITOKT ) = X2
       TOKN1(ITOK) =X2
       IF(MSW(ITOKT+3).EQ.0) RETURN
C-->  èOÉPEòàM...
       X1 = ABS(X1)
       X2 = ABS(X2)
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP ) GO TO 30
       XZ = AMAX1(X1, X2 )
       EPS = ABS(X1-X2) / XZ
       GO TO 31
30     EPS = ABS( X1 - X2 )
31     CONTINUE
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