         SUBROUTINE DM0034 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT MAPTA   1988 É.)------------
C   èOÑèPOÉPAMMA:  DM0034 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HAáHAóEHàE:    B áABàCàMOCTà OT áHAóEHàü îãAÉA ZAPROS [MASPRT(28)]:
C   -----------    BõèOãHüET CãEÑìûôàE îìHKñàà:
C
C                  A) ZAPROS = 1
C                     ----------
C                  îOPMàPOBAHàE BKãAÑOB B YALL MATPàñì èPOBOÑàMOCTEâ à
C                  JALL - BEKTOP àCTOóHàKOB TOKA áHAóEHàâ èAPAMETPOB
C                  ùKBàBAãEHTHõX  G - J BETBEâ  MOÑEãà ùãEMEHTA ñEèà
C
C               *******************************************************
C               *  TàèA # 520 [ ÑBàÉATEãú èOCTOüHHOÉO TOKA (î=CONST)]
C               *  TàèA # 521 [ ÑBàÉATEãú èOCTOüHHOÉO TOKA (î=CONST)]
C               *******************************************************
C
C                  B) ZAPROS = 2
C                     ----------
C                  BõóàCãEHàE áHAóEHàâ TOKOB B BETBüX B T(N+1) MOMEHT
C                  BPEMEHà   àCXOÑü àá G à J èAPAMETPOB à áHAóEHàü
C                  ìáãOBõX HAèPüÜEHàâ HA ùãEMEHTE PACóàTAHHõX Ñãü T(N+1)
C                    à áAèàCú (îàKCAñàü) ùTàX áHAóEHàâ B "TOK T(N+1)";
C
C                  C) ZAPROS = 3
C                     **********
C                     HE èOÑÑEPÜàBAETCü ( PEáEPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE èOÑÑEPÜàBAETCü;
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
C                   ( àCèOãúáìETCü Ñãü MAKPOMOÑEãEâ "àÑEAãúHõX" KãûóEâ)
C
C
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST;
C
C   èPàMEóAHàE:  1.CTPìKTìPì MSW CãOBA MAKPOMOÑEãà ÑAHHOÉO TàèA CM. HàÜE
C   ----------   2. èPà ZAPROS ^=   1 àãà 2 - COOÅôEHàE à ìXOÑ;
C
C
C            "èOCTOüHHAü" óACTú MSW TàèA # 520,521:
C             -------------------------------------
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
C              îOPMAT RMSW Tàè # 520:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # CãOBA   I    # CãOBA B I KTO  I
C OT HAóAãA  I    OTPEáKE   IOèPEÑ.I
C BCEÉO MSW  I    RMSW B    I      I       OÅOáHAóEHàE - áHAóEHàE
C OèàCAHàü   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I      I HOMEP ìáãA HAóAãA
C    10      I     0        I      I HOMEP ìáãA KOHñA
C----------------------------------------------------------------------
C    11      I     1        I  FM  I  R  - COèPOTàBãEHàE üKOPü;
C    12      I     2        I  FM  I  L  - àHÑìKTàBHOCTú üKOPü;
C    13      I     3        I  FM  I  Kî - KOHCTPìKTàBHõâ KOùîîàñàEHT;
C    14      I     4        I  FM  I  JM - MOMEHT àHEPñàà;
C    15      I     5        I  FM  I        PEáEPB
C    16      I     6        I  FM  I  Tü - èOCTOüHHAü üKOPHOâ ñEèà;
C    17      I     7        I  FM  I  TM - èOCTOüHHAü MEXAHàóECKAü;
C    18      I     8        I  DM  I  MBH- áHAóEHàE MBH Ñãü T=TN;
C    19      I     9        I  DM  I  MBH1-áHAóEHàE MBH Ñãü T=TN1;
C    20      I    10        I  DM  I  G -  áHAóEHàE èPOBOÑàMOCTà;
C    21      I    11        I  DM  I  J -  áHAóEHàE àCT. TOKA;
C    22      I    12        I  FM  I         PEáEPB
C    23      I    13        I  FM  I         PEáEPB
C    24      I    14        I  FM  I  NT - KOãàóECTBO TOóEK MBH(T);
C    25      I    15        I  FM  I  T1 - BPEMü;                      ;
C    26      I    16        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  OÅôAü ÑãàHA MSW --> LM<BCü> = 24 + 2*NT
C
C  ÑãàHA èEPEMEH. óACTà --> LM<RMSW> = 14 + 2*NT
C
C----------------------------------------------------------------------
C
C
C
C              îOPMAT RMSW Tàè # 521:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # CãOBA   I    # CãOBA B I KTO  I
C OT HAóAãA  I    OTPEáKE   IOèPEÑ.I
C BCEÉO MSW  I    RMSW B    I      I       OÅOáHAóEHàE - áHAóEHàE
C OèàCAHàü   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -3        I      I HOMEP ìáãA HAóAãA OÅMOTKà üKOPü
C    10      I    -2        I      I HOMEP ìáãA KOHñA  OÅMOTKà üKOPü
C    11      I    -1        I      I HOMEP ìáãA HAóAãA OÅMOTKà TÉ.
C    12      I     0        I      I HOMEP ìáãA KOHñA  OÅMOTKà TÉ.
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  R  - COèPOTàBãEHàE üKOPü;
C    14      I     2        I  FM  I  L  - àHÑìKTàBHOCTú üKOPü;
C    15      I     3        I  FM  I  Kî - KOHCTPìKTàBHõâ KOùîîàñàEHT;
C    16      I     4        I  FM  I  JM - MOMEHT àHEPñàà;
C    17      I     5        I  FM  I  KTG- KOùîàñàEHT èEPEÑAóà TÉ.
C    18      I     6        I  FM  I  GTG- èPOBOÑàMOCTú OÅMOTKà TÉ.
C    19      I     7        I  FM  I  Tü - èOCTOüHHAü üKOPHOâ ñEèà;
C    20      I     8        I  FM  I  TM - èOCTOüHHAü MEXAHàóECKAü;
C    21      I     9        I  DM  I  MBH- áHAóEHàE MBH Ñãü T=TN;
C    22      I    10        I  DM  I  MBH1-áHAóEHàE MBH Ñãü T=TN1;
C    23      I    11        I  DM  I  GR-  èPOBOÑàMOCTú ñEèà üKOPü;
C    24      I    12        I  DM  I  JR-  àCT. TOKA    ñEèà üKOPü;
C    25      I    13        I  DM  I  GRG- èPOBOÑàMOCTú ñEèú üKOPü - TÉ
C    26      I    14        I  DM  I  JRG- àCT. TOKA    ñEèú üKOPü - TÉ
C    27      I    15        I  FM  I  NT - KOãàóECTBO TOóEK MBH(T);
C    28      I    16        I  FM  I  T1 - BPEMü;                      ;
C    29      I    17        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  OÅôAü ÑãàHA MSW --> LM<BCü> = 27 + 2*NT
C
C  ÑãàHA èEPEMEH. óACTà --> LM<RMSW> = 15 + 2*NT
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         REAL JALL,  JM,  KF,  L, J, MBH, MBH1 , KMAS02, KTG, JR, JRG
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
         INTEGER FKOM, ZAPROS
         TN1 = TN + H0
C--->  KOHTPOãú TàèA
         NTIP = MSW(2) - 519
         IF ( NTIP .LT. 1  .OR.  NTIP .GT. 2  ) GO TO  9999
C---> BõÅOP PEÜàMA:
         GO TO ( 667, 666                 ), ZAPROS
C===> àHAóE OòàÅKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHàTOP #', I4, ' áAèPOC OÅCãìÜàBAHàü OòàÅOóEH
     = ***'/ ' *** ZAPROS = ', I5, ' - àÉHOPàPìETCü ***')
         RETURN
667      CONTINUE
C######################################################################
C--------------------ZAPROS=1------------------------------------------
C--->  BõóàCãEHàE G - J èAPAMETPOB à àX áAèàCú B MSW:
         IF( NTIP .EQ. 2 ) GO TO 700
C######################################################################
C
C   ÑBàÉATEãú èOCTOüHHOÉO TOKA ( Tàè # 520 )
C
C######################################################################
C---->   ìáãõ èOÑKãûóEHàü:
         K = MSW(9)
         M = MSW(10)
C-->   èAPAMETPõ
         R  = RMSW(11)
         L  = RMSW(12)
         KF = RMSW(13)
         JM = RMSW(14)
         TR = RMSW(16)
         TM = RMSW(17)
C----------------------------------------------------------------------
         ITOK = MSW(7)
         TOKTN = TOKN( ITOK )
         WN    = TOKN( ITOK + 2 )
         MBH   = TOKN( ITOK + 3 )
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
C----------------------------------------------------------------------
         Z1 = TR*TM + TM*H1 + H1**2
         Z2 = TR*TM - TM*H2 - H1*H2
         Z3 = H1 / KF
C----------------------------------------------------------------------
         MBH1 = KMAS02( TN1 , RMSW(24) )
C----------------------------------------------------------------------
         G = TM*H1 / (R*Z1)
C--->
         J = (TOKTN*Z2 + UN*TM*H2/R - WN*JM*H0/KF + MBH1*Z3*H1 +
     +        MBH*Z3*H2 ) / Z1
C----------------------------------------------------------------------
         RMSW(18) = MBH
         RMSW(19) = MBH1
         RMSW(20) = G
         RMSW(21) = J
C--->   èOùãEMEHTHõâ BKãAÑ:
C======================================================================
         KOD = 0
C--->
         CALL DEPOST(K, M,  G, J, YALL, JALL )
C---->
         RETURN
C======================================================================
700      CONTINUE
C######################################################################
C
C   ÑBàÉATEãú èOCTOüHHOÉO TOKA C TAXOÉEHEPATOPOM ( Tàè # 521 )
C
C######################################################################
C---->   ìáãõ èOÑKãûóEHàü:
         M = MSW(9)
         N = MSW(10)
         K1= MSW(11)
         K2= MSW(12)
C-->   èAPAMETPõ
         R  = RMSW(13)
         L  = RMSW(14)
         KF = RMSW(15)
         JM = RMSW(16)
         KTG= RMSW(17)
         GTG= RMSW(18)
         TR = RMSW(19)
         TM = RMSW(20)
C-->  TOKà à HAèPüÜEHàü Ñãü T=TN
         ITOK = MSW(7)
         TOKTN = TOKN( ITOK )
         WN    = TOKN( ITOK + 2 )
         MBH   = TOKN( ITOK + 3 )
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN(N)
         URN = VM - VN
C----------------------------------------------------------------------
C---> GJ-èAPAMETPõ üKOPHOâ ñEèà.
         Z1 = TR*TM + TM*H1 + H1**2
         Z2 = TR*TM - TM*H2 - H1*H2
         Z3 = H1 / KF
C----------------------------------------------------------------------
         MBH1 = KMAS02( TN1 , RMSW(27) )
C----------------------------------------------------------------------
         GR = TM*H1 / (R*Z1)
C--->
         JR = (TOKTN*Z2 + URN*TM*H2/R - WN*JM*H0/KF + MBH1*Z3*H1 +
     +        MBH*Z3*H2 ) / Z1
C----------------------------------------------------------------------
C---> áAèOMHàM
         RMSW(21) = MBH
         RMSW(22) = MBH1
         RMSW(23) = GR
         RMSW(24) = JR
C--->   èOùãEMEHTHõâ BKãAÑ:
C
         CALL DEPOST(M, N,  GR, JR, YALL, JALL )
C----------------------------------------------------------------------
C---> GJ-èAPAMETPõ ñEèà TÉ.
         GRG = KTG * GTG * H1 * KF / JM
         JRG = KTG * GTG * ( WN - H1*MBH1/JM + H2*(TOKTN*KF-MBH)/JM )
C---> áAèOMHàM
         RMSW(25) = GRG
         RMSW(26) = JRG
C---> BãOÜàM...
         J = - GRG * JR - JRG
C
         CALL DEPOST(K1, K2,  GTG, J, YALL, JALL )
C
         G = - GRG * GR
C
         CALL DEPOSN(K1, K2, M, N, G, YALL )
C
         KOD = 0
         RETURN
C======================================================================
666      CONTINUE
C######################################################################
C--->  OèPEÑEãüEM TOKà:  ZAPROS = 2
         IF( NTIP .EQ. 2 ) GO TO 800
C######################################################################
C
C   ÑBàÉATEãú èOCTOüHHOÉO TOKA ( Tàè # 520 )
C
C######################################################################
C-->   èAPAMETPõ
         KF = RMSW(13)
         JM = RMSW(14)
         MBH = RMSW(18)
         MBH1= RMSW(19)
         G   = RMSW(20)
         J   = RMSW(21)
C---->   ìáãõ èOÑKãûóEHàü:
         K = MSW(9)
         M = MSW(10)
C--->  HOBOE HAèPüÜEHàE:
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UN1= VK - VM
C--->  CTAPõE TOKà:
         ITOK = MSW(7)
         TOKTN = TOKN(ITOK)
         YN    = TOKN(ITOK+1)
         WN    = TOKN(ITOK+2)
C--->  HOBõE TOKà:
         TOKTN1 = G * UN1 + J
         WN1 = WN + ( H1*(TOKTN1*KF-MBH1) + H2*(TOKTN*KF-MBH) ) / JM
         YN1 = YN + H1*WN1 + H2*WN
C----------------------------------------------------------------------
C--->     áAèàCú TOKOB  B TOKN1
         TOKN1( ITOK    ) = TOKTN1
         TOKN1( ITOK +1 ) = YN1
         TOKN1( ITOK +2 ) = WN1
         TOKN1( ITOK +3 ) = MBH1
         KOD = 0
         RETURN
C----------------------------------------------------------------------
800      CONTINUE
C######################################################################
C
C   ÑBàÉATEãú èOCTOüHHOÉO TOKA C TAXOÉEHEPATOPOM ( Tàè # 521 )
C
C######################################################################
C-->   èAPAMETPõ
         KF = RMSW(15)
         JM = RMSW(16)
         GTG= RMSW(18)
         MBH = RMSW(21)
         MBH1= RMSW(22)
         GR  = RMSW(23)
         JR  = RMSW(24)
         GRG = RMSW(25)
         JRG = RMSW(26)
C---->   ìáãõ èOÑKãûóEHàü:
         M = MSW(9)
         N = MSW(10)
         K1= MSW(11)
         K2= MSW(12)
C--->  HOBOE HAèPüÜEHàE:
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         VN = 0.0
         IF ( N .NE. 0 ) VN = VALLN1(N)
         URN1= VM - VN
         VK1 = 0.0
         IF ( K1 .NE. 0 ) VK1 = VALLN1(K1)
         VK2 = 0.0
         IF ( K2 .NE. 0 ) VK2 = VALLN1(K2)
         UTGN1= VK1 - VK2
C--->  CTAPõE TOKà:
         ITOK = MSW(7)
         TOKTN = TOKN(ITOK)
         YN    = TOKN(ITOK+1)
         WN    = TOKN(ITOK+2)
C--->  HOBõE TOKà:
         TOKTN1 = GR * URN1 + JR
         WN1 = WN + ( H1*(TOKTN1*KF-MBH1) + H2*(TOKTN*KF-MBH) ) / JM
         YN1 = YN + H1*WN1 + H2*WN
         TOKTG1 = UTGN1 * GTG - GRG * GR * URN1 - GRG * JR - JRG
C----------------------------------------------------------------------
C--->     áAèàCú TOKOB  B TOKN1
         TOKN1( ITOK    ) = TOKTN1
         TOKN1( ITOK +1 ) = YN1
         TOKN1( ITOK +2 ) = WN1
         TOKN1( ITOK +3 ) = MBH1
         TOKN1( ITOK +4 ) = TOKTG1
         KOD = 0
         RETURN
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ãOÉ. CÅOâ B DM0034 - áAèPOC HA OÅCãìÜàBAHàE óìÜOÉ
     =O TàèA ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
C******* DEBUG INIT
         END
