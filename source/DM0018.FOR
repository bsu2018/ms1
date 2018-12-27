         SUBROUTINE DM0018 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:  DM0018 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C    *                         MSWALL, RMSWAL, ADRMSW )
C
C   HAáHAóEHàE:    B áABàCàMOCTà OT áHAóEHàü îãAÉA ZAPROS [MASPRT(28)]:
C   -----------    BõèOãHüET CãEÑìûôàE îìHKñàà:
C
C                  A) ZAPROS = 1
C                     ----------
C                  îOPMàPOBAHàE BKãAÑOB B YALL MATPàñì èPOBOÑàMOCTEâ à
C                  JALL - BEKTOP àCTOóHàKOB TOKA áHAóEHàâ èAPAMETPOB
C                  ùKBàBAãEHTHõX  G - J BETBEâ  MOÑEãà ùãEMEHTA ñEèà
C                  Tàè     àMü MOÑEãà     OÅOáHAóEHàü B ÑAãúHEâòEM
C
C                  Tàè     àMü MOÑEãà     OÅOáHAóEHàü B ÑAãúHEâòEM
C
C                  64      3-X îAáHAü ù.Ñ.C        E-PEÉ(1)
C                          PEÉìãàPìEMAü OT "PB"
C
C
C                  65      3-X îAáHAü ù.Ñ.C.,
C                          ìèPABãüEMAü BHEòHàM     E-PEÉ(2)
C                          CàÉHAãOM .
C
C
C              ùãEMEHT KOÑàPìETCü:
C              -------------------
C
C              ù # T 64 ì K0 KA KB KC è T-BKã  R-BHìT #-ùã-TA  K-ìCàã;
C
C              ù # T 65 ì K0 KA KB KC è R-BHìT L-BHìT K-ìCàã  W  FI
C                                       #-ùã-TA  #-CMEô.
C
C     K0 - OÅôàâ ìáEã, KA - îAáA A, KB - îAáA B, KC - îAáA C;
C
C---> BõBOÑHõE áHAóEHàü: TOKà: K ìáãAM KA, KB, KC, OT ìáãA K0,
C                              *                   **
C                      áHAóEHàü ù.Ñ.C. E1, E2, E3;
C
C
C !!!!   áAÑEPÜKA HA OÑàH òAÉ àHTEÉPàPOBAHàü !!!
C        -----------------------------------
C
C
C
C                  ***************************************************
C                  B) ZAPROS = 2
C                     ----------
C                  BõóàCãEHàE áHAóEHàâ TOKOB B BETBüX B T(N+1) MOMEHT
C                  BPEMEHà   àCXOÑü àá G à J èAPAMETPOB à áHAóEHàü
C                  ìáãOBõX HAèPüÜEHàâ HA ùãEMEHTE PACóàTAHHõX Ñãü T(N+1)
C                    à áAèàCú (îàKCAñàü) ùTàX áHAóEHàâ B "TOK T(N+1)";
C
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
C   METOÑ:         ÑàCKPETàáAñàü ìPABHEHàâ COCTOüHàü MOÑEãà ÑAHHOÉO
C   -----          TàèA HA OCHOBE METOÑA ãàHàÑÜEPA BàOãìÉÅà:
C                  Y(N+1) = Y(N) + H1*F(N+1) + H2*F(N).
C
C   èAPAMETPõ:     MSW, RMSW - ìèPABãüûôEE CãOBO COCTOüHàü MAKPOMOÑEãà
C   ---------      ( "OTPEáOK" MACCàBõ MSWALL -> C.M. è/è GETMSW );
C                  YALL - MATPàñA èP0BOÑàMOCTà AHAãàáàPìEMOâ ñEèà;
C                  JALL - BEKTOP àCTOóHàKOB TOKA AHAãàáàPìEMOâ ñEèà;
C                  VALLN - BEKTOP ìáãOBõX HAèPüÜEHàâ AHAãàáàPìEMOâ ñEèà:
C                          èPà  T  =  TN;
C                  VALLN1- BEKTOP ìáãOBõX HAèPüÜEHàâ AHAãàáàPìEMOâ ñEèà:
C                           èPà  T  =  TN  + H0;
C                  TOKN  à  TOKN1 - BEKTOPA èEPEMEHHõX "COCTOüHàü"
C                                   ùãEMEHTA èPà  TN  à  TN + H0;
C                                   PAáMEPHOCTà CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP èPàáHAKOB K.á. ìáãOB:
C                  àCèOãúáìETCü B MAKPOMOÑEãüX C èPàáHAKOM TàèA = 5;
C                  PAáMEPHOCà MACCàBOB ÉPìèèõ "ALL" OèPEÑEãüûTCü:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEKìôAü_PAáMEPHOCTú ---> NSYS = MASPRT(26)
C                  KOD - èAPAMETP OÅPATHOâ CBüáà C MOHàTOPOM DM....
C
C
C
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, PROF2, PROF3;
C   ------------             DEPOST;
C
C   èPàMEóAHàE:  1.CTPìKTìPì MSW CãOBA MAKPOMOÑEãà ÑAHHOÉO TàèA CM. HàÜE
C   ----------   2. èPà ZAPROS ^=   1  àãà 2 - COOÅôEHàE à ìXOÑ;
C
C
C               "èOCTOüHHAü" óACTú MSW TàèA # 64 à #65
C                --------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I  èPàá. I KOãà I KOã. IAÑP.   IPE I
C  I ùãEME I TàèA   I è/è    I  TàèA  I óECT I BõB. IáHAó.  IáE I
C  I HTA   I MOÑEãà I PACóETAI  MOÑEãàI BO   I áHAó I"TOKN" IPB I
C  I       I        I DM.....I        I ìáãOBI EHàâ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-----------+
C
C
C
C                  RMSW TàèA # 64 [ àCTOóHàK HAèPüÜEHàü OT "PEÉ.BOáÅ" ]
C                  -----------------------------------------
C
C                    TOK(J) = K_ìC. * E(J) / R_BHìT. + 1/R * U_N+1
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II áHAó.  I áHAó.I # ùã-TAI AÑP. II      IJESIJESI K   I
C   ìáãõ:     II  1/R   I T_BKãI "PB"   I  B   II JESA I   I   I ìCàãI
C  K0         II        I      I        I TOKN II      I B I C I EHàüI
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
C                         RMSW TàèA # 65
C                         --------------
C
C                             SPM                 SRAB
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C            II #   I #   IAÑP.I   I   I K I   I   I   I    I    I    I
C  ìáãõ:     IIùã-TAICMEô.I B  I R I L IìCàI W I FII G IJESAIJESBIJESCI
C K0         II     I     ITOKNI   I   IãEHI   I   I   I    I    I    I
C   K1       ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C     K2     II  1  I  2  I  3 I 4 I 5 I 6 I 7 I 8 I 9 I 10 I 11 I 12 I
C        K3  ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C 9 10 11 12 II 13  I 14  I 15 I16 I17 I18 I19 I20 I21 I 22 I 23 I 24 I
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JESA, JESB, JESC
         INTEGER ADRMSW, TIP
         INTEGER FKOM, ZAPROS
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
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
C
         DATA R120/ 2.0944/, SQ3D2/ 0.8660254 /, PI2/6.283185306/
         DATA RGRAD / 57.29577 /
C
         KOD = 0
       TIP = MSW(2)
       IF ( TIP .EQ. 64  .OR.  TIP .EQ. 65  ) GO TO 10
C----------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ãOÉ. CÅOâ B DM0018 - áAèPOC HA OÅCãìÜàBAHàE óìÜOÉ
     =O TàèA ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
10     CONTINUE
C---> BõÅOP PEÜàMA:
         GO TO ( 667, 666                 ), ZAPROS
C===> àHAóE OòàÅKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHàTOP #', I4, ' áAèPOC OÅCãìÜàBAHàü OòàÅOóEH
     = ***'/ ' *** ZAPROS = ', I5, ' - àÉHOPàPìETCü ***')
         RETURN
667      CONTINUE
C---------ZAPROS = 1---------------------------------------------------
C
C--->   èOùãEMEHTHõâ BKãAÑ:    -->>
C==> "HOBOE" BPEMü   ( ÑA, ìÜ.... )
         TN1 = TN + H0
C-->  ìáãõ*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
       IF( TIP .EQ. 65 ) GO TO 100
C----------------------------------------------------------------------
C      àCTOóHàK 3-X îAáHOâ ù.Ñ.C. (Tàè 64)
C----------------------------------------------------------------------
         GES = RMSW(13)
         JESA = 0.0
         JESB = 0.0
         JESC = 0.0
C-->  PAÅOTAEM ?
         TBKL = RMSW(14)
         IF ( TN .LT. TBKL ) GO TO 50
C-->  BOáMEM AMèãàTìÑõ..
         ITRB = MSW(16)
         RKUS = RMSW(20) * GES
         JESA = TOKN( ITRB + 0 ) * RKUS
         JESB = TOKN( ITRB + 1 ) * RKUS
         JESC = TOKN( ITRB + 2 ) * RKUS
50       CONTINUE
C-->  áAèOMHàMCü
         RMSW(17) = JESA
         RMSW(18) = JESB
         RMSW(19) = JESC
C======================================================================
C----> BKãAÑõBAEM
         CALL DEPOST( K0, KA, GES, JESA, YALL, JALL )
         CALL DEPOST( K0, KB, GES, JESB, YALL, JALL )
         CALL DEPOST( K0, KC, GES, JESC, YALL, JALL )
C---->
         RETURN
C######################################################################
100    CONTINUE
C----------------------------------------------------------------------
C      àCTOóHàK 3-X îAáHOâ ù.Ñ.C. (Tàè 65)
C----------------------------------------------------------------------
CC     Z = RMSW(17) + H0 * RMSW(16)
       Z =                 RMSW(16)
       GES =  1.0 / Z
C--> CóàTAEM AMèãàTìÑì.
       IUPR = MSW(15)
       EM = TOKN(IUPR) * RMSW(18)
C--> HAXOÑàM àCTOóHàKà.
       W = RMSW(19)
       FI= RMSW(20)
       EA1 = EM * SIN( W * TN1 + FI )
       EB1 = EM * SIN( W * TN1 + FI - 2.0944 )
       EC1 = EM * SIN( W * TN1 + FI + 2.0944 )
C---> CTAPõE TOKà.
       ITOK = MSW(7)
       TOKA = TOKN(ITOK)
       TOKB = TOKN(ITOK+1)
       TOKC = TOKN(ITOK+2)
C--> àCTOóHàKà TOKA
       JESA =(      EA1                   ) / Z
       JESB =(      EB1                   ) / Z
       JESC =(      EC1                   ) / Z
C-->  áAèOMHàMCü
         RMSW(21) = GES
         RMSW(22) = JESA
         RMSW(23) = JESB
         RMSW(24) = JESC
C======================================================================
C----> BKãAÑõBAEM
         CALL DEPOST( K0, KA, GES, JESA, YALL, JALL )
         CALL DEPOST( K0, KB, GES, JESB, YALL, JALL )
         CALL DEPOST( K0, KC, GES, JESC, YALL, JALL )
C---->
         RETURN
C======================================================================
C$$$$$$$$$$$$$$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C-->  ìáãõ*******
       K0 = MSW(9)
       KA = MSW(10)
       KB = MSW(11)
       KC = MSW(12)
       IF( TIP .EQ. 65 ) GO TO 200
C----------------------------------------------------------------------
C      àCTOóHàK 3-X îAáHOâ ù.Ñ.C. (Tàè 64)
C----------------------------------------------------------------------
C---->
       GES = RMSW(13)
       JESA = RMSW(17)
       JESB = RMSW(18)
       JESC = RMSW(19)
C--->  HOBõâ TOK:
         V0 = 0.0
         IF ( K0 .NE. 0 ) V0 = VALLN1(K0)
         VA = 0.0
         IF ( KA .NE. 0 ) VA = VALLN1(KA)
         VB = 0.0
         IF ( KB .NE. 0 ) VB = VALLN1(KB)
         VC = 0.0
         IF ( KC .NE. 0 ) VC = VALLN1(KC)
C-->
         TOK1 = GES      * (V0-VA)  + JESA
         TOK2 = GES      * (V0-VB)  + JESB
         TOK3 = GES      * (V0-VC)  + JESC
C--
       TOK0 = TOK1 + TOK2 + TOK3
C-->
C--->     áAèàCú TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOK1
         TOKN1( ITOK + 1 ) = TOK2
         TOKN1( ITOK + 2 ) = TOK3
         TOKN1( ITOK + 3 ) = TOK0
C----------------------------------------------------------------------
C--> áAèàCú ù.Ñ.C.  ìèPABãEHàü
         ITRB = MSW(16)
         RKUS1 = RMSW(20)
         TOKN1(ITOK+ 4 ) = TOKN( ITRB + 0 ) * RKUS1
         TOKN1(ITOK+ 5 ) = TOKN( ITRB + 1 ) * RKUS1
         TOKN1(ITOK+ 6 ) = TOKN( ITRB + 2 ) * RKUS1
C
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
200    CONTINUE
C----------------------------------------------------------------------
C      àCTOóHàK 3-X îAáHOâ ù.Ñ.C. (Tàè 64)
C----------------------------------------------------------------------
       GES = RMSW(21)
       JESA = RMSW(22)
       JESB = RMSW(23)
       JESC = RMSW(24)
C--->  HOBOE HAèPüÜEHàE.
         V0 = 0.0
         IF ( K0 .NE. 0 ) V0 = VALLN1(K0)
         VA = 0.0
         IF ( KA .NE. 0 ) VA = VALLN1(KA)
         VB = 0.0
         IF ( KB .NE. 0 ) VB = VALLN1(KB)
         VC = 0.0
         IF ( KC .NE. 0 ) VC = VALLN1(KC)
C-->   HOBõâ TOK.
         TOK1 = GES * (V0-VA)  + JESA
         TOK2 = GES * (V0-VB)  + JESB
         TOK3 = GES * (V0-VC)  + JESC
C-->
         TOK0 = TOK1 + TOK2 + TOK3
C-->
C--->     áAèàCú TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = TOK1
         TOKN1( ITOK + 1 ) = TOK2
         TOKN1( ITOK + 2 ) = TOK3
         TOKN1( ITOK + 3 ) = TOK0
       RETURN
C----------------------------------------------------------------------
        END
