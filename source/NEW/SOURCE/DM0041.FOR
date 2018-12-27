         SUBROUTINE DM0041 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT CEHTüÅPü 1988 É.)------------
C   èOÑèPOÉPAMMA:  DM0041 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C                  TàèA # 42- 45  -->  TPAHCîOPMATOPõ ;
C                  *********************
C     KOã-BO OÅ-K  Tàè     àMü MOÑEãà     OÅOáHAóEHàü B ÑAãúHEâòEM
C
C     K = 2        42      2-X OÅM. TPAHC.       TP2
C     K = 3        43      3-X OÅM. TPAHC.       TP3
C     K = 4        44      4-X OÅM. TPAHC.       TP4
C     K = 5        45      5‚® OÅM. TPAHC.       TP5
C
C!!!!!
C    B H à M A H à E :   K  OèPEÑEãüETCü àCXOÑü àá HOMEPA TàèA:
C    ****************
C                          K  = TIP - 40
C
C!!!!
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
C
C
C
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOSA,
C
C   èPàMEóAHàE:  1.CTPìKTìPì MSW CãOBA MAKPOMOÑEãà ÑAHHOÉO TàèA CM. HàÜE
C   ----------   2. èPà ZAPROS ^=   1 àãà 2 - COOÅôEHàE à ìXOÑ;
C
C
C             "èOCTOüHHAü" óACTú MSW TàèA # 42, 43, 44, 45
C              -------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I èPàáHAKI KOãà I KOã. IAÑP.   IPE-I
C  I ùãEME I TàèA   I è/è    I TàèA   I óECT I BõB. IHAó.   IáE-I
C  I HTA   I MOÑEãà I PACóETAI MOÑEãà I BO   I áHAó I"TOKN" IPB I
C  I       I        I DM.....I       .I ìáãOBI EHàâ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW TàèOB # 42
C                  ---------------
C
C
C  èOCT.     I èEPEMEHHAü RMSW-->
C +----------+--------+--------+--------+------+------+----+-----+----+
C I   ìáãõ   I R èAPA-I L èAP- I W èAP- I      I      I    I     I    I
C I          I   MET- I   AME- I   AME- I Rsum I Lsum IG21 I G22 I J2 I
C I          I    Põ  I   TPõ  I   TPõ  I      I      I    I     I    I
C +----------+--------+--------+--------+------+------+----+-----+----+
C I          I  1,2   I  3,4   I  5,6   I  7   I  8   I 9  I 10  I 11 I
C +----------+--------+--------+--------+------+------+----+-----+----+
C I9,10,11,12I 13,14  I 15,16  I 17,18  I  19  I  20  I 21 I 22  I 23 I
C +----------+--------+--------+--------+------+------+----+-----+----+
C
C
C
C                  RMSW TàèOB # 43
C                  ---------------
C
C +----------------+--------+--------+--------+-------------+---------+
C I      ìáãõ      I R èAPA-I L èAP- I W èAP- I Ç•pÂ≠•‚p•„- I         I
C I                I   MET- I   AME- I   AME- I£Æ´Ï≠†Ô Á†·‚ÏI    J    I
C I                I    Põ  I   TPõ  I   TPõ  I      G      I         I
C +----------------+--------+--------+--------+-------------+---------+
C I                I 1,2,3  I 4,5,6  I 7,8,9  I   10-15     I  16-18  I
C +----------------+--------+--------+--------+-------------+---------+
C I9,10,11,12,13,14I15,16,17I18,19,20I21,22,23I   24-29     I  30-32  I
C +----------------+--------+--------+--------+-------------+---------+
C
C
C                  RMSW TàèOB # 44
C                  ---------------
C +---------+--------+--------+--------+-------------+---------+
C I  ìáãõ   I        I        I        I Ç•pÂ≠•‚p•„- I         I
C I         I R sum  I  L sum I    W   I£Æ´Ï≠†Ô Á†·‚ÏI    J    I
C I         I        I        I        I      G      I         I
C +---------+--------+--------+--------+-------------+---------+
C I         I 1 - 6  I 7 - 12 I 13-16  I   17-26     I  27-30  I
C +---------+--------+--------+--------+-------------+---------+
C I 9 - 16  I 17 -22 I 23 - 28I 29 - 32I   33-42     I  43-46  I
C +---------+--------+--------+--------+-------------+---------+
C
C
C                  RMSW TàèOB # 45
C                  ---------------
C +---------+--------+--------+--------+-------------+---------+
C I         I        I        I        I Ç•pÂ≠•‚p•„- I         I
C I  ìáãõ   I R sum  I  L sum I    W   I£Æ´Ï≠†Ô Á†·‚ÏI    J    I
C I         I        I        I        I      G      I         I
C +---------+--------+--------+--------+-------------+---------+
C I         I 1 - 10 I 11- 20 I 21-25  I   26-40     I  41-45  I
C +---------+--------+--------+--------+-------------+---------+
C I 9 - 18  I 19 -28 I 29- 38 I 39-43  I   44-58     I  59-63  I
C +---------+--------+--------+--------+-------------+---------+
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         DIMENSION STR(5), TTN(5), ULN(5), Z(25), Z1(25), G0(25)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         INTEGER ADRMSW
         INTEGER TIP,  ZAPROS
         REAL JALL, LSUM
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
C######################################################################
C  BCTPOEHHAü îìHKñàü  IDK(I,J) OèPEÑEãüET HOMEP ùãEMEHTA B BEKTOPE ,
C  B KOTOPOM XPAHàTCü KBAÑPATHAü MATPàñA (èO CTOãÅñAM) PAáMEPHOCTúû K.
C
       IDK(I,J) = K*(J-1) + I
C      **********************
C######################################################################
C--->  KOHTPOãú TàèA
       TIP = MSW(2)
       K = TIP - 40
       IF ( K .GE. 2  .AND. K .LE. 5  ) GO TO 9999
C----------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT(/' *** ãOÉ. CÅOâ B DM0041 - áAèPOC HA OÅCãìÜàBAHàE óìÜOÉ
     =O TàèA ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
9999     CONTINUE
C######################################################################
       K1 = K - 1
       K2 = K1 * (K1+1) / 2
       NRSUM = 9 + 2 * K
       NLSUM = NRSUM + K2
       NW    = NLSUM + K2
       NGALL = NW    + K
       NJALL = NGALL + K*(K+1)/2
C----------------------------------------------------------------------
C---> BõÅOP PEÜàMA:
         GO TO ( 667, 666                 ), ZAPROS
C===> àHAóE OòàÅKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHàTOP #', I4, ' áAèPOC OÅCãìÜàBAHàü OòàÅOóEH
     = ***'/ ' *** ZAPROS = ', I5, ' - àÉHOPàPìETCü ***')
         RETURN
667      CONTINUE
C--------------------ZAPROS=1------------------------------------------
C--->  BõóàCãEHàE G - J èAPAMETPOB à àX áAèàCú A MSW:
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  HAXOÑàM CTAPõE TOKà.
C----------------------------------------------------------------------
       ITOK = MSW(7)
          DO 10 I=1,K
          TTN(I) = TOKN(ITOK+I-1)
10     CONTINUE
C----------------------------------------------------------------------
C-->  HAXOÑàM CTAPõE HAèPüÜEHàü.
C----------------------------------------------------------------------
       DO 20 I=1,K
       IMSW = 8 + I
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       ULN(I) = VN - VK
20     CONTINUE
C----------------------------------------------------------------------
       IF( K .GT. 2 ) GO TO 3000
C----------------------------------------------------------------------
C    OÑHOîAáHõâ ÑBìXOÅMOTOóHõâ TPAHCîOPMATOP (Tàè # 42)
C----------------------------------------------------------------------
       RSUM = RMSW(19)
       LSUM = RMSW(20)
C--> AK - KOùîîàñàÖçí íêÄçëîéêåÄñàà
       AK   = RMSW(18) / RMSW(17)
       ZN   = LSUM + H1 * RSUM
       A    = H1 / ZN
C--> éèêÖÑÖãàå åÄíêàñì  G
       G0(1)= AK**2 * A
       G0(2)= -AK   * A
       G0(3)= -AK   * A
       G0(4)= A
       RMSW(21) = G0(2)
       RMSW(22) = G0(4)
C--> éèêÖÑÖãàå ÇÖäíéê  J
       A = ( H2*(ULN(2)-AK*ULN(1)) + TTN(2)*(LSUM-H2*RSUM) ) / ZN
       TTN(1) = -AK * A
       TTN(2) =       A
       RMSW(23) =     A
C----------------------------------------------------------------------
C-->  BKãAÑõBAEM
C----------------------------------------------------------------------
       KKON = 9 + K
C
C
       CALL DEPOSA( MSW(9), MSW(KKON), K, K, G0,
     =              TTN, YALL, JALL )
       KOD = 0
       RETURN
C----------------------------------------------------------------------
C    OÑHOîAáHõâ TPEX-èüíà OÅMOTOóHõâ TPAHCîOPMATOP (Tàè # 43,44,45)
C----------------------------------------------------------------------
3000   CONTINUE
C----------------------------------------------------------------------
C--> éØp•§•´®¨ ß≠†¨•≠†‚•´Ï   LSUM + H1 * RSUM
       DO 3010 I=1,K2
       I1 = I - 1
       Z(I) = RMSW(NLSUM+I1) + H1 * RMSW(NRSUM+I1)
3010   CONTINUE
C----------------------------------------------------------------------
C--> é°p†È†•¨ ¨†‚p®Ê„   (LSUM + H1 * RSUM)
       IF( K .EQ. 3 ) CALL MMTS2( Z , Z1 )
       IF( K .EQ. 4 ) CALL MMTS3( Z , Z1 )
       IF( K .EQ. 5 ) CALL MMTS4( Z , Z1 )
C----------------------------------------------------------------------
C--> îÆp¨®p„•¨ ¨†‚p®Ê„  G
       I1 = 1
       DO 3020 I=2,K
          DO 3020 J=I,K
          A = H1 * Z1(I1)
          RMSW(NGALL+K+I1-1) = A
          IND0 = IDK(I,J)
          IND1 = IDK(J,I)
          G0(IND0) = A
          G0(IND1) = A
          I1 = I1 + 1
3020   CONTINUE
       G0(1) = 0.0
          DO 3040 J=2,K
          S = 0.0
             DO 3030 I=2,K
             IND = IDK(I,J)
             S = S + G0(IND) * RMSW(NW+I-1)
3030      CONTINUE
          S = -S / RMSW(NW)
          I = 1
          IND0 = IDK(I,J)
          IND1 = IDK(J,I)
          G0(IND0) = S
          G0(IND1) = S
          RMSW(NGALL+J -1) = S
          G0(1) = G0(1) + S * RMSW(NW+J-1)
3040   CONTINUE
       G0(1) = -G0(1) / RMSW(NW)
       RMSW(NGALL) = G0(1)
C----------------------------------------------------------------------
C--> îÆp¨®p„•¨ ¢•™‚Æp  J
C
C--> éØp•§•´®¨   LSUM - H2* RSUM
       DO 3050 I=1,K2
       I1 = I - 1
       Z(I) = RMSW(NLSUM+I1) - H2 * RMSW(NRSUM+I1)
3050   CONTINUE
C
C--> ì¨≠Æ¶®¨  (LSUM - H2* RSUM) * TTN
       IF( K .EQ. 3 ) CALL PMTS2( Z , TTN(2) , STR(2) )
       IF( K .EQ. 4 ) CALL PMTS3( Z , TTN(2) , STR(2) )
       IF( K .EQ. 5 ) CALL PMTS4( Z , TTN(2) , STR(2) )
C
C--> èp®°†¢®¨  (LSUM - H2* RSUM) * TTN + H2 * [UN-U1/W1 * W]
       A = ULN(1) / RMSW(NW)
       DO 3060 I=2,K
       TTN(I) = STR(I) + H2 * ( ULN(I) - A * RMSW(NW+I-1) )
3060   CONTINUE
C--> ì¨≠Æ¶®¨  Z1 * [(LSUM - H2* RSUM) * TTN + H2 * (UN-U1/W1 * W) ]
       IF( K .EQ. 3 ) CALL PMTS2( Z1 , TTN(2) , RMSW(NJALL+1) )
       IF( K .EQ. 4 ) CALL PMTS3( Z1 , TTN(2) , RMSW(NJALL+1) )
       IF( K .EQ. 5 ) CALL PMTS4( Z1 , TTN(2) , RMSW(NJALL+1) )
C--> éØp•§•´®¨   J(1)
       TTN(1) = 0.0
       DO 3070 I=2,K
       TTN(1) = TTN(1) + RMSW(NJALL+I-1)*RMSW(NW+I-1)
3070   CONTINUE
       RMSW(NJALL) = -TTN(1)/RMSW(NW)
C----------------------------------------------------------------------
C-->  BKãAÑõBAEM
C----------------------------------------------------------------------
       KKON = 9 + K
C
C
       CALL DEPOSA( MSW(9), MSW(KKON), K, K, G0,
     =              RMSW(NJALL), YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
C######################################################################
666    CONTINUE
C-->  ZAPROS = 2 ------------------------------------------------------
C######################################################################
       ITOK = MSW(7)
C----------------------------------------------------------------------
C-->  HAXOÑàM HOBõE HAèPüÜEHàü.
C----------------------------------------------------------------------
       DO 100 I=1,K
       IMSW = 8 + I
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN1(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN1(KK)
       ULN(I) = VN - VK
100    CONTINUE
C----------------------------------------------------------------------
C-->  áAèàCú B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       IF( K .GT. 2 ) GO TO 30000
C--> AK - KOùîîàñàÖçí íêÄçëîéêåÄñàà
       AK   = RMSW(18) / RMSW(17)
       A = RMSW(21) * ULN(1) + RMSW(22) * ULN(2) + RMSW(23)
       TOKN1(ITOK  ) = -AK * A
       TOKN1(ITOK+1) =       A
       KOD = 0
       RETURN
C*********************************************************************
30000  CONTINUE
C----------------------------------------------------------------------
C-->  ìMHOÜàM  G * ULN1
C----------------------------------------------------------------------
       IF( K .EQ. 3 ) CALL PMTS3( RMSW(NGALL), ULN, TTN )
       IF( K .EQ. 4 ) CALL PMTS4( RMSW(NGALL), ULN, TTN )
       IF( K .EQ. 5 ) CALL PMTS5( RMSW(NGALL), ULN, TTN )
C----------------------------------------------------------------------
C-->  áAèàCú B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       DO 110 I=1,K
       TOKN1(ITOK - 1 + I) =  TTN(I) + RMSW( NJALL - 1 + I )
110    CONTINUE
C----------------------------------------------------------------------
       KOD = 0
       RETURN
C*********************************************************************
       END
