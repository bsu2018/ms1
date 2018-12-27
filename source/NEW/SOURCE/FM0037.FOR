         SUBROUTINE FM0037( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT CEHTüÅPü 1988 É.)------------
C   èOÑèPOÉPAMMA:   FM0037( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HAáHAóEHàE:    HACTPOâKA MAKPOMOÑEãà àÑÖÄãúçõï TPAHCîOPMATOPOB
C  ------------    ***********************************************
C     KOã-BO OÅ-K  Tàè     àMü MOÑEãà     OÅOáHAóEHàü B ÑAãúHEâòEM
C
C     K = 2        42      2-X OÅM. TPAHC.       TP2
C     K = 3        43      3-X OÅM. TPAHC.       TP3
C     K = 4        44      4-X OÅM. TPAHC.       TP4
C     K = 5        45      5‚® OÅM. TPAHC.       TP5
C
C
C                  BBOÑ èO OèàCAHàû:
C
C        ù # T 42 ì @ @ ..  è R1  R2  L1  L2  W1  W2
C
C
C        ù # T 43 ì @ @ ..  è R1  R2  R3   L1  L2  L3   W1 W2 W3
C
C        ù # T 44 ì @ @ ..  è R1  R2  R3  R4   L1  L2  L3  L4
C                             W1  W2  W3  W4
C
C        ù # T 45 ì @ @ ..  è R1  R2  R3  R4  R5  L1  L2  L3  L4  L5
C                             W1  W2  W3  W4  W5
C
C          ìáãõ èOÑKãûóEHàü: <HAóAãO OÅMOTOK> <KOHñõ OÅMOTOK>
C
C                  èPOàáBOÑàT îOPMàPOBAHàE "èEPEMEHHOâ" óACTà MSW
C                  MAKPOMOÑEãà.
C
C   METOÑ:         PAáMETKA èOãEâ RMSW èOÑ MOÑEãà;
C   -----
C
C   èAPAMETPõ:     BA0T, KNOTT, PAR0T - "OTPEáKà" àHîOPMAñàOHHõX
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 à GETMSW );
C                  RMSW - "èEPEMEHHAü" óACTú MSW ( "OTPEáOK" MSWALL );
C                  LM - ÑãàHA "èEPEMEHHOâ" óACTà RMSW;
C
C   èOÑèPOÉPAMMõ:  1.COMMON PROF1
C   ------------
C
C   èPàMEóAHàE:    1. èPOàáBOÑàTCü äéçíêéãú èÄêÄåÖíêéÇ çÄ èéãéÜàíÖãúçéëíú;
C   -----------    2. BCE HE OèPEÑEãüEMõE èOãü RMSW áAHìãüûTCü;
C                  3. HàÜE èPàBOÑüTCü îOPMATõ RMSW:
C
C
C                  RMSW TàèOB # 42
C                  ---------------
C
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
C                  RMSW TàèOB # 43
C                  ---------------
C
C +----------------+--------+--------+--------+-------------+---------+
C I      ìáãõ      I        I        I        I Ç•pÂ≠•‚p•„- I         I
C I                I R sum  I  L sum I    W   I£Æ´Ï≠†Ô Á†·‚ÏI    J    I
C I                I        I        I        I      G      I         I
C +----------------+--------+--------+--------+-------------+---------+
C I                I 1,2,3  I 4,5,6  I 7,8,9  I   10-15     I  16-18  I
C +----------------+--------+--------+--------+-------------+---------+
C I9,10,11,12,13,14I15,16,17I18,19,20I21,22,23I   24-29     I  30-32  I
C +----------------+--------+--------+--------+-------------+---------+
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
       COMMON /PROF1/ MASPRT(100)
       INTEGER BA0T, TIP
       REAL R(5), L(5), W(5)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE ( RN, N )
C-->  HAò ãà Tàè ?
       NEL = BA0T(1)
       TIP = BA0T(2)
       NWTR = MASPRT(13)
       IF( TIP .EQ. 42  .OR.  TIP .EQ. 43 ) GO TO 1
       IF( TIP .EQ. 44  .OR.  TIP .EQ. 45 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ãOÉ. CÅOâ B FM0037: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C----------------------------------------------------------------------
C--> HAóHEM
C----------------------------------------------------------------------
1        CONTINUE
C-->  K MOÑEãà
       K = TIP - 40
C-->  ÑãàHA èEPEMEHHOâ óACTà
       IF( K .EQ. 2 ) LM = 11
       IF( K .EQ. 3 ) LM = 18
       IF( K .EQ. 4 ) LM = 30
       IF( K .EQ. 5 ) LM = 45
       DO 100 I=1, LM
100    RMSW(I) = 0.0
C----------------------------------------------------------------------
C--> èPàEM èAPAMETPOB...
C  àX KOã-BO...
       IF( K .EQ. 2 ) KPAR = 6
       IF( K .EQ. 3 ) KPAR = 9
       IF( K .EQ. 4 ) KPAR = 12
       IF( K .EQ. 5 ) KPAR = 15
C----------------------------------------------------------------------
C--> áÄèéåçàå èÄêÄåÖíêõ
C*** KOHTPOãú HA èOãOÜàTEãúHOCTú 
C----------------------------------------------------------------------
C       DO 101  I=1, KPAR
C       IF( PAR0T(I) .LE. 0.0 ) GO TO 9999
C101    CONTINUE
       DO 102  I=1,K
       R(I) = PAR0T(I)
       L(I) = PAR0T(I+K)
       W(I) = PAR0T(I+2*K)
102    CONTINUE
C----------------------------------------------------------------------
C--> ÑAãúòE MOÑEãà OTãàóAûTCü
C----------------------------------------------------------------------
       IF( K .GE. 3 ) GO TO 3000
C----------------------------------------------------------------------
C    OÑHOîAáHõâ ÑBìXOÅMOTOóHõâ TPAHCîOPMATOP (Tàè # 42)
C----------------------------------------------------------------------
C--> êÄëóàíÄÖå  Rsum  ®  Lsum
       W21     = ( W(2)/W(1) )**2
       RMSW(1) = R(1)
       RMSW(2) = R(2)
       RMSW(3) = L(1)
       RMSW(4) = L(2)
       RMSW(5) = W(1)
       RMSW(6) = W(2)
       RMSW(7) = R(1) * W21 + R(2)
       RMSW(8) = L(1) * W21 + L(2)
       RETURN
C----------------------------------------------------------------------
C    OÑHOîAáHõâ TPEX- èüíà OÅMOTOóHõâ TPAHCîOPMATOP (Tàè # 43,44,45)
C----------------------------------------------------------------------
3000   CONTINUE
       K1 = K - 1
       K2 = K1 * (K1+1) / 2
       NRSUM   = 1
       NLSUM   = NRSUM + K2
       NW      = NLSUM + K2
       NOM     = 0
          DO 3010 I=2,K
          DO 3010 J=I,K
          RMSW(NRSUM+NOM) = R(1) * W(I)*W(J) / W(1)**2
          RMSW(NLSUM+NOM) = L(1) * W(I)*W(J) / W(1)**2
          IF( I .EQ. J )RMSW(NRSUM+NOM) = RMSW(NRSUM+NOM) + R(I)
          IF( I .EQ. J )RMSW(NLSUM+NOM) = RMSW(NLSUM+NOM) + L(I)
          NOM = NOM + 1
3010   CONTINUE
          DO 3020 I=1,K
          RMSW(NW+I-1) = W(I)
3020   CONTINUE
       RETURN
C**********************************************************************
9999   CONTINUE
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 501) TIP, NEL
501      FORMAT(' *** ãOÉ. CÅOâ B FM0037: ÑAHHõE HE èOãOÜàTEãúHõ  ***',
     =         /' *** HOMEP TàèA ',I4,5X,' ùãEMEHT HOMEP:',I4,5X,'***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
       RETURN
       END
