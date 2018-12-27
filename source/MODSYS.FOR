         SUBROUTINE MODSYS( YALL, JALL, LOGYTR )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉAMMA:    MODSYS( YALL, JALL, LOGYTR )
C   -----------
C   HAáHAóEHàE:   - OÅ'EÑàHEHàE CTPOK à CTOãÅñOB B YALL à JALL Ñãü
C   ----------         ìóETA B OCHOBHOâ CàCTEME àHñàÑEHTHõX ìáãOB;
C                 - èOÑÉOTOBKA MECTA Ñãü "TEHEBOâ" CàCTEMõ;
C
C   METOÑ:        - CãOÜEHàE CTPOK à CTOãÅñOB C "ÅAáOBõMà" ìáãAMà;
C   -----         - OóàCTKA CTOãÅñOB CàCTEMõ Ñãü HE "ÅAáOBõX" ìáãOB.
C
C   èAPAMETPõ:
C   ----------     YALL - MATPàñA èP0BOÑàMOCTà AHAãàáàPìEMOâ ñEèà;
C                  JALL - BEKTOP àCTOóHàKOB TOKA AHAãàáàPìEMOâ ñEèà;
C                  LOGYTR - BEKTOP èPàáHAKOB K.á. ìáãOB:
C                  àCèOãúáìETCü B MAKPOMOÑEãüX C èPàáHAKOM TàèA = 6;
C                  PAáMEPHOCà MACCàBOB ÉPìèèõ "ALL" OèPEÑEãüûTCü:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEKìôAü_PAáMEPHOCTú ---> NSYS = MASPRT(26)
C
C   èOÑèPOÉPAMMõ: 1. COMMON PROF1
C   ------------
C   èPàMEóAHàE:    1. OCHOBHAü CàCTEMA YALL à JALL CîOPMàPOBAHõ
C   -----------       ÅEá ìóETA OTKPõTõX BEHTàãEâ;
C                  2. LOGYTR - CîOPMàPOBAH à MOÑàîàñàPOBAH;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(1)
         COMMON /PROF3/ FUNTAB(1)
         DIMENSION  YALL(1), JALL(1), LOGYTR(1)
         INTEGER OPEN6
         REAL JALL
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(26), NSYS   )
         EQUIVALENCE ( MASPRT(23), NDALL )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
         EQUIVALENCE ( MASPRT(58), KNOT6M )
C===>    BCTPOEHHAü îìHKñàü OÅCãìÜàBAHàü ÑBìXMEPHõX MACCàBOB:
         ID(I,J) = NDALL * (J-1)  +  I
         DO 100  K = 1, NSYS
         M = LOGYTR(K)
         IF ( M. LE. -1 ) GO TO 100
C--->  ECãà LOGYTR(K) = 0, TO "ÅAáOBõâ" ìáEã ùTOâ ÉPìèèõ àHñàÑEHTHõX
C      ìáãOB üBãüETCü "HìãEBõM" ìáãOM, èO ùTOMì HEOÅXOÑàMO TOãúKO
C      OóàCTàTú K-â CTOãÅEñ;
         IF ( M .EQ. 0 ) GO TO 200
C--->  KOHTPOãú  LOGYTR(M) ÑOãÜHA ÅõTú PABHA "-1", àHAóE OòàÅKA
C!!!-------------------------------------------------------------------
         IF ( LOGYTR(M) .EQ. -1 ) GO TO 1
         WRITE(NWTRE,4)
4        FORMAT(' *** ãOÉàóECKàâ CÅOâ B MODSYS ---> HAPìòEHàE CTPìKTìPõ
     = LOGYTR ***')
          WRITE(NWTRE, 2 ) ( I, LOGYTR(I), I=1,NSYS )
2        FORMAT( 5(1X,'LOGYTR(', I3, ')=', I4 ) )
         WRITE(NWTRE,  3 ) K  M,  OPEN6, KNOT6M
3      FORMAT(' K=', I4, ' M=', I4,' OPEN6 = ', I5, '  KNOT6M = ', I5 )
         CALL SUBERR(25)
1        CONTINUE
C!!!-------------------------------------------------------------------
C  K-ü CTPOKA - ùTO CTPOKA "TEHEBOâ" CàCTEMõ
C  M-ü CTPOKA - ùTO CTPOKA "OCHOBHOâ" CàCTEMõ
C----> ÑEãAEM:
C        YALL(M,*) = YALL(M,*) + YALL(K,*)
C        YALL(*,M) = YALL(*,M) + YALL(*,K)
C        JALL(M)   = JALL(M)   + JALL(K)
C        YALL(*,K) = 0.0
C---->
         DO 99 J = 1, NSYS
         IMJ = ID( M, J )
         IKJ = ID( K, J )
99       YALL( IMJ ) = YALL( IMJ ) + YALL( IKJ )
C********
         DO 98 J = 1, NSYS
         IJM = ID( J, M )
         IJK = ID( J, K )
98       YALL( IJM ) = YALL( IJM ) + YALL( IJK )
C--->    àCTOóHàKà....
         JALL( M ) = JALL( M ) + JALL( K )
200      CONTINUE
C--->  OóàCKA K-X CTOãÅñOB===> MECTO èOÑ "TEHEBìû" CàCTEMì
         DO 97  J = 1, NSYS
         IJK = ID( J, K )
97       YALL(IJK) = 0.0
100      CONTINUE
         RETURN
         END
