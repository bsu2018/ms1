      SUBROUTINE PLOTXY (X,Y,NXY,KSX,KSY,ISX,ISY,NFOUT,RAB,IER)
CXXX ---KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C     O„POƒPAMMA:  PLOTXY (X,Y,NXY,KSX,KSY,ISX,ISY,NFOUT,RAB,IER)
C     ------------
C
C     HA‡HA—EHˆE:   B›—EP—ˆBAHˆE ƒPA”ˆKA APAMETPˆ—ECKO‰ ‡ABˆCˆMOCTˆ:
C     -----------       X=F(T), Y=”(T) => Y=FI(X), ‡A„AHHO‰ TA‹ˆ—HO;
C
C     METO„:        OTOPA†EHˆE R(X,Y) HA IR(JX,JY),”OPMˆPOBAHˆE
C     ------         MHO†ECTBA IR,EƒO COPTˆPOBKA ˆ Bˆ‡“A‹ˆ‡A–ˆŸ.
C                $$$ Pˆ O‹“—EHˆˆ IR(JX,JY) POˆ‡BO„ˆTCŸ ‹ˆHE‰HAŸ
C                    ˆHTEPO‹Ÿ–ˆŸ Y=FI(X<˜AƒA>); (HE OKA‡ATE‹œHO)$$$
C
C     APAMETP›:    X - MACCˆB ‡HA—EHˆ‰ APƒ“MEHTA; (PA‡MEPHOCTœ NXY);
C     ----------    Y - MACCˆB ‡HA—EHˆ‰ ”“HK–ˆˆ; (PA‡MEPHOCTœ NXY);
C                   NXY - KO‹ˆ—ECTBO TO—EK OPE„E‹EHˆŸ Y=FI(X),
C                          PA‡MEPHOCTœ MACCˆBOB X ˆ Y; (NXY>1);
C                   KSX - KO‹ˆ—ECTBO TO—EK O X (KO‹-BO CTPOK ƒPA”ˆKA).
C                          (KSX<=NXY; Pˆ KSX<=0 ˆ KSX>NXY --> KSX=NXY);
C                   KSY - KO‹ˆ—ECTBO CˆMBO‹OB B O„HO‰ CTPOKE Y:
C                          B 1-X 12-Tˆ O–ˆ”POBKA, KSY>=42,
C                          Pˆ  KSY<=15 ˆ‹ˆ KSY>129 --> KSY=42 ˆ‹ˆ 129;
C                   ISX - ˆHTEPBA‹ E—ATˆ PA‡„E‹ˆTE‹œH›X OCE‰ O X,
C                          Pˆ ISX > KSX E—ATœ ‹OKˆP“ETCŸ; =0 --> =6;
C                   IXY - ˆHTEPBA‹ E—ATˆ PA‡„E‹ˆTE‹œH›X OCE‰ O Y,
C                          Pˆ ISY > KSY E—ATœ ‹OKˆP“ETCŸ; =0 --> =10;
C                   NFOUT - HOMEP ”A‰‹A B›BO„A; Pˆ <=0 --> =6;
C                   RAB - PAO—ˆ‰ MACCˆB PA‡MEPHOCTˆ <2*NXY>;
C                   IER - ˆH„ˆKATOP O˜ˆKˆ: IER= 0 - O˜ˆKˆ HET;
C                                           IER= 1 - O˜ˆKA ‡APOCA;
C     O„POƒPAMM›: HET;
C     -------------
C
C     PˆME—AHˆE:   O˜ˆKA OKP“ƒ‹EHˆŸ O OCŸM = ˜Aƒ“ „ˆCKPETˆ‡A–ˆˆ IR;
C     -----------
C
CZZZ ------------------------------------------------------------------
      DIMENSION X(1), Y(1), RAB(1)
      DIMENSION BLANK3(10), OUT(117), PPX(10), OUTB(117)
      INTEGER RAB, U, T
      DATA BLANK1,BLANK2,BLANK/'-',':',' '/,CIM/'*'/,ZERO/'.'/
      DATA BLANK3/'0','1','2','3','4','5','6','7','8','9'/
      DATA PPX/'1','2','3','4','5','6','7','8','9','†'/
      KSYP=KSY
      KSXP=KSX
      ISXP=ISX
      ISYP=ISY
      NFOUTP=NFOUT
      IER=0
      IF(NXY.LE.1) IER=1
      IF(KSX.LE.0) KSXP=NXY
      IF(KSX.GT.NXY) KSXP=NXY
      IF(KSX.EQ.1) KSXP=2
      IF(KSY.LE.0) KSYP=123
      IF(KSY.LT.15) KSYP=42
      IF(KSY.GT.129) KSYP=129
      IF(ISX.LE.0) ISXP=6
      IF(ISY.LE.0) ISYP=10
      IF(NFOUT.LE.0) NFOUT=6
      IF(IER.EQ.1) GO TO 100
C     PA‡MEP B CˆMBO‹AX O Y:
      KSY1=KSYP-12
      KSY2=KSY1-1
C     HA‰„EM MIN ˆ MAX -OKHO ƒPA”ˆKA
      XMIN=X(1)
      XMAX=XMIN
      YMIN=Y(1)
      YMAX=YMIN
      DO 1 I=2,NXY
      IF(X(I) .LT. XMIN) XMIN=X(I)
      IF(X(I) .GT. XMAX) XMAX=X(I)
      IF(Y(I) .LT. YMIN) YMIN=Y(I)
      IF(Y(I) .GT. YMAX) YMAX=Y(I)
    1 CONTINUE
C     ˜Aƒˆ B›OPA
      XSCAL=(XMAX-XMIN)/(FLOAT(KSXP-1))
      YSCAL=(YMAX-YMIN)/(FLOAT(KSY1-1))
      IF(XSCAL .EQ. 0.0 .OR. YSCAL .EQ. 0.0) IER=1
      IF(IER .EQ. 1) GO TO 100
C1*********************************************************************
C   EC‹ˆ BO‡MO†HOCTœ OTC‹E„ˆTœ H“‹ˆ?   EC‹ˆ „A, TO K[X/Y]P0 = 1
       KXP0 = 0
       KYP0 = 0
       JXP0 = ( 0.0 - XMIN ) / XSCAL + 1
       JYP0 = ( 0.0 - YMIN ) / YSCAL + 1
       IF ( JXP0 .GE. 1   .AND.  JXP0 .LE. KSXP ) KXP0 = 1
       IF ( JYP0 .GE. 1   .AND.  JYP0 .LE. KSY1 ) KYP0 = 1
C1*********************************************************************
C     O‹“—EHˆE MACCˆBA OTOPA†EHˆŸ:
      DO 2 I=1,NXY
C     KOOP„ˆHATA O X:
      JX=(X(I)-XMIN)/XSCAL+1
C     ‹ˆHE‰HAŸ ˆHTEPO‹Ÿ–ˆŸ, XG-APƒ“MEHT O X, Y-O Y:
      XG=XMIN+XSCAL*(JX-1)
C     ƒP“O:
      YG=Y(I)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$            DX = XG - X(I)
C$    IM= I +  IFIX( SIGN(1.0,DX) )
C     ECTœ ˆHTEPBA‹ „‹Ÿ ˆHTEPO‹Ÿ–ˆˆ?
C$    IF(IM .LE. 0  .OR.  IM .GT .NXY) GO TO 3
C     ‹ˆHE‰HAŸ ˆHTEPO‹Ÿ–ˆŸ
C$    YG=((Y(I)-Y(IM))*XG+Y(IM)*X(I)-Y(I)*X(IM))/
C$   *        (X(I)-X(IM))
C$       IF ( YG .LT. YMIN  .OR.  YG .GT. YMAX ) YG=Y(I)
C$  3 CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C     KOOP„ˆHATA O Y:
      JY=(YG-YMIN)/YSCAL+1
C     ‡AOMHˆM
      RAB(2*I-1) = JX
    2 RAB(2*I) = JY
C     COPTˆP“EM O JX
      NXY1=NXY-1
      DO 4 ISORT=1,NXY1
      KSORT=ISORT
5     KS2=2*KSORT
      KS2M=KS2-1
      JSORT=KSORT+1
      JS2=2*JSORT
      JS2M=JS2-1
      IF(RAB(KS2M) .LE. RAB(JS2M)) GO TO 4
C     OMEH
      U=RAB(KS2M)
      RAB(KS2M)=RAB(JS2M)
      RAB(JS2M)=U
      T=RAB(KS2)
      RAB(KS2)=RAB(JS2)
      RAB(JS2)=T
      KSORT=KSORT - 1
      IF(KSORT .GE. 1) GO TO 5
    4 CONTINUE
C     POˆ‡BO„ˆM B›BO„
      WRITE(NFOUTP,600) XMIN,XSCAL,XMAX,YMIN,YSCAL,YMAX
  600 FORMAT(1X,'X MIN',6X,'˜Aƒ X',6X,'X MAX'/
     *       1X,3(G10.4,1X)/
     *       1X,'Y MIN',6X,'˜Aƒ Y',6X,'Y MAX'/
     *       1X,3(G10.4,1X))
C ISTR - Pˆ‡HAK HE B›BE„EHHO‰ CTPOKˆ, KSS- C.—.K. PA‡„E‹ˆTE‹E‰
C  JXXB - C.—.K. OC‹E„OBATE‹œH›X CTPOK, OUTB - "“CTAŸ" CTPOKA
        JXXB = 1
      ISTR=0
      KSS=1
C     O–ˆ”POBKA HA—A‹A, ISTR=0 CTPOKˆ HA B›BO„ HET.
      DO 6 IX=1,KSY2,10
      DO 6 IZ=1,10
    6 OUT(IZ+IX-1)=BLANK3(IZ)
      WRITE(NFOUTP,601) (OUT(IZ),IZ=1,KSY1)
  601 FORMAT(12X,117A1)
C     B›BO„ ƒPA”ˆKA
      DO 7 NT=1, NXY
      IF(NT .NE. 1) GO TO 8
C     O„ƒOTOBˆTœ HOB“ž CTPOK“
    9 DO 12 IX=1,KSY1
   12 OUT(IX)=BLANK
      IF(KSS .EQ. 1  .OR.  KSS .EQ. (ISXP+1)) GO TO 13
C     CETKA O ƒOPˆ‡OHTA‹ˆ HE H“†HA
      GO TO 14
   13 KSS=1
      DO 15 IX=1,KSY1
   15 OUT(IX)=BLANK1
C     CETKA O BEPTˆKA‹ˆ
   14 DO 16 IX=1,KSY1,ISYP
   16 OUT(IX)=BLANK2
C2*********************************************************************
C  OCTABˆTœ ZERO O BEPTˆKA‹ˆ EC‹ˆ BO‡MO†HO
       IF ( KYP0 .EQ. 1 ) OUT(JYP0) = ZERO
C2*********************************************************************
C     CTPOKA O„ƒOTOB‹EHA, JXX-”ˆKCˆP. O‡ˆ–ˆŸ O X
      JXX=RAB(2*NT-1)
C3*********************************************************************
C   POBECTˆ ZERO O ƒOPˆ‡OHTA‹ˆ EC‹ˆ BO‡MO†HO
       IF ( KXP0 .EQ. 0  .OR. JXX .NE. JXP0 ) GO TO 19
C   ‡AHOCˆM ZERO
       DO 20 IX=1, KSY1
20     OUT(IX) = ZERO
       DO 191 IX=1,KSY1,ISYP
191    OUT(IX)=BLANK2
19     CONTINUE
C3*********************************************************************
       JRAB2= RAB( 2*NT )
      OUT( JRAB2   )=CIM
      ISTR=1
      GO TO 7
C     HE EPBAŸ TO—KA
    8 CONTINUE
      IF(JXX .EQ. RAB(2*NT-1)) GO TO 11
C     B›BECTˆ CTPOK“
   10 XPP=XMIN+(JXX-1)*XSCAL
      WRITE(NFOUTP,602) XPP,(OUT(IZ),IZ=1,KSY1)
  602 FORMAT(1X,G10.4,1X,117A1)
      ISTR=0
      KSS=KSS+1
      JXXB = JXXB + 1
C  H“†HO ‹ˆ B›BECTˆ "“CT›˜K“" ? JXNEXT - HOMEP C‹E„. CTPOKˆ
         JXNEXT = RAB(2*NT-1)
C  KSTRN - KO‹-BO "“CT›˜EK"
         KSTRN = JXNEXT - JXXB
         IF ( KSTRN .LE. 0 ) GO TO 9
C@@@
C  B›BECTˆ "“CT›˜Kˆ"
         DO 21 KSTR = 1, KSTRN
C  ”OPMˆP“EM OUTB
         DO 22 IX = 1, KSY1
22       OUTB(IX) = BLANK
         IF ( KSS .EQ. 1  .OR.  KSS .EQ. (ISXP+1) ) GO TO 23
C   CETKA O ƒOPˆ‡. HE H“†HA
         GO TO 24
23       KSS = 1
         DO 25 IX = 1,KSY1
25       OUTB(IX) = BLANK1
C   CETKA O BEPTˆKA‹ˆ
24       DO 26 IX = 1, KSY1, ISYP
26       OUTB(IX) = BLANK2
C  ZERO O BEPTˆKA‹ˆ
         IF ( KYP0 .EQ. 1 ) OUTB(JYP0) = ZERO
C  ZERO O ƒOPˆ‡OHT“
         IF ( KXP0 .EQ. 0  .OR.  JXXB .NE. JXP0 ) GO TO 29
         DO 30 IX = 1, KSY1
30       OUTB(IX) = ZERO
         DO 31 IX=1,KSY1, ISYP
31       OUTB(IX) = BLANK2
29       CONTINUE
C  B›BO„ "“CT›˜Kˆ"
         XPP = XMIN + (JXXB-1)*XSCAL
         WRITE(NFOUTP,602) XPP, ( OUTB(IZ), IZ=1,KSY1)
         KSS = KSS + 1
         JXXB = JXXB + 1
21       CONTINUE
C@@@@@@
      GO TO 9
C     B CTAPO‰ O‡ˆ–ˆˆ TO—KA ECTœ,„OABˆTœ C‹E„“ž™“ž
   11 JP=RAB(2*NT)
C     OTC‹E„ˆTœ EPECE—EHˆŸ, MO†HO ‡ATˆPATœ?
      IF(OUT(JP) .EQ. BLANK  .OR.  OUT(JP) .EQ. ZERO  .OR.
     *   OUT(JP) .EQ. BLANK1  .OR.
     *   OUT(JP) .EQ. BLANK2) GO TO 17
C     CTˆPATœ HE‹œ‡Ÿ,“CTAHOBˆTœ PˆOPˆTET EPECE—EHˆŸ
      J11=0
      DO 18 J1=1,9
      IF(OUT(JP) .EQ. PPX(J1)) J11=J1
   18 CONTINUE
      OUT(JP)=PPX(J11+1)
      ISTR=1
      GO TO 7
   17 OUT(JP)=CIM
       ISTR = 1
      GO TO 7
C-----
    7 CONTINUE
C     —TO-TO OCTA‹OCœ?
      XPP=XMIN+(JXX-1)*XSCAL
      IF(ISTR .EQ. 1) WRITE(NFOUTP,602) XPP,
     *                               (OUT(IZ),IZ=1,KSY1)
C     ‡A„HŸŸ O–ˆ”POBKA
      DO 190 IX=1,KSY2,10
      DO 190 IZ=1,10
190   OUT(IZ+IX-1)=BLANK3(IZ)
      WRITE(NFOUTP,601) (OUT(IZ),IZ=1,KSY1)
C     MAC˜TA ˆ PˆOPˆTET›
      WRITE(NFOUTP,600) XMIN,XSCAL,XMAX,YMIN,YSCAL,YMAX
      WRITE(NFOUTP,603) (PPX(IZ),IZ=1,10)
  603 FORMAT(1X,'PˆOPˆTET EPECE—EHˆ‰: ',
     *         10(1A1,1X)/)
  100 RETURN
      END
