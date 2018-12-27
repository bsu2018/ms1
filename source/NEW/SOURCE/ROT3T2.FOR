       SUBROUTINE ROT3T2(X1, X2, X3, BX1, BX2, BX3, BQ,
     *                   XQ, XD, XMOD, BXMOD, GQXMOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: ROT3T2(X1, X2, X3, BX1, BX2, BX3, BQ,
C   ------------         XQ, XD, XMOD, BXMOD, GQXMOD     )
C
C   HA‡HA—EHˆE:   EPOPA‡OBAHˆE "TPO‰Kˆ" BE‹ˆ—ˆH
C   ----------    X1, X2, X3, ‡A„AHH›X “ƒ‹AMˆ
C                 BX1, BX2, BX3 OTHOCˆTE‹œHO OCˆ OTC—ETA "A"
C                 B "„BO‰K“" BE‹ˆ—ˆH "XQ"  ˆ  "XD", PACO‹O†EHH›X
C                 O OCŸM HOBO‰ CˆCTEM› OTC—ETA "Q"-"D" -->
C                 OCœ "D" OEPE†AET OCœ "Q" ( POTˆB —AC. CTPE‹Kˆ ), A
C                 OCœ "Q" COCTAB‹ŸET “ƒO‹ "BQ" C OOPHO‰ OCœž "A";
C                   ( XMOD - MO„“‹œ, A "BXMOD" “ƒO‹ OTHOC. OCˆ "A" );
C                 Pˆ TOM TAK †E OPE„E‹ŸžTCŸ:
C                - XMOD - MO„“‹œ BEKTOPA, C“MAPHOƒO K X1, X2, X3;
C                - BXMOD - “ƒO‹ ME†„“ OOPHO‰ OCœž "A" ˆ BEKTOPOM "XMOD"
C                - GQXMOD - “ƒO‹ ME†„“ OCœž "Q" ˆ BEKTOPOM "XMOD";
C   METO„:    "PŸMOE" BEKTPOHOE C“MMˆPOBAHˆE B POEK–ˆŸX ˆ PA‡‹O†EHˆE;
C   -----
C   APAMETP›:  CM. HA‡HA—EHˆE
C   ---------
C   O„POƒPAMM›: HET
C   ------------
C   PˆME—AHˆE:  1. BCE “ƒ‹› B PA„ˆAHAX
C   ----------   2. “ƒO‹ BCEƒ„A ‹E†ˆT B PE„E‹AX OT 0 „O 360 ƒPA„;
C
CZZZ-------------------------------------------------------------------
        DATA PI2 / 6.283185306 /
       D1 =      BX1  -  BQ
       D2 =      BX2  -  BQ
       D3 =      BX3  -  BQ
C--> "Q"  POEK–ˆŸ-->
       XQ = X1 * COS(D1) + X2 * COS(D2) + X3 * COS(D3)
C--> "D"  POEK–ˆŸ -->
       XD = X1 * SIN(D1) + X2 * SIN(D2) + X3 * SIN(D3)
C-->  MO„“‹œ-->
       XMOD = SQRT( XD**2 + XQ**2 )
C-->  “ƒO‹ B CˆCTEME "Q" - "D"
       GQXMOD =  ATAN2( XD, XQ )
       IF ( XD .LT. 0.0 ) GQXMOD = GQXMOD + PI2
       IF ( GQXMOD .GE. PI2 ) GQXMOD = GQXMOD - INT(GQXMOD/PI2)*PI2
C      -------------------------------------------------------------
C-->  “ƒO‹ OTHOC. OCˆ OT—ETA "A"
       BXMOD = GQXMOD + BQ
       IF (  BXMOD .GE. PI2 )  BXMOD =  BXMOD - INT( BXMOD/PI2)*PI2
C      -------------------------------------------------------------
       RETURN
       END
