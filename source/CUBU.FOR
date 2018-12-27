       SUBROUTINE CUBU(A,B,C,X)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT MAPTA    1989 ƒ.)------------
C   O„POƒPAMMA:  CUBU(A,B,C,X)
C  -------------
C
C   HA‡HA—EHˆE:    O„POƒPAMMA PE˜EHˆŸ K“ˆ—HOƒO “PABHEHˆŸ
C   -----------    METO„OM : TPˆƒOHOMETPˆ—ECKOE PE˜EHˆE,KOTOPOE
C                  O„POBHO OˆCAHHO B CPABO—HˆKE O MATEMATˆKE
C                  ABTOPAMˆ ƒ.KOPH ˆ T.KOPH .CTPAHˆ–A  44
C                  APAƒPA” 1.8-4
C                     ----------
C                  ‡„ECœ „AET TO‹œKO „E‰CTBˆTE‹œH›E KOPHˆ
C                  MHˆM›E KOPHˆ PˆPABHŸžT H“‹œž...
C                  OKA POƒPAMMA C—ˆTAETCŸ HE B „BO‰HO‰
C                  TO—HOCTˆ.....
C                  *********************
C     “PABHEHˆE ˆMEET Bˆ„    X**3 + A*X**2 + B*X + C = 0
C
C     BXO„H›E ‡HA—EHˆŸ  A,B,C
C   A B›XO„H›E PE˜EHˆŸ : MACCˆB X(3)
C
C **********************************************************************
       DIMENSION X(3)
C --> PˆBO„ˆTCŸ K Bˆ„“ : Y**3 + P*Y + Q = 0
       P =-(A**2/3.)+ B
       Q = 2.*(A/3.)**3 - A*B/3. + C
       PQ = (P/3.)**3 + (Q/2.)**2
       PI = 3.1415926
       X(1) = 0.
       X(2) = 0.
       X(3) = 0.
C --> PE˜AEM ...PACCMOTPˆM P ˆ Q
       IF(P.NE.0.0) GO TO 30
C --> ‡HA—ˆT P = 0    A    Q ?
       IF (Q.NE .0.0) GOTO 13
C ‡„ECœ P=Q=0
       X(1) = -A/3.
       RETURN
 13    CONTINUE
C --> A T“T P=0 HO Q HE PABHO 0
       TE = ABS(-Q)
       X(1)= (TE)**(1./3.)
       IF ( Q.GT.0.0) X(1)=-X(1)
       X(1) = X(1) -A/3.
       RETURN
 30    CONTINUE
C--> ‡HA—ˆT P HE PABHO 0 OCMOTPˆM Q ?
       IF (Q.NE.0.) GOTO 35
C--> ‡„ECœ Q=0 A P HE PABHO 0
C --> “PABHEHˆE ˆMEET Bˆ„ X**3 +X*P =0  O„HO TPˆBˆA‹œHOE PE˜EHˆE X=0
       X(1)= -A/3.
       IF(P.GT.0.) RETURN
C --> ‡HA—ˆT P<0  X = +(-)SQRT(-P) -A/3.
       X(2) = SQRT(-P) - A/3.
       X(3) = - SQRT(-P) - A/3.
       RETURN
 35    CONTINUE
C --> ˆ P ˆ Q HE PABHO 0 ....
       IF(PQ.GE.0.0) GO TO 10
C--> ‡„ECœ PQ<0 ‡HA—ˆT P<0 ˆMEETCŸ 3 „EˆCTBˆTE‹œH›X PE˜EHˆŸ ...
       COALFA = -Q/(2.*SQRT(-(P/3.)**3))
       ALFA = ACOS(COALFA)
       X(1)= 2.*SQRT(-P/3.)*COS(ALFA/3.)  -A/3.
       X(2)= -2.*SQRT(-P/3.)*COS(ALFA/3.+PI/3.) -A/3.
       X(3)= -2.*SQRT(-P/3.)*COS(ALFA/3.-PI/3.) -A/3.
       RETURN
10     CONTINUE
C--> PQ>0 PACCMOTPˆM P ?...O‹“—AETCŸ O„HO „E‰CTBˆTE‹œHOE PE˜EHˆE
       IF (P.LT.0.0)  GOTO 20
C --> ‡„ECœ P>0
       TBETA = 2.*(P/3.)**(1.5)/Q
       BETA = ATAN(TBETA)
       TE = TAN(BETA/2)
       TALFA = ABS(TE)**(1./3.)
       IF (TE.LT.0.) TALFA =-TALFA
       ALFA  = ATAN(TALFA)
       X(1) = -2.*SQRT(P/3.)/TAN(ALFA*2.)-A/3.
       RETURN
 20    CONTINUE
C--> ‡„ECœ P<0 ... OC‹E„Hˆ‰ C‹“—A‰
       SBETA = 2.*(-P/3.)**(1.5)/Q
       BETA = ASIN(SBETA)
       TE = TAN(BETA/2.)
       TALFA = ABS(TE)**(1./3.)
       IF ( TE.LT.0.) TALFA = -TALFA
       ALFA  = ATAN (TALFA)
       X(1) = -2.*SQRT(-P/3.)/SIN(ALFA*2.) - A/3.
       RETURN
       END
