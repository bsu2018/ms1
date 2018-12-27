       REAL FUNCTION KMAS01( X, TMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA-”“HK–ˆŸ  KMAS01( X, TMAS )
C   --------------------
C   HA‡HA—EHˆE:        B›—ˆC‹EHˆE ‡HA—EHˆŸ F(X), ‡A„AHHO‰ TA‹ˆ—HO
C   ----------         KO”ˆ–ˆEHTAMˆ O‹ˆHOMA:
C
C                      F(X) = A0 + A1* X + A2* X**2 + A3* X**3 + ...
C
C                      „O“CKAETCŸ ‡A„AHˆE ‹žOƒO KO‹ˆ—ECTBA
C                       KO”. O‹ˆHOMA;
C
C                      *** KO„ˆPOBKA B OˆCAHˆˆ APAMETPOB ***
C
C                      MAS < A0 A1 ..... A_N >;
C
C
C   METO„:     CXEMA ƒOPHEPA;
C   -----
C   APAMETP›:  KMAS01 - ‡HA—EHˆE F(X) Pˆ
C   ---------   X      - TO—KA OPE„E‹EHˆŸ F(X);
C               TMAS - MACCˆB „AHH›X ( PA‡MEPHOCTˆ N  C‹OB ):
C   MACCˆB  --> N   A0  A1  A2 ..........................  ˆ T.„.
C   C‹OBO # --> 1    2  3   4  ..........................  ˆ T.„.
C
C
C   O„POƒPAMM›:      HET
C   ------------
C   PˆME—AHˆE:   1.  B TMAS BCE B REAL*4
C   ----------        ********************
C
C
C
CZZZ-------------------------------------------------------------------
       DIMENSION TMAS(1)
CVVVVV REAL KMAS01
       N  = IFIX( TMAS(1) )
C-->  CXEMA ƒOPHEPA...
       NP1 = N+1
       I = NP1
       SP = 0.0
10     CONTINUE
       SP = SP * X  + TMAS(I)
       I = I - 1
       IF ( I .GE. 2 ) GO TO 10
       KMAS01 = SP
       RETURN
       END
