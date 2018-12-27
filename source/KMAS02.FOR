       REAL FUNCTION KMAS02( T, TMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA-”“HK–ˆŸ  KMAS02( T, TMAS )
C   --------------------
C   HA‡HA—EHˆE:        B›—ˆC‹EHˆE ‡HA—EHˆŸ F(T), ‡A„AHHO‰ TA‹ˆ—HO
C   ----------         O OOPH›M TO—KAM (T1,Y1), ...., (T_N,Y_N)
C                      POBO„Ÿ K“CO—HO-‹ˆHE‰H“ž ˆHTEPO‹Ÿ–ˆž;
C                      Pˆ T <= T1, BCEƒ„A F(T) = Y1;
C                      Pˆ T >= T_N, BCEƒ„A F(T) = Y_N;
C                      „O“CKAETCŸ ‡A„AHˆE ‹žOƒO KO‹ˆ—ECTBA TO—EK.
C                      Pˆ N=0 ˆ‹ˆ N=1 - BCEƒ„A F(T)=0.0
C
C                      *** KO„ˆPOBKA B OˆCAHˆˆ APAMETPOB ***
C
C                      MAS < T1 Y1 ..... T_N Y_N >
C
C                      Pˆ MAS < 0 > ˆ‹ˆ MAS < T_‹žOE >
C                      PˆHˆMAETCŸ F(T) = 0.0
C
C   METO„:     K“CO—HO-‹ˆHE‰HAŸ ˆHTEPO‹Ÿ–ˆŸ;
C   -----      „BOˆ—H›‰ OˆCK ˆHTEPBA‹A;
C   APAMETP›:  KMAS02 - ‡HA—EHˆE F(T) Pˆ
C   ---------   T      - BPEMŸ OPE„E‹EHˆŸ F(T);
C               TMAS - MACCˆB „AHH›X ( PA‡MEPHOCTˆ  N C‹OB ):
C   MACCˆB  --> N   T1 Y1   T2 Y2   T3 Y3   T4 Y4   T5 Y5  ˆ T.„.
C   C‹OBO # --> 1    2  3   4   5    6  7    8  9   10  11 ˆ T.„.
C
C    B MACCˆBE OˆC›BAETCŸ [N/2] - TO—EK (T_K, Y_K), OPE„E‹Ÿž™ˆX
C                                   ”“HK–ˆž F(T);
C
C   O„POƒPAMM›:      HET
C   ------------
C   PˆME—AHˆE:     1.  Pˆ K - HE —ETHOE, “—ˆT›BAETCŸ TO‹œKO
C   ----------          —ETHOE ‡HA—EHˆE K;
C                   2. ˆHTEPBA‹› B›ˆPAžTCŸ BCEƒ„A O PˆH–ˆ“:
C                        T#K#    <   T   <=    T#K+1#
C
CZZZ-------------------------------------------------------------------
       DIMENSION TMAS(*)
CVVVVV REAL KMAS02
       N2 = IFIX( TMAS(1) )
C---> CKO‹œKO TO—EK..
       N = N2/2
       IF ( N .NE. 0 ) GO TO 1
C-->  B›PA†„EHˆE..
       KMAS02 = 0.0
       RETURN
1      CONTINUE
C--> HE HA ƒPAHˆ–AX ‹ˆ M›....
       T1 = TMAS(2)
       TN = TMAS( 2*N )
       IF ( T .LE. T1 ) GO TO 2
       IF ( T .GE. TN ) GO TO 3
C--->  OˆCK ˆHTEPBA‹A...
C-->   T(K) -> TMAS(2*K);   Y(K) -> TMAS( 2*K+1 )
       I = 1
       J = N + 1
20     K = (I+J)/2
       TK = TMAS(2*K)
       IF ( T .LT. TK ) J = K
       IF ( T .GE. TK ) I = K
       IF ( J .GT. (I+1) ) GO TO 20
C--> ˆHTEPBA‹ HA˜‹ˆ -->  ME†„“ I  ˆ  (I+1) TO—KO‰;
       I2 =  2*I
       TI =  TMAS(I2)
       YI =  TMAS(I2+1)
       TI1 = TMAS(I2+2)
       YI1 = TMAS(I2+3)
       DTI = TI1 - TI
       DT  = T - TI
       D   = DT/DTI
       KMAS02 = YI + (YI1-YI) * D
       RETURN
C######################################################################
2      CONTINUE
       KMAS02 = TMAS(3)
       RETURN
3      CONTINUE
       KMAS02 = TMAS(2*N+1)
       RETURN
       END
