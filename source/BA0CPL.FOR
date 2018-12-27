         INTEGER  FUNCTION   BA0CPL(I,J, BA0)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA-”“HK–ˆŸ:  BA0CPL(I,J, BA0)
C   ---------------------
C   HA‡HA—EHˆE:    CPABHEHˆE K‹ž—E‰  I  ˆ  J  ‡AˆCˆ B MACCˆBE BA0
C  ------------     ( CM. TRIARB ); K‹ž—AMˆ ŸB‹ŸžTCŸ O‹Ÿ Pˆ‡HAKA
C                    TˆA MAKPOMO„E‹ˆ;
C   METO„:         O—EBˆ„EH;
C   -----
C   APAMETP›:     I,J HOMEPA ‡AˆCE‰ B BA0;
C   ---------      BA0 - CM. INPUT0;
C                  ( „‹ˆHA ‡AˆCˆ  -  N1BA0 =MASPRT(4) )
C                  BA0CPL  =  -1  K‹ž—  I  MEHœ˜E  K‹ž—A  J;
C                          =   0  K‹ž—  I  PABEH   K‹ž—“  J;
C                          =  +1  K‹ž—  I  O‹œ˜E  K‹ž—A  J;
C
C   O„POƒPAMM›:  1. COMMON PROF1
C   -------------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         INTEGER BA0
         DIMENSION BA0(*)
         EQUIVALENCE ( N1BA0, MASPRT(4) )
C--> I0 - A„P. HA—A‹A, IM - A‹P. KOH–A
         I0 = 6
         IM = BA0(3)
C--> A„P. HA—A‹ ‡AˆCE‰ I ˆ J
         IADR = (I-1)*N1BA0 + I0
         JADR = (J-1)*N1BA0 + I0
C---> KOHTPO‹œ
         IF ( IADR+N1BA0 .GT. IM ) CALL SUBERR(IADR)
         IF ( JADR+N1BA0 .GT. IM ) CALL SUBERR(JADR)
C--> CPABHˆBAEM.....
         KEYI = BA0(IADR+4)
         KEYJ = BA0(JADR+4)
         IF ( KEYI - KEYJ ) 1, 2, 3
1        BA0CPL = -1
         RETURN
2        BA0CPL = 0
         RETURN
3        BA0CPL = +1
         RETURN
         END
