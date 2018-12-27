         INTEGER  FUNCTION   BA1CPL(I,J, BA0)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT ABƒ“CTA 1987 ƒ.)------------
C   O„POƒPAMMA-”“HK–ˆŸ:  BA1CPL(I,J, BA0)
C   ---------------------
C   HA‡HA—EHˆE:    CPABHEHˆE K‹ž—E‰  I  ˆ  J  ‡AˆCˆ B MACCˆBE BA0
C  ------------     ( CM. TRIARB ); K‹ž—AMˆ ŸB‹ŸžTCŸ O‹Ÿ
C                    HOMEPOB ‹EMEHTOB.
C   METO„:         O—EBˆ„EH;
C   -----
C   APAMETP›:     I,J HOMEPA ‡AˆCE‰ B BA0;
C   ---------      BA0 - CM. INPUT0;!!! EPE„AETCŸ OTPE‡OK BA0 !!!
C                  ( „‹ˆHA ‡AˆCˆ  -  N1BA0 =MASPRT(4) )
C                  BA1CPL  =  -1  K‹ž—  I  MEHœ˜E  K‹ž—A  J;
C                          =   0  K‹ž—  I  PABEH   K‹ž—“  J;
C                          =  +1  K‹ž—  I  O‹œ˜E  K‹ž—A  J;
C
C   O„POƒPAMM›:  COMMON PROF1
C   -------------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         INTEGER BA0
         DIMENSION BA0(1)
         EQUIVALENCE ( N1BA0, MASPRT(4) )
         I0 = 1
C--> A„P. HA—A‹ ‡AˆCE‰ I ˆ J
         IADR = (I-1)*N1BA0 + I0
         JADR = (J-1)*N1BA0 + I0
C--> CPABHˆBAEM.....
         KEYI = BA0(IADR+0)
         KEYJ = BA0(JADR+0)
         IF ( KEYI - KEYJ ) 1, 2, 3
1        BA1CPL = -1
         RETURN
2        BA1CPL = 0
         RETURN
3        BA1CPL = +1
         RETURN
         END
