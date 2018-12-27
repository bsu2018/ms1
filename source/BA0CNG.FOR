         SUBROUTINE BA0CNG(I,J, BA0)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:   BA0CNG(I,J, BA0)
C   ------------
C   HAáHAóEHàE:    èEPECTABãüET I  à  J  áAèàCà B MACCàBE BA0
C  ------------     ( CM. TRIARB )
C   METOÑ:         OóEBàÑEH;
C   -----
C   èAPAMETPõ:     I,J HOMEPA áAèàCEâ B BA0;
C   ---------      BA0 - CM. INPUT0;
C                  ( ÑãàHA áAèàCà  -  N1BA0 =MASPRT(4) )
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1
C   -------------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         INTEGER BA0
         DIMENSION BA0(*)
         EQUIVALENCE ( N1BA0, MASPRT(4) )
C--> I0 - AÑP. HAóAãA, IM - AãP. KOHñA
         I0 = 6
         IM = BA0(3)
C--> AÑP. HAóAã áAèàCEâ I à J
         IADR = (I-1)*N1BA0 + I0
         JADR = (J-1)*N1BA0 + I0
C---> KOHTPOãú
         IF ( IADR+N1BA0 .GT. IM ) CALL SUBERR(IADR)
         IF ( JADR+N1BA0 .GT. IM ) CALL SUBERR(JADR)
C--> èEPECTABãüEM...
         DO 1 K = 1, N1BA0
         IX = BA0(IADR+K - 1)
         BA0(IADR+K - 1) = BA0(JADR+K - 1)
         BA0(JADR+K - 1) = IX
1        CONTINUE
         RETURN
         END
