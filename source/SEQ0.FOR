         SUBROUTINE SEQ0( X1,  X2, K, IRC )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑPOÉPAMMA:    SEQ0( X1,  X2, K, IRC )
C   ------------
C   HAáHAóEHàE:     CPABHEHàE MACCàBOB X1 à X2 ( K-CãOB);
C   -----------     IRC = 1 - COBèAÑAûT, = 2 - HET;
C   METOÑ:
C   -----
C   èAPAMETPõ:     CM. HAáHAóEHàE;
C   ----------
C   èOÑèPOÉPAMMõ:  HET
C   -------------
C   èPàMEóAHàE:
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION X1(1),  X2(1)
         INTEGER X1, X2
         IRC = 1
         DO 1 I=1,K
         IF ( X1(I) .EQ. X2(I) ) GO TO 1
         IRC = 2
         GO TO 2
1        CONTINUE
2        RETURN
         END
