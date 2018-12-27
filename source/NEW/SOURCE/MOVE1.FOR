         SUBROUTINE MOVE1( OUT, IN, K )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:    MOVE1( OUT, IN, K )
C   ------------
C   HA‡HA—EHˆE:     EPEC›‹KA K C‹OB ˆ‡ MACCˆBA IN B MACCˆB OUT;
C   -----------
C   METO„:             ( A‹TEPHATˆBA K MOVE0 )
C   -----
C   APAMETP›:     CM. HA‡HA—EHˆE;
C   ----------
C   O„POƒPAMM›:  HET
C   -------------
C   PˆME—AHˆE:
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION IN(1), OUT(1)
         INTEGER IN, OUT
         DO 1 I=1,K
         OUT(I) = IN(I)
1        CONTINUE
         RETURN
         END
