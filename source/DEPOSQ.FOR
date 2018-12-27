         SUBROUTINE DEPOSQ( ZY, ZJ, YALL, JALL )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DEPOSQ( ZY, ZJ, YALL, JALL )
C   ------------
C   HA‡HA—EHˆE:    OC“™ECTB‹EHˆE AKTA "—ˆCTKˆ" -
C   ----------     MATPˆ–› “‡‹OB›X POBO„ˆMOCTE‰ YALL ˆ BEKTOPA
C                  ˆCTO—HˆKOB TOKA JALL ‡HA—EHˆŸMˆ, COOTBETCTBEHHO
C                  ZY  ˆ  JY;
C
C
C   METO„:         B V.M.=2.*  -  XPAHEHˆE HE “AKOBAHHO‰ MATPˆ–›;
C   -----
C   APAMETP›:     CM. HA‡HA—EHˆE;
C   ---------
C   O„POƒPAMM›:  1. PROF1
C   -------------
C   PˆME—AHˆE:    1. NDALL - "‡AŸBO—HAŸ" PA‡MEPHOCTœ YALL(*,*);
C   ----------        NSYS ‹EMEHTOB "—ˆCTŸTCŸ";
C                  2. KOHTPO‹œ HE POBO„ˆTCŸ;
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION YALL(1), JALL(1)
         REAL JALL
         EQUIVALENCE ( NDALL, MASPRT(23) )
         EQUIVALENCE ( NSYS,  MASPRT(26) )
         ID( I, J ) = NDALL * ( J - 1 )  +  I
C======================================================================
C======================================================================
         DO 500 I = 1, NSYS
500      JALL(I) = ZJ
C----------------------------------------------------------------------
         DO 501 I = 1, NSYS
            DO 501 J = 1, NSYS
               IJ = ID(I,J)
               YALL(IJ) = ZY
501      CONTINUE
       RETURN
         END
