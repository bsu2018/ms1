         SUBROUTINE DEPOST( K, M, G0, J0, YALL, JALL )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DEPOST( K, M, G0, J0, YALL, JALL )
C   ------------
C   HA‡HA—EHˆE:    OC“™ECTB‹EHˆE AKTA "O‹EMEHTHOƒO" BK‹A„A B
C   ----------     MATPˆ–“ “‡‹OB›X POBO„ˆMOCTE‰ YALL ˆ BEKTOP
C                  ˆCTO—HˆKOB TOKA JALL O—EPE„HO‰ BETBˆ, COE„ˆHŸž™E‰
C                  "K" ˆ "M" “‡‹› ( OT "K" K "M" ), ˆ MO„E‹ˆP“EMO‰
C                  G0 ˆ J0 APAMETPAMˆ;
C
C   METO„:         B V.M.=2.*  -  XPAHEHˆE HE “AKOBAHHO‰ MATPˆ–›;
C   -----
C   APAMETP›:     CM. HA‡HA—EHˆE;
C   ---------
C   O„POƒPAMM›:  1. PROF1
C   -------------
C   PˆME—AHˆE:    1. NDALL - "‡AŸBO—HAŸ" PA‡MEPHOCTœ YALL(*,*);
C   -----------    2. KOHTPO‹œ HE POBO„ˆTCŸ;
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION YALL(1), JALL(1)
         REAL JALL, J0
         EQUIVALENCE ( NDALL, MASPRT(23) )
         ID( I, J ) = NDALL * ( J - 1 )  +  I
C--->   O‹EMEHTH›‰ BK‹A„:
C======================================================================
         IF ( K .EQ. 0 ) GO TO 50000
         IKK = ID( K, K )
         YALL(IKK) = YALL(IKK) + G0
         JALL(K) = JALL(K) - J0
         IF ( M .EQ. 0 ) RETURN
         IMM = ID( M, M )
         YALL(IMM) = YALL(IMM) + G0
         JALL(M) = JALL(M) + J0
         IKM = ID( K, M )
         IMK = ID( M, K )
         YALL(IKM) = YALL(IKM) - G0
         YALL(IMK) = YALL(IMK) - G0
         RETURN
50000    CONTINUE
         IF ( M .EQ. 0 ) GO TO 50001
         IMM = ID( M, M )
         YALL(IMM) = YALL(IMM) + G0
         JALL(M) = JALL(M) + J0
         RETURN
50001    CONTINUE
C===> BCE “‡‹› H“‹EB›E <======
         RETURN
C======================================================================
         END
