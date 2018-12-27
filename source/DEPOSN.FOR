         SUBROUTINE DEPOSN( K, L, M, N, GKLMN, YALL )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT HOŸPŸ  1987 ƒ.)------------
C   O„POƒPAMMA:   DEPOSN( K, L, M, N, GKLMN, YALL )
C   ------------
C   HA‡HA—EHˆE:    OC“™ECTB‹EHˆE AKTA "O‹EMEHTHOƒO" BK‹A„A B
C   ----------     MATPˆ–“ “‡‹OB›X POBO„ˆMOCTE‰ YALL  GJ-APAMETPOB
C                  „‹Ÿ ˆCTO—HˆKA TOKA ,COE„ˆHŸž™EƒO “‡‹› "K" ˆ "L"
C                  ( OT "K" K "L" ), “PAB‹ŸEMOƒO HAPŸ†EHˆEM ME†„“
C                  “‡‹AMˆ "M" ˆ "N" ( OT "M" K "N" ) O ‹ˆHE‰HOM“
C                  ‡AKOH“:  J(K,L) = U(M,N) * GKLMN
C                           ***********************
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
         DIMENSION YALL(1)
         EQUIVALENCE ( NDALL, MASPRT(23) )
         ID( I, J ) = NDALL * ( J - 1 )  +  I
C--->   O‹EMEHTH›‰ BK‹A„:
C======================================================================
         IF ( K .EQ. 0 ) GO TO 1000
         IF ( M .EQ. 0 ) GO TO 2000
         IKM = ID( K, M )
         YALL(IKM) = YALL(IKM) + GKLMN
         IF ( N .EQ. 0 ) GO TO 3000
         IKN = ID( K, N )
         YALL(IKN) = YALL(IKN) - GKLMN
         IF ( L .EQ. 0 ) RETURN
         ILM = ID( L, M )
         ILN = ID( L, N )
         YALL(ILM) = YALL(ILM) - GKLMN
         YALL(ILN) = YALL(ILN) + GKLMN
         RETURN
1000     CONTINUE
         IF ( L .EQ. 0 ) GO TO 50001
         IF ( M .EQ. 0 ) GO TO 1500
         ILM = ID( L, M )
         YALL(ILM) = YALL(ILM) - GKLMN
1500     CONTINUE
         IF ( N .EQ. 0 ) GO TO 50001
         ILN = ID( L, N )
         YALL(ILN) = YALL(ILN) + GKLMN
         RETURN
2000     CONTINUE
         IF ( N .EQ. 0 ) GO TO 50001
         IKN = ID( K, N )
         YALL(IKN) = YALL(IKN) - GKLMN
         IF ( L .EQ. 0 ) RETURN
         ILN = ID( L, N )
         YALL(ILN) = YALL(ILN) + GKLMN
         RETURN
3000     CONTINUE
         IF ( L .EQ. 0 ) RETURN
         ILM = ID( L, M )
         YALL(ILM) = YALL(ILM) - GKLMN
         RETURN
50001    CONTINUE
C===> BCE “‡‹› H“‹EB›E <======
         RETURN
C======================================================================
         END
