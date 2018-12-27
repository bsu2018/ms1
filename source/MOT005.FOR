         SUBROUTINE MOT005(G6,G12,J6,J12)
CXXXX   KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)-------------
C
C     O„POƒPAMMA: MOT005(G6,G12,J6,J12)
C    -------------
C
C     HA‡HA—EHˆE: O‹“—ˆTœ PAC˜ˆPEHH“ž MATPˆ–“ POBO„ˆMOCTE‰ „‹Ÿ
C     ----------  MO„E‹ˆ ‹. MA˜ˆH›, ˆ‡ MATPˆ–› G6 - TO‹œKO BXO„›;
C                 TO †E „‹Ÿ J12 - ˆCTO—HˆKOB TOKA <-- ˆ‡ J6;
C
C    METO„: PŸMOE PAC˜ˆPEHˆE MATPˆ– G6 ˆ J6 COƒ‹ACHO MAT. MO„E‹ˆ;
C    -----
C
C    APAMETP›: G6 - C“‹OK „‹Ÿ G12;
C    ---------
C               G12 - B›XO„HAŸ PAC˜ˆPEHHAŸ MATPˆ–A POBO„ˆMOCTE‰;
C               J6  - C“‹OK „‹Ÿ J12;
C               J12 - B›XO„HO‰ MACCˆB ˆCTO—HˆKOB TOKA;
C
C     ˆC‹œ‡“EM›E O„POƒPAMM›: HET
C     -------------------------
C
CZZZZ    MOT005    -----------------------------------------------------
C
C
         DIMENSION G6(6,6),G12(12,12),J6(6),J12(12)
         REAL*4 G6,G12,J6,J12
         INTEGER*4 I,J,I7,J7
C------
         DO 1 I=1,6
         DO 1 J=1,6
1        G12(I,J)=G6(I,J)
C------
         DO 2 I=7,12
         I7=I-6
         DO 2 J=7,12
         J7=J-6
2        G12(I,J)=G6(I7,J7)
C-------
         DO 3 I=1,6
         DO 3 J=7,12
         J7=J-6
3        G12(I,J)=-G6(I,J7)
C-------
         DO 4 I=7,12
         I7=I-6
         DO 4 J=1,6
4        G12(I,J)=-G6(I7,J)
C-------
         DO 5 I=1,12
         IF ( I .LE. 6 ) J12(I)=J6(I)
         IF ( I .GT. 6 ) J12(I)=-J6(I-6)
5        CONTINUE
         RETURN
         END
