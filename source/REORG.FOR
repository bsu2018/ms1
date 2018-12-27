         SUBROUTINE REORG( K, L, ECHANG, COMPCL,  BA0)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   REORG( K, L, ECHANG, COMPCL,  BA0)
C   -------------
C   HA‡HA—EHˆE:    "PEOPƒAHˆ‡A–ˆŸ" COPTˆP“EMO‰ OC‹E„OBATE‹œHOCTˆ
C   -----------     COƒ‹ACHO A‹ƒOPˆTMA / TRIARB.
C   METO„:         CM. TIRARB;
C  -------
C   APAMETP›:     CM. TRIARB;
C   ---------
C   O„POƒPAMM›:  CM. TRIARB;
C   ------------
C   PˆME—AHˆE:     C‹“†EHAŸ / „‹Ÿ COPTˆPOBKˆ;
C   -----------
C
CZZZ-------------------------------------------------------------------
         EXTERNAL ECHANG, COMPCL
         INTEGER COMPCL
         DIMENSION BA0(1)
         INTEGER GAUCHE,  DROITE
C                C‹EBA    CPABA
         J = K
100      CONTINUE
         GAUCHE = 2*J
         IF( GAUCHE .GT. L ) GO TO 1000
         INDMAX = GAUCHE
         DROITE = GAUCHE + 1
         IF ( DROITE .GT. L ) GO TO 150
         IF ( COMPCL(GAUCHE, DROITE, BA0 ) .LT. 0 ) INDMAX = DROITE
150      CONTINUE
         IF ( COMPCL(INDMAX, J,      BA0 ) .LE. 0 ) GO TO 1000
         CALL ECHANG(J, INDMAX,  BA0)
         J = INDMAX
         GO TO 100
C--->
1000     RETURN
         END
