         SUBROUTINE GETLOG( K, M, LOGYTR )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   GETLOG( K, M, LOGYTR )
C   -------------
C   HA‡HA—EHˆE:    B›O‹HEHˆE O—EPE„HOƒO AKTA ”OPMˆPOBAHˆŸ
C   ----------     MACCˆBA LOGYTR K.‡. “‡‹OB.
C   METO„:         MO„ˆ”ˆKA–ˆŸ "BO‹HOBOƒO" A‹ƒOPˆTMA,
C   -----          OPˆEHˆPOBAHHOƒO HA "O‹EMEHTHOE" ”OPMˆPOBAHˆE
C                  MACCˆBA LOGYTR;
C   APAMETP›:     K, M - ˆH–ˆ„EHTH›E “‡‹›;
C   ---------      LOGYTR - MACCˆB K.‡. “‡‹OB;
C   O„POƒPAMM›:  1. COMMON PROF1;
C   ------------
C   PˆME—AHˆE:    1. MACCˆB LOGYTR PE„O‹OƒAETCŸ ˆHˆ–ˆˆPOBAHH›M;
C   -----------    2. OPEN6 ˆ KNOT6M  TO †E “†E  ˆHˆ–ˆˆPOBAH›;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION LOGYTR(1)
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( KNOT6M, MASPRT(58) )
         EQUIVALENCE ( OPEN6,  MASPRT(57) )
         INTEGER  OPEN6
         OPEN6 = OPEN6 + 1
         KHELP = NSYS
         MHELP = NSYS
         IF ( K .NE. 0 ) KHELP = K
         IF ( M .NE. 0 ) MHELP = M
         KNOT6M = MIN0( KNOT6M, KHELP, MHELP )
         MINKM = MIN0( K, M )
         LK = 0
         LM = 0
         IF ( K .NE. 0 ) LK = LOGYTR(K)
         IF ( M .NE. 0 ) LM = LOGYTR(M)
C$$$$$$$$ BETBˆMCŸ $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         IF ( LK. EQ. -1  .AND.  LM .EQ. -1 ) GO TO 1
         IF ( LK. NE. -1  .AND.  LM .NE. -1 ) GO TO 2
         IF ( LK. EQ. -1 ) GO TO 3
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
4        CONTINUE
C---->  LK ^= -1,  LM = -1
         MING = MIN0( MINKM, LK )
         IF ( K .NE. 0 ) LOGYTR(K) = MING
         IF ( M .NE. 0 ) LOGYTR(M) = MING
C--->  H“†HO ‹ˆ MO„ˆ”ˆ–ˆPOBATœ LOGYTR ?
         IF ( LK .EQ. MING ) GO TO 666
C--> LOGYTR MO„. O LK  ˆ  MING
         MOLD = LK
         MNEW = MING
         ASSIGN 666 TO METKA
         GO TO 600
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
3        CONTINUE
C---> LK = -1,  LM ^= -1
         MING = MIN0( MINKM, LM )
         IF ( K .NE. 0 ) LOGYTR(K) = MING
         IF ( M .NE. 0 ) LOGYTR(M) = MING
         IF ( LM .EQ.  MING ) GO TO 666
         MOLD = LM
         MNEW = MING
         ASSIGN 666 TO METKA
         GO TO 600
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
2        CONTINUE
C--->  LK ^= -1,  LM ^= -1
         MING = MIN0( MINKM, LK, LM )
         IF ( K .NE. 0 ) LOGYTR(K) = MING
         IF ( M .NE. 0 ) LOGYTR(M) = MING
         IF ( LK .EQ. MING ) GO TO 20
C  -> MO„. O LK..
         MOLD = LK
         MNEW = MING
         ASSIGN 20 TO METKA
         GO TO 600
20       CONTINUE
         IF ( LM .EQ. MING ) GO TO 666
C-->  MO„. O LM ...
         MOLD = LM
         MNEW = MING
         ASSIGN 666 TO METKA
         GO TO 600
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
1        CONTINUE
C--->  LK = -1,   LM = -1
         IF ( K.NE. 0 ) LOGYTR(K) = MINKM
         IF ( M.NE. 0 ) LOGYTR(M) = MINKM
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      RETURN
C**********************************************************************
C**********************************************************************
C  O„POƒPAMMA "MO„ˆ”ˆ–ˆPOBATœ LOGYTR"
C  O ‡HA—EHˆŸM "MOLD" ˆ "MNEW"
C**********************************************************************
C**********************************************************************
600      CONTINUE
         DO 601  I = KNOT6M, NSYS
            IF ( LOGYTR(I) .NE. MOLD ) GO TO 601
C---> AKT "MO„ˆ”ˆKA–ˆˆ"
         LOGYTR(I) = MNEW
601      CONTINUE
         GO TO METKA, ( 666, 20 )
602      CALL SUBERR(20)
         END
