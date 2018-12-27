         SUBROUTINE READ00( KOD, TIME, TOK, VALL )
CXXX----KOM‹EKC "MS1" --> FOR DOS                          -----------
C   O„POƒPAMMA:   READ00( KOD, TIME, TOK, VALL )
C   -------------
C   HA‡HA—EHˆE:    —TEHˆE ”A‰‹A  HAKO‹EHˆŸ B MACCˆB› TOK ˆ VALL
C   ----------      ( TO—KA TIME );
C   METO„:         “PAB‹EHˆE ˆ —TEHˆE ”A‰‹A  HAKO‹EHˆŸ;
C   -----
C   APAMETP›:           BXO„:
C   ---------            *****
C                  KOD = 1 - ‡APOC HA OTKP›TˆE ”A‰‹OB HAKO‹EHˆŸ;
C                      = 2 - —TEHˆE O—EPE„HO‰ ‡AˆCˆ;
C                      = 3 - O„BO„ ”A‰‹A NSPOOL ‡A ‡AˆCœ NZAPSP
C                                                *****************
C
C                             ( XO‹OCTOE —TEHˆE NZAPSP ‡AˆCE‰ );
C                         Pˆ TOM TIME, TOK, VALL HE ˆ‡MEHŸžTCŸ;
C
C
C                        B›XO„:
C                        *****
C                  KOD = 1 - „E‰CTBˆE B›O‹HEHO;
C                      = 2 - ‹Oƒˆ—ECKˆ‰ KOHE– ”A‰‹A  HAKO‹EHˆŸ;
C                      = 3 - ”ˆ‡ˆ—ECKˆ‰ KOHE– ”A‰‹A  ˆ‹ˆ CO‰;
C                  TIME BPEMŸ OPE„E‹EHˆŸ MACCˆBOB:
C                  TOK ˆ VALL - "TOKOB" ˆ “‡‹OB›X HAPŸ†EHˆ‰;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------   2. readmb
C
C   PˆME—AHˆE:    Pˆ OTKP›Tˆˆ ”A‰‹OB HAKO‹EHˆŸ B TOK ˆ VALL -
C   ----------     “PAB‹Ÿž™ˆE ‡AˆCˆ ”A‰‹OB, A B TIME - HOUTT -
C                  ˜Aƒ O BPEMEHˆ COXPAHEHˆŸ PE˜EHˆŸ;
C                  O„HA ‡AˆCœ -> TN, TOK, VALL;
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
         DIMENSION TOK(1), VALL(1)
         DIMENSION  V_A(15),  T_A(15)
         EQUIVALENCE( MASPRT(13), NWTRE ),
     =              ( MASPRT(14), NWTR  ),
     =              ( MASPRT(18), NSPOOL),
     =              ( MASPRT(92), NZAPSP),
     =              ( MASPRT(26), NSYS  ),
     =              ( MASPRT(46), NNTOK ),
     =               ( MASPRT(121), FTRSPL )
C
          INTEGER FTRSPL,  SPISNN, SPISVL, SPISTK
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         GO TO ( 1, 2, 3 ),KOD
         CALL SUBERR(KOD)
1        CONTINUE
C
       CALL KSOPEN(NSPOOL,1,ii_rr)
       IF (ii_rr .NE. 0 ) GO TO 21
C
C--->  B›BO„ ”A‰‹OB B HA—A‹O
         REWIND NSPOOL
C
C        READ(NSPOOL,END=20,ERR=21 ) TIME,(TOK(I), I=1, NNTOK  ),
C    =                                  (VALL(I), I=1, NSYS   )
C@@@@@@@@@@@@@@@@@
C      iikod = 0
       READ(NSPOOL,END=20,ERR=21 ) TIME, FTRSPL
C      CALL readmb(nspool,NNTOK,TOK,iikod)
C      CALL readmb(nspool,NSYS,VALL,iikod)
C       IF ( iikod .EQ. 1 ) go to 20
C       IF ( iikod .EQ. 2 ) go to 21
C@@@@@@@@@@@@@@@@@
         KOD = 1
         IF ( TIME .EQ. -1.0 ) KOD = 2
         RETURN
2        CONTINUE
C--->  B‡ŸTœ O—EPE„H“ž
C        READ(NSPOOL, END=20, ERR=21)  TIME, (TOK(I), I=1,NNTOK ),
C    =                                      (VALL(I), I=1,NSYS  )
C@@@@@@@@@@@@@@@@@
       READ(NSPOOL,END=20,ERR=21 ) TIME
C@@@@@@@@@@@@@@@@@
       IF (FTRSPL .EQ. 0 ) THEN
          CALL readmb(nspool,NNTOK,TOK,iikod)
          CALL readmb(nspool,NSYS,VALL,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
       ELSE
          CALL readmb(nspool,SPISTK(1),T_A,iikod)
          CALL readmb(nspool,SPISVL(1),V_A,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
          DO 666 i666=1,SPISTK(1)
             nnii = SPISTK(i666+1)
666          TOK(nnii) = T_A(i666)
          DO 667 i666=1,SPISVL(1)
             nnii = SPISVL(i666+1)
667          VALL(nnii) = V_A(i666)
       ENDIF
C@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@
         KOD = 1
         IF ( TIME .EQ. -1.0 ) KOD = 2
         RETURN
20       CONTINUE
C--->  ”ˆ‡ˆ—ECKˆ‰ KOHE–......(„A “†...)
         KOD = 3
         WRITE(NWTR, 50) NSPOOL
         WRITE(NWTRE, 50) NSPOOL
50      FORMAT(/' *** K®­¥æ ä ©«  ­ ª®¯«¥­¨ï ( ª ­ « # ',  I2, ' ) ***'/
     = ' *** P ¡®â  PEP›BAETCŸ ***'///)
         CLOSE (NSPOOL)
         RETURN
21       CONTINUE
C--->  O˜ˆKˆ —TEHˆŸ....
         WRITE(NWTR,51) NSPOOL
         WRITE(NWTRE,51) NSPOOL
51       FORMAT(/' *** C¡®© çâ¥­¨ï ä ©«  ­ ª®¯«¥­¨ï ( ª ­ « # ', I2,
     =  ' ) ***'/  ' *** PAOTA PEP›BAETCŸ ***'///)
         KOD = 3
         CLOSE (NSPOOL)
         RETURN
C----------------------------------------------------------------------
3      CONTINUE
       KOD = 1
C
       CALL KSOPEN(NSPOOL,1,ii_rr)
       IF (ii_rr .NE. 0 ) GO TO 21
C
       REWIND NSPOOL
       READ(NSPOOL,END=20,ERR=21 ) TIME, FTRSPL
       IF ( NZAPSP  .LE. 1 ) RETURN
       DO 300 NZ = 1, NZAPSP-1
       NZX = NZ
C@@@@@
C      READ(NSPOOL, END=200, ERR=210 )
C@@@@@
C@@@@@@@@@@@@@@@@@
       READ(NSPOOL,END=20,ERR=21 ) TIME
C@@@@@@@@@@@@@@@@@
       IF (FTRSPL .EQ. 0 ) THEN
          CALL readmb(nspool,NNTOK,TOK,iikod)
          CALL readmb(nspool,NSYS,VALL,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
       ELSE
          CALL readmb(nspool,SPISTK(1),T_A,iikod)
          CALL readmb(nspool,SPISVL(1),V_A,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
          DO 668 i666=1,SPISTK(1)
             nnii = SPISTK(i666+1)
668          TOK(nnii) = T_A(i666)
          DO 669 i666=1,SPISVL(1)
             nnii = SPISVL(i666+1)
669          VALL(nnii) = V_A(i666)
       ENDIF
C@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@
CCC    READ(NSPOOL,END=20,ERR=21 )
CCC    CALL readmx(nspool,NNTOK,TOK,iikod)
CCC    CALL readmx(nspool,NSYS,VALL,iikod)
CCC     IF ( iikod .EQ. 1 ) go to 200
CCC     IF ( iikod .EQ. 2 ) go to 210
C@@@@@@@@@@@@@@@@@
300    CONTINUE
       RETURN
C----------------------------------------------------------------------
200    WRITE(NWTRE, 52 ) NZX
       WRITE(NWTR, 52 ) NZX
52     FORMAT(/' *** ˆ‡ ”A‰‹A HAKO‹EHˆŸ PO—ˆTAHO ', I5, ' ‡AˆCE‰',
     =         ' ***' )
       GO TO 20
C->
210    WRITE( NWTRE, 52) NZX
       WRITE( NWTR, 52)  NZX
       GO TO 21
C----------------------------------------------------------------------
         END
