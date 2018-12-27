         SUBROUTINE ENDLOG( LOGYTR )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   ENDLOG( LOGYTR )
C   ------------
C   HA‡HA—EHˆE:    HAXO†HEHˆE ˆ PA‡METKA "A‡OB›X" “‡‹OB BCEX
C   ----------     ˆH–ˆ„EHTH›X ƒP“. HA MECTO "A‡OB›X" ‡AˆC›BAETCŸ
C                  Pˆ‡HAK "-1" -  OCHOBHO‰ “‡E‹. H“‹EB›E “‡‹› HE
C                  ˆ‡MEHŸžTCŸ;
C   METO„:         CKAHˆPOBAHˆE LOGYTR, Pˆ LOGYTR(I) = I -> "A‡OB›‰";
C   -----
C   O„POƒPAMM›:  1. COMMON PROF1
C   ------------
C   PˆME—AHˆE:    1. Pˆ FTEST >= 2 E—ATœ LOGYTR HA NWTRE
C   ----------        OC‹E POBE„EHˆŸ MO„ˆ”ˆKA–ˆˆ;
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION LOGYTR(1)
         EQUIVALENCE ( KNOT6M, MASPRT(58) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( FTEST,  MASPRT(60) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         INTEGER FTEST
         DO 100 I = KNOT6M, NSYS
         IF ( LOGYTR(I) .LE. 0 ) GO TO 100
C---> I - B ƒ“E ˆH–ˆ„EHTH›X
C    MO†ET TO "A‡OB›‰" ?
         IF ( LOGYTR(I) .EQ. I ) LOGYTR(I) = -1
100      CONTINUE
         IF ( FTEST .LT. 2 ) RETURN
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 2 )
2        FORMAT(' ***FTEST_2: MACCˆB K.‡. “‡‹OB C OTME—EHH›Mˆ "A‡OB›Mˆ"
     = “‡‹AMˆ ***')
         WRITE(NWTRE, 1 ) ( I, LOGYTR(I), I=1, NSYS)
1        FORMAT(           5( 2X,   'LOG(', I4, ')=', I4 ) )
C!!!-------------------------------------------------------------------
         RETURN
         END
