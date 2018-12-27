       SUBROUTINE READST( KAR, IK, IKMAX, NRDRX, DSNAME, KFIZ, KODEOF )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: READST( KAR, IK, IKMAX, NRDRX, DSNAME, KFIZ, KODEOF )
C   ------------
C   HA‡HA—EHˆE:  BBO„ CTPOKˆ ˆCXO„H›X „AHH›X „‹Ÿ / INPUT0,
C   ----------   —EPE‡ KAHA‹ NRDRX B ”OPMATE 80A1;
C                 “CTAHOBKA IK=1 ˆ IKMAX=72 <--- BCEƒ„A;
C   METO„:       O—EBˆ„EH
C   -----
C   APAMETP›:   KAR - MACCˆB BBE„EHHO‰ ˆH”OPMA–ˆˆ ( HE O‹EE 80 );
C   ---------    IK, IKMAX - ƒPAHˆ–› "O‹E‡HO‰" ˆH”OPMA–ˆˆ;
C                NRDRX - KAHA‹ BBO„A;
C                DSNAME - MACCˆB C DIM *(11) - ˆMŸ ”A‰‹A BBO„A
C                         „‹Ÿ POƒPAMM CE–. OC‹“†ˆBAHˆŸ;
C                KODEOF = 0  -  CTPOKA BBE„EHA;
C                       = 1  -  CˆT“A–ˆŸ END OF FILE HA NRDRX;
C                KFIZ = 1  - "AKT" BBO„A ”ˆ‡-KO‰ CTPOKˆ COBEP˜EH;
C                     = 0  - ”ˆ‡-K“ž CTPOK“ HE BBO„ˆ‹ˆ;
C
C   O„POƒPAMM›:   HET;
C   ------------
C   PˆME—AHˆE:   KOHTPO‹œ HE POBO„ˆTCŸ;
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION KAR(1), DSNAME(1)
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE (MASPRT(127), NZRDR)
       DATA KOD/0/, K80/80/
       KODEOF = 0
C******IF ( KOD .EQ. 1 ) GO TO 99
C----------------------------------------------------------------------
       READ(NRDRX,1, END=99 ) (KAR(I), I=1,K80)
       NZRDR = NZRDR + 1
1      FORMAT(80A1)
       IK = 1
       IKMAX = 72
       KFIZ = 1
       RETURN
99     CONTINUE
       KODEOF = 1
       KOD = 1
       RETURN
       END
