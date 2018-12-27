       SUBROUTINE DRRES0(KAR, DAT, MSWALL, TOKN, VALLN )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: DRRES0(KAR, DAT, MSWALL, TOKN, VALLN )
C   ------------
C
C   HA‡HA—EHˆE:       OPAOT—ˆK  #BOCT  NFCHEK
C   ----------        **************************
C
C                      NFCHEK - KAHA‹ COXPAHEHˆŸ;
C
C   METO„:     PˆEM AP-B „ˆPEKTˆB› ˆ B›‡OB / SVREST;
C   ------     Pˆ “CE˜HOM BOCT_AHOB‹EHˆˆ ”‹Aƒ "FRESTE = 1"
C
C   APAMETP›: KAR, DAT - MACCˆB› CTPOKˆ ˆ „AHH›X „ˆPEKTˆB›;
C   ---------  MSWALL, TOKN, VALLN - "TO—KA" PE˜EHˆŸ;
C
C   O„POƒPAMM›: 1. PROF1, RSCHEK
C   -------------
C
C   PˆME—AHˆE:  1. "“MO‹—AHˆE" HA HOMEP KAHA‹A ˆ‡ MASPRT(95)
C   ----------       NFCHEK = 10
C   @@@ OT 03.03.87 -> OPž.
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( DTCHEK, MASPRT(90) )
       EQUIVALENCE( FRESTE, MASPRT(98) )
C                   ********************
       EQUIVALENCE( NFCHEK, MASPRT(95) )
       EQUIVALENCE( NZAPSP, MASPRT(92) )
       EQUIVALENCE(  TN,    MASPRT(29) )
       EQUIVALENCE( FCHEK,  MASPRT(91) )
       EQUIVALENCE( NFACHK, MASPRT(97) )
       EQUIVALENCE( NWTR,   MASPRT(13) )
       EQUIVALENCE( NWTRE,  MASPRT(14) )
       EQUIVALENCE( NRDR,   MASPRT(12) )
       EQUIVALENCE( NSPOOL, MASPRT(18) )
       EQUIVALENCE( NMAK,   MASPRT(89) )
       EQUIVALENCE( NWTRD,  MASPRT(16) )
       EQUIVALENCE( NFILER, MASPRT(83) )
       INTEGER FCHEK, FRESTE
       DIMENSION KAR(*), DAT(*), MSWALL(*), TOKN(*), VALLN(*)
C----------------------------------------------------------------------
       NF = NFCHEK
       KKK = IFIX( DAT(1) )
       IF ( KKK .EQ. 0 ) GO TO 100
C-->  HOB›‰ ”A‰‹..
       NF = IFIX( DAT(2) )
C-->  KOHTPO‹œ ”A‰‹A ....
       IF ( NF .EQ. NWTR  .OR.
     =      NF .EQ. NWTRE   .OR.
     =      NF .EQ. NRDR    .OR.
     =      NF .EQ. NSPOOL  .OR.
     =      NF .EQ. NMAK    .OR.
     =      NF .EQ. NWTRD   .OR.
     =      NF .EQ. NFILER  )
     =                                 NF = NFCHEK
C----------------------------------------------------------------------
C@@@@@ NFCHEK = NF
100    CONTINUE
       WRITE(NWTR, 500)         NF
500    FORMAT(/' *** #BOCT ¯à¨­ïâ :==> ª ­ « ä ©«  ¢®áâ ­®¢«¥­¨ï ', I3,
     = ' ***'/)
C@@@@@@NFOLD = NFCHEK
       CALL RSCHEK( NF,     TN, MSWALL, TOKN, VALLN, IRC )
C @@@
       IF ( IRC .NE. 0 ) GO TO 666
CC-->  BOCTAHOBˆ‹ˆ --->
       FRESTE = 1
       CALL PPNCOM( 3, NWTR )
       WRITE(NWTR,501) TN, NF,     NZAPSP
501    FORMAT(/' *** BOCTAHOB‹EHO:=> TN=',G12.5,' CEK., KAHA‹ # ', I2,
     =    ', ‡AˆCœ B NSPOOL ', I6,' ***'/)
C----------------------------------------------------------------------
       RETURN
666    CONTINUE
C-->  HE CMOƒ‹ˆ...
       FRESTE = 0
       WRITE(NWTR, 505)  NF
       WRITE(NWTRE, 505)  NF
505    FORMAT(/' *** BOCTAHOB‹EHˆE C KAHA‹A #',I3,' HE BO‡MO†HO ***'/)
       RETURN
       END
