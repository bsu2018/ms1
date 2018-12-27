       SUBROUTINE DRSAVA(KAR, DAT )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: DRSAVA(KAR, DAT )
C   ------------
C
C   HA‡HA—EHˆE:      OPAOT—ˆK  #ACOXP  DTCHEK   NFACHK
C   ----------       *************************************
C
C              DTCHEK - ˆTEPBA‹ O BPEMEHˆ B›O‹HEHˆŸ ABTO_COXP.
C                       ( B CEK. KPATEH HOUTT )
C              NFACHK - KAHA‹ „‹Ÿ ABTO_COXP_PAHEHˆŸ
C
C
C   METO„:     PˆEM AP-B „ˆPEKTˆB› ˆ MO„ˆ”ˆKA–ˆŸ O‹E‰ B MSWALL
C   ------    *** Pˆ “CEXE --> BO‡“†„EHˆE ”‹AƒA "FCHEK" ****
C                                 -------------------------
C
C   APAMETP›: KAR, DAT - MACCˆB› CTPOKˆ ˆ „AHH›X „ˆPEKTˆB›;
C   ---------
C
C   O„POƒPAMM›: 1. PROF1
C   -------------
C
C   PˆME—AHˆE:  1. "“MO‹—AHˆE" HA HOMEP KAHA‹A ˆ‡ MASPRT(97
C   ----------       NFACHK = 11, ˆHTEPBA‹ O TN = DTCHEK ˆ‡
C                      MASPRT(90) -->  0.5  CEK;
C
C                2.  !! BHˆMAHˆE DTCHEK „O‹†HO ›Tœ
C                      KPATHO HOUTT !!!
C                3. B DIGITA -->  BO‡MO†HO ˆCPAB‹EHˆE;
C                   -----------------------------------
C
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( DTCHEK, MASPRT(90) )
       EQUIVALENCE( FCHEK,  MASPRT(91) )
       EQUIVALENCE( NFACHK, MASPRT(97) )
       EQUIVALENCE( NWTR,   MASPRT(13) )
       EQUIVALENCE( NWTRE,  MASPRT(14) )
       EQUIVALENCE( NRDR,   MASPRT(12) )
       EQUIVALENCE( NSPOOL, MASPRT(18) )
       EQUIVALENCE( NMAK,   MASPRT(89) )
       EQUIVALENCE( NWTRD,  MASPRT(16) )
       EQUIVALENCE( NFILER, MASPRT(83) )
       INTEGER FCHEK
       DIMENSION KAR(*), DAT(*)
C----------------------------------------------------------------------
       KKK = IFIX( DAT(1) )
       IF ( KKK .EQ. 0 ) GO TO 100
C-->  NEW  DTCHEK
       DTCHEK = DAT(2)
       IF ( KKK .EQ. 1 ) GO TO 100
C-->  HOB›‰ ”A‰‹..
       NF = IFIX( DAT(3) )
C-->  KOHTPO‹œ ”A‰‹A ....
       IF ( NF .EQ. NWTR  .OR.
     =      NF .EQ. NWTRE   .OR.
     =      NF .EQ. NRDR    .OR.
     =      NF .EQ. NSPOOL  .OR.
     =      NF .EQ. NMAK    .OR.
     =      NF .EQ. NWTRD   .OR.
     =      NF .EQ. NFILER  )
     =                                 NF = NFACHK
C----------------------------------------------------------------------
       NFACHK = NF
100    CONTINUE
       WRITE(NWTR, 500) DTCHEK, NFACHK
500    FORMAT(/' *** #ACOXP PˆHŸTA:==> ABTO_COXPAHEHˆE —EPE‡',
     = ' ', G12.5, ' CEK. KAHA‹ # ', I2, ' ***'/)
C
       FCHEK = 1
       RETURN
       END
