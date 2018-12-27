       SUBROUTINE DRSAV0(KAR, DAT, MSWALL, TOKN, VALLN )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA: DRSAV0(KAR, DAT, MSWALL, TOKN, VALLN )
C   ------------
C
C   HAáHAóEHàE:       OÅPAÅOTóàK  #COXP  NFCHEK
C   ----------        **************************
C
C                      NFCHEK - KAHAã COXPAHEHàü;
C
C   METOÑ:     èPàEM èAP-B ÑàPEKTàBõ à BõáOB è/è SVCHEK;
C   ------
C
C   èAPAMETPõ: KAR, DAT - MACCàBõ CTPOKà à ÑAHHõX ÑàPEKTàBõ;
C   ---------  MSWALL, TOKN, VALLN - "TOóKA" PEòEHàü;
C
C   èOÑèPOÉPAMMõ: 1. PROF1, SVCHEK
C   -------------
C
C   èPàMEóAHàE:  1. "ìMOãóAHàE" HA HOMEP KAHAãA àá MASPRT(95)
C   ----------       NFCHEK = 10
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( DTCHEK, MASPRT(90) )
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
       INTEGER FCHEK
       DIMENSION KAR(*), DAT(*), MSWALL(*), TOKN(*), VALLN(*)
C----------------------------------------------------------------------
       KKK = IFIX( DAT(1) )
       IF ( KKK .EQ. 0 ) GO TO 100
C-->  HOBõâ îAâã..
       NF = IFIX( DAT(2) )
C-->  KOHTPOãú îAâãA ....
       IF ( NF .EQ. NWTR  .OR.
     =      NF .EQ. NWTRE   .OR.
     =      NF .EQ. NRDR    .OR.
     =      NF .EQ. NSPOOL  .OR.
     =      NF .EQ. NMAK    .OR.
     =      NF .EQ. NWTRD   .OR.
     =      NF .EQ. NFILER  )
     =                                 NF = NFCHEK
C----------------------------------------------------------------------
       NFCHEK = NF
100    CONTINUE
       WRITE(NWTR, 500)         NFCHEK
500    FORMAT(/' *** #COXP èPàHüTA:==> KAHAã îAâãA COXPAHEHàü ', I3,
     = ' ***'/)
C
       CALL SVCHEK( NFCHEK, TN, MSWALL, TOKN, VALLN )
C
       CALL PPNCOM( 3, NWTR )
       WRITE(NWTR,501) TN, NFCHEK, NZAPSP
501    FORMAT(/' *** COXPAHEHO:=> TN=',G12.5,' CEK., KAHAã # ', I2,
     =    ', áAèàCú B NSPOOL ', I6,' ***'/)
C----------------------------------------------------------------------
       RETURN
       END
