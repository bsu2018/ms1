       SUBROUTINE SVCHEK( NF, TN, MSWALL, TOK, VALL )
CXXX----KOM‹EKC "MS1" -->
C   O„POƒPAMMA: SVCHEK( NF, TN, MSWALL, TOK, VALL )
C   ------------
C   HA‡HA—EHˆE:   ‡AˆCœ HA NF KAHA‹ ( B HA—A‹O ”A‰‹A ) K.T. PE˜EHˆŸ
C   -----------
C                      ˆCO‹HˆTE‹œH›‰ MEXAHˆ‡M „ˆPEKTˆB› #COXP
C                      ----------------------------------------
C
C      COXPAHŸžTCŸ 2- ‡AˆCˆ----->
C
C 1-E ‡AˆCˆ-> TN, <PROF1>, <SERVZ4>, <SERVZ5>, <SERVZ6> <SERVZ8>;
C
C 2-E ‡AˆCˆ -> MSWALL, TOKN, VALLN;
C
C   METO„:    ‡AˆCœ „AHH›X HA NF KAHA‹;
C   -----
C   APAMETP›:  CM. HA‡HA—EHˆE; TN - BPEMŸ MO„E‹ˆPOBAHˆŸ, Pˆ KOTOPOM
C  ----------    ”OPMˆP“ETCŸ K.T.
C   O„POƒPAMM›:  1. COMMON_›;
C   ------------   2. MSGCON ( BHE ”OPT. PGM );
C
C  PˆME—AHˆE:     1.  ”OPMAT ”A‰‹A „‹Ÿ K.T --> "VBS" .
C  ----------
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /SERVZ4/ NAMECR(80)
       COMMON /SERVZ5/ KCOM, COMENT(100)
       COMMON /SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
       COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
C
       INTEGER NAMECR, COMENT, SPISON, DMSWAL
C
       DIMENSION MSWALL(1), TOK(1), VALL(1)
       INTEGER  TOK, VALL
C
       EQUIVALENCE ( NNTOK, MASPRT(46) )
       EQUIVALENCE ( NSYS,  MASPRT(26) )
       EQUIVALENCE ( LPRTM, MASPRT(54) )
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       EQUIVALENCE ( NWTR,  MASPRT(13) )
       EQUIVALENCE ( DMSWAL, MASPRT(20) )
       EQUIVALENCE ( NZAPSP, MASPRT(92) )
         CALL KSOPEN(NF, 1,i_rc)
         IF( i_rc .NE. 0 ) CALL STOPP(20)
C********
C-->  ”A‰‹ B HA—A‹O.....
       REWIND NF
C--> HA—A‹ˆ....
       WRITE(NWTRE, 100 ) NF
       CALL MSGCON(1)
C-->  1-Ÿ ‡AˆCœ
C      WRITE(NF) TN, (MASPRT(I1), I1=1,LPRTM ),
C    =               (NAMECR(I2), I2=1,80    ),
C    =     KCOM,     (COMENT(I3), I3=1,100   ),
C    =    TBEGON, TENDON, KOUTON, ( SPISON(I4), I4=1,55 )
C--> 2-Ÿ ‡AˆCœ
C      WRITE(NF) (MSWALL(I1),  I1=1, DMSWAL ),
C    =           ( TOK(I2),  I2=1, NNTOK  ),
C    =           ( VALL(I3), I3=1, NSYS   )
C----------------------------------------------------------------------
       WRITE(NF)  TN,           (MASPRT(I1), I1=1,LPRTM )
       WRITE(NF)                (NAMECR(I2), I2=1,80  ),
     +                        KCOM,(COMENT(I3), I3=1,100 ),
     +                        TBEGON, TENDON, KOUTON,
     +                        ( SPISON(I4), I4=1,55 )
       WRITE(NF)          SPISNN,
     +                         (SPISVL(I3), I3=1,16),
     +                         (SPISTK(I4), I4=1,16)
       CALL WRITMB( NF,   DMSWAL, MSWALL )
       CALL WRITMB( NF,   NNTOK,  TOK    )
       CALL WRITMB( NF,   NSYS,   VALL   )
C----------------------------------------------------------------------
C----------------------------------------------------------------------
       END FILE NF
       CLOSE (NF)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       WRITE(NWTRE, 101 ) TN, NZAPSP
       CALL MSGCON(2)
       RETURN
100    FORMAT( ' *** C«ã¦¡  "K.T." -> #COXP_ ­¥­¨¥ ­  ª ­ «¥ #',
     =         I3, 21X,   ' ***')
101    FORMAT(' *** C«ã¦¡  "K.T." -> "K.T." á®§¤ ­  ( tn=',G12.5,
     = ' NZAPSP=', I6,' ) ***' )
       END
