       SUBROUTINE RSCHEK( NF, TN, MSWALL, TOK, VALL, IRC)
CXXX----KOM‹EKC "MS1" -->
C   O„POƒPAMMA: RSCHEK( NF, TN, MSWALL, TOK, VALL, IRC )
C   ------------
C   HA‡HA—EHˆE:   —TEHˆE C  NF KAHA‹A ( ˆ‡ HA—A‹A ”A‰‹A ) K.T. PE˜EHˆŸ
C   -----------
C                      ˆCO‹HˆTE‹œH›‰ MEXAHˆ‡M „ˆPEKTˆB› #BOCT
C                      ----------------------------------------
C
C      BOCTAHAB‹ˆBAžTCŸ  ‡AˆCˆ --->
C
C 1-E ‡AˆCˆ -> TN, <PROF1>, <SERVZ4>, <SERVZ5>, <SERVZ6> <SERVZ8>;
C
C 2-… ‡AˆCˆ -> MSWALL, TOKN, VALLN;
C
C   METO„:    —TEHˆE „AHH›X C NF KAHA‹A;
C   -----
C   APAMETP›:  CM. HA‡HA—EHˆE; TN - BPEMŸ MO„E‹ˆPOBAHˆŸ, Pˆ KOTOPOM
C  ----------    ”OPMˆP“ETCŸ K.T.
C
C                      IRC = 0 -->  “CE˜HOE ‡ABEP˜EHˆE;
C                      IRC = 1 -->  PO—ˆTATœ HE “„A‹OCœ :
C                               KO„=X01  -> END _OF_FILE
C                               KO„=X02  -> ERROS_TO_READ;
C                                   X - HOMEP ‡AˆCˆ K.T.;
C
C   O„POƒPAMM›:  1. COMMON_›;
C   ------------
C
C  PˆME—AHˆE:     1.  ”OPMAT ”A‰‹A „‹Ÿ K.T --> á¬. ¢ readmb+writmb
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
C
C-->  1-Ÿ ‡AˆCœ
C      KXX = 100
C      READ(NF,END=50,ERR=51)    TN, (MASPRT(I1), I1=1,LPRTM ),
C    =               (NAMECR(I2), I2=1,80    ),
C    =     KCOM,     (COMENT(I3), I3=1,100   ),
C    =    TBEGON, TENDON, KOUTON, ( SPISON(I4), I4=1,55 )
C--> 2-Ÿ ‡AˆCœ
C      KXX = 200
C      READ(NF,END=50,ERR=51) (MSWALL(I1), I1=1, DMSWAL ),
C    =           ( TOK(I2),  I2=1, NNTOK  ),
C    =           ( VALL(I3), I3=1, NSYS   )
C----------------------------------------------------------------------
C--> —¨â ¥¬
       KXX = 100
       READ(NF, END=50,ERR=51) TN,(MASPRT(I1), I1=1,LPRTM )
       KXX = 200
       READ(NF, END=50,ERR=51) (NAMECR(I2), I2=1,80  ),
     +                        KCOM,(COMENT(I3), I3=1,100 ),
     +                        TBEGON, TENDON, KOUTON,
     +                        ( SPISON(I4), I4=1,55 )
       KXX = 300
       READ(NF, END=50,ERR=51) SPISNN,
     +                         (SPISVL(I3), I3=1,16),
     +                         (SPISTK(I4), I4=1,16)
       KXX = 66600
       CALL READMB( NF,   DMSWAL, MSWALL, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
       CALL READMB( NF,   NNTOK,  TOK, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
       CALL READMB( NF,   NSYS, VALL, IRC )
       IF ( IRC .EQ. 1 ) GO TO 50
       IF ( IRC .EQ. 2 ) GO TO 51
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       WRITE(NWTRE, 101 ) TN, NZAPSP
C$$
       IRC = 0
       CLOSE (NF)
       RETURN
100    FORMAT(/' *** C«ã¦¡  "K.T." -> #BOCT_ ­®¢«¥­¨¥ á ª ­ «  #',
     =         I3,' ***')
101    FORMAT(' *** C‹“†A "K.T." -> "K.T." ¢®áâ ­®¢«¥­   ( tn=',G12.5,
     = ' NZAPSP=', I6,' ) ***'/)
C----------------------------------------------------------------------
50     CONTINUE
       KX = 1 + KXX
       GO TO 52
51     CONTINUE
       KX = 2 + KXX
52     WRITE(NWTR, 103) NF, KX
       WRITE(NWTRE, 103) NF, KX
103    FORMAT(/' *** B®áâ ­®¢«¥­¨¥ á ª ­ «  # ', I2, ' ­¥¢®§¬®¦­®',
     =  ' KO„=', I8, ' ***'/)
C+---------------------------------------------------------------------
       IRC = 1
       CLOSE (NF)
       RETURN
       END
