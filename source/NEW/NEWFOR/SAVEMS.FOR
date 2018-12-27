       SUBROUTINE SAVEMS(MSWALL, TOK, VALL, KAR, DAT)
CXXX----KOMèãEKC "MS1" -->
C   èOÑèPOÉPAMMA: SAVEMS(MSWALL, TOK, VALL, KAR, DAT)
C   ------------
C   HAáHAóEHàE:   áÄèàëú çÄ 97 KAHAã  ( MS1_data.MSA ) ¨†··®¢Æ¢ ·¢Ôß®
C   -----------
C
C      áÄèàëõÇÄûíëü      áAèàCà --->
C
C 1-E áAèàCà -> <PROF1>, <SERVZ4>, <SERVZ5>, <SERVZ6> <SERVZ8>;
C
C 2-Ö áAèàCà -> MSWALL, TOKN, VALLN;
C
C   METOÑ:    óTEHàE ÑAHHõX C 97 KAHAãA;
C   -----
C   èAPAMETPõ:  CM. HAáHAóEHàE;
C  ----------
C   èOÑèPOÉPAMMõ:  1. COMMON_õ;
C   ------------
C
C  èPàMEóAHàE:     1.  îOPMAT îAâãA         --> ·¨. ¢ readmb+writmb
C  ----------
C
CZZZ-------------------------------------------------------------------
       PARAMETER (nf=97)
       COMMON /PROF1/ MASPRT(200)
       COMMON /SERVZ4/ NAMECR(80)
       COMMON /SERVZ5/ KCOM, COMENT(100)
       COMMON /SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
       COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
       COMMON /PIEZO/ TNL(200),TNC(200),UN0(200),UN1(200),
     * TJNL(200),TJNC(200),D11(200),D21(200)
C
       INTEGER NAMECR, COMENT, SPISON, DMSWAL
C
       DIMENSION MSWALL(1), TOK(1), VALL(1), KAR(1), DAT(1)
       INTEGER   TOK, VALL
C
       EQUIVALENCE ( NNTOK, MASPRT(46) )
       EQUIVALENCE ( NSYS,  MASPRT(26) )
       EQUIVALENCE ( LPRTM, MASPRT(54) )
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       EQUIVALENCE ( NWTR,  MASPRT(13) )
       EQUIVALENCE ( DMSWAL, MASPRT(20) )
       EQUIVALENCE ( NZAPSP, MASPRT(92) )
C-------
        OPEN(UNIT=nf,file='MS1_DATA.MSA',form='UNFORMATTED',
     =       ERR=666,IOSTAT=ii_rr)
C********
C-->  îAâã B HAóAãO.....
       REWIND nf
C--> HAóAãà....
C--> è®Ë•¨
       WRITE(nf       )    (MASPRT(I1), I1=1,LPRTM )
       WRITE(nf       ) (NAMECR(I2), I2=1,80  ),
     +                        KCOM,(COMENT(I3), I3=1,100 ),
     +                        TBEGON, TENDON, KOUTON,
     +                        ( SPISON(I4), I4=1,55 )
       WRITE(nf        ) SPISNN,
     +                         (SPISVL(I3), I3=1,16),
     +                         (SPISTK(I4), I4=1,16)
c     +  (TNL(J1),J1=1,200) ,(TNC(J2),J2=1,200),
c     +  (UN0(J3),J3=1,200) ,(UN1(J4),J4=1,200),
c     +  (TJNL(J5),J5=1,200) ,(TJNC(J6),J6=1,200),
c     +  (D11(J7),J7=1,200) ,(D21(J8),J8=1,200)
       CALL WRITMB( nf,   DMSWAL, MSWALL )
       CALL WRITMB( nf,   NNTOK, TOK )
       CALL WRITMB( nf,   NSYS, VALL )
       WRITE(nf        )
     +                         (KAR(I3), I3=1,80),
     +                         (DAT(I4), I4=1,21)
C----------------------------------------------------------------------
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       CLOSE (NF)
       RETURN
C----------------------------------------------------------------------
666    WRITE(NWTR, 104) ii_rr
       WRITE(NWTRE, 104)  ii_rr
104    FORMAT(/' *** éË®°™® Æ‚™‡Î‚®Ô ‰†©´† MS1_DATA.MSA ',
     =  ' IOSTAT =', I8, ' ***'/)
C+---------------------------------------------------------------------
       CLOSE (NF)
       CALL STOPP(666)
       RETURN
       END
