       SUBROUTINE SAVE_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 2.0  (OT ÑEKAÅPü 1990 É.)------------
C   èOÑèPOÉPAMMA: SAVE_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IW )
C   ------------
C   HAáHAóEHàE:
C   -----------  èêà  IW = 0
C                     áAèàCú HA NF1  KAHAã ( B HAóAãO îAâãA ) "ÇëÖÉé"
C                     áAèàCú HA NF2  KAHAã TOK  à  VALL
C
C                èêà  IW = 1
C                     áAèàCú HA NF2  KAHAã TOK  à  VALL
C
C
C   METOÑ:    áAèàCú ÑAHHõX HA NF1 à  NF2 äAHAãõ;
C   -----
C   èOÑèPOÉPAMMõ:  1. COMMON_õ;
C   ------------
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /SERVZ4/ NAMECR(80)
       COMMON /SERVZ5/ KCOM, COMENT(100)
       COMMON /SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
       COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)  
C
       INTEGER NAMECR, COMENT, SPISON, DMSWAL, NNTOK, NSYS
C
       REAL MSWALL(*), TOK(*), VALL(*), DAT(*)
C
       EQUIVALENCE ( NNTOK, MASPRT(46) )
       EQUIVALENCE ( NSYS,  MASPRT(26) )
       EQUIVALENCE ( DMSWAL, MASPRT(20) )
C--> HAóAãà....
       IF( IW .EQ. 1 ) GO TO  10
C---------------------------------------------------------------------
C--> èÖêÖåéíÄÖå  NF1
         REWIND NF1
C--> áAèàCõÇÄÖå
       WRITE(NF1) (MASPRT(I1), I1=1,200 )
       WRITE(NF1) (NAMECR(I2), I2=1,80  ),
     +            KCOM,(COMENT(I3), I3=1,100 ),
     +            TBEGON, TENDON, KOUTON,
     +            ( SPISON(I4), I4=1,55 )
       WRITE(NF1) SPISNN,(SPISVL(I2), I2=1,16),
     +                   (SPISTK(I3), I3=1,16) 
        CALL WRITMB( NF1 , DMSWAL, MSWALL )
10      CONTINUE
C---------------------------------------------------------------------
C--> èÖêÖåéíÄÖå  NF2
         REWIND NF2
C--> áAèàCõÇÄÖå
        CALL WRITMB( NF2, NNTOK, TOK  )
        CALL WRITMB( NF2, NSYS , VALL )
C----------------------------------------------------------------------
       RETURN
       END
