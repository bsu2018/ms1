       SUBROUTINE READ_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IRC )
CXXX----KOMèãEKC "MS1" --> BEPCàü 2.0  (OT ÑEKAÅPü 1990 É.)------------
C   èOÑèPOÉPAMMA: READ_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IRC )
C   ------------
C   HAáHAóEHàE:   óíÖçàÖ ë NF1 à NF2 KAHAãéÇ  "ÇëÖÉé"
C   -----------
C
C              IRC = 0 --> ìëèÖòçéÖ áÄÇÖêòÖçàÖ
C              IRC = 1 --> ÇëíêÖíàãëü äéçÖñ
C              IRC = 2 --> éòàÅäà
C
C   èOÑèPOÉPAMMõ:  1. COMMON_õ;
C   ------------
C
C  èPàMEóAHàE:
C  ----------
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
       IRC = 0
C--> HAóAãà....
C---------------------------------------------------------------------
C--> èÖêÖåéíÄÖå  NF1
         REWIND NF1
C--> ó®‚†•¨    
       READ(NF1,END=50,ERR=51) (MASPRT(I1), I1=1,200 )
       READ(NF1,END=50,ERR=51) (NAMECR(I2), I2=1,80  ),
     +                        KCOM,(COMENT(I3), I3=1,100 ),
     +                        TBEGON, TENDON, KOUTON,
     +                        ( SPISON(I4), I4=1,55 )
       READ(NF1,END=50,ERR=51) SPISNN,                 
     +                         (SPISVL(I3), I3=1,16),
     +                         (SPISTK(I4), I4=1,16)
       CALL READMB( NF1 , DMSWAL, MSWALL, IRC )
       IF( IRC .GT. 0 ) RETURN
C---------------------------------------------------------------------
C--> èÖêÖåéíÄÖå  NF2
         REWIND NF2
C--> áAèàCõÇÄÖå
        CALL READMB( NF2, NNTOK, TOK,  IRC )
       IF( IRC .GT. 0 ) RETURN
        CALL READMB( NF2, NSYS , VALL, IRC )
       IF( IRC .GT. 0 ) RETURN
C----------------------------------------------------------------------
       RETURN
50     CONTINUE
       IRC = 1
       RETURN
51     CONTINUE
       IRC = 2
       RETURN
       END
