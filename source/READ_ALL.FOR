       SUBROUTINE READ_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IRC )
CXXX----KOMEKC "MS1" --> BEPC 2.0  (OT EKAP 1990 .)------------
C   OPOPAMMA: READ_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IRC )
C   ------------
C   HAHAEHE:     NF1  NF2 KAHA  ""
C   -----------
C
C              IRC = 0 -->  
C              IRC = 1 -->  
C              IRC = 2 --> 
C
C   OPOPAMM:  1. COMMON_;
C   ------------
C
C  PMEAHE:
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
C--> HAA....
C---------------------------------------------------------------------
C-->   NF1
         REWIND NF1
C--> ¨β ₯¬    
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
C-->   NF2
         REWIND NF2
C--> AC
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