       SUBROUTINE SAVE_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IW )
CXXX----KOMEKC "MS1" --> BEPC 2.0  (OT EKAP 1990 .)------------
C   OPOPAMMA: SAVE_ALL( NF1,NF2,NSXEM,DAT,MSWALL,TOK,VALL,IW )
C   ------------
C   HAHAEHE:
C   -----------    IW = 0
C                     AC HA NF1  KAHA ( B HAAO AA ) ""
C                     AC HA NF2  KAHA TOK    VALL
C
C                  IW = 1
C                     AC HA NF2  KAHA TOK    VALL
C
C
C   METO:    AC AHHX HA NF1   NF2 AHA;
C   -----
C   OPOPAMM:  1. COMMON_;
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
C--> HAA....
       IF( IW .EQ. 1 ) GO TO  10
C---------------------------------------------------------------------
C-->   NF1
         REWIND NF1
C--> AC
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
C-->   NF2
         REWIND NF2
C--> AC
        CALL WRITMB( NF2, NNTOK, TOK  )
        CALL WRITMB( NF2, NSYS , VALL )
C----------------------------------------------------------------------
       RETURN
       END
