         SUBROUTINE PRINT4(      YALL, JALL, LOGYTR )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:  PRINT4(       YALL, JALL, LOGYTR )
C   ------------
C   HAáHAóEHàE:    KOHTPOãúHAü èEóATú MAPàñõ YALL à BEKTOPA JALL
C   ----------     à BEKTOPA LOGYTR;
C
C   METOÑ:
C   -----
C   èAPAMETPõ:     CM. HAáHAóEHàE
C   ---------
C
C   èOÑèPOÉPAMMõ: COMMON PROF1;
C  -------------
C
C  èPàMEóAHàE:     OTãAÑOóHAü èEóATú.
C  -----------     ( KPAâHE ÅEáOÅPAáHAü )
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION YALL(1), JALL(1), LOGYTR(1)
         REAL JALL
         EQUIVALENCE( MASPRT(29), TN ),
     =              ( MASPRT(30), H0 ),
     =              ( MASPRT(32), H1 ),
     =              ( MASPRT(33), H2 ),
     =              ( MASPRT(14), NWTRE),
     =              ( MASPRT(26), NSYS),
     =              ( MASPRT(23), NDALL )
C
         ID(I,J) = (J-1)*NDALL + I
C!!!-------------------------------------------------------------------
       WRITE(NWTRE,1)
1      FORMAT(/)
11       FORMAT( 5 (1X, 'Y(', I3, ',', I3, ')=', G12.5 ))
         DO 2 I =1,NSYS
         WRITE(NWTRE,11) ( I,J, YALL( ID(I,J) ), J=1,NSYS)
         WRITE(NWTRE,1)
2        CONTINUE
         WRITE(NWTRE,1)
         WRITE(NWTRE,13) ( I, JALL(I), I=1,NSYS )
         WRITE(NWTRE,1)
         WRITE(NWTRE,12) ( I, LOGYTR(I), I=1,NSYS)
         WRITE(NWTRE,1)
13       FORMAT( 5( 1X, 'J(', I3, ')=', G12.5 ))
12       FORMAT( 5 (1X, 'LOG(', I3, ')=', I5 ) )
C!!!-------------------------------------------------------------------
         RETURN
         END
