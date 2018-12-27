         SUBROUTINE PRINT0(BA0, KNOT, PAR0)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C    èOÑèPOÉPAMMA:  PRINT0(BA0, KNOT, PAR0)
C   -------------
C    HAáHAóEHàE: KOHTPOãúHAü èEóATú MACCàBOB BA0,[ KNOT, PAR0 ]
C    ----------                                  $ ÅãOK       $
C   èPàMEóAHàE: 1. PAÅOóàâ BAPàAHT--> OTãAÑOóHAü èEóATú;
C   ----------
CZZZ-------------------------------------------------------------------
        DIMENSION BA0(*), KNOT(*), PAR0(*)
         COMMON/PROF1/ MASPRT(200)
         COMMON/PROF2/ MACTAB(1)
         COMMON/PROF3/ FUNTAB(1)
         INTEGER BA0, FUNTAB
         NWTR = MASPRT(14)
C======================================================================
         IMBA0 = BA0(3)  + 10
         IMKNOT = KNOT(3)+ 10
         IMPAR0 = PAR0(3)+ 10
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         WRITE(NWTR,1)
1        FORMAT(10X,'MACCàB BA0')
         WRITE(NWTR,2) ( I, BA0(I), I=1, 5   )
2        FORMAT( 5( 2X,'BA0(', I4, ')=', I11 ) )
       WRITE(NWTR, 20) ( I, BA0(I), I=6, IMBA0 )
20     FORMAT( 8(1X, 'B(', I3, ')=', I7 ) )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          RETURN
C        WRITE(NWTR,3)
C3        FORMAT(10X,'MACCàB KNOT')
C        WRITE(NWTR,4) ( I, KNOT(I), I=1,IMKNOT)
C4        FORMAT( 5( 2X,'KNOT(', I4, ')=', I11 ) )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        WRITE(NWTR,5)
C5        FORMAT(10X,'MACCàB PAR0')
C        WRITE(NWTR,6) ( I, PAR0(I), I=1,IMPAR0)
C6        FORMAT( 5( 1X,'PAR0(', I4, ')=', G12.4 )  )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        RETURN
         END
