         SUBROUTINE PRINT0(BA0, KNOT, PAR0)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C    �O��PO�PAMMA:  PRINT0(BA0, KNOT, PAR0)
C   -------------
C    HA�HA�EH�E: KOHTPO��HA� �E�AT� MACC�BOB BA0,[ KNOT, PAR0 ]
C    ----------                                  $ ��OK       $
C   �P�ME�AH�E: 1. PA�O��� BAP�AHT--> OT�A�O�HA� �E�AT�;
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
1        FORMAT(10X,'MACC�B BA0')
         WRITE(NWTR,2) ( I, BA0(I), I=1, 5   )
2        FORMAT( 5( 2X,'BA0(', I4, ')=', I11 ) )
       WRITE(NWTR, 20) ( I, BA0(I), I=6, IMBA0 )
20     FORMAT( 8(1X, 'B(', I3, ')=', I7 ) )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          RETURN
C        WRITE(NWTR,3)
C3        FORMAT(10X,'MACC�B KNOT')
C        WRITE(NWTR,4) ( I, KNOT(I), I=1,IMKNOT)
C4        FORMAT( 5( 2X,'KNOT(', I4, ')=', I11 ) )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        WRITE(NWTR,5)
C5        FORMAT(10X,'MACC�B PAR0')
C        WRITE(NWTR,6) ( I, PAR0(I), I=1,IMPAR0)
C6        FORMAT( 5( 1X,'PAR0(', I4, ')=', G12.4 )  )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C        RETURN
         END
