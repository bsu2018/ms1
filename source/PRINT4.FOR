         SUBROUTINE PRINT4(      YALL, JALL, LOGYTR )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  PRINT4(       YALL, JALL, LOGYTR )
C   ------------
C   HA�HA�EH�E:    KOHTPO��HA� �E�AT� MAP��� YALL � BEKTOPA JALL
C   ----------     � BEKTOPA LOGYTR;
C
C   METO�:
C   -----
C   �APAMETP�:     CM. HA�HA�EH�E
C   ---------
C
C   �O��PO�PAMM�: COMMON PROF1;
C  -------------
C
C  �P�ME�AH�E:     OT�A�O�HA� �E�AT�.
C  -----------     ( KPA�HE �E�O�PA�HA� )
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