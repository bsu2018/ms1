         SUBROUTINE PRINT3( TOKN, VALLN )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  PRINT3( TOKN, VALLN )
C   ------------
C   HA�HA�EH�E:    KOHTPO��HA� �E�AT� "HA�A�" MACC�BOB TOKN � VALLN
C   ----------     B�BO� TAK �E: TN, TX, H0, H1, H2, FKOM, FLOG
C
C   METO�:
C   -----
C   �APAMETP�:     TOKN � VALLN - �HA�EH�E TOKOB � HA�P��EH��;
C   ---------
C
C   �O��PO�PAMM�: COMMON PROF1;
C  -------------
C
C  �P�ME�AH�E:     OT�A�O�HA� �E�AT� 9-T� �EPB�X �HA�EH��;
C  -----------     ( KPA�HE �E�O�PA�HA� )
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION TOKN(*), VALLN(*)
         INTEGER FKOM, FLOG
         EQUIVALENCE( MASPRT(29), TN ),
     =              ( MASPRT(30), H0 ),
     =              ( MASPRT(32), H1 ),
     =              ( MASPRT(33), H2 ),
     =              ( MASPRT(14), NWTRE),
     =              ( MASPRT(35), TX ),
     =              ( MASPRT(34), FKOM ),
     =              ( MASPRT(37), FLOG )
         NSYS = MASPRT(26)
         IF ( NSYS .GT. 9 ) NSYS = 9
         NNTOK = MASPRT(46)
         IF ( NNTOK .GT. 9 ) NNTOK = 9
         WRITE(NWTRE, 1) TN, TX, H0, H1, H2, FKOM, FLOG, NSYS, NNTOK
1        FORMAT(' TN=', E12.5, ' TX=', E12.5, ' H0=', E12.5,
     =          ' H1=', E12.5, ' H2=', E12.5, ' FKOM=', I2,
     =          ' FLOG=', I2, ' NSYS=', I5, ' NNTOK=', I5 )
         WRITE(NWTRE,2) ( TOKN(I), I=1,NNTOK)
2        FORMAT(1X, 'TOK= ', 9( 1X, G12.5 )  )
         WRITE(NWTRE,3) ( VALLN(I), I=1,NSYS)
3        FORMAT(1X,'VALL=',  9( 1X, G12.5 )  )
         RETURN
         END
