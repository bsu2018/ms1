         SUBROUTINE SUBERR(IDUMP)
CXXX ---KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)-------------
C      �O��PO�PAMMA SUBERR(IDUMP)
C      ------------
C      HA�HA�EH�E: OTPA�OTKA C�O� �PO�PAMMHO�O KOM��EKCA.
C      ----------
C      METO�:      ��MEH�ETC� B �PO�ECCE OT�A�K�.
C      -----
C      �APAMETP�:  DUMP - �POC��A B��A�� O�. � T.�.
C      ---------
C                   �P� IDUMP = 0 "�AM�" HE B�BO��TC�!
C
C   �O��PO�PAMM�: ERRTRA - C��. �OPT. ��M.
C   ------------
C
C
C      �P�ME�AH�E: OT�A�O�HA� �PO�AMMA;
C      ----------
CZZZ -------------------------------------------------------------------
C   V.M.=2.0
C
       I = 10
       IF( IDUMP .NE. 0 ) I = IDUMP
       WRITE(6,666) IDUMP
666    FORMAT(/1X,'***> "SUBERR" :=> IDUMP = ', I10,'       <***' )
       WRITE(6,667)
667    FORMAT( ' ***> �A�POC K "SUBERR" �O �E�O�KE B��OBOB: <***'/)
C----------------------------------------------------------------------
C      CALL ERRTRA
C----------------------------------------------------------------------
         IF ( IDUMP .EQ. 0 ) GO TO 16
        J=5/(IDUMP-I)
16      CALL STOPP(16)
        END
