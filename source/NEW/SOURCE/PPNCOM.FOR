       SUBROUTINE PPNCOM(KOD, NWTR )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: PPNCOM(KOD, NWTR )
C   -------------
C   HA�HA�EH�E:  �E�AT� �O KAHA�� NWTR, B �AB�C�MOCT� OT KOD:
C  -----------   KOD = 1 - �MEH� �E�� (#�M�);
C                    = 2 - KOMEHTAP�EB (#�P�M);
C                    = 3 - �MEH� �E�� � KOMEHTAP�EB;
C   METO�:       O�EB��EH;
C  -------
C   �APAMETP�:   CM. B��E;
C   ---------
C   �O��PO�PAMM� 1. COMMON SERVZ4, SERVZ5, PROF1
C  -------------
C   �P�ME�AH�E:  HET
C   -----------
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       COMMON/ SERVZ4/ NAMECR(80)
       COMMON/ SERVZ5/ KCOM, COMENT(100)
       INTEGER COMENT
       GO TO (1,2,3),KOD
       CALL SUBERR(KOD)
1      WRITE(NWTR,10)   ( NAMECR(I), I=1,72 )
10     FORMAT(/1X,'CXEMA: ', 72A1 /)
       RETURN
2      CONTINUE
       IF ( KCOM .EQ. 0 ) RETURN
       DO 20 IP=1,KCOM
       I1 = (IP-1)*20+1
       I2 = I1+17
       IF( MASPRT(15) .EQ. 0 ) WRITE(NWTR,21)(COMENT(I), I=I1,I2 )
       IF( MASPRT(15) .EQ. 1 ) WRITE(NWTR,22)(COMENT(I), I=I1,I2 )
21     FORMAT(1X, 20A4)
22     FORMAT(    20A4)
20     CONTINUE
       RETURN
3      CONTINUE
       WRITE(NWTR,10) (NAMECR(I), I=1, 72 )
       GO TO 2
       END
