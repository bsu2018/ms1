         SUBROUTINE ENDLOG( LOGYTR )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   ENDLOG( LOGYTR )
C   ------------
C   HA�HA�EH�E:    HAXO�HEH�E � PA�METKA "�A�OB�X" ���OB BCEX
C   ----------     �H���EHTH�X �P���. HA MECTO "�A�OB�X" �A��C�BAETC�
C                  �P��HAK "-1" -  OCHOBHO� ��E�. H��EB�E ���� HE
C                  ��MEH��TC�;
C   METO�:         CKAH�POBAH�E LOGYTR, �P� LOGYTR(I) = I -> "�A�OB��";
C   -----
C   �O��PO�PAMM�:  1. COMMON PROF1
C   ------------
C   �P�ME�AH�E:    1. �P� FTEST >= 2 �E�AT� LOGYTR HA NWTRE
C   ----------        �OC�E �POBE�EH�� MO����KA���;
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION LOGYTR(1)
         EQUIVALENCE ( KNOT6M, MASPRT(58) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( FTEST,  MASPRT(60) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         INTEGER FTEST
         DO 100 I = KNOT6M, NSYS
         IF ( LOGYTR(I) .LE. 0 ) GO TO 100
C---> I - B ����E �H���EHTH�X
C    MO�ET �TO "�A�OB��" ?
         IF ( LOGYTR(I) .EQ. I ) LOGYTR(I) = -1
100      CONTINUE
         IF ( FTEST .LT. 2 ) RETURN
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 2 )
2        FORMAT(' ***FTEST_2: MACC�B K.�. ���OB C OTME�EHH�M� "�A�OB�M�"
     = ���AM� ***')
         WRITE(NWTRE, 1 ) ( I, LOGYTR(I), I=1, NSYS)
1        FORMAT(           5( 2X,   'LOG(', I4, ')=', I4 ) )
C!!!-------------------------------------------------------------------
         RETURN
         END
