         SUBROUTINE MODVAL( VALLN1, LOGYTR )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�AMMA:    MODVAL( VALLN1, LOGYTR )
C   -----------
C   HA�HA�EH�E:   - "PACTAHOBKA" ���OB�X HA�P��EH�� CO��ACHO
C   ----------      �H���EHTH�X ���OB �O �AHH�M MACC�BA LOGYTR.
C                   �H�OPMA��� O PE�EH�� "TEHEBO�" C�CTEM�
C                   �O��HA ��T� �C�O���OBAHA � COXPAHEHA -> TEP�ETC� !!!
C
C   METO�:         CO��ACHO HA�HA�EH�� � CTP�KT�P� LOGYTR:
C   -----          EC�� LOGYTR(K) > 0, TO VALLN1(K) = VALLN1(LOGYTR(K))
C                  EC�� LOGYTR(K) = 0, TO VALLN1(K) = 0.0
C                  EC�� LOGYTR(K) = -1, TO VALLN1(K) HE ��MEH�ETC�;
C   �APAMETP�:
C   ----------     VALLN1 - BEKTOP ���OB�X HA�P��EH��;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 6;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C   �O��PO�PAMM�: 1. COMMON PROF1
C   ------------
C   �P�ME�AH�E: 1. KOHTPO�� HE �PO��BO��TC�;
C   -----------
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION  VALLN1(1),  LOGYTR(1)
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(26), NSYS   )
         DO 100  K = 1, NSYS
         M = LOGYTR(K)
         IF ( M. LE. -1 ) GO TO 100
         IF ( M .NE. 0 ) GO TO 50
C-->  K �H���EHTEH C H��EB�M ���OM �E��:
         VALLN1(K) = 0.0
         GO TO 100
50       CONTINUE
C        K - HOMEP ���A, HA�P��EH�E HA KOTOPOM PABHO HA�P��EH��
C        PAC��TAHHOM� ��� ���A  M...�P� PE�EH��� OCHOBHO� C�CTEM�...
         VALLN1(K) = VALLN1( M )
C---->
100      CONTINUE
         RETURN
         END
