       REAL FUNCTION KMAS01( X, TMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA-��HK���  KMAS01( X, TMAS )
C   --------------------
C   HA�HA�EH�E:        B���C�EH�E �HA�EH�� F(X), �A�AHHO� TA����HO
C   ----------         KO�����EHTAM� �O��HOMA:
C
C                      F(X) = A0 + A1* X + A2* X**2 + A3* X**3 + ...
C
C                      �O��CKAETC� �A�AH�E ���O�O KO���ECTBA
C                       KO��. �O��HOMA;
C
C                      *** KO��POBKA B O��CAH�� �APAMETPOB ***
C
C                      MAS < A0 A1 ..... A_N >;
C
C
C   METO�:     CXEMA �OPHEPA;
C   -----
C   �APAMETP�:  KMAS01 - �HA�EH�E F(X) �P�
C   ---------   X      - TO�KA O�PE�E�EH�� F(X);
C               TMAS - MACC�B �AHH�X ( PA�MEPHOCT� N  C�OB ):
C   MACC�B  --> N   A0  A1  A2 ..........................  � T.�.
C   C�OBO # --> 1    2  3   4  ..........................  � T.�.
C
C
C   �O��PO�PAMM�:      HET
C   ------------
C   �P�ME�AH�E:   1.  B TMAS BCE B REAL*4
C   ----------        ********************
C
C
C
CZZZ-------------------------------------------------------------------
       DIMENSION TMAS(1)
CVVVVV REAL KMAS01
       N  = IFIX( TMAS(1) )
C-->  CXEMA �OPHEPA...
       NP1 = N+1
       I = NP1
       SP = 0.0
10     CONTINUE
       SP = SP * X  + TMAS(I)
       I = I - 1
       IF ( I .GE. 2 ) GO TO 10
       KMAS01 = SP
       RETURN
       END
