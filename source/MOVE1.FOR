         SUBROUTINE MOVE1( OUT, IN, K )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O�PO�PAMMA:    MOVE1( OUT, IN, K )
C   ------------
C   HA�HA�EH�E:     �EPEC��KA K C�OB �� MACC�BA IN B MACC�B OUT;
C   -----------
C   METO�:             ( A�TEPHAT�BA K MOVE0 )
C   -----
C   �APAMETP�:     CM. HA�HA�EH�E;
C   ----------
C   �O��PO�PAMM�:  HET
C   -------------
C   �P�ME�AH�E:
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION IN(1), OUT(1)
         INTEGER IN, OUT
         DO 1 I=1,K
         OUT(I) = IN(I)
1        CONTINUE
         RETURN
         END
