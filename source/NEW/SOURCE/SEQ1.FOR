         SUBROUTINE SEQ1( X1,  X2, K, IRC )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O�PO�PAMMA:    SEQ1( X1,  X2, K, IRC )
C   ------------
C   HA�HA�EH�E:     CPABHEH�E MACC�BOB X1 � X2 ( K-C�OB);
C   -----------     IRC = 1 - COB�A�A�T, = 2 - HET;
C   METO�:           (*** A�TEPHAT�BA K SEQ0 )
C   -----
C   �APAMETP�:     CM. HA�HA�EH�E;
C   ----------
C   �O��PO�PAMM�:  HET
C   -------------
C   �P�ME�AH�E:
C   ----------
CZZZ-------------------------------------------------------------------
         DIMENSION X1(1),  X2(1)
         INTEGER X1, X2
         IRC = 1
         DO 1 I=1,K
         IF ( X1(I) .EQ. X2(I) ) GO TO 1
         IRC = 2
         GO TO 2
1        CONTINUE
2        RETURN
         END
