         SUBROUTINE BA1CNG(I,J, BA0)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT AB��CTA 1987 �.)------------
C   �O��PO�PAMMA:   BA1CNG(I,J, BA0)
C   ------------
C   HA�HA�EH�E:    �EPECTAB��ET I  �  J  �A��C� B MACC�BE BA0
C  ------------     ( CM. TRIARB )
C   METO�:         O�EB��EH;
C   -----
C   �APAMETP�:     I,J HOMEPA �A��CE� B BA0;
C   ---------      BA0 - CM. INPUT0; !!! BH�MAH�E: �EPE�AETC� "OTPE�OK"
C                  ( ���HA �A��C�  -  N1BA0 =MASPRT(4) )
C   �O��PO�PAMM�:  1. COMMON PROF1
C   -------------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         INTEGER BA0
         DIMENSION BA0(1)
         EQUIVALENCE ( N1BA0, MASPRT(4) )
C--> I0 - A�P. HA�A�A
         I0 = 1
C$$$$$$$$$$$$$!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--> A�P. HA�A� �A��CE� I � J
         IADR = (I-1)*N1BA0 + I0
         JADR = (J-1)*N1BA0 + I0
C--> �EPECTAB��EM...
         DO 1 K = 1, N1BA0
         IX = BA0(IADR+K - 1)
         BA0(IADR+K - 1) = BA0(JADR+K - 1)
         BA0(JADR+K - 1) = IX
1        CONTINUE
         RETURN
         END
