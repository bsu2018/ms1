         INTEGER  FUNCTION   BA0CPL(I,J, BA0)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA-��HK���:  BA0CPL(I,J, BA0)
C   ---------------------
C   HA�HA�EH�E:    CPABHEH�E K���E�  I  �  J  �A��C� B MACC�BE BA0
C  ------------     ( CM. TRIARB ); K���AM� �B���TC� �O�� �P��HAKA
C                    T��A MAKPOMO�E��;
C   METO�:         O�EB��EH;
C   -----
C   �APAMETP�:     I,J HOMEPA �A��CE� B BA0;
C   ---------      BA0 - CM. INPUT0;
C                  ( ���HA �A��C�  -  N1BA0 =MASPRT(4) )
C                  BA0CPL  =  -1  K���  I  MEH��E  K���A  J;
C                          =   0  K���  I  PABEH   K����  J;
C                          =  +1  K���  I  �O���E  K���A  J;
C
C   �O��PO�PAMM�:  1. COMMON PROF1
C   -------------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         INTEGER BA0
         DIMENSION BA0(*)
         EQUIVALENCE ( N1BA0, MASPRT(4) )
C--> I0 - A�P. HA�A�A, IM - A�P. KOH�A
         I0 = 6
         IM = BA0(3)
C--> A�P. HA�A� �A��CE� I � J
         IADR = (I-1)*N1BA0 + I0
         JADR = (J-1)*N1BA0 + I0
C---> KOHTPO��
         IF ( IADR+N1BA0 .GT. IM ) CALL SUBERR(IADR)
         IF ( JADR+N1BA0 .GT. IM ) CALL SUBERR(JADR)
C--> CPABH�BAEM.....
         KEYI = BA0(IADR+4)
         KEYJ = BA0(JADR+4)
         IF ( KEYI - KEYJ ) 1, 2, 3
1        BA0CPL = -1
         RETURN
2        BA0CPL = 0
         RETURN
3        BA0CPL = +1
         RETURN
         END
