         INTEGER  FUNCTION   BA1CPL(I,J, BA0)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT AB��CTA 1987 �.)------------
C   �O��PO�PAMMA-��HK���:  BA1CPL(I,J, BA0)
C   ---------------------
C   HA�HA�EH�E:    CPABHEH�E K���E�  I  �  J  �A��C� B MACC�BE BA0
C  ------------     ( CM. TRIARB ); K���AM� �B���TC� �O��
C                    HOMEPOB ��EMEHTOB.
C   METO�:         O�EB��EH;
C   -----
C   �APAMETP�:     I,J HOMEPA �A��CE� B BA0;
C   ---------      BA0 - CM. INPUT0;!!! �EPE�AETC� OTPE�OK BA0 !!!
C                  ( ���HA �A��C�  -  N1BA0 =MASPRT(4) )
C                  BA1CPL  =  -1  K���  I  MEH��E  K���A  J;
C                          =   0  K���  I  PABEH   K����  J;
C                          =  +1  K���  I  �O���E  K���A  J;
C
C   �O��PO�PAMM�:  COMMON PROF1
C   -------------
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         INTEGER BA0
         DIMENSION BA0(1)
         EQUIVALENCE ( N1BA0, MASPRT(4) )
         I0 = 1
C--> A�P. HA�A� �A��CE� I � J
         IADR = (I-1)*N1BA0 + I0
         JADR = (J-1)*N1BA0 + I0
C--> CPABH�BAEM.....
         KEYI = BA0(IADR+0)
         KEYJ = BA0(JADR+0)
         IF ( KEYI - KEYJ ) 1, 2, 3
1        BA1CPL = -1
         RETURN
2        BA1CPL = 0
         RETURN
3        BA1CPL = +1
         RETURN
         END
