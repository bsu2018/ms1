         SUBROUTINE DEPOSQ( ZY, ZJ, YALL, JALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DEPOSQ( ZY, ZJ, YALL, JALL )
C   ------------
C   HA�HA�EH�E:    OC��ECTB�EH�E AKTA "��CTK�" -
C   ----------     MATP��� ���OB�X �POBO��MOCTE� YALL � BEKTOPA
C                  �CTO�H�KOB TOKA JALL �HA�EH��M�, COOTBETCTBEHHO
C                  ZY  �  JY;
C
C
C   METO�:         B V.M.=2.*  -  XPAHEH�E HE ��AKOBAHHO� MATP���;
C   -----
C   �APAMETP�:     CM. HA�HA�EH�E;
C   ---------
C   �O��PO�PAMM�:  1. PROF1
C   -------------
C   �P�ME�AH�E:    1. NDALL - "�A�BO�HA�" PA�MEPHOCT� YALL(*,*);
C   ----------        NSYS ��EMEHTOB "��CT�TC�";
C                  2. KOHTPO�� HE �POBO��TC�;
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION YALL(1), JALL(1)
         REAL JALL
         EQUIVALENCE ( NDALL, MASPRT(23) )
         EQUIVALENCE ( NSYS,  MASPRT(26) )
         ID( I, J ) = NDALL * ( J - 1 )  +  I
C======================================================================
C======================================================================
         DO 500 I = 1, NSYS
500      JALL(I) = ZJ
C----------------------------------------------------------------------
         DO 501 I = 1, NSYS
            DO 501 J = 1, NSYS
               IJ = ID(I,J)
               YALL(IJ) = ZY
501      CONTINUE
       RETURN
         END
