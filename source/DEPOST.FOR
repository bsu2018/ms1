         SUBROUTINE DEPOST( K, M, G0, J0, YALL, JALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DEPOST( K, M, G0, J0, YALL, JALL )
C   ------------
C   HA�HA�EH�E:    OC��ECTB�EH�E AKTA "�O��EMEHTHO�O" BK�A�A B
C   ----------     MATP��� ���OB�X �POBO��MOCTE� YALL � BEKTOP
C                  �CTO�H�KOB TOKA JALL O�EPE�HO� BETB�, COE��H���E�
C                  "K" � "M" ���� ( OT "K" K "M" ), � MO�E��P�EMO�
C                  G0 � J0 �APAMETPAM�;
C
C   METO�:         B V.M.=2.*  -  XPAHEH�E HE ��AKOBAHHO� MATP���;
C   -----
C   �APAMETP�:     CM. HA�HA�EH�E;
C   ---------
C   �O��PO�PAMM�:  1. PROF1
C   -------------
C   �P�ME�AH�E:    1. NDALL - "�A�BO�HA�" PA�MEPHOCT� YALL(*,*);
C   -----------    2. KOHTPO�� HE �POBO��TC�;
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION YALL(1), JALL(1)
         REAL JALL, J0
         EQUIVALENCE ( NDALL, MASPRT(23) )
         ID( I, J ) = NDALL * ( J - 1 )  +  I
C--->   �O��EMEHTH�� BK�A�:
C======================================================================
         IF ( K .EQ. 0 ) GO TO 50000
         IKK = ID( K, K )
         YALL(IKK) = YALL(IKK) + G0
         JALL(K) = JALL(K) - J0
         IF ( M .EQ. 0 ) RETURN
         IMM = ID( M, M )
         YALL(IMM) = YALL(IMM) + G0
         JALL(M) = JALL(M) + J0
         IKM = ID( K, M )
         IMK = ID( M, K )
         YALL(IKM) = YALL(IKM) - G0
         YALL(IMK) = YALL(IMK) - G0
         RETURN
50000    CONTINUE
         IF ( M .EQ. 0 ) GO TO 50001
         IMM = ID( M, M )
         YALL(IMM) = YALL(IMM) + G0
         JALL(M) = JALL(M) + J0
         RETURN
50001    CONTINUE
C===> BCE ���� H��EB�E <======
         RETURN
C======================================================================
         END
