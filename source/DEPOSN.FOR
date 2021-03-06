         SUBROUTINE DEPOSN( K, L, M, N, GKLMN, YALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT HO��P�  1987 �.)------------
C   �O��PO�PAMMA:   DEPOSN( K, L, M, N, GKLMN, YALL )
C   ------------
C   HA�HA�EH�E:    OC��ECTB�EH�E AKTA "�O��EMEHTHO�O" BK�A�A B
C   ----------     MATP��� ���OB�X �POBO��MOCTE� YALL  GJ-�APAMETPOB
C                  ��� �CTO�H�KA TOKA ,COE��H���E�O ���� "K" � "L"
C                  ( OT "K" K "L" ), ��PAB��EMO�O HA�P��EH�EM ME���
C                  ���AM� "M" � "N" ( OT "M" K "N" ) �O ��HE�HOM�
C                  �AKOH�:  J(K,L) = U(M,N) * GKLMN
C                           ***********************
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
         DIMENSION YALL(1)
         EQUIVALENCE ( NDALL, MASPRT(23) )
         ID( I, J ) = NDALL * ( J - 1 )  +  I
C--->   �O��EMEHTH�� BK�A�:
C======================================================================
         IF ( K .EQ. 0 ) GO TO 1000
         IF ( M .EQ. 0 ) GO TO 2000
         IKM = ID( K, M )
         YALL(IKM) = YALL(IKM) + GKLMN
         IF ( N .EQ. 0 ) GO TO 3000
         IKN = ID( K, N )
         YALL(IKN) = YALL(IKN) - GKLMN
         IF ( L .EQ. 0 ) RETURN
         ILM = ID( L, M )
         ILN = ID( L, N )
         YALL(ILM) = YALL(ILM) - GKLMN
         YALL(ILN) = YALL(ILN) + GKLMN
         RETURN
1000     CONTINUE
         IF ( L .EQ. 0 ) GO TO 50001
         IF ( M .EQ. 0 ) GO TO 1500
         ILM = ID( L, M )
         YALL(ILM) = YALL(ILM) - GKLMN
1500     CONTINUE
         IF ( N .EQ. 0 ) GO TO 50001
         ILN = ID( L, N )
         YALL(ILN) = YALL(ILN) + GKLMN
         RETURN
2000     CONTINUE
         IF ( N .EQ. 0 ) GO TO 50001
         IKN = ID( K, N )
         YALL(IKN) = YALL(IKN) - GKLMN
         IF ( L .EQ. 0 ) RETURN
         ILN = ID( L, N )
         YALL(ILN) = YALL(ILN) + GKLMN
         RETURN
3000     CONTINUE
         IF ( L .EQ. 0 ) RETURN
         ILM = ID( L, M )
         YALL(ILM) = YALL(ILM) - GKLMN
         RETURN
50001    CONTINUE
C===> BCE ���� H��EB�E <======
         RETURN
C======================================================================
         END
