           SUBROUTINE SOLVE0( NDIM, N, A, B, IPVT, IRC)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  SOLVE0( NDIM, N, A, B, IPVT, IRC)
C   -------------
C
C   HA�HA�EH�E:    PE�EH�E C�CTEM�: A * X = B   C �AKTOP��OBAHHO� MAT-
C   ----------     P��E� A ( L _ U PA��O�EH�E �POBE�EHO �/� DECOM0 );
C
C   METO�:         �P�MO� � O�PATH�� XO� "�A�CCA";
C   -----
C
C   �APAMETP�:     NDIM - "�A�BO�HA�" PA�MEPHOCT� MATP��� A;
C   ---------      N - PA�MEPHOCT� PE�AEMO� C�CTEM� ( O�PE�E��ET TAK �E
C                   PA�MEPHOCT� BCEX �C�O����EM�X MACC�BOB - BEKTOPOB )
C                  B - BEKTOP �PAB�X �ACTE� C�CTEM�; �OC�E B��O�HEH��
C                      SOLVE0 - BEKTOP PE�EH�� ( �P� IRC = 0 );
C                  IPVT - BEKTOP BE�. ��EMEHTOB, �O���EHH�� ��  DECOM0;
C                  IRC = 0 - C�CTEM� PE����;
C                  IRC ^=0 = K - B�PO��EH�E �P� O�PA�OTKE A(K,K),
C                                �P� �TOM PE�EH�E �PEP�BAETC� -
C                                BEKTOP B - "�C�OP�EH";
C
C
C   �O��PO�PAMM�:  HET;
C   ------------
C
C  �P�ME�AH�E:     CM. �/�   DECOM0;
C  ----------
C
C
CZZZ-------------------------------------------------------------------
         DIMENSION A(1), B(1), IPVT(1)
         INTEGER NDIM, N, IPVT, KB, KM1, NM1, KP1, I, K, M
         REAL A, B, T
         ID(I,J) = NDIM * ( J - 1 ) + I
C
C---> �P�MO� XO�
C
         IF ( N .EQ. 1 ) GO TO 50
         NM1 = N - 1
         DO 20 K = 1, NM1
            KP1 = K + 1
            M = IPVT(K)
            T = B(M)
            B(M) = B(K)
            B(K) = T
            DO 10 I=KP1, N
               B(I) = B(I) + A( ID(I,K) ) * T
10          CONTINUE
20       CONTINUE
C
C---->  O�PATHA� �O�CTAHOBKA
C
         DO 40 KB = 1, NM1
            KM1 = N - KB
            K = KM1 + 1
            AKKX = A( ID(K,K) )
            IF ( AKKX .EQ. 0.0 ) GO TO 100
            B(K) = B(K) / AKKX
            T = - B(K)
            DO 30 I = 1, KM1
               B(I) = B(I) + A( ID(I,K) ) * T
30          CONTINUE
40       CONTINUE
50       B(1) = B(1) / A(1)
         IRC = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
100      CONTINUE
         IRC = K
         RETURN
         END
