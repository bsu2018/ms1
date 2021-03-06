         SUBROUTINE DECOM0( NDIM, N, A, COND, IPVT, WORK )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DECOM0( NDIM, N, A, COND, IPVT, WORK )
C   -------------
C
C   HA�HA�EH�E:    O�PE�E��ET L_U PA��O�EH�E BE�ECTBEHHO� MATP��� A  �
C   ----------      O�EH�BAET EE O��C�OB�EHHOCT�;
C
C   METO�:         �A�COBO �CK���EH�E �O CTO���AM C �ACT��H�M B��OPOM
C   -----           BE���E�O ��EMEHTA;
C
C   �APAMETP�:     NDIM - �A�BO�HA� "CTPO�HA�" PA�MEPHOCT� MATP��� A;
C   ---------      N - �OP��OK C�CTEM� ( MATP��� A � BCEX BEKTOPOB );
C                  COND - O�EHKA O��C�OB�EHHOCT� MATP��� A:
C                    EC��  "COND .EQ. COND+1" --> TO MATP��A ����KA K
C                    B�PO��EHHO�; �P� TO�HOM B�PA��EH�� COND = 1.0E+32;
C                    EC�� �P� O�PA�EH�� K DECOM0 "COND=-1.0" --> O�EHKA
C                    O��C�OB�EHHOCT� HE B���C��ETC� -> KOHTPO�� HET!!!!!
C                  A - �CXO�HA� MATP��A C�CTEM� " A * X = B ";
C                      �OC�E B��O�HEH�� DECOM0 - CO�EP��T:
C                      L _ U PA��O�EH�E A, C TO�HOCT�� �O �EPECTAHOBOK;
C                  IPVT - BEKTOP BE����X ��EMEHTOB:
C                     IPVT(K) - �H�EKC K-� BE���E� CTPOK�;
C                     IPVT(N) = (-1)**<��C�O �EPECTAHOBOK>;
C                  WORK - PA�O��� BEKTOP;
C
C   �O��PO�PAMM�:  HET
C  --------------
C
C
C   �P�ME�AH�E:  1. ��� PE�EH�� C�CTEM�, �OC�E �EKOM�O����� MATP��� A,
C   ----------      HEO�XO��MO �C�O���OBAT� �/� SOLVE0;
C                2. O�PE�E��TE�� DET(A) =
C                      IPVT(N)*A(1,1)*A(2,2)*...*A(N,N);
C                3. BXO� B MATP��� A(I,J)==> �EPE�  A( ID(I,J) ) :
C                   ID(I,J) = NDIM*(J-1) + I
C
CZZZ-------------------------------------------------------------------
         DIMENSION A(1), WORK(1), IPVT(1)
         REAL A, COND, WORK
         INTEGER NDIM, N, IPVT
C--->
         REAL EK, T, ANORM, YNORM, ZNORM
         INTEGER NM1, I, J, K, KP1, KB, KM1, M
C--->
         ID(I,J) = NDIM * ( J - 1 ) + I
C--->
         IPVT(N) = +1
         IF ( N .EQ. 1 ) GO TO 80
         NM1 = N - 1
C
         IF (COND .EQ. -1.0 ) GO TO 100
C--> B���C��T� 1-HOPM� MATP��� A
         ANORM = 0.0
         DO 10 J=1,N
               T = 0.0
               DO 5 I=1,N
                   T = T + ABS( A( ID(I,J) ) )
5              CONTINUE
              IF ( T .GT. ANORM ) ANORM = T
10      CONTINUE
C
100      CONTINUE
C
C---> �A�COBO �CK���EH�E C �ACT��H�M B��OPOM BE���E�O ��EMEHTA
C
         DO 35 K=1, NM1
            KP1 = K + 1
C
C---> HA�T� BE����� ��EMEHT
C
            M = K
            DO 15 I=KP1, N
               IF ( ABS( A( ID(I,K) ) ) .GT. ABS( A( ID(M,K) ) ) )
     =                      M = I
15          CONTINUE
         IPVT(K) = M
         IF ( M .NE. K ) IPVT(N) = - IPVT(N)
         KK = ID(K,K)
         MK = ID(M,K)
         T = A(MK)
         A(MK) = A(KK)
         A(KK) = T
C
C--->    �PO��CK C�E�. �A�A, EC�� BE����� ��EMEHT BCE �E PABEH 0
C
         IF ( T .EQ. 0.0 ) GO TO 35
C
C--->    B���C�EH�E MHO��TE�E�
C
         DO 20 I=KP1, N
            IK = ID(I,K)
            A(IK) = - A(IK) / T
20       CONTINUE
C
C---->   �EPPECTAB��T� � �CK���AT� �O CTO���AM
C
            DO 30 J=KP1, N
               MJ = ID(M,J)
               KJ = ID(K,J)
               T = A(MJ)
               A(MJ) = A(KJ)
               A(KJ) = T
               IF ( T .EQ. 0.0 ) GO TO 30
               DO 25 I = KP1, N
                  IJ = ID(I,J)
                  IK = ID(I,K)
                  A(IJ) = A(IJ) + A(IK) * T
25             CONTINUE
30          CONTINUE
35       CONTINUE
C
C----------------------------------------------------------------------
         IF ( COND .EQ. -1.0 ) RETURN
C----------------------------------------------------------------------
C
C---> COND = <1-HOPMA MATP��� A> * < O�EHKA ��� 1-HOPMA MATP��� A**(-1)>
C     O�EHKA HAXO��TC� �OCPE�CTBOM O�HO�O �A�A METO�A O�PATH�X
C     �TEPA��� ��� HA�MEH��E�O C�H����PHO�O BEKTPA, ��� �E�O:
C     PE�EH�E �B�X C�CTEM �PABHEH��:
C        T
C      A   * Y = E
C
C      A   * Z = Y
C
C      ��E E - BEKTOP �� +1 � -1, B��PAHH�� TAK, �TO�� [Y] --> MAX.
C     (GALIMOTXQ)
C             T
C---> PE�AEM A  * Y  =  E
C
         DO 50 K=1,N
            T = 0.0
            IF ( K .EQ. 1 ) GO TO  45
            KM1 = K - 1
            DO 40 I = 1, KM1
               T = T + A( ID(I,K) ) * WORK(I)
40          CONTINUE
45          EK = 1.0
            IF ( T .LT. 0.0 ) EK = -1.0
            KK = ID(K,K)
            IF ( A(KK) .EQ. 0.0 ) GO TO 90
            WORK(K) = - ( EK + T ) / A(KK)
50       CONTINUE
C----->
         DO 60 KB = 1, NM1
            K = N - KB
            T = 0.0
            KP1 = K + 1
            DO 55 I = KP1, N
               T = T + A( ID(I,K) ) * WORK(K)
55          CONTINUE
            WORK(K) = T
            M = IPVT(K)
            IF ( M .EQ. K ) GO TO 60
            T = WORK(M)
            WORK(M) = WORK(K)
            WORK(K) = T
60       CONTINUE
C--->
         YNORM = 0.0
         DO 65 I =1, N
            YNORM = YNORM + ABS( WORK(I) )
65       CONTINUE
C
C---->  PE��T�  A * Z = Y
C
         CALL SOLVE0( NDIM, N, A, WORK, IPVT, IRC )
C
         ZNORM = 0.0
         DO 70 I=1, N
            ZNORM = ZNORM + ABS( WORK(I) )
70       CONTINUE
C
C--->  O�EH�T� "O��C�OB�EHHOCT�"
C
         COND = ANORM * ZNORM / YNORM
         IF ( COND .LT. 1.0 ) COND = 1.0
C
         RETURN
C
C----> C���A� MATP���  1*1
C
80       CONTINUE
         IF ( COND .EQ. -1.0 ) RETURN
         COND = 1.0
         IF ( A(1) .NE. 0.0 ) RETURN
C
C--->  "TO�HOE" B�PO��EH�E
C
90       COND = 1.0E+32
         RETURN
         END
