       SUBROUTINE SPLIN0( W   )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: SPLIN0 ( W     )
C   ------------
C   HA�HA�EH�E:   B���C�EH�E KO�����EHTOB K��. C��A�HA Y=F(X),
C   -----------   �A�AHHO� TA����HO; HACTPO�KA ��� �/� SEVAL0;
C
C                 B �HTEPBA�E  X(I) <=  X  <=  X(I+1) O�PE�E��ETC�:
C
C    F(X) = Y(I) + B(I)*[X-X(I)] + C(I)*[X-X(I)]**2 + D(I)*[X-X(I)]**3;
C
C   METO�:        CM. HA�P�MEP, �OPCA�T ��. � �P. MA�. METO��
C   -----         MAT. B���C�EH��, M.: M�P, 1980, 279 C.
C                 ==  MO����KA��� ��� KOM��EKCA "MS1"  ====
C   �APAMETP�:    MACC�B W ( XYMAS ) CO�EP��T:
C   ---------     1 C�OBO --------------> N - KO�-BO TO�EK ( INTEGER*4 )
C                OT 2-�O �O (2*N+1)-�O ---------> X1,Y1,...  TO�K� F(X);
C                OT (2*N+2) �O (5*N+1)  MACC�B� "B", "C", "D";
C   �O��PO�PAMM�:  SUBERR
C   ------------
C    �P�ME�AH�E:  1.�P� N < 2 - ABEND 6660
C    ----------   2.�P� N = 2 - ��HE�HA� �HTEP�O�����;
CZZZ-------------------------------------------------------------------
       DIMENSION     W(*)
       EQUIVALENCE( RN, N )
C
C   X,Y,B,C,D  -  BCTPOEHH�E ��HK���. "B�XO�" HA H��H�� MACC�B
C                 (SB,SC,SD  - CME�EH��)
C      X(I)=XYMAS(2*I)
C      Y(I)=XYMAS(2*I+1)
C      B(I)=XYMAS(SB+I)
C      C(I)=XYMAS(SC+I)
C      D(I)=XYMAS(SD+I)
       INTEGER SB,SC,SD
C**********************************************************************
       RN =       W(1)
       SB=2*N+1
       SC=3*N+1
       SD=4*N+1
C**********************************************************************
       NM1=N-1
       NM2 = N - 2
       NM3 = N - 3
       IF(N.LT.2) CALL SUBERR(6660)
       IF(N.LT.3) GO TO 50
C**********************************************************************
C      B - ��A�OHA��, D - HA���A�OHA��, C - �PAB�E �ACT�
       W(SD+1)=W(2*2)-W(2*1)
C
       W(SC+2)=(W(1+2*2)-W(1+2*1))/W(SD+1)
C
       DO 10 I=2,NM1
           IP1 = I+1
           IM1 = I-1
           W(SD+I)=W(2*IP1)-W(2*I)
           W(SB+I)=2*(W(SD+IM1)+W(SD+I))
           W(SC+IP1)=(W(1+2*IP1)-W(1+2*I))/W(SD+I)
           W(SC+I)=W(SC+IP1)-W(SC+I)
  10   CONTINUE
C***********************************************************************
C                       �PAH��H�E  �C�OB��
           W(SB+1) = -W(SD+1)
           W(SB+N) = -W(SD+NM1)
           W(SC+1) = 0.0
           W(SC+N) = 0.0
       IF(N.EQ.3) GO TO 15
           W(SC+1) = W(SC+3)/(W(2*4)-W(2*2))
     =             -  W(SC+2)/(W(2*3)-W(2*1))
           W(SC+N) = W(SC+NM1)/(W(2*N)-W(2*NM2))
     =          - W(SC+NM2)/(W(2*NM1)-W(2*NM3))
           W(SC+1) = W(SC+1)*W(SD+1)**2
     =           /(W(2*4)-W(2*1))
           W(SC+N) = -W(SC+N)*W(SD+NM1)**2
     =          /(W(2*N)-W(2*NM3))
   15  CONTINUE
       DO 20 I=2,N
          T = W(SD+I-1)/W(SB+I-1)
          W(SB+I) = W(SB+I)-T*W(SD+I-1)
          W(SC+I) = W(SC+I)-T*W(SC+I-1)
   20  CONTINUE
C**********************************************************************
C                          �O�CTAHOBK�
          W(SC+N) = W(SC+N)/W(SB+N)
       DO 30 IB=1,NM1
          I = N-IB
          IP1 = I + 1
          W(SC+I) = (W(SC+I)-W(SD+I)*W(SC+IP1))/W(SB+I)
   30  CONTINUE
C**********************************************************************
C                    KO������EHT� �O��HOMOB
       W(SB+N) = (W(1+2*N)-W(1+2*NM1))/W(SD+NM1)
     =      + W(SD+NM1)*(W(SC+NM1)+2*W(SC+N))
       DO 40 I=1,NM1
       IP1 = I + 1
       W(SB+I) = (W(1+2*IP1)-W(1+2*I))/W(SD+I)
     =      - W(SD+I)*(W(SC+IP1)+2*W(SC+I))
       W(SD+I) = (W(SC+IP1)-W(SC+I))/W(SD+I)
       W(SC+I) = 3*W(SC+I)
  40   CONTINUE
       W(SC+N) = 3*W(SC+N)
       W(SD+N) = W(SD+NM1)
       RETURN
C**********************************************************************
  50   CONTINUE
       W(SB+1) = (W(1+2*2)-W(1+2*1))/(W(2*2)-W(2*1))
       W(SC+1) = 0.0
       W(SD+1) = 0.0
       W(SB+2) = W(SB+1)
       W(SC+2) = 0.0
       W(SD+2) = 0.0
       RETURN
C**********************************************************************
C**********************************************************************
        END
