       FUNCTION SEVAL0(KOD,XT,XYMAS)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA_��HK���: SEVAL0(KOD,XT,XYMAS)
C   ---------------------
C   HA�HA�EH�E: �O �AHH�M, �O���EHH�M B PE����TATE PA�OT� �/� SPLIN0
C  ------------ B���C��ET C�E�. �HA�EH�� ��� Y = F(X):
C              KO�       �HA�EH�E SEVAL0
C              1           F(XT)
C
C              2           �EPBA� �PO��BO�HA� B TO�KE X = XT
C
C              3           BTOPA� �PO��BO�HA� B TO�KE X = XT
C
C              4           TPET�� �PO��BO�HA� B TO�KE X = XT
C
C          B��OP �HTEPAB�A:  �P�  XT .LT. X(1) --->  �HTEPBA� �O I=1;
C                            �P�  XT .GE. X(N) --->  �HTEPBA� �OC�E I=N;
C   METO�:   CM. �/� SPLIN0;
C   -----
C   �APAMETP�:  XT - TO�KA �O OC� X O�PE�E�EH�� SEVAL0 CO��ACHO KOD;
C   ---------   XYMAS - MACC�B O��CAH�� C��A�HA -> CM. �/� SPLIN0;
C   �O��PO�PAMM�: SUBERR;
C   ------------
C   �P�ME�AH�E:  HET
C   ----------
C
CZZZ-------------------------------------------------------------------
       EQUIVALENCE( RN, N )
       DIMENSION XYMAS(1)
       INTEGER SB, SC,SD
       DATA I/1/
C      X(I) = XYMAS(2*I)
C      Y(I) = XYMAS(2*I+1)
C      B(I) = XYMAS(SB+I)
C      C(I) = XYMAS(SC+I)
C      D(I) = XYMAS(SD+I)
         RN =     XYMAS(1)
       SB = 2*N+1
       SC = 3*N+1
       SD = 4*N+1
          IF(I.GE.N) I = 1
               IP1 = I + 1
          IF(XT.LT.XYMAS(2*I) )  GO TO 10
          IF(XT.LE.XYMAS(2*IP1) )  GO TO 30
C**********************************************************************
   10  I = 1
       J = N+1
   20  K = (I+J)/2
          IF(XT.LT.XYMAS(2*K)) J = K
          IF(XT.GE.XYMAS(2*K)) I = K
          IF(J.GT.(I+1)) GO TO 20
   30  CONTINUE
C**********************************************************************
       GO TO (1,2,3,4), KOD
        CALL SUBERR(KOD)
    1  CONTINUE
C**********************************************************************
       DX = XT - XYMAS(2*I)
       SEVAL0 = XYMAS(1+2*I)+DX*(XYMAS(SB+I)
     =          +DX*(XYMAS(SC+I)+DX*XYMAS(SD+I)))
          RETURN
    2  CONTINUE
C**********************************************************************
       DX = XT - XYMAS(2*I)
       SEVAL0 = XYMAS(SB+I)+DX*(2*XYMAS(SC+I)+3*DX*XYMAS(SD+I))
       RETURN
    3  CONTINUE
C**********************************************************************
C                      BTOPA� �PO��BO�HA�
       DX = XT - XYMAS(2*I)
       SEVAL0 = 2*XYMAS(SC+I)+6*DX*XYMAS(SD+I)
       RETURN
    4  CONTINUE
C**********************************************************************
       SEVAL0 = 6*XYMAS(SD+I)
       RETURN
       END
