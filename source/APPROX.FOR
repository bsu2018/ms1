      SUBROUTINE APPROX( N, N1, X, Y, A, B, C )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT OKT��P� 1987 �.)------------
C   �O��PO�PAMMA: APPROX( N, N1, X, Y, A, B, C )
C   -------------
C
C   HA�HA�EH�E: �O��PO�PAMMA APPROX �PE�HA�HA�EH� ��� O�PE�E�EH��
C   ----------  KO������EHTOB �HTEP�O��P���E� KBA�PATHO�
C               C��A�H-��HK��� S(X)=Y(I)=A(I)*X(I)**2+B(I)*X(I)+C(I).
C
C
C   METO�:      PE�EH�E C�CTEM� ��HE�H�X �PABHEH�� � O�PE�E�EH�E
C   -----       KO������EHTOB �HTEP�O��P���E� C��A�H-��HK���.
C
C   �APAMETP�:         BXO�H�E �APAMETP�:
C   ----------  N-KO����ECTBO ���OB �HTEP�O�����(N HE MEH��E 3)
C               N1=N-1(��� N>3)
C               N1=1  (��� N=3)
C               X-MACC�B �HA�EH�� AP��MEHTA X(N)
C               Y-MACC�B �HA�EH�� ��HK��� Y(N)
C                      B�XO�H�E �APAMETP�:
C               A,B,C-KO������EHT� C��A�H-��HK��� A(N1),B(N1),C(N1)
C
C   �O��PO�PAMM�: HET.
C   ------------
CZZZ-------------------------------------------------------------------
C>>>>>DIMENSION X(N),Y(N),A(N1),B(N1),C(N1)
      DIMENSION X(*),Y(*),A(*),B(*),C(*)
      REAL X,Y,A,B,C
C**********************************************************************
C   O�PE�E�EH�E KO��. ��� �EPBO�O ��ACTKA.
C**********************************************************************
      A(1)=((Y(2)-Y(1))*(X(3)-X(1))/(X(2)-X(1))-Y(3)+Y(1))/
     *((X(3)-X(1))*(X(2)-X(3)))
      B(1)=(Y(3)-Y(1)-A(1)*(X(3)**2-X(1)**2))/(X(3)-X(1))
      C(1)=Y(1)-A(1)*X(1)**2-B(1)*X(1)
      IF(N1.EQ.1) GO TO 9001
C**********************************************************************
C   O�PE�E�EH�E KO��. ��� I-TO�O ��ACTKA.
C**********************************************************************
         DO 9000 I=2,N1
         A(I)=((Y(I+1)-Y(I))/(X(I+1)-X(I))-2*A(I-1)*X(I)-
     *   B(I-1))/(X(I+1)-X(I))
         B(I)=2*X(I)*(A(I-1)-A(I))+B(I-1)
         C(I)=Y(I)-A(I)*X(I)**2-B(I)*X(I)
 9000 CONTINUE
 9001 CONTINUE
      RETURN
      END
