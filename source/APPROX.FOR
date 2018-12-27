      SUBROUTINE APPROX( N, N1, X, Y, A, B, C )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT OKTüÅPü 1987 É.)------------
C   èOÑèPOÉPAMMA: APPROX( N, N1, X, Y, A, B, C )
C   -------------
C
C   HAáHAóEHàE: èOÑèPOÉPAMMA APPROX èPEÑHAáHAóEHü Ñãü OèPEÑEãEHàü
C   ----------  KOùîîàñàEHTOB àHTEPèOãàPìûôEâ KBAÑPATHOâ
C               CèãAâH-îìHKñàà S(X)=Y(I)=A(I)*X(I)**2+B(I)*X(I)+C(I).
C
C
C   METOÑ:      PEòEHàE CàCTEMõ ãàHEâHõX ìPABHEHàâ à OèPEÑEãEHàE
C   -----       KOùîîàñàEHTOB àHTEPèOãàPìûôEâ CèãAâH-îìHKñàà.
C
C   èAPAMETPõ:         BXOÑHõE èAPAMETPõ:
C   ----------  N-KOããàóECTBO ìáãOB àHTEPèOãüñàà(N HE MEHúòE 3)
C               N1=N-1(Ñãü N>3)
C               N1=1  (Ñãü N=3)
C               X-MACCàB áHAóEHàâ APÉìMEHTA X(N)
C               Y-MACCàB áHAóEHàâ îìHKñàà Y(N)
C                      BõXOÑHõE èAPAMETPõ:
C               A,B,C-KOùîîàñàEHTõ CèãAâH-îìHKñàà A(N1),B(N1),C(N1)
C
C   èOÑèPOÉPAMMõ: HET.
C   ------------
CZZZ-------------------------------------------------------------------
C>>>>>DIMENSION X(N),Y(N),A(N1),B(N1),C(N1)
      DIMENSION X(*),Y(*),A(*),B(*),C(*)
      REAL X,Y,A,B,C
C**********************************************************************
C   OèPEÑEãEHàE KOùî. Ñãü èEPBOÉO ìóACTKA.
C**********************************************************************
      A(1)=((Y(2)-Y(1))*(X(3)-X(1))/(X(2)-X(1))-Y(3)+Y(1))/
     *((X(3)-X(1))*(X(2)-X(3)))
      B(1)=(Y(3)-Y(1)-A(1)*(X(3)**2-X(1)**2))/(X(3)-X(1))
      C(1)=Y(1)-A(1)*X(1)**2-B(1)*X(1)
      IF(N1.EQ.1) GO TO 9001
C**********************************************************************
C   OèPEÑEãEHàE KOùî. Ñãü I-TOÉO ìóACTKA.
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