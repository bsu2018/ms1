         SUBROUTINE RSLMC (A,AF,B,X,N,EPSI,IER,IA,V,PER)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C O„POƒPAMMA:   RSLMC (A,AF,B,X,N,EPSI,IER,IA,V,PER)
C ------------
C***NAME PGM IS THE RSLMC      ˆ‡ SSP CTAH„APTH›X POƒPAMM.
C
CZZZ-------------------------------------------------------------------*
      DIMENSION A(1),AF(1),B(1),X(1),V(1),PER(1)
      DOUBLE PRECISION DP
      D0=0.
      IER=0
      ITE=0
      DO 10 I=1,N
      V(I)=B(I)
   10 X(I)=0.
   20 ITE=ITE+1
      DO 30 I=1,N
      K=PER(I)
      IF (K-I)25,30,25
   25 D1=V(K)
      V(K)=V(I)
      V(I)=D1
   30 CONTINUE
      DO 50 I=2,N
      IM1=I-1
      DP=V(I)
      IK=I
      DO 40 K=1,IM1
      DP=DP-1.D0*AF(IK)*V(K)
   40 IK=IK+IA
   50 V(I)=DP
      IF(AF(IK)) 58,54,58
   54 IER=4
      GO TO 82
   58 V(N)=DP/AF(IK)
      DO 70 I=2,N
      IM1=N-I+1
      INF=IM1+1
      DP=V(IM1)
      IK=(IM1-1)*IA+IM1
      D1=AF(IK)
      DO 60 K=INF,N
      IK=IK+IA
   60 DP=DP-1.D0*AF(IK)*V(K)
   70 V(IM1)=DP/D1
      D1=0.
      D2=0.
      KLE=0
      DO 80 I=1,N
      D1=D1+ABS(V(I))
      D2=D2+ABS(X(I))
      IF (ABS(V(I))-EPSI*ABS(X(I))) 80,80,75
   75 KLE=1
   80 CONTINUE
      IF (KLE)140,82,85
   82 RETURN
   85 IF (ITE-1)140,90,87
   87 IF (D0-2.*D1)120,90,90
   90 DO 95 I=1,N
   95 X(I)=X(I)+V(I)
      DO 110 I=1,N
      DP=B(I)
      IK=I
      DO 100 K=1,N
      DP=DP-1.D0*A(IK)*X(K)
  100 IK=IK+IA
  110 V(I)=DP
      D0=D1
      GO TO 20
  120 IF(ITE-2)140,140,125
  125 IF (D1-EPSI*D2)127,127,130
  127 IER=1
      RETURN
  130 IER=2
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      EPSI1 = D1/D2
C      EPSI2 = D1 - D2*EPSI
C      WRITE(6,12345) D1,D2,EPSI,EPSI1,EPSI2
C12345  FORMAT(' ->', 5(G12.5,1X) )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      EPSI=D1/D2
      RETURN
  140 IER=3
      RETURN
      END
