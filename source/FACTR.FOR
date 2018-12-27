      SUBROUTINE FACTR(A,PER,N,IA,IER)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C***NAME PGM IS THE FACTR      àá SSP CTAHÑAPTHõX èPOÉPAMM.
C èOÑèPOÉPAMMA:  FACTR(A,PER,N,IA,IER)
C ------------
CZZZ-------------------------------------------------------------------*
      DIMENSION A(1),PER(1)
      DOUBLE PRECISION DP
      DO 20 I=1,N
      X=0.
      IJ=I
      DO 10 J=1,N
      IF (ABS(A(IJ))-X)10,10,5
    5 X=ABS(A(IJ))
   10 IJ=IJ+IA
      IF (X) 110,110,20
   20 PER(I)=1./X
      I0=0
      DO 100 I=1,N
      IM1=I-1
      IP1=I+1
      IPIVOT=I
      X=0.
      DO 50 K=I,N
      KI=I0+K
      DP=A(KI)
      IF (I-1) 110,40,25
   25 KJ=K
      DO 30 J=1,IM1
      IJ=I0+J
      DP=DP-1.D0*A(KJ)*A(IJ)
   30 KJ=KJ+IA
      A(KI)=DP
   40 IF (X-DABS(DP)*PER(K))45,50,50
   45 IPIVOT=K
      X=DABS(DP)*PER(K)
   50 CONTINUE
      IF (X)110,110,55
   55 IF (IPIVOT-I) 110,70,57
   57 KI=IPIVOT
      IJ=I
      DO 60 J=1,N
      X=A(IJ)
      A(IJ)=A(KI)
      A(KI)=X
      KI=KI+IA
   60 IJ=IJ+IA
      PER(IPIVOT)=PER(I)
   70 PER(I)=IPIVOT
      IF (I-N) 72,100,100
   72 IJ=I0+I
      X=A(IJ)
      K0=I0+IA
      DO 90 K=IP1,N
      KI=I0+K
      A(KI)=A(KI)/X
      IF (I-1)110,90,75
   75 IJ=I
      KI=K0+I
      DP=A(KI)
      DO 80 J=1,IM1
      KJ=K0+J
      DP=DP-1.D0*A(IJ)*A(KJ)
   80 IJ=IJ+IA
      A(KI)=DP
   90 K0=K0+IA
  100 I0=I0+IA
      IER=0
      RETURN
  110 IER=3
      RETURN
      END
