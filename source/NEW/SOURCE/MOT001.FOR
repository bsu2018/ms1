	 SUBROUTINE MOT001(NDIM,N,A,X)					MOT*0001
CXXXX	 MOT001    -----------------------------------------------------MOT*0002
C    èOÑèPOÉPAMMA:  MOT001(NDIM,N,A,X)					MOT*0003
C    ------------							MOT*0004
C    HAáHAóEHàE:    BõóàCãEHàE MATPàñõ X, OÅPATHOâ MATPàñõ A;		MOT*0005
C		  èPà PAÅOTE MATPàñA A PAáPìòAETCü.			MOT*0006
C									MOT*0007
C    METOÑ:	àCKãûóEHàE ÅEá BõÅOPA BEÑìôEÉO ùãEMEHTA,		MOT*0008
C    ------	 A CóàTAETCü HE BõPOÜÑEHHOâ, KOHTPOã8 HE èPOBOÑàTCü,	MOT*0009
C		 ( Ñãü ìMEH8òEHàü Bõó. OèEPAñàâ )			MOT*0010
C									MOT*0011
C    èAPAMETPõ: NDIM - áAüBOóHAü PAáMEPHTCT8 MACCàBOB A à X;		MOT*0012
C    ---------								MOT*0013
C		N - PAáMEPHOCT8 MATPàñ A à X;				MOT*0014
C		A - àCXOÑHAü MATPàñA ( PAáPìòAETCü ! );			MOT*0015
C		X - BõóàCãüEMAü, Eâ OÅPATHAü;				MOT*0016
C									MOT*0017
C    BõáõBAEMõE èOÑèPOÉPAMMõ:	 HET.					MOT*0018
C    ------------------------						MOT*0019
C									MOT*0020
CZZZZ	 MOT001    ____________________________________________________ MOT*0021
	  DIMENSION A(NDIM,NDIM),X(NDIM,NDIM)				MOT*0022
	  REAL*4 A,X,T,T1						MOT*0023
C	  INTEGER*4 I,J,K,N,NDIM					MOT*0024
C									MOT*0025
C  COáÑAT8 EÑàHàóHâû B X						MOT*0026
C									MOT*0027
	  DO 1 I=1,N							MOT*0028
	  DO 1 J=1,N							MOT*0029
	  X(I,J)=0.0							MOT*0030
	  IF ( I .EQ. J ) X(I,J)=1.0					MOT*0031
1	  CONTINUE							MOT*0032
C									MOT*0033
C  àCKãûóEHàü, BEÑìôàâ BCEÉÑA ÑàAÉOHAã8Hõâ				MOT*0034
C									MOT*0035
	  DO 10 K=1,N							MOT*0036
C									MOT*0037
C---									MOT*0038
C									MOT*0039
	  T = A(K,K)							MOT*0040
C---									MOT*0041
	  A(K,K)=1.0							MOT*0042
C									MOT*0043
C---									MOT*0044
C	 HOPMàPOBKA CTPOKà K						MOT*0045
C									MOT*0046
	  DO 11 J=1,N							MOT*0047
	  X(K,J)=X(K,J)/T						MOT*0048
	  IF ( J .LE. K ) GO TO 11					MOT*0049
	  A(K,J)=A(K,J)/T						MOT*0050
11	  CONTINUE							MOT*0051
C									MOT*0052
C  àáMEHEHàE CTPOK							MOT*0053
C									MOT*0054
	  DO 12 I=1,N							MOT*0055
	  IF ( I .EQ. K) GO TO 12					MOT*0056
	  T1=A(I,K)							MOT*0057
	  A(I,K)=0.0							MOT*0058
	  DO 13 J=1,N							MOT*0059
	  X(I,J)=X(I,J)-X(K,J)*T1					MOT*0060
	  IF ( J .LE. K ) GO TO 13					MOT*0061
	  A(I,J)=A(I,J)-A(K,J)*T1					MOT*0062
13	  CONTINUE							MOT*0063
C---------								MOT*0064
12	  CONTINUE							MOT*0065
C--------								MOT*0066
10	  CONTINUE							MOT*0067
C--------								MOT*0068
	  RETURN							MOT*0069
	  END								MOT*0070
	  END								MOT*0070
