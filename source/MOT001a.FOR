	 SUBROUTINE MOT001(NDIM,N,A,X)					
CXXXX	 MOT001    -----------------------------------------------------
C    �O��PO�PAMMA:  MOT001(NDIM,N,A,X)					
C    ------------							
C    HA�HA�EH�E:    B���C�EH�E MATP��� X, O�PATHO� MATP��� A;		
C		  �P� PA�OTE MATP��A A PA�P��AETC�.			
C									
C    METO�:	�CK���EH�E �E� B��OPA BE���E�O ��EMEHTA,		
C    ------	 A C��TAETC� HE B�PO��EHHO�, KOHTPO�8 HE �POBO��TC�,	
C		 ( ��� �MEH8�EH�� B��. O�EPA��� )			
C									
C    �APAMETP�: NDIM - �A�BO�HA� PA�MEPHTCT8 MACC�BOB A � X;		
C    ---------								
C		N - PA�MEPHOCT8 MATP�� A � X;				
C		A - �CXO�HA� MATP��A ( PA�P��AETC� ! );			
C		X - B���C��EMA�, E� O�PATHA�;				
C									
C    B���BAEM�E �O��PO�PAMM�:	 HET.					
C    ------------------------						
C									
CZZZZ	 MOT001    ____________________________________________________ 
	  DIMENSION A(NDIM,NDIM),X(NDIM,NDIM)				
	  REAL*4 A,X,T,T1						
C	  INTEGER*4 I,J,K,N,NDIM					
C									
C  CO��AT8 E��H��H�� B X						
C									
	  DO 1 I=1,N							
	  DO 1 J=1,N							
	  X(I,J)=0.0							
	  IF ( I .EQ. J ) X(I,J)=1.0					
1	  CONTINUE							
C									
C  �CK���EH��, BE����� BCE��A ��A�OHA�8H��				
C									
	  DO 10 K=1,N							
C									
C---									
C									
	  T = A(K,K)							
C---									
	  A(K,K)=1.0							
C									
C---									
C	 HOPM�POBKA CTPOK� K						
C									
	  DO 11 J=1,N							
	  X(K,J)=X(K,J)/T						
	  IF ( J .LE. K ) GO TO 11					
	  A(K,J)=A(K,J)/T						
11	  CONTINUE							
C									
C  ��MEHEH�E CTPOK							
C									
	  DO 12 I=1,N							
	  IF ( I .EQ. K) GO TO 12					
	  T1=A(I,K)							
	  A(I,K)=0.0							
	  DO 13 J=1,N							
	  X(I,J)=X(I,J)-X(K,J)*T1					
	  IF ( J .LE. K ) GO TO 13					
	  A(I,J)=A(I,J)-A(K,J)*T1					
13	  CONTINUE							
C---------								
12	  CONTINUE							
C--------								
10	  CONTINUE							
C--------								
	  RETURN							
	  END								
	  END								
