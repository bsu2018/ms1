	 SUBROUTINE MOT001(NDIM,N,A,X)					
CXXXX	 MOT001    -----------------------------------------------------
C    O„POƒPAMMA:  MOT001(NDIM,N,A,X)					
C    ------------							
C    HA‡HA—EHˆE:    B›—ˆC‹EHˆE MATPˆ–› X, OPATHO‰ MATPˆ–› A;		
C		  Pˆ PAOTE MATPˆ–A A PA‡P“˜AETCŸ.			
C									
C    METO„:	ˆCK‹ž—EHˆE E‡ B›OPA BE„“™EƒO ‹EMEHTA,		
C    ------	 A C—ˆTAETCŸ HE B›PO†„EHHO‰, KOHTPO‹8 HE POBO„ˆTCŸ,	
C		 ( „‹Ÿ “MEH8˜EHˆŸ B›—. OEPA–ˆ‰ )			
C									
C    APAMETP›: NDIM - ‡AŸBO—HAŸ PA‡MEPHTCT8 MACCˆBOB A ˆ X;		
C    ---------								
C		N - PA‡MEPHOCT8 MATPˆ– A ˆ X;				
C		A - ˆCXO„HAŸ MATPˆ–A ( PA‡P“˜AETCŸ ! );			
C		X - B›—ˆC‹ŸEMAŸ, E‰ OPATHAŸ;				
C									
C    B›‡›BAEM›E O„POƒPAMM›:	 HET.					
C    ------------------------						
C									
CZZZZ	 MOT001    ____________________________________________________ 
	  DIMENSION A(NDIM,NDIM),X(NDIM,NDIM)				
	  REAL*4 A,X,T,T1						
C	  INTEGER*4 I,J,K,N,NDIM					
C									
C  CO‡„AT8 E„ˆHˆ—H‰ž B X						
C									
	  DO 1 I=1,N							
	  DO 1 J=1,N							
	  X(I,J)=0.0							
	  IF ( I .EQ. J ) X(I,J)=1.0					
1	  CONTINUE							
C									
C  ˆCK‹ž—EHˆŸ, BE„“™ˆ‰ BCEƒ„A „ˆAƒOHA‹8H›‰				
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
C	 HOPMˆPOBKA CTPOKˆ K						
C									
	  DO 11 J=1,N							
	  X(K,J)=X(K,J)/T						
	  IF ( J .LE. K ) GO TO 11					
	  A(K,J)=A(K,J)/T						
11	  CONTINUE							
C									
C  ˆ‡MEHEHˆE CTPOK							
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
