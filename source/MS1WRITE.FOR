C   MS1WRITE								
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT HOüÅPü 1987 É.)------------	
C   èPOÉPAMMA:	MS1WRITE						
C  ----------								
C   HAáHAóEHàE:        èEóAT8 àá FT05 HA FT03 ÅàããàCTàHÉOM		
C   ----------	       6O-üT èEPBõX CàMBOãOB				
C		       BBEÑEHHõX  CTOPOK TEKCTA ÅEá àCèOã8áOBAHàü	
C		       ìèP. CàMBOãOB èEóATà, HO OHà BõÑAûTCü.......	
C   èPàHüTO:	       HA ãàCTE 72 CTPOKà, èEóATAETCü 60-T8 CTPOK	
C   -------	       îAâãA FT05 HA ãàCTE,				
C		       èEPBAü áAèàC8 FT05 - KOãOHTàTìã,			
C		       èPà PAGE B èEPBõX èOá. FT05 --> èEPEXOÑ HA	
C		       HOBìû CTPAHàñì;					
C									
CZZZ------------------------------------------------------------------- 
       DIMENSION  LIST1(60,15), LIST2(60,15)				
       DIMENSION  KAR(32) ,KARZ(15)					
       INTEGER FLAG, PAGE						
       DATA PAGE/'PAGE'/, L/'	 ' /					
       DATA KAR/32*'	'/						
C---------------------------------------------------------------------- 
       NR = 5								
       NR1= 1								
       NW = 3								
C---------------------------------------------------------------------- 
C-->  BBOÑàM èEPEóEH8 BõBOÑàMõX CTPAHàñ					
       READ(NR1,5,END=9999) LISTN, LISTK				
5      FORMAT( 2I5 )							
C---------------------------------------------------------------------- 
C-->  BBOÑàM áAÉãABàE							
       READ(NR,10,END=9999) ( KARZ(I), I=1,15)				
10     FORMAT( 15A4 )							
C---------------------------------------------------------------------- 
       NSTR = -1							
C---------------------------------------------------------------------- 
15     CONTINUE								
C-->  BBOÑàM èEPBìû CTPAHàñì						
       NSTR = NSTR + 2							
       FLAG = 0								
       DO 50 I = 1,60							
       IF( FLAG .NE. 0 ) GO TO 30					
18     CONTINUE								
       READ(NR,10, END=19 ) (LIST1(I,J), J=1,15 )			
       IF( LIST1(I,1) .EQ. PAGE ) GO TO 20				
       GO TO 50								
19     CONTINUE								
       FLAG = 2								
       GO TO 30								
20     CONTINUE								
       IF (I .EQ. 1 ) GO TO 18						
       FLAG = 1								
30     CONTINUE								
       DO 40 J= 1,15							
       LIST1(I,J) = L							
40     CONTINUE								
50     CONTINUE								
C---------------------------------------------------------------------- 
C-->  BBOÑàM BTOPìû CTPAHàñì						
       IF( FLAG .NE. 2 ) FLAG = 0					
       DO 90 I = 1,60							
       IF( FLAG .NE. 0 ) GO TO 70					
55     CONTINUE								
       READ(NR,10, END=58 ) (LIST2(I,J), J=1,15 )			
       IF( LIST2(I,1) .EQ. PAGE ) GO TO 60				
       GO TO 90								
58     CONTINUE								
       FLAG = 2								
       GO TO 70								
60     CONTINUE								
       IF (I .EQ. 1 ) GO TO 55						
       FLAG = 1								
70     CONTINUE								
       DO 80 J= 1,15							
       LIST2(I,J) = L							
80     CONTINUE								
90     CONTINUE								
C---------------------------------------------------------------------- 
C-->  BõBOÑ CTPAHàñõ							
       NSTR1 = NSTR + 1							
C---------------------------------------------------------------------- 
C-->  BXOÑüT ãà TEKìôàE CTPAHàñõ B áAÑAHHõâ èEPEóEH8 CTPAHàñ?		
       IF( NSTR .LT. LISTN .AND. NSTR1 .LT. LISTN ) GO TO 15		
       IF( NSTR .GT. LISTK .AND. NSTR1 .GT. LISTK ) GO TO 15		
C---------------------------------------------------------------------- 
       WRITE(NW,13) ( KAR(J),J=1,15) , ( KAR(J),J=18,32)		
       WRITE(NW,11) ( KAR(J),J=1,32)					
       WRITE(NW,11) ( KAR(J),J=1,32)					
       WRITE(NW,11) ( KAR(J),J=1,32)					
11     FORMAT( 1X, 32A4 )						
       WRITE(NW,12) ( KAR(J),J=1,14), NSTR, KAR(16), KAR(17), NSTR1,	
     *		    ( KAR(J),J=19,32)					
12     FORMAT( 1X, 14A4 , I4 , 2A4 , I4 , 14A4 )			
       WRITE(NW,11) ( KARZ(J),J=1,15), L , L , ( KARZ(J),J=1,15)	
       WRITE(NW,11) ( KAR(J),J=1,32)					
       WRITE(NW,11) ( KAR(J),J=1,32)					
	 DO 150 I= 1,60							
	 WRITE(NW,11) ( LIST1(I,J),J=1,15), L , L ,			
     *		      ( LIST2(I,J),J=1,15)				
150	 CONTINUE							
       WRITE(NW,11) ( KAR(J),J=1,32)					
       WRITE(NW,11) ( KAR(J),J=1,32)					
       WRITE(NW,11) ( KAR(J),J=1,32)					
       WRITE(NW,13) ( KAR(J),J=1,15) , ( KAR(J),J=18,32)		
13     FORMAT( 1X, 15A4 ,'   **   ', 15A4 )				
C---------------------------------------------------------------------- 
       IF( FLAG .NE. 2 ) GO TO 15					
       STOP								
C###################################################################### 
9999   CONTINUE								
       WRITE(3,200)							
       WRITE(6,200)							
200    FORMAT(///' *** HAÅOP TEKCTA HA "FT05F001" - èìCTOâ ***'///)	
       STOP 8								
       END								
