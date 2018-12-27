C   MS1PRINT								
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------ 
C   èPOÉPAMMA:	MS1PRINT						
C  ----------								
C   HAáHA^EHàE:        èE^AT8 àá FT05 HA FT03 72-8 èEPBõX CàMBOãOB	
C   ----------	       BBEÑEHHõX  CTOPOK TEKCTA ÅEá àCèOã8áOBAHàü	
C		       ìèP. CàMBOãOB èE^ATà, HO OHà BõÑAûTCü.......	
C   èPàHüTO:	       HA ãàCTE 72 CTPOKà, èE^ATAETCü 60-T8 CTPOK	
C   -------	       îAâãA FT05 HA ãàCTE,				
C		       èEPBAü áAèàC8 FT05 - KOãOHTàTìã,			
C		       èPà PAGE B èEPBõX èOá. FT05 --> èEPEXOÑ HA	
C		       HOBìû CTPAHàñì;					
C									
CZZZ------------------------------------------------------------------- 
       DIMENSION TXT(60,18), KOLON(18)					
       INTEGER TXT, BLANK, PAGE						
       DATA PAGE/'PAGE'/, BLANK/'    ' /				
       READ(5,1,END=666) ( KOLON(I), I=1,18)				
1      FORMAT( 20A4 )							
       KCTR = 0								
       IEND = 0								
C-->  ñàKã èE^ATà							
100    CONTINUE								
       DO 10 I = 1,60							
       DO 10 J = 1,18							
10     TXT(I,J) = BLANK							
C									
       DO 11 I = 1,60							
       IX = I								
       READ(5,1, END=667 ) (TXT(I,J), J=1,18 )				
       IF ( TXT(IX,1 ) .EQ. PAGE ) GO TO 12				
11     CONTINUE								
       GO TO 13								
12     TXT(IX,1) = BLANK						
       IF (IX .EQ. 1 ) GO TO 100					
13     CONTINUE								
101    CONTINUE								
       KCTR = KCTR + 1							
C---------------------------------------------------------------------- 
C-->  BõBOÑ 72 CTOPOK							
       WRITE(3,14) BLANK, BLANK						
14     FORMAT( 1H1, 72X, 3X, '*' / 1X, A4 / 1X, A4 )			
       WRITE(3,15)	 KCTR						
15     FORMAT( 1X, 64X, I8 )						
       WRITE(3,16) ( KOLON(I), I=1,18)					
16     FORMAT( 1X, 20A4 )						
       WRITE( 3,17) BLANK						
17     FORMAT( 1X, A4 )							
       DO  170 I=1,60							
       WRITE(3,18) ( TXT(I,J), J=1,18 )					
18     FORMAT( 1X, 20A4 )						
170    CONTINUE								
       WRITE(3,19) BLANK, BLANK, BLANK, BLANK, BLANK			
19     FORMAT(1X,A4/ 1X,A4/ 1X, A4/ 1X, A4 / 1X, A4 /			
     *	     1X, 72X, 3X, '*' )						
C---------------------------------------------------------------------- 
       IF ( IEND .EQ. 1 ) GO TO 668					
       GO TO 100							
667    IEND = 1								
       GO TO 101							
666    WRITE(3,20)							
       WRITE(6,20)							
20     FORMAT(///' *** HAÅOP TEKCTA HA "FT05F001" - èìCTOâ ***'///)	
       STOP 8								
668    STOP 0								
       END								
