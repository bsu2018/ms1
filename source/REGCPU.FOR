	 SUBROUTINE REGCPU (KOD, KDIM, MASKOD, NBEG, IEROR )		
CXXXX --äOMèãEäC "MS1" --> BEPCàü 1.0  (OT ÑEäAÅPü 1986 É.)------------ 
C      èOÑèPOÉPAMA: REGCPU (äOD, äDIM, MASäOD, NBEG, IEROR )		
C      -----------							
C									
C      çAáçAÁEçàE: ÑàçAMàÁECäOE BõÑEãEçàE èOÑMACCàBOB àá OÅôEÉO		
C      ----------  MACCàBA èAMüTà REGION;				
C		   MACCàBõ OèàCAçõ B COMMON /MEMORY/ à /PONTER/		
C		     PEAãúçOE äOã-BO èAMüTà MOÜçO OèàCõBATú B		
C		   CO èPOÉPAMME BLOCä DATE;				
C									
C      METOÑ:	   MOÑàîàäAñàü "äìÁà" C àCèOãúáOBAçàEM ãOäAãúçOÉO	
C      -----	   "CTEäOBOÉO" èPàEMA CÅOPA MìCOPA, C BOáMOÜçOCTúû	
C		    CÅOPA MìCOPA èO MEPE çEOÅXOÑàMOCTà.			
C									
C     èAPAMETPõ:   KOD - BõÅOP KOÑA OÅCãìÜàBAçàü:			
C     ---------								
C		   KOD		 BXOÑçõE	     BõXOÑçõE		
C  àçàñàAãàáàPOBATú  0		   -			-		
C									
C  COáÑATú MACCàB    1		   KDIM		      MASKOD, NBEG,	
C						       IEROR ( KDIM )	
C  ìÅPATú MACCàB     2		   MASKOD, KDIM,       IEROR		
C				   NBEG					
C  ìTOÁçàTú MECTO    3		    MASKOD		KDIM,NBEG,	
C							  IEROR		
C  COÅPATú MìCOP     4		       -		 KDIM, IEROR	
C									
C    KDIM - PAáMEPçOCTú MACCàBA ( B CãOBAX )				
C	  ( MAX. ÑOCTìèçAü èAMüTú èPà IEROR = 2 );			
C									
C    MASKOD - òàîP ( KOÑ ) MACàBA ( ìKAáATEãú B PRT );			
C									
C    NBEG - çOMEP çAÁAãA MACCàBA B REGION ( AÑPEC çAÁAãA );		
C									
C    IEROR - KOD OòàÅKà:						
C			 0 - ÑEâCTBàE BõèOãçEçO;			
C			 1 - ÑEâCTBàE BõèOãçEçO, çO COÅàPAãà MìCOP;	
C			     ( MACCàBõ çìÜÑAûTCü B ìTOÁçEçàà NBEG )	
C			 2 - ÑEâCTBà çE BõèOãçEçO - OòàÅKà OÅPAôEçàü	
C			      àãà çET èAMüTà ( MAX. B KDIM )		
C									
C									
C      èOÑèPOÉPAMMõ: çET  ( SUBERR ) COMMON: MEMORY, PONTER (PRINTN)	
C      -------------							
C									
C      èPàMEÁAçàE:							
C      -----------							
C	  èOCãE BõÑEãEçàü, MACCàB REGION(NBEG) ÑãàççOâ KDIM OèPEÑEãEç;	
C	  ÑOCTìè ÁEPEá PRT:   NBEG = PRT(MASKOD) - ùTO BCEÉÑA !!!!!!!;	
C	  MACCàBõ OÑçOáçAÁçO OèPEÑEãEçõ CBOàM òàîPOM MASKOD;		
C									
CZZZ------------------------------------------------------------------- 
       COMMON /MEMORY/ NREG,REGION(1)					
C									
       COMMON /PONTER/ NPRT, PRT(1)					
C									
	 COMMON /PRINTN/ NW6						
C									
       INTEGER REGION,PRT,OLDFRE, BEDREG,ADRMAS,OLD			
C									
C!!!------------------------------------------------------------------- 
500    FORMAT('  BXOÑ: KOD=', I2, '; KDIM=', I4, '; MASKOD=', I4,	
     *	      '; NBEG=', I4, '; IE=', I2 )				
501    FORMAT(' BõXOÑ: KOD=', I2, '; KDIM=', I4, '; MASKOD=', I4,	
     *	      '; NBEG=', I4, '; IE=', I2 )				
C									
C      WRITE (NW6,500 ) KOD, KDIM, MASKOD, NBEG, IEROR			
502    FORMAT(' NEWFRE=', I4, '; OLDFRE=', I4, '; NEWPRT=',		
     *	       I4, '; BEDREG=', I2, '; ARDMAS=', I4, '; NEW1=',		
     *	       I4   )							
C!!!------------------------------------------------------------------- 
       GO TO ( 1, 2, 3, 4 ),KOD						
C---------------------------------------------------------------------- 
C  àçàñàAãàáAñàü							
       NEWFRE= 1							
       OLDFRE = 0							
       NEWPRT = 1							
       BEDREG = 0							
       IEROR = 0							
C---------------------------------------------------------------------- 
C!!!  ÁàCTKA	 MACCàBOB						
	 DO 120 I=1,NPRT						
120	 PRT(I)=0							
	 DO 121 I=1,NREG						
121	 REGION(I)=0							
C!!!-----------------							
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
C      WRITE (NW6,501) KOD, KDIM, MASKOD, NBEG, IEROR			
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
       RETURN								
C---------------------------------------------------------------------- 
C********************************************************************** 
C---------------------------------------------------------------------- 
C    COáÑATú MACCàB							
1      CONTINUE								
       IEROR=0								
       IF ( NEWPRT .LE. NPRT ) GO TO 10					
C    çET CBOÅOÑçOÉO MECTA B PRT, àCKATú PAçEE ìÑAãEççõâ			
       IPRT=1								
12     IF ( PRT(IPRT) .EQ. 0 ) GO TO 11					
       IPRT=IPRT+1							
       IF ( IPRT .LE. NPRT ) GO TO 12					
C   *****  OòàÅKA:  çET MECTA B PRT *********************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,13) NPRT						
13     FORMAT(' *****  OòàÅKA REGCPU: MAã PRT, NPRT=',I10)		
C!!!------------------------------------------------------------------- 
       GO TO 666							
C   MECTO B PRT çAòãà							
11     MASKOD = IPRT							
       GO TO 14								
C   CBOÅOÑçOE MECTO B PRT ECTú						
10     MASKOD = NEWPRT							
       NEWPRT = NEWPRT + 1						
C  BõÑEãàM áBEçO B REGION						
14     NEW1 = NEWFRE + KDIM + 2						
C  XBATàT ãà MECTA B REGION						
16     IF ( NEW1 .LE. NREG ) GO TO 15					
C  MECTA çE XBATàãO, èOèPOÅìEM COÅPATú MìCOP......			
       IEROR=1								
       ASSIGN 17 TO METKA						
       GO TO 45								
17     CONTINUE								
C  MìCOP COÅPAãà, ECTú ãà èAMüTú ?					
       IF ((NEWFRE + KDIM  + 2) .LE. NREG ) GO TO 14			
C    èAMüTà BCE PABçO çET						
       KDIM  = KDIMX							
       IEROR = 2							
C---------------------------------------------------------------------- 
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
C      WRITE (NW6,501) KOD, KDIM, MASKOD, NBEG, IEROR			
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
       RETURN								
C---------------------------------------------------------------------- 
15     CONTINUE								
C  èAMüTú ECTú, áAèOãçàM ìKAáATEãà áBEçA à PRT				
       REGION( NEWFRE )=OLDFRE						
       REGION( NEWFRE + 1 ) = KDIM					
       NBEG = NEWFRE +	2						
       PRT ( MASKOD ) = NBEG						
C   OÅçOBàM ÉãOÅAãúçõE ìKAáATEãà					
       OLDFRE = NEWFRE							
       NEWFRE = NEW1							
C---------------------------------------------------------------------- 
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
C      WRITE (NW6,501) KOD, KDIM, MASKOD, NBEG, IEROR			
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
       RETURN								
C---------------------------------------------------------------------- 
C********************************************************************** 
C---------------------------------------------------------------------- 
C    ìÅPATú MACCàB							
2      CONTINUE								
C   èPOBEPKà COOTBETCTBàü						
       IF ( MASKOD .GE. 1  .AND.					
     *	    MASKOD .LE. NPRT  ) GO TO 20				
C*** OòàÅKA ìÑAãEçàü ************************************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,21) MASKOD						
21     FORMAT(' ***** OòàÅKA REGCPU: çEBEPEç MASKOD=',I10)		
C!!!------------------------------------------------------------------- 
       GO TO 666							
20     ADRMAS= PRT(MASKOD)						
       IF ( (ADRMAS-2) .GE. 1	.AND.					
     *	    (ADRMAS-2) .LE. NEWFRE  )  GO TO 22				
C*** OòàÅKA B PRT ***************************************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,23) ADRMAS						
23     FORMAT(' ***** OòàÅKA REGCPU: ADRMAS=',I10)			
C!!!------------------------------------------------------------------- 
       GO TO 666							
22     IF ( REGION(ADRMAS-1)  .EQ.  KDIM  )  GO TO 24			
C***  OòàÅKA B REGION  ************************************************ 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,25)  REGION(ADRMAS-1)					
25     FORMAT(' ***** OòàÅKA Ñãàçõ àá REGION=',I10)			
C!!!------------------------------------------------------------------- 
       GO TO 666							
24     IF ( ADRMAS .EQ. NBEG ) GO TO 26					
C*** OòàÅKA çAÁAãA ìÑAãEçàü  ****************************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,27) NBEG						
27     FORMAT(' ***** OòàÅKA REGCPU: NBEG=',I10)			
C!!!------------------------------------------------------------------- 
       GO TO 666							
26     CONTINUE								
C   ìÑAãEçàE B PRT							
       PRT(MASKOD) = 0							
C ãOKAãúçAü ÁàCKA PRT,, ECãà MOÜçO					
       IF ( MASKOD .EQ. 1 ) GO TO 28					
       IF ( MASKOD .NE.  (NEWPRT -1) ) GO TO 28				
C  ÁàCTàM ãOKAãúçO							
29     NEWPRT = NEWPRT -1						
       IF ( NEWPRT .EQ. 1 ) GO TO 28					
       IF ( PRT(NEWPRT) .NE. 0 ) GO TO 28				
       IF ( NEWPRT .EQ. 1 ) GO TO 28					
       GO TO 29								
28     CONTINUE								
C  ìÑAãüEM B REGION: MOÜET ÅõTú MOÜçO èO CTEK-AãÉOPàTMì?		
       IF (  (ADRMAS-2) .EQ. OLDFRE ) GO TO 30				
C      çET, èPOCTO èOMETàM ìÑAãEççõâ ÅãOK				
       REGION(ADRMAS-1) = -REGION(ADRMAS-1)				
C  áAîàKCàPìEM çAãàÁàE MìCOPA						
       BEDREG = BEDREG + 1						
31     IEROR=0								
C---------------------------------------------------------------------- 
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
C      WRITE (NW6,501) KOD, KDIM, MASKOD, NBEG, IEROR			
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
       RETURN								
C  ãOKAãúçAü ÁàCKA REGION ( èO CTEK-AãÉOPàTMì)				
30     NEWFRE = OLDFRE							
C  èPEÑõÑìôàâ àá BõÅPACõBAEMOÉO áBEçA					
       OLDFRE = REGION(OLDFRE)						
C  èOPOÅìEM èPOÑOãÜàTú ãOKAãúçìû CÅOPKì MìCOPA				
       IF ( REGION(OLDFRE+1) .GE. 0 )  GO TO 31				
C  MOÜçO ÁàCTàTú							
       BEDREG = BEDREG - 1						
       GO TO 30								
C---------------------------------------------------------------------- 
C********************************************************************** 
C---------------------------------------------------------------------- 
C èOãìÁàTú CèPABKì							
3	 CONTINUE							
	 IF ( MASKOD . GE. 1  .AND.					
     *	      MASKOD . LE. NEWPRT  ) GO TO 300				
C*** çAPìòEçàE ÉPAçàñ KOÑA èPà CèPABKE	******************************* 
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,310) MASKOD						
310	 FORMAT(' ***** OòàÅKA REGCPU: ìTOÁçEçàE MASKOD=',I10)		
C!!!------------------------------------------------------------------- 
	 GO TO 666							
300	 CONTINUE							
	 NBEG = PRT(MASKOD)						
	 IF ( NBEG .GE. 3  .AND.					
     *	      NBEG .LE.   (OLDFRE+2) ) GO TO 32				
C***** çAPìòEçàE MASKOD -> PRT(MASKOD)	******************************* 
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,33) MASKOD, NBEG					
33	 FORMAT( ' ***** OòàÅKA REGCPU: MASKOD=',I10,' NBEG=',I10)	
C!!!------------------------------------------------------------------- 
	 GO TO 666							
32	 CONTINUE							
	 KDIM = REGION(NBEG-1)						
	 IEROR = 0							
C---------------------------------------------------------------------- 
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
C      WRITE (NW6,501) KOD, KDIM, MASKOD, NBEG, IEROR			
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
	 RETURN								
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
C    COÅPATú MìCOP:  B KDIMX - CKOãúKO èAMüTà CBOÅOÑçO			
4	 CONTINUE							
	ASSIGN 41 TO METKA						
	 GO TO 45							
41	 IEROR=1							
	 KDIM = KDIMX							
C---------------------------------------------------------------------- 
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
C      WRITE (NW6,501) KOD, KDIM, MASKOD, NBEG, IEROR			
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
	RETURN								
45	CONTINUE							
C  ***** 1 - ü îAáA, PAáMETKA MìCOPA					
	IF ( BEDREG .GT. 0 ) GO TO 47					
C   COÅàPATú çEÁEÉO		$ ***					
	KDIMX = NREG - NEWFRE -2					
	GO TO METKA, (17, 41)						
C!!!------------------------------------------------------------------- 
667	 GO TO 666							
C!!!------------------------------------------------------------------- 
47	CONTINUE							
	IPRT = 1							
C  çAâTà OTKPõTõâ èO PRT à èOMETàTú B REGION ( èOãE OLDFRE)		
49	ADRMAS = PRT(IPRT)						
	IF ( ADRMAS .EQ. 0 ) GO TO 50					
	IF (  (ADRMAS-2) .GE. 1   .AND.					
     *	      (ADRMAS-2) .LE. OLDFRE  ) GO TO 51			
C!!!------------------------------------------------------------------- 
	WRITE(NW6,52) ADRMAS, OLDFRE					
52	FORMAT(' ****** OòàÅKA REGCPU: ADRMAS=',I10,' OLDREF=',I10)	
C!!!------------------------------------------------------------------- 
	GO TO 666							
C  PAáMETKA								
51	REGION(ADRMAS-2) = IPRT						
50	IPRT = IPRT + 1							
	IF ( IPRT .LE. NEWPRT ) GO TO 49				
C-------								
C  2-ü îAáA								
C-------								
	 NFIX = 1							
	 NTEK = 1							
C  ÑãàçA TEK. ÅãOKA							
53	 KDIMX = REGION(NTEK+1)						
	 IF ( KDIMX .GE. 0 ) GO TO 54					
C  ÅãOK BõÁEPKçìT, EÉO èPOèìCTàTú					
	 NTEK = NTEK + IABS(KDIMX) + 2					
C   ìMEçúòàTú KOçTPOãúçõâ CÁK. BõÁEPKçìTõX ÅãOKOB			
	 BEDREG = BEDREG - 1						
C---KOçñOBKA ñàKãA							
55	 IF ( NTEK .LT. NEWFRE ) GO TO 53				
	 GO TO 60							
C  OÅPAÅOTKA XOPOòEÉO ÅãOKA, îàKCàPìEM EÉO çAÁAãO			
54	NMOVE = NTEK							
C  AÑP. CãEÑ.								
	NTEK = NTEK + KDIMX + 2						
C  çìÜçO ãà îàáàÁECKà èEPECõãATú					
	IF ( NFIX .EQ. NMOVE ) GO TO 56					
C èEPECõãAEM, çE CÅàãàCú ?						
	 IF ( NFIX .LT. NMOVE ) GO TO 57				
C**** CÅàãàCú ********************************************************* 
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,58) NFIX, NMOVE					
58	 FORMAT(' ***** CÅOâ B REGCPU: NFIX=',I10,' NMOVN=',I10)	
C!!!------------------------------------------------------------------- 
	 GO TO 666							
57	 CONTINUE							
	 IFF = 0							
59	 REGION( NFIX+IFF ) = REGION( NMOVE+IFF )			
	 IFF = IFF + 1							
	 IF ( IFF .LT. KDIMX+2 ) GO TO 59				
C  KOPEK. ìKAáATEãà áAçüTõX						
56	 NFIX = NFIX + KDIMX + 2					
	 GO TO 55							
C  KOçEñ 2-â îAáõ:							
60	 NEWFRE = NFIX							
C  CAMOèPOBEPKA:							
	 IF ( BEDREG .EQ. 0 ) GO TO 62					
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,61) BEDREG						
61	 FORMAT(' ***** OòàÅKA REGCPU: àTOÉ BEDREG=',I10)		
C!!!------------------------------------------------------------------- 
	 GO TO 666							
C-------								
C   3-ü îAáA								
C-------								
C   PACTABàM ìKAáATEãà							
62	 OLD=0								
C--------IF ( NEWFRE .EQ. 1 ) GO TO 63					
	 NTEK = 1							
C   BõÅPATú KOÑ MAC. ( MECTO B PRT )					
64	 IPP = REGION(NTEK)						
C   èPOBEPàTCü								
	 IF ( IPP .GE. 1  .AND.						
     *	      IPP .LE.	  NPRT ) GO TO 65				
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,66) IPP, NTEK					
66	 FORMAT(' ***** CÅOâ REGCPU: IPP=',I10,' NTEK=',I10)		
C!!!------------------------------------------------------------------- 
	 GO TO 666							
C   BOCTAçOBàTú B PRT							
65	 PRT(IPP) = NTEK + 2						
	 REGION(NTEK) = OLD						
	 OLD = NTEK							
	 NTEK = NTEK + REGION(NTEK+1) + 2				
	 IF ( NTEK .LT. NEWFRE ) GO TO 64				
63	 OLDFRE = OLD							
	 KDIMX = NREG - NEWFRE - 1					
C KOçÁAEM								
	GO TO METKA,(17,41)						
668	GO TO 666							
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
666	 CONTINUE							
C!!!-----ABAPàâHAü èEÁATú-----------------------------------------------
	 IZZZZZ=0							
	 IZZZZZ=1/IZZZZZ						
C									
C!!!------------------------------------------------------------------- 
	 CALL DUMP(REGION(1),REGION(NREG),0,				
     *		   PRT(1),PRT(NPRT),0)					
C********************************************************************** 
	STOP 666							
C********************************************************************** 
C!!!------------------------------------------------------------------- 
C	 DEBUG INIT,	     SUBTRACE					
C!!!------------------------------------------------------------------- 
	 END								
