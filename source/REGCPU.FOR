	 SUBROUTINE REGCPU (KOD, KDIM, MASKOD, NBEG, IEROR )		
CXXXX --�OM��E�C "MS1" --> BEPC�� 1.0  (OT �E�A�P� 1986 �.)------------ 
C      �O��PO�PAMA: REGCPU (�OD, �DIM, MAS�OD, NBEG, IEROR )		
C      -----------							
C									
C      �A��A�E��E: ���AM��EC�OE B��E�E��E �O�MACC�BOB �� O��E�O		
C      ----------  MACC�BA �AM�T� REGION;				
C		   MACC�B� O��CA�� B COMMON /MEMORY/ � /PONTER/		
C		     PEA���OE �O�-BO �AM�T� MO��O O��C�BAT� B		
C		   CO �PO�PAMME BLOC� DATE;				
C									
C      METO�:	   MO�����A��� "���" C �C�O���OBA��EM �O�A���O�O	
C      -----	   "CTE�OBO�O" �P�EMA C�OPA M�COPA, C BO�MO��OCT��	
C		    C�OPA M�COPA �O MEPE �EO�XO��MOCT�.			
C									
C     �APAMETP�:   KOD - B��OP KO�A O�C����BA���:			
C     ---------								
C		   KOD		 BXO���E	     B�XO���E		
C  �����A����POBAT�  0		   -			-		
C									
C  CO��AT� MACC�B    1		   KDIM		      MASKOD, NBEG,	
C						       IEROR ( KDIM )	
C  ��PAT� MACC�B     2		   MASKOD, KDIM,       IEROR		
C				   NBEG					
C  �TO獈T� MECTO    3		    MASKOD		KDIM,NBEG,	
C							  IEROR		
C  CO�PAT� M�COP     4		       -		 KDIM, IEROR	
C									
C    KDIM - PA�MEP�OCT� MACC�BA ( B C�OBAX )				
C	  ( MAX. �OCT���A� �AM�T� �P� IEROR = 2 );			
C									
C    MASKOD - ���P ( KO� ) MAC�BA ( �KA�ATE�� B PRT );			
C									
C    NBEG - �OMEP �A�A�A MACC�BA B REGION ( A�PEC �A�A�A );		
C									
C    IEROR - KOD O���K�:						
C			 0 - �E�CTB�E B��O��E�O;			
C			 1 - �E�CTB�E B��O��E�O, �O CO��PA�� M�COP;	
C			     ( MACC�B� ����A�TC� B �TO�E��� NBEG )	
C			 2 - �E�CTB� �E B��O��E�O - O���K� O�PA�E���	
C			      ��� �ET �AM�T� ( MAX. B KDIM )		
C									
C									
C      �O��PO�PAMM�: �ET  ( SUBERR ) COMMON: MEMORY, PONTER (PRINTN)	
C      -------------							
C									
C      �P�ME�A��E:							
C      -----------							
C	  �OC�E B��E�E���, MACC�B REGION(NBEG) �����O� KDIM O�PE�E�E�;	
C	  �OCT�� �EPE� PRT:   NBEG = PRT(MASKOD) - �TO BCE��A !!!!!!!;	
C	  MACC�B� O��O��A�O O�PE�E�E�� CBO�M ���POM MASKOD;		
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
500    FORMAT('  BXO�: KOD=', I2, '; KDIM=', I4, '; MASKOD=', I4,	
     *	      '; NBEG=', I4, '; IE=', I2 )				
501    FORMAT(' B�XO�: KOD=', I2, '; KDIM=', I4, '; MASKOD=', I4,	
     *	      '; NBEG=', I4, '; IE=', I2 )				
C									
C      WRITE (NW6,500 ) KOD, KDIM, MASKOD, NBEG, IEROR			
502    FORMAT(' NEWFRE=', I4, '; OLDFRE=', I4, '; NEWPRT=',		
     *	       I4, '; BEDREG=', I2, '; ARDMAS=', I4, '; NEW1=',		
     *	       I4   )							
C!!!------------------------------------------------------------------- 
       GO TO ( 1, 2, 3, 4 ),KOD						
C---------------------------------------------------------------------- 
C  �����A���A���							
       NEWFRE= 1							
       OLDFRE = 0							
       NEWPRT = 1							
       BEDREG = 0							
       IEROR = 0							
C---------------------------------------------------------------------- 
C!!!  �CTKA	 MACC�BOB						
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
C    CO��AT� MACC�B							
1      CONTINUE								
       IEROR=0								
       IF ( NEWPRT .LE. NPRT ) GO TO 10					
C    �ET CBO�O��O�O MECTA B PRT, �CKAT� PA�EE ��A�E����			
       IPRT=1								
12     IF ( PRT(IPRT) .EQ. 0 ) GO TO 11					
       IPRT=IPRT+1							
       IF ( IPRT .LE. NPRT ) GO TO 12					
C   *****  O���KA:  �ET MECTA B PRT *********************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,13) NPRT						
13     FORMAT(' *****  O���KA REGCPU: MA� PRT, NPRT=',I10)		
C!!!------------------------------------------------------------------- 
       GO TO 666							
C   MECTO B PRT �A���							
11     MASKOD = IPRT							
       GO TO 14								
C   CBO�O��OE MECTO B PRT ECT�						
10     MASKOD = NEWPRT							
       NEWPRT = NEWPRT + 1						
C  B��E��M �BE�O B REGION						
14     NEW1 = NEWFRE + KDIM + 2						
C  XBAT�T �� MECTA B REGION						
16     IF ( NEW1 .LE. NREG ) GO TO 15					
C  MECTA �E XBAT��O, �O�PO��EM CO�PAT� M�COP......			
       IEROR=1								
       ASSIGN 17 TO METKA						
       GO TO 45								
17     CONTINUE								
C  M�COP CO�PA��, ECT� �� �AM�T� ?					
       IF ((NEWFRE + KDIM  + 2) .LE. NREG ) GO TO 14			
C    �AM�T� BCE PAB�O �ET						
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
C  �AM�T� ECT�, �A�O���M �KA�ATE�� �BE�A � PRT				
       REGION( NEWFRE )=OLDFRE						
       REGION( NEWFRE + 1 ) = KDIM					
       NBEG = NEWFRE +	2						
       PRT ( MASKOD ) = NBEG						
C   O��OB�M ��O�A����E �KA�ATE��					
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
C    ��PAT� MACC�B							
2      CONTINUE								
C   �POBEPK� COOTBETCTB��						
       IF ( MASKOD .GE. 1  .AND.					
     *	    MASKOD .LE. NPRT  ) GO TO 20				
C*** O���KA ��A�E��� ************************************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,21) MASKOD						
21     FORMAT(' ***** O���KA REGCPU: �EBEPE� MASKOD=',I10)		
C!!!------------------------------------------------------------------- 
       GO TO 666							
20     ADRMAS= PRT(MASKOD)						
       IF ( (ADRMAS-2) .GE. 1	.AND.					
     *	    (ADRMAS-2) .LE. NEWFRE  )  GO TO 22				
C*** O���KA B PRT ***************************************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,23) ADRMAS						
23     FORMAT(' ***** O���KA REGCPU: ADRMAS=',I10)			
C!!!------------------------------------------------------------------- 
       GO TO 666							
22     IF ( REGION(ADRMAS-1)  .EQ.  KDIM  )  GO TO 24			
C***  O���KA B REGION  ************************************************ 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,25)  REGION(ADRMAS-1)					
25     FORMAT(' ***** O���KA ����� �� REGION=',I10)			
C!!!------------------------------------------------------------------- 
       GO TO 666							
24     IF ( ADRMAS .EQ. NBEG ) GO TO 26					
C*** O���KA �A�A�A ��A�E���  ****************************************** 
C!!!------------------------------------------------------------------- 
       WRITE(NW6,27) NBEG						
27     FORMAT(' ***** O���KA REGCPU: NBEG=',I10)			
C!!!------------------------------------------------------------------- 
       GO TO 666							
26     CONTINUE								
C   ��A�E��E B PRT							
       PRT(MASKOD) = 0							
C �OKA���A� �CKA PRT,, EC�� MO��O					
       IF ( MASKOD .EQ. 1 ) GO TO 28					
       IF ( MASKOD .NE.  (NEWPRT -1) ) GO TO 28				
C  �CT�M �OKA���O							
29     NEWPRT = NEWPRT -1						
       IF ( NEWPRT .EQ. 1 ) GO TO 28					
       IF ( PRT(NEWPRT) .NE. 0 ) GO TO 28				
       IF ( NEWPRT .EQ. 1 ) GO TO 28					
       GO TO 29								
28     CONTINUE								
C  ��A��EM B REGION: MO�ET ��T� MO��O �O CTEK-A��OP�TM�?		
       IF (  (ADRMAS-2) .EQ. OLDFRE ) GO TO 30				
C      �ET, �POCTO �OMET�M ��A�E���� ��OK				
       REGION(ADRMAS-1) = -REGION(ADRMAS-1)				
C  �A��KC�P�EM �A���E M�COPA						
       BEDREG = BEDREG + 1						
31     IEROR=0								
C---------------------------------------------------------------------- 
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
C      WRITE (NW6,501) KOD, KDIM, MASKOD, NBEG, IEROR			
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ 
       RETURN								
C  �OKA���A� �CKA REGION ( �O CTEK-A��OP�TM�)				
30     NEWFRE = OLDFRE							
C  �PE������� �� B��PAC�BAEMO�O �BE�A					
       OLDFRE = REGION(OLDFRE)						
C  �OPO��EM �PO�O���T� �OKA����� C�OPK� M�COPA				
       IF ( REGION(OLDFRE+1) .GE. 0 )  GO TO 31				
C  MO��O �CT�T�							
       BEDREG = BEDREG - 1						
       GO TO 30								
C---------------------------------------------------------------------- 
C********************************************************************** 
C---------------------------------------------------------------------- 
C �O���T� C�PABK�							
3	 CONTINUE							
	 IF ( MASKOD . GE. 1  .AND.					
     *	      MASKOD . LE. NEWPRT  ) GO TO 300				
C*** �AP��E��E �PA��� KO�A �P� C�PABKE	******************************* 
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,310) MASKOD						
310	 FORMAT(' ***** O���KA REGCPU: �TO�E��E MASKOD=',I10)		
C!!!------------------------------------------------------------------- 
	 GO TO 666							
300	 CONTINUE							
	 NBEG = PRT(MASKOD)						
	 IF ( NBEG .GE. 3  .AND.					
     *	      NBEG .LE.   (OLDFRE+2) ) GO TO 32				
C***** �AP��E��E MASKOD -> PRT(MASKOD)	******************************* 
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,33) MASKOD, NBEG					
33	 FORMAT( ' ***** O���KA REGCPU: MASKOD=',I10,' NBEG=',I10)	
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
C    CO�PAT� M�COP:  B KDIMX - CKO��KO �AM�T� CBO�O��O			
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
C  ***** 1 - � �A�A, PA�METKA M�COPA					
	IF ( BEDREG .GT. 0 ) GO TO 47					
C   CO��PAT� �E�E�O		$ ***					
	KDIMX = NREG - NEWFRE -2					
	GO TO METKA, (17, 41)						
C!!!------------------------------------------------------------------- 
667	 GO TO 666							
C!!!------------------------------------------------------------------- 
47	CONTINUE							
	IPRT = 1							
C  �A�T� OTKP�T�� �O PRT � �OMET�T� B REGION ( �O�E OLDFRE)		
49	ADRMAS = PRT(IPRT)						
	IF ( ADRMAS .EQ. 0 ) GO TO 50					
	IF (  (ADRMAS-2) .GE. 1   .AND.					
     *	      (ADRMAS-2) .LE. OLDFRE  ) GO TO 51			
C!!!------------------------------------------------------------------- 
	WRITE(NW6,52) ADRMAS, OLDFRE					
52	FORMAT(' ****** O���KA REGCPU: ADRMAS=',I10,' OLDREF=',I10)	
C!!!------------------------------------------------------------------- 
	GO TO 666							
C  PA�METKA								
51	REGION(ADRMAS-2) = IPRT						
50	IPRT = IPRT + 1							
	IF ( IPRT .LE. NEWPRT ) GO TO 49				
C-------								
C  2-� �A�A								
C-------								
	 NFIX = 1							
	 NTEK = 1							
C  ����A TEK. ��OKA							
53	 KDIMX = REGION(NTEK+1)						
	 IF ( KDIMX .GE. 0 ) GO TO 54					
C  ��OK B��EPK��T, E�O �PO��CT�T�					
	 NTEK = NTEK + IABS(KDIMX) + 2					
C   �ME����T� KO�TPO����� C�K. B��EPK��T�X ��OKOB			
	 BEDREG = BEDREG - 1						
C---KO��OBKA ��K�A							
55	 IF ( NTEK .LT. NEWFRE ) GO TO 53				
	 GO TO 60							
C  O�PA�OTKA XOPO�E�O ��OKA, ��KC�P�EM E�O �A�A�O			
54	NMOVE = NTEK							
C  A�P. C�E�.								
	NTEK = NTEK + KDIMX + 2						
C  ����O �� �����ECK� �EPEC��AT�					
	IF ( NFIX .EQ. NMOVE ) GO TO 56					
C �EPEC��AEM, �E C����C� ?						
	 IF ( NFIX .LT. NMOVE ) GO TO 57				
C**** C����C� ********************************************************* 
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,58) NFIX, NMOVE					
58	 FORMAT(' ***** C�O� B REGCPU: NFIX=',I10,' NMOVN=',I10)	
C!!!------------------------------------------------------------------- 
	 GO TO 666							
57	 CONTINUE							
	 IFF = 0							
59	 REGION( NFIX+IFF ) = REGION( NMOVE+IFF )			
	 IFF = IFF + 1							
	 IF ( IFF .LT. KDIMX+2 ) GO TO 59				
C  KOPEK. �KA�ATE�� �A��T�X						
56	 NFIX = NFIX + KDIMX + 2					
	 GO TO 55							
C  KO�E� 2-� �A��:							
60	 NEWFRE = NFIX							
C  CAMO�POBEPKA:							
	 IF ( BEDREG .EQ. 0 ) GO TO 62					
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,61) BEDREG						
61	 FORMAT(' ***** O���KA REGCPU: �TO� BEDREG=',I10)		
C!!!------------------------------------------------------------------- 
	 GO TO 666							
C-------								
C   3-� �A�A								
C-------								
C   PACTAB�M �KA�ATE��							
62	 OLD=0								
C--------IF ( NEWFRE .EQ. 1 ) GO TO 63					
	 NTEK = 1							
C   B��PAT� KO� MAC. ( MECTO B PRT )					
64	 IPP = REGION(NTEK)						
C   �POBEP�TC�								
	 IF ( IPP .GE. 1  .AND.						
     *	      IPP .LE.	  NPRT ) GO TO 65				
C!!!------------------------------------------------------------------- 
	 WRITE(NW6,66) IPP, NTEK					
66	 FORMAT(' ***** C�O� REGCPU: IPP=',I10,' NTEK=',I10)		
C!!!------------------------------------------------------------------- 
	 GO TO 666							
C   BOCTA�OB�T� B PRT							
65	 PRT(IPP) = NTEK + 2						
	 REGION(NTEK) = OLD						
	 OLD = NTEK							
	 NTEK = NTEK + REGION(NTEK+1) + 2				
	 IF ( NTEK .LT. NEWFRE ) GO TO 64				
63	 OLDFRE = OLD							
	 KDIMX = NREG - NEWFRE - 1					
C KO��AEM								
	GO TO METKA,(17,41)						
668	GO TO 666							
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
666	 CONTINUE							
C!!!-----ABAP��HA� �E�AT�-----------------------------------------------
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
