	 SUBROUTINE REGCPU (�OD, �DIM, MAS�OD, NBEG, IEROR )		REG00010
CXXXX --�OM��E�C "MS1" --> BEPC�� 1.0  (OT �E�A�P� 1986 �.)------------ REG00020
C      �O��PO�PAMA: REGCPU (�OD, �DIM, MAS�OD, NBEG, IEROR )		REG00030
C      -----------							REG00040
C									REG00050
C      �A��A�E��E: ���AM��EC�OE B��E�E��E �O�MACC�BOB �� O��E�O		REG00060
C      ----------  MACC�BA �AM�T� REGION;				REG00070
C		   MACC�B� O��CA�� B COMMON /MEMORY/ � /PONTER/		REG00080
C		     PEA���OE �O�-BO �AM�T� MO��O O��C�BAT� B		REG00090
C		   CO �PO�PAMME BLOC� DATE;				REG00100
C									REG00110
C      METO�:	   MO�����A��� "���" C �C�O���OBA��EM �O�A���O�O	REG00120
C      -----	   "CTE�OBO�O" �P�EMA C�OPA M�COPA, C BO�MO��OCT��	REG00130
C		    C�OPA M�COPA �O MEPE �EO�XO��MOCT�.			REG00140
C									REG00150
C     �APAMETP�:   �OD - B��OP �O�A O�C����BA���:			REG00160
C     ---------								REG00170
C		   �OD		 BXO���E	     B�XO���E		REG00180
C  �����A����POBAT�  0		   -			-		REG00190
C									REG00200
C  CO��AT� MACC�B    1		   �DIM		      MAS�OD, NBEG,	REG00210
C						       IEROR ( �DIM )	REG00220
C  ��PAT� MACC�B     2		   MAS�OD, �DIM,       IEROR		REG00230
C				   NBEG					REG00240
C  �TO獈T� MECTO    3		    MAS�OD		�DIM,NBEG,	REG00250
C							  IEROR		REG00260
C  CO�PAT� M�COP     4		       -		 �DIM, IEROR	REG00270
C									REG00280
C    �DIM - PA�MEP�OCT� MACC�BA ( B C�OBAX )				REG00290
C	  ( MAX. �OCT���A� �AM�T� �P� IEROR = 2 );			REG00300
C									REG00310
C    MAS�OD - ���P ( �O� ) MAC�BA ( ��A�ATE�� B PRT );			REG00320
C									REG00330
C    NBEG - �OMEP �A�A�A MACC�BA B REGION ( A�PEC �A�A�A );		REG00340
C									REG00350
C    IEROR - �OD O�����:						REG00360
C			 0 - �E�CTB�E B��O��E�O;			REG00370
C			 1 - �E�CTB�E B��O��E�O, �O CO��PA�� M�COP;	REG00380
C			     ( MACC�B� ����A�TC� B �TO�E��� NBEG )	REG00390
C			 2 - �E�CTB� �E B��O��E�O - O����� O�PA�E���	REG00400
C			      ��� �ET �AM�T� ( MAX. B �DIM )		REG00410
C									REG00420
C									REG00430
C      �O��PO�PAMM�: �ET  ( SUBERR ) COMMON: MEMORY, PONTER (PRINTN)	REG00440
C      -------------							REG00450
C									REG00460
C      �P�ME�A��E:							REG00470
C      -----------							REG00480
C	  �OC�E B��E�E���, MACC�B REGION(NBEG) �����O� �DIM O�PE�E�E�;	REG00490
C	  �OCT�� �EPE� PRT:   NBEG = PRT(MAS�OD) - �TO BCE��A !!!!!!!;	REG00500
C	  MACC�B� O��O��A�O O�PE�E�E�� CBO�M ���POM MAS�OD;		REG00510
C									REG00520
CZZZ------------------------------------------------------------------- REG00530
       COMMON /MEMORY/ NREG,REGION(1)					REG00540
C									REG00550
       COMMON /PONTER/ NPRT, PRT(1)					REG00560
C									REG00570
	 COMMON /PRINTN/ NW6						REG00580
C									REG00590
       INTEGER REGION,PRT,OLDFRE, BEDREG,ADRMAS,OLD			REG00600
C									REG00610
C!!!------------------------------------------------------------------- REG00620
500    FORMAT('  BXO�: �OD=', I2, '; �DIM=', I4, '; MAS�OD=', I4,	REG00630
     *	      '; NBEG=', I4, '; IE=', I2 )				REG00640
501    FORMAT(' B�XO�: �OD=', I2, '; �DIM=', I4, '; MAS�OD=', I4,	REG00650
     *	      '; NBEG=', I4, '; IE=', I2 )				REG00660
C									REG00670
C      WRITE (NW6,500 ) �OD, �DIM, MAS�OD, NBEG, IEROR			REG00680
502    FORMAT(' NEWFRE=', I4, '; OLDFRE=', I4, '; NEWPRT=',		REG00690
     *	       I4, '; BEDREG=', I2, '; ARDMAS=', I4, '; NEW1=',		REG00700
     *	       I4   )							REG00710
C!!!------------------------------------------------------------------- REG00720
       GO TO ( 1, 2, 3, 4 ),�OD						REG00730
C---------------------------------------------------------------------- REG00740
C  �����A���A���							REG00750
       NEWFRE= 1							REG00760
       OLDFRE = 0							REG00770
       NEWPRT = 1							REG00780
       BEDREG = 0							REG00790
       IEROR = 0							REG00800
C---------------------------------------------------------------------- REG00810
C!!!  �CT�A	 MACC�BOB						REG00820
	 DO 120 I=1,NPRT						REG00830
120	 PRT(I)=0							REG00840
	 DO 121 I=1,NREG						REG00850
121	 REGION(I)=0							REG00860
C!!!-----------------							REG00870
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG00880
C      WRITE (NW6,501) �OD, �DIM, MAS�OD, NBEG, IEROR			REG00890
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG00900
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG00910
       RETURN								REG00920
C---------------------------------------------------------------------- REG00930
C********************************************************************** REG00940
C---------------------------------------------------------------------- REG00950
C    CO��AT� MACC�B							REG00960
1      CONTINUE								REG00970
       IEROR=0								REG00980
       IF ( NEWPRT .LE. NPRT ) GO TO 10					REG00990
C    �ET CBO�O��O�O MECTA B PRT, �C�AT� PA�EE ��A�E����			REG01000
       IPRT=1								REG01010
12     IF ( PRT(IPRT) .EQ. 0 ) GO TO 11					REG01020
       IPRT=IPRT+1							REG01030
       IF ( IPRT .LE. NPRT ) GO TO 12					REG01040
C   *****  O����A:  �ET MECTA B PRT *********************************** REG01050
C!!!------------------------------------------------------------------- REG01060
       WRITE(NW6,13) NPRT						REG01070
13     FORMAT(' *****  O����A REGCPU: MA� PRT, NPRT=',I10)		REG01080
C!!!------------------------------------------------------------------- REG01090
       GO TO 666							REG01100
C   MECTO B PRT �A���							REG01110
11     MAS�OD = IPRT							REG01120
       GO TO 14								REG01130
C   CBO�O��OE MECTO B PRT ECT�						REG01140
10     MAS�OD = NEWPRT							REG01150
       NEWPRT = NEWPRT + 1						REG01160
C  B��E��M �BE�O B REGION						REG01170
14     NEW1 = NEWFRE + �DIM + 2						REG01180
C  XBAT�T �� MECTA B REGION						REG01190
16     IF ( NEW1 .LE. NREG ) GO TO 15					REG01200
C  MECTA �E XBAT��O, �O�PO��EM CO�PAT� M�COP......			REG01210
       IEROR=1								REG01220
       ASSIGN 17 TO MET�A						REG01230
       GO TO 45								REG01240
17     CONTINUE								REG01250
C  M�COP CO�PA��, ECT� �� �AM�T� ?					REG01260
       IF ((NEWFRE + �DIM  + 2) .LE. NREG ) GO TO 14			REG01270
C    �AM�T� BCE PAB�O �ET						REG01280
       �DIM  = �DIMX							REG01290
       IEROR = 2							REG01300
C---------------------------------------------------------------------- REG01310
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01320
C      WRITE (NW6,501) �OD, �DIM, MAS�OD, NBEG, IEROR			REG01330
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG01340
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01350
       RETURN								REG01360
C---------------------------------------------------------------------- REG01370
15     CONTINUE								REG01380
C  �AM�T� ECT�, �A�O���M ��A�ATE�� �BE�A � PRT				REG01390
       REGION( NEWFRE )=OLDFRE						REG01400
       REGION( NEWFRE + 1 ) = �DIM					REG01410
       NBEG = NEWFRE +	2						REG01420
       PRT ( MAS�OD ) = NBEG						REG01430
C   O��OB�M ��O�A����E ��A�ATE��					REG01440
       OLDFRE = NEWFRE							REG01450
       NEWFRE = NEW1							REG01460
C---------------------------------------------------------------------- REG01470
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01480
C      WRITE (NW6,501) �OD, �DIM, MAS�OD, NBEG, IEROR			REG01490
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG01500
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01510
       RETURN								REG01520
C---------------------------------------------------------------------- REG01530
C********************************************************************** REG01540
C---------------------------------------------------------------------- REG01550
C    ��PAT� MACC�B							REG01560
2      CONTINUE								REG01570
C   �POBEP�� COOTBETCTB��						REG01580
       IF ( MAS�OD .GE. 1  .AND.					REG01590
     *	    MAS�OD .LE. NPRT  ) GO TO 20				REG01600
C*** O����A ��A�E��� ************************************************** REG01610
C!!!------------------------------------------------------------------- REG01620
       WRITE(NW6,21) MAS�OD						REG01630
21     FORMAT(' ***** O����A REGCPU: �EBEPE� MAS�OD=',I10)		REG01640
C!!!------------------------------------------------------------------- REG01650
       GO TO 666							REG01660
20     ADRMAS= PRT(MAS�OD)						REG01670
       IF ( (ADRMAS-2) .GE. 1	.AND.					REG01680
     *	    (ADRMAS-2) .LE. NEWFRE  )  GO TO 22				REG01690
C*** O����A B PRT ***************************************************** REG01700
C!!!------------------------------------------------------------------- REG01710
       WRITE(NW6,23) ADRMAS						REG01720
23     FORMAT(' ***** O����A REGCPU: ADRMAS=',I10)			REG01730
C!!!------------------------------------------------------------------- REG01740
       GO TO 666							REG01750
22     IF ( REGION(ADRMAS-1)  .EQ.  �DIM  )  GO TO 24			REG01760
C***  O����A B REGION  ************************************************ REG01770
C!!!------------------------------------------------------------------- REG01780
       WRITE(NW6,25)  REGION(ADRMAS-1)					REG01790
25     FORMAT(' ***** O����A ����� �� REGION=',I10)			REG01800
C!!!------------------------------------------------------------------- REG01810
       GO TO 666							REG01820
24     IF ( ADRMAS .EQ. NBEG ) GO TO 26					REG01830
C*** O����A �A�A�A ��A�E���  ****************************************** REG01840
C!!!------------------------------------------------------------------- REG01850
       WRITE(NW6,27) NBEG						REG01860
27     FORMAT(' ***** O����A REGCPU: NBEG=',I10)			REG01870
C!!!------------------------------------------------------------------- REG01880
       GO TO 666							REG01890
26     CONTINUE								REG01900
C   ��A�E��E B PRT							REG01910
       PRT(MAS�OD) = 0							REG01920
C �O�A���A� �C�A PRT,, EC�� MO��O					REG01930
       IF ( MAS�OD .EQ. 1 ) GO TO 28					REG01940
       IF ( MAS�OD .NE.  (NEWPRT -1) ) GO TO 28				REG01950
C  �CT�M �O�A���O							REG01960
29     NEWPRT = NEWPRT -1						REG01970
       IF ( NEWPRT .EQ. 1 ) GO TO 28					REG01980
       IF ( PRT(NEWPRT) .NE. 0 ) GO TO 28				REG01990
       IF ( NEWPRT .EQ. 1 ) GO TO 28					REG02000
       GO TO 29								REG02010
28     CONTINUE								REG02020
C  ��A��EM B REGION: MO�ET ��T� MO��O �O CTE�-A��OP�TM�?		REG02030
       IF (  (ADRMAS-2) .EQ. OLDFRE ) GO TO 30				REG02040
C      �ET, �POCTO �OMET�M ��A�E���� ��O�				REG02050
       REGION(ADRMAS-1) = -REGION(ADRMAS-1)				REG02060
C  �A���C�P�EM �A���E M�COPA						REG02070
       BEDREG = BEDREG + 1						REG02080
31     IEROR=0								REG02090
C---------------------------------------------------------------------- REG02100
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02110
C      WRITE (NW6,501) �OD, �DIM, MAS�OD, NBEG, IEROR			REG02120
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG02130
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02140
       RETURN								REG02150
C  �O�A���A� �C�A REGION ( �O CTE�-A��OP�TM�)				REG02160
30     NEWFRE = OLDFRE							REG02170
C  �PE������� �� B��PAC�BAEMO�O �BE�A					REG02180
       OLDFRE = REGION(OLDFRE)						REG02190
C  �OPO��EM �PO�O���T� �O�A����� C�OP�� M�COPA				REG02200
       IF ( REGION(OLDFRE+1) .GE. 0 )  GO TO 31				REG02210
C  MO��O �CT�T�							REG02220
       BEDREG = BEDREG - 1						REG02230
       GO TO 30								REG02240
C---------------------------------------------------------------------- REG02250
C********************************************************************** REG02260
C---------------------------------------------------------------------- REG02270
C �O���T� C�PAB��							REG02280
3	 CONTINUE							REG02290
	 IF ( MAS�OD . GE. 1  .AND.					REG02300
     *	      MAS�OD . LE. NEWPRT  ) GO TO 300				REG02310
C*** �AP��E��E �PA��� �O�A �P� C�PAB�E	******************************* REG02320
C!!!------------------------------------------------------------------- REG02330
	 WRITE(NW6,310) MAS�OD						REG02340
310	 FORMAT(' ***** O����A REGCPU: �TO�E��E MAS�OD=',I10)		REG02350
C!!!------------------------------------------------------------------- REG02360
	 GO TO 666							REG02370
300	 CONTINUE							REG02380
	 NBEG = PRT(MAS�OD)						REG02390
	 IF ( NBEG .GE. 3  .AND.					REG02400
     *	      NBEG .LE.   (OLDFRE+2) ) GO TO 32				REG02410
C***** �AP��E��E MAS�OD -> PRT(MAS�OD)	******************************* REG02420
C!!!------------------------------------------------------------------- REG02430
	 WRITE(NW6,33) MAS�OD, NBEG					REG02440
33	 FORMAT( ' ***** O����A REGCPU: MAS�OD=',I10,' NBEG=',I10)	REG02450
C!!!------------------------------------------------------------------- REG02460
	 GO TO 666							REG02470
32	 CONTINUE							REG02480
	 �DIM = REGION(NBEG-1)						REG02490
	 IEROR = 0							REG02500
C---------------------------------------------------------------------- REG02510
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02520
C      WRITE (NW6,501) �OD, �DIM, MAS�OD, NBEG, IEROR			REG02530
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG02540
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02550
	 RETURN								REG02560
C-----------------------------------------------------------------------REG02570
C***********************************************************************REG02580
C-----------------------------------------------------------------------REG02590
C    CO�PAT� M�COP:  B �DIMX - C�O���O �AM�T� CBO�O��O			REG02600
4	 CONTINUE							REG02610
	ASSIGN 41 TO MET�A						REG02620
	 GO TO 45							REG02630
41	 IEROR=1							REG02640
	 �DIM = �DIMX							REG02650
C---------------------------------------------------------------------- REG02660
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02670
C      WRITE (NW6,501) �OD, �DIM, MAS�OD, NBEG, IEROR			REG02680
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG02690
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02700
	RETURN								REG02710
45	CONTINUE							REG02720
C  ***** 1 - � �A�A, PA�MET�A M�COPA					REG02730
	IF ( BEDREG .GT. 0 ) GO TO 47					REG02740
C   CO��PAT� �E�E�O		$ ***					REG02750
	�DIMX = NREG - NEWFRE -2					REG02760
	GO TO MET�A, (17, 41)						REG02770
C!!!------------------------------------------------------------------- REG02780
667	 GO TO 666							REG02790
C!!!------------------------------------------------------------------- REG02800
47	CONTINUE							REG02810
	IPRT = 1							REG02820
C  �A�T� OT�P�T�� �O PRT � �OMET�T� B REGION ( �O�E OLDFRE)		REG02830
49	ADRMAS = PRT(IPRT)						REG02840
	IF ( ADRMAS .EQ. 0 ) GO TO 50					REG02850
	IF (  (ADRMAS-2) .GE. 1   .AND.					REG02860
     *	      (ADRMAS-2) .LE. OLDFRE  ) GO TO 51			REG02870
C!!!------------------------------------------------------------------- REG02880
	WRITE(NW6,52) ADRMAS, OLDFRE					REG02890
52	FORMAT(' ****** O����A REGCPU: ADRMAS=',I10,' OLDREF=',I10)	REG02900
C!!!------------------------------------------------------------------- REG02910
	GO TO 666							REG02920
C  PA�MET�A								REG02930
51	REGION(ADRMAS-2) = IPRT						REG02940
50	IPRT = IPRT + 1							REG02950
	IF ( IPRT .LE. NEWPRT ) GO TO 49				REG02960
C-------								REG02970
C  2-� �A�A								REG02980
C-------								REG02990
	 NFIX = 1							REG03000
	 NTE� = 1							REG03010
C  ����A TE�. ��O�A							REG03020
53	 �DIMX = REGION(NTE�+1)						REG03030
	 IF ( �DIMX .GE. 0 ) GO TO 54					REG03040
C  ��O� B��EP���T, E�O �PO��CT�T�					REG03050
	 NTE� = NTE� + IABS(�DIMX) + 2					REG03060
C   �ME����T� �O�TPO����� C�. B��EP���T�X ��O�OB			REG03070
	 BEDREG = BEDREG - 1						REG03080
C---�O��OB�A ����A							REG03090
55	 IF ( NTE� .LT. NEWFRE ) GO TO 53				REG03100
	 GO TO 60							REG03110
C  O�PA�OT�A XOPO�E�O ��O�A, ���C�P�EM E�O �A�A�O			REG03120
54	NMOVE = NTE�							REG03130
C  A�P. C�E�.								REG03140
	NTE� = NTE� + �DIMX + 2						REG03150
C  ����O �� �����EC�� �EPEC��AT�					REG03160
	IF ( NFIX .EQ. NMOVE ) GO TO 56					REG03170
C �EPEC��AEM, �E C����C� ?						REG03180
	 IF ( NFIX .LT. NMOVE ) GO TO 57				REG03190
C**** C����C� ********************************************************* REG03200
C!!!------------------------------------------------------------------- REG03210
	 WRITE(NW6,58) NFIX, NMOVE					REG03220
58	 FORMAT(' ***** C�O� B REGCPU: NFIX=',I10,' NMOVN=',I10)	REG03230
C!!!------------------------------------------------------------------- REG03240
	 GO TO 666							REG03250
57	 CONTINUE							REG03260
	 IFF = 0							REG03270
59	 REGION( NFIX+IFF ) = REGION( NMOVE+IFF )			REG03280
	 IFF = IFF + 1							REG03290
	 IF ( IFF .LT. �DIMX+2 ) GO TO 59				REG03300
C  �OPE�. ��A�ATE�� �A��T�X						REG03310
56	 NFIX = NFIX + �DIMX + 2					REG03320
	 GO TO 55							REG03330
C  �O�E� 2-� �A��:							REG03340
60	 NEWFRE = NFIX							REG03350
C  CAMO�POBEP�A:							REG03360
	 IF ( BEDREG .EQ. 0 ) GO TO 62					REG03370
C!!!------------------------------------------------------------------- REG03380
	 WRITE(NW6,61) BEDREG						REG03390
61	 FORMAT(' ***** O����A REGCPU: �TO� BEDREG=',I10)		REG03400
C!!!------------------------------------------------------------------- REG03410
	 GO TO 666							REG03420
C-------								REG03430
C   3-� �A�A								REG03440
C-------								REG03450
C   PACTAB�M ��A�ATE��							REG03460
62	 OLD=0								REG03470
C--------IF ( NEWFRE .EQ. 1 ) GO TO 63					REG03480
	 NTE� = 1							REG03490
C   B��PAT� �O� MAC. ( MECTO B PRT )					REG03500
64	 IPP = REGION(NTE�)						REG03510
C   �POBEP�TC�								REG03520
	 IF ( IPP .GE. 1  .AND.						REG03530
     *	      IPP .LE.	  NPRT ) GO TO 65				REG03540
C!!!------------------------------------------------------------------- REG03550
	 WRITE(NW6,66) IPP, NTE�					REG03560
66	 FORMAT(' ***** C�O� REGCPU: IPP=',I10,' NTE�=',I10)		REG03570
C!!!------------------------------------------------------------------- REG03580
	 GO TO 666							REG03590
C   BOCTA�OB�T� B PRT							REG03600
65	 PRT(IPP) = NTE� + 2						REG03610
	 REGION(NTE�) = OLD						REG03620
	 OLD = NTE�							REG03630
	 NTE� = NTE� + REGION(NTE�+1) + 2				REG03640
	 IF ( NTE� .LT. NEWFRE ) GO TO 64				REG03650
63	 OLDFRE = OLD							REG03660
	 �DIMX = NREG - NEWFRE - 1					REG03670
C �O��AEM								REG03680
	GO TO MET�A,(17,41)						REG03690
668	GO TO 666							REG03700
C-----------------------------------------------------------------------REG03710
C-----------------------------------------------------------------------REG03720
C-----------------------------------------------------------------------REG03730
666	 CONTINUE							REG03740
C!!!-----ABAP���A� �E�AT�-----------------------------------------------REG03750
	 IZZZZZ=0							REG03760
	 IZZZZZ=1/IZZZZZ						REG03770
C									REG03780
C!!!------------------------------------------------------------------- REG03790
	 CALL DUMP(REGION(1),REGION(NREG),0,				REG03800
     *		   PRT(1),PRT(NPRT),0)					REG03810
C********************************************************************** REG03820
	STOP 666							REG03830
C********************************************************************** REG03840
C!!!------------------------------------------------------------------- REG03850
C	 DEBUG INIT,	     SUBTRACE					REG03860
C!!!------------------------------------------------------------------- REG03870
	 END								REG03880
