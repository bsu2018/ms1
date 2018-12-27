	 SUBROUTINE REGCPU (äOD, äDIM, MASäOD, NBEG, IEROR )		REG00010
CXXXX --äOMèãEäC "MS1" --> BEPCàü 1.0  (OT ÑEäAÅPü 1986 É.)------------ REG00020
C      èOÑèPOÉPAMA: REGCPU (äOD, äDIM, MASäOD, NBEG, IEROR )		REG00030
C      -----------							REG00040
C									REG00050
C      çAáçAÁEçàE: ÑàçAMàÁECäOE BõÑEãEçàE èOÑMACCàBOB àá OÅôEÉO		REG00060
C      ----------  MACCàBA èAMüTà REGION;				REG00070
C		   MACCàBõ OèàCAçõ B COMMON /MEMORY/ à /PONTER/		REG00080
C		     PEAãúçOE äOã-BO èAMüTà MOÜçO OèàCõBATú B		REG00090
C		   CO èPOÉPAMME BLOCä DATE;				REG00100
C									REG00110
C      METOÑ:	   MOÑàîàäAñàü "äìÁà" C àCèOãúáOBAçàEM ãOäAãúçOÉO	REG00120
C      -----	   "CTEäOBOÉO" èPàEMA CÅOPA MìCOPA, C BOáMOÜçOCTúû	REG00130
C		    CÅOPA MìCOPA èO MEPE çEOÅXOÑàMOCTà.			REG00140
C									REG00150
C     èAPAMETPõ:   äOD - BõÅOP äOÑA OÅCãìÜàBAçàü:			REG00160
C     ---------								REG00170
C		   äOD		 BXOÑçõE	     BõXOÑçõE		REG00180
C  àçàñàAãàáàPOBATú  0		   -			-		REG00190
C									REG00200
C  COáÑATú MACCàB    1		   äDIM		      MASäOD, NBEG,	REG00210
C						       IEROR ( äDIM )	REG00220
C  ìÅPATú MACCàB     2		   MASäOD, äDIM,       IEROR		REG00230
C				   NBEG					REG00240
C  ìTOÁçàTú MECTO    3		    MASäOD		äDIM,NBEG,	REG00250
C							  IEROR		REG00260
C  COÅPATú MìCOP     4		       -		 äDIM, IEROR	REG00270
C									REG00280
C    äDIM - PAáMEPçOCTú MACCàBA ( B CãOBAX )				REG00290
C	  ( MAX. ÑOCTìèçAü èAMüTú èPà IEROR = 2 );			REG00300
C									REG00310
C    MASäOD - òàîP ( äOÑ ) MACàBA ( ìäAáATEãú B PRT );			REG00320
C									REG00330
C    NBEG - çOMEP çAÁAãA MACCàBA B REGION ( AÑPEC çAÁAãA );		REG00340
C									REG00350
C    IEROR - äOD OòàÅäà:						REG00360
C			 0 - ÑEâCTBàE BõèOãçEçO;			REG00370
C			 1 - ÑEâCTBàE BõèOãçEçO, çO COÅàPAãà MìCOP;	REG00380
C			     ( MACCàBõ çìÜÑAûTCü B ìTOÁçEçàà NBEG )	REG00390
C			 2 - ÑEâCTBà çE BõèOãçEçO - OòàÅäà OÅPAôEçàü	REG00400
C			      àãà çET èAMüTà ( MAX. B äDIM )		REG00410
C									REG00420
C									REG00430
C      èOÑèPOÉPAMMõ: çET  ( SUBERR ) COMMON: MEMORY, PONTER (PRINTN)	REG00440
C      -------------							REG00450
C									REG00460
C      èPàMEÁAçàE:							REG00470
C      -----------							REG00480
C	  èOCãE BõÑEãEçàü, MACCàB REGION(NBEG) ÑãàççOâ äDIM OèPEÑEãEç;	REG00490
C	  ÑOCTìè ÁEPEá PRT:   NBEG = PRT(MASäOD) - ùTO BCEÉÑA !!!!!!!;	REG00500
C	  MACCàBõ OÑçOáçAÁçO OèPEÑEãEçõ CBOàM òàîPOM MASäOD;		REG00510
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
500    FORMAT('  BXOÑ: äOD=', I2, '; äDIM=', I4, '; MASäOD=', I4,	REG00630
     *	      '; NBEG=', I4, '; IE=', I2 )				REG00640
501    FORMAT(' BõXOÑ: äOD=', I2, '; äDIM=', I4, '; MASäOD=', I4,	REG00650
     *	      '; NBEG=', I4, '; IE=', I2 )				REG00660
C									REG00670
C      WRITE (NW6,500 ) äOD, äDIM, MASäOD, NBEG, IEROR			REG00680
502    FORMAT(' NEWFRE=', I4, '; OLDFRE=', I4, '; NEWPRT=',		REG00690
     *	       I4, '; BEDREG=', I2, '; ARDMAS=', I4, '; NEW1=',		REG00700
     *	       I4   )							REG00710
C!!!------------------------------------------------------------------- REG00720
       GO TO ( 1, 2, 3, 4 ),äOD						REG00730
C---------------------------------------------------------------------- REG00740
C  àçàñàAãàáAñàü							REG00750
       NEWFRE= 1							REG00760
       OLDFRE = 0							REG00770
       NEWPRT = 1							REG00780
       BEDREG = 0							REG00790
       IEROR = 0							REG00800
C---------------------------------------------------------------------- REG00810
C!!!  ÁàCTäA	 MACCàBOB						REG00820
	 DO 120 I=1,NPRT						REG00830
120	 PRT(I)=0							REG00840
	 DO 121 I=1,NREG						REG00850
121	 REGION(I)=0							REG00860
C!!!-----------------							REG00870
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG00880
C      WRITE (NW6,501) äOD, äDIM, MASäOD, NBEG, IEROR			REG00890
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG00900
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG00910
       RETURN								REG00920
C---------------------------------------------------------------------- REG00930
C********************************************************************** REG00940
C---------------------------------------------------------------------- REG00950
C    COáÑATú MACCàB							REG00960
1      CONTINUE								REG00970
       IEROR=0								REG00980
       IF ( NEWPRT .LE. NPRT ) GO TO 10					REG00990
C    çET CBOÅOÑçOÉO MECTA B PRT, àCäATú PAçEE ìÑAãEççõâ			REG01000
       IPRT=1								REG01010
12     IF ( PRT(IPRT) .EQ. 0 ) GO TO 11					REG01020
       IPRT=IPRT+1							REG01030
       IF ( IPRT .LE. NPRT ) GO TO 12					REG01040
C   *****  OòàÅäA:  çET MECTA B PRT *********************************** REG01050
C!!!------------------------------------------------------------------- REG01060
       WRITE(NW6,13) NPRT						REG01070
13     FORMAT(' *****  OòàÅäA REGCPU: MAã PRT, NPRT=',I10)		REG01080
C!!!------------------------------------------------------------------- REG01090
       GO TO 666							REG01100
C   MECTO B PRT çAòãà							REG01110
11     MASäOD = IPRT							REG01120
       GO TO 14								REG01130
C   CBOÅOÑçOE MECTO B PRT ECTú						REG01140
10     MASäOD = NEWPRT							REG01150
       NEWPRT = NEWPRT + 1						REG01160
C  BõÑEãàM áBEçO B REGION						REG01170
14     NEW1 = NEWFRE + äDIM + 2						REG01180
C  XBATàT ãà MECTA B REGION						REG01190
16     IF ( NEW1 .LE. NREG ) GO TO 15					REG01200
C  MECTA çE XBATàãO, èOèPOÅìEM COÅPATú MìCOP......			REG01210
       IEROR=1								REG01220
       ASSIGN 17 TO METäA						REG01230
       GO TO 45								REG01240
17     CONTINUE								REG01250
C  MìCOP COÅPAãà, ECTú ãà èAMüTú ?					REG01260
       IF ((NEWFRE + äDIM  + 2) .LE. NREG ) GO TO 14			REG01270
C    èAMüTà BCE PABçO çET						REG01280
       äDIM  = äDIMX							REG01290
       IEROR = 2							REG01300
C---------------------------------------------------------------------- REG01310
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01320
C      WRITE (NW6,501) äOD, äDIM, MASäOD, NBEG, IEROR			REG01330
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG01340
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01350
       RETURN								REG01360
C---------------------------------------------------------------------- REG01370
15     CONTINUE								REG01380
C  èAMüTú ECTú, áAèOãçàM ìäAáATEãà áBEçA à PRT				REG01390
       REGION( NEWFRE )=OLDFRE						REG01400
       REGION( NEWFRE + 1 ) = äDIM					REG01410
       NBEG = NEWFRE +	2						REG01420
       PRT ( MASäOD ) = NBEG						REG01430
C   OÅçOBàM ÉãOÅAãúçõE ìäAáATEãà					REG01440
       OLDFRE = NEWFRE							REG01450
       NEWFRE = NEW1							REG01460
C---------------------------------------------------------------------- REG01470
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01480
C      WRITE (NW6,501) äOD, äDIM, MASäOD, NBEG, IEROR			REG01490
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG01500
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG01510
       RETURN								REG01520
C---------------------------------------------------------------------- REG01530
C********************************************************************** REG01540
C---------------------------------------------------------------------- REG01550
C    ìÅPATú MACCàB							REG01560
2      CONTINUE								REG01570
C   èPOBEPäà COOTBETCTBàü						REG01580
       IF ( MASäOD .GE. 1  .AND.					REG01590
     *	    MASäOD .LE. NPRT  ) GO TO 20				REG01600
C*** OòàÅäA ìÑAãEçàü ************************************************** REG01610
C!!!------------------------------------------------------------------- REG01620
       WRITE(NW6,21) MASäOD						REG01630
21     FORMAT(' ***** OòàÅäA REGCPU: çEBEPEç MASäOD=',I10)		REG01640
C!!!------------------------------------------------------------------- REG01650
       GO TO 666							REG01660
20     ADRMAS= PRT(MASäOD)						REG01670
       IF ( (ADRMAS-2) .GE. 1	.AND.					REG01680
     *	    (ADRMAS-2) .LE. NEWFRE  )  GO TO 22				REG01690
C*** OòàÅäA B PRT ***************************************************** REG01700
C!!!------------------------------------------------------------------- REG01710
       WRITE(NW6,23) ADRMAS						REG01720
23     FORMAT(' ***** OòàÅäA REGCPU: ADRMAS=',I10)			REG01730
C!!!------------------------------------------------------------------- REG01740
       GO TO 666							REG01750
22     IF ( REGION(ADRMAS-1)  .EQ.  äDIM  )  GO TO 24			REG01760
C***  OòàÅäA B REGION  ************************************************ REG01770
C!!!------------------------------------------------------------------- REG01780
       WRITE(NW6,25)  REGION(ADRMAS-1)					REG01790
25     FORMAT(' ***** OòàÅäA Ñãàçõ àá REGION=',I10)			REG01800
C!!!------------------------------------------------------------------- REG01810
       GO TO 666							REG01820
24     IF ( ADRMAS .EQ. NBEG ) GO TO 26					REG01830
C*** OòàÅäA çAÁAãA ìÑAãEçàü  ****************************************** REG01840
C!!!------------------------------------------------------------------- REG01850
       WRITE(NW6,27) NBEG						REG01860
27     FORMAT(' ***** OòàÅäA REGCPU: NBEG=',I10)			REG01870
C!!!------------------------------------------------------------------- REG01880
       GO TO 666							REG01890
26     CONTINUE								REG01900
C   ìÑAãEçàE B PRT							REG01910
       PRT(MASäOD) = 0							REG01920
C ãOäAãúçAü ÁàCäA PRT,, ECãà MOÜçO					REG01930
       IF ( MASäOD .EQ. 1 ) GO TO 28					REG01940
       IF ( MASäOD .NE.  (NEWPRT -1) ) GO TO 28				REG01950
C  ÁàCTàM ãOäAãúçO							REG01960
29     NEWPRT = NEWPRT -1						REG01970
       IF ( NEWPRT .EQ. 1 ) GO TO 28					REG01980
       IF ( PRT(NEWPRT) .NE. 0 ) GO TO 28				REG01990
       IF ( NEWPRT .EQ. 1 ) GO TO 28					REG02000
       GO TO 29								REG02010
28     CONTINUE								REG02020
C  ìÑAãüEM B REGION: MOÜET ÅõTú MOÜçO èO CTEä-AãÉOPàTMì?		REG02030
       IF (  (ADRMAS-2) .EQ. OLDFRE ) GO TO 30				REG02040
C      çET, èPOCTO èOMETàM ìÑAãEççõâ ÅãOä				REG02050
       REGION(ADRMAS-1) = -REGION(ADRMAS-1)				REG02060
C  áAîàäCàPìEM çAãàÁàE MìCOPA						REG02070
       BEDREG = BEDREG + 1						REG02080
31     IEROR=0								REG02090
C---------------------------------------------------------------------- REG02100
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02110
C      WRITE (NW6,501) äOD, äDIM, MASäOD, NBEG, IEROR			REG02120
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG02130
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02140
       RETURN								REG02150
C  ãOäAãúçAü ÁàCäA REGION ( èO CTEä-AãÉOPàTMì)				REG02160
30     NEWFRE = OLDFRE							REG02170
C  èPEÑõÑìôàâ àá BõÅPACõBAEMOÉO áBEçA					REG02180
       OLDFRE = REGION(OLDFRE)						REG02190
C  èOPOÅìEM èPOÑOãÜàTú ãOäAãúçìû CÅOPäì MìCOPA				REG02200
       IF ( REGION(OLDFRE+1) .GE. 0 )  GO TO 31				REG02210
C  MOÜçO ÁàCTàTú							REG02220
       BEDREG = BEDREG - 1						REG02230
       GO TO 30								REG02240
C---------------------------------------------------------------------- REG02250
C********************************************************************** REG02260
C---------------------------------------------------------------------- REG02270
C èOãìÁàTú CèPABäì							REG02280
3	 CONTINUE							REG02290
	 IF ( MASäOD . GE. 1  .AND.					REG02300
     *	      MASäOD . LE. NEWPRT  ) GO TO 300				REG02310
C*** çAPìòEçàE ÉPAçàñ äOÑA èPà CèPABäE	******************************* REG02320
C!!!------------------------------------------------------------------- REG02330
	 WRITE(NW6,310) MASäOD						REG02340
310	 FORMAT(' ***** OòàÅäA REGCPU: ìTOÁçEçàE MASäOD=',I10)		REG02350
C!!!------------------------------------------------------------------- REG02360
	 GO TO 666							REG02370
300	 CONTINUE							REG02380
	 NBEG = PRT(MASäOD)						REG02390
	 IF ( NBEG .GE. 3  .AND.					REG02400
     *	      NBEG .LE.   (OLDFRE+2) ) GO TO 32				REG02410
C***** çAPìòEçàE MASäOD -> PRT(MASäOD)	******************************* REG02420
C!!!------------------------------------------------------------------- REG02430
	 WRITE(NW6,33) MASäOD, NBEG					REG02440
33	 FORMAT( ' ***** OòàÅäA REGCPU: MASäOD=',I10,' NBEG=',I10)	REG02450
C!!!------------------------------------------------------------------- REG02460
	 GO TO 666							REG02470
32	 CONTINUE							REG02480
	 äDIM = REGION(NBEG-1)						REG02490
	 IEROR = 0							REG02500
C---------------------------------------------------------------------- REG02510
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02520
C      WRITE (NW6,501) äOD, äDIM, MASäOD, NBEG, IEROR			REG02530
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG02540
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02550
	 RETURN								REG02560
C-----------------------------------------------------------------------REG02570
C***********************************************************************REG02580
C-----------------------------------------------------------------------REG02590
C    COÅPATú MìCOP:  B äDIMX - CäOãúäO èAMüTà CBOÅOÑçO			REG02600
4	 CONTINUE							REG02610
	ASSIGN 41 TO METäA						REG02620
	 GO TO 45							REG02630
41	 IEROR=1							REG02640
	 äDIM = äDIMX							REG02650
C---------------------------------------------------------------------- REG02660
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02670
C      WRITE (NW6,501) äOD, äDIM, MASäOD, NBEG, IEROR			REG02680
C      WRITE (NW6,502) NEWFRE, OLDFRE, NEWPRT, BEDREG, ADRMAS, NEW1	REG02690
C!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ REG02700
	RETURN								REG02710
45	CONTINUE							REG02720
C  ***** 1 - ü îAáA, PAáMETäA MìCOPA					REG02730
	IF ( BEDREG .GT. 0 ) GO TO 47					REG02740
C   COÅàPATú çEÁEÉO		$ ***					REG02750
	äDIMX = NREG - NEWFRE -2					REG02760
	GO TO METäA, (17, 41)						REG02770
C!!!------------------------------------------------------------------- REG02780
667	 GO TO 666							REG02790
C!!!------------------------------------------------------------------- REG02800
47	CONTINUE							REG02810
	IPRT = 1							REG02820
C  çAâTà OTäPõTõâ èO PRT à èOMETàTú B REGION ( èOãE OLDFRE)		REG02830
49	ADRMAS = PRT(IPRT)						REG02840
	IF ( ADRMAS .EQ. 0 ) GO TO 50					REG02850
	IF (  (ADRMAS-2) .GE. 1   .AND.					REG02860
     *	      (ADRMAS-2) .LE. OLDFRE  ) GO TO 51			REG02870
C!!!------------------------------------------------------------------- REG02880
	WRITE(NW6,52) ADRMAS, OLDFRE					REG02890
52	FORMAT(' ****** OòàÅäA REGCPU: ADRMAS=',I10,' OLDREF=',I10)	REG02900
C!!!------------------------------------------------------------------- REG02910
	GO TO 666							REG02920
C  PAáMETäA								REG02930
51	REGION(ADRMAS-2) = IPRT						REG02940
50	IPRT = IPRT + 1							REG02950
	IF ( IPRT .LE. NEWPRT ) GO TO 49				REG02960
C-------								REG02970
C  2-ü îAáA								REG02980
C-------								REG02990
	 NFIX = 1							REG03000
	 NTEä = 1							REG03010
C  ÑãàçA TEä. ÅãOäA							REG03020
53	 äDIMX = REGION(NTEä+1)						REG03030
	 IF ( äDIMX .GE. 0 ) GO TO 54					REG03040
C  ÅãOä BõÁEPäçìT, EÉO èPOèìCTàTú					REG03050
	 NTEä = NTEä + IABS(äDIMX) + 2					REG03060
C   ìMEçúòàTú äOçTPOãúçõâ CÁä. BõÁEPäçìTõX ÅãOäOB			REG03070
	 BEDREG = BEDREG - 1						REG03080
C---äOçñOBäA ñàäãA							REG03090
55	 IF ( NTEä .LT. NEWFRE ) GO TO 53				REG03100
	 GO TO 60							REG03110
C  OÅPAÅOTäA XOPOòEÉO ÅãOäA, îàäCàPìEM EÉO çAÁAãO			REG03120
54	NMOVE = NTEä							REG03130
C  AÑP. CãEÑ.								REG03140
	NTEä = NTEä + äDIMX + 2						REG03150
C  çìÜçO ãà îàáàÁECäà èEPECõãATú					REG03160
	IF ( NFIX .EQ. NMOVE ) GO TO 56					REG03170
C èEPECõãAEM, çE CÅàãàCú ?						REG03180
	 IF ( NFIX .LT. NMOVE ) GO TO 57				REG03190
C**** CÅàãàCú ********************************************************* REG03200
C!!!------------------------------------------------------------------- REG03210
	 WRITE(NW6,58) NFIX, NMOVE					REG03220
58	 FORMAT(' ***** CÅOâ B REGCPU: NFIX=',I10,' NMOVN=',I10)	REG03230
C!!!------------------------------------------------------------------- REG03240
	 GO TO 666							REG03250
57	 CONTINUE							REG03260
	 IFF = 0							REG03270
59	 REGION( NFIX+IFF ) = REGION( NMOVE+IFF )			REG03280
	 IFF = IFF + 1							REG03290
	 IF ( IFF .LT. äDIMX+2 ) GO TO 59				REG03300
C  äOPEä. ìäAáATEãà áAçüTõX						REG03310
56	 NFIX = NFIX + äDIMX + 2					REG03320
	 GO TO 55							REG03330
C  äOçEñ 2-â îAáõ:							REG03340
60	 NEWFRE = NFIX							REG03350
C  CAMOèPOBEPäA:							REG03360
	 IF ( BEDREG .EQ. 0 ) GO TO 62					REG03370
C!!!------------------------------------------------------------------- REG03380
	 WRITE(NW6,61) BEDREG						REG03390
61	 FORMAT(' ***** OòàÅäA REGCPU: àTOÉ BEDREG=',I10)		REG03400
C!!!------------------------------------------------------------------- REG03410
	 GO TO 666							REG03420
C-------								REG03430
C   3-ü îAáA								REG03440
C-------								REG03450
C   PACTABàM ìäAáATEãà							REG03460
62	 OLD=0								REG03470
C--------IF ( NEWFRE .EQ. 1 ) GO TO 63					REG03480
	 NTEä = 1							REG03490
C   BõÅPATú äOÑ MAC. ( MECTO B PRT )					REG03500
64	 IPP = REGION(NTEä)						REG03510
C   èPOBEPàTCü								REG03520
	 IF ( IPP .GE. 1  .AND.						REG03530
     *	      IPP .LE.	  NPRT ) GO TO 65				REG03540
C!!!------------------------------------------------------------------- REG03550
	 WRITE(NW6,66) IPP, NTEä					REG03560
66	 FORMAT(' ***** CÅOâ REGCPU: IPP=',I10,' NTEä=',I10)		REG03570
C!!!------------------------------------------------------------------- REG03580
	 GO TO 666							REG03590
C   BOCTAçOBàTú B PRT							REG03600
65	 PRT(IPP) = NTEä + 2						REG03610
	 REGION(NTEä) = OLD						REG03620
	 OLD = NTEä							REG03630
	 NTEä = NTEä + REGION(NTEä+1) + 2				REG03640
	 IF ( NTEä .LT. NEWFRE ) GO TO 64				REG03650
63	 OLDFRE = OLD							REG03660
	 äDIMX = NREG - NEWFRE - 1					REG03670
C äOçÁAEM								REG03680
	GO TO METäA,(17,41)						REG03690
668	GO TO 666							REG03700
C-----------------------------------------------------------------------REG03710
C-----------------------------------------------------------------------REG03720
C-----------------------------------------------------------------------REG03730
666	 CONTINUE							REG03740
C!!!-----ABAPàâçAü èEÁATú-----------------------------------------------REG03750
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
