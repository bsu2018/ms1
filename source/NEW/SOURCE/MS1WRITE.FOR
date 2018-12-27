C   MS1WRITE								MS100010
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT HOüÅPü 1987 É.)------------	MS100020
C   èPOÉPAMMA:	MS1WRITE						MS100030
C  ----------								MS100040
C   HAáHAóEHàE:        èEóAT8 àá FT05 HA FT03 ÅàããàCTàHÉOM		MS100050
C   ----------	       6O-üT èEPBõX CàMBOãOB				MS100060
C		       BBEÑEHHõX  CTOPOK TEKCTA ÅEá àCèOã8áOBAHàü	MS100070
C		       ìèP. CàMBOãOB èEóATà, HO OHà BõÑAûTCü.......	MS100080
C   èPàHüTO:	       HA ãàCTE 72 CTPOKà, èEóATAETCü 60-T8 CTPOK	MS100090
C   -------	       îAâãA FT05 HA ãàCTE,				MS100100
C		       èEPBAü áAèàC8 FT05 - KOãOHTàTìã,			MS100110
C		       èPà PAGE B èEPBõX èOá. FT05 --> èEPEXOÑ HA	MS100120
C		       HOBìû CTPAHàñì;					MS100130
C									MS100140
CZZZ------------------------------------------------------------------- MS100150
       DIMENSION  LIST1(60,15), LIST2(60,15)				MS100160
       DIMENSION  KAR(32) ,KARZ(15)					MS100170
       INTEGER FLAG, PAGE						MS100180
       DATA PAGE/'PAGE'/, L/'	 ' /					MS100190
       DATA KAR/32*'	'/						MS100200
C---------------------------------------------------------------------- MS100210
       NR = 5								MS100220
       NR1= 1								MS100230
       NW = 3								MS100240
C---------------------------------------------------------------------- MS100250
C-->  BBOÑàM èEPEóEH8 BõBOÑàMõX CTPAHàñ					MS100260
       READ(NR1,5,END=9999) LISTN, LISTK				MS100270
5      FORMAT( 2I5 )							MS100280
C---------------------------------------------------------------------- MS100290
C-->  BBOÑàM áAÉãABàE							MS100300
       READ(NR,10,END=9999) ( KARZ(I), I=1,15)				MS100310
10     FORMAT( 15A4 )							MS100320
C---------------------------------------------------------------------- MS100330
       NSTR = -1							MS100340
C---------------------------------------------------------------------- MS100350
15     CONTINUE								MS100360
C-->  BBOÑàM èEPBìû CTPAHàñì						MS100370
       NSTR = NSTR + 2							MS100380
       FLAG = 0								MS100390
       DO 50 I = 1,60							MS100400
       IF( FLAG .NE. 0 ) GO TO 30					MS100410
18     CONTINUE								MS100420
       READ(NR,10, END=19 ) (LIST1(I,J), J=1,15 )			MS100430
       IF( LIST1(I,1) .EQ. PAGE ) GO TO 20				MS100440
       GO TO 50								MS100450
19     CONTINUE								MS100460
       FLAG = 2								MS100470
       GO TO 30								MS100480
20     CONTINUE								MS100490
       IF (I .EQ. 1 ) GO TO 18						MS100500
       FLAG = 1								MS100510
30     CONTINUE								MS100520
       DO 40 J= 1,15							MS100530
       LIST1(I,J) = L							MS100540
40     CONTINUE								MS100550
50     CONTINUE								MS100560
C---------------------------------------------------------------------- MS100570
C-->  BBOÑàM BTOPìû CTPAHàñì						MS100580
       IF( FLAG .NE. 2 ) FLAG = 0					MS100590
       DO 90 I = 1,60							MS100600
       IF( FLAG .NE. 0 ) GO TO 70					MS100610
55     CONTINUE								MS100620
       READ(NR,10, END=58 ) (LIST2(I,J), J=1,15 )			MS100630
       IF( LIST2(I,1) .EQ. PAGE ) GO TO 60				MS100640
       GO TO 90								MS100650
58     CONTINUE								MS100660
       FLAG = 2								MS100670
       GO TO 70								MS100680
60     CONTINUE								MS100690
       IF (I .EQ. 1 ) GO TO 55						MS100700
       FLAG = 1								MS100710
70     CONTINUE								MS100720
       DO 80 J= 1,15							MS100730
       LIST2(I,J) = L							MS100740
80     CONTINUE								MS100750
90     CONTINUE								MS100760
C---------------------------------------------------------------------- MS100770
C-->  BõBOÑ CTPAHàñõ							MS100780
       NSTR1 = NSTR + 1							MS100790
C---------------------------------------------------------------------- MS100800
C-->  BXOÑüT ãà TEKìôàE CTPAHàñõ B áAÑAHHõâ èEPEóEH8 CTPAHàñ?		MS100810
       IF( NSTR .LT. LISTN .AND. NSTR1 .LT. LISTN ) GO TO 15		MS100820
       IF( NSTR .GT. LISTK .AND. NSTR1 .GT. LISTK ) GO TO 15		MS100830
C---------------------------------------------------------------------- MS100840
       WRITE(NW,13) ( KAR(J),J=1,15) , ( KAR(J),J=18,32)		MS100850
       WRITE(NW,11) ( KAR(J),J=1,32)					MS100860
       WRITE(NW,11) ( KAR(J),J=1,32)					MS100870
       WRITE(NW,11) ( KAR(J),J=1,32)					MS100880
11     FORMAT( 1X, 32A4 )						MS100890
       WRITE(NW,12) ( KAR(J),J=1,14), NSTR, KAR(16), KAR(17), NSTR1,	MS100900
     *		    ( KAR(J),J=19,32)					MS100910
12     FORMAT( 1X, 14A4 , I4 , 2A4 , I4 , 14A4 )			MS100920
       WRITE(NW,11) ( KARZ(J),J=1,15), L , L , ( KARZ(J),J=1,15)	MS100930
       WRITE(NW,11) ( KAR(J),J=1,32)					MS100940
       WRITE(NW,11) ( KAR(J),J=1,32)					MS100950
	 DO 150 I= 1,60							MS100960
	 WRITE(NW,11) ( LIST1(I,J),J=1,15), L , L ,			MS100970
     *		      ( LIST2(I,J),J=1,15)				MS100980
150	 CONTINUE							MS100990
       WRITE(NW,11) ( KAR(J),J=1,32)					MS101000
       WRITE(NW,11) ( KAR(J),J=1,32)					MS101010
       WRITE(NW,11) ( KAR(J),J=1,32)					MS101020
       WRITE(NW,13) ( KAR(J),J=1,15) , ( KAR(J),J=18,32)		MS101030
13     FORMAT( 1X, 15A4 ,'   **   ', 15A4 )				MS101040
C---------------------------------------------------------------------- MS101050
       IF( FLAG .NE. 2 ) GO TO 15					MS101060
       STOP								MS101070
C###################################################################### MS101080
9999   CONTINUE								MS101090
       WRITE(3,200)							MS101100
       WRITE(6,200)							MS101110
200    FORMAT(///' *** HAÅOP TEKCTA HA "FT05F001" - èìCTOâ ***'///)	MS101120
       STOP 8								MS101130
       END								MS101140
