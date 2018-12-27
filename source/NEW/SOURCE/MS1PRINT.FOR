C   MS1PRINT								MS100010
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------ MS100020
C   èPOÉPAMMA:	MS1PRINT						MS100030
C  ----------								MS100040
C   HAáHA^EHàE:        èE^AT8 àá FT05 HA FT03 72-8 èEPBõX CàMBOãOB	MS100050
C   ----------	       BBEÑEHHõX  CTOPOK TEKCTA ÅEá àCèOã8áOBAHàü	MS100060
C		       ìèP. CàMBOãOB èE^ATà, HO OHà BõÑAûTCü.......	MS100070
C   èPàHüTO:	       HA ãàCTE 72 CTPOKà, èE^ATAETCü 60-T8 CTPOK	MS100080
C   -------	       îAâãA FT05 HA ãàCTE,				MS100090
C		       èEPBAü áAèàC8 FT05 - KOãOHTàTìã,			MS100100
C		       èPà PAGE B èEPBõX èOá. FT05 --> èEPEXOÑ HA	MS100110
C		       HOBìû CTPAHàñì;					MS100120
C									MS100130
CZZZ------------------------------------------------------------------- MS100140
       DIMENSION TXT(60,18), KOLON(18)					MS100150
       INTEGER TXT, BLANK, PAGE						MS100160
       DATA PAGE/'PAGE'/, BLANK/'    ' /				MS100170
       READ(5,1,END=666) ( KOLON(I), I=1,18)				MS100180
1      FORMAT( 20A4 )							MS100190
       KCTR = 0								MS100200
       IEND = 0								MS100210
C-->  ñàKã èE^ATà							MS100220
100    CONTINUE								MS100230
       DO 10 I = 1,60							MS100240
       DO 10 J = 1,18							MS100250
10     TXT(I,J) = BLANK							MS100260
C									MS100270
       DO 11 I = 1,60							MS100280
       IX = I								MS100290
       READ(5,1, END=667 ) (TXT(I,J), J=1,18 )				MS100300
       IF ( TXT(IX,1 ) .EQ. PAGE ) GO TO 12				MS100310
11     CONTINUE								MS100320
       GO TO 13								MS100330
12     TXT(IX,1) = BLANK						MS100340
       IF (IX .EQ. 1 ) GO TO 100					MS100350
13     CONTINUE								MS100360
101    CONTINUE								MS100370
       KCTR = KCTR + 1							MS100380
C---------------------------------------------------------------------- MS100390
C-->  BõBOÑ 72 CTOPOK							MS100400
       WRITE(3,14) BLANK, BLANK						MS100410
14     FORMAT( 1H1, 72X, 3X, '*' / 1X, A4 / 1X, A4 )			MS100420
       WRITE(3,15)	 KCTR						MS100430
15     FORMAT( 1X, 64X, I8 )						MS100440
       WRITE(3,16) ( KOLON(I), I=1,18)					MS100450
16     FORMAT( 1X, 20A4 )						MS100460
       WRITE( 3,17) BLANK						MS100470
17     FORMAT( 1X, A4 )							MS100480
       DO  170 I=1,60							MS100490
       WRITE(3,18) ( TXT(I,J), J=1,18 )					MS100500
18     FORMAT( 1X, 20A4 )						MS100510
170    CONTINUE								MS100520
       WRITE(3,19) BLANK, BLANK, BLANK, BLANK, BLANK			MS100530
19     FORMAT(1X,A4/ 1X,A4/ 1X, A4/ 1X, A4 / 1X, A4 /			MS100540
     *	     1X, 72X, 3X, '*' )						MS100550
C---------------------------------------------------------------------- MS100560
       IF ( IEND .EQ. 1 ) GO TO 668					MS100570
       GO TO 100							MS100580
667    IEND = 1								MS100590
       GO TO 101							MS100600
666    WRITE(3,20)							MS100610
       WRITE(6,20)							MS100620
20     FORMAT(///' *** HAÅOP TEKCTA HA "FT05F001" - èìCTOâ ***'///)	MS100630
       STOP 8								MS100640
668    STOP 0								MS100650
       END								MS100660
