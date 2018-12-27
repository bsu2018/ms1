         SUBROUTINE SEQDIR(KD, KAR, DAT, IRC )
CXXX----KOMèãEKC "MS1" --> BEPCàü 2.0
C   èOÑèPOÉPAMMA:   SEQDIR(KD, KAR, DAT, IRC )
C   -------------
C   HAáHAóEHàE:    MOHàTOP BõÑEãEHàü à AHAãàáA ÑàPEKTàB
C   ----------     ************************************
C
C
C   METOÑ:         CàHT. PAáÅOP, CPABHEHàE C òAÅãOHOM;
C   -----           $$$ OT 2.08.89 ===> èEPEBOÑ àMEHà ÑàP. B CTP. ÅìKBõ.
C
C   èAPAMETPõ:     KD - KOÑ BõÑEãEHHOâ ÑàPEKTàBõ:
C   ---------
C
C----------------------------------------------------------------------
C    KD     àMü         è/è OÅPAÅOTKà  ÑãàHA  <ECT8 ãà óàCãOBõE ÑAHHõE>
C----------------------------------------------------------------------
C    0       %           SEQDIR          1          HET
C    1      #àMü         SEQDIR          4          HET
C    2      #èPàM        SEQDIR          5          HET
C    3      #PEÜàM       DRREG0          6          HET
C    4      #ñEèú        DRCIRC          5          ECT8
C    5      #METOÑ       DRMET0          6          HET
C    6      #èEó         DRPRNT          4          ECT8
C    7      #TAB         DRTAB0          4          ECT8
C    8      #ÉPAî        DRGRF0          5          ECT8
C    9      #ÉPAî1       DRGRF1          6          ECT8
C   10      #CóET        DRDIG0          5          ECT8
C   11      #CTOè        MAIN_PGM        5          HET
C   12      #COXP        DRSAV0          5          ECT8
C   13      #BOCT        DRRES0          5          ECT8
C   14      #ÑàAã        SEQDIR          5          HET
C   15      #CèPAB       DRDAT0          6          HET
C   16      HELP         DRHELP          4          HET
C   17      #XñEèú       DRCIRX          6          ECT8
C   18      #èCóET       DRDIGP          6          ECT8
C   19      #ACOXP       DRSAVA          6          ECT8
C   20      #CèEKTP      DRFURE          7          ECT8
C   21      #MOôHOCTú    DRPPPP          9          ECT8
C   22      #MPEÜàM      DRRMMM          7          HET
C   23      #ÇçÖò        DROUT0          5          Öëíú
C   24      #SPOOL       DRSPOL          6          çÖí
C   25      #êÖÜáÄè      DRREGS          7          Öëíú
C   26      #êÖÜÇéëí     DRREGL          8          Öëíú
C----------------------------------------------------------------------
C
C                  KAR - MACCàB ÑàPEKTàBõ;
C                  DAT - MACCàB ÑAHHõX  ÑàPEKTàBõ;
C
C                  IRC = 1 -  OóEPEÑHìû ÑàPEKTàBì èPàHüãà;
C                      = 2 -  KOHEñ ÑAHHõX HA CàCTEMHOM NRDR;
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, SERVZ4,  SERVZ5
C   -------------  2. GETDAT, SEQ1, MOVE1;
C                  3. APFCAS - èEPEBOÑ B ÅOã8òàE ÅìKBõ....
C                              àMEH ÑàPEKTàB...
C   èPàMEóAHàE:    1.  <# >  à  <#BCE>  -  CàHOHàMõ;
C   ----------     2.  <%>  ÑO KOHñA CTPOKà  -  KOMEHTAPàà;
C  OT 4.04.89 --> "+"  èEPBAü ÑàPEKTàBA KOHTPOãàPìETCü HA #ÑàAã....
C  éí 13.06.90 --> SEQ1 Ì†¨•≠•≠† ≠† SEQ1M ---  Ø•Á†‚Ï ®¨•≠® §®‡•™‚®¢Î
C
CZZZ-------------------------------------------------------------------
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/SERVZ4/ NAMECR(80)
         COMMON/SERVZ5/ KCOM, COMENT(100)
         DIMENSION KOMENT(100)
         EQUIVALENCE( COMENT, KOMENT )
         DIMENSION KAR(*), DAT(*)
         DIMENSION YES(2), NO(3)
         INTEGER YES, NO, DIEZ, BCE, ABCE
         DIMENSION D1(4), D2(5), D3(6), D4(5), D5(6), D6(4),
     = D20(7),     D7(4), D8(5), D9(6), D10(5), D11(5), D12(5),
     = D21(9),     D13(5), D14(5), D15(6), D16(4), D17(6),D18(6),D19(6)
         INTEGER D1, D2, D3, D4, D5, D6, D7, D8, D9, D10,
     = D20, D21, D11, D12, D13, D14, D15, D16, D17, D18, D19
         DIMENSION D22(7),d23(5),D24(6),D25(7),D26(8)
         INTEGER D22,d23, D24 ,D25,D26
         EQUIVALENCE( MASPRT(12), NRDR)
         EQUIVALENCE( MASPRT(13), NWTR)
         EQUIVALENCE( MASPRT(14), NWTRE)
         EQUIVALENCE( MASPRT(15), DIAL)
         EQUIVALENCE( MASPRT(127), NZRDR)
         INTEGER DIAL, BLANK, FDIAL
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         DATA YES/'Ñ','A'/, NO/'H', 'E', 'T' /
         DATA DIEZ/'#'/, BCE/'#BCE'/, ABCE/'#   '/
         DATA ICOM/'%'/, ICOM1/'%   '/ , K72/72/, K21/21/
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C----------------------------------------------------------------------
         DATA D1/ '#', 'à', 'M', 'ü'            /
         DATA D2/ '#', 'è', 'P', 'à', 'M'       /
         DATA D3/ '#', 'P', 'E', 'Ü', 'à', 'M'  /
         DATA D4/ '#', 'ñ', 'E', 'è', 'ú'       /
         DATA D5/ '#', 'M', 'E', 'T', 'O', 'Ñ'  /
         DATA D6/ '#', 'è', 'E', 'ó'            /
         DATA D7/ '#', 'T', 'A', 'Å'            /
         DATA D8/ '#', 'É', 'P', 'A', 'î'       /
         DATA D9/ '#', 'É', 'P', 'A', 'î', '1'  /
         DATA D10/ '#', 'C', 'ó', 'E', 'T'      /
         DATA D11/ '#', 'C', 'T', 'O', 'è'      /
         DATA D12/ '#', 'C', 'O', 'X', 'ê'      /
         DATA D13/ '#', 'B', 'O', 'C', 'T'      /
         DATA D14/ '#', 'Ñ', 'à', 'A', 'ã'      /
         DATA D15/ '#', 'C', 'è', 'P', 'A', 'B' /
         DATA D16/ 'H', 'E', 'L', 'P'           /
         DATA D17/ '#', 'X', 'ñ', 'E', 'è', 'ú' /
         DATA D18/ '#', 'è', 'C', 'ó', 'E', 'T' /
         DATA D19/ '#', 'A', 'C', 'O', 'X', 'P' /
         DATA D20/ '#', 'C', 'è', 'E', 'K', 'T', 'P' /
         DATA D21/ '#', 'M', 'O', 'ô', 'H', 'O', 'C', 'T', 'ú' /
         DATA D22/ '#', 'M', 'P', 'E', 'Ü', 'à', 'M' /
         DATA D23/ '#', 'B', 'H', 'E', 'ò'           /
         DATA D24/ '#', 'S', 'P', 'O', 'O', 'L'      /
         DATA D25/ '#', 'P', 'E', 'Ü', 'á', 'A', 'è' /
         DATA D26/ '#', 'P',  'E', 'Ü', 'B', 'O', 'C', 'T'     /
C----------------------------------------------------------------------
         DATA BLANK /' '/,FDIAL/0/
C######################################################################
         IRC = 1
666      CONTINUE
         DO 667 I = 1, K21
667      DAT(I) = 0.0
         READ(NRDR, 100, END = 9999 ) (KAR(I), I=1,80 )
         NZRDR = NZRDR + 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C --->   èEPEBOÑ àá MAãEHÏKàX ÅìKB B ÅOãÏòàE ......
         CALL APFCAS(1,80,KAR,KAR)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
100      FORMAT(80A1)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         CALL SEQ1M(KAR, D14, 5, I)
         GO TO (14),I
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF ( DIAL .EQ. 0 ) WRITE(NWTR,101) (KAR(I), I=1,80)
         IF ( DIAL .EQ. 1 ) WRITE(NWTR,1010) (KAR(I), I=1,80)
101      FORMAT(/20X, 80A1)
1010     FORMAT(/ 80A1)
         IK = 1
C--> CàTO Ñãü ÑàPEKTàB.....
         IF ( KAR(1) .EQ. ICOM ) GO TO 666
C  ->  óTO    C  óEM   CKOãúKO  1-ÑA, 2=HET:::::::
         CALL SEQ1M(KAR, D1, 4, I)
         GO TO (1),I
         CALL SEQ1M(KAR, D2, 5, I)
         GO TO (2),I
         CALL SEQ1M(KAR, D3, 6, I)
         GO TO (3),I
         CALL SEQ1M(KAR, D4, 5, I)
         GO TO (4),I
         CALL SEQ1M(KAR, D5, 6, I)
         GO TO (5),I
         CALL SEQ1M(KAR, D6, 4, I)
         GO TO (6),I
         CALL SEQ1M(KAR, D7, 4, I)
         GO TO (7),I
         CALL SEQ1M(KAR, D9, 6, I)
         GO TO (9),I
         CALL SEQ1M(KAR, D8, 5, I)
         GO TO (8),I
         CALL SEQ1M(KAR, D10, 5, I)
         GO TO (10),I
         CALL SEQ1M(KAR, D11, 5, I)
         GO TO (11),I
         CALL SEQ1M(KAR, D12, 5, I)
         GO TO (12),I
         CALL SEQ1M(KAR, D13, 5, I)
         GO TO (13),I
C+       CALL SEQ1M(KAR, D14, 5, I)
C+       GO TO (14),I
         CALL SEQ1M(KAR, D15, 6, I)
         GO TO (15),I
         CALL SEQ1M(KAR, D16, 4, I)
         GO TO (16),I
         CALL SEQ1M( KAR, D17, 6, I )
         GO TO (17),I
         CALL SEQ1M(KAR, D18, 6, I)
         GO TO (18),I
         CALL SEQ1M( KAR, D19, 6, I )
         GO TO (19),I
         CALL SEQ1M( KAR, D20, 7, I )
         GO TO (20),I
         CALL SEQ1M( KAR, D21, 9, I )
         GO TO (21),I
         CALL SEQ1M( KAR, D22, 7, I )
         GO TO (22),I
         CALL SEQ1M( KAR, D23, 5, I )
         GO TO (23),I
         CALL SEQ1M( KAR, D24, 6, I )
         GO TO (24),I
         CALL SEQ1M( KAR, D25, 7, I )
         GO TO (25),I
         CALL SEQ1M( KAR, D26, 8, I )
         GO TO (26),I
C--->  MOÜHO áÑECú AKTàBàáàPOBAT8 CàHOHàMõ:::::::
C######################################################################
         WRITE(NWTR, 102)
 102     FORMAT(/' *** Ñ®‡•™‚®¢† ≠• ÆØÆß≠†≠† - ¢Æß¨Æ¶≠Æ: ≠•‚ -->',
     =    ' "#BCE" ®´® ";" ***' )
         WRITE(NWTR, 1022) (KAR(iiiii),iiiii=1,80)
 1022    FORMAT('*** B¢•§•≠≠†Ô ·‚‡Æ™†:'/80A1/)
         GO TO 666
C######################################################################
1        CONTINUE
C---> #àMü
         KD = 1
         CALL MOVE1( NAMECR, KAR, 80 )
         GO TO 666
2        CONTINUE
C---> #èPàM
         KD = 2
         KCOM = 0
2002     CONTINUE
         KCOM = KCOM + 1
         IF ( KCOM .GT. 6 ) GO TO 2003
         I1 = (KCOM-1)*20 + 1
2000     CONTINUE
         READ(NRDR, 203, END = 9999) (KAR(I), I=1, 20 )
         NZRDR = NZRDR + 1
         IF ( DIAL .EQ. 0 ) WRITE(NWTR, 201 ) ( KAR(I), I=1, 20 )
         IF ( DIAL .EQ. 1 ) WRITE(NWTR, 2010 ) ( KAR(I), I=1, 20 )
203      FORMAT(20A4)
201      FORMAT(20X, 20A4)
2010     FORMAT(     20A4)
         IF ( KAR(1) .EQ. ICOM ) GO TO 2000
         IF ( KAR(1) .EQ. ICOM1 ) GO TO 2000
         KCOM = KCOM - 1
         IF ( KAR(1) .EQ. BCE ) GO TO 666
         IF ( KAR(1) .EQ. ABCE ) GO TO 666
         KCOM = KCOM + 1
         CALL MOVE1( KOMENT(I1), KAR, 20 )
         GO TO 2002
2003     CONTINUE
         WRITE(NWTR, 104)
104     FORMAT(/' *** ì¶• Ø‡®≠Ô´® 6-Ó ·‚‡Æ™„ #èPàM, † £§• "# " ®´® "#BCE
     ="? ***'/ ' *** BÆß¨Æ¶≠Æ - Á†·‚Ï ®≠‰Æ‡¨†Ê®® "„Ë´†" ¢ #èPàM ***'/
     =      ' *** Å„§‚• ¢≠®¨†‚•´Ï≠Î ***'/)
         GO TO 666
C######################################################################
14       CONTINUE
C   #ÑàAã
         KD = 14
         DIAL = 0
C-- ÉÑE èOÅEãõ
         DO 14000 I = 1, K72
         IK = I
         IF ( KAR(I) .EQ. BLANK )  GO TO 14001
14000    CONTINUE
         WRITE(NWTR,14005) DIAL
14005    FORMAT(/5X,'*** #ÑàAã  „·‚†≠Æ¢´•≠: "HET" (DIAL=',I1,') ***')
         GO TO 666
14001    CONTINUE
C--> ìÅEPEM èPOÅEãõ
       I1= IK
       DO 14002 I=I1, K72
       IK = I
       IF( KAR(I) .NE. BLANK ) GO TO 14003
14002  CONTINUE
         WRITE(NWTR,14005)   DIAL
       GO TO 666
14003  CONTINUE
         IF (KAR(IK) .EQ. YES(1) .AND. KAR(IK+1) .EQ. YES(2) )
     =     DIAL = 1
         IF ( DIAL .EQ. 1 ) WRITE(NWTR,14006) DIAL
         IF ( DIAL .EQ. 0 ) WRITE(NWTR,14005) DIAL
14006    FORMAT(/5X,'*** #ÑàAã  „·‚†≠Æ¢´•≠: "ÑA" (DIAL=',I1,') ***')
         GO TO 666
C######################################################################
3        CONTINUE
C   #PEÜàM
         KD = 3
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
4        CONTINUE
C   #ñEèú
         KD = 4
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
5        CONTINUE
C   #METOÑ
         KD = 5
         RETURN
C######################################################################
6        CONTINUE
C  #èEó
         KD = 6
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
7        CONTINUE
C  #TAÅ
         KD = 7
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
8        CONTINUE
C   #ÉPAî
         KD = 8
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
9        CONTINUE
C  #ÉPAî1
         KD = 9
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
10       CONTINUE
C  #CóET
         KD = 10
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
11       CONTINUE
C  #CTOè
         KD = 11
         RETURN
C######################################################################
12       CONTINUE
C   #COXP
         KD = 12
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
13       CONTINUE
C  #BOCT
         KD = 13
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
15       CONTINUE
C  #CèPAB
         KD = 15
         RETURN
C######################################################################
16       CONTINUE
C  #HELP
         KD = 16
         RETURN
C######################################################################
17     CONTINUE
C   #XñEèú  <àMü>   -->  îAâã XDATA....
       KD = 17
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
18       CONTINUE
C  #èCóET
         KD = 18
       CALL GETDAD( KAR, DAT, III )
         RETURN
C######################################################################
19     CONTINUE
C   #ACOXP
       KD = 19
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
20       CONTINUE
C  #CèEKTP
         KD = 20
       CALL GETDAD( KAR, DAT, III )
         RETURN
C######################################################################
21     CONTINUE
C   #MOôHOCTú
       KD = 21
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
22     CONTINUE
C   #MPEÜàM
       KD = 22
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
C######################################################################
23       CONTINUE
C  #BHEò
         KD = 23
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
24       CONTINUE
C  #SPOOL
         KD = 24
C*****   CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
C######################################################################
25       CONTINUE
C  #áÄèêÖÜ
         KD = 25
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
C######################################################################
26       CONTINUE
C  #ÇéëíêÖÜ
         KD = 26
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
C  - NRDR CKOHóAãCü
         WRITE(NWTR, 105 ) NRDR
105   FORMAT(///' *** KOHEñ àHîOPMAñàà HA KAHAãE CàCTEMHOÉO BBOÑA ', I2,
     =  ' ***'/ ' *** PAÅOTA èPEPõBAETCü  [#SEQ_DIR#]  ***'//)
         IRC = 2
         RETURN
         END
