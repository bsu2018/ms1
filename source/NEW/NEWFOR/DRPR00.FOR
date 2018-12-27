         SUBROUTINE DRPR00(KOD, SPIS, TIME, TOKN, VALLN )
CXXX----KOM‹EKC "MS1" -->
C   O„POƒPAMMA:   DRPR00(KOD, SPIS, TIME, TOKN, VALLN )
C   ------------
C   HA‡HA—EHˆE:    *** OPAOT—ˆK „ˆPEKTˆB #E—  ˆ   #TA  ***
C   -----------      E—ATœ PE‡“‹œTATOB B TA‹ˆ—HO‰ ”OPME;
C
C   METO„:         B›OPKA ˆ‡ TIME, TOKN  ˆ  VALLN COƒ‹ACHO CˆCKA SPIS;
C  -------
C
C   APAMETP›:     SPIS - CˆCOK ‡APOCOB ( C.M. GETSPS )
C   ---------      TIME, TOKN, VALLN - BEKTOPA PE˜EHˆ‰;
C                  KOD = 1 - E—ATœ ˜AKˆ TA‹ˆ–›;
C                      = 2 - E—ATœ O—EPE„HO‰ CTPOKˆ;
C                      = 3 - ‡AKP›Tœ TA‹ˆ–“;
C                      = 4 - E—ATœ O—EPE„HO‰ TO—Kˆ C Pˆ‡HAKOM
C                            "B MOMEHT KOMM“TA–ˆˆ";
C                      = 5 - E—ATœ O—EPE„HO‰ TO—Kˆ C Pˆ‡HAKOM
C                            "TECT _ P“HƒE";
C
C                      = 6 - E—ATœ TO—Kˆ Pˆ CO‡„AHˆˆ K.T. C Pˆ‡HAKOM
C                            "@";
C                      = 7 - E—ATœ TO—Kˆ C Pˆ‡HAKOM "ˆHTEPO‹Ÿ–ˆŸ";
C
C
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, SERVZ2, 3;
C   -------------  2. MOVE0,PICTURE
C   PˆME—AHˆE:    KOHTPO‹œ ‡AO‹HEHˆŸ SPIS HE POˆ‡BO„ˆTCŸ;
C   ----------
C                Žâ 02.04.1992 £. Œ®¤¨ä¨æ¨à®¢ ­  ¤«ï ¤¨­ ¬¨ç¥áª®£® 
C                ¢ë¢®¤  6 £à ä¨ª®¢ *
C
C
CZZZ-------------------------------------------------------------------
         COMMON/ PROF1 / MASPRT(200)
         COMMON/ SERVZ2/        TABTOK(135)
         COMMON/ SERVZ3/        TABTIP(300)
         COMMON/SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
         INTEGER TABTOK, TABTIP, SPIS, DIAL
         DIMENSION SPIS(1), TOKN(1), VALLN(1), NAMEI(3), NAMEV(3),
     =             INTO(3)
         DIMENSION HELP(32), RHELP(1),lm(6),TMAX(6),kor(6),XLM(6)
         INTEGER HELP, BLANK, TIRE, PX, ANM, ANT
         INTEGER ln,lm,kor,kpline                  
         EQUIVALENCE( NWTR,  MASPRT(13)),
     =              ( NWTRE, MASPRT(14)),
     =              ( DIAL,  MASPRT(15)),
     =              ( Kxxx,  MASPRT(99)),
     =              ( RHELP(1), HELP(1) ),
     =              (MASPRT(48),HOUTT),
     =              (TMAX(1) ,MASPRT(130)),
     =              (TMAX(2) ,MASPRT(131)),
     =              (TMAX(3) ,MASPRT(132)),
     =              (TMAX(4) ,MASPRT(133)),
     =              (TMAX(5) ,MASPRT(134)),
     =              (TMAX(6) ,MASPRT(135)),
     =              (kor(2) ,MASPRT(136)),
     =              (kor(3) ,MASPRT(137)),
     =              (kor(4) ,MASPRT(138)),
     =              (kor(5) ,MASPRT(139)),
     =              (kor(6) ,MASPRT(140))
C
         DATA NAMEI/ '«-­', 'â & ', '§­ ç' /
         DATA NAMEV/ ' H ¯', 'àï¦¥', '­¨¥ ' /
         DATA INTO / '¬¥¦¤', 'ã ã§', '« ¬¨' /
         DATA BLANK/'    '/, TIRE/'ÄÄÄÄ'/ 
C**********************************************************************
         KP = SPIS(1)
         kpline = 350/kp
         kplin2 = kpline/2
         GO TO ( 1,2,3,2,2,2,2 ), KOD
         CALL SUBERR(KOD)
1        CONTINUE
         IF ( DIAL .EQ. 1  .AND. KP .GT. 5 ) KP = 5
         IF ( KP .GT. 9 ) KP = 9
C--> „‹ˆHA B›B. CTPOKˆ ==> KSTR  ˆ KSTR4
         KSTR = 11 + 13 * KP
C---->
         KSTR4 = KSTR/4
C===> B›BO„ ‡AƒO‹OBKA....
         WRITE(NWTR, 360)
360      FORMAT(1H1,// 20X, 'T ¡«¨æ  §­ ç¥­¨© ¨áª®¬ëå ¢¥«¨ç¨­'/  )
C--> CPABKA ...
         ISPIS = 2
         IHELP = 1
         KPP = KP
34       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 35
         PX = SPIS(ISPIS)
         IF ( PX .NE. 3 ) GO TO 36
         NEL = SPIS( ISPIS + 2 )
         ANM = SPIS( ISPIS + 3 )
         ANT = SPIS( ISPIS + 4 )
         NSM = SPIS( ISPIS + 5 )
         IHELP = 1
         HELP(IHELP) = NEL
         IHELP = IHELP + 1
         CALL MOVE0( HELP(IHELP), TABTIP(ANM), 5 )
         IHELP = IHELP + 5
         CALL MOVE0( HELP(IHELP), TABTOK(ANT), 3 )
         IHELP = IHELP + 3
         HELP(IHELP) = NSM
         WRITE(NWTR, 37) NEL, ( HELP(I),I=2,9 ), NSM
37       FORMAT(1X, '«¥¬¥­â # ', I5, 1X, 5A4, 1X, '¯¥à¥¬¥­­ ï ', 3A4,
     =           1X, '(§­ ç¥­¨¥ # ', I3, ')'  )
36       KPP = KPP - 1
         ISPIS = ISPIS + 6
         GO TO 34
35       CONTINUE
         DO 11 I=1,32
11       HELP(I) = TIRE
c****************************************************
               call init(1)
               call dcolor(7)
c  ç¥àâ¨¬ ª®àà¤¨­ âë
c               do 111 i = 1,kp
c               call moveab(0,kpline*i - kplin2 )
c               call drawab (639,kpline*i - kplin2 )
111            continue
c               do 112  i =1,31
c               call moveab(20*i,0)
c               call drawab(20*i,349)
c112            continue       
               call dcolor(15)
               call moveab(0,0)
               call drawab(0,349)
c               call drawab(639,175)
c               call drwlog(2)
c               print *,'[24;',12x,'1',13x,'2',
c     +         12x,'3',13x,'4',12x,'5',13x,'6'
c*****************************************************    
c@@@@@@@@@@@ÄÍ³º
c         WRITE(99,12000)
c12000    FORMAT( '[12;0H',79('Ä'))
c@@@@@@@@@@@
         WRITE(NWTR,12) ( HELP(I), I=1,KSTR4 )
12       FORMAT( /1X, 32A4 )
         DO 13 I=1,32
13       HELP(I) = BLANK
C
C--->  TEKCT ‡AƒO‹OBKA...
         ISPIS = 2
         IHELP = 1
         KPP = KP
15       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 16
         PX = SPIS(ISPIS)
         IF ( PX .EQ. 1  .OR.  PX .EQ. 2 )
     =      CALL MOVE0( HELP(IHELP), NAMEV, 3 )
         IF ( PX .NE. 3 ) GO TO 14
C-->  „O ˆH”.
         NEL = SPIS(ISPIS+2)
         ANM = SPIS(ISPIS+3)
         ANT = SPIS(ISPIS+4)
14       CONTINUE
         IF ( PX .EQ. 3 ) CALL MOVE0( HELP(IHELP), TABTOK(ANT), 3 )
         IHELP = IHELP + 3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 15
16       CONTINUE
         IH = IHELP - 1
         WRITE(NWTR, 17 ) ( HELP(I), I=1,IH )
c         WRITE(99, 17000 ) ( HELP(I), I=1,IH )
17       FORMAT(1X, ' ’¥ªãé¥¥  ', 9('³': 3A4 ) )
c17000    FORMAT('[13;0H',' T¥ªãé¥¥  ', 9('³': 3A4 ) )
C===> 2- CTPOKA
         DO 18 I = 1, 32
18       HELP(I) = BLANK
         ISPIS = 2
         IHELP = 1
         KPP = KP
19       CONTINUE
         IF ( KPP .LE. 0 ) GO  TO  20
         PX = SPIS(ISPIS)
         IF ( PX. EQ. 1  .OR.   PX .EQ. 2 )
     =         CALL MOVE0( HELP(IHELP), INTO,  3  )
         IF ( PX .EQ. 3 )
     =         CALL MOVE0( HELP(IHELP), NAMEI,  3  )
         IHELP = IHELP + 3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 19
20       CONTINUE
         IH = IHELP - 1
         WRITE(NWTR, 21 ) ( HELP(I), I=1, IH )
21       FORMAT(1X, '   ¢à¥¬ï  ',  9('³': 3A4 ) )
c         WRITE(99, 21000 ) ( HELP(I), I=1, IH )
c21000    FORMAT( '[14;0H','   ¢à¥¬ï  ',  9('³': 3A4 ) )
C--->  3-Ÿ CTPOKA
         DO 22 I = 1, 32
22       HELP(I) = 0
C-->  KOL-BO —ˆCE‹  KPX = 2*<KO‹-BO EPEMEHH›X>
         KPX = 2*KP
         ISPIS = 2
         IHELP = 1
         KPP = KP
23       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 24
         PX = SPIS(ISPIS)
         IF ( PX .EQ. 1 ) HELP(IHELP) = SPIS(ISPIS+1)
         IF ( PX .EQ. 1 ) HELP(IHELP+1) = 0
         IF ( PX .EQ. 2 ) HELP(IHELP) = SPIS(ISPIS+1)
         IF ( PX .EQ. 2 ) HELP(IHELP+1) = SPIS(ISPIS+2)
         IF ( PX .EQ. 3 ) HELP(IHELP) = SPIS(ISPIS+2)
         IF ( PX .EQ. 3 ) HELP(IHELP+1) = SPIS(ISPIS+5)
         IHELP = IHELP + 2
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 23
24       CONTINUE
         IH = IHELP - 1
         WRITE(NWTR,25) (HELP(I), I=1,IH )
25       FORMAT(1X, '  [CEK.]  ', 9('³': I5, 1X, I5,1X ) )
c         WRITE(99,25000) (HELP(I), I=1,IH )
c25000    FORMAT('[15;0H','  [CEK.]  ', 9('³': I5, 1X, I5,1X ) )
C-->  4-Ÿ CTPOKA
         DO 26 I = 1, 32
26       HELP(I) = TIRE
c         WRITE(99  , 12001)
c12001    FORMAT( '[16;0H',79('Ä'))
         WRITE(NWTR, 1200) ( HELP(I), I=1,KSTR4)
1200     FORMAT(1X,32A4)
         RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
C######################################################################
2        CONTINUE
C--> B›BO„ O—EPE„HO‰
         DO 27  I=1,9
27       RHELP(I) = 0.0
         IHELP = 1
         ISPIS = 2
         KP = SPIS(1)
         IF ( DIAL .EQ. 1  .AND. KP .GT. 5 ) KP = 5
         IF ( KP .GT. 9 ) KP = 9
         KPP = KP
28       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 29
         PX = SPIS(ISPIS)
         I2 = SPIS(ISPIS+1)
C$$      I3 = SPIS(ISPIS+2)
C$$      IF ( PX .EQ. 1 ) RHELP(IHELP) = VALLN(I2)
C$$      IF ( PX .EQ. 2 ) RHELP(IHELP) = VALLN(I2) - VALLN(I3)
C$$      IF ( PX .EQ. 3 ) RHELP(IHELP) = TOKN(I2)
C--
       GO TO ( 51, 52, 53 ), PX
       CALL SUBERR(PX)
51     CONTINUE
C-->  HAPŸ†EHˆŸ
       VI2 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN(I2)
       RHELP(IHELP) = VI2
       GO TO 50
52     CONTINUE
C-->  PA‡HOCTœ
       I3 = SPIS( ISPIS+2 )
       VI2 = 0.0
       VI3 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
       IF ( I3 .NE. 0 ) VI3 = VALLN( I3 )
       RHELP( IHELP ) = VI2 - VI3
       GO TO 50
53     CONTINUE
C-->> "TOK"
       RHELP(IHELP) = TOKN(I2)
C$$$$$$GO TO 50
50     CONTINUE
         KPP = KPP - 1
         IHELP = IHELP + 1
         ISPIS = ISPIS + 6
         GO TO 28
29       CONTINUE
c         WRITE(99,30000) TIME,(RHELP(I), I=1,5  )
c30000    FORMAT('[18;0H',G10.5, 9('³': G12.5 )  )
C*****************************************
         WRITE(99,30000) TIME
30000    FORMAT(1X,'[0;0H',G10.5 )
c ‡ ¤ ¥¬ ¬ áèâ ¢ &&&&&&&&&&&&&&&&%%%%%%%%%%%%%%%%%%^^^^^^^^^^^^^^^^%$#^%$
         XLN = TIME/(HOUTT)
         ln =IFIX (XLN)
         do 345 i = 1,kp 
         XLM(i) = RHELP(i)*kplin2/TMAX(i) 
         lm(i) = IFIX (XLM(i))
345      continue         
c         PRINT *,TIME,HOUTT,RHELP(1),RHELP(2),TMAX1,TMAX2
c         PRINT *,ln,lm1,lm2
          IF (ln.LE.639) GOTO  333
c********************************************
C   made files 'picture'
          if(ln.EQ.640) goto 553
          kpic = MOD(ln,640)
          if (kpic.NE.637) goto 555
553       kppp = (ln+3)/640
c          call picture(kppp)
555       ILL = ln - 639
          call pan(ILL,0)
          CALL DCOLOR(0)
          call drwlog(0)
c          if(ILL.EQ.1)  call rectab(ln,0,ln+2,349,1)
          call rectab(ln,0,ln+3,349,1)
          call drwlog(2)
          kkk = MOD(ln,20)
          if (kkk.NE.1) goto 333
c          call dcolor(7)
c          call drawab(ln-1,0)
c          call drawab(ln-1,349)
333       continue
c  make koordinat
c          call drwlog(0)
c         call wrpixl( ln, 175, 15)
         if (ln.EQ.0) goto 191
         print *,'[0;1H','[s'
         jk = MOD(ln,100)
         if (jk.NE.0) goto 191
                jk1 = ln/100
            jkp = ln
           if (ln.GE.700) jkp = MOD(ln,640)
                jk2 = (jkp-16)/8 
         do 1331 i =1,jk2
         print *,'[u','[1C','[s'
1331     continue
c         write(*,1333) jk1
1333     format(1x,'[u',I2)
191     continue 
         do 195 i = 1,kp
         IF (lm(i).GE.kplin2) THEN
         lm(i) = kplin2
         ELSEIF (lm(i).LE. -kpline) THEN
         lm(i) = - kplin2
         ELSE
         ENDIF
195      continue          
          call dcolor(15)
         call drawab(ln,kplin2 - lm(1))
         do 199 i = 2,kp
         call dcolor(15 - i)
         if ((lm(i).GE.kplin2).OR.(lm(i).LE.-kplin2)) 
     =                   call dcolor(0)
         call moveab(ln-1,kplin2 + (i-1)*kpline - kor(i))
         call drawab(ln,kplin2 + (i-1)*kpline - lm(i))
         kor(i) = lm(i)
199      continue         
         call moveab(ln,kplin2 - lm(1))
         do 331 i = 1,kp
          call wrpixl(ln,kpline*i -kplin2,7)
          call wrpixl(ln-2,kpline*i -kplin2,7)
331       continue
1111     continue
C ---> ‚ë¢®¤¨¬ ­  ¯¥ç âì á § ¤ ­­ë¬ è £®¬ !
C*****************************************************
         IF ( KOD .EQ. 4  .OR. KOD .EQ. 5 ) GO TO 4
         Kxxx = Kxxx + 1
         IF ( Kxxx .EQ. KOUTON ) Kxxx = 0
         IF ( Kxxx .EQ. 0 ) GO TO 4
         RETURN
C######################################################################
4        CONTINUE 
         IF ( KOD .EQ. 2 ) WRITE(NWTR,30) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 4 ) WRITE(NWTR,31) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 5 ) WRITE(NWTR,46) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 6 ) WRITE(NWTR,47) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 7 ) WRITE(NWTR,48) TIME,(RHELP(I), I=1,KP )
30       FORMAT(1X,G10.5, 9('³', G12.5 )  )
31       FORMAT(1X,G10.5, 9('*', G12.5 )  )
46       FORMAT(1X,G10.5, 9('T', G12.5 )  )
47       FORMAT(1X,G10.5, 9('@', G12.5 )  )
48       FORMAT(1X,G10.5, 9('I', G12.5 )  )
CC31       FORMAT(1X,'*',G9.4, '*',  G12.5, 8( '!', G12.5 )  )
CC46       FORMAT(1X,'T',G9.4, 'T',  G12.5, 8( '!', G12.5 )  )
CC47       FORMAT(1X,'@',G9.4, '@',  G12.5, 8( '!', G12.5 )  )
         RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
C######################################################################
3        CONTINUE
C--> ‡AKP›Tœ
         DO 32 I=1,32
32       HELP(I) = TIRE
         KSTR = 11 + 13 * KP
C--------
         KSTR4 = KSTR/4
         WRITE(NWTR,33) ( HELP(I), I=1, KSTR4 )
c                     call picture(0)
33       FORMAT(1X, 32A4 // )
c*******************************************
                     pause ''
                     call finit()
c*****************************************************
         RETURN
         END
