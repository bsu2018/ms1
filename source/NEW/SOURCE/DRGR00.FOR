         SUBROUTINE DRGR00(KOD, MKOD, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DRGR00(KOD, MKOD, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
C   ------------
C   HA‡HA—EHˆE:    ˆCO‹HˆTE‹œH›‰ "MEXAHˆ‡M" B›BO„A "ƒPA”ˆ—ECKO‰"
C   ----------     ˆH”OPMA–ˆˆ „ˆPEKTˆB› #ƒPA”;
C
C   METO„:         O—EBˆ„EH;
C   -----          (EC‹ˆ Pˆ ‡APOCE HAKO‹EHˆŸ TO—EK B›BO„A PE„‹AƒAžT
C                  PˆHŸTœ O‹œ˜E TO—EK, —EM ‡A„AHO B NTGRAF Pˆ KOD=1,
C                  TO ˆ‡›TO—H›E TO—Kˆ ˆƒHOPˆP“žTCŸ...)
C
C   APAMETP›:     KOD = 1 - OTKP›Tœ PˆEM TO—EK B›BO„A;
C   ---------          = 2 - PˆHŸTœ O—EPE„H“ž TO—K“;
C                      = 3 - ‡AKP›Tœ PˆEM TO—EK, E—ATATœ HA NWTR;
C                  MKOD = 0 - B›BO„ ƒPA”ˆKOB B ˆH„ˆBˆ„“A‹œH›X MAC˜TAAX;
C                       = 1 - B›BO„ BCEX ƒPA”. B O„HOM MAC˜TAE;
C                       = 2 - B›BO„ KAK Pˆ =0 ˆ Pˆ =1 („BA PˆC“HKA);
C                  SPIS - CˆCOK "‡APOC B›BO„A";
C                  TIME, TOKN, VALLN - MACCˆB› ‡HA—EHˆ‰ O—EPE„HO‰ TO—Kˆ;
C                  NDX - PA‡MEPHOCTœ - PAO—EƒO MACCˆBA XGRAF;
C                  XGRAF - PAO—ˆ‰ MACCˆB, MAX PA‡MEPHOCTˆ
C                        <1+KO‹-BO ƒPA”ˆKOB> * <KO‹-BO TO—EK B›BO„A >
C                         ( CM. PˆME—AHˆE );
C
C   O„POƒPAMM›:  1. PLOT2, MOVE0
C   ------------   2. COMMON PROF1, SERVZ2, SERVZ3;
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HE PBO„ˆTCŸ;
C                  2. KO‹ˆ—ECTBA TO—EK B›BO„A B›ˆPAETCŸ ˆ‡
C                     MASPRT(47) = NTGRAF
C                  3. Pˆ MA‹OM PA‡MEPE MACCˆB› XGRAF:
C                     KOƒ„A NTGRAF > NDX, OC“™ECTB‹ŸETCŸ
C                     PA‡ˆEHˆE B›BO„HOƒO OTOKA HA "OP–ˆˆ"
C                  4. "CETKA" O BPEMEHHO‰ OCˆ B›ˆPAETCŸ ˆ‡ XGRGAD
C                      MASPRT(63), ‡A„ABAEMAŸ B ‹. ƒPA„“CAX;
C
CZZZ-------------------------------------------------------------------
         DIMENSION SPIS(1), TOKN(1), VALLN(1), XGRAF(1)
         INTEGER SPIS, FRAGM, ANM, ANT, TABTOK, TABTIP, TIP
         COMMON/PROF1/ MASPRT(80)
         COMMON/SERVZ2/ TABTOK(1)
         COMMON /SERVZ3/ TABTIP(1)
         INTEGER HELP, DIAL, PX
         DIMENSION HELP(80), RHELP(1)
         EQUIVALENCE ( HELP(1),  RHELP(1) )
         EQUIVALENCE ( NRDR,     MASPRT(12) )
         INTEGER YES, NO
         DIMENSION YES(2), NO(3)
         EQUIVALENCE ( NWTR, MASPRT(13) )
         EQUIVALENCE ( NWTRE, MASPRT(14) )
         EQUIVALENCE ( DIAL,  MASPRT(15) )
         EQUIVALENCE ( NTGRAF,  MASPRT(47) )
         EQUIVALENCE ( HOUTT,   MASPRT(48) )
         EQUIVALENCE ( XGRGAD,  MASPRT(63) )
C
         DATA YES/'„', 'A' /, NO / 'H', 'E', 'T' /
C**********************************************************************
         KP = SPIS(1)
         IF ( KP .GT. 9 ) KP = 9
         GO TO (1,2,3), KOD
         CALL SUBERR(KOD)
1        CONTINUE
C--->  OTKP›Tœ „OCT“
C-->  CETKA:
         NXX = 0
         IF ( (XGRGAD/1.8E+4) .GT. HOUTT )
     =                         NXX = INT( XGRGAD/(HOUTT*1.8E+4) + 0.5 )
         IF ( NXX .EQ. 0 ) NXX = 5
C->> CKO‹œKO MAX TO—EK MO†EM B›BECTˆ ‡A O„ˆH "‡AXO„"
         NX1 = NDX/(1+KP)
C---> HO HAM HA„O B›BO„ˆTœ NXT TO—EK:
         NXT = NTGRAF
C-->  HA‰„EM CKO‹œKO MO†EM B›BO„ˆTœ B O„HOM ”PAƒMEHTE...
C     FRANM - ”‹Aƒ ”PAƒMEHTOB, NREAL - C.—.K. PˆHŸT›X TO—EK
         IF ( NXT .LE. NX1 ) GO TO 10
C--> NX B›EPEM O MAX, HO KPATHO CETKE...
         NX = (NX1/NXX) * NXX
         FRAGM = 1
         GO TO 11
10       NX = NXT
         FRAGM = 0
11       CONTINUE
         NREAL = 0
C--->  B›BO„ ‡AƒO‹OBKA...
         WRITE(NWTR,12)
12       FORMAT(1H1/20X,' ƒPA”ˆKˆ ‡HA—EHˆ‰ '/)
         ISPIS = 2
         IHELP = 1
         KPP = KP
         NNG = 0
13       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 14
         NNG = NNG + 1
         PX = SPIS(ISPIS)
         GO TO ( 15, 16, 17 ), PX
         CALL SUBERR(PX)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
15       CONTINUE
C--->  HAPŸ†EHˆŸ
         NV = SPIS(ISPIS+1)
         WRITE(NWTR, 502) NNG, NV
502      FORMAT(1X,'CˆMBO‹ ', I2, ' - ', 'HAPŸ†EHˆE “‡‹A ', I5, ' ;')
         KPP = KPP -1
         ISPIS = ISPIS + 6
         GO TO 13
16       CONTINUE
C-->  PA‡HOCTœ
         NV = SPIS(ISPIS+1)
         NV1 = SPIS(ISPIS+2)
         WRITE(NWTR,503) NNG, NV, NV1
503      FORMAT(1X,'CˆMBO‹ ', I2, ' - ', 'HAPŸ†EHˆE ME†„“ “‡‹AMˆ ',
     =  I5, ' ˆ ', I5, ' ;' )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         GO TO 13
17       CONTINUE
C---> TOKˆ
         NEL = SPIS(ISPIS + 2)
         ANM = SPIS(ISPIS+3)
         ANT = SPIS(ISPIS+4)
         NSM = SPIS(ISPIS+5)
         IHELP = 1
         HELP(IHELP) = NNG
         IHELP = IHELP + 1
         CALL MOVE0( HELP(IHELP), TABTOK(ANT), 3 )
         IHELP = IHELP + 3
         HELP(IHELP) = NEL
         IHELP = IHELP + 1
         HELP(IHELP) = NSM
         IHELP = IHELP + 1
         CALL MOVE0( HELP(IHELP), TABTIP(ANM), 5 )
         IHELP = IHELP + 4
         WRITE(NWTR, 504) NNG,(HELP(I),I=2,4),NEL,NSM,(HELP(I),I=7,11)
504      FORMAT(' CˆMBO‹ ', I2, ' - ',3A4, ' --> ', '‹EMEHT ', I5,
     =        ' ‡HA—EHˆE ', I3, ' (', 5A4, ');'  )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         GO TO 13
14       CONTINUE
C-->  "“CE..."
       WRITE(NWTR, 5040 )
5040   FORMAT(/)
         RETURN
C######################################################################
2        CONTINUE
C-->  PˆHŸTœ O—EPE„H–ž, NX  ƒOTOBO...
         NREAL = NREAL + 1
C--->  BO‡MO†HO := PE„‹AƒAžT ‹ˆ˜Hžž TO—K“ ??, EC‹ˆ „A, TO ˆƒHOPTP“EM!
         IF ( NREAL .GT. NX ) RETURN
         NNG = 0
         ISPIS = 2
         KPP = KP
         XGRAF(NREAL) = TIME
20       CONTINUE
C-->  PA‡OP CˆCKA, NNG - HOMEP ƒPA”ˆKA
         NNG = NNG + 1
         IF ( KPP .LE. 0 ) GO TO 21
         PX = SPIS(ISPIS)
         I2 = SPIS(ISPIS+1)
C$$$$$$$ I3 = SPIS(ISPIS+2)
         GO TO (22, 23, 24), PX
         CALL SUBERR(PX)
C--->
C--->  HAPŸ†EHˆŸ:
22     CONTINUE
       VI2 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
         XGRAF( NREAL + NNG*NX ) = VI2
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 20
C--->  PA‡HOCTœ
23     CONTINUE
       I3 = SPIS( ISPIS + 2 )
       VI2 = 0.0
       VI3 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
       IF ( I3 .NE. 0 ) VI3 = VALLN( I3 )
         XGRAF( NREAL + NNG*NX ) = VI2 - VI3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 20
C--->  TOKˆ
24       XGRAF( NREAL + NNG*NX ) = TOKN(I2)
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 20
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
21       CONTINUE
C-->  TO—K“ PˆHŸ‹ˆ, HEH“†HO ‹ˆ B›BECTˆ ”PAƒMEHT....
         IF ( NREAL .EQ. NX  .AND.  FRAGM .EQ. 1 ) GO TO 25
         RETURN
C######################################################################
25       CONTINUE
C--> B›BO„ ”PAƒMEHTA
         KSY = 123
         IF ( DIAL .EQ. 1 ) KSY = 74
         NIII = NX + 1
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX,
     =               10, NWTR, MKOD )
C======================================================================
C         IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C510      FORMAT('*** †E‹AETE POCMOTPETœ E™E PA‡? („A/HET) ***')
C         IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C         IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C     =        HELP(2) .EQ. YES(2) ) GO TO 25
C======================================================================
C-->  HE H“†HO ‹ˆ OBTOPˆTœ, Pˆ MKOD = 2
         IF ( MKOD .NE. 2 ) GO TO 26
C--> OBTOP
250      WRITE(NWTR, 505 )
505      FORMAT(//' *** B›BO„ PE„›„“™ˆX ƒPA”ˆKOB B ˆH„ˆBˆ„“A‹œH›X',
     = ' MAC˜TAAX ***'//)
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX,
     =                10, NWTR, 0 )
C======================================================================
C         IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C511      FORMAT(80A1)
C         IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C         IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C     =        HELP(2) .EQ. YES(2) ) GO TO 250
C======================================================================
26       CONTINUE
C-->  ”PAƒMEHT B›BE‹ˆ
         NXT = NXT - NX
         NREAL = 0
         IF ( NXT .LE. NX1 ) GO TO 27
C--> E™E H“†EH ”PAƒMEHT...O„ƒOTOBˆM EƒO....
         FRAGM = 1
         NX =(NX1/NXX) * NXX
         RETURN
27       CONTINUE
C-->  HAKOˆM OC‹E„Hˆ‰ ”PAƒMEHT...
         NX = NXT
         FRAGM = 0
         RETURN
C######################################################################
3        CONTINUE
C===> KOHTPO‹œ HA KO‹-BO TO—EK...
         IF ( NREAL .LT. NX ) WRITE(NWTRE,2501) NREAL, NX, NXX
2501    FORMAT(' *** ‹Oƒ. HECOOTBETCTBˆ B DRGR00: - PAHHEE ‡AKP›TˆE B›BO
     =„A ***'/ ' *** NREAL =',I5,' NX =',I5, ' NXX=',I5,' ***')
C===>
C--> ‡AKPOEM PˆEM...
C!!!  ˆ‡Œ…… ‚›‚Ž„  €  80  Ž‡ˆ–ˆ‰
         KSY = 74
         IF ( DIAL .EQ. 1 ) KSY = 74
         NIII = NX + 1
C!!!-------------------------------------------------------------------
C        WRITE(NWTRE,700) NREAL, NX, NIII, NXX, KP
C700     FORMAT(' ***NREAL=',I5,' NX=',I5, ' NIII=', I5,
C    =  ' NXX=',I5, ' KP=',I5,'  ***')
C        IXXX=(KP+1)*NX+10
C        WRITE(NWTRE,701) (I, XGRAF(I), I=1,IXXX)
C701     FORMAT( 5(1X,'G(', I4, ')=', G12.5 ) )
C!!!-------------------------------------------------------------------
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX,
     =                10, NWTR, MKOD)
C======================================================================
C        IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C-->
C        IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C        IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C    =        HELP(2) .EQ. YES(2) ) GO TO 3
C======================================================================
         IF ( MKOD .NE. 2 ) RETURN
2500     WRITE (NWTR, 505)
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX, 10,
     =               NWTR, 0 )
C======================================================================
C        IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C--->
C        IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C        IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C    =        HELP(2) .EQ. YES(2) ) GO TO 2500
C======================================================================
         RETURN
         END
