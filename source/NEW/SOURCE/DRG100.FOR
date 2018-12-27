         SUBROUTINE DRG100(KOD, KSX, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DRG100(KOD, KSX, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
C   ------------
C   HA‡HA—EHˆE:    ˆCO‹HˆTE‹œH›‰ "MEXAHˆ‡M" B›BO„A "ƒPA”ˆ—ECKO‰"
C   ----------     ˆH”OPMA–ˆˆ „ˆPEKTˆB› #ƒPA”1;
C
C   METO„:         O—EBˆ„EH;
C   -----          (EC‹ˆ Pˆ ‡APOCE HAKO‹EHˆŸ TO—EK B›BO„A PE„‹AƒAžT
C                  PˆHŸTœ O‹œ˜E TO—EK, —EM ‡A„AHO B NTGRAF Pˆ KOD=1,
C                  TO ˆ‡›TO—H›E TO—Kˆ ˆƒHOPˆP“žTCŸ...)
C
C   APAMETP›:     KOD = 1 - OTKP›Tœ PˆEM TO—EK B›BO„A;
C   ---------          = 2 - PˆHŸTœ O—EPE„H“ž TO—K“;
C                      = 3 - ‡AKP›Tœ PˆEM TO—EK, E—ATATœ HA NWTR;
C                  KSX - KO‹ˆ—ECTBO CTPOK O OCˆ "X" ƒPA”ˆKA;
C                  SPIS - CˆCOK "‡APOC B›BO„A";
C                  TIME, TOKN, VALLN - MACCˆB› ‡HA—EHˆ‰ O—EPE„HO‰ TO—Kˆ;
C                  NDX - PA‡MEPHOCTœ - PAO—EƒO MACCˆBA XGRAF;
C                  XGRAF - PAO—ˆ‰ MACCˆB, MAX PA‡MEPHOCTˆ
C                        2 * <KO‹-BO TO—EK OCTPOEHˆŸ ƒPA”ˆKA, NTGRAF>
C                         ( CM. PˆME—AHˆE );
C
C   O„POƒPAMM›:  1. PLOMXY,  MOVE0
C   ------------   2. COMMON PROF1, SERVZ2, SERVZ3;
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HE PBO„ˆTCŸ;
C                  2. KO‹ˆ—ECTBA TO—EK B›BO„A B›ˆPAETCŸ ˆ‡
C                     MASPRT(47) = NTGRAF
C                  3. Pˆ MA‹OM PA‡MEPE MACCˆB› XGRAF:
C                     KOƒ„A NTGRAF > NDX, OC“™ECTB‹ŸETCŸ
C                     OƒPAHˆ—EHˆE NTGRAF = NDX --> E‡ COO™EHˆŸ
C                  4. "CETKA" O "X" OCˆ “CTAHAB‹ˆBAETCŸ PABHO‰ 10;
C
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
         DIMENSION OSXY(4)
         INTEGER OSXY
         EQUIVALENCE ( NWTR, MASPRT(13) )
         EQUIVALENCE ( NWTRE, MASPRT(14) )
         EQUIVALENCE ( DIAL,  MASPRT(15) )
         EQUIVALENCE ( NTGRAF,  MASPRT(47) )
         EQUIVALENCE ( HOUTT,   MASPRT(48) )
         EQUIVALENCE ( XGRGAD,  MASPRT(63) )
C
         DATA OSXY / ' OCœ', ' X  ',  ' OCœ', ' Y  ' /
         DATA YES/'„', 'A' /, NO / 'H', 'E', 'T' /
C**********************************************************************
         KP = SPIS(1)
         IF ( KP .NE. 2 ) CALL SUBERR(KP)
         GO TO (1,2,3), KOD
         CALL SUBERR(KOD)
1        CONTINUE
C--->  OTKP›Tœ „OCT“
C-->  CETKA O "X"
         NXX = 10
C-->  “KA‡ATE‹œ "OCˆ"
         IOSXY = 1
         IOSXY1 = 2
C->> CKO‹œKO MAX TO—EK MO†EM B›BECTˆ ‡A O„ˆH "‡AXO„"
         NX1 = NDX/2
C---> HO HAM HA„O B›BO„ˆTœ NTX TO—EK:
         NTX = NTGRAF
C      NREAL - C.—.K. PˆHŸT›X TO—EK
C-->  EC‹ˆ O˜ˆKA, OƒPAHˆ—ˆMCŸ........
         IF ( NTX .GT. NX1 ) NTX = NX1
         NX = NTX
C        *********
         NREAL = 0
C--->  B›BO„ ‡AƒO‹OBKA...
         WRITE(NWTR,12)
12       FORMAT(1H1/20X,' ƒPA”ˆK APAMETPˆ—ECKO‰ ‡ABˆCˆMOCTˆ '/)
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
         WRITE(NWTR, 502) ( OSXY(II), II=IOSXY,IOSXY1), NNG, NV
502      FORMAT(1X, 2A4,      I2, ' - ', 'HAPŸ†EHˆE “‡‹A ', I5, ' ;')
         KPP = KPP -1
         ISPIS = ISPIS + 6
         IOSXY = 3
         IOSXY1 = 4
         GO TO 13
16       CONTINUE
C-->  PA‡HOCTœ
         NV = SPIS(ISPIS+1)
         NV1 = SPIS(ISPIS+2)
         WRITE(NWTR,503) ( OSXY(II), II=IOSXY, IOSXY1), NNG, NV, NV1
503      FORMAT(1X,  2A4,     I2, ' - ', 'HAPŸ†EHˆE ME†„“ “‡‹AMˆ ',
     =  I5, ' ˆ ', I5, ' ;' )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         IOSXY = 3
         IOSXY1 = 4
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
         WRITE(NWTR, 504) (OSXY(II), II= IOSXY, IOSXY1),
     =                    NNG,(HELP(I), I=2,4),NEL,NSM,(HELP(I),I=7,11)
504      FORMAT(1X,2A4,        I2, ' - ',3A4, ' --> ', '‹EMEHT ', I5,
     =        ' ‡HA—EHˆE ', I3, ' (', 5A4, ');'  )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         IOSXY = 3
         IOSXY1 = 4
         GO TO 13
14       CONTINUE
C-->  "“CE..."
       WRITE(NWTR,5040 )
5040   FORMAT( / )
         RETURN
C######################################################################
2        CONTINUE
C-->  PˆHŸTœ O—EPE„H–ž, NX  ƒOTOBO...
         NREAL = NREAL + 1
C--->  BO‡MO†HO := PE„‹AƒAžT ‹ˆ˜Hžž TO—K“ ??, EC‹ˆ „A, TO ˆƒHOPTP“EM!
         IF ( NREAL .GT. NX ) RETURN
         NNG = 1
         ISPIS = 2
         KPP = KP
C
20       CONTINUE
C-->  PA‡OP CˆCKA, NNG - Pˆ‡HAK OCˆ-->  =1 -> "X",  =0 --> "Y"
C-->  PA‡OP COƒ‹ACHO PˆOPˆTETAM-->  (CM. DRG1F0 )
         IF ( KPP .LE. 0 ) GO TO 21
         PX = SPIS(ISPIS)
         I2 = SPIS(ISPIS+1)
C$$$$$$  I3 = SPIS(ISPIS+2)
         GO TO (22, 23, 24), PX
         CALL SUBERR(PX)
C--->
C--->  HAPŸ†EHˆŸ:
22     CONTINUE
       VI2 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
         XGRAF( 2*NREAL - NNG ) = VI2
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         NNG = 0
         GO TO 20
C--->  PA‡HOCTœ
23     CONTINUE
       I3 = SPIS( ISPIS + 2 )
       VI2 = 0.0
       VI3 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
       IF ( I3 .NE. 0 ) VI3 = VALLN( I3 )
         XGRAF( 2*NREAL - NNG ) = VI2 - VI3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         NNG = 0
         GO TO 20
C--->  TOKˆ
24       XGRAF( 2*NREAL - NNG ) = TOKN(I2)
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         NNG = 0
         GO TO 20
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
21       CONTINUE
C**********************************************************************
C**********************************************************************
         RETURN
C######################################################################
3        CONTINUE
C===> KOHTPO‹œ HA KO‹-BO TO—EK...
         IF ( NREAL .LT. NX ) WRITE(NWTRE,2501) NREAL, NX
2501    FORMAT(' *** ‹Oƒ. HECOOTBETCTBˆ B DRG100: - PAHHEE ‡AKP›TˆE B›BO
     =„A ***'/ ' *** NREAL =',I5,' NX =',I5,            ' ***')
C===>
         IXXI = 0
301      CONTINUE
C--> ‡AKPOEM PˆEM...
C!!!  ˆ‡Œ…… ‚›‚Ž„ € 80 Ž‡ˆ–ˆ‰
         KSY = 74
         IF ( DIAL .EQ. 1 ) KSY = 74
         CALL PLOMXY( XGRAF,              NX,    KSX, KSY, 10,
     =                10, NWTR, XGRAF, IXXI )
C======================================================================
         IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C-->
         IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
         IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
     =        HELP(2) .EQ. YES(2) ) GO TO 300
         RETURN
300      CONTINUE
         IXXI = -1
         GO TO 301
C======================================================================
510      FORMAT(' *** †E‹AETE OCMOTPETœ E™E PA‡ ? („A/HET) ***')
511      FORMAT(80A1)
C--->
         END
