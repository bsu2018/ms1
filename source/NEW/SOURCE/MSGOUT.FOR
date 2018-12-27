         SUBROUTINE MSGOUT( MSG )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C     O„POƒPAMMA : MSGOUT ( MSG )
C     ------------
C     HA‡HA—EHˆE :  B›BO„ PAC˜ˆPEHHOƒO TEKCTOBOƒO COO™EHˆŸ HA NWTR
C    ------------
C     METO„:        B›OP HOMEPOB ˆ‡ BXO„HOƒO MACCˆBA MSG ˆ E—ATœ.
C     -----
C     APAMETP›:   MACCˆB MSG => MSG(1) - KO‹ˆ—ECTBO ‡AHŸT›X O‡ˆ–ˆ‰
C     ---------                  (HOMEPOB COO™EHˆ‰, KOTOP›X HEOXO„ˆMO
C                                 B›BECTˆ) B MSG(2) ... MSG( MSG(1) );
C                                MSG(J) => HOMEP COO™EHˆŸ;
C    O„POƒPAMM›:  COMMON / PROF1 /
C    ------------
C    PˆME—AHˆE: 1. †E‹ATE‹œHO OECE—ˆTœ „‹ˆH“ CTPOKˆ COO™EHˆŸ HE
C    ----------     O‹EE 80 CˆMBO‹OB;
C                2. B TEK“™E‰ BEPCˆˆ  Pˆ„“CMOTPEHHO HE O‹EE MMSG
C                   COO™EHˆ‰ [ 100 ];
C                3. PEA‹ˆ‡OBAHO MAXMSG COO™EHˆ‰  [ 33 ];
C                4. „‹Ÿ „O ‡AˆCˆ HOMEPA COO™EHˆŸ B MSG(*):
C                   I = MSG(1); MSG(I+2) = HOMEP; MSG(1) = I + 1;
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSG(1)
         COMMON /PROF1/ MASPRT(200)
        DATA MAXMSG/35/, MMSG/100/
C             *******************
         NWTR = MASPRT(13)
         NWTRE = MASPRT(14)
         MAX = MSG(1)
         IF ( MAX .EQ. 0 ) RETURN
         IF (MAX .LT. 0  .OR.  MAX .GT. 100 ) GO TO 667
         IMAX = MAX + 1
         DO 666 I=2,IMAX
         IF ( MSG(I) .GT. MAXMSG ) WRITE(NWTR,668) MSG(I)
C----------------------------------------------------------------------
         IF ( MSG(I) .EQ. 1 ) WRITE(NWTR,1)
         IF ( MSG(I) .EQ. 2 ) WRITE(NWTR, 2)
         IF ( MSG(I) .EQ. 3 ) WRITE(NWTR, 3)
         IF ( MSG(I) .EQ. 4 ) WRITE(NWTR, 4)
         IF ( MSG(I) .EQ. 5 ) WRITE(NWTR, 5)
         IF ( MSG(I) .EQ. 6 ) WRITE(NWTR, 6)
         IF ( MSG(I) .EQ. 7 ) WRITE(NWTR, 7)
         IF ( MSG(I) .EQ. 8 ) WRITE(NWTR, 8)
         IF ( MSG(I) .EQ. 9 ) WRITE(NWTR, 9)
         IF ( MSG(I) .EQ.10 ) WRITE(NWTR, 10)
         IF ( MSG(I) .EQ.11 ) WRITE(NWTR, 11)
         IF ( MSG(I) .EQ.12 ) WRITE(NWTR, 12)
         IF ( MSG(I) .EQ.13 ) WRITE(NWTR, 13)
         IF ( MSG(I) .EQ.14 ) WRITE(NWTR, 14)
         IF ( MSG(I) .EQ.15 ) WRITE(NWTR, 15)
         IF ( MSG(I) .EQ.16 ) WRITE(NWTR, 16)
         IF ( MSG(I) .EQ.17 ) WRITE(NWTR, 17)
         IF ( MSG(I) .EQ.18 ) WRITE(NWTR, 18)
         IF ( MSG(I) .EQ.19 ) WRITE(NWTR, 19)
         IF ( MSG(I) .EQ.20 ) WRITE(NWTR, 20)
         IF ( MSG(I) .EQ.21 ) WRITE(NWTR, 21)
         IF ( MSG(I) .EQ.22 ) WRITE(NWTR, 22)
         IF ( MSG(I) .EQ.23 ) WRITE(NWTR, 23)
         IF ( MSG(I) .EQ.24 ) WRITE(NWTR, 24)
         IF ( MSG(I) .EQ.25 ) WRITE(NWTR, 25)
         IF ( MSG(I) .EQ.26 ) WRITE(NWTR, 26)
         IF ( MSG(I) .EQ.27 ) WRITE(NWTR, 27)
         IF ( MSG(I) .EQ.28 ) WRITE(NWTR, 28)
         IF ( MSG(I) .EQ.29 ) WRITE(NWTR, 29)
         IF ( MSG(I) .EQ.30 ) WRITE(NWTR, 30)
         IF ( MSG(I) .EQ.31 ) WRITE(NWTR, 31)
         IF ( MSG(I) .EQ.32 ) WRITE(NWTR, 32)
         IF ( MSG(I) .EQ.33 ) WRITE(NWTR, 33)
         IF ( MSG(I) .EQ.34 ) WRITE(NWTR, 34)
         IF ( MSG(I) .EQ.35 ) WRITE(NWTR, 35)
C
C----------------------------------------------------------------------
666      CONTINUE
         MSG(1) = 0
         RETURN
667      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 500) MAX
500      FORMAT(' *** ‹Oƒ. CO‰ Pˆ B›‡OBE MSGOUT, MSG(1)=', I12)
         CALL SUBERR(1)
         RETURN
668      FORMAT (' *** COO™EHˆE # ', I4, ' HE PEA‹ˆ‡OBAHO ***')
C######################################################################
1        FORMAT(' *** HE OO‡HAHO HA—A‹O ‹Oƒˆ—ECKO‰ CTPOKˆ ***')
C---> 2
2        FORMAT(' *** BBO„ˆMOE —ˆC‹O C‹ˆ˜KOM BE‹ˆKO ***'/
     =          ' *** BO‡MO†HO - PO“CK PA‡„E‹ˆTE‹œHOƒO POE‹A ***')
C--->3
3        FORMAT(' *** BBO„ˆMOE —ˆC‹O CO„EP†ˆT HE „O“CTˆM‰ CˆMBO‹ ***')
C----->4
4        FORMAT(' *** Tˆ MAKPOMO„E‹ˆ O˜ˆO—EH ˆ‹ˆ HE PEA‹ˆ‡OBAH ***')
C-----> 5
5        FORMAT(' *** HE OPE„E‹EH HOMEP MAKPOMO„E‹ˆ ***'/
     =  ' *** OˆCAHˆE CˆCKA "“" [ “‡‹› ]  PˆHŸTœ HEBO‡MO†HO ***')
C-----> 6
6        FORMAT(' *** HE „O“CTˆM›‰ HOMEP „‹Ÿ OO‡HA—EHˆŸ “‡‹A ***')
C---->  7
7        FORMAT(' *** HE OPE„E‹EH HOMEP MAKPOMO„E‹ˆ ***'/
     = ' *** OˆCAHˆE CˆCKA "" [ APAMETP› ] PˆHŸTœ HEBO‡MO†HO ***')
C----->8
8        FORMAT(' *** „AHHO‰ MAKPOMO„E‹ˆ APAMETP› HE H“†H› ***')
C---->9
9        FORMAT(' *** B ‹Oƒˆ—ECKO‰ CTPOKE OˆCAHˆŸ “KA‡AHA HE BCŸ ˆH”OPM
     =A–ˆŸ ***')
C---->10
10       FORMAT(' *** HAP“˜EHA CTP“KT“PA CˆCKA "" - O†ˆ„AETCŸ "MAS" **
     =*')
C---->11
11       FORMAT(' *** HET "<" B CˆCKE APAMETPOB [ "MAS" ] ***')
C----->12
12     FORMAT(' *** HAP“˜EHA CTP“KT“PA CˆCKA "" - O†ˆ„AETCŸ "FT" ***')
C------>13
13       FORMAT(' *** BCTPOEHAŸ ”“HK–ˆŸ "FT" ?? HE PEA‹ˆ‡OBAHA ***')
C------>14
14    FORMAT(' *** B CˆCKE "" OˆCAHˆŸ "FT" HET OƒPAHˆ—ˆTE‹Ÿ ">" ***')
C------>15
15       FORMAT(' *** B TEK“™EM CˆCKE MHOƒO APAMETPOB ***')
C---->16
16       FORMAT(' *** CˆCOK FT # "<......>" - MA‹ ***')
C---->17
17       FORMAT(' *** HET "<" B CˆCKE APAMETPOB [ "FT" ] ***')
C---->18
18       FORMAT(' *** HAP“˜EHA CTP“KT“PA CˆCKA "" - O†ˆ„AETCŸ "PSUB" *
     =**')
C---->19
19       FORMAT(' *** HET "<" B CˆCKE APAMETPOB "PSUB" ***')
C---->20
20       FORMAT(' *** HEO†ˆ„AHH›‰ KOHE– ‹Oƒˆ—ECKO‰ CTPOKˆ - MA‹O „AHH›X
     =B CˆCKE ***')
C---->21
21       FORMAT(' *** HEOO‡HAHO HA—A‹O O„CˆCKA ***' / ' *** HET "V",
     = "U", "I", "GAMMA", "OMEGA", "MM", "MBH" ***')
C--->22
22       FORMAT(' *** CˆCOK "‡APOC B›BO„A" O‹œ˜O‰ - “CEKAETCŸ ***')
C--->23
23       FORMAT(' *** OTME—EHHOƒO “‡‹A B –Eˆ HET ***')
C--->24
24       FORMAT(' *** OTME—EHHOƒO ‹EMEHTA –Eœ HE CO„EP†ˆT ***')
C---->25
25       FORMAT(' *** OTME—EHHOE "‡HA—EHˆE" HE „O“CTˆMO ***')
C----> 26
26       FORMAT(' *** OTME—EHH›‰ ‹EMEHT, ‡APO˜EHH›X HA B›BO„ BE‹ˆ—ˆH,
     = HE CO„EP†ˆT ***')
C--->27
27       FORMAT(' *** HE OPE„E‹EH HOMEP ‡BEHA  ***')
C--->28
28       FORMAT(' *** HOMEP CBŸ‡ˆ O‹œ˜E HOMEPA ‡BEHA  ***')
C--->29
29       FORMAT(' *** CBŸ‡œ C ‡BEHOM, ˆMEž™ˆM O‹œ˜ˆ‰ HOMEP ***')
C--->30
30       FORMAT(' *** Tˆ CBŸ‡ˆ O˜ˆO—EH  ***')
C--->31
31       FORMAT(' *** CBŸ‡œ HE OPE„E‹EHA ***')
C--->32
32       FORMAT(' *** BBO„ˆM XAPAKTEPˆCTˆKˆ ‡BEHA, HE BBE„Ÿ OˆCAHˆŸ',
     *                ' CBŸ‡E‰ *** ' )
C--->33
33       FORMAT(' *** HAP“˜EH OPŸ„OK C‹E„OBAHˆŸ APAMETPOB  ***')
C--->34
34       FORMAT(' *** OTME—EHH›‰ “‡…‹ … ‘Ž•€Ÿ…’‘Ÿ ¢ #SPOOL ***')
C---> 35
35    FORMAT(' *** OTME—EHHŽ… "‡€—…ˆ…" … ‘Ž•€Ÿ…’‘Ÿ ¢ #SPOOL ***')
C
C
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
