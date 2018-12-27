         SUBROUTINE GETMSW( BA0, KNOT, PAR0, MSWALL, IER )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C
C      O„POƒPAMMA: GETMSW( BA0, KNOT, PAR0, MSWALL, IER )
C      ------------
C
C      HA‡HA—EHˆE: -  ”OPMˆP“ET CˆCOK “PAB‹Ÿž™ˆX C‹OB MAKPOMO„E‹E‰
C      -----------    MSWALL  [ M_ACRO S_TATUS W_ORD ],
C                     CO„EP†A™EƒO BH“TPEHHEE OˆCAHˆE MAKPOMO„E‹E‰;
C                  - ”ˆKCˆP“ET HOMEP HA—A‹A EPBOƒO OˆCAHˆŸ B MSWALL
C                    ‹EMEHTOB C Pˆ‡HAKOM TˆA MO„E‹ˆ PABH›M # 5
C                    ( T5MSW = MASPRT(41) );
C                  - ”ˆKCˆP“ET HOMEP HA—A‹A EPBOƒO OˆCAHˆŸ B MSWALL
C                    ‹EMEHTOB C Pˆ‡HAKOM TˆA MO„E‹ˆ PABH›M # 6
C                    ( T6MSW = MASPRT(49) );
C                  - OPE„E‹ŸET KO‹ˆ—ECTBO B›BO„H›X ‡HA—EHˆ‰ TOKOB ˆ
C                    CE–. EPEMEHH›X ( COƒ‹ACHO O‹ž MACTAB(6) ),
C                    O‹“—EHHOE ‡HA—EHˆE   ===>  ‡AˆC›BAETCŸ B
C                    NNTOK O‹E MASPRT(46);
C                  - Pˆ HA‹ˆ—ˆˆ ‹EMEHTOB, MO„E‹ˆP“EM›X MAKPO-
C                    MO„E‹ŸMˆ C ˆ„EA‹œH›Mˆ BEHTˆ‹ŸMˆ ( .Tˆ # 6 ) -
C                    BO‡“†„AET ”‹Aƒ  FLOG  ( MASPRT(37) <-- 1 );
C                    !!! BHˆMAHˆE !!! PE„O‹OƒAETCŸ, —TO ˆH”OPMA–ˆŸ
C                     B MSWALL OTCOPTˆPOBAHA O O‹ž "Pˆ‡HAK TˆA";
C
C      METO„:       ‡AO‹HEHˆE MSW COƒ‹ACHO ˆH”OPMA–ˆOHH›X MACˆBOB
C      -----        BA0, KNOT ˆ PAR0; „ETA‹œHAŸ HACTPO‰KA
C                   KOHKPETHO‰ MAKPOMO„E‹ˆ POˆ‡BO„ˆCŸ /
C                   C ˆMEHAMˆ FM_XXXX ( CM. TA. MACTAB )
C                   HACTPO‰KA ‡AK‹ž—AETCŸ B B›—ˆC‹EHˆˆ BCEX
C                   HEOXO„ˆM›X KOHCTAHT, ˆCO‹œ‡“EM›X B
C                   / DM_XXXX - PAC—ETA MAKPOMO„E‹ˆ;
C
C                   „‹Ÿ ‹Oƒˆ—ECKO‰ CBŸ‡ˆ MCW C‹OB--> Pˆ HEOXO„ˆMOCTˆ
C                       B›‡›BAET  CALLLM - / POCMOTPA MSW_ALL
C                       ˆ B›‡OBA Pˆ ‡APOCE ( —EPE‡ MACTAB OˆCAHˆE )
C                       COOTBET. LM..@..  MOHˆTOPOB HACTPO‰Kˆ..
C
C
C
C     APAMETP›:    ˆH”OPMA–ˆOHH›E MACˆB›:
C     ---------     BA0 - MACCˆB CˆCKA ‹EMEHTOB;
C                   KNOT - MACCˆB CˆCKA “‡‹OB O„K‹ž—EHˆŸ;
C                   PAR0 - MACCˆB CˆCKA APAMETPOB;
C                          ( CM. INPUT0 );
C                   MSWALL - MACCˆB “PAB‹Ÿž™ˆX C‹OB MAKPOMO„E‹ˆ;
C                   IER -  = 0  BCE Tˆ› MO„E‹E‰ „OCT“H›;
C                          = N  Tˆ HOMEP N HE „OCT“EH,
C                               („A‹œHE‰˜ˆ‰ POCMOTP BA0
C                                PEKPA™EH);
C                          = 1 - LM MOHˆTOP B›ŸBˆ‹ ‹Oƒˆ—ECK“ž O˜ˆK“;
C                          ******************************************
C
C
C     O„POƒPAMM›: 1. CALLFM - "B›‡›BA‹œ™ˆK" MOHˆTOPOB FM..@...
C     ------------
C                   2. COMMON PROF1, PROF2, PROF3
C                   3.         SUBERR;
C                   4. CALL_LM - "B›‡›BA‹œ™ˆK" MOHˆTOPOB LM..@..
C
C
C    PˆME—AHˆE:    1. OC‹E “CE˜HOƒO ”OPMˆPOBAHˆŸ MSWALL
C    ----------        MACCˆB› BA0, KNOT ˆ PAR0 HE H“†H›;
C                   2. CTP“KT“PA B›‡›BAž™EƒO PE„‹O†EHˆŸ FM.( B CALLFM):
C                      CALL FM.....( BA0T, KNOTT, PAR0T,
C                                    MSW<EPEMEHHAŸ —ACTœ>,
C                                    LM );
C                      ‡„ECœ:
C                      ...T - TEK“™ˆE ‡‹EMEHT› MACCˆBOB,
C                      MSW<EP. —ACTœ> - TEK“™EE MECTO B MSWALL,
C                      HO !!!BHˆMAHˆE!!! HA—ˆHAŸ C O‹Ÿ "SMP"
C                       ( MˆH“Ÿ CˆCOK “‡‹OB );
C                      LM - „‹ˆHA EPEMEHHO‰ —ACTˆ OˆCAHˆŸ MSW;
C                      ( „‹ˆHA TO‹œKO O‹E‰ SPM ˆ SRAB );
C
C                   3. PE„O‹OƒAETCŸ, —TO BA0 OTCOPTˆPOBAH O
C                      BO‡PACTAHˆž Pˆ‡HAKA TˆA MAKPOMO„E‹ˆ;
C
C
C                  CTP“KT“PA MACCˆBA  MSWALL:
C                  --------------------------
C
C  +-------------...+------+------------+------------+...+----------+
C  I            I   I A„P. I"OCTOŸHHAŸ"I"EPEMEHHAŸ"I   IA„P. C‹E„.I
C  I „ˆCKPˆTOP I   I C‹E„.I —ACTœ MSW  I —ACTœ MSW  I   I   ˆ‹ˆ    I
C  I            I   I MSW  I (INTEGER*4)I            I   I  HO‹œ [0]I
C  +------------+...+------+------------+------------+...+----X-----+
C  I1 2 3 4 5   I   I  1   I   8 C‹OB   I    ????    I        I
C  +------------+   +------+------------+------------+        I
C                      I                                      I
C                      V                                      I
C                      +>------------------------------------>+
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW:
C                ----------------------
C
C  +-------+--------+--------+--------+------+------+----------+------+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. I A„P. B›B.I PE   I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I ‡HA—. B  I ‡E   I
C  I HTA   I MO„E‹ˆ I PAC—ETAI        I BO   I ‡HA— I "TOKN" ˆ I PB   I
C  I       I        I DM.....I        I “‡‹OBI EHˆ‰.I B "TOKN1"I      I
C  +-------+--------+--------+--------+------+------+----------+------+
C  I  1    I   2    I   3    I    4   I   5  I  6   I    7     I  8   I
C  +-------+--------+--------+--------+------+------+----------+------+
C
C
C               "EPEMEHHAŸ" —ACTœ MSW:
C               -----------------------
C
C  +----------++-------------+-------------+
C  I CˆCOK   II CˆCOK      I PAO—AŸ     I
C  I “‡‹OB    II BH“TPEHHˆX  I O‹ACTœ     I
C  I O„K‹ž-  II APAMETPOB  I MAKPOMO„E‹ˆ I
C  I —EHˆŸ    II MAKPOMO„E‹ˆ I             I
C  I ‹EMEHTA II             I             I
C  I          II             I             I
C  +----------++-------------+-------------I
C  I 9+K"“‡‹OBII   SPM       I  SRAB       I
C  +----------++-------------+-------------+
C
C  EPEMEHHAŸ —ACTœ O‹HOCTœ ‡AO‹HŸETCŸ / FM.... HA OCHOBE "OTPE‡KOB"
C  ˆH”OPMA–ˆOHH›X MACCˆBOB BA0, KNOT, PAR0
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/PROF2/ MACTAB(800)
         COMMON/PROF3/ FUNTAB(50)
         DIMENSION BA0(*), KNOT(*), PAR0(*), MSWALL(*)
         INTEGER BA0, FM, TIP, TKEL, FMM, DM, ADRMSW, T5MSW
         INTEGER PTIP, T6MSW, ADRTOK, T4MSW
         IER = 0
C===> KAHA‹› E—ATˆ
         NWTR = MASPRT(13)
         NWTRE = MASPRT(14)
C===>  “KA‡ATE‹ˆ
         NNTOK = 0
         MMSWA = MSWALL(2)
         T5MSW = 0
         T6MSW = 0
         T4MSW = 0
C---> “KA‡ATE‹œ CBOO„. MECTA B TOKN ˆ TOKN1
         ADRTOK = 1
         IMSWA = 6
         MSWALL(IMSWA) = 0
         IBA0 = 6
C===> KEL - KO‹-BO ‹EMEHTOB OˆCAHˆŸ B BA0, TKEL - C.—.K.
         KEL = MASPRT(3)
         TKEL = 0
C######################################################################
C  P A ‡  O P   O — E P E „ H O ƒ O  O  ˆ C A H ˆ Ÿ
C######################################################################
1        CONTINUE
C===> HE EPEO‹Hˆ‹ˆCœ ‹ˆ M›?
         IF ( IBA0 .LE. BA0(3) ) GO TO 100
105      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 500) IBA0, TKEL
500      FORMAT(' *** ‹Oƒ. CO‰ B GETMSW: IBA0=', I10, ' TKEL=',I10)
C******* CALL PRINT0( BA0, KNOT, PAR0 )
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
100      CONTINUE
C===>  O‹E O„ A„P. C‹E„.
         ADRMSW = IMSWA
         MSWALL(ADRMSW) = 0
C===> OPE„E‹EHˆE OCTOŸHHO‰ —ACTˆ TEK“™EƒO MSW, “CTAH. HA—. EƒO:
         IMSWA = IMSWA + 1
C===> A„P. HA—A‹A MSW E‡ A„P. C‹E„.
         IMSWA0 = IMSWA
         MSWALL( IMSWA + 0 ) = BA0( IBA0 )
         MSWALL( IMSWA + 1 ) = BA0( IBA0 + 1 )
         MSWALL( IMSWA + 2 ) = -1
C===> KO‹-BO “‡‹OB:
         MSWALL( IMSWA + 4 ) = BA0( IBA0 + 7 )
C===> ‡AˆCœ CˆCKA “‡‹OB IMP ˜T“K;
         IMP = BA0( IBA0 + 7 )
         IK = BA0( IBA0 + 2 )
         DO  101  I=1, IMP
101      MSWALL( IMSWA+ 8 + (I-1) ) = KNOT( IK + (I-1) )
C===> “KA‡ATE‹œ -> HA HA—A‹O EPEMEHHO‰ —ACTˆ, ‡A CˆCKOM “‡‹OB
         IMSWA = IMSWA + 8 + IMP
C==> ƒOTOBˆMCŸ K ”OPMˆPOBAHˆž EPEMEHHO‰ —ACTˆ:
         TIP = BA0( IBA0 + 1 )
         PTIP = BA0( IBA0 + 4 )
         MSWALL( IMSWA0 + 3 ) = PTIP
         IKNOT = BA0( IBA0 + 2 )
         IPAR0 = BA0( IBA0 + 3 )
C===> POBEPˆM HA‹ˆ—ˆE PEA‹ˆ‡A–ˆˆ FM .... & DM....
         I1 = 1
104      CONTINUE
         IF ( MACTAB(I1) .EQ. 0  ) GO TO 102
         IF ( MACTAB(I1) .EQ. TIP ) GO TO 103
         I1 = I1 + MASPRT(17)
         GO TO 104
102      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 501) TIP
501      FORMAT(' *** ‹Oƒ. CO‰--> TˆA # ', I10, ' HET B MACTAB')
C!!!-------------------------------------------------------------------
         GO TO 105
103      CONTINUE
C===> ECTœ ‹ˆ PEA‹ˆ‡A–ˆŸ?
         IF ( MACTAB(I1+7) .GT. 0 ) GO TO 106
C===>  HET !!!
         WRITE(NWTR, 502) TIP
502      FORMAT(//' *** MAKPOMO„E‹œ Tˆ HOMEP: ', I5,
     =          ' OKA HE O„„EP†ˆBAETCŸ ***'/
     =          ' *** OTC“TCB“ET POƒPAMMHAŸ PEA‹ˆ‡A–ˆŸ ***'/
     =          ' *** OˆCAHˆE –Eˆ CPAC›BAETCŸ ***'///)
         IER = TIP
         RETURN
C######################################################################
106      CONTINUE
C===> *********** KO‹-BO B›BO„H›X ‡HA—EHˆ‰
         IKK = MACTAB( I1+5 )
         IKK1 = IKK / 100
         IKK2 = IKK - IKK1 * 100
         MSWALL(IMSWA0 + 5) = IKK2
C===>   ‡AOMHˆM TO KO‹-BO B NNTOK--->
         NNTOK = NNTOK + IKK2
C===> OPE„E‹ŸEM A„PECA B TOKN ˆ TOKN1.....
         MSWALL( IMSWA0 + 6 ) = ADRTOK
C---> B PE‡EPB -1....
         MSWALL( IMSWA0 + 7 ) = -1
C===> HOBOE CBO. MECTO B TOKN ˆ TOKN1....
         ADRTOK = ADRTOK + IKK2
C   ( ADRTOK  <==>  NNTOK + 1  B KOH–E O–ECCA )
C===> B›„E‹ˆM FM ˆ DM:
         KODFM = MACTAB(I1+6)
         FM = KODFM/1000
         DM = KODFM - FM*1000
         MSWALL(IMSWA0 + 2 ) = DM
C**********************************************************************
C  B›‡›BAEM FM@@@@.....
C  IMSWA - “KA‡ATE‹œ TEK. O‹Ÿ B MSWALL;   BO‡BPAT› HA METK“ 1000
C######################################################################
          CALL CALLFM( FM, BA0(IBA0), KNOT(IKNOT), PAR0(IPAR0),
     =                 MSWALL(IMSWA), LM )
C######################################################################
1000    CONTINUE
C----> „‹Ÿ O—EPE„HOƒO ‹EMEHTA C”OPMˆPOBA‹ˆ "EPEMEHH“ž" —ACTœ MSW
C                          ( EE „‹ˆHA LM C‹OB )
         TKEL = TKEL + 1
         IMSWA = IMSWA + LM
C---> HE EPEO‹Hˆ‹ˆCœ ‹ˆ M› “†E???
         IF ( IMSWA .GT. MMSWA ) GO TO 9999
C---> ‡Aˆ˜EM B HA—A‹O OˆCAHˆŸ A„P. C‹E„.
         MSWALL( ADRMSW ) = IMSWA
       IF ( PTIP .NE. 4 ) GO TO 155
C-->  ”ˆKCA–ˆŸ ‹-TA C .TˆOM # 4
       IF ( T4MSW .EQ. 0 ) T4MSW = ADRMSW
155    CONTINUE
         IF ( PTIP .NE. 5 ) GO TO 150
C===>  ”ˆKCA–ˆŸ ‹EMEHTOB C Pˆ‡HAKOM TˆA MAKPOMO„E‹ˆ # 5
         IF ( T5MSW .EQ. 0 ) T5MSW = ADRMSW
150      CONTINUE
         IF ( PTIP .NE. 6 ) GO TO 151
C==> ”ˆKCA–ˆŸ ‹EMEHTA C .TˆA # 6
         IF ( T6MSW .EQ. 0 ) T6MSW = ADRMSW
151      CONTINUE
C---> “KA‡. C‹. OH“‹ˆM
         MSWALL( IMSWA ) = 0
C----> CMECTˆMCŸ B BA0
         IBA0 = IBA0 + MASPRT(4)
C---->  O„„EP†KA „ˆCKPˆTOPA..
         MSWALL(3) = IMSWA
         IF ( TKEL .LT. KEL ) GO TO 1
C---> K O H — A E M:
         MSWALL(3) = IMSWA
         MASPRT(41) = T5MSW
         MASPRT(49) = T6MSW
         MASPRT(93) = T4MSW
         MASPRT(37) = 0
         IF ( T6MSW .NE. 0 ) MASPRT(37) = 1
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 504) KEL, IMSWA, T4MSW, T5MSW, T6MSW
504      FORMAT(/' *** HACTPO‰KA MAKPOMO„E‹E‰ „‹Ÿ ', I5, ' ‹EMEHTOB',
     = ' ‡ABEP˜EHA ***'/
     =' *** ( ‡AHŸTO ', I10, ' C‹OB B CˆCKE M_S_W_ALL )        ***'/
     =' *** [ T4MSW = ',I5,'  T5MSW = ',I5,' T6MSW = ',I5 ,' ]       ***
     ='/ )
C!!!-------------------------------------------------------------------
C====> B MASPRT(46) - KO‹-BO  B›B. ‡HA—EHˆ‰  TOKA ˆ CE–. EPEMEHH›X.
         MASPRT(46) = NNTOK
C===>  CAMO POBEPKA...
         IF ( ADRTOK .EQ. (NNTOK+1) ) GO TO 152
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 153 ) NNTOK, ADRTOK
153      FORMAT(' *** CO‰ B KOH–E  GETMSW: NNTOK = ', I5,
     =          ' ADRTOK = ', I5, ' ***' )
C!!!-------------------------------------------------------------------
         CALL  SUBERR(1)
152      CONTINUE
C######################################################################
C   O K O H — A T E ‹ œ H A Ÿ  HACTPO‰KA —EPE‡ LM_MOHˆTOP›:
C
       CALL CALLLM( MSWALL, KOD )
       IER = KOD
C----------------------------------------------------------------------
         RETURN
9999     CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 505 ) TKEL, LM, ADRMSW , FM
505   FORMAT(' *** HET MECTA B M_S_W_ALL „‹Ÿ...', I4, ' ‹EMEHTA ***'/
     =       ' *** „‹ˆHA ‡APO˜EHHO‰ . —ACTˆ MSW ', I10, ' C‹OB ***'/
     =       ' *** HA—A‹O OˆCAHˆŸ B MSWALL.......', I10, ' C‹OBA ***'/
     =       ' *** HOMEP MOHˆTOPA FM..............', I10 /)
C----------------------------------------------------------------------
         CALL SUBERR(0)
         RETURN
         END
