         SUBROUTINE FM0012( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C
C
C   O„POƒPAMMA:  FM0012( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹E‰ C‹E„“ž™ˆX TˆOB:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  63      3-X ”A‡HAŸ .„.C        ES3
C                              CˆH“COˆ„A‹œHOŸ
C
C
C              ‹EMEHT KO„ˆP“ETCŸ:
C              -------------------
C
C               # T 63 “ K0 KA KB KC  R EMAX OMEGA FAZ0;
C
C     K0 - O™ˆ‰ “‡E‹, KA - ”A‡A A, KB - ”A‡A B, KC - ”A‡A C;
C
C---> B›BO„H›E ‡HA—EHˆŸ: TOKˆ: K “‡‹AM KA, KB, KC, OT “‡‹A K0;
C                              *                   **
C
C
C                  13      "ˆ‡MEPˆTE‹œ"
C                           HAPŸ†EHˆŸ             V3
C
C
C              ‹EMEHT KO„ˆP“ETCŸ:
C              -------------------
C
C               # T 13 “ K1 K2 K3;
C
C   B CˆCOK B›—EC‹EHH›X ‡HA—EHˆ‰ OME™AET HAPŸ†EHˆE “‡‹OB K1, K2, K3;
C
C
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹E‰ “KA‡AHH›X TˆOB.
C
C   METO„:         B›—ˆC‹ŸETCŸ ˆ ‡AˆC›BAETCŸ B MSW BCŸ HEOXO„ˆMAŸ
C   -----          ˆH”OPMA–ˆŸ, HEOXO„ˆMAŸ „‹Ÿ OC‹E„“ž™EƒO
C                  PAC—ETA MAKPOMO„E‹ˆ. A„ATˆPOBAHO K METO„“
C                  ‹ˆHˆ„†EPA - BˆO‹“ƒˆ.
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW - O‹Ÿ SPM ˆ SRAB;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HE POˆ‡BO„ˆTCŸ;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C
C                  RMSW Tˆ # 13 [ ˆ‡MEPˆTE‹œ HAPŸ†EHˆŸ ]
C                  ----------------------------------------
C
C                  LM = 0, XPAHŸTCŸ TO‹œKO “‡‹›;
C                  LM<O‹HAŸ> = 8+3=11;
C
C
C                  RMSW TˆA # 63 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ - 3-ˆ ”A‡›]
C                  -----------------------------------------
C                    ( U1 = EM * SIN( OMEGA * T  +  FAZ0 )     LM = 8
C                    ( U2 = EM * SIN( OMEGA * T  +  FAZ0 - 120 )
C                    ( U3 = EM * SIN( OMEGA * T  +  FAZ0 + 120 )
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II ‡HA—.  I ‡HA—.I        I FAZ0 II      IJESIJESI PE  I
C   “‡‹›:     II  1/R   I EM/R I OMEGA  I  P   II JESA I   I   I ‡E  I
C  K0         II        I      I        I PA„. II      I B I C I PB  I
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 63 )   GO TO 63
         IF (TIP .EQ. 13 )   GO TO 13
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0012: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
13       CONTINUE
          LM = 0
C---> ˆ‡MEPˆTE‹œ
        RETURN
C----------------------------------------------------------------------
63     CONTINUE
       LM = 8
C######################################################################
C######################################################################
C---> SIN ˆCT. HAP.
         R = PAR0T(1)
         EM = PAR0T(2)
         OMEGA = PAR0T(3)
         FAZA = PAR0T(4) * 3.141592654 / 180.0
         IF ( R .EQ. 0.0 ) GO TO 60
C
61       RMSW(1) = 1.0 / R
         RMSW(2) = EM / R
         RMSW(3) = OMEGA
         RMSW(4) = FAZA
         RMSW(5) = 0.0
         RMSW(6) = 0.0
         RMSW(7) = 0.0
         RMSW(8) = 0.0
C
C
         RETURN
60       WRITE (NWTR,505) NEL
         R = 1.0E-32
         GO TO 61
C----------------------------------------------------------------------
505      FORMAT(' *** BH“TPEHHE COPOTˆB‹EHˆE ˆCTO—HˆKA HAPŸ†EHˆŸ ***'/
     =          ' *** ‹EMEHTA HOMEP ', I10,  ' PABHO H“‹ž         ***'/
     =          ' *** PˆHˆMAETCŸ PABH›M 1.0E-32  [OM]             ***')
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
