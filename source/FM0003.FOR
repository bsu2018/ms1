         SUBROUTINE FM0003( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  FM0003( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹ˆ:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  80       "„ˆO„ - RL"                D_RL
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹ˆ.
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ "RL _ „ˆO„";
C   -----
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HA ‡HA—EHˆŸ L(0) ˆ L(@) HE POˆ‡BO„ˆTCŸ;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C                  RMSW TˆA # 80 [ RL _ „ˆO„ ]
C                  --------------------------------
C                                                             LM = 10
C                                                             *******
C
C                    SPM                      SRAB
C          ++---+---+---+--++----+---+-------+------+--------+----+---+
C          II R I L I R I L II G I J I PE‡E  I COCT.I BPEMŸ  I PE‡EPB I
C          II 0 I 0 I @ I @ II R I R I  PB   I TEK. I EPEK‹.I        I
C          II   I   I   I   II L I L I       I "KD" I "TX"[*]I        I
C          ++---+---+---+---++---+---+-------+------+--------+--------+
C          II 1 I 2 I 3 I 4 II 5 I 6 I   7   I  8   I   9    I   10   I
C          ++---+---+---+---++---+---+-------+------+--------+--------+
C          II 11I 12I 13I 14II 15I 16I   17  I  18  I   19   I   20   I
C          ++---+---+---+---++---+---+-------+------+--------+----+---+
C
C  0 - „ˆO„ OTKP›T(KD=1) ; @ - „ˆO„ ‡AKP›T (KD=0) ;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 80 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0003: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> OH“‹EHˆE BCEX O‹E‰ RMSW „‹Ÿ „ˆO„A
         LM = 10
         DO 100 I=1,LM
100      RMSW(I) = 0.0
C===> PˆHŸTœ ˆCXO„. APAMETP›:
         RMSW(1) = PAR0T(1)
         RMSW(2) = PAR0T(2)
         RMSW(3) = PAR0T(3)
         RMSW(4) = PAR0T(4)
C---->
         IF ( RMSW(1) .EQ. 0.0 ) WRITE(NWTR, 2) NEL
         IF ( RMSW(1) .EQ. 0.0 ) RMSW(1) = 1.0E-32
2        FORMAT(' *** COPOTˆB‹EHˆE "OTKP›TOƒO" ‹EMEHTA # ', I4,
     = ' PABHO 0.0 ***'/' *** PˆHˆMAETCŸ PABH›M 1.0E-32 [OM] ***')
C---->
         IF ( RMSW(3) .EQ. 0.0 ) WRITE(NWTR, 3) NEL
         IF ( RMSW(3) .EQ. 0.0 ) RMSW(3) = 1.0E+32
3        FORMAT(' *** COPOTˆB‹EHˆE "‡AKP›TOƒO" ‹EMEHTA # ', I4,
     = ' PABHO 0.0 ***'/' *** PˆHˆMAETCŸ PABH›M 1.0E+32 [OM] ***')
         RETURN
         END
