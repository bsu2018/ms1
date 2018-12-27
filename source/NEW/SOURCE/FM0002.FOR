         SUBROUTINE FM0002( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  FM0002( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹ˆ:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                   8        „ˆO„ (HE “P. BEHTˆ‹œ)    D
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹ˆ.
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ "ˆ„EA‹œH›‰ K‹ž—";
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
C   PˆME—AHˆE:    1. KOHTPO‹œ HE POˆ‡BO„ˆTCŸ;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C                  RMSW TˆA # 8 [ ˆ„EA‹œH›‰ „ˆO„ ]
C                  --------------------------------
C                                                             LM = 4
C                                                             ******
C
C                    SRAB
C             +------+--------+-----+-------+
C             I COCT.I BPEMŸ  I PE  I PE    I         KD = 0 BEHTˆ‹œ
C             I TEK. I EPEK‹.I ‡E  I  ‡E   I                ‡AKP›T;
C             I "KD" I "TX"  .I PB  I   PB  I
C             +------+--------+-----+-------+         KD = 1 BEHTˆ‹œ
C             I  1   I   2    I  3  I   4   I                OTKP›T;
C             +------+--------+-----+-------+
C             I  11  I   12   I 13  I  14   I         TX - BO‡MO†HOE
C             +------+--------+-----+-------+              BPEMŸ KOM-ˆˆ;
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 8 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0002: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> OH“‹EHˆE BCEX O‹E‰ RMSW „‹Ÿ „ˆO„A
         LM = 4
         DO 100 I=1,LM
100      RMSW(I) = 0.0
         RETURN
         END
