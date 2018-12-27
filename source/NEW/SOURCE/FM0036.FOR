         SUBROUTINE FM0036( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT €…‹Ÿ  1990 ƒ.)------------
C
C
C   O„POƒPAMMA:  FM0036( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹E‰ C‹E„“ž™ˆX TˆOB:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  66            .„.C             ESG
C                              ƒ€ŒŽˆ—…‘Š€Ÿ
C                              CˆH“COˆ„A‹œH€Ÿ
C
C
C                  67          ƒ€ŒŽˆ—…‘Šˆ‰       JSC
C                              CˆH“COˆ„A‹œH›‰
C                              ˆ‘’. ’ŽŠ€
C
C
C              ‹EMEHT KO„ˆP“ETCŸ:
C              -------------------
C
C               # T 66 “ K0 K1     R    L
C                                   MAS < NGARM1  E1  FI1
C                                         NGARM2  E2  FI2
C                                         .....   ..  ...  >  ;
C
C
C
C
C               # T 67 “ K0 K1   MAS < NGARM1  J1  FI1
C                                        NGARM2  J2  FI2
C                                        .....   ..  ...  >  ;
C
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
C                  RMSW TˆA # 66 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ ]
C                  -----------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II     I     I ŠŽ‹-‚Ž I    I    I
C   “‡‹›:     II  R  I  L  I ƒ€ŒŽ.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
C
C
C                  RMSW TˆA # 67 [ ˆCTO—HˆK SIN TOKA ]
C                  ------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II  …-I  …-I ŠŽ‹-‚Ž I    I    I
C   “‡‹›:     II ‡…‚I ‡…‚I ƒ€ŒŽ.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         EQUIVALENCE ( N     , RN         )
         EQUIVALENCE ( FGMSW , MASPRT(87) )
         INTEGER BA0T, TIP, FGMSW
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 66 .OR. TIP .EQ. 67 ) GO TO 6667
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0036: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
6667   CONTINUE
         IF( TIP .EQ. 66 ) KGARM   = IFIX (PAR0T(3))
         IF( TIP .EQ. 67 ) KGARM   = IFIX (PAR0T(1))
         IF( MOD(KGARM,3) .NE. 0 ) GO TO 9999
         KGARM   = KGARM / 3
C--> —’Ž •Ž’ˆŒ ?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 15 + 3*KGARM
       RETURN
C######################################################################
1000     CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
         LM = 5 + 3*KGARM
         DO 2 I=1,LM
2        RMSW(I) = 0.0
C----------------------------------------------------------------------
         IF( TIP .EQ. 67 ) GO TO 1100
         R = PAR0T(1)
         L = PAR0T(2)
         IF( R .EQ. 0.0  .AND. L .EQ. 0.0 ) GO TO 9000
1050     CONTINUE
         RMSW(1) = R
         RMSW(2) = L
         IPAR0   = 4
         GO TO 1200
1100     CONTINUE
         IPAR0   = 2
1200     CONTINUE
         N       = KGARM
         RMSW(3) = RN
         IMSW    = 6
         DO 1300 I=1,KGARM
         NG  = IFIX( PAR0T(IPAR0) )
         EG  = PAR0T(IPAR0+1)
         FIG = PAR0T(IPAR0+2)
         N = NG
         RMSW(IMSW) = RN
         RMSW(IMSW+1) = EG
         RMSW(IMSW+2) = FIG * 3.141592654 / 180.0
         IPAR0 = IPAR0 + 3
         IMSW  = IMSW  + 3
1300     CONTINUE
C
         RETURN
C---------------------------------------------------------------------
9000     WRITE (NWTR,505) NEL
         R = 1.0E-32
         GO TO 1050
C---------------------------------------------------------------------
505      FORMAT(' *** BH“TPEHHE COPOTˆB‹EHˆE ˆCTO—HˆKA HAPŸ†EHˆŸ ***'/
     =          ' *** ‹EMEHTA HOMEP ', I10,  ' PABHO H“‹ž         ***'/
     =          ' *** PˆHˆMAETCŸ PABH›M 1.0E-32  [OM]             ***')
C!!!-------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 555) TIP, NEL
555      FORMAT(' *** ‹Oƒ. CO‰ B FM0036: …€‚ˆ‹œŽ ‡€„€Ž ŠŽ‹-‚Ž
     =ƒ€ŒŽˆŠ ***'/' *** HOMEP TˆA ',I10,' ‹EMEHT HOMEP:',I10,'***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
         RETURN
C######################################################################
         END
