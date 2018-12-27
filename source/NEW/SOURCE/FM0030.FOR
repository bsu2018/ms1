       SUBROUTINE FM0030(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT  MAPTA  1988 ƒ.)------------
C   O„POƒPAMMA: FM0030(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹E‰ ‹ˆHE‰H›X „ˆHAMˆ—ECKˆX ‡BEHœEB:
C   ----------
C              +-----+-----------------------------------------------+
C              I     I                                   I  EPE„A-  I
C              I Tˆ I       HAˆMEHOBAHˆE ‡BEHA          I  TO—HAŸ   I
C              I     I                                   I  ”“HK–ˆŸ  I
C              I-----I-----------------------------------I-----------I
C              I     I  POOP–ˆOHA‹œHO-C“MMˆP“ž™EE      I           I
C              I 318 I       (  - PEƒ“‹ŸTOP )           I     K     I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I  POOP–ˆOHA‹œH›‰ C“MMATOP        I           I
C              I 324 I       KBA„PATOB                   I           I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              *******************************************************
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0035)
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C                      „‹ˆHA:  Tˆ 318:==> +1(VAR)
C
C----------------------------------------------------------------------
C              ‹EMEHT OˆC›BAETCŸ:
C
C       # T 3XX “ 0 
C              MAS < NNAPR  “‡H1 “‡K1 K1  “‡H2 “‡K2 K2  ... >
C              MAS < NGARM  W  A0  A1  FI1  A2  FI2... >
C
C      ƒ„E
C          NNAPR  -   KO‹ˆ—ECTBO HAPŸ†EHˆ‰ CXEM› , KOTOP›E
C                     ˆCO‹œ‡“žTCŸ B KA—ECTBE BXO„H›X CˆƒHA‹OB;
C          “‡HI,“‡KI- HOMEPA “‡‹OB, HAPŸ†EHˆE ME†„“ KOTOP›Mˆ
C                     ˆCO‹œ‡“ETCŸ B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          KI       - KO””ˆ–ˆEHT POOP–ˆOHA‹œHOCTˆ I-TOƒO
C                     HAPŸ†EHˆŸ;
C          NGARM  -   KO‹ˆ—ECTBO ‹EMEHTOB ƒAPMOHˆ—ECKOƒO PŸ„A
C                     OˆC›BAž™EƒO ‡A„AHH›‰ CˆƒHA‹, O„ABAEM›‰
C                     HA BXO„ PEƒ“‹ŸTOPA (C “—ETOM OCT. COCTAB‹.);
C          W    -     —ACTOTA OCHOBHO‰ ƒAPMOHˆKˆ ( ƒ– );
C          A0,A1,...- AM‹ˆT“„› COOTBETB“ž™ˆX ƒAPMOHˆK CˆƒHA‹A;
C          FI1,FI2...-HA—. ”A‡› COOTBETB“ž™ˆX ƒAPMOHˆK CˆƒHA‹A;
C----------------------------------------------------------------------
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ
C   -----
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW Pˆ FGMSW = 0;
C                  LM - „‹ˆHA BCEƒO MSW OˆCAHˆŸ      Pˆ FGMSW = 1;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ B MˆHˆMA‹œHOM O"EME;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C
C              ”OPMAT RMSW Tˆ # 318, #324:
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0030    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NNAPR-KO‹ˆ—ECTBO HAP., ˆCO‹œ-
C            I              I      I        ‡“EM›X KAK BXO„H›E CˆƒHA‹›;
C    12      I     3        I  FM  I  “‡H1 - “‡E‹ HA—A‹A;
C    13      I     4        I  FM  I  “‡K1 - “‡E‹ KOH–A;
C    14      I     5        I  FM  I  K1   - KO””ˆ–ˆEHT POOP–ˆOHA‹œ-
C            I              I      I         HOCTˆ HAPŸ†EHˆŸ;
C  9+I*3     I     I*3      I  FM  I  “‡HI -      -"-
C 10+I*3     I   1+I*3      I  FM  I  “‡KI -      -"-
C 11+I*3     I   2+I*3      I  FM  I  KI   -      -"-
C .......    I   .......    I  ... I  ..........................
C12+NNAPR*3  I  3+NBXOD*3   I  FM  I  NGARM -KO‹ˆ—ECTBO ƒAPMOHˆK, ˆC.
C            I              I      I         „‹Ÿ OˆCAHˆŸ ‡A„. CˆƒHA‹A
C            I              I      I         (C “—ETOM OCT. COCTAB‹.);
C 13+NNAPR*3 I  4+NNAPR*3   I  FM  I  W  -   —ACTOTA OCH. ƒAPMOHˆKˆ(ƒ–);
C 14+NNAPR*3 I  5+NNAPR*3   I  FM  I  A0  -  AM‹ˆT“„A OCT. COCTAB‹Ÿ-
C            I              I      I         ž™E‰ ‡A„. CˆƒHA‹A;
C 15+NNAPR*3 I  6+NNAPR*3   I  FM  I  A1  -  AM‹ˆT“„A 1-O‰ ƒAPMOHˆKˆ
C            I              I      I         ‡A„AHHOƒO CˆƒHA‹A;
C 16+NNAPR*3 I  7+NNAPR*3   I  FM  I  FI1 -  HA—. ”A‡A 1-O‰ ƒAPMOHˆKˆ;
C 17+NNAPR*3 I  8+NNAPR*3   I  FM  I  A2  -  AM‹ˆT“„A 2-O‰ ƒAPMOHˆKˆ;
C 18+NNAPR*3 I  9+NNAPR*3   I  FM  I  FI2 -  HA—. ”A‡A 2-O‰ ƒAPMOHˆKˆ;
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 12 + 3*NNAPR + 2*NGARM
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 3 + 3*NNAPR + 2*NGARM
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( NMKNOT,MASPRT(7) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 317
       IF ( NTIP .EQ. 1 ) GO TO 1
       IF ( NTIP .EQ. 7 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0030 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100),NTIP
C----------------------------------------------------------------------
C---> POOP–ˆOHA‹œHO C“MMˆP“ž™EE ‡BEHO   ( Tˆ  # 318 )
C---> POOP–ˆOHA‹œH›‰ C“MMATOP KBA„PATOB ( Tˆ  # 324 )
C----------------------------------------------------------------------
100   CONTINUE
C--> OPE„E‹EHˆE KOHCTAHT
       NNAPR = 0
       IF( PAR0T(2) .GT. 0.0 ) NNAPR = IFIX ( PAR0T(2) )
       INGARM = 4 + 3*NNAPR
       NGARM  = 0
       IF( PAR0T(INGARM) .GT. 0.0 ) NGARM = IFIX ( PAR0T(INGARM) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 12 + 3*NNAPR + 2*NGARM
       RETURN
C######################################################################
1000   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 3 + 3*NNAPR + 2*NGARM
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C---> POBEPˆM  PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       NNAPR1 = (IFIX ( PAR0T(1) ) - 1 ) / 3
       IF ( NNAPR .EQ. NNAPR1 ) GO TO 3
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O˜ˆKA B FM0030 -->HEPABˆ‹œHO ‡A„AHO KO‹ˆ—ECTBO BX
     =O„OB***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
3      CONTINUE
       NGARM1 = (IFIX( PAR0T(INGARM-1) ) - 1) / 2
       IF ( NGARM .EQ. NGARM1 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 503 ) NEL, TIP
503    FORMAT(/' *** O˜ˆKA B FM0030 -->HEPABˆ‹œHO ‡A„AHO KO‹-BO ƒAPMOH
     =ˆK  ***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
4      CONTINUE
C----------------------------------------------------------------------
C---->  ‡AOMHˆM HOMEPA “‡‹OB.
       N = NNAPR
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
       IF( NNAPR .EQ. 0 ) GO TO 50000
          DO 5 I = 1 , NNAPR
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          NYZN  = IFIX ( PAR0T(IPAR0T) )
          IF( NYZN .LT. 0 .OR. NYZN .GT. NMKNOT ) GO TO 9999
          N = NYZN
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          NYZK = IFIX ( PAR0T(IPAR0T) )
          IF( NYZK .LT. 0 .OR. NYZK .GT. NMKNOT ) GO TO 9999
          N = NYZK
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) =  PAR0T(IPAR0T)
5      CONTINUE
50000  CONTINUE
C----------------------------------------------------------------------
C---->  ‡AOMHˆM AM‹ˆT“„› ˆ ”A‡› ƒAPMOHˆK ----------------------------
       IPAR0T = IPAR0T + 2
       IRMSW = IRMSW + 1
       N = NGARM
       RMSW(IRMSW) = RN
       IF( NGARM .EQ. 0 ) GO TO 60000
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       RMSW(IRMSW) = PAR0T(IPAR0T)
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       RMSW(IRMSW) = PAR0T(IPAR0T)
       I1 = NGARM-1
       IF( I1 .LT. 1 ) GO TO 7
          DO 6 I = 1 , I1
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T)
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T) * 3.1416 / 180.
6      CONTINUE
60000  CONTINUE
7      CONTINUE
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 504 ) NEL, TIP
504    FORMAT(/' *** O˜ˆKA B FM0030 -->HEPABˆ‹œHO ‡A„AH› BXO„H›E „AHH›
     =E   ***'/' *** HA BXO„ ‹EMEHTA ', I5, ' HOMEP TˆA ', I5,' ***'/
     =         ' *** O„AETCŸ HAPŸ†EHˆE C HEC“™ECTB“ž™ˆX “‡‹OB
     =    *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
