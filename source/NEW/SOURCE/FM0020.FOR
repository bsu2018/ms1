       SUBROUTINE FM0020(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAŸ   1987 ƒ.)------------
C   O„POƒPAMMA: FM0020(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹E‰ ‹ˆHE‰H›X „ˆHAMˆ—ECKˆX ‡BEHœEB:
C   ----------
C              +-----+-----------------------------------------------+
C              I     I                                   I  EPE„A-  I
C              I Tˆ I       HAˆMEHOBAHˆE ‡BEHA          I  TO—HAŸ   I
C              I     I                                   I  ”“HK–ˆŸ  I
C              I-----I-----------------------------------I-----------I
C              I     I  POOP–ˆOHA‹œHO-C“MMˆP“ž™EE      I           I
C              I 310 I       (  - PEƒ“‹ŸTOP )           I     K     I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I         ˆHTEƒPˆP“ž™EE             I           I
C              I 311 I       ( ˆ - PEƒ“‹ŸTOP )           I   1/(TS)  I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I                                   I           I
C              I 312 I         ˆHEP–ˆOHHOE               I 1/(TS+1)  I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I         „ˆ””EPEH–ˆP“ž™EE          I           I
C              I 313 I       ( „ - PEƒ“‹ŸTOP )           I TS/(TS+1) I
C              I     I                                   I           I
C              I-----I-----------------------------------I-----------I
C              I     I                                   I I*        I
C              I 319 I  ƒEHEPATOP ˆ‹OOPA‡HOƒO CˆƒHA‹A  I I  *      I
C              I     I                                   I I    *    I
C              I-----I-----------------------------------I-----------I
C              *******************************************************
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0024, LM0007 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C                              Tˆ 310:==> +1(VAR)
C                      „‹ˆHA:  Tˆ 311:==>  7
C                              Tˆ 312:==>  7
C                              Tˆ 313:==>  7
C                              Tˆ 319:==>  7
C
C----------------------------------------------------------------------
C              ‹EMEHT OˆC›BAETCŸ:
C
C       # T 310 “ 0 
C              MAS < NBXOD  #‹1 #CM1 K1  #‹2 #CM2 K2  ... >
C              MAS < NGARM  W  A0  A1  FI1  A2  FI2... >
C
C      ƒ„E
C          NBXOD  -   KO‹ˆ—ECTBO TOKOB ‹EMEHTOB CXEM› , KOTOP›E
C                     ˆCO‹œ‡“žTCŸ B KA—ECTBE BXO„H›X CˆƒHA‹OB;
C          #‹I,#CMI- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          KI       - KO””ˆ–ˆEHT POOP–ˆOHA‹œHOCTˆ TOKA I-TOƒO
C                     ‹EMEHTA;
C          NGARM  -   KO‹ˆ—ECTBO ‹EMEHTOB ƒAPMOHˆ—ECKOƒO PŸ„A
C                     OˆC›BAž™EƒO ‡A„AHH›‰ CˆƒHA‹, O„ABAEM›‰
C                     HA BXO„ PEƒ“‹ŸTOPA (C “—ETOM OCT. COCTAB‹.);
C          W    -     —ACTOTA OCHOBHO‰ ƒAPMOHˆKˆ ( 1/C );
C          A0,A1,...- AM‹ˆT“„› COOTBETB“ž™ˆX ƒAPMOHˆK CˆƒHA‹A;
C          FI1,FI2...-HA—. ”A‡› COOTBETB“ž™ˆX ƒAPMOHˆK CˆƒHA‹A;
C      ----------------------------------------------------------------
C
C       # T 311 “ 0   #‹  #CM  TAU
C       # T 312 “ 0   #‹  #CM  TAU
C       # T 313 “ 0   #‹  #CM  TAU
C
C      ƒ„E
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          TAU    -   OCTOŸHHAŸ BPEMEHˆ;
C----------------------------------------------------------------------
C       # T 319 “ 0  UMAX UMIN TZ TP TI
C
C      ƒ„E
C          UMAX  -MAKCˆMA‹œHOE HAPŸ†EHˆE  CˆƒHA‹A
C          UMIN  -MˆHˆMA‹œHOE  HAPŸ†EHˆE  CˆƒHA‹A
C          TZ    - HA—A‹œH›‰   C„Bˆƒ O ”A‡E
C          TP    - EPˆO„  CˆƒHA‹A ˆ‹OOPA‡HOƒO
C          TI    - „‹ˆTE‹œHOCTœ CˆƒHA‹A
C
C
C----------------------------------------------------------------------
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
C              ”OPMAT RMSW Tˆ # 310:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NBXOD-KO‹ˆ—ECTBO TOKOB, ˆCO‹œ-
C            I              I      I        ‡“EM›X KAK BXO„H›E CˆƒHA‹›;
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  FM  I  K1   - KO””ˆ–ˆEHT POOP–ˆOHA‹œ-
C            I              I      I         HOCTˆ TOKA #CM1 ‹-TA #‹1;
C    15      I     6        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C  8+I*4     I  -1+I*4      I  FM  I  #‹I -      -"-
C  9+I*4     I     I*4      I  FM  I  #CMI -      -"-
C 10+I*4     I   1+I*4      I  FM  I  KI   -      -"-
C 11+I*4     I   2+I*4      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NBXOD*4  I  3+NBXOD*4   I  FM  I  NGARM -KO‹ˆ—ECTBO ƒAPMOHˆK, ˆC.
C            I              I      I         „‹Ÿ OˆCAHˆŸ ‡A„. CˆƒHA‹A
C            I              I      I         (C “—ETOM OCT. COCTAB‹.);
C 13+NBXOD*4 I  4+NBXOD*4   I  FM  I  W  -   —ACTOTA OCH. ƒAPMOHˆKˆ(1/C)
C 14+NBXOD*4 I  5+NBXOD*4   I  FM  I  A0  -  AM‹ˆT“„A OCT. COCTAB‹Ÿ-
C            I              I      I         ž™E‰ ‡A„. CˆƒHA‹A;
C 15+NBXOD*4 I  6+NBXOD*4   I  FM  I  A1  -  AM‹ˆT“„A 1-O‰ ƒAPMOHˆKˆ
C            I              I      I         ‡A„AHHOƒO CˆƒHA‹A;
C 16+NBXOD*4 I  7+NBXOD*4   I  FM  I  FI1 -  HA—. ”A‡A 1-O‰ ƒAPMOHˆKˆ;
C 17+NBXOD*4 I  8+NBXOD*4   I  FM  I  A2  -  AM‹ˆT“„A 2-O‰ ƒAPMOHˆKˆ;
C 18+NBXOD*4 I  9+NBXOD*4   I  FM  I  FI2 -  HA—. ”A‡A 2-O‰ ƒAPMOHˆKˆ;
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 12 + 4*NBXOD + 2*NGARM
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 3 + 4*NBXOD + 2*NGARM
C
C----------------------------------------------------------------------
C
C              ”OPMAT RMSW Tˆ # 311,312,313:
C              ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I         PE‡EPB
C            I              I      I
C    12      I     3        I  FM  I  #‹  - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM  - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ‹EMEHTA #‹;
C    15      I     6        I  FM  I  TAU  - OCTOŸHHAŸ BPEMEHˆ;
C    16      I     7        I  FM  I          PE‡EPB
C----------------------------------------------------------------------
C
C              ”OPMAT RMSW Tˆ # 319
C              ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I         PE‡EPB
C            I              I      I
C    12      I     3        I  FM  I  UMAX - MAK. HAPŸ†EHˆE
C    13      I     4        I  FM  I  UMIN - MˆH. HAPŸ†EHˆE
C    14      I     5        I  LM  I  TZ   - C„Bˆƒ O ”A‡E
C    15      I     6        I  FM  I  TP   - EPˆO„ CˆƒHA‹A
C    16      I     7        I  FM  I  TI   - „‹ˆTE‹œHOCTœ CˆƒHA‹A
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 16
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 7
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 309
       IF ( NTIP .EQ. 10 ) GO TO 300
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0020 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100,200,200,200),NTIP
C----------------------------------------------------------------------
C---> POOP–ˆOHA‹œHO C“MMˆP“ž™EE ‡BEHO ( Tˆ  # 310 )
C----------------------------------------------------------------------
100   CONTINUE
C--> OPE„E‹EHˆE KOHCTAHT
       NBXOD = 0
       IF( PAR0T(2) .GT. 0.0 ) NBXOD = IFIX ( PAR0T(2) )
       INGARM = 4 + 3*NBXOD
       NGARM  = 0
       IF( PAR0T(INGARM) .GT. 0.0 ) NGARM = IFIX ( PAR0T(INGARM) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 12 + 4*NBXOD + 2*NGARM
       RETURN
C######################################################################
1000   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 3 + 4*NBXOD + 2*NGARM
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C---> POBEPˆM  PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       NBXOD1 = (IFIX ( PAR0T(1) ) - 1 ) / 3
       IF ( NBXOD .EQ. NBXOD1 ) GO TO 3
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O˜ˆKA B FM0020 -->HEPABˆ‹œHO ‡A„AHO KO‹ˆ—ECTBO BX
     =O„OB***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
3      CONTINUE
       NGARM1 = (IFIX( PAR0T(INGARM-1) ) - 1) / 2
       IF ( NGARM .EQ. NGARM1 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 503 ) NEL, TIP
503    FORMAT(/' *** O˜ˆKA B FM0020 -->HEPABˆ‹œHO ‡A„AHO KO‹-BO ƒAPMOH
     =ˆK  ***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
4      CONTINUE
C----------------------------------------------------------------------
C---->  ‡AOMHˆM HOMEPA TOKOB, ŸB‹Ÿž™ˆXCŸ BXO„H›Mˆ CˆƒHA‹AMˆ  ---------
       N = NBXOD
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
       IF( NBXOD .EQ. 0 ) GO TO 50000
          DO 5 I = 1 , NBXOD
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          NELBX = IFIX ( PAR0T(IPAR0T) )
          IF( NELBX .GT. NEL ) GO TO 9999
          N = NELBX
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) =  PAR0T(IPAR0T)
          IRMSW = IRMSW + 1
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
       IF( I1 .LT. 1 ) GO TO 60000
          DO 6 I = 1 , I1
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T)
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T) * 3.1416 / 180.
6      CONTINUE
60000  CONTINUE
       RETURN
C======================================================================
C----------------------------------------------------------------------
C---> ˆHTEƒPˆP“ž™EE     ‡BEHO ( Tˆ  # 311 )
C---> ˆHEP–ˆOHHOE       ‡BEHO ( Tˆ  # 312 )
C---> „ˆ””EPEH–ˆP“ž™EE  ‡BEHO ( Tˆ  # 313 )
C----------------------------------------------------------------------
200    CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1001
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 16
       RETURN
C######################################################################
1001   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 7
       DO 7 I=1,LM
7      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> ‡AOMHˆM „AHH›E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
C----------------------------------------------------------------------
C---> POBEPˆM, PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       IF( PAR0T(3) .GT. 0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** O˜ˆKA B FM0020 -->HEPABˆ‹œHO ‡A„AHA OCTOŸHHAŸ BP
     =EMEHˆ ***'/' ***  ‹EMEHT ', I5, ' HOMEP TˆA ', I5,' ***'/
     =           ' *** B›—ˆC‹EHˆŸ PEKPA™AžTCŸ              ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
C######################################################################
C----------------------------------------------------------------------
C  ƒEHEPATOP ˆ‹OOPA‡HOƒO CˆƒHA‹A   ( Tˆ # 319 )
C----------------------------------------------------------------------
300    CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 2001
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 16
       RETURN
C######################################################################
2001   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 7
       DO 9 I=1,LM
9      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> ‡AOMHˆM „AHH›E  ------------------------------------------------
       DO 10 I = 1,5
       RMSW(2+I) = PAR0T(I)
 10    CONTINUE
C----------------------------------------------------------------------
C---> POBEPˆM, PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       IF( PAR0T(4) .GT. 0..AND.PAR0T(5).GT.0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 507 ) NEL, TIP
507    FORMAT(/' *** O˜ˆKA B FM0020 -->HEPABˆ‹œHO ‡A„AHA OCTOŸHHAŸ BP
     =EMEHˆ ***'/' ***  ‹EMEHT ', I5, ' HOMEP TˆA ', I5,' ***'/
     =           ' *** B›—ˆC‹EHˆŸ PEKPA™AžTCŸ              ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 504 ) NEL, TIP,NELBX
504    FORMAT(/' *** O˜ˆKA B FM0020 -->HEPABˆ‹œHO ‡A„AH› BXO„H›E „AHH›
     =E   ***'/' *** HA BXO„ ‹EMEHTA ', I5, ' HOMEP TˆA ', I5,' ***',
     =         ' *** O„AETCŸ TOK ‹EMEHTA, ˆMEž™EƒO O‹œ˜ˆ‰ OPŸ„KOB›‰
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
