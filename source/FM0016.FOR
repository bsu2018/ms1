       SUBROUTINE FM0016(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT  „EKAPŸ  1987 ƒ.)-----------
C   O„POƒPAMMA: FM0016(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹E‰ –ˆ”POB›X ‹Oƒˆ—ECKˆX ‹EMEHTOB:
C   ----------
C               +-----+--------------------------------------+
C               I     I                                      I
C               I Tˆ I    HAˆMEHOBAHˆE ‹EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 333 I –ˆ”POBO‰  RS-TPˆƒƒEP                 I
C               +-----+--------------------------------------+
C               I 334 I –ˆ”POBO‰   D-TPˆƒƒEP                 I
C               +-----+--------------------------------------+
C               I 335 I –ˆ”POBO‰   T-TPˆƒƒEP                 I
C               +-----+--------------------------------------+
C               I 360 I KOMAPATOP(1)                        I
C               +-----+--------------------------------------+
C               I 361 I KOMAPATOP(2)                        I
C               +-----+--------------------------------------+
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0020, LM0008 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C
C                      „‹ˆHA:  „‹Ÿ BCEX TˆOB :==>+1<VAR>
C----------------------------------------------------------------------
C              ‹EMEHT› OˆC›BAžTCŸ:
C
C          # T 33X “ 0    #‹1 #CM1  #‹2 #CM2 ;
C
C         ƒ„E
C          #‹I,#CMI- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C         -------------------------------------------------------------
C
C          # T 360 “ 0    #‹1 #CM1  #‹2 #CM2 ;
C
C         ƒ„E
C          #‹I,#CMI- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C         -------------------------------------------------------------
C
C          # T 361 “ 0    “1  “2  “3  “4 ;
C
C         ƒ„E
C            “I     - HOMEPA “‡‹OB, HAPŸ†EHˆŸ ME†„“ KOTOP›Mˆ
C                     ˆCO‹œ‡“žTCŸ B KA—ECTBE BXO„H›X CˆƒHA‹OB;
C         -------------------------------------------------------------
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
C              ”OPMAT RMSW Tˆ # 33X ˆ 360:
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I        PE‡EPB
C            I              I      I
C    11      I     2        I  DM  I        PE‡EPB
C            I              I      I
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C    15      I     6        I  FM  I  #‹2 -      -"-
C    16      I     7        I  FM  I  #CM2 -      -"-
C    17      I     8        I  LM  I  ATOKM2 -    -"-
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 17
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 8
C----------------------------------------------------------------------
C
C
C              ”OPMAT RMSW Tˆ # 335:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I        PE‡EPB
C            I              I      I
C    11      I     2        I  DM  I        PE‡EPB
C            I              I      I
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 14
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 5
C----------------------------------------------------------------------
C
C
C                   ”OPMAT RMSW Tˆ # 361:
C                   ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I        PE‡EPB
C            I              I      I
C    11      I     2        I  DM  I        PE‡EPB
C            I              I      I
C    12      I     3        I  FM  I  “1   - HOMEP “‡‹A;
C    13      I     4        I  FM  I  “2   - HOMEP “‡‹A;
C    14      I     5        I  FM  I  “3   - HOMEP “‡‹A;
C    15      I     6        I  FM  I  “4   - HOMEP “‡‹A;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 15
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 6
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 332
       GO TO ( 1 , 1 , 2  ), NTIP
       IF (  TIP .EQ. 360 ) GO TO 3
       IF (  TIP .EQ. 361 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0016 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->           –ˆ”POBO‰ RS-TPˆƒƒEP
C --->           –ˆ”POBO‰  D-TPˆƒƒEP
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 8
       DO 10 I=1,LM
10     RMSW(I) = 0.0
C---> BBE„EM HOMEPA BXO„H›X TOKOB.
       NEL1 = IFIX ( PAR0T(1) )
       NEL2 = IFIX ( PAR0T(3) )
       NSM1 = IFIX ( PAR0T(2) )
       NSM2 = IFIX ( PAR0T(4) )
C---> ‡AOMHˆM HOMEPA BXO„H›X TOKOB.
       N = NEL1
       IF( N .GE. NEL ) GO TO 9999
       RMSW(3) = RN
       N = NEL2
       IF( N .GE. NEL ) GO TO 9999
       RMSW(6) = RN
       N = NSM1
       RMSW(4) = RN
       N = NSM2
       RMSW(7) = RN
       RETURN
C######################################################################
2      CONTINUE
C----------------------------------------------------------------------
C --->           –ˆ”POBO‰  T-TPˆƒƒEP
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 5
       DO 20 I=1,LM
20     RMSW(I) = 0.0
C---> BBE„EM HOMEPA BXO„H›X TOKOB.
       NEL1 = IFIX ( PAR0T(1) )
       NSM1 = IFIX ( PAR0T(2) )
C---> ‡AOMHˆM HOMEPA BXO„H›X TOKOB.
       N = NEL1
       IF( N .GE. NEL ) GO TO 9999
       RMSW(3) = RN
       N = NSM1
       RMSW(4) = RN
       RETURN
C######################################################################
3      CONTINUE
C----------------------------------------------------------------------
C --->           KOMAPATOP(1)
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 8
       DO 30 I=1,LM
30     RMSW(I) = 0.0
C---> BBE„EM HOMEPA BXO„H›X TOKOB.
       NEL1 = IFIX ( PAR0T(1) )
       NEL2 = IFIX ( PAR0T(3) )
       NSM1 = IFIX ( PAR0T(2) )
       NSM2 = IFIX ( PAR0T(4) )
C---> ‡AOMHˆM HOMEPA BXO„H›X TOKOB.
       N = NEL1
       RMSW(3) = RN
       N = NSM1
       RMSW(4) = RN
       N = NEL2
       RMSW(6) = RN
       N = NSM2
       RMSW(7) = RN
       RETURN
C######################################################################
4      CONTINUE
C----------------------------------------------------------------------
C --->           KOMAPATOP(2)
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 6
       DO 40 I=1,LM
40     RMSW(I) = 0.0
C---> BBE„EM HOMEPA BXO„H›X “‡‹OB.
       NY1 = IFIX ( PAR0T(1) )
       NY2 = IFIX ( PAR0T(2) )
       NY3 = IFIX ( PAR0T(3) )
       NY4 = IFIX ( PAR0T(4) )
C---> ‡AOMHˆM HOMEPA BXO„H›X “‡‹OB.
       N = NY1
       RMSW(3) = RN
       N = NY2
       RMSW(4) = RN
       N = NY3
       RMSW(5) = RN
       N = NY4
       RMSW(6) = RN
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) NEL, TIP,N
555    FORMAT(/' *** O˜ˆKA B FM0016 -->HEPABˆ‹œHO ‡A„AH› BXO„H›E „AHH›
     =E   ***'/' *** HA BXO„ ‹EMEHTA ', I5, ' HOMEP TˆA ', I5,' ***',
     =         ' *** O„AETCŸ TOK ‹EMEHTA, ˆMEž™EƒO O‹œ˜ˆ‰ OPŸ„KOB›‰
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
       END
