        SUBROUTINE FM0022(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   OKTŸPŸ 1987 ƒ.)-----------
C   O„POƒPAMMA: FM0022(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹E‰ ‹ˆHE‰H›X „ˆHAMˆ—ECKˆX ‡BEHœEB:
C   ----------
C    +-----+-----------------------------------+---------------------+
C    I     I                                   I    EPE„ATO—HAŸ     I
C    I Tˆ I       HAˆMEHOBAHˆE ‡BEHA          I                     I
C    I     I                                   I       ”“HK–ˆŸ       I
C    I-----I-----------------------------------I---------------------I
C    I     I   POOP–ˆOHA‹œHO-ˆHTEƒPˆP“ž™EE   I    TAU1*S + 1       I
C    I 314 I         ( ˆ - PEƒ“‹ŸTOP )        I   -------------     I
C    I     I                                   I      TAU2*S         I
C    I-----I-----------------------------------I---------------------I
C    I     I POOP–ˆOHA‹œHO-„ˆ””EPEH–ˆP“ž™EE  I    TAU1*S + 1       I
C    I 315 I         ( „ - PEƒ“‹ŸTOP )        I   -------------     I
C    I     I                                   I    TAU2*S + 1       I
C    I-----I-----------------------------------I---------------------I
C    I     I                                   I    1 - TAU*S        I
C    I 316 I       ”A‡OBPA™ATE‹œ               I   -------------     I
C    I     I                                   I    1 + TAU*S        I
C    I-----I-----------------------------------I---------------------I
C    I     I                                   I       W**2          I
C    I 317 I       KO‹‹EATE‹œHOE              I  -----------------  I
C    I     I                                   I  S**2+2*E*W*S+W**2  I
C    +-----+-----------------------------------+---------------------+
C              *******************************************************
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0026, LM0007 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C                              Tˆ 314:==> 7
C                      „‹ˆHA:  Tˆ 315:==> 7
C                              Tˆ 316:==> 7
C                              Tˆ 317:==>11
C
C----------------------------------------------------------------------
C              ‹EMEHT› OˆC›BAžTCŸ:
C
C       # T 314 “ 0   #‹  #CM  TAU1  TAU2  UMAX U0 T0
C       # T 315 “ 0   #‹  #CM  TAU1  TAU2  UMAX
C       # T 316 “ 0   #‹  #CM  TAU   UMAX
C       # T 317 “ 0   #‹  #CM   W    E     UMAX
C
C      ƒ„E
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          TAU,TAU1,
C              TAU2 - OCTOŸHH›E BPEMEHˆ;
C              W    - —ACTOTA KO‹‹EAHˆ‰;
C              E    - KO””ˆ–ˆEHT „EM”ˆPOBAHˆŸ;
C              UMAX - MAKCˆMA‹œH›‰ B›XO„HO‰ CˆƒHA‹.
C              U0, T0 - ‚›„€…’ U0 „Ž ‚…Œ…ˆ ’0   
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
C----------------------------------------------------------------------
C
C              ”OPMAT RMSW Tˆ # 314,315:
C              --------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0022    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  UMAX - MAKCˆM. B›XO„HO‰ CˆƒHA‹.
C            I              I      I
C    12      I     3        I  FM  I  #‹  - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM  - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ‹EMEHTA #‹;
C    15      I     6        I  FM  I  TAU1 - OCTOŸHHAŸ BPEMEHˆ;
C    16      I     7        I  FM  I  TAU2 - OCTOŸHHAŸ BPEMEHˆ;
C    17      I     8        I  FM  I  U0   - €—€‹œŽ… ‡€—…ˆ…;
C    18      I     9        I  FM  I  T0   - ‚…ŒŸ ‚Š‹ž—…ˆŸ   ;
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 18
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 9
C
C----------------------------------------------------------------------
C
C
C              ”OPMAT RMSW Tˆ # 316:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0022    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  UMAX - MAKC. B›XO„HO‰ CˆƒHA‹;
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
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 16
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 7
C
C----------------------------------------------------------------------
C
C
C                    ”OPMAT RMSW Tˆ # 317:
C                    ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0022    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  UMAX - MAKC. B›XO„HOE HAPŸ†EHˆE;
C            I              I      I
C    12      I     3        I  FM  I  #‹  - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM  - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ‹EMEHTA #‹;
C    15      I     6        I  FM  I   W   - —ACTOTA KO‹‹EAHˆ‰ (ƒ–);
C    16      I     7        I  FM  I   E   - KO””ˆ–ˆEHT „EM”ˆPOBAHˆŸ;
C    17      I     8        I  DM  I  TN   - BPEMŸ Pˆ T=TN
C    18      I     9        I  DM  I  TN-1 - BPEMŸ Pˆ T=TN-1
C    19      I    10        I  DM  I  YN   - B›XO„HO‰ CˆƒHA‹ Pˆ T=TN
C    20      I    11        I  DM  I  YN-1 - B›XO„HO‰ CˆƒHA‹ Pˆ T=TN-1
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 20
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 11
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 313
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0022 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100,100,200,300),NTIP
C----------------------------------------------------------------------
C---> POOP–ˆOHA‹œHO-ˆHTEƒPˆP“ž™EE    ‡BEHO ( Tˆ  # 314 )
C---> POOP–ˆOHA‹œHO-„ˆ””EPEH–ˆP“ž™EE ‡BEHO ( Tˆ  # 315 )
C----------------------------------------------------------------------
100   CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1001
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 18
       RETURN
C######################################################################
1001   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 9
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> ‡AOMHˆM „AHH›E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(7) = PAR0T(4)
       RMSW(2) = PAR0T(5)
       RMSW(8) = PAR0T(6)
       RMSW(9) = PAR0T(7)
C----------------------------------------------------------------------
C---> POBEPˆM, PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       IF( PAR0T(3) .GT. 0. .AND. PAR0T(4) .GT. 0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** O˜ˆKA B FM0022 -->HEPABˆ‹œHO ‡A„AH› OCTOŸHH›E BP
     =EMEHˆ ***'/' ***  ‹EMEHT ', I5, ' HOMEP TˆA ', I5,' ***'/
     =           ' *** B›—ˆC‹EHˆŸ PEKPA™AžTCŸ              ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
C######################################################################
C----------------------------------------------------------------------
C---> ”A‡OBPA™ATE‹œ ( Tˆ  # 316 )
C----------------------------------------------------------------------
200   CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 2001
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 16
       RETURN
C######################################################################
2001   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 7
       DO 3 I=1,LM
3      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> ‡AOMHˆM „AHH›E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(2) = PAR0T(4)
C----------------------------------------------------------------------
C---> POBEPˆM, PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       IF( PAR0T(3) .GT. 0. ) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
C----------------------------------------------------------------------
       RETURN
C######################################################################
C----------------------------------------------------------------------
C---> KO‹‹EATE‹œHOE   ‡BEHO ( Tˆ  # 317 )
C----------------------------------------------------------------------
300   CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 3001
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 20
       RETURN
C######################################################################
3001   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 11
       DO 4 I=1,LM
4      RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> ‡AOMHˆM „AHH›E  ------------------------------------------------
       NELBX = IFIX( PAR0T(1) )
       IF( NELBX .GT. NEL ) GO TO 9999
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(7) = PAR0T(4)
       RMSW(2) = PAR0T(5)
C----------------------------------------------------------------------
C---> POBEPˆM, PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       IF( PAR0T(3) .GT. 0. .AND.
     =     PAR0T(4) .GE. 0. .AND.  PAR0T(4) .LT. 1.) RETURN
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O˜ˆKA B FM0022 -->HEPABˆ‹œHO ‡A„AH› „AHH›E ***'/
     =         ' ***  ‹EMEHT ', I5, ' HOMEP TˆA ', I5,' ***'/
     =         ' *** B›—ˆC‹EHˆŸ PEKPA™AžTCŸ              ***')
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 504 ) NEL, TIP,NELBX
504    FORMAT(/' *** O˜ˆKA B FM0022 -->HEPABˆ‹œHO ‡A„AH› BXO„H›E „AHH›
     =E   ***'/' *** HA BXO„ ‹EMEHTA ', I5, ' HOMEP TˆA ', I5,' ***',
     =         ' *** O„AETCŸ TOK ‹EMEHTA, ˆMEž™EƒO O‹œ˜ˆ‰ OPŸ„KOB›‰
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
