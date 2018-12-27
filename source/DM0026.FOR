          SUBROUTINE DM0026 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAŸ   1987 ƒ.)------------
C   O„POƒPAMMA:  DM0026 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C
C                  A) ZAPROS = 1
C                  -------------
C                    CPOC ”‹AƒA FQ - BE‹ˆ—ˆH› “P. APAM. OPE„E‹EH›;
C                  === Hˆ —EƒO HE PAC—ˆT›BAEM ==
C
C                  B) ZAPROS = 2
C                     ----------
C                      == B C‹“—AE H“‹EBOƒO ‡HA—EHˆŸ ”‹AƒA FQ -->
C                  B›—ˆC‹EHˆE
C
C
C              FQ - PAC—ET “P. HAPŸ†EHˆ‰ B›O‹HEH -->
C                  MEXAHˆ‡M TOƒO ”‹AƒA O‡BO‹ŸET CˆHXPOHˆ‡ˆPOBATœ
C                  B›O‹HEHˆŸ PA‡‹ˆ—H›X DM_MOHˆTOPOB C .TˆA # 4;
C
C                  C) ZAPROS = 3
C                     **********
C                     HE O„„EP†ˆBAETCŸ ( PE‡EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE O„„EP†ˆBAETCŸ;
C
C----------------------------------------------------------------------
C
C              OPAAT›BAET MAKPOMO„E‹ˆ ‹ˆHE‰H›X „ˆHAMˆ—ECKˆX ‡BEHœEB:
C
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
C                ******************************************************
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C
C----------------------------------------------------------------------
C              ‹EMEHT› OˆC›BAžTCŸ:
C
C       # T 314 “ 0   #‹  #CM  TAU1  TAU2  UMAX  U0  T0
C       # T 315 “ 0   #‹  #CM  TAU   TAU2  UMAX 
C       # T 316 “ 0   #‹  #CM  TAU   UMAX
C       # T 317 “ 0   #‹  #CM   W    E     UMAX
C
C      ƒ„E
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          TAU,TAU1,
C              TAU2 - OCTOŸHH›E BPEMEHˆ;
C              W    - —ACTOTA KO‹‹EAHˆ‰;
C              E    - KO””ˆ–ˆEHT „EM”ˆPOBAHˆŸ.
C          U0, T0   - €—€‹œŽ… ‡€—…ˆ… ˆˆŒ€…ŒŽ… „Ž ’=’0 
C
C----------------------------------------------------------------------
C
C   METO„:     OPE„E‹EHˆE B›XO„H›X CˆƒHA‹OB ‹ˆHE‰H›X „ˆHAMˆ—ECKˆX
C   -----      ‡BEHœEB B COOTBETCTBˆˆ C ˆX EPE„ATO—H›Mˆ ”“HK–ˆŸMˆ.
C
C   APAMETP›:     MSW, RMSW - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------      ( "OTPE‡OK" MACCˆB› MSWALL -> C.M. / GETMSW );
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          B T = TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          B T = TN + H0;
C                  TOKN  ˆ  TOKN1  -  ‡HA—EHˆŸ "TOKOB" Pˆ T :=
C                                     TN  ˆ  TN + H0;
C                                   PA‡MEPHOCTˆ CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 5;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C                  KOD - APAMETP OPATHO‰ CBŸ‡ˆ C MOHˆTOPOM DM....
C                   ( =1 Pˆ O‹œ˜O‰ ‹OK. OƒPE˜HOCTˆ );
C                 MSWALL( (RMSWAL ) - BECœ CˆCOK MSW_ALL,
C                 ADRMSW - A„PEC TEK. C‹OAB MSW ( RMSW );
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=   1 ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆOB # 314,315,316,317:
C                ----------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. IHA—.   I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
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
C    11      I     2        I  FM  I  UMAX - MAKC. B›X. CˆƒHA‹
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
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 16
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 7
C
C----------------------------------------------------------------------
C
C
C                      ”OPMAT RMSW Tˆ # 316:
C                      ----------------------
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
C    11      I     2        I  FM  I  UMAX - MAKC. B›X. CˆƒHA‹;
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
C    11      I     2        I  FM  I  UMAX - MAKC. B›X. CˆƒHA‹;
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
C----------------------------------------------------------------------
C     B MACCˆBE TOKN ˆ TOKN1 CO„EP†ˆTCŸ:
C     **********************************
C     TOKN/N1(1) - ‡HA—EHˆE B›XO„HOƒO CˆƒHA‹A;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION X(3), Y(3), A0(1), A1(1), A2(1)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR,INTMAS
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER, TIP, ZAPROS
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       NTIP = TIP - 313
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0026 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C--->  B›OP PE†ˆMA ‡APOCA..
6666   GO TO( 100, 200), ZAPROS
C----------------------------------------------------------------------
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C==> CPOC ”‹AƒA       OCHOBHO‰ PAC—ET B›O‹HEH;
       MSW(10) = 0
       RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-----ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  H“†HO ‹ˆ PAOTATœ ?
       IF ( MSW(10) .EQ. 1 ) RETURN
C----------------------------------------------------------------------
C-->  Pˆ„ETCŸ .............................
C-->  ZAPROS = 2 --> PAC—ˆTATœ ‡HA—EHˆŸ....
       GO TO (1000,2000,3000,4000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   POOP–ˆOHA‹œHO-ˆHTEƒPˆP“ž™EE ( Tˆ 314 )
C----------------------------------------------------------------------
       ITOK = MSW(7)
       IF( RMSW(18) .LT. TN ) GO TO 10001
       TOKN1(ITOK) = RMSW(17)
       GO TO 10002 
10001  TAU1 = RMSW(15)
       TAU2 = RMSW(16)
       NTOK = MSW(14)
       TOKBX = TOKN(NTOK)
       TOKBX1= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       TOKBIX = TOKN(ITOK) + TOKBX1 * (TAU1 + H0) / TAU2 -
     -               TOKBX * TAU1 / TAU2
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
10002  CONTINUE
       MSW(10) = 1
       RETURN
C######################################################################
2000   CONTINUE
C----------------------------------------------------------------------
C-->   POOP–ˆOHA‹œHO-„ˆ””EPEH–ˆP“ž™EE ‡BEHO  ( Tˆ 315 )
C----------------------------------------------------------------------
       TAU1 = RMSW(15)
       TAU2 = RMSW(16)
       NTOK =  MSW(14)
       TOKBX = TOKN(NTOK)
       TOKBX1= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       EXPHT2 = EXP( - H0 / TAU2 )
       TOKBIX = TOKN(ITOK) * EXPHT2 + EXPHT2 * ( TOKBX1 - TOKBX )*
     *               TAU1 / TAU2 + ( 1.- EXPHT2 ) * TOKBX1
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
3000   CONTINUE
C----------------------------------------------------------------------
C-->   ”A‡OBPA™ATE‹œ ( Tˆ 316 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK =  MSW(14)
       TOKBX = TOKN(NTOK)
       TOKBX1= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       EXPHT = EXP( - H0 / TAU )
       TOKBIX = EXPHT * TOKN(ITOK) + ( 1.- 2.* EXPHT ) * TOKBX1 +
     +               EXPHT * TOKBX
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
4000   CONTINUE
C----------------------------------------------------------------------
C-->   KO‹‹EATE‹œHOE ‡BEHO ( Tˆ 317 )
C----------------------------------------------------------------------
       W = RMSW(15)
       E = RMSW(16)
       NTOK =  MSW(14)
       ITOK =  MSW(7)
       TOKBX = TOKN1(NTOK)
C----------------------------------------------------------------------
C-->  POBO„ˆM KBA„PATˆ—H“ž ˆHTEPO‹Ÿ–ˆž „‹Ÿ OPE„E‹EHˆŸ ‡HA—EHˆŸ
C-->  B›XO„HOƒO CˆƒHA‹A Pˆ  T = TN - H0
C----------------------------------------------------------------------
       TOKBIX = TOKN(ITOK)
       X(1) = RMSW(18)
       X(2) = RMSW(17)
       X(3) = TN
       Y(1) = RMSW(20)
       Y(2) = RMSW(19)
       Y(3) = TOKN(ITOK)
       IF( TN .LE. 1.5*H0 ) GO TO 4050
       XN = TN - H0
       CALL APPROX( 3, 1, X, Y, A2, A1, A0 )
       TOKBIX = A2(1) * XN**2 + A1(1) * XN + A0(1)
4050   CONTINUE
C--> ‡Aˆ˜EM B TOKN1
       B = 1.
       IF( E .NE. 0.) B = EXP(-2.*E*W*H0)
       A = 2.* COS(W*H0)
       IF( E .NE. 0.) A = 2.* EXP(-E*W*H0) * COS( W*H0* SQRT(1-E**2) )
       TOKBIX = A * TOKN(ITOK) - B * TOKBIX + (1.- A + B) * TOKBX
       IF( TOKBIX .GT. RMSW(11) ) TOKBIX = RMSW(11)
       IF( TOKBIX .LT.-RMSW(11) ) TOKBIX =-RMSW(11)
       TOKN1(ITOK) = TOKBIX
C----------------------------------------------------------------------
C--->  ‡AOMHˆM B  MSW  HOBOE ‡HA—EHˆE
C----------------------------------------------------------------------
       IF( FITER. EQ. 1 ) GO TO 4100
       RMSW(18) = X(2)
       RMSW(17) = X(3)
       RMSW(20) = Y(2)
       RMSW(19) = Y(3)
4100   CONTINUE
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
       END
