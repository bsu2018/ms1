         SUBROUTINE DM0035 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT  APE‹Ÿ 1988 ƒ.)------------
C   O„POƒPAMMA:  DM0035 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C  +-----+-------------------------------------------+----------------+
C  I     I                               I  EPE„A-  I   PAC—ETHAŸ    I
C  I Tˆ I       HAˆMEHOBAHˆE ‡BEHA      I  TO—HAŸ   I    ”OPM“‹A     I
C  I     I                               I  ”“HK–ˆŸ  I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I  POOP–ˆOHA‹œHO-C“MMˆP“ž™EE  I           I                I
C  I 318 I       (  - PEƒ“‹ŸTOP )       I     K     I YN1=SUM(K*UN1) I
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I  POOP–ˆOHA‹œH›‰ C“MMATOP    I           I                I
C  I 324 I       KBA„PATOB               I           IYN1=SUM(K*UN1**2)
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C                ******************************************************
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C
C----------------------------------------------------------------------
C              ‹EMEHT› OˆC›BAžTCŸ:
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
C          W    -     —ACTOTA OCHOBHO‰ ƒAPMOHˆKˆ ( 1/C );
C          A0,A1,...- AM‹ˆT“„› COOTBETB“ž™ˆX ƒAPMOHˆK CˆƒHA‹A;
C          FI1,FI2...-HA—. ”A‡› COOTBETB“ž™ˆX ƒAPMOHˆK CˆƒHA‹A;
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
C               "OCTOŸHHAŸ" —ACTœ MSW TˆOB # 318, # 324:
C                -----------------------------------------
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
C              ”OPMAT RMSW Tˆ # 318, # 324:
C              -----------------------------
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
C 13+NNAPR*3 I  4+NNAPR*3   I  FM  I  W  -   —ACTOTA OCH. ƒAPMOHˆKˆ;
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
C----------------------------------------------------------------------
C
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
         DIMENSION INTMAS(1), STMAS(100), DGMAS(12)
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
       NTIP = TIP - 317
       IF ( NTIP .EQ. 1 ) GO TO 9900
       IF ( NTIP .EQ. 7 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0035 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
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
       GO TO (1000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   POOP–ˆOHA‹œHO C“MMˆP“ž™EE ‡BEHO ( Tˆ 318 )
C----------------------------------------------------------------------
       NNAPR = MSW(11)
       NGARM = MSW(12 + 3*NNAPR)
       SIGNAL = 0.
       IF( NGARM .LT. 1 ) GO TO 1110
C----------------------------------------------------------------------
C--> PˆABˆM ‡HA—EHˆE ‡A„AHHOƒO BXO„HOƒO CˆƒHA‹A
       W = RMSW(13 + 3*NNAPR)
       A0 = RMSW(14 + 3*NNAPR)
       SIGNAL = A0
       I1 = 2*(NGARM - 1)
       IF( I1 .EQ. 0 ) GO TO 1110
          DO 1100 I=1,I1,2
          NRMSW = 14 + I + 3*NNAPR
          AI = RMSW(NRMSW)
          FII= RMSW(NRMSW+1)
          N = I / 2 + 1
          SIGNAL = SIGNAL + AI * SIN(N*W*(TN+H0) + FII)
1100   CONTINUE
1110   CONTINUE
       IF( NTIP .EQ. 7 ) SIGNAL = SIGNAL**2
C----------------------------------------------------------------------
C--> POC“MMˆP“EM BXO„H›E CˆƒHA‹›
       IF(NNAPR .EQ. 0 ) GO TO 1210
          DO 1200 I=1,NNAPR
          NMSW = 11 + I*3
          AKI  = RMSW(NMSW)
          NYZN = MSW(NMSW-2)
          NYZK = MSW(NMSW-1)
          VN = 0.0
          IF( NYZN .NE. 0 ) VN = VALLN1(NYZN)
          VK = 0.0
          IF( NYZK .NE. 0 ) VK = VALLN1(NYZK)
          IF( NTIP .EQ. 1 ) SIGNAL = SIGNAL + AKI*(VN - VK)
          IF( NTIP .EQ. 7 ) SIGNAL = SIGNAL + AKI*(VN - VK)**2
1200   CONTINUE
1210   CONTINUE
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       ITOK = MSW(7)
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
       END
