         SUBROUTINE DM0024 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (19 ä¥¢à «ì 1992ƒ.)------------
C   O„POƒPAMMA:  DM0024 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C  I 310 I       (  - PEƒ“‹ŸTOP )       I     K     I YN1=SUM(K*XN1) I
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I         ˆHTEƒPˆP“ž™EE         I           I YN1 = YN +     I
C  I 311 I       ( ˆ - PEƒ“‹ŸTOP )       I   1/(TS)  I H0 * XN1 / TAU I
C  I     I                               I           I                I
C  I-----I-------------------------------I-----------I----------------I
C  I     I                               I           I YN1=YN*EXP(-H0/I
C  I 312 I         ˆHEP–ˆOHHOE           I 1/(TS+1)  ITAU)+(1-EXP(-H0/I
C  I     I                               I           ITAU))*XN1       I
C  I-----I-------------------------------I-----------I----------------I
C  I     I    ƒEHEPATOP ˆ‹O-OPA‡HOƒO   I I*        I I = UMAX -     I
C  I 319 I       –ˆƒHA‹A                 I I  *      I (UMAX-UMIN)*T/ I
C  I     I                               I I    *    I  TI            I
C  I-----I-------------------------------I-----------I----------------I
C  I     I         „ˆ””EPEH–ˆP“ž™EE      I           I YN1=YN*EXP(-H0/I
C  I 313 I       ( „ - PEƒ“‹ŸTOP )       I TS/(TS+1) ITAU) + EXP(-H0/ I
C  I     I                               I           ITAU * (XN1-XN)  I
C  +-----+-------------------------------+-----------+----------------+
C  I     I                                   I I*        I
C  I 350 I  à…ƒ“‹Ÿ’Ž ‘…–. „‹Ÿ ‘ˆ•ŽŽ‰   I I  *      I
C  I     I    Œ€˜ˆ›                         I I    *    I
C  +-----+-------------------------------+-----------+----------------+
C  I     I                                   I I*        I
C  I 355 I  …ƒ“‹Ÿ’Ž ‘…–. „‹Ÿ ‘ˆ•ŽŽ‰   I I  *      I
C  I     I  Œ€˜ˆ›,¯à¨­ã¤¨â¥«ì­ ï ª®¬¬ãâ æ¨¨ I I    *    I
C  I-----I-----------------------------------I-----------I
C                ******************************************************
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C
C----------------------------------------------------------------------
C              ‹EMEHT› OˆC›BAžTCŸ:
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
C          W    -     —ACTOTA OCHOBHO‰ ƒAPMOHˆKˆ ( ƒ– );
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
C        # T 319 UMAX UMIN TZ TP TI
C       ƒ„E
C            UMAX  - MAKCˆM.HAPŸ†EHˆE
C            UMIN  - MˆHˆMA.HAPŸ†EHˆE
C            TZ    =  C„Bˆƒ
C             TP   -  EPˆO„
C              TI  -  „‹ˆTE‹œHOCTœ
C      ----------------------------------------------------------------
C----------------------------------------------------------------------
C       # T 350 “ 0   #‹1 #CM1 #‹2 #‘Œ2   … L
C
C      ƒ„E
C          #‹1,#CM1- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO ’ŽŠ€;
C          #‹2,#CM2- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO “ƒ‹Ž‚Ž‰ ‘ŠŽŽ‘’ˆ ;
C              -  “ƒŽ‹ ‡€€‘€ „‹Ÿ “€‚. ’ˆˆ‘’ŽŽ‚
C          …    -  ŽŒˆ€‹œ›‰ ŠŽ””ˆ–ˆ…’ „‘
C          L      -   ˆ„“Š’ˆ‚Ž‘’œ ‘’€’ŽŽ‰ ŽŒŽ’Šˆ
C----------------------------------------------------------------------
C       # T 355 “ 0   #‹1 #CM1 #‹2 #‘Œ2 #‹3 #‘Œ3 #‹4 #‘Œ4 ŽŒ… UY
C
C      ƒ„E
C          #‹1,#CM1- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE “€‚‹Ÿ…ŒŽƒŽ ’ŽŠ€;
C          #‹2,#CM2- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO …ƒ“‹Ÿ’Ž€  ;
C          #‹3,#CM3- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE ‘ˆƒ€‹€ ……Š‹ž—…ˆŸ ;
C          #‹4,#CM4- HOMEP ‹EMEHTA ˆ ŽŒ… TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO “ƒ‹Ž‚Ž‰ ‘ŠŽŽ‘’ˆ ;
C          ŽŒ…   -  à¥¤¥« ã£«®¢®© áª®à®áâ¨ ,¯à¨ ¤®áâ¨¦¥­¨¥ ª®â®à®£® § ª -
C                   ­ç¨¢ ¥âáï à¥¦¨¬ ¯à¨­ã¤¨â¥«ì­®© ª®¬¬ãâ æ¨¨ .
C          UY     -  ‡­ ç¥­¨¥ á¨£­ «  ã¯à ¢«¥­¨ï ¢ ¨­¢¥àâ®à­®¬ à¥¦¨¬¥
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
C               "OCTOŸHHAŸ" —ACTœ MSW TˆOB # 310,311,312,313:
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
C                ”OPMAT RMSW Tˆ # 310:
C                ----------------------
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
C            I              I      I         „‹Ÿ OˆCAHˆŸ ‡A„. CˆƒHA‹A;
C            I              I      I         (C “—ETOM OCT. COCTAB‹.);
C 13+NBXOD*4 I  4+NBXOD*4   I  FM  I  W  -   —ACTOTA OCH. ƒAPMOHˆKˆ(ƒ–);
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
C    16      I     7        I  FM  I         PE‡EPB
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
C             ”OPMAT RMSW Tˆ # 350
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
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C    15      I     6        I  FM  I  #‹2 - HOMEP ‹EMEHTA;
C    16      I     7        I  FM  I  #CM2 - HOMEP TOKA ‹EMEHTA #‹1;
C    17      I     8        I  LM  I  ATOKM2 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM2
C            I              I      I           ‹EMEHTA #‹2;
C    18      I     9        I  FM  I     - “ƒŽ‹ ‡€€‘€       ;
C    19      I     10       I  FM  I  …   - ŽŒˆ€‹›‰ ŠŽ.€Ÿ†…ˆŸ ;
C    20      I     11       I  FM  I  L    - ˆ„“Š’ˆ‚Ž‘’œ     ;
C    21      I     12       I  FM  I          PE‡EPB
C----------------------------------------------------------------------
C             ”OPMAT RMSW Tˆ # 355
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
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;(”« £ à¥¦¨¬ 
C            I              I      I  ¯à¨­ã¤¨â¥«ì­®© ª®¬¬ãâ æ¨¨ )
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C    15      I     6        I  FM  I  #‹2 - HOMEP ‹EMEHTA;
C    16      I     7        I  FM  I  #CM2 - HOMEP TOKA ‹EMEHTA #‹1;
C    17      I     8        I  LM  I  ATOKM2 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM2
C            I              I      I           ‹EMEHTA #‹2;
C    18      I     9        I  FM  I  #‹3 - HOMEP ‹EMEHTA;
C    19      I     10       I  FM  I  #CM3 - HOMEP TOKA ‹EMEHTA #‹1;
C    20      I     11       I  LM  I  ATOKM3 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM3
C            I              I      I           ‹EMEHTA #‹3;
C    21      I     12       I  FM  I  #‹4 - HOMEP ‹EMEHTA;
C    22      I     13       I  FM  I  #CM4 - HOMEP TOKA ‹EMEHTA #‹1;
C    23      I     14       I  LM  I  ATOKM4 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM4
C            I              I      I           ‹EMEHTA #‹4;
C    24      I     15       I  FM  I  OME  - à¥¤¥« ã£«®¢®© áª®à®áâ¨ ;
C    25      I     16       I  FM  I  UY   - ‡­ ç¥­¨¥ á¨£­ «  ã¯à ¢«¥­¨ï;
C----------------------------------------------------------------------
C
C----------------------------------------------------------------------
C
C     B MACCˆBE TOKN ˆ TOKN1 CO„EP†ˆTCŸ:
C     **********************************
C     TOKN/N1(1) - ‡HA—EHˆE B›XO„HOƒO CˆƒHA‹A;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(100)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION INTMAS(1), STMAS(100), DGMAS(12)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( N , RN )         
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
       NTIP = TIP - 309
       IF ( TIP .EQ. 355 ) GO TO 9900 
       IF ( NTIP . EQ . 10.OR.NTIP.EQ.41 ) GO TO 9900
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0024 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
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
       IF ( NTIP .EQ . 10) GO TO 5000
       IF ( TIP .EQ . 355) GO TO 7000       
       IF ( NTIP .EQ . 41) GO TO 6000
       GO TO (1000,2000,3000,4000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   POOP–ˆOHA‹œHO C“MMˆP“ž™EE ‡BEHO ( Tˆ 310 )
C----------------------------------------------------------------------
       NBXOD = MSW(11)
       NGARM = MSW(12 + 4*NBXOD)
       SIGNAL = 0.
       IF( NGARM .LT. 1 ) GO TO 1110
C----------------------------------------------------------------------
C--> PˆABˆM ‡HA—EHˆE ‡A„AHHOƒO BXO„HOƒO CˆƒHA‹A
       W = RMSW(13 + 4*NBXOD)
       A0 = RMSW(14 + 4*NBXOD)
       SIGNAL = A0
       I1 = 2*(NGARM - 1)
       IF( I1 .EQ. 0 ) GO TO 1110
          DO 1100 I=1,I1,2
          NRMSW = 14 + I + 4*NBXOD
          AI = RMSW(NRMSW)
          FII= RMSW(NRMSW+1)
          N = I / 2 + 1
          SIGNAL = SIGNAL + AI * SIN(N*W*(TN+H0) + FII)
1100   CONTINUE
1110   CONTINUE
C----------------------------------------------------------------------
C--> POC“MMˆP“EM BXO„H›E CˆƒHA‹›
       IF(NBXOD .EQ. 0 ) GO TO 1210
          DO 1200 I=1,NBXOD
          NMSW = 10 + I*4
          AKI  = RMSW(NMSW)
          TOKI = TOKN1( MSW(NMSW + 1) )
          SIGNAL = SIGNAL + AKI*TOKI
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
C######################################################################
2000   CONTINUE
C----------------------------------------------------------------------
C-->   ˆHTEƒPˆP“ž™EE ‡BEHO ( Tˆ 311 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK=  MSW(14)
       TOKBX= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = TOKN(ITOK) + H0 * TOKBX / TAU
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
3000   CONTINUE
C----------------------------------------------------------------------
C-->   ˆHEP–ˆOHHOE ‡BEHO ( Tˆ 312 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK=  MSW(14)
       TOKBX= TOKN1(NTOK)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = TOKN(ITOK) * EXP(-H0/TAU) +
     +               ( 1 - EXP(-H0/TAU) ) * TOKBX
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
4000   CONTINUE
C----------------------------------------------------------------------
C-->   „ˆ””EPEH–ˆP“ž™EE ‡BEHO ( Tˆ 313 )
C----------------------------------------------------------------------
       TAU = RMSW(15)
       NTOK=  MSW(14)
       TOKBX = TOKN1(NTOK) - TOKN(NTOK)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = TOKN(ITOK) * EXP(-H0/TAU) +
     +               EXP(-H0/TAU) * TOKBX
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
5000   CONTINUE
C----------------------------------------------------------------------
C-->   ƒEHEPATOP ˆ‹O-OPA‡HOƒO CˆƒHA‹A ( Tˆ # 319 )
C----------------------------------------------------------------------
       UMAX  = RMSW(12)
       UMIN = RMSW(13)
       TZ = RMSW(14)
       TP = RMSW(15)
       TI = RMSW(16)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = 0.0
       T1 = TN +H0 - TZ
       IF ( T1 .LT.0.) GO TO 5005
       T2 = AMOD(T1,TP)
       IF(T2.LE.TI) TOKN1(ITOK)=UMAX -(UMAX-UMIN)*T2/TI
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
5005   MSW(10) = 1
       RETURN
C######################################################################
6000   CONTINUE
C----------------------------------------------------------------------
C-->   ¥£ã« â®à á¯¥æ¨ «ì­® ¤«ï á¨­åà®­­®© ¬ è¨­ë (’ˆ 350 -150)
C----------------------------------------------------------------------
       NTOK=  MSW(14)
       TOKBX = ABS(TOKN1(NTOK)) 
       NTOK=  MSW(17)
       OMEGA = TOKN1(NTOK) 
       TETA  = RMSW(18)*OMEGA   
       EML   = RMSW(19)
       XR    = RMSW(20)
C----------------------------------------------------------------------
       TOKID = (COS(TETA) - 0.5)*EML / (XR*2.)
       IF ( TOKBX.GE.TOKID ) GOTO 6010
        UY0 = 2.*TOKBX*XR / EML - COS(TETA)
        GOTO  6100
6010   CONTINUE
       TOKID = COS(TETA + 0.5236 )*EML / (XR*2.)
       IF ( TOKBX.GE.TOKID ) GOTO 6020
        GAMMA = (1.0472 - TETA)*0.5
        FI   = ASIN(XR*TOKBX / (EML*SIN(GAMMA)))
        UY0 = COS(3.14159 - FI -GAMMA )
        GOTO  6100
6020   CONTINUE
       TOKID = (SIN(TETA) + COS(TETA+ 0.5236))*EML / (XR*2.)
       IF ( TOKBX.GE.TOKID ) GOTO 6030
        UY0 = 2.*TOKBX*XR / EML - COS(TETA + 0.5236)
        GOTO  6100
6030   CONTINUE
       TOKID = (COS(TETA) + 1.0 )*EML / (XR*2.)
       IF ( TOKBX.GE.TOKID ) GOTO 6040
        FI   = ACOS(2.*XR*TOKBX / EML - COS(TETA))
        UY0 = COS(FI + 0.5236 )
        GOTO  6100
6040   CONTINUE
         WRITE(NWTRE,6998) MSW(12), MSW(13)
6998     FORMAT( ' *** CO‰ B DM0024 - ‘‹ˆ˜œŠŽŒ Ž‹œ˜Ž‰ ’ŽŠ „‹Ÿ 350
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
6100     CONTINUE
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = UY0
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
7000   CONTINUE
C----------------------------------------------------------------------
C-->   ¥£ã« â®à á¯¥æ¨ «ì­® ¤«ï á¨­åà®­­®© ¬ è¨­ë (’ˆ 355)
C----------------------------------------------------------------------
       NTOK=  MSW(14)
       TOKYP = ABS(TOKN1(NTOK)) 
       NTOK=  MSW(17)
       TOKPE = TOKN1(NTOK) 
       NTOK=  MSW(20)
       CPERE = TOKN1(NTOK) 
       NTOK=  MSW(23)
       OMEGA = TOKN1(NTOK) 
       WOGR  = RMSW(24)   
       UY   = RMSW(25)
C----------------------------------------------------------------------
C--> ¥¦¨¬ ¯à¨­ã¤¨â¥«ì­®© ª®¬¬ãâ æ¨¨ «¨ ??? ....
C----------------------------------------------------------------------
       IF ( OMEGA .LE. WOGR ) GO TO 7010 
        UY =   TOKPE
       GO TO 7100
7010   CONTINUE
C--> ‚ ª ª®¬ à¥¦¨¬¥ : ¢ë¯àï¬¨â¥«ì­ë© ¨«¨ ¨­¢¥àâ®à­ë© ?       
        KEY = MSW(12)
        IF ( TOKYP .LE. 0.001 )  KEY = 0
        IF ( CPERE .GT. 0.1 )  KEY = 1
        MSW(12) = KEY        
        IF (KEY .EQ. 0 )  UY = TOKPE 
C----------------------------------------------------------------------
7100     CONTINUE
C--> ‡Aˆ˜EM B TOKN1
       ITOK=  MSW(7)
       TOKN1(ITOK) = UY
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
       END
