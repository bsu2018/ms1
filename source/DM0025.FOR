         SUBROUTINE DM0025 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAŸ   1987 ƒ.)------------
C   O„POƒPAMMA:  DM0025 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C        OPAAT›BAET MAKPOMO„E‹ˆ HE‹ˆHE‰H›X „ˆHAMˆ—ECKˆX ‡BEHœEB:
C
C   +-----+-------------------------------+------------------------+
C   I     I                               I         EPE„A-        I
C   I Tˆ I       HAˆMEHOBAHˆE ‡BEHA      I         TO—HAŸ         I
C   I     I                               I         ”“HK–ˆŸ        I
C   +-----+-------------------------------+------------------------+
C   I     I                               I             I Y    *   I
C   I     I                               I             I     *    I
C   I     I        HE‹ˆHE‰HOE ‡BEHO       I             I*  *      I
C   I 320 I  (EPE„ATO—HAŸ ”“HK–ˆŸ EƒO    I      ------*+------->  I
C   I     I  ‡A„AHHO HAOPOM TO—EK)       I         *   I       X  I
C   I     I                               I    *  *     I          I
C   I     I                               I             I          I
C   +-----+-------------------------------+------------------------+
C   I     I                               I            !Y          I
C   I     I                               I      !--<--*----!----- I
C   I     I                               I      !     !“2  !      I
C   I     I                               I      !     !    !    X I
C   I 321 I       PE‹E‰HOE ‡BEHO          I -----*-----I----*----- I
C   I     I                               I      !X1   !    !X2    I
C   I     I                               I      !     !    !      I
C   I     I                               I------!-----*-->-!      I
C   I     I                               I            !“1         I
C   +-----+-----------------------------------+--------------------+
C   I     I                               I !X         *      *    I
C   I     I                               I !    *                 I
C   I     I                               I !*                  T  I
C   I     I                               I *--------------------- I
C   I 322 I  ‡BEHO "—ˆCTOE ‡AA‡„›BAHˆE"  I !“                     I
C   I     I                               I !              *      *I
C   I     I                               I !TAU     *             I
C   I     I                               I !<->!*              T  I
C   I     I                               I *---*----------------- I
C   +-----+-------------------------------+------------------------+
C   I     I                               I                        I
C   I 323 I       “MHO†ˆTE‹œ              I “ = K * X1 * X2 *...   I
C   I     I                               I                        I
C   +-----+-------------------------------+------------------------+
C   I     I                               I                        I
C   I 325 I    KOPEHœ KBA„PATH›‰          I “ = SQRT(X)            I
C   I     I                               I                        I
C   +-----+-------------------------------+------------------------+
C                ******************************************************
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C
C----------------------------------------------------------------------
C              ‹EMEHT› OˆC›BAžTCŸ:
C
C       # T 320 “ 0   #‹  #CM  P
C              MAS < NT  X1  Y1  X2  Y2 ... >
C
C      ƒ„E
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          P      -   Pˆ‡HAK Bˆ„A APOKCˆMA–ˆˆ:
C                     P = 1 - K“CO—HO-‹ˆHE‰HAŸ ;
C                     P = 2 - KBA„PATˆ—HAŸ ;
C          NT     -   KO‹ˆ—ECTBO  TO—EK EPE„ATO—HO‰ ”“HK–ˆˆ
C                     ‡A„AHHO‰ TA‹ˆ—HO;
C          X1 Y1  -   COOTBETCTBEHHO A–ˆCCA ˆ OP„ˆHATA TO—Kˆ
C          ˆ T.„.     EPE„ATO—HO‰ ”“HK–ˆˆ
C
C          ------------------------------------------------------------
C
C          # T 321 “ 0   #‹  #CM  X1  X2  “1  “2
C
C         ƒ„E:
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          X1,X2  -   A–ˆCC› TO—EK EPECE—EHˆŸ EPE„ATO—HO‰ ”“HK–ˆˆ
C                     C OCœž A–ˆCC.
C          “1,“2  -   OP„ˆHAT› TO—EK EPECE—EHˆŸ EPE„ATO—HO‰ ”“HK–ˆˆ
C                     C OCœž OP„ˆHAT.
C          ------------------------------------------------------------
C
C          # T 322 “ 0   #‹  #CM  TAU  HA—.“C‹.
C
C         ƒ„E:
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          TAU    -   OCTOŸHHAŸ BPEMEHˆ ‡A„EP†Kˆ;
C          HA—.“C‹. - ‡HA—EHˆE B›XO„HOƒO CˆƒHA‹A Pˆ T=[0,TAU];
C          ------------------------------------------------------------
C
C          # T 323 “ 0   K
C              MAS < NBXOD  #‹1 #CM1  #‹2 #CM2   ... >;
C
C         ƒ„E
C          K        - KO””ˆ–ˆEHT POOP–ˆOHA‹œHOCTˆ POˆ‡BE„EHˆŸ
C                     BXO„H›X TOKOB;
C          NBXOD  -   KO‹ˆ—ECTBO TOKOB ‹EMEHTOB CXEM› , KOTOP›E
C                     ˆCO‹œ‡“žTCŸ B KA—ECTBE BXO„H›X CˆƒHA‹OB;
C          #‹I,#CMI- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C          ------------------------------------------------------------
C
C          # T 325 “ 0   #‹  #CM  X1  X2  “1  “2
C
C         ƒ„E:
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
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
C          "OCTOŸHHAŸ" —ACTœ MSW TˆOB # 320,321,322,323,325:
C           --------------------------------------------------
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
C                ”OPMAT RMSW Tˆ # 320:
C                ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I  NMIN - HOMEP ‹ˆ†A‰˜E‰ MEHœ˜E‰
C            I              I      I         TO—Kˆ „‹Ÿ T=TN+
C    12      I     3        I  FM  I  #‹  - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM  - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ‹EMEHTA #‹;
C    15      I     6        I  FM  I  P   - Pˆ‡HAK Bˆ„A APOKCˆMA–ˆˆ
C    16      I     7        I  FM  I  NT  - KO‹ˆ—ECTBO  TO—EK
C            I              I      I        EPE„ATO—HO‰ ”“HK–ˆˆ
C    17      I     8        I  FM  I  X1  - A–ˆCCA 1-‰ TO—Kˆ
C            I              I      I        EPE„ATO—HO‰ ”“HK–ˆˆ
C   ....     I    .....     I      I  .....
C    16+NT   I     7+NT     I  FM  I  X(NT) - A–ˆCCA NT-O‰  TO—Kˆ
C            I              I      I          EPE„ATO—HO‰ ”“HK–ˆˆ
C    16+NT+1 I     7+NT+1   I  FM  I  Y1    - OP„ˆHATA 1-O‰ TO—Kˆ
C            I              I      I          EPE„ATO—HO‰ ”“HK–ˆˆ
C    ......  I    ......    I      I .......
C    16+2NT  I     7+2NT    I  FM  I  Y(NT) - OP„ˆHATA NT-O‰ TO—Kˆ
C            I              I      I          EPE„ATO—HO‰ ”“HK–ˆˆ
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 16 + 2 * NT
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 7 + 2 * NT
C----------------------------------------------------------------------
C
C                      ”OPMAT RMSW Tˆ # 321
C                 ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I         PE‡EPB
C    12      I     3        I  FM  I  #‹  - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM  - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ‹EMEHTA #‹;
C    15      I     6        I  FM  I  X1  - (CM. PˆC.)
C    16      I     7        I  FM  I  X2  - (CM. PˆC.)
C    17      I     8        I  FM  I  Y1  - (CM. PˆC.)
C    18      I     9        I  FM  I  Y2  - (CM. PˆC.)
C    19      I     10       I  DM  I  PAR - APAMETP, OPE„E‹Ÿž™ˆ‰
C            I              I      I        COCTOŸHˆE B›X. CˆƒHA‹A
C            I              I      I        Pˆ T=TN.
C            I              I      I        PAR=0 - IB›X=“1
C            I              I      I        PAR=1 - IB›X=“2
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 19
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 10
C----------------------------------------------------------------------
C
C
C                      ”OPMAT RMSW Tˆ # 322
C                 ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I         PE‡EPB
C    12      I     3        I  FM  I  #‹  - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM  - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ‹EMEHTA #‹;
C    15      I     6        I  FM  I  TAU - OCTOŸHHAŸ BPEMEHˆ ‡A„EP†Kˆ;
C    16      I     7        I  FM  I  HA—.“C‹.- HA—A‹œH›E “C‹OBˆŸ;
C    17      I     8        I  FM  I  NT  - KO‹-BO TO—EK BXO„HOƒO CˆƒH.
C    18      I     9        I  DM  I  IPAR- “KA‡ATE‹œ;
C    19      I     10       I  DM  I  T1  - BPEMŸ;
C    20      I     11       I  DM  I  T2  - BPEMŸ;
C   ....     I    .....     I .... I .............................
C   18+NT    I    9+NT      I  DM  I  TNT - BPEMŸ;
C   18+NT+1  I    9+NT+1    I  DM  I  IBX1- BXO„HO‰ CˆƒHA‹ Pˆ T=T1
C   18+NT+2  I    9+NT+2    I  DM  I  IBX2- BXO„HO‰ CˆƒHA‹ Pˆ T=T2
C   ....     I    .....     I .... I .............................
C   18+2*NT  I    9+2*NT    I  DM  I  IBXNT-BXO„HO‰ CˆƒHA‹ Pˆ T=TNT
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 18 + 2*NT
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 9 + 2*NT
C----------------------------------------------------------------------
C
C
C              ”OPMAT RMSW Tˆ # 323:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NBXOD-KO‹ˆ—ECTBO TOKOB, ˆCO‹œ-
C            I              I      I        ‡“EM›X KAK BXO„H›E CˆƒHA‹›;
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C .......    I   .......    I  ... I  ..........................
C  9+I*3     I     I*3      I  FM  I  #‹I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C 12+3*NBXOD I  3+3*NBXOD   I  FM  I  K    - KO””ˆ–ˆEHT POOP–ˆOHA‹œ-
C            I              I      I         HOCTˆ;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 12 + 3*NBXOD
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 3 + 3*NBXOD
C----------------------------------------------------------------------
C
C
C
C                      ”OPMAT RMSW Tˆ # 325
C                 ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I         PE‡EPB
C    12      I     3        I  FM  I  #‹  - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM  - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ‹EMEHTA #‹;
C    15      I     6        I      I         PE‡EPB
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 15
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 6
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
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR
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
         INTEGER FITER, TIP, ZAPROS, P
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       NTIP = TIP - 319
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 9991
       IF ( NTIP .EQ. 6                   ) GO TO 9991
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0025 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9991   CONTINUE
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
C######################################################################
       ITOK = MSW(7)
       GO TO (1000,2000,3000,4000,6000,6000),NTIP
1000   CONTINUE
C----------------------------------------------------------------------
C-->   HE‹ˆHE‰HOE ‡BEHO ( Tˆ 320 )
C----------------------------------------------------------------------
       NMIN = MSW(11)
       P    = MSW(15)
       NT   = MSW(16)
       NTM1 = NT - 1
       SIGNAL = 0.
C----------------------------------------------------------------------
C--> HE‰„EM BXO„HO‰ CˆƒHA‹
       SIGBX = TOKN1( MSW(14) )
       IF( SIGBX .LT. RMSW(17) ) GO TO 1800
       IF( SIGBX .GT. RMSW(16+NT) ) GO TO 1700
C--> CPABHˆM C PE„›„“™ˆM.
       IF( SIGBX .LE. RMSW(16+NMIN) ) GO TO 1200
C--> CˆƒHA‹ “BE‹ˆ—ˆ‹CŸ.
1100   CONTINUE
       IF( SIGBX .LE. RMSW(17+NMIN) ) GO TO 1500
       NMIN = NMIN + 1
       GO TO 1100
1200   CONTINUE
C--> CˆƒHA‹ “MEHœ˜ˆ‹CŸ
       NMIN = NMIN - 1
       IF( SIGBX .GE. RMSW(16+NMIN) ) GO TO 1500
       GO TO 1200
C--> OA‹ˆ B CEPE„ˆH“ TA‹ˆ–›. ˆHTEPO‹ˆP“EM...
1500   CONTINUE
       X1 = RMSW(16+NMIN)
       X2 = RMSW(17+NMIN)
       Y1 = RMSW(16+NT+NMIN)
       Y2 = RMSW(17+NT+NMIN)
       IF( P .EQ. 2 ) GO TO 1600
1550   CONTINUE
C--> ‹ˆHE‰HAŸ ˆHTEPO‹Ÿ–ˆŸ.
       SIGNAL = ( (Y1-Y2)*SIGBX + Y2*X1 - Y1*X2 ) / ( X1 - X2 )
       MSW(11) = NMIN
       GO TO 9900
1600   CONTINUE
C--> KBA„PATˆ—HAŸ ˆHTEPO‹Ÿ–ˆŸ.
       N3 = NMIN - 1
       IF ( N3 .EQ. 0 ) N3 = NMIN + 2
       X3 = RMSW(16+N3)
       Y3 = RMSW(16+NT+N3)
       A = ( (Y3-Y1)/(X3-X1) - (Y2-Y1)/(X2-X1) ) / (X3-X2)
       B = (Y2-Y1)/(X2-X1) - A * (X2+X1)
       C = Y1 - A * X1**2 - B * X1
       SIGNAL = A * SIGBX**2 + B * SIGBX + C
       MSW(11) = NMIN
       GO TO 9900
1700   CONTINUE
C--> B›˜‹ˆ ‡A BEPXHžž ƒPAHˆ–“...
       X1 = RMSW(15+NT)
       X2 = RMSW(16+NT)
       Y1 = RMSW(15+2*NT)
       Y2 = RMSW(16+2*NT)
       NMIN = NT
       GO TO 1550
1800   CONTINUE
C--> B›˜‹ˆ ‡A Hˆ†Hžž ƒPAHˆ–“...
       X1 = RMSW(17)
       X2 = RMSW(18)
       Y1 = RMSW(17+NT)
       Y2 = RMSW(18+NT)
       NMIN = 1
       GO TO 1550
C######################################################################
2000   CONTINUE
C----------------------------------------------------------------------
C-->   PE‹E‰HOE ‡BEHO ( Tˆ 321 )
C----------------------------------------------------------------------
       NPAR = MSW(19)
       X1  = RMSW(15)
       X2  = RMSW(16)
       Y1  = RMSW(17)
       Y2  = RMSW(18)
C----------------------------------------------------------------------
C--> HE‰„EM BXO„HO‰ CˆƒHA‹
       SIGBX = TOKN1( MSW(14) )
       IF( SIGBX .LT. X1 ) GO TO 2500
       IF( SIGBX .GT. X2 ) GO TO 2600
C--> —TO ›‹O HA PE„›„“™EM ˜AƒE?
       IF( NPAR .EQ. 1 )   GO TO 2600
C--> CˆƒHA‹ PABEH  Y1 .
2500   CONTINUE
       SIGNAL = Y1
       MSW(19) = 0
       GO TO 9900
C--> CˆƒHA‹ PABEH  Y2
2600   CONTINUE
       SIGNAL = Y2
       MSW(19) = 1
C======================================================================
9900   CONTINUE
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
3000   CONTINUE
C----------------------------------------------------------------------
C-->  ‡BEHO TˆA "—ˆCTOE ‡AA‡„›BAHˆE"
C----------------------------------------------------------------------
       TN1 = TN + H0
       TAU = RMSW(15)
       HU  = RMSW(16)
       NT  =  MSW(17)
       IPAR=  MSW(18)
C----------------------------------------------------------------------
C--> HA‰„EM BXO„HO‰ CˆƒHA‹.
       SIGBX = TOKN (MSW(14))
       SIGBX1= TOKN1(MSW(14))
       IF( FITER .EQ. 1 ) GO TO 3010
C----------------------------------------------------------------------
C--> ‡AOMHˆM PE„›„“™ˆ‰ BXO„HO‰ TOK
       RMSW(IPAR) = TN
       RMSW(IPAR+NT) = SIGBX
3010   CONTINUE
C----------------------------------------------------------------------
C--> —TO CO BPEMEHEM?
       IF( TN1 .GE. TAU ) GO TO 3020
C--> TEK“™EE BPEMŸ MEHœ˜E BPEMEHˆ ‡A„EP†Kˆ.
       TOKN1(ITOK) = HU
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       IF( FITER .EQ. 1 ) RETURN
       IF( (IPAR-18) .EQ. NT ) IPAR = 18
       MSW(18) = IPAR + 1
       RETURN
C======================================================================
3020   CONTINUE
C----------------------------------------------------------------------
C--> TEK“™EE BPEMŸ O‹œ˜E BPEMEHˆ ‡A„EP†Kˆ.
       IF( TAU .GT. H0 ) GO TO 3030
C--> ‡A„EP†KA MEHœ˜E ˜AƒA.
       X1 = TN
       X2 = TN1
       Y1 = SIGBX
       Y2 = SIGBX1
       XN = TN1 - TAU
       GO TO 3100
3030   CONTINUE
C----------------------------------------------------------------------
C--> ‡A„EP†KA O‹œ˜E ˜AƒA.
C--> ˆ™EM BXO„HO‰ CˆƒHA‹.
       XN = TN1 - TAU
       ISIG = IPAR
       NT1 = 0
3040   CONTINUE
       NT1 = NT1 + 1
       IF( NT1 .GT. NT ) GO TO 3200
       ISIG = ISIG - 1
       IF( ISIG .EQ. 18 ) ISIG = 18 + NT
       ISIG1= ISIG - 1
       IF( ISIG1.EQ. 18 ) ISIG1= 18 + NT
       IF( XN .GT. RMSW(ISIG) .OR. XN .LT. RMSW(ISIG1) ) GO TO 3040
       X1 = RMSW(ISIG1)
       X2 = RMSW(ISIG)
       Y1 = RMSW(ISIG1+NT)
       Y2 = RMSW(ISIG+NT)
C----------------------------------------------------------------------
C--> ‹ˆHE‰HAŸ ˆHTEPO‹Ÿ–ˆŸ B›XO„HOƒO CˆƒHA‹A.
3100   CONTINUE
       TOKN1(ITOK) = ( (Y1-Y2)*XN + Y2*X1 - Y1*X2 ) / (X1 - X2)
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       IF( FITER .EQ. 1 ) RETURN
       IF( (IPAR-18) .EQ. NT ) IPAR = 18
       MSW(18) = IPAR + 1
       RETURN
C======================================================================
3200   CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,501) MSW(1), MSW(2)
501     FORMAT(' *** KO‹ˆ—ECTBO ‡AOMˆHAEM›X TO—EK HE „OCTATO—HO ***'/
     =         ' *** „‹Ÿ O‹“—EHˆŸ ‡A„AHHO‰ ‡A„EP†Kˆ O BPEMEHˆ  ***'/
     =         ' *** ‹EMEHT # ',I5,' Tˆ MO„E‹ˆ # ',I5, ' ***'//)
         CALL SUBERR(0)
        RETURN
C######################################################################
4000    CONTINUE
C----------------------------------------------------------------------
C-->  “MHO†ˆTE‹œ
C----------------------------------------------------------------------
        NBXOD = MSW(11)
        AK = RMSW(12+3*NBXOD)
C-->  HAXO„ˆM B›XO„HO‰ CˆƒHA‹, EPEMHO†ˆB BXO„H›E.
        SIGNAL = AK
           DO 4100 I=1,NBXOD
           SIGNAL = SIGNAL * TOKN1(MSW(11+3*I))
4100       CONTINUE
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
C######################################################################
6000   CONTINUE
C----------------------------------------------------------------------
C-->   KOPEHœ KBA„PATH›‰ (Tˆ 325 )
C----------------------------------------------------------------------
C--> HE‰„EM BXO„HO‰ CˆƒHA‹
       SIGBX = TOKN1( MSW(14) )
       SIGNAL = 0.0
       IF( SIGBX .GT. 0.0 ) SIGNAL = SQRT(SIGBX)
C----------------------------------------------------------------------
C--> ‡Aˆ˜EM B TOKN1
       TOKN1(ITOK) = SIGNAL
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
       END
