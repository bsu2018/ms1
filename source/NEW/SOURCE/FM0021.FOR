       SUBROUTINE FM0021(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   OKTŸPŸ 1987 ƒ.)-----------
C   O„POƒPAMMA: FM0021(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹E‰ HE‹ˆHE‰H›X „ˆHAMˆ—ECKˆX ‡BEHœEB:
C   ----------
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
C   +-----+-------------------------------+------------------------+
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
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0025, LM0007 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C
C                      „‹ˆHA:  Tˆ 320:==>+1<VAR>
C                      „‹ˆHA:  Tˆ 321:==> 10
C                      „‹ˆHA:  Tˆ 322:==>+1<VAR>
C                      „‹ˆHA:  Tˆ 323:==>+1<VAR>
C                      „‹ˆHA:  Tˆ 325:==> 6
C----------------------------------------------------------------------
C              ‹EMEHT OˆC›BAETCŸ:
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
C          # T 325 “ 0   #‹  #CM
C
C         ƒ„E:
C          #‹ ,#CM - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
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
C                      ”OPMAT RMSW Tˆ # 320
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
C
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
C            I              I      I        PAR=0 - IB›X=Y1
C            I              I      I        PAR=1 - IB›X=Y2
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 319
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
       IF ( NTIP .EQ. 6                   ) GO TO 1
500    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0021 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       GO TO (100,200,300,400,500,600),NTIP
C######################################################################
100   CONTINUE
C----------------------------------------------------------------------
C---> HE‹ˆHE‰HOE ‡BEHO ( Tˆ  # 320 )
C----------------------------------------------------------------------
C--> OPE„E‹EHˆE KOHCTAHT
       NT = IFIX ( PAR0T(5) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 16 + 2*NT
       RETURN
C######################################################################
1000   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 7 + 2*NT
       DO 102 I=1,LM
102    RMSW(I) = 0.0
       N = 1
       RMSW(2) = RN
C---> POBEPˆM  PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       NT1 = (IFIX ( PAR0T(4) ) - 1 ) / 2
       IF ( NT .EQ. NT1 ) GO TO 103
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O˜ˆKA B FM0021 -->HEPABˆ‹œHO ‡A„AHO KO‹ˆ—ECTBO TO
     =—EK ***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
103    CONTINUE
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  ‡AOMHˆM #‹ , #CM  ˆ  P
       N = IFIX( PAR0T(1) )
       RMSW(3)=RN
       N = IFIX( PAR0T(2) )
       RMSW(4)=RN
       N = IFIX ( PAR0T(3) )
       RMSW (6) = RN
C---->  ‡AOMHˆM —ˆC‹O TO—EK EPE„ATO—HO‰ XAPAKTEPˆCTˆKˆ ---------
       N = NT
       RMSW(7) = RN
       IF( NT.EQ. 0 ) GO TO 105
C---->  ‡AOMHˆM KOOP„ˆHAT› TO—EK EPE„ATO—HO‰ XAPAKTEPˆCTˆKˆ ---------
          DO 104 I = 1 , NT
          RMSW(7+I)   = PAR0T( 4 + 2*I )
          RMSW(7+NT+I)= PAR0T( 5 + 2*I )
104    CONTINUE
105    CONTINUE
C----> POBEPˆM,PAB‹œHO ‹ˆ BBE„EH› „AHH›E ?
       DO 106 I=2,NT
       IF ( RMSW(6+I) .LE. RMSW(7+I) ) GO TO 106
C----------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** O˜ˆKA B FM0021-->OP„ˆHAT› ‡A„AH› HE B BO‡PACAž™EM
     =OPŸ„KE ***'/' *** HOMEP ‹EMEHTA',I5,' HOMEP TˆA',I5,' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
106    CONTINUE
       RETURN
C######################################################################
200   CONTINUE
C----------------------------------------------------------------------
C---> PE‹E‰HOE ‡BEHO ( Tˆ  # 321 )
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 10
       DO 212 I=1,LM
212    RMSW(I) = 0.0
C---> POBEPˆM  PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       X1 =  PAR0T(3)
       X2 =  PAR0T(4)
       Y1 =  PAR0T(5)
       Y2 =  PAR0T(6)
       IF ( X1 .LT. X2  .AND. Y1 .LT. Y2 ) GO TO 213
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 512 ) NEL, TIP
512    FORMAT(/' *** O˜ˆKA B FM0021 -->HEPABˆ‹œHO ‡A„AH› APAMETP›
     =***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
213    CONTINUE
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  ‡AOMHˆM #‹ , #CM , X1 , X2 , Y1 , Y2.
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = X1
       RMSW(7) = X2
       RMSW(8) = Y1
       RMSW(9) = Y2
       RETURN
C######################################################################
300    CONTINUE
C----------------------------------------------------------------------
C--> ‡BEHO "—ˆCTOE ‡AA‡„›BAHˆE" ( Tˆ # 322 )
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ POBEPˆTœ „AHH›E.
       TAU = PAR0T(3)
       IF ( TAU .GE. 0. ) GO TO 314
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 513 ) NEL, TIP
513   FORMAT(/' *** O˜ˆKA B FM0021 -->HEPABˆ‹œHO ‡A„AHA BPEM.‡A„EP†KA
     =***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
314    CONTINUE
       NT = 2 * ( IFIX( TAU / H0 ) + 1 )
       IF ( NT .LE. 100 ) GO TO 315
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 514 ) NEL, TIP
514   FORMAT(/' *** O˜ˆKA B FM0021 -->C‹ˆ˜KOM O‹œ˜AŸ BPEMEH. ‡A„EP†KA
     =***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
315    CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 3000
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 18 + 2 * NT
       RETURN
C######################################################################
3000   CONTINUE
       LM = 9 + 2 * NT
       DO 316 I=1,LM
316    RMSW(I) = 0.0
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  ‡AOMHˆM #‹ , #CM , TAU , HU
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RMSW(6) = PAR0T(3)
       RMSW(7) = PAR0T(4)
       N = NT
       RMSW(8) = RN
       N = 19
       RMSW(9) = RN
       RETURN
C######################################################################
400    CONTINUE
C----------------------------------------------------------------------
C-->  “MHO†ˆTE‹œ ( Tˆ # 323 )
C----------------------------------------------------------------------
C--> OPE„E‹EHˆE KOHCTAHT
       NBXOD = IFIX ( PAR0T(3) )
       IF ( FGMSW .EQ. 0 ) GO TO 4000
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 12 + 3*NBXOD
       RETURN
C######################################################################
4000   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 3 + 3*NBXOD
       DO 420 I=1,LM
420    RMSW(I) = 0.0
       RMSW(3+3*NBXOD) = PAR0T(1)
C---> POBEPˆM  PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       NBXOD1 = ( IFIX( PAR0T(2)) - 1 ) / 2
       IF ( NBXOD .EQ. NBXOD1 ) GO TO 422
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
422    CONTINUE
C---> ‡AOMHˆM HOMEPA BXO„H›X TOKOB.
       N = NBXOD
       RMSW(2) = RN
       IPAR0T = 3
       IRMSW = 2
          DO 425 I = 1 , NBXOD
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IRMSW = IRMSW + 1
425    CONTINUE
       RETURN
C######################################################################
600   CONTINUE
C----------------------------------------------------------------------
C---> KOPEHœ KBA„PATH›‰ (Tˆ  # 325 )
C----------------------------------------------------------------------
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 6
       DO 612 I=1,LM
612    RMSW(I) = 0.0
C---> POBEPˆM  PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  ‡AOMHˆM #‹ , #CM
       N = NELBX
       RMSW(3) = RN
       N = IFIX( PAR0T(2) )
       RMSW(4) = RN
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) NEL, TIP,NELBX
555    FORMAT(/' *** O˜ˆKA B FM0021 -->HEPABˆ‹œHO ‡A„AH› BXO„H›E „AHH›
     =E   ***'/' *** HA BXO„ ‹EMEHTA ', I5, ' HOMEP TˆA ', I5,' ***',
     =         ' *** O„AETCŸ TOK ‹EMEHTA, ˆMEž™EƒO O‹œ˜ˆ‰ OPŸ„KOB›‰
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
