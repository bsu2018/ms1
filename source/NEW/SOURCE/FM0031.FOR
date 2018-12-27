       SUBROUTINE FM0031( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT  MAPTA  1988 ƒ.)------------
C   O„POƒPAMMA  FM0031( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA‡HA—EHˆE:  1) HACTPO‰KA MAKPOMO„E‹E‰ „BˆƒATE‹E‰ OCTOŸHHOƒO TOKA
C   ----------        Pˆ  ”=CONST.( Tˆ # 520 )
C                     Pˆ  ”=CONST,C TAXOƒEHEPATOPOM.( Tˆ # 521 )
C
C----------------------------------------------------------------------
C                  ‹EMEHT OˆC›BAETCŸ:
C
C            ?? T 520 “  K1  K2
C                        R  L  K”  J
C                      MAS < NT T1 Y1 ....   > ;
C
C           ƒ„E   R   -AKTˆBHOE COPOTˆB‹EHˆE OMOTKˆ ŸKOPŸ(OM);
C                 L   -ˆH„“KTˆBHOE COPOTˆB‹EHˆE OMOTKˆ ŸKOPŸ(ƒH);
C                 K”  -KOHCTP“KTˆBHAŸ OCTOŸHHAŸ „BˆƒATE‹Ÿ (B*C);
C                 J   -MOMEHT(B Kƒ*M)ˆHEP–ˆˆ BPA™. —ACTˆ (Kƒ*M**2);    ,
C              T1 Y1..-TA‹ˆ—HO ‡A„AHHAŸ ‡ABˆCˆMOCTœ MBH(T) ;          ,
C
C           *----------------------------------------------------------
C
C            ?? T 521 “  K1  K2
C                        R  L  K”  J  KTƒ  RTƒ
C                      MAS < NT T1 Y1 ....   > ;
C
C           ƒ„E   R   -AKTˆBHOE COPOTˆB‹EHˆE OMOTKˆ ŸKOPŸ(OM);
C                 L   -ˆH„“KTˆBHOE COPOTˆB‹EHˆE OMOTKˆ ŸKOPŸ(ƒH);
C                 K”  -KOHCTP“KTˆBHAŸ OCTOŸHHAŸ „BˆƒATE‹Ÿ (B*C);
C                 J   -MOMEHT(B Kƒ*M)ˆHEP–ˆˆ BPA™. —ACTˆ (Kƒ*M**2);
C                 KTƒ -KO””ˆ–ˆEHT EPE„A—ˆ TAXOƒEHEPATOPA (B/C**-1)
C                 RTƒ -BH“TPEHHEE COPOTˆB‹EHˆE OMOTKˆ TAXOƒEHEPATOPA;
C              T1 Y1..-TA‹ˆ—HO ‡A„AHHAŸ ‡ABˆCˆMOCTœ MBH(T) ;
C
C
C----------------------------------------------------------------------
C            2) Pˆ  FGMSW = 1 -> “TO—HŸETCŸ ‡HA—EHˆE LM -
C               O™E‰ „‹ˆH› MSW „AHHOƒO TˆA ( C “—ETOM MAS AP-OB );
C
C   METO„:      ”OPMˆPOBAHˆE EPEMEHHO‰ —ACTˆ MSW_ALL OˆCAHˆŸ ‹-TA,
C   -----       B›—ˆC‹EHˆE BCOMOƒATE‹œH›X „AHH›X, HEOXO„ˆM›X „‹Ÿ
C               „A‹œHE‰˜E‰ PAOT› MAKPOMO„E‹ˆ;
C
C   APAMETP›:  BA0T, KNOT, PAR0T - "OTPE‡Kˆ" MACCˆBOB BA0, KNOT, PAR0
C   ---------   BBE„EHH›X —EPE‡ / INPUT0;
C               RMSW - EPEMEHHAŸ —ACTœ MSW_ALL OˆCAHˆŸ ‹EMEHTA;
C               LM - Pˆ FGMSW = 0 => „‹ˆHA EPEMEHHO‰ —ACTˆ MSW_ALL;
C                    Pˆ FGMSW = 1 => „‹ˆHA BCEƒO MACCˆBA    MSW_ALL;
C   PˆME—AHˆE: 1. BCE O‹Ÿ  ‡AH“‹ŸžTCŸ
C   ----------
C
C              ”OPMAT RMSW Tˆ # 520:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I      I HOMEP “‡‹A HA—A‹A
C    10      I     0        I      I HOMEP “‡‹A KOH–A
C----------------------------------------------------------------------
C    11      I     1        I  FM  I  R  - COPOTˆB‹EHˆE ŸKOPŸ;
C    12      I     2        I  FM  I  L  - ˆH„“KTˆBHOCTœ ŸKOPŸ;
C    13      I     3        I  FM  I  K” - KOHCTP“KTˆBH›‰ KO””ˆ–ˆEHT;
C    14      I     4        I  FM  I  JM - MOMEHT ˆHEP–ˆˆ;
C    15      I     5        I  FM  I        PE‡EPB
C    16      I     6        I  FM  I  TŸ - OCTOŸHHAŸ ŸKOPHO‰ –Eˆ;
C    17      I     7        I  FM  I  TM - OCTOŸHHAŸ MEXAHˆ—ECKAŸ;
C    18      I     8        I  DM  I  MBH- ‡HA—EHˆE MBH „‹Ÿ T=TN;
C    19      I     9        I  DM  I  MBH1-‡HA—EHˆE MBH „‹Ÿ T=TN1;
C    20      I    10        I  DM  I  G -  ‡HA—EHˆE POBO„ˆMOCTˆ;
C    21      I    11        I  DM  I  J -  ‡HA—EHˆE ˆCT. TOKA;
C    22      I    12        I  FM  I         PE‡EPB
C    23      I    13        I  FM  I         PE‡EPB
C    24      I    14        I  FM  I  NT - KO‹ˆ—ECTBO TO—EK MBH(T);
C    25      I    15        I  FM  I  T1 - BPEMŸ;                      ;
C    26      I    16        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 24 + 2*NT
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 14 + 2*NT
C
C----------------------------------------------------------------------
C
C
C
C              ”OPMAT RMSW Tˆ # 521:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -3        I      I HOMEP “‡‹A HA—A‹A OMOTKˆ ŸKOPŸ
C    10      I    -2        I      I HOMEP “‡‹A KOH–A  OMOTKˆ ŸKOPŸ
C    11      I    -1        I      I HOMEP “‡‹A HA—A‹A OMOTKˆ Tƒ.
C    12      I     0        I      I HOMEP “‡‹A KOH–A  OMOTKˆ Tƒ.
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  R  - COPOTˆB‹EHˆE ŸKOPŸ;
C    14      I     2        I  FM  I  L  - ˆH„“KTˆBHOCTœ ŸKOPŸ;
C    15      I     3        I  FM  I  K” - KOHCTP“KTˆBH›‰ KO””ˆ–ˆEHT;
C    16      I     4        I  FM  I  JM - MOMEHT ˆHEP–ˆˆ;
C    17      I     5        I  FM  I  KTG- KO”ˆ–ˆEHT EPE„A—ˆ Tƒ.
C    18      I     6        I  FM  I  GTG- POBO„ˆMOCTœ OMOTKˆ Tƒ.
C    19      I     7        I  FM  I  TŸ - OCTOŸHHAŸ ŸKOPHO‰ –Eˆ;
C    20      I     8        I  FM  I  TM - OCTOŸHHAŸ MEXAHˆ—ECKAŸ;
C    21      I     9        I  DM  I  MBH- ‡HA—EHˆE MBH „‹Ÿ T=TN;
C    22      I    10        I  DM  I  MBH1-‡HA—EHˆE MBH „‹Ÿ T=TN1;
C    23      I    11        I  DM  I  GR-  POBO„ˆMOCTœ –Eˆ ŸKOPŸ;
C    24      I    12        I  DM  I  JR-  ˆCT. TOKA    –Eˆ ŸKOPŸ;
C    25      I    13        I  DM  I  GRG- POBO„ˆMOCTœ –Eœ ŸKOPŸ - Tƒ
C    26      I    14        I  DM  I  JRG- ˆCT. TOKA    –Eœ ŸKOPŸ - Tƒ
C    27      I    15        I  FM  I  NT - KO‹ˆ—ECTBO TO—EK MBH(T);
C    28      I    16        I  FM  I  T1 - BPEMŸ;                      ;
C    29      I    17        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 27 + 2*NT
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 15 + 2*NT
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       INTEGER BA0T, FGMSW, TIP
       EQUIVALENCE ( FGMSW,  MASPRT(87) )
       EQUIVALENCE ( N    ,  RN         )
       REAL  JM, L, KF, KTG
C--->—TO ‡A ‹EMEHT?
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP = TIP - 519
       GO TO (99999,99999),NTIP
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0031: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
99999  CONTINUE
C--->  HA‰„EM „‹ˆH“ MSW
       INT = 6
       IF( NTIP .EQ. 2 ) INT = 8
       NT = IFIX( PAR0T( INT ) )
       NT1= ( IFIX( PAR0T( INT-1 ) ) - 1 ) / 2
       IF ( NT .EQ. NT1  ) GO TO 750
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 700) TIP, NEL
700      FORMAT(' *** O˜ˆKA Pˆ ‡A„AHˆˆ KO‹-BA ‹EMEHTOB MACCˆBA ***'/
     =   ' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
         RETURN
750    CONTINUE
C-->  —TO POCŸT?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  “TO—HˆTœ MSW_ALL PA‡MEP
       LM = 24 + 2 * NT
       IF( NTIP .EQ. 2 ) LM = 27 + 2 * NT
       RETURN
C######################################################################
1000   CONTINUE
       IF( NTIP .EQ. 2 ) GO TO 3000
C######################################################################
C
C   „BˆƒATE‹œ OCTOŸHHOƒO TOKA ( Tˆ # 520 )
C
C######################################################################
C-->  ”OPMˆPOBATœ RMSW....
C-->  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW...
       LM = 14 + 2 * NT
C********************************************************************
C-> —ˆCTKA
       DO 1001 I=1,LM
       RMSW(I) = 0.0
1001   CONTINUE
C--------------------------------------------------------------------
C-->  PˆEM OCTOŸHH›X APAMETPOB...
C--------------------------------------------------------------------
          R   = PAR0T(1)
          L   = PAR0T(2)
          KF  = PAR0T(3)
          JM  = PAR0T(4)
C----------------------------------------------------------------------
C--->  ‡AO‹HEHˆE RMSW OCTOŸHH›Mˆ APAMETPAMˆ.
C----------------------------------------------------------------------
       RMSW (1) = R
       RMSW (2) = L
       RMSW (3) = KF
       RMSW (4) = JM
       RMSW (6) = L / R
       RMSW (7) = JM * R / KF**2
C----------------------------------------------------------------------
C--->  ‡AO‹HEHˆE RMSW MACCˆBOM.
C      ”“HK–ˆˆ MBH(T).
C----------------------------------------------------------------------
C      IPAR0 - C—ET—ˆK MACCˆBA PAR0T.
C--------------------------------------------------------------------
C
       RMSW ( 14 ) = 2 * FLOAT(NT)
       I1 = 15
       I2 = 14 + 2 * NT
       IPAR0 = 6
          DO 1100 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
1100      CONTINUE
       RETURN
C######################################################################
C
C   „BˆƒATE‹œ OCTOŸHHOƒO TOKA C TAXOƒEHEPATOPOM HA BA‹“ ( Tˆ # 521 )
C
C######################################################################
3000   CONTINUE
C-->  ”OPMˆPOBATœ RMSW....
C-->  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW...
       LM = 15 + 2 * NT
C********************************************************************
C-> —ˆCTKA
       DO 3001 I=1,LM
       RMSW(I) = 0.0
3001   CONTINUE
C--------------------------------------------------------------------
C-->  PˆEM OCTOŸHH›X APAMETPOB...
C--------------------------------------------------------------------
          R   = PAR0T(1)
          L   = PAR0T(2)
          KF  = PAR0T(3)
          JM  = PAR0T(4)
          KTG = PAR0T(5)
          RTG = PAR0T(6)
          IF( RTG .LT. 1.E-32 ) RTG = 1.E-32
C----------------------------------------------------------------------
C--->  ‡AO‹HEHˆE RMSW OCTOŸHH›Mˆ APAMETPAMˆ.
C----------------------------------------------------------------------
       RMSW (1) = R
       RMSW (2) = L
       RMSW (3) = KF
       RMSW (4) = JM
       RMSW (5) = KTG
       RMSW (6) = 1./RTG
       RMSW (7) = L / R
       RMSW (8) = JM * R / KF**2
C----------------------------------------------------------------------
C--->  ‡AO‹HEHˆE RMSW MACCˆBOM.
C      ”“HK–ˆˆ MBH(T).
C----------------------------------------------------------------------
C      IPAR0 - C—ET—ˆK MACCˆBA PAR0T.
C--------------------------------------------------------------------
C
       RMSW ( 15 ) = 2 * FLOAT(NT)
       I1 = 16
       I2 = 15 + 2 * NT
       IPAR0 = 8
          DO 3100 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
3100      CONTINUE
       RETURN
C----------------------------------------------------------------------
       END
