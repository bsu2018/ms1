       SUBROUTINE LM0013( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT  ˆžHŸ   1989 ƒ.)------------
C   O„POƒPAMMA: LM0013( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹Ÿ TˆA
C   ----------             380
C              +-----+-----------------------------------------I
C              I     I                                         I
C              I Tˆ I        HAˆMEHOBAHˆE ‹EMEHTA            I
C              I     I                                         I
C              +-----+-----------------------------------------+
C              I 380 I  MˆKPOPO–ECOP  K1801BM1(2,3)           I
C              +-----+-----------------------------------------+
C
C
C   METO„:        OˆCK B MSW_ALL OˆCAHˆˆ ‹EMEHTOB , A„PECOB ‡A„AHH›X
C   -----         TOKOB ‹EMEHTOB B MACCˆBAX TOKN/TOKN1.
C                 B C‹“—AE HAXO†„EHˆŸ -> „O”OPMˆPOBAHˆE MSW_OˆCAHˆŸ;
C                 B TOM C‹“—AE KOD = 0, ˆHA—E = 1;
C
C   APAMETP›:    MSW(RMSW) - "OTPE‡OK" MSW_MO„E‹ˆ Tˆ # 380;
C   ----------    ADR - A„PEC B MSW_ALL MSW_OˆCAHˆŸ TO‰ MO„E‹ˆ;
C                 MSWALL(RMSWAL) - BECœ CˆCOK MSW C‹OB;
C                 KOD - = 0 - C”OPMˆPOBA‹ˆ MSW,
C                       = 1 - ‹Oƒ. O˜ˆKˆ;
C                      ( HA NWTR COO™EHˆE B›BO„ˆTCŸ );
C   O„POƒPAMM›: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C                   ”OPMAT RMSW Tˆ # 380:
C                  -----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0032    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I TX - BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ;
C    11      I     2        I  DM  I KX - “KA‡ATE‹œ ˆ‡MEHEHˆŸ COCTOŸHˆŸ
C            I              I      I      OPTA  B/B HA ˜AƒE ;
C    12      I     3        I  FM  I TTAKT - EPˆO„ TAKTOBO‰ —ACTOT› ;
C    13      I     4        I  FM  I P - ”‹Aƒ OT‹A„O—HO‰ E—ATˆ:
C            I              I      I     P=0 - OT‹A„O—HAŸ E—ATœ OTK‹.
C            I              I      I     P=1 - OT‹A„O—HAŸ E—ATœ BK‹.
C    14      I     5        I  FM  I           PE‡EPB
C    15      I     6        I  FM  I           PE‡EPB
C    16      I     7        I  DM  I KX - “KA‡ATE‹œ ˆ‡MEHEHˆŸ COCTOŸHˆŸ
C            I              I      I      OPTA B/B HA ˜AƒE ;
C    17      I     8        I  DM  I TT - TEK“™EE BPEMŸ ;
C    18      I     9        I  DM  I R0 - CO„EP†ˆMOE POH R0 Pˆ T=TT ;
C    19      I    10        I  DM  I R1 - CO„EP†ˆMOE POH R1 Pˆ T=TT ;
C    20      I    11        I  DM  I R2 - CO„EP†ˆMOE POH R2 Pˆ T=TT ;
C    21      I    12        I  DM  I R3 - CO„EP†ˆMOE POH R3 Pˆ T=TT ;
C    22      I    13        I  DM  I R4 - CO„EP†ˆMOE POH R4 Pˆ T=TT ;
C    23      I    14        I  DM  I R5 - CO„EP†ˆMOE POH R5 Pˆ T=TT ;
C    24      I    15        I  DM  I R6 - CO„EP†ˆMOE POH R6 Pˆ T=TT ;
C    25      I    16        I  DM  I R7 - CO„EP†ˆMOE POH R7 Pˆ T=TT ;
C    26      I    17        I  DM  I FLC-‡HA—EHˆE ”‹AƒA "C" CC Pˆ T=TT
C    27      I    18        I  DM  I FLV-‡HA—EHˆE ”‹AƒA "V" CC Pˆ T=TT
C    28      I    19        I  DM  I FLZ-‡HA—EHˆE ”‹AƒA "Z" CC Pˆ T=TT
C    29      I    20        I  DM  I FLN-‡HA—EHˆE ”‹AƒA "N" CC Pˆ T=TT
C    30      I    21        I  DM  I FLT-‡HA—EHˆE ”‹AƒA "T" CC Pˆ T=TT
C    31      I    22        I  DM  I FLIO-‡HA—EHˆE ”‹AƒA "I/O" CC
C            I              I      I             Pˆ T=TT ;
C    32      I    23        I  DM  I IPORT  -CO„EP†ˆMOE OPTA B›BO„A
C    33      I    24        I  DM  I IPORT1 -CO„EP†ˆMOE OPTA BBO„A
C            I              I      I             Pˆ T=TT ;
C    34      I    25        I  FM  I ADRPROG-MAKCˆMA‹œH›‰ A„PEC O‡“ ;
C    35      I    26        I  FM  I ADRPRTO-A„PEC OPTA B›BO„A;
C    36      I    27        I  FM  I ADRPRTI-A„PEC OPTA BBO„A ;
C    37      I    28        I  DM  I TO - CTAPOE BPEMŸ ;
C    38      I    29        I  DM  I R0 - CO„EP†ˆMOE POH R0 Pˆ T=TO ;
C    39      I    30        I  DM  I R1 - CO„EP†ˆMOE POH R1 Pˆ T=TO ;
C    40      I    31        I  DM  I R2 - CO„EP†ˆMOE POH R2 Pˆ T=TO ;
C    41      I    32        I  DM  I R3 - CO„EP†ˆMOE POH R3 Pˆ T=TO ;
C    42      I    33        I  DM  I R4 - CO„EP†ˆMOE POH R4 Pˆ T=TO ;
C    43      I    34        I  DM  I R5 - CO„EP†ˆMOE POH R5 Pˆ T=TO ;
C    44      I    35        I  DM  I R6 - CO„EP†ˆMOE POH R6 Pˆ T=TO ;
C    45      I    36        I  DM  I R7 - CO„EP†ˆMOE POH R7 Pˆ T=TO ;
C    46      I    37        I  DM  I FLC-‡HA—EHˆE ”‹AƒA "C" CC Pˆ T=TO
C    47      I    38        I  DM  I FLV-‡HA—EHˆE ”‹AƒA "V" CC Pˆ T=TO
C    48      I    39        I  DM  I FLZ-‡HA—EHˆE ”‹AƒA "Z" CC Pˆ T=TO
C    49      I    40        I  DM  I FLN-‡HA—EHˆE ”‹AƒA "N" CC Pˆ T=TO
C    50      I    41        I  DM  I FLT-‡HA—EHˆE ”‹AƒA "T" CC Pˆ T=TO
C    51      I    42        I  DM  I FLIO-‡HA—EHˆE ”‹AƒA "I/O" CC
C            I              I      I             Pˆ T=TO ;
C    52      I    43        I  DM  I IPORT  -CO„EP†ˆMOE OPTA B›BO„A
C    53      I    44        I  DM  I IPORT1 -CO„EP†ˆMOE OPTA BBO„A
C            I              I      I             Pˆ T=TO ;
C    54      I    45        I  FM  I  #‹ - HOMEP ‹EMEHTA;
C    55      I    46        I  FM  I  #CM - HOMEP TOKA ‹EMEHTA #‹ ;
C    56      I    47        I  LM  I  ATOKM - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEPA #CM ;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 56
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 47
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM, ADRMSW
C----------------------------------------------------------------------
C-->  HA—A‹ˆ...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
C----------------------------------------------------------------------
100   CONTINUE
C======================================================================
C----------------------------------------------------------------------
C---> MˆKPOPO–ECCOP K1801BM1(2,3)            ( Tˆ  # 380 )
C----------------------------------------------------------------------
          NELMOT = MSW(54)
          NSM = MSW(55)
          ADRMSW = 56
1         CONTINUE
C----------------------------------------------------------------------
C-->  ˆCKATœ B MSWALL ‹EMEHT C #‹-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HA˜‹ˆ ˆ‹ˆ HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 4
C----------------------------------------------------------------------
C-->  ‹EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 500) NEL, TIP, NELMOT
500    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     =  ' ( Tˆ # ', I3, ' ) ***'/
     =  ' *** C HE C“™ECTB“ž™ˆM B –Eˆ ‹EMEHTOM # ', I5,' ***'/
     =  ' *** OPAOTKA –Eˆ PEKPA™AETCŸ *** '//)
       RETURN
C----------------------------------------------------------------------
4      CONTINUE
C-->  MSW - HA˜‹ˆ, ECTœ ‹ˆ “ ‹EMEHTA TPE“EM›‰ TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 6
C----------------------------------------------------------------------
C-->  CBŸ‡œ C TOKOM , KOTOPOƒO HE C“™ECTB“ET “ ‡A„AHHOƒO ‹EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,501) NEL, TIP, NELMOT, NSM
C----------------------------------------------------------------------
501    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     = ' ( Tˆ # ', I3, ' ) ***'//
     = ' *** C ‹EMEHTOM # ', I5, ' ( CME™EHˆE',I3, ' ) - O˜ˆO—EH',
     = ' ‹EMEHT HE ˆMEET TPE“EMOƒO TOKA ***'/
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
6      CONTINUE
          MSW(ADRMSW) = ATOKM + NSM - 1
C----------------------------------------------------------------------
C--> ‡AKOH—ˆ‹ˆ!
C----------------------------------------------------------------------
C
       KOD = 0
       RETURN
       END
