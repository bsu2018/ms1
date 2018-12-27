       SUBROUTINE LM0009( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAPTA 1988 ƒ.)------------
C   O„POƒPAMMA: LM0009( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹ˆ TˆA:
C   ----------  #  18 - TPEX”A‡H›‰  ˆCTO—HˆK TOKA,
C                       “PAB‹ŸEM›‰ / O‹œ‡OBATE‹Ÿ;
C               *******************************************************
C
C   METO„:        OˆCK B MSW_ALL OˆCAHˆˆ ‹EMEHTOB , A„PECOB ‡A„AHH›X
C   -----         TOKOB ‹EMEHTOB B MACCˆAX TOKN/TOKN1.
C                 B C‹“—AE HAXO†„EHˆŸ -> „O”OPMˆPOBAHˆE MSW_OˆCAHˆŸ;
C                 B TOM C‹“—AE KOD = 0, ˆHA—E = 1;
C
C   APAMETP›:    MSW(RMSW) - "OTPE‡OK" MSW_MO„E‹ˆ Tˆ # 301;
C   ----------    ADR - A„PEC B MSW_ALL MSW_OˆCAHˆŸ TO‰ MO„E‹ˆ;
C                 MSWALL(RMSWAL) - BECœ CˆCOK MSW C‹OB;
C                 KOD - = 0 - C”OPMˆPOBA‹ˆ MSW,
C                       = 1 - ‹Oƒ. O˜ˆKˆ;
C                      ( HA NWTR COO™EHˆE B›BO„ˆTCŸ );
C   O„POƒPAMM›: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C              ”OPMAT RMSW Tˆ # 18:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0029    I      I
C------------I--------------I------I-----------------------------------
C     9      I     A        I      I HOMEP “‡‹A A
C    10      I     B        I      I HOMEP “‡‹A B
C    11      I     C        I      I HOMEP “‡‹A C
C    12      I     0        I      I HOMEP “‡‹A 0
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  NTOKOB -KO‹ˆ—ECTBO TOKOB, ˆCO‹œ-
C            I              I      I          ‡“EM›X Pˆ PAC—ETE TOKA
C            I              I      I          ˆCTO—HˆKA;
C    14      I     2        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    15      I     3        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    16      I     4        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C 11+I*3     I  -1+I*3      I  FM  I  #‹I -      -"-
C 12+I*3     I     I*3      I  FM  I  #CMI -      -"-
C 13+I*3     I   1+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C14+NTOKOB*3 I  2+NTOKOB*3  I  FM  I  NHAP -KO‹ˆ—ECTBO HAPŸ†EHˆ‰,
C            I              I      I         ˆC. „‹Ÿ PAC—ETA TOKA
C            I              I      I         ˆCTO—HˆˆKA;
C15+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  “‡E‹N1 -“‡E‹ HA—A‹A HAPŸ†EHˆŸ;
C16+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  “‡E‹K1 -“‡E‹ KOH–A  HAPŸ†EHˆŸ;
C .........  I ............ I .... I  ...........................
C            I              I      I
C15+NTOKOB*3+I 2+NTOKOB*3+  I      I  NPSUB - HOMEP / O‹œ‡OBATE‹Ÿ
C NHAP*2    I  NHAP*2     I  FM  I         ( PSUB) B KOTOPO‰ POˆ‡BO-
C            I              I      I         „ˆTCŸ PAC—ET TOKA ˆCTO—HˆKA
C (...)+1    I  (...)+1     I  FM  I  NPAR - KO‹ˆ—ECTBO APAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  APAMETP› PSUB , ‡A„AHH›E
C„O(...)+NPARI „O(...)+NPAR I      I  O‹œ‡OBATE‹EM.
C            I              I      I
C 17+3*NTOKOBI 5 + 3*NTOKOB I  DM  I  TOKTA - ‡HA—EHˆE TOKA ˆCTO—HˆKA
C +2*NNAPR + I + 2*NNAPR +  I      I          ”A‡›   A    Pˆ  T=TN+1
C NPAR       I NPAR         I      I
C
C 18+3*NTOKOBI 6 + 3*NTOKOB I  DM  I  TOKTA - ‡HA—EHˆE TOKA ˆCTO—HˆKA
C +2*NNAPR + I + 2*NNAPR +  I      I          ”A‡›   B    Pˆ  T=TN+1
C NPAR       I NPAR         I      I
C
C 19+3*NTOKOBI 7 + 3*NTOKOB I  DM  I  TOKTA - ‡HA—EHˆE TOKA ˆCTO—HˆKA
C +2*NNAPR + I + 2*NNAPR +  I      I          ”A‡›   C    Pˆ  T=TN+1
C NPAR       I NPAR         I      I
C
C 20+3*NTOKOBI 8 + 3*NTOKOB I  DM  I  TOKTA - ‡HA—EHˆE TOKA ˆCTO—HˆKA
C +2*NNAPR + I + 2*NNAPR +  I      I          ”A‡›   0    Pˆ  T=TN+1
C NPAR       I NPAR         I      I
C
C 21+3*NTOKOBI 9 + 3*NTOKOB I  DM  I  G12 - ‡HA—EHˆE POBO„ˆMOCTˆ
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 22+3*NTOKOBI10 + 3*NTOKOB I  DM  I  G13 - ‡HA—EHˆE POBO„ˆMOCTˆ
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 23+3*NTOKOBI11 + 3*NTOKOB I  DM  I  G23 - ‡HA—EHˆE POBO„ˆMOCTˆ
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 24+3*NTOKOBI12 + 3*NTOKOB I  DM  I  G14 - ‡HA—EHˆE POBO„ˆMOCTˆ
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 25+3*NTOKOBI13 + 3*NTOKOB I  DM  I  G24 - ‡HA—EHˆE POBO„ˆMOCTˆ
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 26+3*NTOKOBI14 + 3*NTOKOB I  DM  I  G34 - ‡HA—EHˆE POBO„ˆMOCTˆ
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C
C 27+3*NTOKOBI15 + 3*NTOKOB I  DM  I  J12 - ‡HA—EHˆE ˆCTO—HˆKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 28+3*NTOKOBI16 + 3*NTOKOB I  DM  I  J13 - ‡HA—EHˆE ˆCTO—HˆKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 29+3*NTOKOBI17 + 3*NTOKOB I  DM  I  J23 - ‡HA—EHˆE ˆCTO—HˆKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 30+3*NTOKOBI18 + 3*NTOKOB I  DM  I  J14 - ‡HA—EHˆE ˆCTO—HˆKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 31+3*NTOKOBI19 + 3*NTOKOB I  DM  I  J24 - ‡HA—EHˆE ˆCTO—HˆKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C 32+3*NTOKOBI20 + 3*NTOKOB I  DM  I  J34 - ‡HA—EHˆE ˆCTO—HˆKA TOKA
C +2*NNAPR + I + 2*NNAPR +  I      I         Pˆ T=TN+1
C NPAR       I NPAR         I      I
C
C 33+3*NTOKOBI21 + 3*NTOKOB I  DM  I  NMOD - Pˆ‡HAK MO„E‹ˆ
C +2*NNAPR + I + 2*NNAPR +  I      I     0 - ‹ˆHE‰HAŸ
C NPAR       I NPAR         I      I   > 0 - HE‹ˆHE‰HAŸ
C            I              I      I
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 33 + 3*NTOKOB + 2*NNAPR + NPAR
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 21 + 3*NTOKOB + 2*NNAPR + NPAR
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM
C----------------------------------------------------------------------
C-->  HA—A‹ˆ...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
C----------------------------------------------------------------------
C--> CKO‹œKO ‹EMEHTOB?
C----------------------------------------------------------------------
       NTOKOB = MSW(13)
       IF( NTOKOB .EQ. 0 ) RETURN
C----------------------------------------------------------------------
C--> „‹Ÿ KA†„OƒO ‹EMEHTA ˆ™EM...
C----------------------------------------------------------------------
          DO 15 I= 1 , NTOKOB
          NELMOT = MSW(11 + 3*I)
          NSM = MSW(12 + 3*I)
C----------------------------------------------------------------------
C-->  ˆCKATœ B MSWALL ‹EMEHT C #‹-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HA˜‹ˆ ˆ‹ˆ HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C-->  ‹EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 100) NEL, TIP, NELMOT
100    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     =  ' ( Tˆ # ', I3, ' ) ***'/
     =  ' *** C HE C“™ECTB“ž™ˆM B –Eˆ ‹EMEHTOM # ', I5,' ***'/
     =  ' *** OPAOTKA –Eˆ PEKPA™AETCŸ *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA˜‹ˆ, ECTœ ‹ˆ “ ‹EMEHTA TPE“EM›‰ TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 3
2      CONTINUE
C----------------------------------------------------------------------
C-->  CBŸ‡œ C TOKOM , KOTOPOƒO HE C“™ECTB“ET “ ‡A„AHHOƒO ‹EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELMOT, NSM
101    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     = ' ( Tˆ # ', I3, ' ) ***'//
     = ' *** C ‹EMEHTOM # ', I5, ' ( CME™EHˆE',I3, ' ) - O˜ˆO—EH',
     = ' ‹EMEHT HE ˆMEET TPE“EMOƒO TOKA ***'/
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
3      CONTINUE
          MSW(13+3*I) = ATOKM + NSM - 1
15     CONTINUE
C----------------------------------------------------------------------
C--> ‡AKOH—ˆ‹ˆ!
       KOD = 0
       RETURN
       END
