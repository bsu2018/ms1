       SUBROUTINE LM0008( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAŸ   1987 ƒ.)------------
C   O„POƒPAMMA: LM0008( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹E‰ TˆOB
C   ----------             333,334,335,360:
C              +-----+-----------------------------------------I
C              I     I                                         I
C              I Tˆ I        HAˆMEHOBAHˆE ‹EMEHTA            I
C              I     I                                         I
C              +-----+-----------------------------------------+
C              I 333 I  –ˆ”POBO‰  RS-TPˆƒƒEP                   I
C              +-----+-----------------------------------------+
C              I 334 I  –ˆ”POBO‰   D-TPˆƒƒEP                   I
C              +-----+-----------------------------------------+
C              I 335 I  –ˆ”POBO‰   T-TPˆƒƒEP                   I
C              +-----+-----------------------------------------+
C              I 360 I  KOMAPATOP(1)                          I
C              +-----+-----------------------------------------+
C
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
C
C              ”OPMAT RMSW Tˆ # 33X ˆ 360:
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0026    I      I
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
C
C
C              ”OPMAT RMSW Tˆ # 335:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0026    I      I
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
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
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
C---> –ˆ”POBO‰ RS-TPˆƒƒEP                     ( Tˆ  # 333 )
C---> –ˆ”POBO‰  D-TPˆƒƒEP                     ( Tˆ  # 334 )
C---> –ˆ”POBO‰  T-TPˆƒƒEP                     ( Tˆ  # 335 )
C---> KOMAPATOP                              ( Tˆ  # 360 )
C----------------------------------------------------------------------
          NOMTOK = 1
          NELMOT = MSW(12)
          NSM = MSW(13)
          ADRMSW = 14
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
C--->   ˆ™EM C‹E„“ž™ˆ‰ TOK
          NOMTOK = NOMTOK + 1
          NELMOT = MSW( 15 )
          NSM    = MSW( 16 )
          ADRMSW   = 17
          IF( NOMTOK .EQ. 2  .AND.  TIP .NE. 335  ) GO TO 1
C----------------------------------------------------------------------
C--> ‡AKOH—ˆ‹ˆ!
C----------------------------------------------------------------------
C
       KOD = 0
       RETURN
       END
