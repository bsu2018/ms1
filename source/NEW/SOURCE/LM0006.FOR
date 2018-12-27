       SUBROUTINE LM0006( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAŸ   1987 ƒ.)------------
C   O„POƒPAMMA: LM0006( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹E‰ TˆOB:
C   ----------  # 301 - CˆCTEMA ˆM“‹œCHO-”A‡OBOƒO “PAB‹EHˆŸ ( Cˆ”“ );
C               #  17 - ˆCTO—HˆK TOKA, “PAB‹ŸEM›‰ / O‹œ‡OBATE‹Ÿ;
C               # 323 - HE‹ˆHE‰HOE „ˆHAMˆ—ECKOE ‡BEHO ( “MHO†ˆTE‹œ).
C               # 330 - –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆ"
C               # 331 - –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆ‹ˆ"
C               # 332 - –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆCK‹. ˆ‹ˆ"
C
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
C              ”OPMAT RMSW Tˆ # 301,17,323,330,331,332:
C              ----------------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW      I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I              I      I
C------------I--------------I------I-----------------------------------
C .........  I ...........  I .... I .................................
C .........  I ...........  I .... I .................................
C    11      I     2        I  FM  I  NTOKOB -KO‹ˆ—ECTBO TOKOB, ˆCO‹œ-
C            I              I      I          ‡“EM›X Pˆ PAC—ETE Cˆ“;
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C  9+I*3     I     I*3      I  FM  I  #‹I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C .......    I   .......    I  ... I  ..........................
C .......    I   .......    I  ... I  ..........................
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
       NTOKOB = MSW(11)
       IF( NTOKOB .EQ. 0 ) RETURN
C----------------------------------------------------------------------
C--> „‹Ÿ KA†„OƒO ‹EMEHTA ˆ™EM...
C----------------------------------------------------------------------
          DO 15 I= 1 , NTOKOB
          NELMOT = MSW(9 + 3*I)
          NSM = MSW(10 + 3*I)
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
     = ' *** C ‹EMEHTOM # ', I5, ' ( CME™EHˆE',I3, ' ) - HE BEPEH',
     = ' ‹EMEHT HE ˆMEET TPE“EMOƒO TOKA ***'/
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
3      CONTINUE
          MSW(11+3*I) = ATOKM + NSM - 1
15     CONTINUE
C----------------------------------------------------------------------
C--> ‡AKOH—ˆ‹ˆ!
       KOD = 0
       RETURN
       END
