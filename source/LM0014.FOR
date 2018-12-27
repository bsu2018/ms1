       SUBROUTINE LM0014( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (AT OCTOBER 1989 ƒ.)------------
C   O„POƒPAMMA: LM0014( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹Ÿ TˆA
C   ----------             800
C              +-----+-----------------------------------------I
C              I     I                                         I
C              I Tˆ I        HAˆMEHOBAHˆE ‹EMEHTA            I
C              I     I                                         I
C              +-----+-----------------------------------------+
C              I 800 I  œE‡OKOMEHCATOP                       I
C              +-----+-----------------------------------------+
C
C
C   METO„:        OˆCK B MSW_ALL OˆCAHˆˆ ‹EMEHTOB , A„PECOB ‡A„AHH›X
C   -----         TOKOB ‹EMEHTOB B MACCˆBAX TOKN/TOKN1.
C                 B C‹“—AE HAXO†„EHˆŸ -> „O”OPMˆPOBAHˆE MSW_OˆCAHˆŸ;
C                 B TOM C‹“—AE KOD = 0, ˆHA—E = 1;
C
C   APAMETP›:    MSW(RMSW) - "OTPE‡OK" MSW_MO„E‹ˆ Tˆ # 800;
C   ----------    ADR - A„PEC B MSW_ALL MSW_OˆCAHˆŸ TO‰ MO„E‹ˆ;
C                 MSWALL(RMSWAL) - BECœ CˆCOK MSW C‹OB;
C                 KOD - = 0 - C”OPMˆPOBA‹ˆ MSW,
C                       = 1 - ‹Oƒ. O˜ˆKˆ;
C                      ( HA NWTR COO™EHˆE B›BO„ˆTCŸ );
C   O„POƒPAMM›: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C                   ”OPMAT RMSW Tˆ # 800:
C                    "OCTOŸHHAŸ" —ACTœ MSW
C                     ----------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I‡HA—.  I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I        I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW TˆA  # 800
C                  --------------------
C
C
C  OCT.  I EPEMEHHAŸ RMSW-->
C +----------+----+---+----+---+----+---+------+------+---+---+---+...>
C I “‡‹›I    I    I   I RI I   I    I   I      I      I   I   I   I...>
C I     I G1 I #‹I#CMI    I GLI GC I KOI  KJP I G(20)IG11IG12IG22I...>
C I     I    I    I   I    I   I    I   I      I      I   I   I   I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I   3 I 1  I 1  I 1 I  1 I 1 I  1 I 1 I  1   I  20  I 1 I 1 I 1 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I 9-11I 12 I 13 I 14I 15 I16 I 17 I18 I 19   I 20-39I 40I41 I42 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I     I 1  I 2  I 3 I 4  I 5 I 6  I 7 I 8    I 9-28 I29 I30 I 31I
C
C
C
C ...>+---------+--------+------+
C ...>I G MAT-  I PE‡EPB I      I
C ...>I   Pˆ–A  I        I ATOKMI
C ...>I BCOMOƒ.I        I      I
C ...>+---------+--------+------+
C ...>I   16    I   1    I   1  I
C ...>+---------+--------+------+
C ...>I 43 - 58 I  59    I  60  I
C ...>+---------+--------+------+
C     I 32 - 47 I  48    I  49  I
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 60
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 49
C
C
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
C---> œE‡OKOMEHCATOP            ( Tˆ  # 800 )
C----------------------------------------------------------------------
          NELMOT = MSW(13)
          NSM = MSW(14)
          ADRMSW = 60
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