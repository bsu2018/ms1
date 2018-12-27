       SUBROUTINE LM0003( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: LM0003( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹ˆ TˆA # 64
C   ----------     ***  .„.C. PEƒ“‹ŸTOPA BO‡“†„EHˆŸ 3X3X0 ******
C                  * * * * * * * * * * * * * * * * * * * * * * * *
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  64      3-X ”A‡HAŸ .„.C        E_PEƒ
C                          PEƒ“‹ˆP“EMAŸ OT "PB"
C
C
C              ‹EMEHT KO„ˆP“ETCŸ:
C              -------------------
C
C               # T 64 “ K0 KA KB KC  T_BK‹  R_BH“T #_‹-TA  K_“Cˆ‹;
C
C     K0 - O™ˆ‰ “‡E‹, KA - ”A‡A A, KB - ”A‡A B, KC - ”A‡A C;
C
C---> B›BO„H›E ‡HA—EHˆŸ: TOKˆ: K “‡‹AM KA, KB, KC, OT “‡‹A K0,
C                              *                   **
C                      ‡HA—EHˆŸ .„.C. E1, E2, E3;
C
C
C        ˆ‡ TOKN ‹EMEHTA #_‹-TA B›ˆPAETCŸ 3-ˆ EPB›X
C           ‡HA—EHˆ‰, “MHO†AžTCŸ HA K_“Cˆ‹. ˆ PˆHˆMAžTCŸ
C           ‡A ‡HA—EHˆE .„.C. 3-X ”A‡HO‰ CˆCTEM› HAPŸ†EHˆŸ;
C
C  ===  „O“CKAETCŸ -->  MO„E‹ˆ Tˆ # 40, # 13, # 63
C
C !!!!   ‡A„EP†KA HA O„ˆH ˜Aƒ ˆHTEƒPˆPOBAHˆŸ !!!
C        -----------------------------------
C
C  Pˆ TN , T_BK‹ --> “—ˆT›BAžTCŸ TO‹œKO O„Hˆ COPOTˆB‹EHˆŸ R_BH“T;
C
C
C   METO„:        OˆCK B MSW_ALL OˆCAHˆˆ ‹EMEHTA *‹. MA˜.*
C   -----         ( „O. Tˆ COƒ‹ACHO BCTPOEHHO‰ TA. "LTIP" )
C                 B C‹“—AE HAXO†„EHˆŸ -> „O”OPMˆPOBAHˆE MSW_OˆCAHˆŸ;
C                 B TOM C‹“—AE KOD = 0, ˆHA—E = 1;
C   APAAMETP›:   MSW(RMSW) - "OTPE‡OK" MSW_MO„E‹ˆ Tˆ # 40;
C   ----------    ADR - A„PEC B MSW_ALL MSW_OˆCAHˆŸ TO‰ MO„E‹ˆ;
C                 MSWALL(RMSWAL) - BECœ CˆCOK MSW C‹OB;
C                 KOD - = 0 - C”OPMˆPOBA‹ˆ MSW,
C                       = 1 - ‹Oƒ. O˜ˆKˆ;
C                      ( HA NWTR COO™EHˆE B›BO„ˆTCŸ );
C   O„POƒPAMM›: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C   PˆME—AHˆE:  ”OPMAT› MSW_OˆCAHˆ‰ ---->
C   ----------
C
C
C                  RMSW TˆA # 64 [ ˆCTO—HˆK HAPŸ†EHˆŸ OT "PEƒ.BO‡" ]
C                  -----------------------------------------
C
C                    TOK(J) = K_“C. * E(J) / R_BH“T. + 1/R * U_N+1
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II ‡HA—.  I ‡HA—.I # ‹-TAI A„P. II      IJESIJESI K   I
C   “‡‹›:     II  1/R   I T_BK‹I "PB"   I  B   II JESA I   I   I “Cˆ‹I
C  K0         II        I      I        I TOKN II      I B I C I EHˆŸI
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 40, 13, 63, 7*0/
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ‹-TA "PEƒ. BO‡“†„EHˆŸ"
       NELMOT = MSW(15)
C-->
       NEL = MSW(1)
       TIP = MSW(2)
C-->
C-->  ˆCKATœ B MSWALL ‹EMEHT C #‹-TA = NELMOT:
       CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, KVIV, ATOKM )
C-->
       IF   ( IRC .EQ. 1 ) GO TO 1
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
C-->  MSW - HA˜‹ˆ, —TO ‡A Tˆ TAM †ˆBET ?
       TIPX = MSWALL(AMOT + 1 )
       DO 2 I = 1, K10
       IF ( TIPX .EQ. LTAB(I) ) GO TO 3
2      CONTINUE
C-->  CBŸ‡œ C HE HA˜ˆM TˆOM.....
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELMOT, TIPX
101    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     = ' ( Tˆ # ', I3, ' ) ***'//
     = ' *** C ‹EMEHTOM # ', I5, ' ( Tˆ # ', I3, ' ) - O˜ˆO—EH',
     = ' ˆ‹ˆ HE PEA‹ˆ‡OBAH ***'/
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
3      CONTINUE
C-->  HA—A‹O OˆCAHˆŸ B MSW --->
C              = AMOT
C-->  HA—A‹O CˆCKA “‡‹OB „‹Ÿ #_TA
C              = AMOT+8
C-->  A„PEC B TOKN/TOKN1 „‹Ÿ EPEMEHH›X COCTOŸHˆŸ PEƒ“‹ŸTOPA
       MSW(16) = ATOKM
       KOD = 0
       RETURN
       END
