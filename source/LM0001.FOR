       SUBROUTINE LM0001( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: LM0001( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹ˆ TˆA # 40
C   ----------     ***  PEƒ“‹ŸTOP BO‡“†„EHˆŸ 3X3X0 ******
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
C              ”OPMAT RMSW Tˆ # 40:
C              ---------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I       OO‡HA—EHˆE - ‡HA—EHˆE
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I
C OˆCAHˆŸ   I    FM0010    I      I
C            I              I      I
C------------I--------------I------I-----------------------------------
C     9      I    -2        I      I HOMEP “‡‹A KA ( ”A‡A A )
C    10      I    -1        I      I HOMEP “‡‹A KB ( ”A‡A B )
C    11      I     0        I      I HOMEP “‡‹A KC ( ”A‡A C )
C----------------------------------------------------------------------
C    12      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4
C    13      I     2        I  FM  I  NELT - HOMEP ‹. PEƒ“‹ˆP. ‹. MA˜.
C    14      I     3        I  LM  I  AMOT - A„P. B MSWALL OˆCAHˆŸ
C            I              I      I         PEƒ. ‹. MA˜ˆH›
C    15      I     4        I  LM  I  APOT - A„PEC B MSWALL “‡‹OB MA˜ˆH›
C    16      I     5        I  LM  I  ATOKM - A„P B TOKN/N1 BE‹ˆ—ˆH,
C            I              I      I          PAC—ˆT. „‹Ÿ ‹. MA˜.
C    17      I     6        I  FM  I  TBKL - BPEMŸ, HA—ˆHAŸ C KOTOPOƒO
C            I              I      I         BK‹ž—AETCŸ PEƒ“‹ŸTOP;
C    18,19   I     7,8      I      I  PE‡EPB.
C    20      I     9        I  FM  I  UPOT - OTO‹O—HOE HAPŸ†EHˆE
C    21      I     10       I  FM  I  NSUB - HOMEP / O‹œ‡OBATE‹Ÿ
C22,23,24,25 I11,12,13,14,15I      I  PA. MECTO „‹Ÿ ˆHTEƒPATOPOB;
C  27        I    16        I  FM  I  NPAR - KO‹-BO APAMETPOB,
C            I              I      I         BBE„EHH›X B PSUB<..>
C OT 28 „O   I OT 17 „O     I FM   I  APAMETP› PSUB, ‡A„AHH›E
C 28 + NPAR  I 17 + NPAR    I      I  O‹œ‡OBATE‹EM
C            I              I      I
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 28+NPAR
C
C
C
C    „ ‹ Ÿ    ‹.  MA˜ˆH›:
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 11, 110, 12, 120:
C                ----------------------------------------------
C    + 14  ˆ 140
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. IHA—.   I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C  +--------------------------------------------------------------+
C  I “‡‹› O„K‹ž—EHˆŸ S1N S2N S3N F1N F2N F3N S1K S2K F1K F2K F3K I
C  I           12-Tœ C‹OB: OT 9 „O 20                             I
C  +--------------------------------------------------------------+
C
C              RMSW Tˆ # 11, 110, 12, 120 [ ‹. MA˜ˆHA 3X3X0 ] :
C              ************************************************
C
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PE‡EPB I"DOM"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 5-Tœ   I „‹Ÿ I      I      I       I       I       I       I     I
C  I  C‹OB  IMEX. I 9-Tœ I 9-Tœ I 36-Tœ I 36-Tœ I 6-Tœ  I 6-Tœ  I NMASI
C  I        I“P-Ÿ I C‹OB I C‹OB I C‹OB  I C‹OB  I C‹OB  I C‹OB  I C‹OBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I        I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - OPE„E‹ŸETCŸ PA‡MEPOM MAS <...> ==> ...+ NMAS + 1
C
C      INTEGER D1,D2,D3,D4,D5,D6,D7,D8
C
C
C      DATA D1, D2, D3, D4, D5, D6,  D7,  D8
C    =  /   21, 27, 36, 45, 81, 117, 123, 129 /
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 11, 110, 12, 120, 14, 140, 4*0 /
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ‹-TA ‹. MA˜ˆH›:
       NELMOT = MSW(13)
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
C-->  HA—A‹O OˆCAHˆŸ MA˜ˆH›--->
       MSW(14) = AMOT
C-->  HA—A‹O CˆCKA “‡‹OB MA˜ˆH›-->
       MSW(15) = AMOT+8
C-->  A„PEC B TOKN/TOKN1 „‹Ÿ EPEMEHH›X COCTOŸHˆŸ MA˜ˆH›-->
       MSW(16) = ATOKM
       KOD = 0
       RETURN
       END
