       SUBROUTINE LM0004( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: LM0004( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹E‰ TˆA #93 ˆ #668
C   ----------     ***  BEHTˆ‹œ(K‹ž—),“PAB‹ŸEM›‰ Cˆ”“  ***
C   METO„:        OˆCK B MSW_ALL OˆCAHˆˆ “PAB‹Ÿž™EƒO ‹EMEHTA ,
C   -----         B C‹“—AE HAXO†„EHˆŸ -> „O”OPMˆPOBAHˆE MSW_OˆCAHˆŸ;
C                 B TOM C‹“—AE KOD = 0, ˆHA—E KOD = 1;
C   APAMETP›:    MSW(RMSW) - "OTPE‡OK" MSW_MO„E‹ˆ Tˆ # 93 ˆ‹ˆ # 668
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
C              ”OPMAT RMSW Tˆ # 93 ˆ # 668
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I       OO‡HA—EHˆE - ‡HA—EHˆE
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I
C OˆCAHˆŸ   I    FM0016    I      I
C            I              I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I  FM  I HOMEP “‡‹A A ( AHO„ )
C    10      I     0        I  FM  I HOMEP “‡‹A K ( KATO„ )
C----------------------------------------------------------------------
C    11      I     1        I  FM  I NELT  -HOMEP ‹EMEHTA, “PAB‹.
C            I              I      I        BEHTˆ‹EM ( Cˆ”“ );
C    12      I     2        I  FM  I NSM   -OPŸ„KOB›‰ HOMEP CˆƒHA‹A
C            I              I      I        “PAB‹EHˆŸ Cˆ”“;
C    13      I     3        I  LM  I ATOKM -A„PEC B TOKN/N1 APAMETPA
C            I              I      I        “PAB‹EHˆŸ;
C    14      I     4        I  DM  I KD    -KO„ COCTOŸHˆŸ BEHTˆ‹Ÿ:
C            I              I      I        KD = 0 -‡AKP›T,
C            I              I      I        KD = 1 -OTKP›T;
C    15      I     5        I  DM  I HX    -BPEMŸ BO‡MO†HO‰ KOMM“TA–ˆˆ;
C    16      I     6        I  DM  I MCIU  -KO„ EPEXO„A ˆ‡ O„HOƒO
C            I              I      I        COCTOŸHˆŸ B „P“ƒOE;
C    17      I     7        I  DM  I HXCIU -BPEMŸ EPEXO„A;
C  18,19     I    8,9       I      I    PE‡EPB
C----------------------------------------------------------------------
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, AELT, ATOKM, TIPX
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ‹-TA ,“PAB‹Ÿž™EƒO BEHTˆ‹EM.
       NELT = MSW(11)
C-->
       NEL = MSW(1)
       TIP = MSW(2)
C----------------------------------------------------------------------
C-->  ˆCKATœ B MSWALL ‹EMEHT C #‹-TA = NELT:
       CALL SEQMS0(MSWALL, NELT, AELT, IRC, NSM, ATOKM )
C-->  HA˜‹ˆ ?
       IF   ( IRC .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C-->  ‹EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 100) NEL, TIP, NELT
100    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     =  ' ( Tˆ # ', I3, ' ) ***'/
     =  ' *** C HE C“™ECTB“ž™ˆM B –Eˆ ‹EMEHTOM # ', I5,' ***'/
     =  ' *** OPAOTKA –Eˆ PEKPA™AETCŸ *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA˜‹ˆ, —TO ‡A Tˆ TAM †ˆBET ?
       TIPX = MSWALL(AELT + 1 )
       IF( TIPX .EQ. 301 ) GO TO 2
C----------------------------------------------------------------------
C-->  CBŸ‡œ C HE HA˜ˆM TˆOM.....
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELT, TIPX
101    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     = ' ( Tˆ # ', I3, ' ) ***'//
     = ' *** C ‹EMEHTOM # ', I5, ' ( Tˆ # ', I3, ' ) - O˜ˆO—EH',
     = ' ˆ‹ˆ HE PEA‹ˆ‡OBAH ***'/
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
C----------------------------------------------------------------------
2      CONTINUE
C-->  MSW - HA˜‹ˆ, A ECTœ ‹ˆ ‡A„AHHOE CME™EHˆE “ HA‰„EHHOƒO TˆA?
       IF ( MSW(12) .LE. NSM ) GO TO 3
C----------------------------------------------------------------------
C-->  CBŸ‡œ C APAMETPOM ,KOTOPOƒO HE C“™ECTB“ET “ ‡A„AHHOƒO ‹EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,102) NEL, TIP, NELT, MSW(12) , NSM
102    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     = ' ( Tˆ # ', I3, ' ) ***'//
     = ' *** C ‹EMEHTOM # ', I5, 'CO CME™EHˆEM ', I5, ' HE BO‡MO†EH ',
     = ' T.K. ‡APO˜EHH›‰ ‹EMEHT ˆMEET MAKC. BO‡M. CME™EHˆE ', I5 //
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
3      CONTINUE
C----------------------------------------------------------------------
C-->  HA—A‹O OˆCAHˆŸ BEHTˆ‹Ÿ--->
C-->  A„PEC B TOKN/TOKN1 „‹Ÿ “PAB‹Ÿž™E‰ EPEMEHHO‰  -->
       MSW(13) = ATOKM + MSW(12) - 1
       KOD = 0
       RETURN
       END
