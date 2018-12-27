       SUBROUTINE LM0005( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA: LM0005( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HAáHAóEHàE:   ãOÉàóECKAü HACTPOâKA MSW MAKPOMOÑEãà TàèA # 14 à 140
C   ----------     ***  ùã. MAò. C XàTPõM M_BHEò.   ******
C   METOÑ:        èOàCK B MSW_ALL OèàCAHàà ùãEMEHTA *ùã. MAò.*
C   -----         ( ÑOè. Tàè COÉãACHO BCTPOEHHOâ TAÅ. "LTIP" )
C                 B CãìóAE HAXOÜÑEHàü -> ÑOîOPMàPOBAHàE MSW_OèàCAHàü;
C                 B ùTOM CãìóAE KOD = 0, àHAóE = 1;
C   èAPAMETPõ:   MSW(RMSW) - "OTPEáOK" MSW_MOÑEãà Tàè # 13 àãà 130
C   ----------    ADR - AÑPEC B MSW_ALL MSW_OèàCAHàü ùTOâ MOÑEãà;
C                 MSWALL(RMSWAL) - BECú CèàCOK MSW CãOB;
C                 KOD - = 0 - CîOPMàPOBAãà MSW,
C                       = 1 - ãOÉ. OòàÅKà;
C                      ( HA NWTR COOÅôEHàE BõBOÑàTCü );
C   èOÑèPOÉPAMMõ: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C   èPàMEóAHàE:  îOPMATõ MSW_OèàCAHàâ ---->
C   ----------
C
C    Ñ ã ü    ùã.  MAòàHõ:
C   à TOãúKO ******
C
C               "èOCTOüHHAü" óACTú MSW TàèA # 11, 110, 12, 120, 14, 140:
C                ------------------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I èPàáHAKI KOãà I KOã. IAÑP.   IPE I
C  I ùãEME I TàèA   I è/è    I TàèA   I óECT I BõB. IHAó.   IáE I
C  I HTA   I MOÑEãà I PACóETAI MOÑEãà I BO   I áHAó I"TOKN" IPB I
C  I       I        I DM.....I       .I ìáãOBI EHàâ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C  +--------------------------------------------------------------+
C  I ìáãõ èOÑKãûóEHàü S1N S2N S3N F1N F2N F3N S1K S2K F1K F2K F3K I
C  I           12-Tú CãOB: OT 9 ÑO 20                             I
C  +--------------------------------------------------------------+
C
C              RMSW Tàè # 11, 110, 12, 120 [ ùã. MAòàHA 3X3X0 ] :
C              ************************************************
C
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PEáEPB I"DOM"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 3-Tú   I Ñãü I      I      I       I       I       I       I     I
C  I  CãOB  IMEX. I 9-Tú I 9-Tú I 36-Tú I 36-Tú I 6-Tú  I 6-Tú  I NMASI
C  I        IìP-ü I CãOB I CãOB I CãOB  I CãOB  I CãOB  I CãOB  I CãOBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I *****  I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - OèPEÑEãüETCü PAáMEPOM MAS <...> ==> ...+ NMAS + 1
C
C       ***  1-E CãOBO HOMEP #ùã.,
C            2-E CãOBO AÑPEC B TOKN/N1 MFT_N1 Ñãü #ù.
C
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
       DATA LTAB/ 11, 110, 12, 120, 14, 140,  4*0 /
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
C-->  HOMEP ùã-TA ùã. MAòàHõ:
       NELMOT = MSW(21)
C-->
       NEL = MSW(1)
       TIP = MSW(2)
C-->
C-->  àCKATú B MSWALL ùãEMEHT C #ùã-TA = NELMOT:
       CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, KVIV, ATOKM )
C-->
       IF   ( IRC .EQ. 1 ) GO TO 1
C-->  ùãEMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 100) NEL, TIP, NELMOT
100    FORMAT(//' *** áAèPOC HA ãOÉ. CBüáú ùãEMEHTA # ', I5,
     =  ' ( Tàè # ', I3, ' ) ***'/
     =  ' *** C HE CìôECTBìûôàM B ñEèà ùãEMEHTOM # ', I5,' ***'/
     =  ' *** OÅPAÅOTKA ñEèà èPEKPAôAETCü *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HAòãà, óTO áA Tàè TAM ÜàBET ?
       TIPX = MSWALL(AMOT + 1 )
       DO 2 I = 1, K10
       IF ( TIPX .EQ. LTAB(I) ) GO TO 3
2      CONTINUE
C-->  CBüáú C HE HAòàM TàèOM.....
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELMOT, TIPX
101    FORMAT(//' *** áAèPOC HA ãOÉ. CBüáú ùãEMEHTA # ', I5,
     = ' ( Tàè # ', I3, ' ) ***'//
     = ' *** C ùãEMEHTOM # ', I5, ' ( Tàè # ', I3, ' ) - OòàÅOóEH',
     = ' àãà HE PEAãàáOBAH ***'/
     = ' *** OÅPAÅOTKA ñEèà èPEKPAôAETCü ***'//)
       RETURN
3      CONTINUE
C-->  HAóAãO OèàCAHàü MAòàHõ--->
C******MSW( *) = AMOT
C-->  HAóAãO CèàCKA ìáãOB MAòàHõ-->
C******MSW( *) = AMOT+8
C-->  AÑPEC B TOKN/TOKN1 Ñãü èEPEMEHHõX COCTOüHàü MAòàHõ-->  M_FT
       MSW(22) = ATOKM + 9 - 1
       KOD = 0
       RETURN
       END
