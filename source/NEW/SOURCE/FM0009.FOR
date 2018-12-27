         SUBROUTINE FM0009( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT ”EBPA‹Ÿ 1989 ƒ.)------------
C   O„POƒPAMMA:  FM0009( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹ˆ:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  10       "MOCT ‹APˆOHOBA"          TM
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹ˆ.
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ "MOCT ‹APˆOHOBA"
C   -----
C
C          CBŸ‡œ C BHE˜HE‰ CPE„O‰:
C
C            ”A‡A "A"       ”A‡A "B"     ”A‡A  "C"
C              ! I             ! I          ! I
C              ! V             ! V          ! V
C   +--< V1 ]--*---------------!------------!----< V4 ]----+
C   !           <-- IV1        !            !   <-- IV4    !
C   !                          !            !              !
C   *--< V2 ]------------------*------------!----< V5 ]----*
C   !           <-- IV2                     !   <-- IV5    !
C   !                                       !              !
C   +--< V3 ]-------------------------------*----< V6 ]----*
C   !           <-- IV3                                    !
C   !                                            <-- IV6   !
C   !    ---> TOK_A                         ----> TOK_K    !
C   +---------*                                 *----------+
C     "AHO„"  +                                 - "KATO„"
C
C
C
C     KO‹-BO “‡‹OB --- 5
C
C     B›BO„H›E ‡HA—EHˆŸ- 17 ˜T. :
C      TOK ”A‡›-A, TOK ”A‡›-B, TOK ”A‡›-C, TOK AHO„A, TOK KATO„A,
C      TOKˆ V1, V2, V3, V4, V5, V6,
C      Cˆ”“ V1, V2, V3, V4, V5, V6;
C
C   KO„ˆPOBKA:
C
C    # T 10 “  1 2 3 4 5
C                MAS < KOD [#‹] >;
C
C ƒ„E:  KOD = 0 EC‹ˆ MOCT HE “PAB‹ŸEM›‰, TOƒ„A #‹ MO†HO HE KO„ˆPOBATœ;
C
C       KOD = 1 EC‹ˆ MOCT “PAB‹ŸEM›‰, TOƒ„A --->
C                #‹ - HOMEP ‹EMEHTA –Eˆ, ŸB‹Ÿž™ˆMCŸ Cˆ”“ „‹Ÿ MOCTA
C                      B TOKN/N1 CO„EP†ATCŸ: MMCIU - 6 ˜T“K.
C                                            HXCIU - 6 ˜T“K.
C                B COOTBETCTBˆˆ CO CXEMO‰ ˆ H“MEPA–ˆE‰ BEHTˆ‹E‰
C
C     ˆCO‹œ‡“ETCŸ: FM0009, LM0011, DM0013;
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ B MˆHˆMA‹œHOM O"EME;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. PEA‹ˆ‡A–ˆŸ „‹Ÿ KOD  = 0  , KOD  = 1
C                     Pˆ O˜ˆKAX --> PˆHˆMAETCŸ KOD  = 0;
C                     ‡HA—EHˆŸ APAMETPOB ---->
C
C              ”OPMAT MSW # TˆA 10 [ MOCT ‹APˆOHOBA  ]:
C              -------------------------------------------
C               "OCTOŸHHAŸ" —ACTœ
C               ******************
C
C  +-------+--------+--------+--------+------+------+--------+----+
C  I HOMEP I HOMEP  I HOMEP  I  Pˆ‡. I KO‹ˆ I KO‹. I A„P.   I PE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I ‡HA—.  I ‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI (PE‡.) I BO   I ‡HA— I "TOKN" I PB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I "TOKN1"I    I
C  +-------+--------+--------+--------+------+------+--------+----+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7    I 8  I
C  +-------+--------+--------+--------+------+------+--------+----+
C
C
C              EPEMEHHAŸ —ACTœ MSW:
C              *********************
C
C  C‹OBO     C‹OBO     OO‡HA—EHˆ ˆ ‹Oƒˆ—ECKOE ‡HA—EHˆE
C  OT        OT
C  HA—A‹A    EPEMEH.
C  MSW       —ACTˆ        „‹Ÿ BEHTˆ‹E‰:
C     “‡‹›                  “‡‹›:
C  9 - ”A‡A "A"           V1 --> <09> - <12>
C 10 - ”A‡A "B"           V2 --> <10> - <12>
C 11 - ”A‡A "C"           V3 --> <11> - <12>
C 12 - "A"- AHO„          V4 --> <13> - <09>
C 13 - "K" - KATO„        V5 --> <13> - <10>
C                         V6 --> <13> - <11>
C
C    14       1   [FM]  1 Pˆ‡HAK “PAB‹EHˆŸ --> 0 HET,
C                                                1 - “PAB‹EHˆE O Cˆ“
C    15       2   [FM]  NPB - #‹., „Až™EƒO Cˆ“
C    16       3   [LM]  ANPB - A„PEC #‹. B MSWALL
C    17       4   [LM]  AUYP - A„PEC B TOKN/N1 ‡HA—EHˆ‰ Cˆ“
C    18       5   [ ]    PE‡EPB
C    19       6   [ ]    PE‡EPB
C    20-25    7-12    MSWX[1] V1-> #O„.‹; KD  HX  MMCIU  HXCIU  ”‹. KD
C    26-31   13-18    MSWX[2] V2-> #O„.‹; KD  HX  MMCIU  HXCIU  --
C    32-37   19-24    MSWX[3] V3-> #O„.‹; KD  HX  MMCIU  HXCIU  --
C    38-43   25-30    MSWX[4] V4-> #O„.‹; KD  HX  MMCIU  HXCIU  --
C    44-49   31-36    MSWX[5] V5-> #O„.‹; KD  HX  MMCIU  HXCIU  --
C    50-55   37-42    MSWX[6] V6-> #O„.‹; KD  HX  MMCIU  HXCIU  ---
C
C  LM<O™ˆ‰> = 55;      LM<‹OKA‹œH›‰>=42
C
C
C
C               PAO—ˆ‰ MACCˆB TˆA  MSWX TˆA # A9 ( „‹Ÿ Tˆ. #10)
C             -------------------------------------
C
C
C  +-------+--------+--------+--------+------+-------+
C  I HOMEP I  "KD"  I "HX"   I MMCIU  I  HX  I ”‹Aƒ  I
C  I O„ - I Pˆ‡-  I BPEMŸ  I KO„ “PI CIU  I       I
C  I ‹EME I HAK CO-I PE„-O‰I ˆM“‹œ I ˆM-CAI "KD"  I
C  I HTA   I CTOŸHˆŸI KOM-–ˆˆI  CA    I      I       I
C  +-------+--------+--------+--------+------+-------+
C  I  1    I   2    I   3    I    4   I   5  I       I
C  +-------+--------+--------+--------+------+-------+
C
C   MMCIU  ˆ  HXCIU OPE„E‹ŸET MOHˆTOP MAKPOMO„E‹ˆ BEHTˆ‹Ÿ......
C
C
C    BEHTˆ‹œ OTKP›T ==>  KD=1;;;;BEHTˆ‹œ ‡AKP›T ==>  KD=0;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         EQUIVALENCE ( NUM, RNUM )
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 10 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0009: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1        CONTINUE
C---> OH“‹EHˆE BCEX O‹E‰ RMSW „‹Ÿ BEHTˆ‹Ÿ  (INTEGER)
         LM = 42
         NUM = 0
         DO 100 I=1,LM
100      RMSW(I) = RNUM
C===> PˆHŸTœ ˆCXO„. APAMETP›:
C-->  KOTPO‹œ
         KKM = PAR0T(1)
C-->  Bˆ„ “PAB‹EHˆŸ..
         PMAS = PAR0T(2)
         IF ( PMAS .EQ. 0.0 ) GO TO 2
         IF ( PMAS .EQ. 1.0 ) GO TO 3
C----------------------------------------------------------------------
         WRITE(NWTR,4) NEL, PMAS
4        FORMAT(' *** Tˆ “PAB‹EHˆŸ "Cˆ“" „‹Ÿ ‹EMEHTA #', I5, ' ***',
     =          ' *** PABH›‰ ', F5.0,' O˜ˆO—EH  --> PˆHˆMAETCŸ',
     =    ' PABH›M H“‹ž ***')
         GO TO 2
C----------------------------------------------------------------------
2        CONTINUE
C-->  PMAS = 0   HE “PAB. BEHTˆ‹œ.....
C MMCIU=11,  HXCIU=0.0
         NUM=11
         DO 201 I=1,6
         JMSW =  (I-1)*6 + 10
         RMSW(JMSW) = RNUM
         RMSW(JMSW+1) = 0.0
201      CONTINUE
         GO TO 5
C######################################################################
3        CONTINUE
C---> PMAS = 1 --> ECTœ “PAB‹EHˆE (HOMEP ‹-TA)
         NUM=INT(PAR0T(3))
         RMSW(2) = RNUM
C--> Pˆ‡HAK “PAB‹EHˆŸ....
         NUM = 1
         RMSW(1) = RNUM
         GO TO 5
C----------------------------------------------------------------------
5         CONTINUE
          RETURN
          END
