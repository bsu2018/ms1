         SUBROUTINE  CALLDM( KF, MSWALL, YALL, JALL, LOGYTR ,
     *                       TOKN, TOKN1, VALLN, VALLN1, KODM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT OKTüÅPü 1987 É.)------------
C   èOÑèPOÉPAMMA:  CALLDM( KF, MSWALL, YALL, JALL, LOGYTR ,
C   -------------         TOKN, TOKN1, VALLN, VALLN1, KODM )
C
C   HAáHAóEHàE:    èOCãEÑOBATEãúHõâ BõáOB DM MOHàTOPOB Ñãü ùãEMEHTOB,
C  -----------     MAKPOMOÑEãà KOTOPõX àMEûT PAáãàóHõE áHAóEHàü
C                  èPàáHAKA TàèA ( B áABàCàMOCTà OT KF - áAèPOCA );
C
C
C   METOÑ:         CKAHàPOBAHàE à OèPOC DM MOHàTOPOB COÉãACHO CèàCKA
C   -----           MSWALL;
C
C   èAPAMETPõ:     KF - KOÑ áAèPOCA HA ÑEâCTBàE:
C   ---------         = 1 OèPOC BCEX DM MOHàTOPOB;
C                     = 2 OèPOC DM MOHàTOPOB, MOÑEãàPìEMõX MAKPO-
C                         MOÑEãüMà C èPàáHAKAMà TàèA #5 à #6
C                         ( "èEPEKãûóATEãúHõE" ùãEMEHTõ );
C                     = 3 OèPOC DM MOHàTOPOB TOãúKO BEHTàãúHõX
C                         ùãEMEHTOB ( èPàá. TàèA #6);
C                     = 4 OèPOC DM MOHàTOPOB TOãúKO Ñãü ùãEMEHTOB
C                         "èOCTOüHHOâ" CTPìKTìPõ ( #1, #2, #3, #4 );
C                     = 5 OèPOC DM MOHàTOPOB TOãúKO Ñãü ùãEMEHTOB
C                         "èOCTOüHOâ" à "TOèOãOÉàóECKà èOCTOüHHOâ"
C                          CTPìKTìPõ ( #1, #2, #3, #4, #5 );
C                     = 6  OèPOC DM MOHàTOPOB TOãúKO Ñãü ùãEMEHTOB
C                           C èPàá. TàèA # 5 ;
C
C                        MSWALL - MACCàB CèàCKA ìèP. CãOB ùãEMEHTOB.
C
C
C                  TOKN - MACCàB áHAóEHàâ "TOKOB" à ÑP. èEPEMEHHõX
C                         COCTOüHàü MOÑEãEâ ùãEMEHTOB Ñãü TN MOMEHTA
C                         BPEMEHà;
C                  TOKN1 - TO ÜE, HO Ñãü TN = TN + H0 MOMEHTA BPEMEHà;
C
C                  VALLN - áHAóEHàü ìáãOBõX HAèPüÜEHàâ B TN;
C                  VALLN1 - áHAóEHàü ìáãOBõX HAèPüÜEHàâ B TN + H0;
C                  KODM - èAPAMETP OÅPATHOâ CBüáà MOHàTOPOB,
C                         BõÅàPAETCü KAK MAX OT KOD - BCEX
C                         BõáBAHõX MOHàTOPOB;
C                  YALL   - MATPàñA èPOBOÑàMOCTEâ;
C                  VALL   - BEKTOP ìáãOBõX HAèPüÜEHàâ ( àCKOMõâ );
C                  JALL   - BEKTOP àCTOóHàKOB TOKA BETBEâ;
C                  LOGYTR - BEKTOP èPàáHAKOB K.á. ìáãOB;
C
C             ====== BCE èAPAMETPõ, KPOME KF èEPEÑAûTCü DM MOHàTOPAM,
C                    èPàóEM: èEPEÑAûTCü "OTPEáKà" (INTEGER + REAL )
C                    ******  MSW_ALL Ñãü ÑAHHOÉO ùãEMEHTA, A TAK ÜE
C                            BECú MACCàB MSW_ALL ( INT. + REAL ) à
C                            AÑPEC "OTPEáKA" B MSW_ALL CèàCKE TEKìôEÉO
C                            CãOBA ( Ñãü OÅPAÅATõBAEMOÉO ùãEMEHTA );
C            ========
C
C
C   èOÑèPOÉPAMMõ: 1. HAÅOP è/è C èPEîàKCOM DM.....;
C   ------------  2. COMMON PROF1;         SUBERR;
C
C  èPàMEóAHàE:    1. HAãàóàE MOHàTOPOB DM..... HE èPOBEPüETCü;
C  ----------     2. èPà KF > 1 - B Cãìó. OTCìTCTBàü ùãEMEHTOB
C                    C è.TàèA B COOTBETCTBàà C áAèPOCOM, BõBOÑ COOÅ-
C                    ôEHàü à BOáBPAT B BõáõBAûôìû èPOÉPAMMì;
C                 3. PEAãàáOBAHO OÅPAôEHàE K 41 è/è DM.....;
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSWALL(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION TOKN(*), TOKN1(*), VALLN(*), VALLN1(*)
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE ( T5MSW, MASPRT(41) )
         EQUIVALENCE ( T6MSW, MASPRT(49) )
         EQUIVALENCE ( T4MSW, MASPRT(93) )
         INTEGER T5MSW, T6MSW, T4MSW
         REAL JALL
         INTEGER ADRN,  TIP,  DM, PTIP
         KODM = 0
         KD = 0
C===> MAX - MAX ÑOè. HOMEP B MSWALL
         MAX = MSWALL(3)
C
C==>  BõÅOP KF áAèPOCA, IEND = AÑP. èOCã. CãOBA B MSWALL;
C
         IEND = 0
         GO TO (100, 200, 300, 400, 500, 600 ), KF
C!!!-------------------------------------------------------------------
         NWTRE=MASPRT(14)
         WRITE(NWTRE,2102) KF
2102     FORMAT(' *** OòàÅKA BõáOBA CALLDM, KF = ', I5,' ***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
100      CONTINUE
C===> HAó. ìCTAHOB. TEK. ìKAá. ADRN - ìKAá. Cã. áBEHA:= <A.NEXT><MSW>
         ADRN = 6
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
200      CONTINUE
C===> èPOCMOTP C è.TàèA #5 ( T5MSW ) T.E.    è.Tàè #5  à è.Tàè # 6
         ADRN = T5MSW
         IF ( ADRN .NE. 0 ) GO TO 900
C===>  MOÜET ECTú è.Tàè # 6 ?
         GO TO 300
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
201      NWTRE = MASPRT(14)
         WRITE(NWTRE,2103) KF, ADRN, IEND
2103     FORMAT(' *** áAèPOC BõáOBA MOHàTOPOB B CALLDM HE èOÑTBEPÜÑAETCü
     = ***' / ' *** KF = ', I5, ' ADRN = ', I5, ' IEND = ', I5, ' ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
300      CONTINUE
C===> èPOCMOTP C è.TàèA #6 ( T6MSW)
         ADRN = T6MSW
         IF ( ADRN .NE. 0 ) GO TO 900
         GO TO 201
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
400      CONTINUE
C===> èPOCMOTP ÑO è.TàèA #5 ( ÑO T5MSW )
         IEND = T5MSW
         ADRN = 6
         IF ( ADRN .EQ. IEND  ) GO TO 201
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
C===> èPOCMOTP ÑO è.TàèA #6 ( ÑO T6MSW )
         ADRN = 6
         IEND = T6MSW
         IF ( ADRN .EQ. IEND ) GO TO 201
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C===>  èPOCMOTP TOãúKO è.Tàè_A # 5
         ADRN = T5MSW
         IEND = T6MSW
         IF ( ADRN .EQ. 0 ) GO TO 201
         GO TO 900
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$   èOÑÉOTOBKA à
C$   BõáOB               M E T K A  # 900
C$   MOHàTOPOB DM....    -----------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
900      CONTINUE
         KODM = MAX0( KD, KODM)
C===> HE èOCãEÑHàâ ãà ùTO MSW?
         I1 = MSWALL(ADRN)
         IF ( I1 .EQ. 0 ) GO TO 666
C===>  HE èOCãEÑHEE ãà ùTO CãOBO B MSWALL, àá áAKAáAHHõX
         IF ( ADRN .EQ. IEND ) GO TO 666
C===> KOHTPOãú CÅOü..
         IF ( I1 .GT. MAX ) GO TO 667
C===> AÑP. TEK. MSW ==> IM  ( 1-E CãOBO - ìKA. HA CãEÑ. MSW ):
         IM = ADRN + 1
C===> AÑP. CãEÑ. MSW:
         ADRN = I1
C===> BõáOB MOHàTOPOB COÉãACHO HOMEPOB MOHàTOPOB DM <=========
C$$$$$$$$TIP = MSWALL(IM + 1)
         DM = MSWALL(IM + 2 )
         KD = 0
C===> É ã O Å A ã ú H õ E   è E P E X O Ñ õ ==========
         GO TO (  1,  2,  3,  4,  5,  6,  7,  8,  9,  10,
     =           11,  12,  13,  14,  15,  16,  17,  18,  19,  20,
     =           21,  22,  23,  24,  25 , 26,  27,  28,  29,  30,
     =           31,  32,  33,  34,  35 , 36,  37,  38,  39,  40,
     =           41,  42, 43                                     ), DM
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE,2101) DM
2101     FORMAT(' *** ãOÉ. CÅOâ B CALLDM -> CTPAHHõâ # DM MOHàTOPA   ***
     ='/ ' ***  DM  = ', I10, ' ***')
C!!!-------------------------------------------------------------------
         GO TO 667
667      CONTINUE
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 2100) ADRN, I1, MAX
2100     FORMAT(' *** ãOÉ. CÅOâ B CALLDM -> MSW_ALL èãOXOâ ***'/
     =   ' *** ADRN = ', I10, ' I1 = ', I10, ' MAX = ', I10, ' ***')
C********CALL PRINT1(MSWALL)
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$  CàCTEMA CîOPMàPOBAHA      M E T K A  # 666
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C***  BõáõBAãúôàK MOHàTOPOB DM..... ***********************************
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
1     CALL DM0001(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
2     CALL DM0002(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
3     CALL DM0003(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
4     CALL DM0004(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
5     CALL DM0005(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
6     CALL DM0006(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
7     CALL DM0007(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
8     CALL DM0008(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
9     CALL DM0009(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD   , MSWALL, MSWALL, IM)
         GO TO 900
C----------------------------------------------------------------------
10    CALL DM0010(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
11    CALL DM0011(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
12    CALL DM0012(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
13    CALL DM0013(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
14    CALL DM0014(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
15    CALL DM0015(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
16    CALL DM0016(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
17    CALL DM0017(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
18    CALL DM0018(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
19    CALL DM0019(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
20    CALL DM0020(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
21    CALL DM0021(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
22    CALL DM0022(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
23    CALL DM0023(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
24    CALL DM0024(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
25    CALL DM0025(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
26    CALL DM0026(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
27    CALL DM0027(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
28    CALL DM0028(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
29    CALL DM0029(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
30    CALL DM0030(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
31    CALL DM0031(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
32    CALL DM0032(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
33    CALL DM0033(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
34    CALL DM0034(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
35    CALL DM0035(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
36    CALL DM0036(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
37    CALL DM0037(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
38    CALL DM0038(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
39    CALL DM0039(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
40    CALL DM0040(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
41    CALL DM0041(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
42    CALL DM0042(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
43    CALL DM0043(MSWALL(IM), MSWALL(IM), YALL,  JALL, LOGYTR,
     =            TOKN, TOKN1, VALLN, VALLN1, KD    , MSWALL, MSWALL,IM)
         GO TO 900
C----------------------------------------------------------------------
         END
