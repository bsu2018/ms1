       SUBROUTINE LM0011( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT îEBPAãü 1989 É.)------------
C   èOÑèPOÉPAMMA: LM0011( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HAáHAóEHàE:   ãOÉàóECKAü HACTPOâKA MSW MAKPOMOÑEãà TàèA # 10
C   ----------     *** MOCT ãAPàOHOBA ***
C
C
C              MCIU - KOÑ àMèìãúCA --->
C                     MCIU   = 00  --> "Càì" HET B [TN, TN+H0];
C                                       HXCIU = 0.0;
C                     MCIU   = 01  --> B [TN, TN+H0] -> èEPEÑHàâ îPOHT
C                                   àMèìãúCA "Càì", HXCIU = àHTEPBAã OT
C                                      TN ÑO èEPEÑHEÉO îPOHTA "Càì";
C                     MCIU   = 10 -->  B [TN, TN+H0] -> áAÑHàâ îPOHT
C                                   àMèìãúCA "Càì", HXCIU = àHTEPBAã
C                                      OT TN ÑO áAÑHEÉO îPOHTA àMèìãúCA;
C                     MCIU   = 11 -->  B [TN, TN+H0] "Càì" BKãûóEH,
C                                      COCTOüHàE HE MEHüã, HXCIU=0.0;
C
C
C
C                  ùãEMEHT MOÜET ÅõTú ãOÉàóECKà CBüáAH C MAKPOMOÑEãüMà:
C                       Tàè # 301 ( Càîì )
C
C
C   METOÑ:        èOàCK B MSW_ALL OèàCAHàà ùãEMEHTA *Càîì
C   ------        B CãìóAE HAXOÜÑEHàü -> ÑOîOPMàPOBAHàE MSW_OèàCAHàü;
C                 B ùTOM CãìóAE KOD = 0, àHAóE = 1;
C   èAPAMETPõ:    MSW(RMSW) - "OTPEáOK" MSW_MOÑEãà Tàè # 301;
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
C              îOPMAT MSW # TàèA 10 [ MOCT ãAPàOHOBA  ]:
C              -------------------------------------------
C               "èOCTOüHHAü" óACTú
C               ******************
C
C  +-------+--------+--------+--------+------+------+--------+----+
C  I HOMEP I HOMEP  I HOMEP  I  èPàá. I KOãà I KOã. I AÑP.   I PE I
C  I ùãEME I TàèA   I è/è    I TàèA   I óECT I BõB. I áHAó.  I áE I
C  I HTA   I MOÑEãà I PACóETAI (PEá.) I BO   I áHAó I "TOKN" I PB I
C  I       I        I DM.....I       .I ìáãOBI EHàâ.I "TOKN1"I    I
C  +-------+--------+--------+--------+------+------+--------+----+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7    I 8  I
C  +-------+--------+--------+--------+------+------+--------+----+
C
C
C              èEPEMEHHAü óACTú MSW:
C              *********************
C
C  CãOBO     CãOBO     OÅOáHAóEHà à ãOÉàóECKOE áHAóEHàE
C  OT        OT
C  HAóAãA    èEPEMEH.
C  MSW       óACTà        Ñãü BEHTàãEâ:
C     ìáãõ                  ìáãõ:
C  9 - îAáA "A"           V1 --> <09> - <12>
C 10 - îAáA "B"           V2 --> <10> - <12>
C 11 - îAáA "C"           V3 --> <11> - <12>
C 12 - "A"- AHOÑ          V4 --> <13> - <09>
C 13 - "K" - KATOÑ        V5 --> <13> - <10>
C                         V6 --> <13> - <11>
C
C    14       1   [FM]  1 èPàáHAK ìèPABãEHàü --> 0 HET,
C                                                1 - ìèPABãEHàE èO Càì
C    15       2   [FM]  NPB - #ùã., ÑAûôEÉO Càì
C    16       3   [LM]  ANPB - AÑPEC #ùã. B MSWALL
C    17       4   [LM]  AUYP - AÑPEC B TOKN/N1 áHAóEHàâ Càì
C    18       5   [ ]    PEáEPB
C    19       6   [ ]    PEáEPB
C    20-25    7-12    MSWX[1] V1-> #èOÑ.ùã; KD  HX  MMCIU  HXCIU  îã. KÑ
C    26-31   13-18    MSWX[2] V2-> #èOÑ.ùã; KD  HX  MMCIU  HXCIU  îã. KÑ
C    32-37   19-24    MSWX[3] V3-> #èOÑ.ùã; KD  HX  MMCIU  HXCIU  îã. KÑ
C    38-43   25-30    MSWX[4] V4-> #èOÑ.ùã; KD  HX  MMCIU  HXCIU  îã. KÑ
C    44-49   31-36    MSWX[5] V5-> #èOÑ.ùã; KD  HX  MMCIU  HXCIU  îã. KÑ
C    50-55   37-42    MSWX[6] V6-> #èOÑ.ùã; KD  HX  MMCIU  HXCIU  îã. KÑ
C
C----------------------------------------------------------------------
C  LM<OÅôàâ> = 55;      LM<ãOKAãúHõâ>=42
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 301, 9*0 /
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
       IF ( MSW(14) .EQ. 0 ) RETURN
C-->  HOMEP ùã-TA C U. ìèP
       NELMOT = MSW(15)
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
C-->  AMOT - AÑP MSW ùãEMEHTA, C KOTOPõM B CBüáà..
       MSW(16) = AMOT
C-->    ATOKM -->  AÑPEC BõB. áHAó. ùTOÉO ùãEMEHTA
       MSW(17) = ATOKM
       KOD = 0
C-->  CîOPMàPOBATú # èOÑùãEMEHTOB
       NN = NEL * 1000
       DO 4 I=1,6
       J = 20 + (I-1)*6
       MSW(J) = NN + I
4      CONTINUE
       RETURN
       END
