       SUBROUTINE FM0018( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA  FM0018( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HAáHAóEHàE:  1) HACTPOâKA MAKPOMOÑEãà M-îAáHOÉO òAÉOBOÉO ÑBàÉATEãü
C   ----------      C MAÉHàTHO HE CBüáAHHõMà OÅMOTKAMà.
C                   ***************************************************
C                  Tàè # 502 --> 2-X  îAáHõâ BPAô. òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 503 --> 3-X  îAáHõâ BPAô. òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 504 --> 4-X  îAáHõâ BPAô. òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 505 --> 5-Tà îAáHõâ BPAô. òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 506 --> 6-Tà îAáHõâ BPAô. òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 507 --> 7-Mà îAáHõâ BPAô. òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 508 --> 8-Mà îAáHõâ BPAô. òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 512 --> 2-X  îAáHõâ ãàH.  òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 513 --> 3-X  îAáHõâ ãàH.  òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 514 --> 4-X  îAáHõâ ãàH.  òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 515 --> 5-Tà îAáHõâ ãàH.  òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 516 --> 6-Tà îAáHõâ ãàH.  òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 517 --> 7-Mà îAáHõâ ãàH.  òAÉOBõâ ÑBàÉATEãú;
C                  Tàè # 518 --> 8-Mà îAáHõâ ãàH.  òAÉOBõâ ÑBàÉATEãú;
C
C
C
C                  ùãEMEHT OèàCõBAETCü:
C           ù ?? T 5?? ì #H1 #H2 ... #K1 #K2...
C                      è  M  P  R  J  MT  TL
C                      MAS < L0 L1 ....LN >
C                      MAS < T1 Y1 ....   > ;
C
C           ÉÑE   M   -óàCãO îAá ÑBàÉATEãü;
C                 P   -óàCãO èAP èOãûCOB ÑBàÉATEãü;
C                 R   -AKTàBHOE COèPOTàBãEHàE OÅMOTKà,(OM);
C                 J   -MOMEHT(B KÉ*M)àHEPñàà BPAô. óACTà (Ñãü BPAô. òÑ),
C                      MACCA (B KÉ) èOÑBàÜHOâ óACTà ( Ñãü ãàH. òÑ );
C                 MT  -MOMEHT TPEHàü HA BAãì (B H*M)(Ñãü BPAô. òÑ),
C                      CàãA TPEHàü (B H) (Ñãü ãàH. òÑ);
C                 TL  -èEPàOÑ àáMEHEHàü àHÑìKTàBHOCTà îAáõ,
C                      TL = TAU (ÉPAÑ) Ñãü BPAô. òÑ,
C                      TL = TAU (B M ) Ñãü ãàH.  òÑ;
C                 L0..-KOùîîàñàEHTõ ÉAPMOHàóECKOÉO PüÑA, OèàCõBAûôEÉO
C                      áABàCàMOCTú àHÑìKTàBHOCTà îAáõ OT èEPEMEôEHàü:
C                      L(X)=L0+L1*COS(2*PI*X/TL)+L2*COS(4*PI*X/TL)...,
C                      ÉÑE X - èEPEMEôEHàE ùã. èOãü,
C                          X = XMEX * P;
C              T1 Y1..-TAÅãàóHO áAÑAHHAü áABàCàMOCTú MBH(T) Ñãü BPAô.òÑ,
C                      TAÅãàóHO áAÑAHHAü áABàCàMOCTú FBH(T) Ñãü ãàH.òÑ;
C
C
C            2) èPà  FGMSW = 1 -> ìTOóHüETCü áHAóEHàE LM -
C               OÅôEâ ÑãàHõ MSW ÑAHHOÉO TàèA ( C ìóETOM MAS èAP-OB );
C
C   METOÑ:      îOPMàPOBAHàE èEPEMEHHOâ óACTà MSW_ALL OèàCAHàü ùã-TA,
C   -----       BõóàCãEHàE BCèOMOÉATEãúHõX ÑAHHõX, HEOÅXOÑàMõX Ñãü
C               ÑAãúHEâòEâ PAÅOTõ MAKPOMOÑEãà;
C
C   èAPAMETPõ:  BA0T, KNOT, PAR0T - "OTPEáKà" MACCàBOB BA0, KNOT, PAR0
C   ---------   BBEÑEHHõX óEPEá è/è INPUT0;
C               RMSW - èEPEMEHHAü óACTú MSW_ALL OèàCAHàü ùãEMEHTA;
C               LM - èPà FGMSW = 0 => ÑãàHA èEPEMEHHOâ óACTà MSW_ALL;
C                    èPà FGMSW = 1 => ÑãàHA BCEÉO MACCàBA    MSW_ALL;
C   èPàMEóAHàE: 1. BCE èOãü  áAHìãüûTCü
C   ----------
C
C
C                       RMSW Tàè # 502...518
C                      **********************
C
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  I     I     I     I     I      I      I       I         I     I
C  I  M  I  P  I  R  I  J  I  MT  I  TL  I PEáEPBI PEáEPB  I W0  I
C  I     I     I     I     I      I      I       I         I     I
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  I  1  I  2  I  3  I  4  I  5   I   6  I   7   I    8    I  9  I
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  I D1  I D1+1I D1+2I D1+3I D1+4 I D1+5 I  D1+6 I  D1+7   I D1+8I
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  +D1=9+2*M
C  +DR1=1
C
C
C
C  +--------+--------+--------+--------+---------+----------+----------+
C  I        I  D L   I        I        I         I  NGARM,  I   NT,    I
C  I   L    I  ----- I   J0   I G(N+1) I J(N+1)  I L0,L1... I T1,Y1... I
C  I        I  D GAM I        I        I         I          I          I
C  I M CãOB I M CãOB I M CãOB I M CãOB I M CãOB  INMAS1 CãOBINMAS2 CãOBI
C  +--------+--------+--------+--------+---------+---------------------+
C  I DR2+M  I DR3+M  I DR4+M  I DR5+M  I DR6+M   I DR7+NMAS1I DR8+NMAS2I
C  +--------+--------+--------+--------+---------+----------+----------+
C  ID2..D3-1ID3..D4-1ID4..D5-1ID5..D6-1ID6...D7-1I D7..D8-1 I D8+NMAS2 I
C  +--------+--------+--------+--------+---------+----------+----------+
C  +D2=D1+9 +D3      +D4      +D5      +D6       +D7        +D8
C  +DR2=10  +DR3     +DR4     +DR5     +DR6      +DR7       +DR8
C
C   D@ -HOMEPA èOãEâ MSW_ALL MACCàBA;
C   DR@-HOMEPA èOãEâ  R_MSW  MACCàBA;
C
C                           DR1  DR2  DR3  DR4  DR5  DR6  DR7    DR8
C   Ñãü 2-X  îAáHOÉO òÑ-->  1    10   12   14   16   18   20   20+NMAS1
C   Ñãü 3-X  îAáHOÉO òÑ-->  1    10   13   16   19   22   25   25+NMAS1
C   Ñãü 4-X  îAáHOÉO òÑ-->  1    10   14   18   22   26   30   30+NMAS1
C   Ñãü 5-Tà îAáHOÉO òÑ-->  1    10   15   20   25   30   35   35+NMAS1
C   Ñãü 6-Tà îAáHOÉO òÑ-->  1    10   16   22   28   34   40   40+NMAS1
C   Ñãü 7-Mà îAáHOÉO òÑ-->  1    10   17   24   31   38   45   45+NMAS1
C   Ñãü 8-Mà îAáHOÉO òÑ-->  1    10   18   26   34   42   50   50+NMAS1
C
C
C                           D1   D2   D3   D4   D5   D6   D7     D8
C   Ñãü 2-X  îAáHOÉO òÑ-->  13   22   24   26   28   30   32   32+NMAS1
C   Ñãü 3-X  îAáHOÉO òÑ-->  15   24   27   30   33   36   39   39+NMAS1
C   Ñãü 4-X  îAáHOÉO òÑ-->  17   26   30   34   38   42   46   46+NMAS1
C   Ñãü 5-Tà îAáHOÉO òÑ-->  19   28   33   38   43   48   53   53+NMAS1
C   Ñãü 6-Tà îAáHOÉO òÑ-->  21   30   36   42   48   54   60   60+NMAS1
C   Ñãü 7-Mà îAáHOÉO òÑ-->  23   32   39   46   53   60   67   67+NMAS1
C   Ñãü 8-Mà îAáHOÉO òÑ-->  25   34   42   50   58   66   74   74+NMAS1
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       INTEGER D1 , D2 , D3 , D4 , D5 , D6 , D7 , D8
       INTEGER DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8
       INTEGER BA0T, FGMSW, TIP, M
       EQUIVALENCE ( FGMSW,  MASPRT(87) )
       EQUIVALENCE ( N    ,  RN         )
       REAL P, R, J, MT, TL
C--->óTO áA ùãEMEHT?
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP = TIP - 501
       IF ( NTIP .GT. 10 ) NTIP = NTIP - 10
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 7 ) GO TO 99999
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ãOÉ. CÅOâ B FM0018: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
99999  CONTINUE
C--->  HAâÑEM D@  à  DR@
       M = PAR0T( 1 )
          IF ( M .EQ. NTIP + 1 ) GO TO 750
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 700) TIP, NEL, M
700      FORMAT(' *** OòàÅKA èPà áAÑAHàà èAPAMETPA "M" ***'/
     =   ' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10,
     =   '     M =', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
         RETURN
750    CONTINUE
       D1 = 9 + 2 * M
       D7 = D1 + 9 + 5 * M
       DR1 = 1
       DR7 = DR1 + 9 + 5 * M
C-->  óTO èPOCüT?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  ìTOóHàTú MSW_ALL PAáMEP
       NMAS1 = IFIX ( PAR0T (7) )
       NMAS2 = IFIX ( PAR0T (8 + NMAS1 ) )
       D8 = D7 + NMAS1 + 1
       LM = D8 + NMAS2
       RETURN
C######################################################################
1000   CONTINUE
       TIP = BA0T(2)
C-->  îOPMàPOBATú RMSW....
C-->  LM - ÑãàHA "èEPEMEHHOâ" óACTà RMSW...
       NMAS1 = IFIX ( PAR0T (7) )
       NMAS2 = IFIX ( PAR0T (8 + NMAS1 ) )
       DR8 = DR7 + NMAS1 + 1
       LM = DR8 + NMAS2
C********************************************************************
C-> óàCTKA
       DO 1 I=1,LM
       RMSW(I) = 0.0
1      CONTINUE
C--------------------------------------------------------------------
C-->  èPàEM èOCTOüHHõX èAPAMETPOB...
C--------------------------------------------------------------------
          P   = PAR0T(2)
          R   = PAR0T(3)
          J   = PAR0T(4)
          MT  = PAR0T(5)
          TL  = PAR0T(6)
C----------------------------------------------------------------------
C--->  áAèOãHEHàE RMSW èOCTOüHHõMà èAPAMETPAMà.
C----------------------------------------------------------------------
       N = M
       RMSW (1) = RN
       RMSW (2) = P
       RMSW (3) = R
       RMSW (4) = J
       RMSW (5) = MT
       RMSW (6) = TL
C----------------------------------------------------------------------
C--->  áAèOãHEHàE RMSW MACCàBAMà.
C        MACCàB KOùî. ÉAPMOHàóECKOÉO PüÑA.
C----------------------------------------------------------------------
C      IPAR0 - CóETóàK MACCàBA PAR0T.
C--------------------------------------------------------------------
       N = NMAS1
       RMSW ( DR7 ) = RN
       I1 = DR7 + 1
       I2 = DR7 + NMAS1
       IPAR0 = 7
          DO 2000 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
2000      CONTINUE
C----------------------------------------------------------------------
C      MACCàB îìHKñàà   MBH (T)
C--------------------------------------------------------------------
       IPAR0 = IPAR0 + 1
       RN = FLOAT(NMAS2)
       RMSW ( DR8 ) = RN
       I1 = DR8 + 1
       I2 = DR8 + NMAS2
          DO 3000 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
3000      CONTINUE
       RETURN
       END
