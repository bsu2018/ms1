         SUBROUTINE APFTEM ( KAR, KSIM, SIM, RSIM, ER, IER )
CXXX ---KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C      O„POƒPAMMA: APFTEM ( KAR, KSIM, SIM, RSIM, ER, IER )
C      -------------
C      HA‡HA—EHˆE:   HA—A‹œHOE CKAHˆPOBAHˆE TEKCTA ˆ PE„BAPˆTE‹œHOE
C      -----------    B›„E‹EHˆE TEPMOB ˆ —ˆC‹OB›X KOHCTAHT E‡ ‡HAKA
C                     ˆ‡ MACCˆBA KAR ( A-KO„ ) ˆ ”OPMˆPOBAHˆE MACCˆBA
C                     SIM ( F-KO„ ) O O‹“—EHH›M TEPMAM ˆ‡ KAR;
C                     ”ˆKCA–ˆŸ ƒP“›X O˜ˆOK B OˆCAHˆˆ B›PA†EHˆ‰.
C
C      METO„:        OˆCK, B›„E‹EHˆE APˆ”M. B›PA†EHˆ‰ ˆ AKOBKA
C      -----         ˆ„EHTˆ”ˆKATOPOB B SIM. Pˆ TOM:
C                    "C‹“†. CˆMBO‹›" - PE„CTAB‹EH› CBOˆMˆ F-KO„AMˆ, HO
C                                      C OPATH›Mˆ ‡HAKAMˆ, T.E. < 0;
C                    "ˆ„EHTˆ”ˆKATOP›" - PE„CTAB‹EH› O„HO‰ O‡ˆ–ˆE‰ B
C                                       SIM B C†ATOM F-KO„E. Pˆ TOM
C                                       ˆCO‹œ‡“žTCŸ EPB›E —ET›PE
C                                       CˆMBO‹A, OCTA‹œH›E ˆƒHOPˆP“žT-
C                                       CŸ.  ‡HA—EHˆE BCEƒ„A > 0.
C                    "—ˆC‹OB›E
C                     KOHCTAHT›" - PE„CTAB‹EH› B MACCˆBE SIM
C                                  „B“MŸ OC‹E„OBATE‹œH›Mˆ —ˆC‹AMˆ
C                                  H“‹EM ˆ ‡HA—EHˆEM KOHCTAHT› B
C                                  PE„CTAB‹EHˆˆ REAL*4.
C                                  T.E. B HA—A‹E 0, ‡ATEM ‡A—EHˆE > 0.
C
C      APAMETP›:  KAR - MACCˆB CTPOKˆ B A-KO„E ( PA‡MEPHOCTœ >= 72 ),
C      ----------        CO„EP†A™ˆ‰ ˆCXO„H›‰ „‹Ÿ PA‡OPA TEKCT.
C                         Tˆ INTEGER*4;
C                   KSIM - KO‹ˆ—ECTBO ‡AHŸT›X O‡ˆ–ˆ‰ B MACCˆBE SIM,
C                          ( <= 100 ), INTEGER*4;
C                   SIM - MACCˆB TEPMOB B F-KO„E ( PA‡MEPHOCTœ >=100 ).
C                          CM. METO„, INTEGER*4 + REAL*4;
C                   RSIM - TOT †E MACCˆB —TO ˆ SIM, HO OˆCAH›‰
C                           B B›‡›BAž™E‰ POƒPAMME KAK REAL*4;
C                           ˆ‹ˆ Pˆ B›‡OBE APFTEM “KA‡ATœ:
C                           CALL APFTEM ( KAR, KSIM, SIM, SIM, ER,IER)
C                                                         ----
C
C                   ER  -  MACCˆB PA‡METKˆ O˜ˆOK ( 80 CˆMBO‹OB )
C                           B A - KO„E; INTEGER*4.
C                   IER - KO‹ˆ—ECTBO O˜ˆOK; Pˆ = 0 B MACCˆBE ER
C                         HAXO„ŸTCŸ POE‹› B A-KO„E;
C                           Tˆ: INTEGER*4;
C      O„POƒPAMM›: APFTRT, APFSQE, APFPAK; ( COMMON PRINTN );
C      ------------
C      PˆME—AHˆE: 1.  ˆ‡ KAR B›ˆPAETCŸ K72 = 72 EPB›X CˆMBO‹OB;
C      ----------- 2.   B C E      POE‹› ˆƒHOPˆP“žTCŸ;
C                       *****
CZZZ ------------------------------------------------------------------
C!!!-------------------------------------------------------------------
       COMMON /PRINTN/ NW6
C!!!-------------------------------------------------------------------
       DIMENSION KAR(1), SIM(1), RSIM(1), ER(1), FCAR(4)
       INTEGER KAR, SIM, ER, FLAG, FCAR
C------EQUIVALENCE ( RSIM(1),  SIM(1) )--------------------------------
       DATA K72 / 72 /
C EPE‰„EM B F-KO„:
       CALL APFTRT ( 1, K72, KAR, KAR )
C ˆHˆ–ˆA‹ˆ‡A–ˆŸ:
       IK=1
       IS=1
       FLAG=0
       IER=0
C B ER POE‹› B F=KO„E:
       DO 100 J=1,80
100    ER(J)=11
C   CKAHˆP“EM KAR, B›ˆPAEM:
2      CONTINUE
C   POE‹› ? --> 30  *** 01.02.86 ***
       IF ( KAR(IK) .EQ. 11 ) GO TO 30
C  C‹“†EH›E ‡HAKˆ ?  ---> 4
       IF ( KAR(IK) .GE. 12  .AND. KAR(IK)  .LE. 26 ) GO TO 4
C  ˆH„ˆBˆ„“A‹œHO, <.> ˆ <E> ? ---> 5  / <E> ‹OKˆPOBAHO /
       IF ( KAR(IK) .EQ. 27                        ) GO TO 5
C  –ˆ”P› ? ---> 6
       IF ( KAR(IK) .GE. 1  .AND. KAR(IK) .LE. 10 ) GO TO 6
C  “KB› ? ---> 7
       IF ( KAR(IK) .GE. 35  .AND. KAR(IK) .LE.  83 ) GO TO 7
C====== „OO‹HˆTE‹œH›E C‹“†EH›E CˆMBO‹› ---> 4  / 01.02.86 /
       IF ( KAR(IK) .GE. 28  .AND. KAR(IK) .LE. 34 ) GO TO 4
C  ˆHA—E ŸBHAŸ O˜ˆKA, OPAOTATœ EE:
8      IER = IER + 1
C  ‡AˆCATœ "†-“" $
       ER(IK) = 37
       FLAG = 0
       GO TO 3
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C  O†ˆ„AETCŸ –ˆ”PA, ˆ‹ˆ PO„O‹†EHˆE ˆ„EHTˆ”ˆKATOPA
6      CONTINUE
       IF ( FLAG .EQ. 1 ) GO TO 7
C  <.>, <E>, <–ˆ”PA> - EPEKO„ˆPOBATœ KOHCTAHT“
5       CONTINUE
C   H“†HO ‹ˆ B›TO‹KH“Tœ ˆ„EHTˆ”ˆKATOP
       IF ( FLAG .EQ. 0 ) GO TO 9
C   B›TO‹KH“Tœ FCAR
       FLAG = 0
C   “AKOBATœ
       CALL APF PAK ( 1, IDKOD, FCAR )
       SIM(IS) = IDKOD
       IS = IS + 1
C   PˆHŸTœ KOHCTAHT“
9      N1 = IK
       N2 = K72 - IK + 1
       CALL APF SQE ( KAR, E0, N1, N2, IF1 )
       GO TO ( 11, 11,   11,   10, 10, 10 ), IF1
10     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NW6,12) IF1, N1, N2, IK, IS
12     FORMAT(' ***** CO‰ B A‹ƒOPˆTME APFTEM: IF1 =',I6,
     *        ' N1=',I6,' N2=',I6, ' IK=',I3,' IS=',I4)
C!!!-------------------------------------------------------------------
       CALL SUBERR(1)
C====>
C====>
C====>
C====>
C====>
C  ‡AˆCATœ KOHCTAHT“
11     SIM(IS) = 0
       RSIM(IS + 1) = E0
       IS = IS + 2
C  KOPPEKTˆP“EM C.—.K. IK ˆ PO„O‹†AEM
       IK = N1 - 1
       GO TO ( 3, 3, 8 ), IF1
C!!!-------------------------------------------------------------------
       GO TO 10
C!!!-------------------------------------------------------------------
C  PˆHŸTœ C‹“†EH›‰ ‡HAK
4      CONTINUE
C   B›TO‹KH“Tœ FCAR, EC‹ˆ HEOXO„ˆMO
       IF ( FLAG .EQ. 0 ) GO TO 13
       FLAG = 0
       CALL APF PAK ( 1, IDKOD, FCAR )
       SIM(IS) = IDKOD
       IS = IS + 1
C
13     SIM(IS) = -KAR(IK)
       IS = IS + 1
       GO TO 3
C   OPAOTKA ˆ„EHTˆ”ˆKATOPA
7      CONTINUE
       IF ( FLAG .EQ. 1 ) GO TO 14
C  EPB›‰ BXO„: HA—A‹O ˆ„EHTˆ”ˆKATOPA
       FLAG = 1
       IC = 1
       FCAR(1) = 11
       FCAR(2) = 11
       FCAR(3) = 11
       FCAR(4) = 11
       GO TO 15
C  ˆƒHOPˆPOBATœ „‹ˆHH›‰ K“COK ˆ„EHTˆ”ˆKATOPA ( > 4 )
14     CONTINUE
       IF ( IC .GT. 4 ) GO TO 3
C ‡AˆCATœ CˆMBO‹
15     FCAR(IC) = KAR(IK)
       IC =IC + 1
       GO TO 3
C======>  OŸTœ POE‹›, HE KOHE– ‹ˆ TO ˆ„EHTˆ”ˆKATOPA ? /01.02.86/
30      CONTINUE
         IF ( FLAG .EQ. 0 ) GO TO 3
         FLAG = 0
         CALL APF PAK (1, IDKOD, FCAR)
         SIM(IS) = IDKOD
         IS = IS + 1
         GO TO 3
C----------------------------------------------------------------------
C   KOH–OBKA O™EƒO –ˆK‹A  ********************************************
C----------------------------------------------------------------------
3      CONTINUE
       IK = IK + 1
       IF ( IK .LE. K72 ) GO TO 2
       IF ( FLAG .EQ. 0 ) GO TO 16
C  B KOH–E B›TO‹KHEM FCAR
       FLAG = 0
       CALL APF PAK ( 1, IDKOD, FCAR )
       SIM(IS) = IDKOD
       IS = IS + 1
C  KOH—AEM
16     KSIM = IS - 1
       IF ( KSIM .LT. 0 ) KSIM = 0
C  EPE‰„EM K A-KOD“
       CALL APF TRT ( 2, K72, KAR, KAR )
       CALL APF TRT ( 2,  80, ER, ER )
C!!!-------------------------------------------------------------------
       IF ( KSIM. GT. 100 ) WRITE ( NW6,17) KSIM
17     FORMAT(' ***** CO‰ B APFTEM; KSIM=',I10 )
C!!!-------------------------------------------------------------------
       RETURN
       END
