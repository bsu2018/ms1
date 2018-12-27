         SUBROUTINE APFSQE  ( KOD, E0, N1, N2, IF )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)----------
C    O„POƒPAMMA    APFSQE(KOD,E0,N1,N2,IF)
C
C      HA‡HA—EHˆE: EPEBO„ —ˆC‹A, ‡AˆCAHHOƒO –ˆ”POB›Mˆ CˆMBO‹AMˆ B
C      ----------  „ECŸTˆ—HOE —ˆC‹O C ‹ABAž™E‰ TO—KO‰ ( REAL*4 );
C
C      METO„:      O‹EMEHTHOE PEOPA‡OBAHˆE (C‹EBA HAPABO) ˆCXO„H›X
C      -----       –ˆ”POB›X „AHH›X, PE„CTAB‹Ÿž™ˆX BHE˜H“ž ”OPM“ ‡AˆCˆ
C                  PEOPA‡“EMOƒO —ˆC‹A;
C
C      APAMETP›:  KOD - MACCˆB „AHH›X „‹Ÿ PEOPA‡OBAHˆŸ ( B F-KO„E.);
C      ---------   E0  - PEOPA‡OBAHHOE „ECŸTˆ—HOE —ˆC‹O ( REAL*4 );
C                  N1  - HOMEP ‹EMEHTA B MACCˆBE KOD HA—A‹A ˆH”OPMA–ˆˆ
C                         O„‹E†A™E‰ PEOPA‡OBAHˆž;
C                  N2  - KO‹ˆ—ECTBO ‹EMEHTOB „OCT“H›X „‹Ÿ
C                         PEOPA‡OBAHˆŸ;
C                  IF  - ˆH„ˆKATOP PEOPA‡OBAHˆŸ:
C                      IF = 1  - Pˆ ‹AƒOO‹“—HOM PEOPA‡OBAHˆˆ:
C                                Pˆ TOM: N1 = N1 + N2; N2 = 0;
C
C                      IF = 2  MACCˆB KOD HE KOH—ˆ‹CŸ, OHAP“†EH CˆMBO‹
C                              "HE –ˆ”PA",B N1 - HOMEP B MACCˆBE KOD,
C                               COOTBETCTB“ž™ˆ‰ OHAP“†EHHOM“ HE –ˆ”-
C                               POBOM“ CˆMBO‹“; B N2 - „‹ˆHA ( KO‹-BO
C                               ‹EMEHTOB B MACCˆBE KOD ) OCTAB˜E‰CŸ
C                               —ACTˆ - E‡ CˆMBO‹A "HE –ˆ”PA" -
C                                B MACCˆBE KOD;
C                               E0 - PEOPA‡OBAHHOE —ˆC‹O, PE„CTAB‹EH-
C                               HOE –ˆ”PAMˆ OT N1 „O CˆMBO‹A "HE –ˆ”PA";
C                     IF = 3 - Pˆ PEOPA‡OBAHˆˆ —ˆC‹A POˆ‡O˜‹O
C                              EPEO‹HEHˆE; Pˆ TOM CTPOKA ( MACCˆB
C                              KOD ) PO„O‹†AET POCMATPˆBATœCŸ.
C                         A)  Pˆ BCTPE—E CˆMBO‹A "HE –ˆ”PA" -
C                             N1 - A„PEC "HE –ˆ”PA" ( HOMEP B KOD ), A
C                              N2 - „‹ˆHA OCTAB˜E‰CŸ —ACTˆ, E‡ CˆMBO‹A
C                                "HE –ˆ”PA";
C                         )  EC‹ˆ CˆT“A–ˆŸ "HE –ˆ”PA" HE ‡A”ˆKCˆPOBAHA,
C                             A O‹E ( MACCˆB KOD ) ˆC—EPAHO, TO:
C                              N1=N1+N2, N2=0.
C                          BO BCEX C‹“—AŸX IF=3, PˆHˆMAETCŸ E0=MAX/MIN
C                          ( B ‡ABˆCˆMOCTˆ OT ‡HAKA OPŸ„KA ) ‡HA—EHˆž
C                          REAL*4 B PE„CTAB‹EHˆˆ HA ˆCO‹œ‡“EMO‰ BM.
C
C                    IF = 4 - N2 ‡A„AH MEHœ˜E H“‹Ÿ ( N2<0 ), Pˆ TOM
C                          E0=0, N1 ˆ N2 E‡ ˆ‡MEHEHˆŸ
C                    IF = 5 - N2 ‡A„AHO PABHO‰ H“‹ž ( N2=0 ), Pˆ TOM
C                          E0=0, N1=N1+1, N2=0;
C                    IF = 6 - BO‡HˆKAET Pˆ O›TKE PEOPA‡OBAHˆŸ
C                         O‹Ÿ ( MACCˆBA KOD ) COCTOŸ™EƒO ˆ‡ O„HˆX
C                         HE‡HA—E™ˆX POE‹OB ˆ‹ˆ CˆMBO‹OB:
C                       <.>,<E>,<->,<+>; Pˆ TOM: E0=0, N1=N1+N2, N2=0;
C
C      O„POƒPAMM›: APFTRT;
C      ------------
C
C
C      ----------     COƒ‹ACHO TA‹ˆ–E B O„POƒPAMM› APFTRT .
C                    ( Pˆ TOM <–ˆ”PA> = FKOD(I) - 1 );
C
C
CZZZ -------------------------------------------------------------------
         INTEGER * 4  P, P1, TKOD, LIMITM, LIMITP
         DIMENSION KOD(1), TKOD(15)
         DATA IVV/0/, LIMITM/1677216/, LIMITP/72/
         DATA EMAX/0.99E+31/
         DATA TKOD/ '0','1','2','3','4','5','6','7','8','9',
     *             ' ', '+', '-', '.', 'E' /
         IF ( IVV .EQ. 1 ) GO TO 1
C  EPB›‰ BXO„: EPEKO„ˆP. TKOD
         IVV=1
         CALL  APFTRT(1, 15,  TKOD,  TKOD  )
C----------------------------------------------------------------------
1        CONTINUE
         IF = 4
         IF ( N2 ) 100, 101, 2
2         NMAX = N1 + N2 -1
          IT=N1
4         CONTINUE
          IF ( KOD(IT) .NE. TKOD(11) ) GO TO 3
          IT = IT + 1
          IF ( IT - NMAX ) 4, 4, 108
3         IZM=+1
          IF ( KOD(IT) .EQ. TKOD(12) .OR.
     *         KOD(IT) .EQ. TKOD(13) .OR.
     *         KOD(IT) .EQ. TKOD(14) .OR.
     *         KOD(IT) .EQ. TKOD(15) )    GO TO 5
          GO TO 6
5         CONTINUE
          IF ( KOD(IT) .EQ. TKOD(15) ) GO TO 7
          IF ( KOD(IT) .EQ. TKOD(14) ) GO TO 8
          IF ( KOD(IT) .EQ. TKOD(13) ) IZM=-1
          IT = IT + 1
          IF ( IT - NMAX ) 6, 6, 108
7         M=1
          P=0
          P1=0
          IT = IT +1
          IF ( IT - NMAX ) 103, 103, 108
8         M=0
          P=0
          P1=0
          GO TO 104
6         M=0
          P=0
          P1=0
12        CONTINUE
          IF ( KOD(IT) . LT. TKOD(1) .OR.
     *         KOD(IT) . GT. TKOD(10)  )  GO TO  9
       IF ( M .LE. LIMITM) GO TO 10
          P = P + 1
          GO TO 11
10        M = M*10 + KOD(IT) - 1
11        IT = IT +1
          IF ( IT - NMAX ) 12, 12, 102
9         CONTINUE
          IF ( KOD(IT) .EQ. TKOD(14) ) GO TO 104
21        CONTINUE
          IF ( KOD(IT) .EQ. TKOD(15) ) GO TO 13
C$$$$$$$$$IF ( KOD(IT) .EQ. TKOD(12) .OR.
C$$$$$$$$$     KOD(IT) .EQ. TKOD(13)   )  GO TO 105
          IF = 2
          N1 = IT
          N2 = NMAX - IT
          GO TO 106
13        IT = IT + 1
          IF ( IT - NMAX ) 14, 14, 102
14        P1 = 0
C         PA‡OP OPŸ„KA ( ‡HAK )
103       IZP= +1
          IF ( KOD(IT) .EQ. TKOD(12) .OR.
     *         KOD(IT) .EQ. TKOD(13)  )  GO TO  105
          GO TO 15
C         PA‡OP OPŸ„KA  ( —ˆC‹O )
105       CONTINUE
          IF ( KOD(IT) .EQ. TKOD(13) ) IZP= -1
          IT = IT + 1
          IF ( IT - NMAX ) 15, 15, 102
C         –ˆK‹ O –ˆ”PAM OPŸ„KA
15        CONTINUE
          IF ( KOD(IT) .LT. TKOD(1) .OR.
     *         KOD(IT) .GT. TKOD(10)  )  GO TO 16
          IPP = IABS( P + IZP*P1)
          IF ( IPP - LIMITP ) 18,18,17
17        CONTINUE
C  EPEO‹HEHˆE OPŸ„KA, POCMOTP „O KOH–A
       P = P + IZP*P1
53     IT= IT + 1
       IF ( IT - NMAX ) 50, 50, 51
51     N1 = N1 + N2
       N2 = 0
       GO TO 107
50     IF ( KOD(IT) .LT. TKOD(1) .OR. KOD(IT) .GT. TKOD(10) ) GO TO 52
       GO TO 53
52     N1 = IT
       N2 = NMAX - IT
          GO TO 107
C         PˆHŸTœ –ˆ”P“ OPŸ„KA
18        P1 = P1*10 + KOD(IT) - 1
          IT = IT + 1
          IF ( IT - NMAX ) 15, 15, 19
19        P = P + IZP * P1
          GO TO 102
C         HE –ˆ”PA
16        P = P + IZP * P1
          IF = 2
          N1 = IT
          N2 = NMAX - IT
          GO TO 106
C         POCMOTP ‡A TO—KO‰
104       IT = IT + 1
          IF ( IT - NMAX ) 20, 20, 102
20        CONTINUE
          IF ( KOD(IT) .LT. TKOD(1) .OR.
     *         KOD(IT) .GT. TKOD(10)  )  GO  TO  21
          IF ( M .GT. LIMITM ) GO TO 22
          M = M*10 + KOD(IT) - 1
          P = P - 1
22        IT = IT + 1
          IF ( IT - NMAX ) 20,20, 102
C         “XO„›:
101       IF = 5
          N1 = N1 + 1
          E0 = 0.0
          N2 = 0
          GO TO 100
C
108       IF=6
          N1=N1+1
          E0=0.0
          N2=0
          GO TO 100
C
102       IF = 1
          N1 = N1 + N2
          N2 = 0
C
106       CONTINUE
          IF ( IABS(P) .GT. LIMITP ) GO TO 107
          E0 = M
          P10 = 10.0**P
          E0 = IZM * E0 * P10
          GO TO 100
C
107       IF = 3
          E0 = IZM * EMAX
          IF ( P. LT. 0 ) E0 = 0.0
          GO TO 100
100       RETURN
          END
