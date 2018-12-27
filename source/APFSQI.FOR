         SUBROUTINE APFSQI  ( KOD, N0, N1, N2, IF )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)----------
C    O„POƒPAMMA    APFSQI(KOD,N0,N1,N2,IF)
C
C      HA‡HA—EHˆE: EPEBO„ —ˆC‹A, ‡AˆCAHHOƒO –ˆ”POB›Mˆ CˆMBO‹AMˆ B
C      ----------  „ECŸTˆ—HOE —ˆC‹O C ”ˆKCˆPOBAHHO‰ TO—KO‰ (INTEGER*4)
C
C      METO„:      O‹EMEHTHOE PEOPA‡OBAHˆE (C‹EBA HAPABO) ˆCXO„H›X
C      -----       –ˆ”POB›X „AHH›X, PE„CTAB‹Ÿž™ˆX BHE˜H“ž ”OPM“ ‡AˆCˆ
C                  PEOPA‡“EMOƒO —ˆC‹A;
C
C      APAMETP›:  KOD - MACCˆB „AHH›X „‹Ÿ PEOPA‡OBAHˆŸ ( B F-KO„E.);
C      ---------   N0  - PEOPA‡OBAHHOE –E‹OE —ˆC‹O ( INTEGER*4 );
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
C                               N0 - PEOPA‡OBAHHOE —ˆC‹O, PE„CTAB‹EH-
C                               HOE –ˆ”PAMˆ OT N1 „O CˆMBO‹A "HE –ˆ”PA";
C                               (ˆ‹ˆ PABHOE 0 );
C                     IF = 3 - Pˆ PEOPA‡OBAHˆˆ —ˆC‹A POˆ‡O˜‹O
C                              EPEO‹HEHˆE; Pˆ TOM CTPOKA ( MACCˆB
C                              KOD ) PO„O‹†AET POCMATPˆBATœCŸ.
C                         A)  Pˆ BCTPE—E CˆMBO‹A "HE –ˆ”PA" -
C                             N1 - A„PEC "HE –ˆ”PA" ( HOMEP B KOD ), A
C                             N2 - „‹ˆHA OCTAB˜E‰CŸ —ACTˆ, E‡ CˆMBO‹A 0
C                                "HE –ˆ”PA";
C                         )  EC‹ˆ CˆT“A–ˆŸ "HE –ˆ”PA" HE ‡A”ˆKCˆPOBAHA,
C                             A O‹E ( MACCˆB KOD ) ˆC—EPAHO, TO:
C                              N1=N1+N2, N2=0.
C                          BO BCEX C‹“—AŸX IF=3, PˆHˆMAETCŸ N0=MAX/MIN
C                          ( B ‡ABˆCˆMOCTˆ OT ‡HAKA )  ‡HA—EHˆž
C                       INTEGER*4 B PE„CTAB‹EHˆˆ HA ˆCO‹œ‡“EMO‰ BM.
C
C                    IF = 4 - N2 ‡A„AH MEHœ˜E H“‹Ÿ ( N2<0 ), Pˆ TOM
C                          N0=0, N1 ˆ N2 E‡ ˆ‡MEHEHˆŸ
C                    IF = 5 - N2 ‡A„AHO PABHO‰ H“‹ž ( N2=0 ), Pˆ TOM
C                          N0=0, N1=N1+1, N2=0;
C                    IF = 6 - BO‡HˆKAET Pˆ O›TKE PEOPA‡OBAHˆŸ
C                         O‹Ÿ ( MACCˆBA KOD ) COCTOŸ™EƒO ˆ‡ O„HˆX
C                         HE‡HA—E™ˆX POE‹OB ˆ‹ˆ CˆMBO‹OB:
C                               <->,<+>; Pˆ TOM: N0=0, N1=N1+N2, N2=0;
C
C      O„POƒPAMM›: APFTRT;
C      ------------
C
C      PˆME—AHˆE:   PE„O‹OƒAETCŸ, —TO MACCˆB B F-KO„E “OPŸ„O—EH
C      ----------     COƒ‹ACHO TA‹ˆ–E B O„POƒPAMM› APFTRT .
C                    ( Pˆ TOM <–ˆ”PA> = FKOD(I) - 1 );
C
C
CZZZ ------------------------------------------------------------------_
         INTEGER * 4  P, P1, TKOD, LIMITM, LIMITP, LIMIT
         DIMENSION KOD(1), TKOD(13)
         DATA IVV/0/, LIMITM/-2147483647/, LIMITP/+2147483647/
         DATA                        LIMIT/214748364/
         DATA TKOD/'0','1','2','3','4','5','6','7','8','9',
     *             ' ', '+', '-' /
         IF ( IVV .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C EPB›‰ BXO„: O„PABˆM LIMITM
         LIMITM = LIMITM - 1
C  EPB›‰ BXO„: EPEKO„ˆP. TKOD
         IVV=1
         CALL  APFTRT(1, 13,  TKOD,  TKOD  )
C----------------------------------------------------------------------
1        CONTINUE
         NMAX = N1 + N2 -1
         IF = 4
         N0 = 0
         IF ( N2 .LT. 0 ) GO TO 100
         IF ( N2 .NE. 0 ) GO TO 3
C=====>  N2 = 0
         N0 = 0
         N1 = N1 + 1
         N2 = 0
         IF = 5
         GO TO 100
C----------------------------------------------------------------------
C    “EPEM POE‹›
C----------------------------------------------------------------------
3        DO 4 I=N1,NMAX
         IT = I
         IF ( KOD(I) .NE. TKOD(11) ) GO TO 5
4        CONTINUE
C=====> O„Hˆ POE‹›
         N0 = 0
         IF = 6
         N1 = N1 + N2
         N2 = 0
         GO TO 100
C====> ECTœ ‹ˆ ‡HAK ?  (IZ=0 =EQ= "+")
5        CONTINUE
         IZ = 0
         IF ( KOD(IT) .EQ. TKOD(12)  .OR. KOD(IT) .EQ. TKOD(13) )
     =                       GO TO 60
         GO TO 6
60       CONTINUE
         IF ( KOD(IT) .EQ. TKOD(13) ) IZ = -1
C====> HA HOB›‰ CˆMBO‹
         IT = IT + 1
         IF ( IT .LE. NMAX ) GO TO 6
C=====> O‹E KOH—ˆ‹OCœ TO‹œKO O„ˆH ‡HAK...
         IF = 6
         N0 = 0
         N1 = N1 + N2
         N2 = 0
         GO TO 100
C
C  ==== –ˆK‹ PˆEMA —ˆCE‹ ====
C
6        CONTINUE
         ID = 0
         I1 = IT
         IF = 2
         N0 = 0
         DO 7 I=I1, NMAX
         IT = I
         IF ( KOD(I) .LT. TKOD(1)  .OR.  KOD(I) .GT. TKOD(10) )
     =                       GO TO 8
C========> HA˜A –ˆ”PA...
         IF ( N0 .GT. LIMIT ) GO TO 9
         N0 = N0 * 10
         IF ( N0 .EQ. 10*LIMIT .AND. KOD(I) .GT. 7 .AND. IZ .EQ. 0 )
     =                       GO TO 9
         IF ( N0 .EQ. 10*LIMIT .AND. IZ .EQ. -1 ) GO TO 10
C========>  BCE HOPMA‹œHO
         N0 = N0 +(KOD(I) - 1)
         GO TO 7
C=======> Pˆ  OTP. —ˆC‹E "B PˆT›K" K MIN. „O“CTˆMOM“
10       ID = -1
         NX =(KOD(I) - 1) -1
         N0 = N0 + NX
7        CONTINUE
C======> O‹E KOH—ˆ‹OCœ, BCE PA‡OPA‹ˆ
         IF = 1
         N1 = N1 + N2
         N2 = 0
C======> KAK ‡HAK ?
         IF ( IZ .EQ. 0 ) GO TO 100
         N0 = - N0
         IF ( ID .EQ. -1 ) N0 = N0 -1
         GO TO 100
C**********************************************************************
C  OCO›E C‹“—Aˆ
C**********************************************************************
8        CONTINUE
         IF = 2
         N1 = IT
         N2 = NMAX - IT
         IF ( IZ .EQ. 0 ) GO TO 100
         N0 = -N0
         IF ( ID .EQ. -1 ) N0 = N0 -1
         GO TO 100
C====>  EPEO‹HEHˆE.....
9        CONTINUE
         N0 = LIMITP
         IF ( IZ .EQ. -1 ) N0 = LIMITM
         IF = 3
C====CKAHˆP“EM „A‹œ˜E....
         I1 = IT
         DO 12 I=I1, NMAX
         IT = I
         IF ( KOD(I) .LT. TKOD(1)  .OR.  KOD(I) .GT. TKOD(10) )
     =                            GO TO 11
12       CONTINUE
C======> O‹E KOH—ˆ‹OCœ, HO EPEO‹HEHˆE....
         N1 = N1 + N2
         N2 = 0
         GO TO 100
C======> BCTETˆ‹ACœ HE –ˆ”PA
11       N1 = IT
         N2 = NMAX - IT
         GO TO 100
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
100      R E T U R N
         END
