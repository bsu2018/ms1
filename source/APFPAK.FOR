       SUBROUTINE APFPAK ( FUN, IDKOD, FCAR )
CXXX ---KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C      O„POƒPAMMA: APFPAK ( FUN, IDKOD, FCAR )
C      ------------
C      HA‡HA—EHˆE: POˆ‡BO„ˆT KO„ˆPOBAHˆE 4-X ‹EMEHTOB MACCˆBA FCAR
C      ----------  B C†AT›‰ ”OPMAT - B O„HO —ˆC‹O IDKOD ˆ OPATHO;
C      METO„:      PE„CTAB‹EHˆE —ˆC‹A O „ECŸTˆ—H›M „ˆA„AM;
C      ------
C      APAMETP›: FUN - =1  O FCAR CO‡„AETCŸ IDKOD,
C      ---------        =2  O IDKOD CO‡„AETCŸ FCAR;
C                 IDKOD - ˜ˆ”P ˆ„EHTˆ”ˆKATOPA ( CˆHTE‡ ˆ‡ 4-X );
C                 FCAR  - MACCˆB, PA‡MEPHOCTˆ 4, C ‹EMEHTAMˆ B F-KODE;
C                  BCE APAMETP› B INTEGER*4; FCAR(I) <= 99;
C
C      O„POƒPAMM›: HET; (SUBERR, COMMON: PRINTN)
C
CZZZ-------------------------------------------------------------------
C!!!---------------
         COMMON/PRINTN/ NW6
         DIMENSION FCAR(*)
         INTEGER FUN, FCAR
C!!!---------------
         DATA KP1 / 1 00 00 00 /,
     *        KP2 /    1 00 00 /,
     *        KP3 /       1 00 /
         GO TO ( 1, 2 ), FUN
C!!!--------------
         WRITE(NW6,10) FUN
10       FORMAT(' ***** CO‰ Pˆ OPA™EHˆˆ K APFPAK, FUN=',I10)
         CALL SUBERR(1)
C!!!--------------
C     O FCAR  „E‹AEM IDKOD
1        IDKOD = KP1 * FCAR(1) +
     *           KP2 * FCAR(2) +
     *           KP3 * FCAR(3) +
     *                 FCAR(4)
         RETURN
C   O  IDKOD  „E‹AEM FCAR(1...4)
2        FCAR(1) = IDKOD/KP1
         IX1 = IDKOD - FCAR(1)*KP1
         FCAR(2) = IX1/KP2
         IX2 = IX1 - FCAR(2)*KP2
         FCAR(3) = IX2/KP3
         FCAR(4) =  IX2 - FCAR(3)*KP3
         RETURN
         END
