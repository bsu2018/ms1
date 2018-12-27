         SUBROUTINE APFTRT ( FUN, KCAR, ACAR, FCAR )
CXXX ---KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)-------------
C      O„POƒPAMMA APFTRT ( FUN, KCAR, ACAR, FCAR )
C      ------------
C      HA‡HA—EHˆE: EPEKO„ˆPOBKA ˆ‡ "BHE˜HEƒO" A-PE„CTAB‹EHˆŸ
C      ----------  CˆMBO‹œHO‰ CTPOKˆ BBE„EHHO‰ B CPE„“ ”OPTPAH-A
C                  ”OPMATOM A1, BO "BH“TPEHHEE" F-PE„CTAB‹EHˆE
C                  ( INTEGER*4 ) ˆ OPATHO;
C                  EPEKO„ˆPOBKˆ ˆ‡ A-KOD B F-KOD ˆ OPATHO.
C
C      METO„:  OˆCK COOTBETCTBˆŸ B TA‹ˆ–E CˆMBO‹OB;
C      -----
C      APAMETP›: FUN - =1 ˆ‡ ACAR  B  FCAR,
C      ---------        =2 ˆ‡ FCAR  B  ACAR;
C                 KCAR - KO‹ˆ—ECTBO CˆMBO‹OB B MACCˆBAX,
C                          PA‡MEPHOCTœ ACAR ˆ FCAR;
C                 ACAR - MACCˆB "BHE˜HEƒO" PE„CTAB‹EHˆŸ: A-KOD;
C
C                 FCAR - MACCˆB "BH“TPEHHEƒO" PE„CTAB‹EHˆŸ: F-KOD;
C
C                 BCE APAMETP› INTEGER*4;
C
C      O„POƒPAMM›: HET   ( SUBERR ), COMMON: PRINTP
C      ------------
C      PˆME—AHˆE: 1. TA‹ˆ–A CˆMBO‹OB “OPŸ„O—EHA !!!;
C      ----------  2. Pˆ HEBO‡MO†HOCTˆ PEOPA‡OBAHˆŸ:
C                     B ACAR <- CˆMBO‹ ?,
C                     B FCAR <- —ˆC‹O 0;
C
CZZZ--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
        COMMON/PRINTN/ NW6
C!!!-------------------------------------------------------------------
        DIMENSION ACAR(1),FCAR(1),TAB(83)
        INTEGER ACAR, FCAR, TAB, FUN, XXX
        DATA NTAB/83/, XXX/'?'/
        DATA TAB /
     *  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',  '8',  '9',
     1  ' ',  '+',  '-',  '*',  '/',  '(',  ')',  '=',  ';',  '[',
     2  ']',  '>',  '<',  '^',  ':',  ',',  '.',  '''', '"',  '%',
     3  '&',  '!',  '?',  '_',  '#',  '@',  '$',  'A',  'B',  'C',
     4  'D',  'E',  'F',  'G',  'H',  'I',  'J',  'K',  'L',  'M',
     5  'N',  'O',  'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
     6  'X',  'Y',  'Z',  '',  'ƒ',  '„',  '†',  '‡',  'ˆ',  '‰',
     7  '‹',  '',  '“',  '”',  '–',  '—',  '˜',  '™',  'œ',  '›',
     8  '',  'ž',  'Ÿ'
     9                                                              /
C----------------------------------------------------------------------
         GO TO (1,2), FUN
C!!!-------------------------------------------------------------------
           WRITE(NW6,10) FUN
10         FORMAT(' ***** CO‰ Pˆ OPA™EHˆˆ K APFTRT, FUN=',I10)
           CALL SUBERR(1)
C!!!-------------------------------------------------------------------
C      ˆ‡   ACAR    B   FCAR
1         CONTINUE
          DO 50  I=1,KCAR
          DO 51 J=1, NTAB
          JF=J
          IF ( ACAR(I) .EQ. TAB(J) )  GO TO 53
51        CONTINUE
          FCAR(I)=0
          GO TO 50
53        FCAR(I)=JF
50        CONTINUE
          RETURN
C     ˆ‡  FCAR  B  ACAR
2         DO 60 I=1,KCAR
          JF=FCAR(I)
          IF ( JF .LE. 0  .OR.  JF .GT. NTAB ) GO TO 61
          ACAR(I)=TAB(JF)
          GO TO 60
61        ACAR(I)=XXX
60        CONTINUE
          RETURN
          END
