         SUBROUTINE APFCAS ( FUN, KCAR, LCAR, BCAR )
CXXX ---KOM‹EKC "MS1" --> BEPCˆŸ 1.5  (OT ˆž‹Ÿ    1989 ƒ.)-------------
C      O„POƒPAMMA APFCAS ( FUN, KCAR, LCAR, BCAR )
C      ------------
C      HA‡HA—EHˆE: EPEKO„ˆPOBKA "BHE˜HEƒO" A-PE„CTAB‹EHˆŸ
C      ----------  CˆMBO‹œHO‰ CTPOKˆ BBE„EHHO‰ B CPE„“ ”OPTPAH-A
C                  ”OPMATOM A1, BO "BHE˜HEE" A-PE„CTAB‹EHˆE
C                  ˆ OPATHO;
C                  EPEKO„ˆPOBKˆ ‡AK‹ž—AETCŸ B ‡AMEHE CˆMBO‹OB
C                  CTPOKˆ: <POˆCH›E “KB›> <==> <c’po—›e yŠ‚›>
C      METO„:  OˆCK COOTBETCTBˆŸ B TA‹ˆ–E CˆMBO‹OB;
C      -----
C      APAMETP›: FUN - =1 ˆ‡ LCAR  B  BCAR,
C      ---------        =2 ˆ‡ BCAR  B  LCAR;
C                 KCAR - KO‹ˆ—ECTBO CˆMBO‹OB B MACCˆBAX,
C                          PA‡MEPHOCTœ ACAR ˆ FCAR;
C                 LCAR - MACCˆB "BHE˜HEƒO" PE„CTAB‹EHˆŸ,
C                        CO„EP†A™ˆ‰ "MA‹EHœKˆE" “KB›
C
C                 BCAR - MACCˆB "BHE˜HEƒO" PE„CTAB‹EHˆŸ,
C                        CO„EP†A™ˆ‰ "O‹œ˜ˆE" “KB›
C
C                 BCE APAMETP› INTEGER*4;
C
C      O„POƒPAMM›: HET   ( SUBERR ), COMMON: PRINTP
C      ------------
C      PˆME—AHˆE: 1. TA‹ˆ–A CˆMBO‹OB “OPŸ„O—EHA !!!;
C      ----------- 2. Pˆ KOPEKTˆPOBKE --->  CM. APFTRT
CZZZ--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
        COMMON/PRINTN/ NW6
C!!!------------------------------------------------------------------
        DIMENSION LCAR(1),BCAR(1),LTAB(98),BTAB(98)
        INTEGER BCAR, LCAR, LTAB, FUN, XXX, BTAB
        DATA NTAB/98/, XXX/'?'/
        DATA BTAB /
     *  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',  '8',  '9',
     1  ' ',  '+',  '-',  '*',  '/',  '(',  ')',  '=',  ';',  '[',
     2  ']',  '>',  '<',  '^',  ':',  ',',  '.',  '''', '"',  '%',
     3  '&',  '!',  '?',  '_',  '#',  '@',  '$',  'A',  'B',  'C',
     4  'D',  'E',  'F',  'G',  'H',  'I',  'J',  'K',  'L',  'M',
     5  'N',  'O',  'P',  'Q',  'R',  'S',  'T',  'U',  'V',  'W',
     6  'X',  'Y',  'Z',  '',  'ƒ',  '„',  '†',  '‡',  'ˆ',  '‰',
     7  '‹',  '',  '“',  '”',  '–',  '—',  '˜',  '™',  'œ',  '›',
     8  '',  'ž',  'Ÿ',  'T',  'M',  'B',  'A',  'H',  'E',  'K',
     9  'X',  'B',  'A',  'P',  'O',  'C',  'M',  'T'               /
C!!!!!!!!!!!!! „‹Ÿ RUS CBM B TEXT ˆCPABˆTœ — HA —————!!!!!!!!!!!!!!!!!!
        DATA LTAB /
     *  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',  '8',  '9',
     1  ' ',  '+',  '-',  '*',  '/',  '(',  ')',  '=',  ';',  '[',
     2  ']',  '>',  '<',  '^',  ':',  ',',  '.',  '''', '"',  '%',
     3  '&',  '!',  '?',  '_',  '#',  '@',  '$',  'a',  'b',  'c',
     4  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',
     5  'n',  'o',  'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
     6  'x',  'y',  'z',  '¡',  '£',  '¤',  '¦',  '§',  '¨',  '©',
     7  '«',  '¯',  'ã',  'ä',  'æ',  'ç',  'è',  'é',  'ì',  'ë',
     8  'í',  'î',  'ï',  'â',  '¬',  '¢',  ' ',  '',  '…',  'Š',
     9  '•',  '‚',  '€',  '',  'Ž',  '‘',  'Œ',  '’'               /
C----------------------------------------------------------------------
         GO TO (1,2), FUN
C!!!-------------------------------------------------------------------
           WRITE(NW6,10) FUN
10         FORMAT(' ***** CO‰ Pˆ OPA™EHˆˆ K APFCAS, FUN=',I10)
           CALL SUBERR(1)
C!!!-------------------------------------------------------------------
C      ˆ‡   LCAR    B   BCAR
1         CONTINUE
          DO 50  I=1,KCAR
          DO 51 J=1, NTAB
          JF=J
          IF ( LCAR(I) .EQ. LTAB(J) )  GO TO 53
51        CONTINUE
          BCAR(I)=LCAR(I)
          GO TO 50
53        BCAR(I)=BTAB(JF)
50        CONTINUE
          RETURN
C     ˆ‡  BCAR  B  LCAR
2         CONTINUE
          DO 250  I=1,KCAR
          DO 251 J=1, NTAB
          JF=J
          IF ( BCAR(I) .EQ. BTAB(J) )  GO TO 253
251        CONTINUE
          LCAR(I)=BCAR(I)
          GO TO 250
253        LCAR(I)=LTAB(JF)
250        CONTINUE
          RETURN
          END
