         SUBROUTINE APFCAS ( FUN, KCAR, LCAR, BCAR )
CXXX ---KOM��EKC "MS1" --> BEPC�� 1.5  (OT ����    1989 �.)-------------
C      �O��PO�PAMMA APFCAS ( FUN, KCAR, LCAR, BCAR )
C      ------------
C      HA�HA�EH�E: �EPEKO��POBKA "BHE�HE�O" A-�PE�CTAB�EH��
C      ----------  C�MBO��HO� CTPOK� BBE�EHHO� B CPE�� �OPTPAH-A
C                  �OPMATOM A1, BO "BHE�HEE" A-�PE�CTAB�EH�E
C                  � O�PATHO;
C                  �EPEKO��POBK� �AK���AETC� B �AMEHE C�MBO�OB
C                  CTPOK�: <�PO��CH�E ��KB�> <==> <c�po���e �y���>
C      METO�:  �O�CK COOTBETCTB�� B TA����E C�MBO�OB;
C      -----
C      �APAMETP�: FUN - =1 �� LCAR  B  BCAR,
C      ---------        =2 �� BCAR  B  LCAR;
C                 KCAR - KO���ECTBO C�MBO�OB B MACC�BAX,
C                          PA�MEPHOCT� ACAR � FCAR;
C                 LCAR - MACC�B "BHE�HE�O" �PE�CTAB�EH��,
C                        CO�EP�A��� "MA�EH�K�E" ��KB�
C
C                 BCAR - MACC�B "BHE�HE�O" �PE�CTAB�EH��,
C                        CO�EP�A��� "�O����E" ��KB�
C
C                 BCE �APAMETP� INTEGER*4;
C
C      �O��PO�PAMM�: HET   ( SUBERR ), COMMON: PRINTP
C      ------------
C      �P�ME�AH�E: 1. TA����A C�MBO�OB ��OP��O�EHA !!!;
C      ----------- 2. �P� KOPEKT�POBKE --->  CM. APFTRT
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
     6  'X',  'Y',  'Z',  '�',  '�',  '�',  '�',  '�',  '�',  '�',
     7  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',
     8  '�',  '�',  '�',  'T',  'M',  'B',  'A',  'H',  'E',  'K',
     9  'X',  'B',  'A',  'P',  'O',  'C',  'M',  'T'               /
C!!!!!!!!!!!!! ��� RUS CBM B TEXT �C�PAB�T� � HA �����!!!!!!!!!!!!!!!!!!
        DATA LTAB /
     *  '0',  '1',  '2',  '3',  '4',  '5',  '6',  '7',  '8',  '9',
     1  ' ',  '+',  '-',  '*',  '/',  '(',  ')',  '=',  ';',  '[',
     2  ']',  '>',  '<',  '^',  ':',  ',',  '.',  '''', '"',  '%',
     3  '&',  '!',  '?',  '_',  '#',  '@',  '$',  'a',  'b',  'c',
     4  'd',  'e',  'f',  'g',  'h',  'i',  'j',  'k',  'l',  'm',
     5  'n',  'o',  'p',  'q',  'r',  's',  't',  'u',  'v',  'w',
     6  'x',  'y',  'z',  '�',  '�',  '�',  '�',  '�',  '�',  '�',
     7  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',
     8  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',
     9  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�'               /
C----------------------------------------------------------------------
         GO TO (1,2), FUN
C!!!-------------------------------------------------------------------
           WRITE(NW6,10) FUN
10         FORMAT(' ***** C�O� �P� O�PA�EH�� K APFCAS, FUN=',I10)
           CALL SUBERR(1)
C!!!-------------------------------------------------------------------
C      ��   LCAR    B   BCAR
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
C     ��  BCAR  B  LCAR
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