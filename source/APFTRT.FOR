         SUBROUTINE APFTRT ( FUN, KCAR, ACAR, FCAR )
CXXX ---KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)-------------
C      �O��PO�PAMMA APFTRT ( FUN, KCAR, ACAR, FCAR )
C      ------------
C      HA�HA�EH�E: �EPEKO��POBKA �� "BHE�HE�O" A-�PE�CTAB�EH��
C      ----------  C�MBO��HO� CTPOK� BBE�EHHO� B CPE�� �OPTPAH-A
C                  �OPMATOM A1, BO "BH�TPEHHEE" F-�PE�CTAB�EH�E
C                  ( INTEGER*4 ) � O�PATHO;
C                  �EPEKO��POBK� �� A-KOD B F-KOD � O�PATHO.
C
C      METO�:  �O�CK COOTBETCTB�� B TA����E C�MBO�OB;
C      -----
C      �APAMETP�: FUN - =1 �� ACAR  B  FCAR,
C      ---------        =2 �� FCAR  B  ACAR;
C                 KCAR - KO���ECTBO C�MBO�OB B MACC�BAX,
C                          PA�MEPHOCT� ACAR � FCAR;
C                 ACAR - MACC�B "BHE�HE�O" �PE�CTAB�EH��: A-KOD;
C
C                 FCAR - MACC�B "BH�TPEHHE�O" �PE�CTAB�EH��: F-KOD;
C
C                 BCE �APAMETP� INTEGER*4;
C
C      �O��PO�PAMM�: HET   ( SUBERR ), COMMON: PRINTP
C      ------------
C      �P�ME�AH�E: 1. TA����A C�MBO�OB ��OP��O�EHA !!!;
C      ----------  2. �P� HEBO�MO�HOCT� �PEO�PA�OBAH��:
C                     B ACAR <- C�MBO� ?,
C                     B FCAR <- ��C�O 0;
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
     6  'X',  'Y',  'Z',  '�',  '�',  '�',  '�',  '�',  '�',  '�',
     7  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',  '�',
     8  '�',  '�',  '�'
     9                                                              /
C----------------------------------------------------------------------
         GO TO (1,2), FUN
C!!!-------------------------------------------------------------------
           WRITE(NW6,10) FUN
10         FORMAT(' ***** C�O� �P� O�PA�EH�� K APFTRT, FUN=',I10)
           CALL SUBERR(1)
C!!!-------------------------------------------------------------------
C      ��   ACAR    B   FCAR
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
C     ��  FCAR  B  ACAR
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