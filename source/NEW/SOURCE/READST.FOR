       SUBROUTINE READST( KAR, IK, IKMAX, NRDRX, DSNAME, KFIZ, KODEOF )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: READST( KAR, IK, IKMAX, NRDRX, DSNAME, KFIZ, KODEOF )
C   ------------
C   HA�HA�EH�E:  BBO� CTPOK� �CXO�H�X �AHH�X ��� �/� INPUT0,
C   ----------   �EPE� KAHA� NRDRX B �OPMATE 80A1;
C                 �CTAHOBKA IK=1 � IKMAX=72 <--- BCE��A;
C   METO�:       O�EB��EH
C   -----
C   �APAMETP�:   KAR - MACC�B BBE�EHHO� �H�OPMA��� ( HE �O�EE 80 );
C   ---------    IK, IKMAX - �PAH��� "�O�E�HO�" �H�OPMA���;
C                NRDRX - KAHA� BBO�A;
C                DSNAME - MACC�B C DIM *(11) - �M� �A��A BBO�A
C                         ��� �PO�PAMM C�E�. O�C����BAH��;
C                KODEOF = 0  -  CTPOKA BBE�EHA;
C                       = 1  -  C�T�A��� END OF FILE HA NRDRX;
C                KFIZ = 1  - "AKT" BBO�A ���-KO� CTPOK� COBEP�EH;
C                     = 0  - ���-K�� CTPOK� HE BBO����;
C
C   �O��PO�PAMM�:   HET;
C   ------------
C   �P�ME�AH�E:   KOHTPO�� HE �POBO��TC�;
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION KAR(1), DSNAME(1)
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE (MASPRT(127), NZRDR)
       DATA KOD/0/, K80/80/
       KODEOF = 0
C******IF ( KOD .EQ. 1 ) GO TO 99
C----------------------------------------------------------------------
       READ(NRDRX,1, END=99 ) (KAR(I), I=1,K80)
       NZRDR = NZRDR + 1
1      FORMAT(80A1)
       IK = 1
       IKMAX = 72
       KFIZ = 1
       RETURN
99     CONTINUE
       KODEOF = 1
       KOD = 1
       RETURN
       END
