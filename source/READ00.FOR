         SUBROUTINE READ00( KOD, TIME, TOK, VALL )
CXXX----KOM��EKC "MS1" --> FOR DOS                          -----------
C   �O��PO�PAMMA:   READ00( KOD, TIME, TOK, VALL )
C   -------------
C   HA�HA�EH�E:    �TEH�E �A��A  HAKO��EH�� B MACC�B� TOK � VALL
C   ----------      ( TO�KA TIME );
C   METO�:         ��PAB�EH�E � �TEH�E �A��A  HAKO��EH��;
C   -----
C   �APAMETP�:           BXO�:
C   ---------            *****
C                  KOD = 1 - �A�POC HA OTKP�T�E �A��OB HAKO��EH��;
C                      = 2 - �TEH�E O�EPE�HO� �A��C�;
C                      = 3 - �O�BO� �A��A NSPOOL �A �A��C� NZAPSP
C                                                *****************
C
C                             ( XO�OCTOE �TEH�E NZAPSP �A��CE� );
C                         �P� �TOM TIME, TOK, VALL HE ��MEH��TC�;
C
C
C                        B�XO�:
C                        *****
C                  KOD = 1 - �E�CTB�E B��O�HEHO;
C                      = 2 - �O���ECK�� KOHE� �A��A  HAKO��EH��;
C                      = 3 - �����ECK�� KOHE� �A��A  ��� C�O�;
C                  TIME BPEM� O�PE�E�EH�� MACC�BOB:
C                  TOK � VALL - "TOKOB" � ���OB�X HA�P��EH��;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------   2. readmb
C
C   �P�ME�AH�E:    �P� OTKP�T�� �A��OB HAKO��EH�� B TOK � VALL -
C   ----------     ��PAB�����E �A��C� �A��OB, A B TIME - HOUTT -
C                  �A� �O BPEMEH� COXPAHEH�� PE�EH��;
C                  O�HA �A��C� -> TN, TOK, VALL;
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
         DIMENSION TOK(1), VALL(1)
         DIMENSION  V_A(15),  T_A(15)
         EQUIVALENCE( MASPRT(13), NWTRE ),
     =              ( MASPRT(14), NWTR  ),
     =              ( MASPRT(18), NSPOOL),
     =              ( MASPRT(92), NZAPSP),
     =              ( MASPRT(26), NSYS  ),
     =              ( MASPRT(46), NNTOK ),
     =               ( MASPRT(121), FTRSPL )
C
          INTEGER FTRSPL,  SPISNN, SPISVL, SPISTK
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         GO TO ( 1, 2, 3 ),KOD
         CALL SUBERR(KOD)
1        CONTINUE
C
       CALL KSOPEN(NSPOOL,1,ii_rr)
       IF (ii_rr .NE. 0 ) GO TO 21
C
C--->  B�BO� �A��OB B HA�A�O
         REWIND NSPOOL
C
C        READ(NSPOOL,END=20,ERR=21 ) TIME,(TOK(I), I=1, NNTOK  ),
C    =                                  (VALL(I), I=1, NSYS   )
C@@@@@@@@@@@@@@@@@
C      iikod = 0
       READ(NSPOOL,END=20,ERR=21 ) TIME, FTRSPL
C      CALL readmb(nspool,NNTOK,TOK,iikod)
C      CALL readmb(nspool,NSYS,VALL,iikod)
C       IF ( iikod .EQ. 1 ) go to 20
C       IF ( iikod .EQ. 2 ) go to 21
C@@@@@@@@@@@@@@@@@
         KOD = 1
         IF ( TIME .EQ. -1.0 ) KOD = 2
         RETURN
2        CONTINUE
C--->  B��T� O�EPE�H��
C        READ(NSPOOL, END=20, ERR=21)  TIME, (TOK(I), I=1,NNTOK ),
C    =                                      (VALL(I), I=1,NSYS  )
C@@@@@@@@@@@@@@@@@
       READ(NSPOOL,END=20,ERR=21 ) TIME
C@@@@@@@@@@@@@@@@@
       IF (FTRSPL .EQ. 0 ) THEN
          CALL readmb(nspool,NNTOK,TOK,iikod)
          CALL readmb(nspool,NSYS,VALL,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
       ELSE
          CALL readmb(nspool,SPISTK(1),T_A,iikod)
          CALL readmb(nspool,SPISVL(1),V_A,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
          DO 666 i666=1,SPISTK(1)
             nnii = SPISTK(i666+1)
666          TOK(nnii) = T_A(i666)
          DO 667 i666=1,SPISVL(1)
             nnii = SPISVL(i666+1)
667          VALL(nnii) = V_A(i666)
       ENDIF
C@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@
         KOD = 1
         IF ( TIME .EQ. -1.0 ) KOD = 2
         RETURN
20       CONTINUE
C--->  �����ECK�� KOHE�......(�A ��...)
         KOD = 3
         WRITE(NWTR, 50) NSPOOL
         WRITE(NWTRE, 50) NSPOOL
50      FORMAT(/' *** K���� 䠩�� ���������� ( ����� # ',  I2, ' ) ***'/
     = ' *** P���� �PEP�BAETC� ***'///)
         CLOSE (NSPOOL)
         RETURN
21       CONTINUE
C--->  O���K� �TEH��....
         WRITE(NWTR,51) NSPOOL
         WRITE(NWTRE,51) NSPOOL
51       FORMAT(/' *** C��� �⥭�� 䠩�� ���������� ( ����� # ', I2,
     =  ' ) ***'/  ' *** PA�OTA �PEP�BAETC� ***'///)
         KOD = 3
         CLOSE (NSPOOL)
         RETURN
C----------------------------------------------------------------------
3      CONTINUE
       KOD = 1
C
       CALL KSOPEN(NSPOOL,1,ii_rr)
       IF (ii_rr .NE. 0 ) GO TO 21
C
       REWIND NSPOOL
       READ(NSPOOL,END=20,ERR=21 ) TIME, FTRSPL
       IF ( NZAPSP  .LE. 1 ) RETURN
       DO 300 NZ = 1, NZAPSP-1
       NZX = NZ
C@@@@@
C      READ(NSPOOL, END=200, ERR=210 )
C@@@@@
C@@@@@@@@@@@@@@@@@
       READ(NSPOOL,END=20,ERR=21 ) TIME
C@@@@@@@@@@@@@@@@@
       IF (FTRSPL .EQ. 0 ) THEN
          CALL readmb(nspool,NNTOK,TOK,iikod)
          CALL readmb(nspool,NSYS,VALL,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
       ELSE
          CALL readmb(nspool,SPISTK(1),T_A,iikod)
          CALL readmb(nspool,SPISVL(1),V_A,iikod1)
          IF ( iikod .EQ. 1 .OR. iikod1 .EQ. 1 ) go to 20
          IF ( iikod .EQ. 2 .OR. iikod1 .EQ. 1 ) go to 21
          DO 668 i666=1,SPISTK(1)
             nnii = SPISTK(i666+1)
668          TOK(nnii) = T_A(i666)
          DO 669 i666=1,SPISVL(1)
             nnii = SPISVL(i666+1)
669          VALL(nnii) = V_A(i666)
       ENDIF
C@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@
CCC    READ(NSPOOL,END=20,ERR=21 )
CCC    CALL readmx(nspool,NNTOK,TOK,iikod)
CCC    CALL readmx(nspool,NSYS,VALL,iikod)
CCC     IF ( iikod .EQ. 1 ) go to 200
CCC     IF ( iikod .EQ. 2 ) go to 210
C@@@@@@@@@@@@@@@@@
300    CONTINUE
       RETURN
C----------------------------------------------------------------------
200    WRITE(NWTRE, 52 ) NZX
       WRITE(NWTR, 52 ) NZX
52     FORMAT(/' *** �� �A��A HAKO��EH�� �PO��TAHO ', I5, ' �A��CE�',
     =         ' ***' )
       GO TO 20
C->
210    WRITE( NWTRE, 52) NZX
       WRITE( NWTR, 52)  NZX
       GO TO 21
C----------------------------------------------------------------------
         END
