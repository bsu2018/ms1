         SUBROUTINE GETDAD(KAR, DAT, IRC )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   GETDAD(KAR, DAT, IRC )
C   -------------
C
C   HA�HA�EH�E:    �EPEBO� �AHH�X C�MBO��HO� CTPOK� ��PEKT�B� KAR BO
C   ----------     BH�TPEHHEE �PE�CTAB�EH�E (REAL*4) --> MACC�B DAT;
C                  ** ABTOMAT��ECK�� B�XO� HA HA�A�O PA��OPA �A��C�
C                     O ��C�AX ��PEKT�B: HA�A�O PA��OPA - OT �EPBO�O
C                     �PO�E�A �OC�E TEKCTA ��PEKT�B� ***
C
C   METO�:         CKAH�POBAH�E KAR � �P�EM ��CE�.
C   -----
C
C   �APAMETP�:     KAR - MACC�B CTPOK� ��PEKT�B�;
C   ---------      DAT - MACC�B ��H�T�X ��CE� (REAL*4):
C                   PA�MEPHOCT� = 21 C�OBO, B DAT(1) - KO���ECTBO,
C                   �ATEM  -  �P�H�T�E ��C�A;
C                  IRC = 1  -  �AHH�E �P�H�T�,
C                      = 2  -  ���� O���K� �P� �P�EME ��CE�;
C
C   �O��PO�PAMM�:  1. COMMON PROF1
C   -------------  2. MSGOUT, APFTRT, APFSQE, SUBEROR
C
C   �P�ME�AH�E:    1. CTPOKA �PO�E�OB - �O��CT�MA;
C   ----------     2. MACCOB DAT - ��CT�TC� H���M�;
C                  3. �P� O���KAX - �E�AT� COO��EH��;
C                  4. PA��E��TE�� "," �B��ETC� �O��CT�M�M;
C                  5. �P� O���KE - B DAT �A��C� 0.0;
C                  6. �A��C�: <NNN,,NNN> <NNN,0,NNN> �KB�BA�EHTH�;
C                  7. KOHE� MO�HO OTME�AT� C�MBO�OM ";" ;
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE ( NWTR, MASPRT(13) )
         EQUIVALENCE ( NWTRE, MASPRT(14) )
         EQUIVALENCE ( DIAL,  MASPRT(15) )
         EQUIVALENCE ( NRDR,  MASPRT(12) )
         DIMENSION DAT(1), KAR(1), ER(80), FKAR(80), MSG(20)
         INTEGER DIAL, PRT90, ER, BLANK, COMA, FKAR, DIEZ, TERM
         DATA K72/72/, K21/21/
         DATA BLANK/' '/, COMA/','/, LGOPA/'$'/, DIEZ/'#'/, TERM/';'/
C--> B�XO� HA HA�A�O PA��OPA
         DO 67 I = 1, K72
         IK = I
         IF ( KAR(I) .EQ. BLANK ) GO TO 68
67       CONTINUE
C--> �PO�E�OB B CTPOKE HET.....
         IK = K72 + 1
68       CONTINUE
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK, (KAR(IZ),IZ=1,K72)
C660   FORMAT(' *** IK =', I5, ' ***'/1X, 80A1 )
C!!--------------------------------------------------------------------
66       CONTINUE
C--->  O��CTKA...
         DO 1 I = 1, 80
1        ER(I) = BLANK
         MSG(1) = 0
         IER = 0
         PRT90 = 0
         DO 2 I=1,K21
2        DAT(I) = 0.0
C--->  �APA�E��H�� MACC�B...
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
         CALL APF TRT( 1, K72, KAR, FKAR )
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
C--->  �O�CK...
         IRC = 1
3        CONTINUE
         IF ( IK .GT. K72 ) GO TO 666
         IF ( KAR(IK) .NE. BLANK ) GO TO 10
30       CONTINUE
C--> HE KOH��T� �� HAM..
         IF ( KAR(IK) .EQ. TERM ) GO TO 666
         IK = IK + 1
         GO TO 3
C---
10       CONTINUE
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
C--> �TO-TO  HA���:
         N2 = K72 - IK + 1
         CALL APF SQE( FKAR, E0, IK, N2,  IFE )
         GO TO ( 11, 12, 13, 4, 4, 3 ), IFE
4        CALL SUBERR(IFE)
11       CONTINUE
C--> �OC�E�HEE ��C�O
         ID = DAT(1) + 1
         IF ( (ID+1) .GT. K21 ) CALL SUBERR( ID   )
         DAT(ID+1) = E0
         DAT(1) = ID
         GO TO 666
C######################################################################
12       CONTINUE
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
C-->  HE ���PA
         IF ( KAR(IK) .EQ. BLANK .OR. KAR(IK) .EQ. COMA ) GO TO 120
         IF ( KAR(IK) .EQ. TERM ) GO TO 120
C--> KOHE� ��OXO�
         IRC = 2
         DAT(1) = DAT(1) + 1.0
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I+1
121      CONTINUE
C--- ��K� ��HOP�POBAH��::::
         IK = IK + 1
         IF ( KAR(IK) .EQ. BLANK .OR. KAR(IK) .EQ. COMA ) GO TO 30
         IF ( KAR(IK) .EQ. TERM ) GO TO 666
         IF ( IK .GT. K72 ) GO TO 666
         GO TO 121
120      CONTINUE
C--> �P�H�T� ��C�O...
         ID = DAT(1) + 1
         IF ( (ID+1) .GT. K21 ) CALL SUBERR( ID )
         DAT(ID+1) = E0
         DAT(1) = ID
C-->  HA HOBOE
         GO TO 30
C######################################################################
13       CONTINUE
C--> �EPE�O�HEH�E
         IRC = 2
         DAT(1) = DAT(1) + 1.0
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 2
         MSG(1) = I+1
         GO TO 121
C######################################################################
C  BCE
C######################################################################
666      CONTINUE
C!!!-------------------------------------------------------------------
C        WRITE(NWTRE, 50 ) ( I, DAT(I), I=1,K21)
C0       FORMAT( 5(2X,'DAT(', I2,')=', G12.5 ) )
C!!!-------------------------------------------------------------------
         IF ( IER .EQ. 0 ) RETURN
C--->�E�AT� O���OK
         IF ( DIAL .EQ. 1 ) WRITE(NWTR, 1003) ( ER(I), I=1,80)
1003     FORMAT(80A1/' *** �AHH�E ��HOP�P��TC� - �PE��O�O�AETC�',
     = ' OTC�TCTB�E ��C�A ***')
         IF ( DIAL .EQ. 0 ) WRITE(NWTR,1004) IER,(ER(I), I=1,80 )
1004     FORMAT(1X,'*** O���OK ',I2,' ***=>',80A1/' *** �AHH�E ��HOP�P��
     =TC� - �PE��O�O�AETC� OTC�TCTB�E ��C�A ***')
         CALL MSGOUT(MSG)
         IF ( DIAL .EQ. 0 ) RETURN
         WRITE(NWTR, 51)
51       FORMAT(' *** BBO��TE TO��KO �AHH�E ��PEKT�B� ��� # ***')
         READ(NRDR,52, END=53  )  ( KAR(I), I=1,80)
52       FORMAT(80A1)
         IRC = 1
         IK = 1
         IF ( KAR(1) .EQ. DIEZ ) RETURN
C--->  �OPO��EM C HA�A�A....
         GO TO 66
C----------------------------------------------------------------------
53       CONTINUE
         WRITE( NWTR, 54 ) NRDR
54       FORMAT(/' *** KOHE� �A��A HA KAHA�E NRDR = ', I2,
     = ' ***'/ ' *** PA�OTA �PEP�BAETC� [ SEQDAT ] ***'/)
         CALL SUBERR(0)
         RETURN
         END
