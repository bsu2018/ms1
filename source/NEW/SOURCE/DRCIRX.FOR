         SUBROUTINE DRCIRX( KAR, DAT, BA0, KNOT, PAR0, IRC )
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0
C   �O��PO�PAMMA:   DRCIRX( KAR, DAT, BA0, KNOT, PAR0, IRC )
C   ------------
C   HA�HA�EH�E:    *** O�PA�OT��K #X�E��:
C  ------------       #X�E��  N
C
C                  N - HOMEP KAHA�A BBO�A �H�OPMA���;
C
C                  �P� N "O���EHO" ��� "O���O�HO" �CTAHAB��BAETC�
C                  PABH�M NRDR KAHA��;
C
C                  �OPMAP�ET MACC�B� BA0, KNOT, PAR0
C                  ( 1-� �ACT� ��PEKT�B� #X�E�� )
C   METO�:         CM. �/� INPUT0;
C   -----
C   �APAMETP�:     KAR - C�MBO��HA� CTPOKA ��PEKT�B�,
C   ---------      DAT - MACC�B ��CE� ��PEKT�B�;
C                  IRC = 1  MACC�B� BA0, KNOT, PAR0 O�PE�E�EH�;
C                      = 2  �H�OPMA��� HE �P�H�TA;
C                  �P. �AP. CM. INPUT0
C
C   �O��PO�PAMM�:  1. COMMON PROF1
C   -------------  2. INPUT0   PPNCOM  READMK
C   �P�ME�AH�E:    1. B C���AE "�ATA��H�X" O���OK - STOP 666
C   ----------        ( KOHE� C�CTEMHO�O KAHK�A NRDR )
C                  2. �O�PO�HEE CM. INPUT0
C                  3. MACC�BA BA0, KNOT, PAR0 - �H����POBAHH�;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         DIMENSION BA0(*), PAR0(*), KNOT(*), KAR(*), DAT(*), DSNAME(11)
       EXTERNAL READMK
         INTEGER BA0, FMSWAL
         EQUIVALENCE ( MASPRT(13),  NWTR )
         EQUIVALENCE ( MASPRT(89),  NMAK  )
         EQUIVALENCE ( MASPRT(14),  NWTRE )
         EQUIVALENCE ( MASPRT(74),  FMSWAL )
         EQUIVALENCE ( MASPRT(12),  NRDR   )
         DATA DSNAME/ 11*'    '/
C-->  C KAKO�O KAHA�A �A�POC BBO�A ��� #�E��:
         NRDR1 = NRDR
         IF ( DAT(1) .EQ. 0.0 ) GO TO 1
C--> �TO-TO ECT�
         NRDR1 = IFIX( DAT(2) )
C--> KOHTPO��
         IF ( NRDR1 .LE. 0  .OR. NRDR1. GT. 100 .OR. NRDR1.EQ.NMAK.OR.
     =  NRDR1 .EQ. NWTR  .OR. NRDR1 .EQ. NWTRE ) NRDR1 = NRDR
C--> COO���M..
1        WRITE(NWTR, 50 ) NRDR1, NMAK
50     FORMAT(/' *** #X�E�� �ਭ�� ==> ���ᠭ�� 楯� �������� �१ ',
     = '����� # ',I2, ' ***'/,
     =  5X,'���ᠭ�� "���� ����⠭����" �������� �१ ',
     = '����� # ',I2, ' ***'/)
         IRC = 1
C--->
       CALL PPNCOM(3,NWTR)
C--->
         ii_rr = 0
         IF ( NRDR1 .NE. NRDR ) CALL KSOPEN(NRDR1,1,ii_rr)
         IF ( ii_rr .NE. 0 )  CALL STOPP(666)
         CALL KSOPEN(NMAK,1,ii_rr)
         IF ( ii_rr .NE. 0 )  CALL STOPP(666)
         CALL INPUT0( BA0, KNOT, PAR0, NRDR1, DSNAME, READMK,  III )
         IF ( NRDR1 .NE. NRDR ) CLOSE (NRDR1)
         CLOSE (NMAK)
C--->EC�� #C�POC ��� �TO-TO ��HOP�POBA��, TO B���MO "�P�EXA��"...
         IF ( III .EQ. 1  .OR. III .EQ. 2 ) IRC = 2
C--> A MO�ET �TO "�CE"
         IF ( III .EQ. 3 ) WRITE(NWTR,51)
51     FORMAT(//' *** P���� ���뢠���� - ����� ��⥬���� ����� ������
     =㯥� ***'//)
         IF ( III. EQ. 3 ) CALL STOPP(666)
         RETURN
         END
