         SUBROUTINE DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRC )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRC )
C   ------------
C   HA�HA�EH�E:    *** O�PA�OT��K #�E��:
C  ------------       #�E��  N
C
C                  N - HOMEP KAHA�A BBO�A �H�OPMA���;
C
C                  �P� N "O���EHO" ��� "O���O�HO" �CTAHAB��BAETC�
C                  PABH�M NRDR KAHA��;
C
C                  �OPMAP�ET MACC�B� BA0, KNOT, PAR0
C                  ( 1-� �ACT� �EP�KT�B� #�E�� )
C   METO�:         CM. �/� INPUT0,INPUT2
C   -----
C   �APAMETP�:     KAR - C�MBO��HA� CTPOKA �EP�KT�B�,
C   ---------      DAT - MACC�B ��CE� �EP�KT�B�;
C                  IRC = 1  MACC�B� BA0, KNOT, PAR0 O�PE�E�EH�;
C                      = 2  �H�OPMA��� HE �P�H�TA;
C                  �P. �AP. CM. INPUT0,INPUT2
C
C   �O��PO�PAMM�:  1. COMMON PROF1
C   -------------  2. INPUT0  INPUT2   PPNCOM  READST
C   �P�ME�AH�E:    1. B C���AE "�ATA��H�X" O���OK - STOP 666
C   ----------        ( KOHE� C�CTEMHO�O KAHK�A NRDR )
C                  2. �O�PO�HEE CM. INPUT0,INPUT2
C                  3. MACC�BA BA0, KNOT, PAR0 - �H����POBAHH�;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         DIMENSION BA0(*), PAR0(*), KNOT(*), KAR(*), DAT(*), DSNAME(11)
C++++++++!!!!
         INTEGER DSNAME
         EXTERNAL READST
         INTEGER BA0, FMSWAL, DIAL
         EQUIVALENCE ( MASPRT(13),  NWTR )
         EQUIVALENCE ( MASPRT(14),  NWTRE )
         EQUIVALENCE ( MASPRT(74),  FMSWAL )
         EQUIVALENCE ( MASPRT(12),  NRDR   )
         EQUIVALENCE ( MASPRT(15),  DIAL   )
         DATA DSNAME/ 11*'    '/
C---         DATA DSNAME(1)/'    '/   
C-->  ��KCA��� �KA�ATE�E�
         IBA01 = BA0(3)
         IKNOT1 = KNOT(3)
         IPAR01 = IFIX (PAR0(3))
C-->  C KAKO�O KAHA�A �A�POC BBO�A ��� #�E��:
         NRDR1 = NRDR
         IF ( DAT(1) .EQ. 0.0 ) GO TO 1
C--> �TO-TO ECT�
         NRDR1 = IFIX( DAT(2) )
C--> KOHTPO��
         IF ( NRDR1 .LE. 0  .OR. NRDR1. GT. 100 .OR.
     =  NRDR1 .EQ. NWTR  .OR. NRDR1 .EQ. NWTRE ) NRDR1 = NRDR
C--> COO���M..
1        WRITE(NWTR, 50 ) NRDR1
50     FORMAT(/' *** #�E�� �ਭ��:==> ���ᠭ�� 楯� �������� �१ ',
     = '����� # ',I2, ' ***'/)
         IRC = 1
C---> HA�E�ATAEM.
       CALL PPNCOM(3,NWTR)
C######################################################################
C--->  HA��HAEM BBO��T� �AHH�E
         ii_rr = 0
         IF ( NRDR1 .NE. NRDR ) CALL KSOPEN(NRDR1,0,ii_rr)
         IF ( ii_rr .NE. 0 )  call STOPP(20)
C----------------------------------------------------------------------
         CALL INPUT0( BA0, KNOT, PAR0, NRDR1, DSNAME, READST,  III )
         IF ( NRDR1 .NE. NRDR ) CLOSE (NRDR1)
C--->EC�� #C�POC ��� �TO-TO ��HOP�POBA��, TO B���MO "�P�EXA��"...
         IF ( III .EQ. 1  .OR. III .EQ. 2 ) IRC = 2
         IF ( IRC .EQ. 2 ) RETURN
C--> A MO�ET �TO "�CE"
         IF ( III .EQ. 3 ) GO TO 9999
C--> A MO�ET �O���EHA �EP�KT�BA #MEX
         IF ( III .EQ. 4 ) GO TO 2
         RETURN
C######################################################################
C  O�PA�OT��K
C  �O��EP�KT�B�        METKA 2
C  #MEX
C----------------------------------------------------------------------
2       CONTINUE
        IF( BA0(3) .NE. IBA01 .OR. KNOT(3) .NE. IKNOT1 .OR.
     *      IFIX(PAR0(3)) .NE. IPAR01  ) GO TO 5
        IF( DIAL .EQ. 0 ) WRITE(NWTR,9995)
9995   FORMAT(/20X,
     *         ' *** MO�E��P�EMA� C�CTEMA HE CO�EP��T ��EKTP��ECKO�',
     *         ' �E�� ***'/)
        IF( DIAL .EQ. 1 ) WRITE(NWTR,9996)
9996   FORMAT(/10X,
     *         ' *** MO�E��P�EMA� C�CTEMA HE CO�EP��T ��EKTP��ECKO�',
     *         ' �E�� ***'/)
5      CONTINUE
C----------------------------------------------------------------------
C-->  BBO��M �A���E
C!!!     IF ( NRDR1 .NE. NRDR ) CALL KSOPEN(NRDR1,0,ii_rr)
C!!!     CALL INPUT2( BA0, KNOT, PAR0, NRDR1,  READST,  III )
C!!!     IF ( NRDR1 .NE. NRDR ) CLOSE (NRDR1)
C--->EC�� #C�POC ��� �TO-TO ��HOP�POBA��, TO B���MO "�P�EXA��"...
         IF ( III .EQ. 1  .OR. III .EQ. 2 ) IRC = 2
         IF ( IRC .EQ. 2 ) RETURN
C--> A MO�ET �TO "�CE"
         IF ( III .EQ. 3 ) GO TO 9999
         RETURN
C######################################################################
C     KOHE� BXO�HO�O HA�OPA
C---> KOH�AEM....
9999    CONTINUE
        WRITE(NWTR,51)
51     FORMAT(//' *** P���� ���뢠���� - ����� ��⥬���� ����� ������
     =㯥� ***'//)
         call STOPP(16)
         END
