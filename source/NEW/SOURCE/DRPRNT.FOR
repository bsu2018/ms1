         SUBROUTINE DRPRNT(KAR, DATDIR, MSWALL, TOKN, VALN )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRPRNT(KAR, DATDIR, MSWALL )
C   ------------
C   HA�HA�EH�E :   *** O�PA�OTKA ��PEKT�B�  #�E�:
C   -----------
C                    #�E� T0 TM KOUT  PPVENT
C
C            ��E:    T0 - BPEM� HA�A�A B�BO�A;
C                    TM - BPEM� KOHE�A B�BO�A;
C                    KOUT - "KA���� KOUT - �� TO�K� �� P��A COXPAH�EM�X
C                            B�BO�H�X �HA�EH�� PE�EH�� (�A� COXPAHEH��
C                            HOUTT) B�BO��T� HA �E�AT� ";              0
C                      PPVENT - �P� = 0 HE �E�ATAETC� �H�OPMA��� B
C                                       MOMEHT KOMM�TA���;
C                               �P� = 1 - �E�ATAETC�;
C                                 ( �MO��AH�E = 0 );
C
C
C
C                  �P� �C�E�HOM �P�H�T�� �APAMETPOB ��PEKT�B� �
C                   C��CKA "�AKA� B�BO�A" --> �A�O�H�ET �O��
C                   COMMON/SERVZ6/ � BO�����AET ��A� "PON",
C                    + BO�����EH�E MASPRT(106) <-> PPVENT;
C   METO�:         O�EB��EH;
C   ------
C
C   �APAMETP�:     KAR - C�MBO��HA� CTPOKA TEKCTA ��PEKT�B� #�E�;
C   ---------            (�OCTAB��ETC� MOH�TOPOM - CKAHEPOM );
C                  DATDIR - MACC�B �P�H�T�X ��CE� �� KAR->(CTPOKA ��P.)
C                  MSWALL - C��COK COCTO�H�� ��EMEHTOB �E��;
C   �O��PO�PAMM�:  1. GETSPS
C   ------------   2. COMMON PROF1, SERVZ6
C   �P�ME�AH�E:    1. B COMMON SERVZ6 ��KC�P��TC�:
C   ----------        TBEGON, TENDON, KOUTON, SPISON
C                  2.  �O �MO��AH�� T0=0.0, TM=666.0, KOUT=1;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
         REAL TOKN(*),VALN(*)
         INTEGER SPISON
         DIMENSION KAR(*), DATDIR(*), MSWALL(*)
         NRDR = MASPRT(12)
         NWTR = MASPRT(13)
C--->  ECT� �� MSWALL
       IF ( MASPRT(74) .EQ. 1 ) GO TO 200
       WRITE(NWTR,201)
201    FORMAT(/' *** HET O��CAH�� �E�� - ��PEKT�BA ��HOP�P�ETC� ***'/)
        RETURN
200    CONTINUE
C        ***************************
         KOUT = 1
         IPP = 0
C********READ(NRDR, PON)
       IF ( DATDIR(1) .EQ. 0.0 ) GO TO 100
C--> �TO-TO ECT�
       T0 = DATDIR(2)
       TM = DATDIR(3)
       KOUT = IFIX( DATDIR(4) )
       IPP = IFIX( DATDIR(5) )
       IF ( IPP .EQ. 0 ) GO  TO   101
       IPP = 1
       GO TO 101
100    CONTINUE
C-->  � M O � � A H � E
       T0 = 0.0
       TM = +666.0
       KOUT = 1
       IPP = 0
101    CONTINUE
C********WRITE(NWTR, PON)
         IF ( KOUT .LE. 0 ) KOUT = 1
         TBEGON = T0
         TENDON = TM
         KOUTON = KOUT
         WRITE(NWTR, 50) T0, TM, KOUT, IPP
50       FORMAT(/' *** #�E� �P�H�TA:==> T0=',G12.5,' TM=',G12.5,
     =  ' KOUT=',I5, ' PPVENT=', I2, ' ***'/)
C--------
         CALL GETSPS(MSWALL, SPISON, IRC )
         GO TO (1, 2, 3 ), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR,10)
10       FORMAT(/' *** KOHE� �A��A "RDR" - PA�OTA �PEP�BAETC� ***'/
     = ' *** [ DRPRNT ] ***'/)
         STOP 666
C######################################################################
2        CONTINUE
C--->  SPIS ��CT....   C�POC PON....
         MASPRT(56) = 0
         MASPRT(106) = 0
         RETURN
C######################################################################
1        CONTINUE
C--> C��COK ECT�.....  PON  "B��EPHEM"
         MASPRT(56) = 1
         MASPRT(106) = IPP
         RETURN
         END
