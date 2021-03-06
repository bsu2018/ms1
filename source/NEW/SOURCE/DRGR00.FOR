         SUBROUTINE DRGR00(KOD, MKOD, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRGR00(KOD, MKOD, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
C   ------------
C   HA�HA�EH�E:    �C�O�H�TE��H�� "MEXAH��M" B�BO�A "�PA���ECKO�"
C   ----------     �H�OPMA��� ��PEKT�B� #�PA�;
C
C   METO�:         O�EB��EH;
C   -----          (EC�� �P� �A�POCE HAKO��EH�� TO�EK B�BO�A �PE��A�A�T
C                  �P�H�T� �O���E TO�EK, �EM �A�AHO B NTGRAF �P� KOD=1,
C                  TO ����TO�H�E TO�K� ��HOP�P��TC�...)
C
C   �APAMETP�:     KOD = 1 - OTKP�T� �P�EM TO�EK B�BO�A;
C   ---------          = 2 - �P�H�T� O�EPE�H�� TO�K�;
C                      = 3 - �AKP�T� �P�EM TO�EK, �E�ATAT� HA NWTR;
C                  MKOD = 0 - B�BO� �PA��KOB B �H��B���A��H�X MAC�TA�AX;
C                       = 1 - B�BO� BCEX �PA�. B O�HOM MAC�TA�E;
C                       = 2 - B�BO� KAK �P� =0 � �P� =1 (�BA P�C�HKA);
C                  SPIS - C��COK "�A�POC B�BO�A";
C                  TIME, TOKN, VALLN - MACC�B� �HA�EH�� O�EPE�HO� TO�K�;
C                  NDX - PA�MEPHOCT� - PA�O�E�O MACC�BA XGRAF;
C                  XGRAF - PA�O��� MACC�B, MAX PA�MEPHOCT�
C                        <1+KO�-BO �PA��KOB> * <KO�-BO TO�EK B�BO�A >
C                         ( CM. �P�ME�AH�E );
C
C   �O��PO�PAMM�:  1. PLOT2, MOVE0
C   ------------   2. COMMON PROF1, SERVZ2, SERVZ3;
C
C   �P�ME�AH�E:    1. KOHTPO�� HE �PBO��TC�;
C                  2. KO���ECTBA TO�EK B�BO�A B���PAETC� ��
C                     MASPRT(47) = NTGRAF
C                  3. �P� MA�OM PA�MEPE MACC�B� XGRAF:
C                     KO��A NTGRAF > NDX, OC��ECTB��ETC�
C                     PA���EH�E B�BO�HO�O �OTOKA HA "�OP���"
C                  4. "CETKA" �O BPEMEHHO� OC� B���PAETC� �� XGRGAD
C                      MASPRT(63), �A�ABAEMA� B ��. �PA��CAX;
C
CZZZ-------------------------------------------------------------------
         DIMENSION SPIS(1), TOKN(1), VALLN(1), XGRAF(1)
         INTEGER SPIS, FRAGM, ANM, ANT, TABTOK, TABTIP, TIP
         COMMON/PROF1/ MASPRT(80)
         COMMON/SERVZ2/ TABTOK(1)
         COMMON /SERVZ3/ TABTIP(1)
         INTEGER HELP, DIAL, PX
         DIMENSION HELP(80), RHELP(1)
         EQUIVALENCE ( HELP(1),  RHELP(1) )
         EQUIVALENCE ( NRDR,     MASPRT(12) )
         INTEGER YES, NO
         DIMENSION YES(2), NO(3)
         EQUIVALENCE ( NWTR, MASPRT(13) )
         EQUIVALENCE ( NWTRE, MASPRT(14) )
         EQUIVALENCE ( DIAL,  MASPRT(15) )
         EQUIVALENCE ( NTGRAF,  MASPRT(47) )
         EQUIVALENCE ( HOUTT,   MASPRT(48) )
         EQUIVALENCE ( XGRGAD,  MASPRT(63) )
C
         DATA YES/'�', 'A' /, NO / 'H', 'E', 'T' /
C**********************************************************************
         KP = SPIS(1)
         IF ( KP .GT. 9 ) KP = 9
         GO TO (1,2,3), KOD
         CALL SUBERR(KOD)
1        CONTINUE
C--->  OTKP�T� �OCT��
C-->  CETKA:
         NXX = 0
         IF ( (XGRGAD/1.8E+4) .GT. HOUTT )
     =                         NXX = INT( XGRGAD/(HOUTT*1.8E+4) + 0.5 )
         IF ( NXX .EQ. 0 ) NXX = 5
C->> CKO��KO MAX TO�EK MO�EM B�BECT� �A O��H "�AXO�"
         NX1 = NDX/(1+KP)
C---> HO HAM HA�O B�BO��T� NXT TO�EK:
         NXT = NTGRAF
C-->  HA��EM CKO��KO MO�EM B�BO��T� B O�HOM �PA�MEHTE...
C     FRANM - ��A� �PA�MEHTOB, NREAL - C.�.K. �P�H�T�X TO�EK
         IF ( NXT .LE. NX1 ) GO TO 10
C--> NX B��EPEM �O MAX, HO KPATHO CETKE...
         NX = (NX1/NXX) * NXX
         FRAGM = 1
         GO TO 11
10       NX = NXT
         FRAGM = 0
11       CONTINUE
         NREAL = 0
C--->  B�BO� �A�O�OBKA...
         WRITE(NWTR,12)
12       FORMAT(1H1/20X,' �PA��K� �HA�EH�� '/)
         ISPIS = 2
         IHELP = 1
         KPP = KP
         NNG = 0
13       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 14
         NNG = NNG + 1
         PX = SPIS(ISPIS)
         GO TO ( 15, 16, 17 ), PX
         CALL SUBERR(PX)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
15       CONTINUE
C--->  HA�P��EH��
         NV = SPIS(ISPIS+1)
         WRITE(NWTR, 502) NNG, NV
502      FORMAT(1X,'C�MBO� ', I2, ' - ', 'HA�P��EH�E ���A ', I5, ' ;')
         KPP = KPP -1
         ISPIS = ISPIS + 6
         GO TO 13
16       CONTINUE
C-->  PA�HOCT�
         NV = SPIS(ISPIS+1)
         NV1 = SPIS(ISPIS+2)
         WRITE(NWTR,503) NNG, NV, NV1
503      FORMAT(1X,'C�MBO� ', I2, ' - ', 'HA�P��EH�E ME��� ���AM� ',
     =  I5, ' � ', I5, ' ;' )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         GO TO 13
17       CONTINUE
C---> TOK�
         NEL = SPIS(ISPIS + 2)
         ANM = SPIS(ISPIS+3)
         ANT = SPIS(ISPIS+4)
         NSM = SPIS(ISPIS+5)
         IHELP = 1
         HELP(IHELP) = NNG
         IHELP = IHELP + 1
         CALL MOVE0( HELP(IHELP), TABTOK(ANT), 3 )
         IHELP = IHELP + 3
         HELP(IHELP) = NEL
         IHELP = IHELP + 1
         HELP(IHELP) = NSM
         IHELP = IHELP + 1
         CALL MOVE0( HELP(IHELP), TABTIP(ANM), 5 )
         IHELP = IHELP + 4
         WRITE(NWTR, 504) NNG,(HELP(I),I=2,4),NEL,NSM,(HELP(I),I=7,11)
504      FORMAT(' C�MBO� ', I2, ' - ',3A4, ' --> ', '��EMEHT ', I5,
     =        ' �HA�EH�E ', I3, ' (', 5A4, ');'  )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         GO TO 13
14       CONTINUE
C-->  "�CE..."
       WRITE(NWTR, 5040 )
5040   FORMAT(/)
         RETURN
C######################################################################
2        CONTINUE
C-->  �P�H�T� O�EPE�H��, NX  �OTOBO...
         NREAL = NREAL + 1
C--->  BO�MO�HO := �PE��A�A�T ���H�� TO�K� ??, EC�� �A, TO ��HOPTP�EM!
         IF ( NREAL .GT. NX ) RETURN
         NNG = 0
         ISPIS = 2
         KPP = KP
         XGRAF(NREAL) = TIME
20       CONTINUE
C-->  PA��OP C��CKA, NNG - HOMEP �PA��KA
         NNG = NNG + 1
         IF ( KPP .LE. 0 ) GO TO 21
         PX = SPIS(ISPIS)
         I2 = SPIS(ISPIS+1)
C$$$$$$$ I3 = SPIS(ISPIS+2)
         GO TO (22, 23, 24), PX
         CALL SUBERR(PX)
C--->
C--->  HA�P��EH��:
22     CONTINUE
       VI2 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
         XGRAF( NREAL + NNG*NX ) = VI2
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 20
C--->  PA�HOCT�
23     CONTINUE
       I3 = SPIS( ISPIS + 2 )
       VI2 = 0.0
       VI3 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
       IF ( I3 .NE. 0 ) VI3 = VALLN( I3 )
         XGRAF( NREAL + NNG*NX ) = VI2 - VI3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 20
C--->  TOK�
24       XGRAF( NREAL + NNG*NX ) = TOKN(I2)
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 20
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
21       CONTINUE
C-->  TO�K� �P�H���, HEH��HO �� B�BECT� �PA�MEHT....
         IF ( NREAL .EQ. NX  .AND.  FRAGM .EQ. 1 ) GO TO 25
         RETURN
C######################################################################
25       CONTINUE
C--> B�BO� �PA�MEHTA
         KSY = 123
         IF ( DIAL .EQ. 1 ) KSY = 74
         NIII = NX + 1
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX,
     =               10, NWTR, MKOD )
C======================================================================
C         IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C510      FORMAT('*** �E�AETE �POCMOTPET� E�E PA�? (�A/HET) ***')
C         IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C         IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C     =        HELP(2) .EQ. YES(2) ) GO TO 25
C======================================================================
C-->  HE H��HO �� �OBTOP�T�, �P� MKOD = 2
         IF ( MKOD .NE. 2 ) GO TO 26
C--> �OBTOP
250      WRITE(NWTR, 505 )
505      FORMAT(//' *** B�BO� �PE������X �PA��KOB B �H��B���A��H�X',
     = ' MAC�TA�AX ***'//)
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX,
     =                10, NWTR, 0 )
C======================================================================
C         IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C511      FORMAT(80A1)
C         IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C         IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C     =        HELP(2) .EQ. YES(2) ) GO TO 250
C======================================================================
26       CONTINUE
C-->  �PA�MEHT B�BE��
         NXT = NXT - NX
         NREAL = 0
         IF ( NXT .LE. NX1 ) GO TO 27
C--> E�E H��EH �PA�MEHT...�O��OTOB�M E�O....
         FRAGM = 1
         NX =(NX1/NXX) * NXX
         RETURN
27       CONTINUE
C-->  HAKO��M �OC�E�H�� �PA�MEHT...
         NX = NXT
         FRAGM = 0
         RETURN
C######################################################################
3        CONTINUE
C===> KOHTPO�� HA KO�-BO TO�EK...
         IF ( NREAL .LT. NX ) WRITE(NWTRE,2501) NREAL, NX, NXX
2501    FORMAT(' *** �O�. HECOOTBETCTB� B DRGR00: - PAHHEE �AKP�T�E B�BO
     =�A ***'/ ' *** NREAL =',I5,' NX =',I5, ' NXX=',I5,' ***')
C===>
C--> �AKPOEM �P�EM...
C!!!  ������� �����  ��  80  �������
         KSY = 74
         IF ( DIAL .EQ. 1 ) KSY = 74
         NIII = NX + 1
C!!!-------------------------------------------------------------------
C        WRITE(NWTRE,700) NREAL, NX, NIII, NXX, KP
C700     FORMAT(' ***NREAL=',I5,' NX=',I5, ' NIII=', I5,
C    =  ' NXX=',I5, ' KP=',I5,'  ***')
C        IXXX=(KP+1)*NX+10
C        WRITE(NWTRE,701) (I, XGRAF(I), I=1,IXXX)
C701     FORMAT( 5(1X,'G(', I4, ')=', G12.5 ) )
C!!!-------------------------------------------------------------------
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX,
     =                10, NWTR, MKOD)
C======================================================================
C        IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C-->
C        IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C        IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C    =        HELP(2) .EQ. YES(2) ) GO TO 3
C======================================================================
         IF ( MKOD .NE. 2 ) RETURN
2500     WRITE (NWTR, 505)
         CALL PLOT2( XGRAF, XGRAF(NIII), NX, KP, NX, KSY, NXX, 10,
     =               NWTR, 0 )
C======================================================================
C        IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C--->
C        IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
C        IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
C    =        HELP(2) .EQ. YES(2) ) GO TO 2500
C======================================================================
         RETURN
         END
