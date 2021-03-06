         SUBROUTINE DRG100(KOD, KSX, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRG100(KOD, KSX, SPIS, TIME,TOKN,VALLN,NDX,XGRAF)
C   ------------
C   HA�HA�EH�E:    �C�O�H�TE��H�� "MEXAH��M" B�BO�A "�PA���ECKO�"
C   ----------     �H�OPMA��� ��PEKT�B� #�PA�1;
C
C   METO�:         O�EB��EH;
C   -----          (EC�� �P� �A�POCE HAKO��EH�� TO�EK B�BO�A �PE��A�A�T
C                  �P�H�T� �O���E TO�EK, �EM �A�AHO B NTGRAF �P� KOD=1,
C                  TO ����TO�H�E TO�K� ��HOP�P��TC�...)
C
C   �APAMETP�:     KOD = 1 - OTKP�T� �P�EM TO�EK B�BO�A;
C   ---------          = 2 - �P�H�T� O�EPE�H�� TO�K�;
C                      = 3 - �AKP�T� �P�EM TO�EK, �E�ATAT� HA NWTR;
C                  KSX - KO���ECTBO CTPOK �O OC� "X" �PA��KA;
C                  SPIS - C��COK "�A�POC B�BO�A";
C                  TIME, TOKN, VALLN - MACC�B� �HA�EH�� O�EPE�HO� TO�K�;
C                  NDX - PA�MEPHOCT� - PA�O�E�O MACC�BA XGRAF;
C                  XGRAF - PA�O��� MACC�B, MAX PA�MEPHOCT�
C                        2 * <KO�-BO TO�EK �OCTPOEH�� �PA��KA, NTGRAF>
C                         ( CM. �P�ME�AH�E );
C
C   �O��PO�PAMM�:  1. PLOMXY,  MOVE0
C   ------------   2. COMMON PROF1, SERVZ2, SERVZ3;
C
C   �P�ME�AH�E:    1. KOHTPO�� HE �PBO��TC�;
C                  2. KO���ECTBA TO�EK B�BO�A B���PAETC� ��
C                     MASPRT(47) = NTGRAF
C                  3. �P� MA�OM PA�MEPE MACC�B� XGRAF:
C                     KO��A NTGRAF > NDX, OC��ECTB��ETC�
C                     O�PAH��EH�E NTGRAF = NDX --> �E� COO��EH��
C                  4. "CETKA" �O "X" OC� �CTAHAB��BAETC� PABHO� 10;
C
C
CZZZ-------------------------------------------------------------------
         DIMENSION SPIS(1), TOKN(1), VALLN(1), XGRAF(1)
         INTEGER SPIS, FRAGM, ANM, ANT, TABTOK, TABTIP, TIP
         COMMON/PROF1/ MASPRT(80)
         COMMON/SERVZ2/ TABTOK(1)
         COMMON /SERVZ3/ TABTIP(1)



!!           common/xxtt/ NREAL, NX, FRAGM, NXX
!!           integer NREAL, NX, NXX
!            save NREAL, NX, FRAGM, NXX
!!           save


         INTEGER HELP, DIAL, PX
         DIMENSION HELP(80), RHELP(1)
         EQUIVALENCE ( HELP(1),  RHELP(1) )
         EQUIVALENCE ( NRDR,     MASPRT(12) )
         INTEGER YES, NO
         DIMENSION YES(2), NO(3)
         DIMENSION OSXY(4)
         INTEGER OSXY
         EQUIVALENCE ( NWTR, MASPRT(13) )
         EQUIVALENCE ( NWTRE, MASPRT(14) )
         EQUIVALENCE ( DIAL,  MASPRT(15) )
         EQUIVALENCE ( NTGRAF,  MASPRT(47) )
         EQUIVALENCE ( HOUTT,   MASPRT(48) )
         EQUIVALENCE ( XGRGAD,  MASPRT(63) )
C
         DATA OSXY / ' OC�', ' X  ',  ' OC�', ' Y  ' /
         DATA YES/'�', 'A' /, NO / 'H', 'E', 'T' /
C**********************************************************************
         KP = SPIS(1)
         IF ( KP .NE. 2 ) CALL SUBERR(KP)
         GO TO (1,2,3), KOD
         CALL SUBERR(KOD)
1        CONTINUE
C--->  OTKP�T� �OCT��
C-->  CETKA �O "X"
         NXX = 10
C-->  �KA�ATE�� "OC�"
         IOSXY = 1
         IOSXY1 = 2
C->> CKO��KO MAX TO�EK MO�EM B�BECT� �A O��H "�AXO�"
         NX1 = NDX/2
C---> HO HAM HA�O B�BO��T� NTX TO�EK:
         NTX = NTGRAF
C      NREAL - C.�.K. �P�H�T�X TO�EK
C-->  EC�� O���KA, O�PAH���MC�........
         IF ( NTX .GT. NX1 ) NTX = NX1
         NX = NTX
C        *********
         NREAL = 0
C--->  B�BO� �A�O�OBKA...
         WRITE(NWTR,12)
12       FORMAT(1H1/20X,' �PA��K �APAMETP��ECKO� �AB�C�MOCT� '/)
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
         WRITE(NWTR, 502) ( OSXY(II), II=IOSXY,IOSXY1), NNG, NV
502      FORMAT(1X, 2A4,      I2, ' - ', 'HA�P��EH�E ���A ', I5, ' ;')
         KPP = KPP -1
         ISPIS = ISPIS + 6
         IOSXY = 3
         IOSXY1 = 4
         GO TO 13
16       CONTINUE
C-->  PA�HOCT�
         NV = SPIS(ISPIS+1)
         NV1 = SPIS(ISPIS+2)
         WRITE(NWTR,503) ( OSXY(II), II=IOSXY, IOSXY1), NNG, NV, NV1
503      FORMAT(1X,  2A4,     I2, ' - ', 'HA�P��EH�E ME��� ���AM� ',
     =  I5, ' � ', I5, ' ;' )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         IOSXY = 3
         IOSXY1 = 4
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
         WRITE(NWTR, 504) (OSXY(II), II= IOSXY, IOSXY1),
     =                    NNG,(HELP(I), I=2,4),NEL,NSM,(HELP(I),I=7,11)
504      FORMAT(1X,2A4,        I2, ' - ',3A4, ' --> ', '��EMEHT ', I5,
     =        ' �HA�EH�E ', I3, ' (', 5A4, ');'  )
         KPP = KPP - 1
         ISPIS = ISPIS + 6
         IOSXY = 3
         IOSXY1 = 4
         GO TO 13
14       CONTINUE
C-->  "�CE..."
       WRITE(NWTR,5040 )
5040   FORMAT( / )
         RETURN
C######################################################################
2        CONTINUE
C-->  �P�H�T� O�EPE�H��, NX  �OTOBO...
         NREAL = NREAL + 1
C--->  BO�MO�HO := �PE��A�A�T ���H�� TO�K� ??, EC�� �A, TO ��HOPTP�EM!
         IF ( NREAL .GT. NX ) RETURN
         NNG = 1
         ISPIS = 2
         KPP = KP
C
20       CONTINUE
C-->  PA��OP C��CKA, NNG - �P��HAK OC�-->  =1 -> "X",  =0 --> "Y"
C-->  PA��OP CO��ACHO �P�OP�TETAM-->  (CM. DRG1F0 )
         IF ( KPP .LE. 0 ) GO TO 21
         PX = SPIS(ISPIS)
         I2 = SPIS(ISPIS+1)
C$$$$$$  I3 = SPIS(ISPIS+2)
         GO TO (22, 23, 24), PX
         CALL SUBERR(PX)
C--->
C--->  HA�P��EH��:
22     CONTINUE
       VI2 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
         XGRAF( 2*NREAL - NNG ) = VI2
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         NNG = 0
         GO TO 20
C--->  PA�HOCT�
23     CONTINUE
       I3 = SPIS( ISPIS + 2 )
       VI2 = 0.0
       VI3 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
       IF ( I3 .NE. 0 ) VI3 = VALLN( I3 )
         XGRAF( 2*NREAL - NNG ) = VI2 - VI3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         NNG = 0
         GO TO 20
C--->  TOK�
24       XGRAF( 2*NREAL - NNG ) = TOKN(I2)
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         NNG = 0
         GO TO 20
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
21       CONTINUE
C**********************************************************************
C**********************************************************************
         RETURN
C######################################################################
3        CONTINUE
C===> KOHTPO�� HA KO�-BO TO�EK...
         IF ( NREAL .LT. NX ) WRITE(NWTRE,2501) NREAL, NX
2501    FORMAT(' *** �O�. HECOOTBETCTB� B DRG100: - PAHHEE �AKP�T�E B�BO
     =�A ***'/ ' *** NREAL =',I5,' NX =',I5,            ' ***')
C===>
         IXXI = 0
301      CONTINUE
C--> �AKPOEM �P�EM...
C!!!  ������� ����� �� 132 !!!! 80 �������
         KSY = 132 !!!! 74
         IF ( DIAL .EQ. 1 ) KSY = 74
         CALL PLOMXY( XGRAF,              NX,    KSX, KSY, 10,
     =                10, NWTR, XGRAF, IXXI )
C======================================================================
         IF ( DIAL .EQ. 1 ) WRITE(NWTR,510)
C-->
         IF ( DIAL .EQ. 1 ) READ(NRDR,511) (HELP(I), I=1,80)
         IF ( DIAL .EQ. 1 .AND. HELP(1) .EQ. YES(1) .AND.
     =        HELP(2) .EQ. YES(2) ) GO TO 300
         RETURN
300      CONTINUE
         IXXI = -1
         GO TO 301
C======================================================================
510      FORMAT(' *** �E�AETE �OCMOTPET� E�E PA� ? (�A/HET) ***')
511      FORMAT(80A1)
C--->
         END
