         SUBROUTINE DRG1F0(KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRG1F0(KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
C   ------------
C   HA�HA�EH�E:    *** �C�O�H�TE�� � O�PA�OT��K ��PEKT�B� #�PA�1 ***
C   -----------      #�PA�1 T0 TM KOUT KSX
C
C                  T0 - HA�A��HOE BPEM� �E�AT� �PA��KOB;
C                  TM - KOHE�HOE  BPEM� �E�AT� �PA��KOB;
C                  KOUT - �EPE� CKO��KO TO�EK B�BO��T�;
C                  KSX -  KO���ECTBO CTPOK �O OC� "X" (BH��) B �PA��KE;
C
C                  B C��CKE �A�POCA HEO�XO��MO �KA��BAT� CTPO�O
C                                                        *******
C                  �BE BE����H�; �P� �TOM �P�OP�TET PA��OPA
C                  ( CO��ACHO GETSPS ):
C                  "V", "U", "TOK".....
C                   �EPBA� -> OC� "X",  BTOPA� -> OC� "Y";
C   METO�:         O�EB��EH;
C   -----
C   �APAMETP�:     KAR - C�MBO��HA� CTPOKA TEKCTA ��PEKT�B�(A-KOD,80);
C   ---------      DAT - MACC�B �P�H�T�X �AHH�X ��PEKT�B�;
C                  MSWALL - C�OBA COCTO�H�� �E��;
C                  TOKN, VALLN - MACC�B� XPAHEH�� "TO�EK";
C                  NDX - PA�MEPHOCT� XGRAF;
C                  XGRAF - "C����H�" ��� B�BO�A �PA��KOB;
C
C   �O��PO�PAMM�:  1. COMMON PROF1;
C   -------------  2. GETSPS, DRG100, READ00 , PPNCOM
C
C   �P�ME�AH�E:    1. "�MO��AH�E" --> TO��KO ��� KOUT � KSX---->
C   ----------         1   100
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
C-->   E�O B���PAEM �� �A��A HAKO��EH��
         EQUIVALENCE ( MASPRT(48), HOUTT )
         EQUIVALENCE ( SETKA,  MASPRT(63) )
         EQUIVALENCE ( NTGRAF, MASPRT(47) )
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( NRDR,   MASPRT(12) )
         INTEGER SPIS
         DIMENSION SPIS(55), MSWALL(*), TOKN(*), VALLN(*) , XGRAF(*)
         DIMENSION KAR(*), DAT(*)
C-->  ECT� �� MSWALL ?
       IF ( MASPRT(74) .EQ. 1 ) GO TO 59
       WRITE(NWTR, 60)
60     FORMAT(/' *** HET O��CAH�� �E�� - ��PEKT�BA ��HOP�P�ETC� ***'/)
       RETURN
59     CONTINUE
C===>
C
         KOUT = 1
         KSX = 100
C===>
C=======>READ(NRDR, GRAF)
       KKK = IFIX( DAT(1) )
       IF ( KKK .GE. 2 ) GO TO 5600
       WRITE(NWTR, 57 )
57     FORMAT(/' *** HET "T0" �/��� "TM" - ��PEKT�BA ��HOP�P�ETC� ***'/)
       RETURN
5600   CONTINUE
       T0 = DAT(2)
       TM = DAT(3)
       IF ( KKK. EQ. 2 ) GO TO 58
       KOUT = IFIX( DAT(4) )
       IF ( KKK .EQ. 3 ) GO TO 58
       KSX = IFIX( DAT(5) )
C
C
58     CONTINUE
C=======>WRITE(NWTR, GRAF)
C====>
         IF ( KSX .LT. 0 ) KSX =100
C
         IF ( KOUT .LE. 0 ) KOUT = 1
C===>
         WRITE(NWTR, 50) T0, TM, KOUT, KSX
50     FORMAT(/' *** #�PA�1 �P�H�TA:==> T0=', G12.5,' TM=',G12.5,' ***'/
     =          '                    ==> KOUT=',I3,' KSX=',I5,
     =   9X,    '     ***'/)
C----------------------------------------------------------------------
         CALL GETSPS( MSWALL, SPIS, IRC )
         GO TO ( 1,2,3), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR, 51)
51      FORMAT(//' *** KOHE� �A��A HA "RDR" - PA�OTA �PEP�BAETC� ***'//)
         STOP 666
C----------------------------------------------------------------------
2        CONTINUE
C-->  SPIS ��CT
         RETURN
C**********************************************************************
1        CONTINUE
C--> �O��OTOBKA K B�BO��...
       IF ( SPIS(1) .GT. 2 ) WRITE(NWTR,400) SPIS(1)
400    FORMAT(/' *** B �A�POCE ', I2, ' �AB�C�MOCTE� ��� "#�PA�1"',
     = ', �O��CT�MO 2-E ***'/)
       IF ( SPIS(1) .GT. 2 ) SPIS(1) = 2
       IF ( SPIS(1) .LT. 2 ) WRITE(NWTR,4000)
4000   FORMAT(/' *** �A�POC ��� "#�PA�1" - O���O�EH, MA�O �AHH�X ***'/)
       IF ( SPIS(1) .LT. 2 ) RETURN
C                            *******
       TIME = TM
       RK = KOUT
       KTXX = 0
C$$$$$ NTGRAF = INT( (TM-T0)/(HOUTT*RK) +0.5 )  + 0
       NTGRAF = 0
C-->
C-->
C-->
C--> OTKPOEMC�       -->  �O����M HOUTT
         KOD = 1
         CALL READ00( KOD, HOUTT,  TOKN, VALLN)
         GO TO ( 100, 200, 300 ), KOD
         CALL SUBERR(KOD)
100      CONTINUE
C***** ��OK�P�EM ******************************************************
C---> KOHTPO��P�EM TM, B TIME --> MAX BPEM�...
C        IF ( TIME .LT. TM ) WRITE(NWTR,56) TIME, TM, TIME
C56       FORMAT(/' *** �AHH�E �A��OB HAKO��EH�� O�PE�E�EH� TO��KO �O ',
C    = G12.5, ' CEK. ***'/
C    = ' *** �A�PO�EHA� BEPXH�� �PAH��A BPEMEHHO�O �HTEPBA�A TM=',
C    = G12.5, ' CEK. ***'/
C    = ' *** �EPEO�PE�E��ETC� HA �HA�EH�E TM=', G12.5, ' CEK. ***')
C        IF ( TIME .LT. TM ) TM = TIME
C**********************************************************************
C----------------------------------------------------------------------
       HOUTTX = HOUTT * 1.8E+4
       WRITE(NWTR, 5667 ) HOUTT, HOUTTX
5667   FORMAT(/' *** �HA�EH�� �EPEMEHH�X �E�� O�PE�E�EH� C �A�OM',
     = 1X,G12.5, ' (', G12.5, ' ) ***'/)
C----------------------------------------------------------------------
         RK = KOUT
C--> CKO��KO �POC�T B�BECT�
         NTGRAF = INT( (TM-T0)/(HOUTT*RK) + 0.5 ) + 0
C                                                *******
C----------------------------------------------------------------------
C  CKO��KO �O�BO��ET XGRAF ( TXXM - COOTB. BPEM� --> TM ):
       NXXDX = NDX/2
       TXXM = (NXXDX-1) * HOUTT * RK  +  T0
       IF ( NXXDX .LT. NTGRAF ) WRITE(NWTR,401) NTGRAF, NXXDX, TXXM
401    FORMAT(/' *** B �HTEPBA�E [T0,TM] CO�EP��TC� ', I5,
     =   ' TO�EK ***'/' *** PA�MEP� O.�. �O��CKA�T',
     =   ' �C�O���OBAT� TO��KO ', I5, ' TO�EK O�PE�E�EH�� ***'/
     =   ' *** �HA�EH�E TM ��MEH�ETC� HA TM = ', G12.5, ' ***'/)
       IF ( NXXDX .LT. NTGRAF ) TM = TXXM
C
       NTGRAF = INT( (TM-T0)/(HOUTT*RK) + 0.5 )  + 0
C                                                 ****
C****CTPAXOBKA*********************************************************
       IF ( TM .LE. T0 ) WRITE(NWTR,5666) T0, TM
5666  FORMAT(//' *** �HTEPBA� �HA�EH�� [T0,TM]:=> ','[',G12.5,',',G12.5,
     =   '] ***'/ ' *** O�PE�E�EH O���O�HO --> "#�PA�1" - ',
     =        ' ��HOP�P�ETC� ***'//)
       IF ( TM .LE. T0 ) RETURN
C**********************************************************************
C----------------------------------------------------------------------
         WRITE(NWTR ,52) NTGRAF
52       FORMAT(/' *** CTPO�TC� �PA��K �O ', I5, ' TO�KAM ***' /)
C-->  OTKP�T� �E�AAT�
       CALL PPNCOM(3,NWTR)
         KTXX = 0
         KOD = 1
         CALL DRG100(KOD, KSX, SPIS, TIME, TOKN, VALLN, NDX, XGRAF )
         KXX = 0
C==> ��CTKA XGRAF
         DO 53 I = 1, NDX
53       XGRAF(I) = 0.0
C-->  ��K� HAKO��EH��
102      CONTINUE
         KOD = 2
         CALL READ00(KOD, TIME, TOKN, VALLN )
         GO TO (101, 200, 300 ), KOD
         CALL SUBERR(KOD)
101      CONTINUE
C-->  HA�A �� TO�KA
         IF ( TIME .LT. T0 ) GO TO 102
         IF ( TIME .GT. TM ) GO TO 105
103      CONTINUE
         KOD = 2
         IF ( KXX .EQ. 0 )
     =     CALL DRG100(KOD, KSX, SPIS, TIME, TOKN, VALLN, NDX, XGRAF)
         IF ( KXX .EQ. 0    ) KTXX = KTXX + 1
         KXX = KXX + 1
         IF ( KXX .EQ. KOUT ) KXX = 0
         GO TO 102
105      CONTINUE
C!!!===================================================================
         IF ( KTXX .NE. NTGRAF ) WRITE(NWTRE, 110) KTXX,NTGRAF
110      FORMAT(' *** PEA��HO �P�H��� ', I5, ' TO�EK, A �A�POC��� ',
     = I5, ' ***')
C!!!===================================================================
C---> B�BECT� �PA��K...
         KOD = 3
         CALL DRG100(KOD,KSX, SPIS, TIME, TOKN, VALLN, NDX, XGRAF)
C===>          ��PEKT�BA B��O�HEHA
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
200      CONTINUE
C--->   �O�. KOHE�, BCE�O B �A��E KTXX TO�EK
         WRITE(NWTR, 54) KTXX, NTGRAF
54       FORMAT(/' *** �OCT��H�T KOHE� �A��OB HAKO��EH��    ***'/
     = ' *** B �A�PO�EHHOM ��A�O�OHE OT T0 �O TM ***'/
     =          ' *** O�HAP��EHO BCE�O ', I5, ' TO�EK PE�EH�� ***'/
     =' *** B� �A�PA��BAETE ', I5, ' TO�EK ��� �OCTPOEH�� �PA��KA ***'/)
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
300      CONTINUE
C-->  ���. KOHE�
         WRITE(NWTR,55) KTXX, NTGRAF, TIME
55       FORMAT(/' *** �A��� HAKO��EH�� PE�EH�� HE ���� �AKP�T� ***'/
     = ' *** B �A�PO�EHHOM ��A�O�OHE OT T0 �O TM ***'/
     = ' *** O�HAP��EHO BCE�O ', I5, ' TO�EK PE�EH�� ***'/
     = ' *** B� �A�PA��BAETE ', I5, ' TO�EK ��� �OCTPOEH�� �PA��KA ***'/
     = ' *** �OC�E�H�� TO�KA O�PE�E�EHA ��� T =',G12.5, ' CEK. ***'/
     = ' *** PA�OTA �PEP�BAETC� ***********************************'///)
         STOP 666
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
