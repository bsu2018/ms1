         SUBROUTINE DRout0(KAR, DAT, MSWALL, TOKN, VALLN )
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   DRout0(KAR, DAT, MSWALL, TOKN, VALLN )
C   ------------
C   HA�HA�EH�E:    *** �C�O�H�TE�� � O�PA�OT��K ��PEKT�B� #���� ***
C   -----------      #����  T0 TM KOUT
C
C                  T0 - HA�A��HOE BPEM�
C                  TM - KOHE�HOE  BPEM�
C                  KOUT - �EPE� CKO��KO TO�EK B�BO��T�;
C
C
C   METO�:         O�EB��EH;
C   -----
C   �APAMETP�:     KAR - C�MBO��HA� CTPOKA TEKCTA ��PEKT�B�(A-KOD,80);
C   ---------      DAT - MACC�B �P�H�T�X �AHH�X ��PEKT�B�;
C                  MSWALL - C�OBA COCTO�H�� �E��;
C                  TOKN, VALLN - MACC�B� XPAHEH�� "TO�EK";
C
C   �O��PO�PAMM�:  1. COMMON PROF1;
C   -------------  2. GETSPS, DRPR00, READ00 , PPNCOM
C
C   �P�ME�AH�E:    1. "�MO��AH�E" --> TO��KO ��� KOUT      ---->
C   ----------         1
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
C-->  ��  1-� �A��C� NSPOOL
         EQUIVALENCE ( MASPRT(48), HOUTT )
C********EQUIVALENCE ( SETKA,  MASPRT(63) )
C
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
C********EQUIVALENCE ( NRDR,   MASPRT(12) )
         INTEGER SPIS
         DIMENSION SPIS(91), MSWALL(*), TOKN(*), VALLN(*)
         DIMENSION KAR(*), DAT(*)
C-->  ECT� �� MSWALL ?
       IF ( MASPRT(74) .EQ. 1 ) GO TO 59
       WRITE(NWTR, 60)
60     FORMAT(/' *** HET O��CAH�� �E�� - ��PEKT�BA ��HOP�P�ETC� ***'/)
       RETURN
59     CONTINUE
         T0 = 0.0
         TM = 666.0
         KOUT = 1
       KKK = IFIX( DAT(1) )
       IF ( KKK .LT. 2 ) GO TO 202
       T0 = DAT(2)
       TM = DAT(3)
       KOUT = IFIX( DAT(4) )
         IF ( KOUT .LE. 0 ) KOUT = 1
202     CONTINUE
         WRITE(NWTR, 203) T0, TM, KOUT
203      FORMAT(/' *** #���� �ਭ��:=> T0=', G12.5,
     = ' TM=',G12.5, ' KOUT=',I5, ' ***'/)
C----------------------------------------------------------------------
         CALL GETSPO( MSWALL, SPIS, IRC )
         GO TO ( 1,2,3), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR, 204)
204      FORMAT(/' *** KOHE� �A��A HA "RDR" - PA�OTA �PEP�BAETC� ***'/)
         call STOPP(16)
C----------------------------------------------------------------------
2        CONTINUE
C-->  SPIS ��CT
         RETURN
C**********************************************************************
1        CONTINUE
C--> �O��OTOBKA K B�BO��...
       TIME = 0.0
C
       KTXX = 0
C
C                                             ???????????
C--> OTKPOEMC�
C
       TIME = 0.0
       KTXX = 0
         KOD = 1
         CALL READ00( KOD, HOUTT, TOKN, VALLN)
         GO TO ( 1000, 2000, 3000 ), KOD
         CALL SUBERR(KOD)
1000     CONTINUE
C*** ��OK�P�EM ********************************************************
C---> KOHTPO��P�EM TM, B TIME --> MAX BPEM�...
C        IF ( TIME .LT. TM ) WRITE(NWTR,205) TIME, TM, TIME
C205      FORMAT(' *** �AHH�E �A��OB HAKO��EH�� O�PE�E�EH� TO��KO �O ',
C    = G12.5, ' CEK. ***'/
C    = ' *** �A�PO�EHA� BEPXH�� �PAH��A BPEMEHHO�O �HTEPBA�A TM=',
C    = G12.5, ' CEK. ***'/
C    = ' *** �EPEO�PE�E��ETC� HA �HA�EH�E TM=', G12.5, ' CEK. ***')
C        IF ( TIME .LT. TM ) TM = TIME
C=*********************************************************************
C----------------------------------------------------------------------
       HOUTTX = HOUTT * 1.8E+4
       WRITE(NWTR, 5667 ) HOUTT, HOUTTX
5667   FORMAT(/' *** �HA�EH�� �EPEMEHH�X �E�� O�PE�E�EH� C �A�OM',
     = 1X,G12.5, ' (', G12.5, ') ***'/)
C----------------------------------------------------------------------
C****CTPAXOBKA*********************************************************
       IF ( TM .LE. T0 ) WRITE(NWTR,5666) T0, TM
5666  FORMAT(//' *** �HTEPBA� �HA�EH�� [T0,TM]:=> ','[',G12.5,',',G12.5,
     =   '] ***'/ ' *** O�PE�E�EH O���O�HO --> "#���� - ',
     =        ' ��HOP�P�ETC� ***'//)
       IF ( TM .LE. T0 ) RETURN
C**********************************************************************
C-->  OTKP�T� �E�AAT�
       CALL PPNCOM(3,NWTR)
C
         KOD = 1
         CALL DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
         KXX = 0
C-->  ��K� �E�AT�
206      CONTINUE
         KOD = 2
         CALL READ00(KOD, TIME, TOKN, VALLN )
         GO TO (207, 2000, 3000 ), KOD
         CALL SUBERR(KOD)
207      CONTINUE
C-->  HA�A �� TO�KA
         IF ( TIME .LT. T0 ) GO TO 206
         IF ( TIME .GT. TM ) GO TO 208
C   TO�KA HA�A
         KOD = 2
         IF ( KXX .EQ. 0 )
     =     CALL DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
         KTXX = KTXX + 1
         KXX = KXX + 1
         IF ( KXX .EQ. KOUT ) KXX = 0
         GO TO 206
208      CONTINUE
         KOD = 3
         CALL DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
C===>          ��PEKT�BA B��O�HEHA
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
2000     CONTINUE
C--->   �O�. KOHE�, BCE�O B �A��E KTXX TO�EK
         WRITE(NWTR, 54) KTXX
54       FORMAT(/' *** �OCT��H�T KOHE� �A��OB HAKO��EH��    ***'/
     = ' *** B �A�PO�EHHOM ��A�O�OHE OT T0 �O TM ***'/
     =          ' *** O�HAP��EHO BCE�O ', I5, ' TO�EK PE�EH�� ***'/)
C
       GO TO 208
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
3000     CONTINUE
C-->  ���. KOHE�
         WRITE(NWTR,55) KTXX,         TIME
55       FORMAT(/' *** �A��� HAKO��EH�� PE�EH�� HE ���� �AKP�T� ***'/
     = ' *** B �A�PO�EHHOM ��A�O�OHE OT T0 �O TM ***'/
     = ' *** O�HAP��EHO BCE�O ', I5, ' TO�EK PE�EH�� ***'/
     = ' *** �OC�E�H�� TO�KA O�PE�E�EHA ��� T =',G12.5, ' CEK. ***'/
     = ' *** PA�OTA �PEP�BAETC� ***********************************'//)
         call STOPP(16)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
