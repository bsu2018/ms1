C====== MAKROMOD ======================================================
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0  (OT AB��CTA 1990 �.)------------
C   KOPHEBA�_�PO�PAMMA: MAKROMOD
C  -------------------
C   HA�HA�EH�E:        O��EE ��PAB�EH�E �PO�PAMMAMA MO�E��POBAH��
C  ------------
C   METO�:             ��AKO�T�KA, MAKPOMO�E��POBAH�E
C   -----
C   �APAMETP�:
C   ----------
C   �O��PO�PAMM�:      1. REGCPU, SEQDIR
C   ------------       2. COMMON PROF1,2,3, SERVZ1,2,3,4,5,6,7
C                      3. O�PA�OT��K� ��PEKT�B DR******;
C   �P�ME�AH�E:         1.  V.M=1.* - CTAT��ECKOE B��E�EH�E O.�.
C  -----------
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       COMMON/PROF2/ MACTAB(1)
       COMMON/PROF3/ FUNTAB(1)

       COMMON/MEMORY/ NREG, REGION(1)
       COMMON/PONTER/ NPRT, PRT(1)
C--->
       COMMON/SERVZ1/ TABSPC(1)
       COMMON/SERVZ2/ TABTOK(1)
       COMMON/SERVZ3/ TABTIP(1)
       COMMON/SERVZ4/ NAMECR(80)
       COMMON/SERVZ5/ KCOM, COMENT(100)
       COMMON/SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
C---> HOBOCT�
       COMMON/SERVZ7/ MAKMOD(3), MAKSEQ, NAMEB
       COMMON/SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
C  ᯨ᪨ �࠭���� ����஢ 㧫�� � ����஢ ⮪��, ������������� � SPOOL
C   �� ����� � ��४⨢� #SPOOL (䫠� FTRSPL{MASPRT(121)}
       COMMON /PRINTN/ NW6
C******�O BK�. A��APATA "REGION" **************************************
       DIMENSION XGRAF(10 000)
       DIMENSION TOKN(200), TOKN1(200),VALLN(50),VALLN1(50), WORKX(50)
       DIMENSION JALL(50), YALL(50,50), LOGYTR(50), YALLHP(50,50)
       DIMENSION MSWALL(2000), WORKY(50)
       DIMENSION KAR(80), DAT(21), MAKNAM(3)
C**********************************************************************
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( TN   , MASPRT(29) )
       EQUIVALENCE( FMSWAL, MASPRT(74) )
       EQUIVALENCE( NNTOK,  MASPRT(46) )
       EQUIVALENCE( INTIM,  MASPRT(119) )
       EQUIVALENCE( NSXEM,  MASPRT(120) )
       EQUIVALENCE( NZRDR,  MASPRT(127) )
       INTEGER FMSWAL
       REAL JALL , MSWALL
       INTEGER REGION, PRNT, FUNTAB, TABSPC, TABTOK, TABTIP, COMENT
       INTEGER        SPISNN, SPISVL,SPISTK
       INTEGER MYY,DYY,YYY,HXX,MXX,SXX
       DATA NDX/10 000/, NXTOK/200/
       DATA MAKNAM/' ***', ' MS1', ' ***'/, NAMEB1/'�OP�'/
       DATA nnn_1/1997673236/, nnn_2/2010028909/
C-->  �H���A���A��� ----
C         NAMEB = NAMEB1
          NAMEB = nnn_1 + nnn_2
C  T...C....
       call intprt
       CALL FIDEF(MSWALL, TOKN, VALLN, KAR, DAT,irest)
       CALL DRMET0(1,0)
       MAKSEQ = 0
       DO 6661 I=1,3
6661   MAKMOD(I) = MAKNAM(I)
C
C  T����
       CALL INTBSU( MAKNAM )
       NW6 = NWTRE
       FMSWAL = 0
C**********************************************************************
C######################################################################
         call beg99
C######################################################################
C----------------------------------------------------------------------
C**********************************************************************
C      KOD = 0
C      CALL REGCPU(KOD, KDIM, MASKOD, NBEG, IEROR )
C
C
C  *** �P�EM ��PEKT�B:
C
C  �O �OPM�POBAH��  MSWALL( #�E�� ), MO�HO O�PA�OTAT� TO��KO:
C
C  #�E��(4), #METO�(5), %(1), #�M�(2), #�P�M(3), #CTO�(11)
C #BOCT(13),  #��A�(14), #C�PAB(15),  HELP(16), #X�E�� (17)
C----------------------------------------------------------------------
C      DO 6660 I=1,3
C6660   MAKMOD(I) = MAKNAM(I)
100    CONTINUE
       CALL SEQDIR(KDIR, KAR, DAT, IRC )
C      write(6,*) 'NZRDR=', NZRDR
       CALL INTBSU( MAKNAM )
       IF ( IRC .EQ. 2 ) CALL STOPP(666)
C-->  ��PEKT�B�, KOTOP�E BCE��A MO�HO �P�HTMAT�:
       IF ( KDIR .EQ. 11 ) GO TO 11
       IF ( KDIR .EQ. 13 ) GO TO 13
       IF ( KDIR .EQ.  5 ) GO TO  5
       IF ( KDIR .EQ. 15 ) GO TO 15
       IF ( KDIR .EQ. 16 ) GO TO 16
       IF ( KDIR .EQ.  4 ) GO TO  4
       IF ( KDIR .EQ. 17 )  GO TO 17
       IF ( KDIR .EQ. 19 )  GO TO 19
C
C--->   C�E�. ��P. TO�8KO �OC�E �OPM�POBAH�� MSWALL
C
       IF ( FMSWAL .EQ. 1 ) GO TO 101
       WRITE(NWTR, 200)
200    FORMAT(/' *** HAP��EH �OP��OK C�E�OBAH�� ��PEKT�B ***'/
     = ' *** HET O��CAH�� �E�� -> ��PEKT�B� B��O�H�T� HEBO�MO�HO ***'/)
       GO TO 100
C**********************************************************************
101    CONTINUE
       IF ( KDIR .EQ. 3 ) GO TO 3
       IF ( KDIR .EQ. 6 ) GO TO 6
       IF ( KDIR .EQ. 7 ) GO TO 7
       IF ( KDIR .EQ. 8 ) GO TO 8
       IF ( KDIR .EQ. 9 ) GO TO 9
       IF ( KDIR .EQ. 10 ) GO TO 10
       IF ( KDIR .EQ. 12 ) GO TO 12
       IF ( KDIR .EQ. 18 ) GO TO 18
       IF ( KDIR .EQ. 20 ) GO TO 20
       IF ( KDIR .EQ. 21 ) GO TO 21
       IF ( KDIR .EQ. 22 ) GO TO 22
       IF ( KDIR .EQ. 23 ) GO TO 23
       IF ( KDIR .EQ. 24 ) GO TO 24
       IF ( KDIR .EQ. 25 ) GO TO 25
       IF ( KDIR .EQ. 26 ) GO TO 26
C--->  EC�� �O�A�� C��A --> �O�. C�O�!?!?!?!?!
       CALL SUBERR(KDIR)
C######################################################################
C######################################################################
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
666    CONTINUE
       WRITE(NWTR,9000)
9000   FORMAT(/' *** ��PEKT�BA �OKA HE PEA���OBAHA ***'/ )
       GO TO 100
C######################################################################
3      CONTINUE
C    #PE��M
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C        CALL PRINT1( MSWALL )
C        CALL DRMET0( 3, 1 )
C        CALL SUBERR( 3 )
C
C
C
C######################################################################
        CALL DRREG0( KAR, DAT, MSWALL, TOKN, VALLN )
       GO TO 100
C######################################################################
4      CONTINUE
C   #�E��                       "CTAT��ACKOE" B��E�EH�E
       CALL DRCIRC(KAR, DAT, MSWALL, IRCC)
4000   CONTINUE
       IF ( IRCC .NE. 0 ) GO TO 4004
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      IF( INTIM .EQ. 1 ) CALL PRINT1( MSWALL, MSWALL )
C      CALL DRMET0( 3, 1 )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C######################################################################
4006   CONTINUE
C--> CTAT. KOHTPO�� HA NNTOK.....
       IF ( NXTOK .GE. NNTOK ) GO TO 4001
C-->  MA�O HAM..
       WRITE(NWTR,  4002 ) NNTOK, NXTOK
       WRITE(NWTRE, 4002 ) NNTOK, NXTOK
4002   FORMAT(/' *** �E�� CO�EP��T ', I5, ' B�BO�H�X ( COXPAH�EM�X )',
     = ' �HA�EH�� ***'/ ' *** �AHHA� BEPC�� �O��CKAET TO��KO ', I5,
     = ' �HA�EH�� ***'/ ' *** O�PAT�TEC� K PA�PA�OT��K� "***MS1***"',
     = ' ***'/)
       CALL STOPP(20)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
4004   IF ( DIAL .EQ. 1 ) GO TO 4007
       WRITE(NWTR,  4005 )
       WRITE(NWTRE, 4005 )
4005   FORMAT(/' ***** PA�OTA ABAP��HO �ABEP�EHA *****'/)
       CALL STOPP(20)
4007   WRITE(NWTR,  4008 )
       WRITE(NWTRE, 4008 )
4008   FORMAT(/' ***** BH�MAH�E: O���K� B O��CAH�� �E�� *****'/)
       GO TO 4006
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C######################################################################
4001   CONTINUE
C  ****  �P� REGCPU -->  B��E��ETC� O.�. �O� MSWALL
C******GO TO 3003
       GO TO 100
C######################################################################
5      CONTINUE
C   #METO�
       CALL DRMET0(2,0)
       GO TO 100
C######################################################################
6      CONTINUE
C    #�E�
       CALL DRPRNT(KAR, DAT, MSWALL, TOKN, VALLN)
       GO TO 100
C######################################################################
7      CONTINUE
C   #TA�
       CALL DRTAB0( KAR, DAT, MSWALL, TOKN, VALLN )
       GO TO 100
C######################################################################
8      CONTINUE
C   #�PA�
C  *** �P� REGCPU --> "���T�" BCE MACC�B�, KPOME TOKN, VALLN � CO��AT
C                       XGRAF
C
       CALL DRGRF0( KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
       GO TO 100
C######################################################################
9      CONTINUE
C    #�PA�1
C      CALL DRG1F0(KAR,DAT,MSWALL, TOKN, VALLN, NDX, XGRAF )
       GO TO 100
C######################################################################
10     CONTINUE
C  #C�ET
C      IF( INTIM .NE. 2 ) GO TO 10000
C      OPEN (UNIT=70,FILE='MS1-1989.MSW',FORM='UNFORMATTED')
C      OPEN (UNIT=71,FILE='MS1-1989.TV',FORM='UNFORMATTED')
C      CALL SAVE_ALL( 70, 71, NSXEM, DAT, MSWALL, TOKN, VALLN, 0)
10000  CONTINUE
       CALL DRDIG0( KAR, DAT, MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =     YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
       GO TO 100
C######################################################################
11     CONTINUE
C   #CTO�
       WRITE(NWTR, 11000)
11000  FORMAT(///' *** #CTO� �ਭ��:==> ࠡ�� �����襭� ***'///)
         call end99
       CALL STOPP(0)
C######################################################################
12     CONTINUE
C  #COXP
       CALL DRSAV0( KAR, DAT, MSWALL, TOKN, VALLN )
       GO TO 100
C######################################################################
13     CONTINUE
C  -  #BOCT
        CALL DRRES0( KAR, DAT, MSWALL, TOKN, VALLN )
C$$$$$$GO TO 100
       GO TO 4000
C######################################################################
15     CONTINUE
C    #C�PAB
C      CALL DR>.....
       GO TO 666
C######################################################################
16     CONTINUE
C   HELP
C    CALL DR.....
       GO TO 666
C######################################################################
17     CONTINUE
C   #X�E��  <#KAHA�A> --> �POCTE���E MAKPO�O�CTAHOBK�........
       CALL DRCIRX( KAR, DAT, MSWALL, IRCC)
C
       GO TO 4000
C######################################################################
18     CONTINUE
C  #�C�ET
       CALL DRDIGP( KAR, DAT, MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =        YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
       GO TO 100
C######################################################################
19     CONTINUE
C    #ACOXP
       CALL DRSAVA( KAR, DAT )
       GO TO 100
C######################################################################
20     CONTINUE
C   #C�EKTP
       CALL DRFURE( KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
C
       GO TO 100
C######################################################################
21     CONTINUE
C    #MO�HOCT�
C$$$$$$CALL DRPPPP( KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
       GO TO 100
C######################################################################
22       CONTINUE
C  #MPE��M
C        CALL DRRMMM( MSWALL, TOKN )
         GO TO 100
C######################################################################
23     CONTINUE
C   #BHE�
       CALL DRout0( KAR, DAT, MSWALL, TOKN, VALLN )
       GO TO 100
C######################################################################
24       CONTINUE
C  #SPOOL
         CALL DRSPOL( KAR, DAT, MSWALL, TOKN, VALLN )
C         write(3,*) 'SPISVL=',SPISVL
C         write(3,*) 'SPISTK=',SPISTK
C         write(99,*) 'SPISVL=',SPISVL
C         write(99,*) 'SPISTK=',SPISTK
         GO TO 100
C######################################################################
25       CONTINUE
C  #������
         CALL DRREGS( KAR, DAT, MSWALL, TOKN, VALLN )
         GO TO 100
C######################################################################
26       CONTINUE
C  #�������
         CALL DRREGL( KAR, DAT, MSWALL, TOKN, VALLN )
         GO TO 100
C######################################################################
C######################################################################
C######################################################################
       END
