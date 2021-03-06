C====== MS1CIRCT======================================================
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0  (OT AB��CTA 1990 �.)------------
C   KOPHEBA�_�PO�PAMMA: MS1CIRCT
C  -------------------
C   HA�HA�EH�E:        O��EE ��PAB�EH�E ���������� #���� � #�����
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
C***** DIMENSION XGRAF(10 000)
C***** DIMENSION TOKN(200), TOKN1(200),VALLN(50),VALLN1(50), WORKX(50)
       DIMENSION TOKN(200),            VALLN(50)
C***** DIMENSION JALL(50), YALL(50,50), LOGYTR(50), YALLHP(50,50)
C***** DIMENSION MSWALL(2000), WORKY(50)
       DIMENSION MSWALL(2000)
       DIMENSION KAR(80), DAT(21), MAKNAM(3)
C**********************************************************************
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( TN   , MASPRT(29) )
       EQUIVALENCE( FMSWAL, MASPRT(74) )
       EQUIVALENCE( NNTOK,  MASPRT(46) )
       EQUIVALENCE( INTIM,  MASPRT(119) )
       EQUIVALENCE( NSXEM,  MASPRT(120) )
         EQUIVALENCE(JNAME,   MASPRT(125))
       EQUIVALENCE( NZRDR,  MASPRT(127) )
       EQUIVALENCE( FLGCIR,  MASPRT(129) )
       INTEGER FMSWAL, FLGCIR
C**********************************************************************
       REAL JALL , MSWALL
         CHARACTER*8 JNAME
         CHARACTER*12 START_N
       INTEGER REGION, PRNT, FUNTAB, TABSPC, TABTOK, TABTIP, COMENT
       INTEGER        SPISNN, SPISVL,SPISTK
CCCCCC INTEGER MYY,DYY,YYY,HXX,MXX,SXX
         DATA START_N/'MS1_DATA.$$$'/, k_kk/'.K'/
       DATA NDX/10 000/, NXTOK/200/
       DATA MAKNAM/' ***', ' MS1', ' ***'/, NAMEB1/'�OP�'/
       DATA nnn_1/1997673236/, nnn_2/2010028909/
C-->  �H���A���A��� ----
C         NAMEB = NAMEB1
          NAMEB = nnn_1 + nnn_2
       call intprt
C  T...C....
       CALL FIDEF(MSWALL,TOKN, VALLN,KAR, DAT, irest )
       MAKSEQ = 0
       DO 6661 I=1,3
6661   MAKMOD(I) = MAKNAM(I)
C
C  T����
       CALL INTBSU( MAKNAM , IREST )
       NW6 = NWTRE
C**********************************************************************
C######################################################################
C######################################################################
        IF (FLGCIR .EQ. 0 ) THEN
C   #�E��                       "CTAT��ACKOE" B��E�EH�E
         call beg99(irest,3)
       CALL DRCIRC(KAR, DAT, MSWALL, IRCC)
4000   CONTINUE
       IF ( IRCC .NE. 0 ) GO TO 4004
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       IF( INTIM .EQ. 1 ) CALL PRINT1( MSWALL, MSWALL )
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
CCCCCC GO TO 100
C######################################################################
       call savems(mswall, tokn, valln, kar, dat )
       STOP 6
       ELSE
C
C   #X�E��  <#KAHA�A> --> �POCTE���E MAKPO�O�CTAHOBK�........
         call beg99(irest,4)
       CALL DRCIRX( KAR, DAT, MSWALL, IRCC)
C
       GO TO 4000
C######################################################################
CCC    STOP 4
C######################################################################
       ENDIF
C######################################################################
C######################################################################
C######################################################################
       END
