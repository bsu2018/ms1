C====== MS1DIGIT======================================================
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0  (OT AB��CTA 1990 �.)------------
C   KOPHEBA�_�PO�PAMMA: MS1DIGIT
C  -------------------
C   HA�HA�EH�E:        ��PAB�EH�E #���� � #�����
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
         EQUIVALENCE(JNAME,   MASPRT(125))
       EQUIVALENCE( NZRDR,  MASPRT(127) )
       EQUIVALENCE( FLGDIG,  MASPRT(128) )
       INTEGER FMSWAL, FLGDIG
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
        IF (FLGDIG .EQ. 0 ) THEN
         call beg99(irest,1)
C  #����       ........
       CALL DRDIG0( KAR, DAT, MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =     YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
       call savems(mswall, tokn, valln, kar, dat )
       STOP 6
       ELSE
C
C  #�C�ET
         call beg99(irest,2)
C
       CALL DRDIGP( KAR, DAT, MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =        YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
       call savems(mswall, tokn, valln, kar, dat )
       STOP 6
C######################################################################
       ENDIF
C######################################################################
C######################################################################
C######################################################################
       END
