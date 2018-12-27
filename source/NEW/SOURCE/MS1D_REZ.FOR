C====== MS1_REGC  ======================================================
CXXX----KOMèãEKC "MS1" --> BEPCàü 2.0  (OT åÄêíÄ   1990 É.)------------
C   KOPHEBAü_èPOÉPAMMA: MS1_REGC
C  -------------------
C   HAáHAóEHàE:        ÇõÇéÑ êÖáìãúíÄíéÇ
C  ------------
C   METOÑ:
C   -----
C   èAPAMETPõ:
C   ----------
C   èOÑèPOÉPAMMõ:      1. COMMON PROF1,2,3, SERVZ1,2,3,4,5,6,7
C   ------------
C   èPàMEóAHàE:         1.  V.M=1.* - CTATàóECKOE BõÑEãEHàE O.è.
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
C---> HOBOCTà
       COMMON/SERVZ7/ MAKMOD(3), MAKSEQ, NAMEB
       COMMON /PRINTN/ NW6
C******ÑO BKã. AèèAPATA "REGION" **************************************
       DIMENSION XGRAF(05 000)
       DIMENSION TOKN(200), TOKN1(200),VALLN(50),VALLN1(50), WORKX(50)
       DIMENSION JALL(50), YALL(50,50), LOGYTR(50), YALLHP(50,50)
       DIMENSION MSWALL(2000), WORKY(50)
       DIMENSION KAR(80), DAT(21), MAKNAM(3)
C**********************************************************************
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FMSWAL, MASPRT(74) )
       EQUIVALENCE( NNTOK,  MASPRT(46) )
       EQUIVALENCE( INTIM,  MASPRT(119) )
       EQUIVALENCE( NSXEM,  MASPRT(120) )
       INTEGER FMSWAL
       REAL JALL , MSWALL
       INTEGER REGION, PRNT, FUNTAB, TABSPC, TABTOK, TABTIP, COMENT
       CHARACTER NFI1*6, NFI2*2, NFI*12
       CHARACTER NFI_MSW*4, NFI_CON*4, NFI_TAB*4, NFI_TV*4
       CHARACTER NFI_FRM*4, NFI_SPL*4
       DATA NFI1/'MS1D_F'/
       DATA NFI_MSW/'.MSW'/
       DATA NFI_CON/'.CON'/
       DATA NFI_TAB/'.TAB'/
       DATA NFI_TV /'.TV '/
       DATA NFI_FRM/'.FRM'/
       DATA NFI_SPL/'.SPL'/
       DATA NDX/05 000/, NXTOK/200/
       DATA MAKNAM/' ***', ' MS1', ' ***'/, NAMEB1/'ÅOPû'/
C----------------------------------------------------------------------
C      éèàëÄçàÖ àëèéãúáìÖåõï îÄâãéÇ:
C----------------------------------------------------------------------
C--> ëóàíÄÖå çéåÖê ëïÖåõ
         OPEN (UNIT=30,FILE='MS1D_NCX.TEK')
         READ (30,1122) NFI2
1122     FORMAT(A2)
         OPEN (UNIT=99,FILE='CON')
         NFI = NFI1//NFI2//NFI_TAB
         OPEN (UNIT=03,FILE=NFI)
         NFI = NFI1//NFI2//NFI_CON
         OPEN (UNIT=06,FILE=NFI)
         NFI = NFI1//NFI2//NFI_FRM
         OPEN (UNIT=05,FILE=NFI)
         NFI = NFI1//NFI2//NFI_SPL
         OPEN (UNIT=09,FILE=NFI,FORM='UNFORMATTED')
         NFI = NFI1//NFI2//NFI_MSW
         OPEN (UNIT=70,FILE=NFI,FORM='UNFORMATTED')
         NFI = NFI1//NFI2//NFI_TV
         OPEN (UNIT=71,FILE=NFI,FORM='UNFORMATTED')
C**********************************************************************
         CALL BEG99
C**********************************************************************
C-->  àHàñàAãàáAñàü -----
         NAMEB = NAMEB1
C  T...C....
C**********************************************************************
       MAKSEQ = 0
       DO 6661 I=1,3
6661   MAKMOD(I) = MAKNAM(I)
C
C  Tññññ
       CALL INTBSU( MAKNAM )
       NW6 = NWTRE
       FMSWAL = 0
C**********************************************************************
C######################################################################
C--> ÇéëëíÄçéÇàå  "ÇëÖ"
       CALL READ_ALL( 70, 71, NSXEM, DAT, MSWALL, TOKN, VALLN, IRC )
       IF( IRC .EQ. 0 ) GO TO 100
       STOP 1
C######################################################################
100    CONTINUE
       CALL SEQDIR(KDIR, KAR, DAT, IRC )
       CALL INTBSU( MAKNAM )
       IF ( IRC .EQ. 2 ) STOP 666
C-->  ÑàPEKTàBõ, KOTOPõE BCEÉÑA MOÜHO èPàHTMAT8:
       IF ( KDIR .EQ. 11 ) GO TO 11
C
C--->   CãEÑ. ÑàP. TOãúKO èOCãE îOPMàPOBAHàü MSWALL
C
       IF ( FMSWAL .EQ. 1 ) GO TO 101
       WRITE(NWTR, 200)
200    FORMAT(/' *** HAPìòEH èOPüÑOK CãEÑOBAHàü ÑàPEKTàB ***'/
     = ' *** HET OèàCAHàü ñEèà -> ÑàPEKTàBì BõèOãHàTú HEBOáMOÜHO ***'/)
       GO TO 100
C**********************************************************************
101    CONTINUE
       IF ( KDIR .EQ. 6 ) GO TO 6
       IF ( KDIR .EQ. 7 ) GO TO 7
       IF ( KDIR .EQ. 8 ) GO TO 8
       IF ( KDIR .EQ. 9 ) GO TO 9
       IF ( KDIR .EQ. 20 ) GO TO 20
       IF ( KDIR .EQ. 21 ) GO TO 21
C--->  ECãà èOèAãà CûÑA --> ãOÉ. CÅOâ!?!?!?!?!
       CALL SUBERR(KDIR)
C######################################################################
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
666    CONTINUE
       WRITE(NWTR,9000)
9000   FORMAT(/' *** ÑàPEKTàBA èOKA HE PEAãàáOBAHA ***'/ )
       GO TO 100
C######################################################################
6      CONTINUE
C    #èEó
       CALL DRPRNT(KAR, DAT, MSWALL, TOKN, VALLN)
       GO TO 100
C######################################################################
7      CONTINUE
C   #TAB
       CALL DRTAB0( KAR, DAT, MSWALL, TOKN, VALLN )
       GO TO 100
C######################################################################
8      CONTINUE
C   #ÉPAî
C  *** èPà REGCPU --> "ìÅàT8" BCE MACCàBõ, KPOME TOKN, VALLN à COáÑAT
C                       XGRAF
C
       CALL DRGRF0( KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
       GO TO 100
C######################################################################
9      CONTINUE
C    #ÉPAî1
       CALL DRG1F0(KAR,DAT,MSWALL, TOKN, VALLN, NDX, XGRAF )
       GO TO 100
C######################################################################
11     CONTINUE
C   #CTOè
       WRITE(NWTR, 11000)
11000  FORMAT(///' *** #CTOè èPàHüTA:==> PAÅOTA áABEPòEHA ***'///)
       CALL END99
       STOP 000
C######################################################################
20     CONTINUE
C   #CèEKTP
       CALL DRFURE( KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
C
       GO TO 100
C######################################################################
21     CONTINUE
C    #MOôHOCT8
C$$$$$$CALL DRPPPP( KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
       GO TO 100
C######################################################################
C######################################################################
       END
