C====== MS1_INPT  ======================================================
CXXX----KOMèãEKC "MS1" --> BEPCàü 2.0  (OT åÄêíÄ   1990 É.)------------
C   KOPHEBAü_èPOÉPAMMA: MS1_INPT
C  -------------------
C   HAáHAóEHàE:        ÇÇéÑ ñÖèà
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
C-->
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
       CHARACTER NFI_JOB*4, NFI_MSW*4, NFI_CON*4, NFI_REZ*4, NFI_TV0*4
       DATA NFI1/'MS1D_F'/
       DATA NFI_JOB/'.JOB'/
       DATA NFI_MSW/'.MSW'/
       DATA NFI_CON/'.CON'/
       DATA NFI_REZ/'.REZ'/
       DATA NFI_TV0/'.TV0'/
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
         NFI = NFI1//NFI2//NFI_REZ
         OPEN (UNIT=03,FILE=NFI)
         NFI = NFI1//NFI2//NFI_JOB
         OPEN (UNIT=05,FILE=NFI)
         NFI = NFI1//NFI2//NFI_CON
         OPEN (UNIT=06,FILE=NFI)
         NFI = NFI1//NFI2//NFI_MSW
         OPEN (UNIT=70,FILE=NFI,FORM='UNFORMATTED')
         NFI = NFI1//NFI2//NFI_TV0
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
100    CONTINUE
       CALL SEQDIR(KDIR, KAR, DAT, IRC )
       CALL INTBSU( MAKNAM )
       IF ( IRC .EQ. 2 ) STOP 666
C-->  ÑàPEKTàBõ, KOTOPõE BCEÉÑA MOÜHO èêàçàåÄTú:
       IF ( KDIR .EQ. 11 ) GO TO 11
       IF ( KDIR .EQ. 5  ) GO TO 5
       IF ( KDIR .EQ. 4  ) GO TO 4
C
C--->   CãEÑ. ÑàP. TOãúKO èOCãE îOPMàPOBAHàü MSWALL
C
       IF ( FMSWAL .EQ. 1 ) GO TO 101
       WRITE(NWTR, 200)
200    FORMAT(/' *** çÄêìòÖç èéêüÑéä ëãÖÑéÇÄçàü ÑàêÖäíàÇ ***'/
     = ' *** çÖí éèàëÄçàü ñÖèà -> ÑàêÖäíàÇì Çõèéãçàíú çÖÇéáåéÜçé ***'/)
       GO TO 100
C**********************************************************************
101    CONTINUE
       IF ( KDIR .EQ. 3 ) GO TO 3
       IF ( KDIR .EQ. 6 ) GO TO 6
C--->  ECãà èOèAãà CûÑA --> ãOÉ. CÅOâ!?!?!?!?!
       CALL SUBERR(KDIR)
C######################################################################
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
666    CONTINUE
       WRITE(NWTR,9000)
9000   FORMAT(/' *** ÑàPEKTàBA èOKA HE PEAãàáOBAHA ***'/ )
       GO TO 100
C######################################################################
3      CONTINUE
C    #PEÜàM
        CALL DRREG0( KAR, DAT, MSWALL, TOKN, VALLN )
       GO TO 100
C######################################################################
4      CONTINUE
C   #ñEèú                       "ëíÄíàóÖëäéÖ" ÇõÑÖãÖçàÖ
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
C--> CTAT. KOHTPOã8 HA NNTOK.....
       IF ( NXTOK .GE. NNTOK ) GO TO 4001
C-->  MAãO HAM..
       WRITE(NWTR,  4002 ) NNTOK, NXTOK
       WRITE(NWTRE, 4002 ) NNTOK, NXTOK
4002   FORMAT(/' *** ñÖèú ëéÑÖêÜàí ', I5, ' ÇõÇéÑçõï ( ëéïêÄçüÖåõï )',
     = ' áçÄóÖçàâ ***'/ ' *** ÑÄççÄü ÇÖêëàü ÑéèìëäÄÖí íéãúäé ', I5,
     = ' áçÄóÖçàâ ***'/ ' *** éÅêÄíàíÖëú ä êÄáêÄÅéíóàäì "***MS1***"',
     = ' ***'/)
       STOP 25
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
4004   IF ( DIAL .EQ. 1 ) GO TO 4007
       WRITE(NWTR,  4005 )
       WRITE(NWTRE, 4005 )
4005   FORMAT(/' ***** êÄÅéíÄ ÄÇÄêàâçé áÄÇÖêòÖçÄ *****'/)
       STOP  4
4007   WRITE(NWTR,  4008 )
       WRITE(NWTRE, 4008 )
4008   FORMAT(/' ***** ÇçàåÄçàÖ: éòàÅäà Ç éèàëÄçàà ñÖèà *****'/)
       GO TO 4006
C######################################################################
4001   CONTINUE
C  ****  èPà REGCPU -->  BõÑEãüETCü O.è. èOÑ MSWALL
C******GO TO 3003
       GO TO 100
C######################################################################
5      CONTINUE
C   #METOÑ
       CALL DRMET0(2,0)
       GO TO 100
C######################################################################
6      CONTINUE
C    #èEó
       CALL DRPRNT(KAR, DAT, MSWALL, TOKN, VALLN)
       GO TO 100
C######################################################################
C######################################################################
11     CONTINUE
C   #ëíéè
       CALL SAVE_ALL( 70, 71, NSXEM, DAT, MSWALL, TOKN, VALLN, 0)
       WRITE(NWTR, 11000)
11000  FORMAT(///' *** #CTOè èPàHüTA:==> PAÅOTA áABEPòEHA ***'///)
       CALL END99
       STOP 000
C######################################################################
C######################################################################
       END
