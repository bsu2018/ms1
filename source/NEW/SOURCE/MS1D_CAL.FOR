C====== MS1_CALC ======================================================
CXXX----KOMèãEKC "MS1" --> BEPCàü 2.0  (OT åÄêíÄ   1990 É.)------------
C   KOPHEBAü_èPOÉPAMMA: MS1_CALC
C   -------------------
C   HAáHAóEHàE:        MOÑEãàPOBAHàE
C   ------------
C   METOÑ:             ÑàAKOèTàKA, MAKPOMOÑEãàPOBAHàE
C   -----
C   èAPAMETPõ:
C   ----------
C   èOÑèPOÉPAMMõ:      1. COMMON PROF1,2,3, SERVZ1,2,3,4,5,6,7
C   ------------
C   èPàMEóAHàE:         1.  V.M=1.* - CTATàóECKOE BõÑEãEHàE O.è.
C   -----------
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
       DIMENSION TOKN(200), TOKN1(200),VALLN(50),VALLN1(50), WORKX(50)
       DIMENSION JALL(50), YALL(50,50), LOGYTR(50), YALLHP(50,50)
       DIMENSION MSWALL(2000), WORKY(50)
       DIMENSION KAR(80), DAT(21), MAKNAM(3)
C**********************************************************************
       EQUIVALENCE( NWTR, MASPRT(13)   )
       EQUIVALENCE( NWTRE, MASPRT(14)  )
       EQUIVALENCE( FMSWAL, MASPRT(74) )
       EQUIVALENCE( DMSWAL, MASPRT(20) )
       EQUIVALENCE( NNTOK,  MASPRT(46) )
       EQUIVALENCE( NSYS ,  MASPRT(26) )
       EQUIVALENCE( INTIM,  MASPRT(119))
       EQUIVALENCE( NSXEM,  MASPRT(120))
       INTEGER FMSWAL, SPISON, DMSWAL
       REAL JALL , MSWALL
       INTEGER REGION, PRNT, FUNTAB, TABSPC, TABTOK, TABTIP, COMENT
       CHARACTER NFI1*6, NFI2*2, NFI*12
       CHARACTER NFI_MSW*4, NFI_CON*4, NFI_REZ*4, NFI_TV*4, NFI_TV1*4
       CHARACTER NFI_SCH*4, NFI_SPL*4
       DATA MAKNAM/' ***', ' MS1', ' ***'/, NAMEB1/'ÅOPû'/
       DATA NFI1/'MS1D_F'/
       DATA NFI_MSW/'.MSW'/
       DATA NFI_CON/'.CON'/
       DATA NFI_REZ/'.REZ'/
       DATA NFI_TV /'.TV '/
       DATA NFI_TV1/'.TV1'/
       DATA NFI_SCH/'.SCH'/
       DATA NFI_SPL/'.SPL'/
C-->  àHàñàAãàáAñàü -----
         NAMEB = NAMEB1
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
         NFI = NFI1//NFI2//NFI_CON
         OPEN (UNIT=06,FILE=NFI)
         NFI = NFI1//NFI2//NFI_SCH
         OPEN (UNIT=05,FILE=NFI)
         NFI = NFI1//NFI2//NFI_SPL
         OPEN (UNIT=09,FILE=NFI,FORM='UNFORMATTED')
         NFI = NFI1//NFI2//NFI_MSW
         OPEN (UNIT=70,FILE=NFI,FORM='UNFORMATTED')
         NFI = NFI1//NFI2//NFI_TV
         OPEN (UNIT=71,FILE=NFI,FORM='UNFORMATTED')
         NFI = NFI1//NFI2//NFI_TV1
         OPEN (UNIT=72,FILE=NFI,FORM='UNFORMATTED')
C----------------------------------------------------------------------
       MAKSEQ = 0
       DO 6661 I=1,3
6661   MAKMOD(I) = MAKNAM(I)
C
C  Tññññ
       CALL INTBSU( MAKNAM )
       NW6 = NWTRE
       FMSWAL = 0
C**********************************************************************
       CALL  BEG99
C######################################################################
C--> ÇéëëíÄçéÇàå  "ÇëÖ"
C######################################################################
       CALL READ_ALL( 70, 71, NSXEM, DAT, MSWALL, TOKN, VALLN, IRC )
       IF( IRC .EQ. 0 ) GO TO 100
       WRITE(99,9999)
9999   FORMAT(' ***   éòàÅäÄ èêà óíÖçàà îÄâãÄ   *** ')
       STOP 1
C######################################################################
C--> ÇéëëíÄçéÇàå èÄêÄåÖíêõ Ñãü #ëóÖí
C-->
100    CONTINUE
       READ(5,50) DAT(2), DAT(3), DAT(4)
50     FORMAT(5X, 3(1X,F11.8) )
C-->
       dat(1) = 3.0
C  #CóET
       CALL DRDIG0( KAR, DAT, MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     =     YALL, JALL, LOGYTR, WORKX, YALLHP, WORKY )
C######################################################################
C--> áÄèéåçàå  "ÇëÖ"
       CALL SAVE_ALL( 70, 72, NSXEM, DAT, MSWALL, TOKN, VALLN, 1 )
C######################################################################
       CALL  END99
       STOP 000
       END
