         SUBROUTINE DRCIRX( KAR, DAT, BA0, KNOT, PAR0, IRC )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 2.0
C   ПOДПPOГPAMMA:   DRCIRX( KAR, DAT, BA0, KNOT, PAR0, IRC )
C   ------------
C   HAЗHAЧEHИE:    *** OБPAБOTЧИK #XЦEПЬ:
C  ------------       #XЦEПЬ  N
C
C                  N - HOMEP KAHAЛA BBOДA ИHФOPMAЦИИ;
C
C                  ПPИ N "OПУЩEHO" ИЛИ "OШИБOЧHO" УCTAHABЛИBAETCЯ
C                  PABHЫM NRDR KAHAЛУ;
C
C                  ФOPMAPУET MACCИBЫ BA0, KNOT, PAR0
C                  ( 1-Я ЧACTЬ ДИPEKTИBЫ #XЦEПЬ )
C   METOД:         CM. П/П INPUT0;
C   -----
C   ПAPAMETPЫ:     KAR - CИMBOЛЬHAЯ CTPOKA ДИPEKTИBЫ,
C   ---------      DAT - MACCИB ЧИCEЛ ДИPEKTИBЫ;
C                  IRC = 1  MACCИBЫ BA0, KNOT, PAR0 OПPEДEЛEHЫ;
C                      = 2  ИHФOPMAЦИЯ HE ПPИHЯTA;
C                  ДP. ПAP. CM. INPUT0
C
C   ПOДПPOГPAMMЫ:  1. COMMON PROF1
C   -------------  2. INPUT0   PPNCOM  READMK
C   ПPИMEЧAHИE:    1. B CЛУЧAE "ФATAЛЬHЫX" OШИБOK - STOP 666
C   ----------        ( KOHEЦ CИCTEMHOГO KAHKЛA NRDR )
C                  2. ПOДPOБHEE CM. INPUT0
C                  3. MACCИBA BA0, KNOT, PAR0 - ИHИЦИИPOBAHHЫ;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         DIMENSION BA0(*), PAR0(*), KNOT(*), KAR(*), DAT(*), DSNAME(11)
       EXTERNAL READMK
         INTEGER BA0, FMSWAL
         EQUIVALENCE ( MASPRT(13),  NWTR )
         EQUIVALENCE ( MASPRT(89),  NMAK  )
         EQUIVALENCE ( MASPRT(14),  NWTRE )
         EQUIVALENCE ( MASPRT(74),  FMSWAL )
         EQUIVALENCE ( MASPRT(12),  NRDR   )
         DATA DSNAME/ 11*'    '/
C-->  C KAKOГO KAHAЛA ЗAПPOC BBOДA ДЛЯ #ЦEПЬ:
         NRDR1 = NRDR
         IF ( DAT(1) .EQ. 0.0 ) GO TO 1
C--> ЧTO-TO ECTЬ
         NRDR1 = IFIX( DAT(2) )
C--> KOHTPOЛЬ
         IF ( NRDR1 .LE. 0  .OR. NRDR1. GT. 100 .OR. NRDR1.EQ.NMAK.OR.
     =  NRDR1 .EQ. NWTR  .OR. NRDR1 .EQ. NWTRE ) NRDR1 = NRDR
C--> COOБЩИM..
1        WRITE(NWTR, 50 ) NRDR1, NMAK
50     FORMAT(/' *** #XЦEПЬ принята ==> описание цепи вводится через ',
     = 'канал # ',I2, ' ***'/,
     =  5X,'описание "макро подстановок" вводится через ',
     = 'канал # ',I2, ' ***'/)
         IRC = 1
C--->
       CALL PPNCOM(3,NWTR)
C--->
         ii_rr = 0
         IF ( NRDR1 .NE. NRDR ) CALL KSOPEN(NRDR1,1,ii_rr)
         IF ( ii_rr .NE. 0 )  CALL STOPP(666)
         CALL KSOPEN(NMAK,1,ii_rr)
         IF ( ii_rr .NE. 0 )  CALL STOPP(666)
         CALL INPUT0( BA0, KNOT, PAR0, NRDR1, DSNAME, READMK,  III )
         IF ( NRDR1 .NE. NRDR ) CLOSE (NRDR1)
         CLOSE (NMAK)
C--->ECЛИ #CБPOC ИЛИ ЧTO-TO ИГHOPИPOBAЛИ, TO BИДИMO "ПPИEXAЛИ"...
         IF ( III .EQ. 1  .OR. III .EQ. 2 ) IRC = 2
C--> A MOЖET ЭTO "УCE"
         IF ( III .EQ. 3 ) WRITE(NWTR,51)
51     FORMAT(//' *** Pабота прерывается - канал системного ввода недост
     =упен ***'//)
         IF ( III. EQ. 3 ) CALL STOPP(666)
         RETURN
         END
