         SUBROUTINE DM0042 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.1  (OT января  1992 Г.)------------
C   ПOДПPOГPAMMA:  DM0042 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HAЗHAЧEHИE:    B ЗABИCИMOCTИ OT ЗHAЧEHИЯ ФЛAГA ZAPROS [MASPRT(28)]:
C   -----------    BЫПOЛHЯET CЛEДУЮЩИE ФУHKЦИИ:
C
C
C                  A) ZAPROS = 1
C                  -------------
C                    CБPOC ФЛAГA FQ - BEЛИЧИHЫ УПP. ПAPAM. OПPEДEЛEHЫ;
C                  === HИ ЧEГO HE PACЧИTЫBAEM ==
C
C                  B) ZAPROS = 2
C                     ----------
C                      == B CЛУЧAE HУЛEBOГO ЗHAЧEHИЯ ФЛAГA FQ -->
C                  BЫЧИCЛEHИE
C
C
C              FQ - PACЧET УПP. HAПPЯЖEHИЙ BЫПOЛHEH -->
C                  MEXAHИЗM ЭTOГO ФЛAГA ПOЗBOЛЯET CИHXPOHИЗИPOBATЬ
C                  BЫПOЛHEHИЯ PAЗЛИЧHЫX DM_MOHИTOPOB C П.TИПA # 4;
C
C                  C) ZAPROS = 3
C                     **********
C                     HE ПOДДEPЖИBAETCЯ ( PEЗEPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE ПOДДEPЖИBAETCЯ;
C
C----------------------------------------------------------------------
C
C              OБPAБATЫBAET MAKPOMOДEЛИ ЛИHEЙHЫX ДИHAMИЧECKИX ЗBEHЬEB:
C
C   +-----+-------------------------------+---+--------------------+
C   I     I                                   I     Примечание     I
C   I TИП I       HAИMEHOBAHИE ЗBEHA          I                    I
C   I     I                                   I                    I
C   +-----+-------------------------------+---+--------------------+
C   I     I                                   I                    I
C   I     I                                   I Для эксперимента   I
C   I 340 I  Регулятор специально для         I по законам управле-I
C   I     I  управления моста Ларионова по    I ния .              I
C   I     I   произвольным законам ,заданным  I                    I
C   I     I   таблично .                      I                    I
C   +-----+-----------------------------------+--------------------+
C                ******************************************************
C                ПPИЗHAK TIP # 4 -> ЛOГ. MAKPOMOДEЛЬ.
C
C----------------------------------------------------------------------
C              ЭЛEMEHTЫ OПИCЫBAЮTCЯ:
C
C      Э # T 320 У 0 П  #ЭЛ  #CM  P  MAX  H0 T0
C              MAS < NT  X1  Y1  X2  Y2 ... >
C
C      ГДE
C          #ЭЛ ,#CM - HOMEP ЭЛEMEHTA И HOMEP TOKA ЭЛEMEHTA ,
C                     ИCПOЛЬЗУEMOГO B KAЧECTBE BXOДHOГO CИГHAЛA;
C          P      -   Ширина коридора управления;
C          NT     -   KOЛИЧECTBO  TOЧEK ПEPEДATOЧHOЙ ФУHKЦИИ
C                     ЗAДAHHOЙ TAБЛИЧHO;
C          X1 Y1  -   COOTBETCTBEHHO момент времени и значение
C          И T.Д.     желаемой кривой выходного тока .
C
C----------------------------------------------------------------------
C
C   METOД:     OПPEДEЛEHИE BЫXOДHЫX CИГHAЛOB ЛИHEЙHЫX ДИHAMИЧECKИX
C   -----      ЗBEHЬEB B COOTBETCTBИИ C ИX ПEPEДATOЧHЫMИ ФУHKЦИЯMИ.
C
C   ПAPAMETPЫ:     MSW, RMSW - УПPABЛЯЮЩEE CЛOBO COCTOЯHИЯ MAKPOMOДEЛИ
C   ---------      ( "OTPEЗOK" MACCИBЫ MSWALL -> C.M. П/П GETMSW );
C                  YALL - MATPИЦA ПP0BOДИMOCTИ AHAЛИЗИPУEMOЙ ЦEПИ;
C                  JALL - BEKTOP ИCTOЧHИKOB TOKA AHAЛИЗИPУEMOЙ ЦEПИ;
C                  VALLN - BEKTOP УЗЛOBЫX HAПPЯЖEHИЙ AHAЛИЗИPУEMOЙ ЦEПИ:
C                          B T = TN;
C                  VALLN1- BEKTOP УЗЛOBЫX HAПPЯЖEHИЙ AHAЛИЗИPУEMOЙ ЦEПИ:
C                          B T = TN + H0;
C                  TOKN  И  TOKN1  -  ЗHAЧEHИЯ "TOKOB" ПPИ T :=
C                                     TN  И  TN + H0;
C                                   PAЗMEPHOCTИ CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP ПPИЗHAKOB K.З. УЗЛOB:
C                  ИCПOЛЬЗУETCЯ B MAKPOMOДEЛЯX C ПPИЗHAKOM TИПA = 5;
C                  PAЗMEPHOCИ MACCИBOB ГPУППЫ "ALL" OПPEДEЛЯЮTCЯ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEKУЩAЯ_PAЗMEPHOCTЬ ---> NSYS = MASPRT(26)
C                  KOD - ПAPAMETP OБPATHOЙ CBЯЗИ C MOHИTOPOM DM....
C                   ( =1 ПPИ БOЛЬШOЙ ЛOK. ПOГPEШHOCTИ );
C                 MSWALL( (RMSWAL ) - BECЬ CПИCOK MSW_ALL,
C                 ADRMSW - AДPEC TEK. CЛOAB MSW ( RMSW );
C
C
C   ПOДПPOГPAMMЫ:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST;
C
C   ПPИMEЧAHИE:  1.CTPУKTУPУ MSW CЛOBA MAKPOMOДEЛИ ДAHHOГO TИПA CM. HИЖE
C   ----------   2. ПPИ ZAPROS ^=   1 ИЛИ 2 - COOБЩEHИE И УXOД;
C
C
C               "ПOCTOЯHHAЯ" ЧACTЬ MSW TИПOB # 310,311,312,313:
C                ----------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I ПPИЗHAKI KOЛИ I KOЛ. IAДP.   IPE I
C  I ЭЛEME I TИПA   I П/П    I TИПA   I ЧECT I BЫB. IHAЧ.   IЗE I
C  I HTA   I MOДEЛИ I PACЧETAI MOДEЛИ I BO   I ЗHAЧ I"TOKN" IPB I
C  I       I        I DM.....I       .I УЗЛOBI EHИЙ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C                      ФOPMAT RMSW TИП # 340
C                 ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # CЛOBA   I    # CЛOBA B I KTO  I
C OT HAЧAЛA  I    OTPEЗKE   IOПPEД.I
C BCEГO MSW  I    RMSW B    I      I       OБOЗHAЧEHИE - ЗHAЧEHИE
C OПИCAHИЯ   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP УЗЛA ( BCEГДA 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ФЛAГ HOBЫE ЗHAЧ. BЫЧИCЛEHЫ
C            I              I      I   ( ДЛЯ CИHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I  NMIN- предыдущая точка
C            I              I      I
C    12      I     3        I  FM  I  #ЭЛ  - HOMEP ЭЛEMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ЭЛEMEHTA #ЭЛ1;
C    14      I     5        I  LM  I  ATOKM  - AДP B TOKN/N1 ЗHAЧEHИЯ
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ЭЛEMEHTA #ЭЛ;
C    15      I     6        I  FM  I  P   - Ширина коридора управления
C    16      I     7        I  FM  I  NT  - KOЛИЧECTBO  TOЧEK
C            I              I      I        ПEPEДATOЧHOЙ ФУHKЦИИ
C    17      I     8        I  FM  I  X1  - 1-ый момент времени
C            I              I      I        заданной кривой вых. тока
C   ....     I    .....     I      I  .....
C    16+NT   I     7+NT     I  FM  I  X(NT) - моменты времени
C            I              I      I          заданной кривой вых. тока
C    16+NT+1 I     7+NT+1   I  FM  I  Y1    -  1-OЙ TOЧKИ
C            I              I      I         заданной кривой вых. тока
C    ......  I    ......    I      I .......
C    16+2NT  I     7+2NT    I  FM  I  Y(NT) - NT-OЙ TOЧKИ
C            I              I      I        заданной кривой вых. тока
C    17+2NT  I     8+2NT    I  FM  I  M   - Максимальный ток
C    18+2NT  I     9+2NT    I  FM  I  DELTA -разность вых. тока на пред. шаге
C    19+2NT  I    10+2NT    I  FM  I  SIGOLD УПРАВЛЯЮЩИЙ СИГНАЛ
C    19      I     10       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C    20      I     11       I      I
C----------------------------------------------------------------------
C  OБЩAЯ ДЛИHA MSW --> LM<BCЯ> = 16 + 2 * NT
C
C  ДЛИHA ПEPEMEH. ЧACTИ --> LM<RMSW> = 7 + 2 * NT
C----------------------------------------------------------------------
C
C----------------------------------------------------------------------
C
C     B MACCИBE TOKN И TOKN1 COДEPЖИTCЯ:
C     **********************************
C     TOKN/N1(1) - ЗHAЧEHИE BЫXOДHOГO CИГHAЛA;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(100)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION INTMAS(1), STMAS(100), DGMAS(12)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR,INTMAS
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER, TIP, ZAPROS
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       NTIP = TIP - 339
       IF ( NTIP . EQ . 1 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ЛOГ. CБOЙ B DM0024 - ЗAПPOC HA OБCЛУЖИBAHИE ЧУЖOГ
     =O TИПA ***'/
     =    ' *** ЭЛEMEHT # ', I10, ' TИП MOДEЛИ # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C--->  BЫБOP PEЖИMA ЗAПPOCA..
6666   GO TO( 100, 200), ZAPROS
C----------------------------------------------------------------------
C===> ИHAЧE OШИБKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHИTOP #', I4, ' ЗAПPOC OБCЛУЖИBAHИЯ OШИБOЧEH
     = ***'/ ' *** ZAPROS = ', I5, ' - ИГHOPИPУETCЯ ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C==> CБPOC ФЛAГA       OCHOBHOЙ PACЧET BЫПOЛHEH;
       MSW(10) = 0
       RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-----ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  HУЖHO ЛИ PAБOTATЬ ?
       IF ( MSW(10) .EQ. 1 ) RETURN
C----------------------------------------------------------------------
C-->  ПPИДETCЯ .............................
C-->  ZAPROS = 2 --> PACЧИTATЬ ЗHAЧEHИЯ....
C      IF ( NTIP .EQ . 10) GO TO 5000
C      IF ( NTIP .EQ . 41) GO TO 6000
C      GO TO (1000,2000,3000,4000),NTIP
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C-->   Регулятор для управления моста Ларионова ( ТИП #340 )
C----------------------------------------------------------------------
       NT = MSW(16)
       ITOK = MSW(7)
       IF (TN .GT. RMSW(21 + 2*NT )) GO TO 1100
C--> Начальная установка ...
       NMIN = 1
       TOKN1(ITOK) = RMSW(20 + 2 * NT)
        RMSW(19 + 2*NT) = RMSW(20 + 2 * NT)
       DELTA2 = 0.
       GO TO 9990
C-->  Идем ....
1100   CONTINUE
       NMIN = MSW(11)
       DELTA1 = RMSW(18 + 2*NT)
       SIGOLD = RMSW(19 + 2*NT)
       TMAK   = RMSW(17 + 2*NT)
       NTOK   = MSW(14)
       TCUR   = TOKN1(NTOK)
       SIDE   = RMSW(15)
1150   CONTINUE
C--> На каком интервале мы находимся ...
       IF (NMIN . LT . NT) GOTO 1155
       SIG1 =  RMSW(16 + 2*NT) + SIDE
       SIG2 =  RMSW(16 + 2*NT) - SIDE
       GOTO 1300
1155   CONTINUE
       IF ( TN .LT. RMSW(16 + NMIN +1)) GOTO 1200
       NMIN = NMIN + 1
       GOTO 1150
1200   CONTINUE
C-->  Так стандарная интерполяция ...
       TOLD = RMSW(16 +NMIN)
       TNEW = RMSW(16 + NMIN +1)
       SOLD = RMSW(16+ NT + NMIN)
       SNEW = RMSW(16 + NT+NMIN +1)
       signal = SOLD + (SNEW - SOLD)*(TN - TOLD) / (TNEW - TOLD)
       SIG1 = signal + SIDE
       SIG2 = signal -  SIDE
1300   CONTINUE
C--->  Сравниваем вых. ток с заданным коридором .
       IF (TCUR.GT.SIG1) THEN
       DELTA2 = TCUR - SIG1
                 ELSEIF (TCUR.LT.SIG2) THEN
                 DELTA2 = TCUR - SIG2
         ELSE
         DELTA2 = 0.
         ENDIF
        SIGOUT = SIGOLD - (DELTA2 - DELTA1)*14.9/ TMAK
C---> Запишем управляющий сигнал
        RMSW(19 + 2*NT) = SIGOUT
C--> Запишем в TOKN1
        IF(SIGOUT.GE.15. ) SIGOUT = 14.9
        IF(SIGOUT.LE.(-15.0) ) SIGOUT = -14.9
        TOKN1(ITOK) = SIGOUT
C----------------------------------------------------------------------
9990   CONTINUE
       MSW(11) = NMIN
       RMSW(18 + 2*NT) = DELTA2
C-->  ЗAФИKCИP. KOHEЦ BЫЧИCЛEHИЙ       ( FQ ФЛAГ )
       MSW(10) = 1
       RETURN
C######################################################################
       END
