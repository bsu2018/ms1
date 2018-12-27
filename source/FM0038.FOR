       SUBROUTINE FM0038(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.1  (OT  января   1992 Г.)-----------
C   ПOДПPOГPAMMA: FM0038(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HAЗHAЧEHИE:  HACTPOЙKA MAKPOMOДEЛEЙ регулятора для управления моста:
C   ----------
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
C                ПPИЗHAK TIP # 4 -> ЛOГ. MAKPOMOДEЛЬ.
C                ( DM0042, LM0007 )
C                      ФOPMИPУET "ПEPEMEHHУЮ" ЧACTЬ RMSW OПИCAHИЯ
C
C                      ДЛИHA:  TИП 340:==>+1<VAR>
C----------------------------------------------------------------------
C              ЭЛEMEHT OПИCЫBAETCЯ:
C
C      Э # T 320 У 0 П  #ЭЛ  #CM  P   MAX  H0  T0
C              MAS < NT  X1  Y1  X2  Y2 ... >
C
C      ГДE
C          #ЭЛ ,#CM - HOMEP ЭЛEMEHTA И HOMEP TOKA ЭЛEMEHTA ,
C                     ИCПOЛЬЗУEMOГO B KAЧECTBE BXOДHOГO CИГHAЛA;
C          P      -   Ширина коридора управления;
C          MAX    -   Максимальный управляемый ток
C          H0     -   нАЧАЛЬНОЕ значение сигнала
C          T0     -   время включения сигнала
C          NT     -   KOЛИЧECTBO  TOЧEK ПEPEДATOЧHOЙ ФУHKЦИИ
C                     ЗAДAHHOЙ TAБЛИЧHO;
C          X1 Y1  -   COOTBETCTBEHHO момент времени и значение
C          И T.Д.     желаемой кривой выходного тока .
C          ------------------------------------------------------------
C----------------------------------------------------------------------
C
C   METOД:         PAЗMETKA ПOЛEЙ RMSW ПOД MOДEЛЬ
C   -----
C
C   ПAPAMETPЫ:     BA0T, KNOTT, PAR0T - "OTPEЗKИ" ИHФOPMAЦИOHHЫX
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 И GETMSW );
C                  RMSW - "ПEPEMEHHAЯ" ЧACTЬ MSW ( "OTPEЗOK" MSWALL );
C                  LM - ДЛИHA "ПEPEMEHHOЙ" ЧACTИ RMSW ПPИ FGMSW = 0;
C                  LM - ДЛИHA BCEГO MSW OПИCAHИЯ      ПPИ FGMSW = 1;
C
C   ПOДПPOГPAMMЫ:  1.COMMON PROF1
C   ------------
C
C   ПPИMEЧAHИE:    1. KOHTPOЛЬ B MИHИMAЛЬHOM OБ"EME;
C   -----------    2. BCE HE OПPEДEЛЯEMЫE ПOЛЯ RMSW ЗAHУЛЯЮTCЯ;
C                  3. HИЖE ПPИBOДЯTCЯ ФOPMATЫ RMSW:
C
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
C    11      I     2        I  DM  I  NMIN - HOMEP БЛИЖAЙШEЙ MEHЬШEЙ
C            I              I      I         TOЧKИ ДЛЯ T=TN+
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
CC   17+2NT  I     8+2NT    I  FM  I    Максимальный ток
C    18+2NT  I     9 + 2NT  I  FM  I    Ошибка регулирования  DELTA
C    19+2NT  I     10 + 2NT I  FM  I    Сигнал управления    SIGOLD
C    20+2NT  I     11 + 2NT I  FM  I    Начальное значение сигнала
C    21+2NT  I     12 + 2NT I  FM  I    Время включения
C----------------------------------------------------------------------
C  OБЩAЯ ДЛИHA MSW --> LM<BCЯ> = 19 + 2 * NT
C
C  ДЛИHA ПEPEMEH. ЧACTИ --> LM<RMSW> =10 + 2 * NT
C----------------------------------------------------------------------
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 339
C      IF ( NTIP .GE. 1 .AND. NTIP .LE. 4 ) GO TO 1
       IF ( NTIP .EQ. 1                   ) GO TO 1
500    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ЛOГ. CБOЙ B FM0021 --> HOMEP TИПA HE HAШ ***'/
     =  ' *** HOMEP ЭЛEMEHTA ', I5, ' HOMEP TИПA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C      GO TO (100,200,300,400,500,600),NTIP
C######################################################################
100   CONTINUE
C----------------------------------------------------------------------
C---> Регулятор управления моста Ларионова ( ТИП #340)
C----------------------------------------------------------------------
C--> OПPEДEЛEHИE KOHCTAHT
       NT = IFIX ( PAR0T(8))
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  УTOЧHИTЬ LM <OБЩУЮ >
       LM = 21 + 2 * NT
       RETURN
C######################################################################
1000   CONTINUE
C-->  ПPИHЯTЬ И ЧTO MOЖHO CФOPMИPOBATЬ..
       LM = 12+ 2 * NT
       DO 102 I=1,LM
102    RMSW(I) = 0.0
       N = 1
       RMSW(2) = RN
C---> ПPOBEPИM  ПPABИЛЬHO ЛИ BBEДEHЫ ДAHHЫE?
       NT1 = (IFIX ( PAR0T(7) ) - 1 ) / 2
       IF ( NT .EQ. NT1 ) GO TO 103
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** OШИБKA B FM0021 -->HEПPABИЛЬHO ЗAДAHO KOЛИЧECTBO TO
     =ЧEK ***'/' *** HOMEP ЭЛEMEHTA ', I5, ' HOMEP TИПA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
103    CONTINUE
       NELBX = IFIX ( PAR0T(1) )
       IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  ЗAПOMHИM #ЭЛ , #CM  И  P
       N = IFIX( PAR0T(1) )
       RMSW(3)=RN
       N = IFIX( PAR0T(2) )
       RMSW(4)=RN
       RMSW (6) = PAR0T(3)
       RMSW(8 +2*NT) = PAR0T(4)
       RMSW(11+2*NT) = PAR0T(5)
       RMSW(12+2*NT) = PAR0T(6)
C---->  ЗAПOMHИM ЧИCЛO TOЧEK выходной XAPAKTEPИCTИKИ ---------
       N = NT
       RMSW(7) = RN
       IF( NT.EQ. 0 ) GO TO 105
C---->  ЗAПOMHИM KOOPДИHATЫ TOЧEK выходной XAPAKTEPИCTИKИ ---------
          DO 104 I = 1 , NT
          RMSW(7+I)   = PAR0T( 7 + 2*I )
          RMSW(7+NT+I)= PAR0T( 8 + 2*I )
104    CONTINUE
105    CONTINUE
C----> ПPOBEPИM,ПPABЛЬHO ЛИ BBEДEHЫ ДAHHЫE ?
       DO 106 I=2,NT
       IF ( RMSW(6+I) .LE. RMSW(7+I) ) GO TO 106
C----------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** OШИБKA B FM0021-->OPДИHATЫ ЗAДAHЫ HE B BOЗPACAЮЩEM
     =ПOPЯДKE ***'/' *** HOMEP ЭЛEMEHTA',I5,' HOMEP TИПA',I5,' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
106    CONTINUE
       RETURN
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) NEL, TIP,NELBX
555    FORMAT(/' *** OШИБKA B FM0021 -->HEПPABИЛЬHO ЗAДAHЫ BXOДHЫE ДAHHЫ
     =E   ***'/' *** HA BXOД ЭЛEMEHTA ', I5, ' HOMEP TИПA ', I5,' ***',
     =         ' *** ПOДAETCЯ TOK ЭЛEMEHTA, ИMEЮЩEГO БOЛЬШИЙ ПOPЯДKOBЫЙ
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
C######################################################################
       END
