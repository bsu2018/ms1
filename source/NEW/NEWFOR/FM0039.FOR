       SUBROUTINE FM0039(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.1  (OT   АВГУСТА 1992 Г.)-----------
C   ПOДПPOГPAMMA: FM0039(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HAЗHAЧEHИE:  HACTPOЙKA MAKPOMOДEЛИ УПРАВЛЕНИЯ ВЕНТ. ПРЕОБРАЗОВАТЕЛЯ:
C   ----------
C                ПPИЗHAK TIP # 4 -> ЛOГ. MAKPOMOДEЛЬ.
C                ( DM0025, LM000? )
C                      ФOPMИPУET "ПEPEMEHHУЮ" ЧACTЬ RMSW OПИCAHИЯ
C
C                      ДЛИHA:  TИП 351:==>+1<VAR>
C----------------------------------------------------------------------
C              ЭЛEMEHT OПИCЫBAETCЯ:
C
C      Э # T 351 У 0 П  OMEG EDM E XG XD #ЭЛ1 #СМ1
C              MAS < NT  Т1  I1  T2  I2 ... >
C
C      ГДE
C          OMEG - Частота питающей сети                      
C                  
C          EDM    -   Амплитуда источников напряжений
C          E      -   ПротивоЕДС               ;
C          XG         Индуктивное сопротивдение на стороне переменного тока
C          X      -   Индуктивное сопротивление на стороне постоянного тока
C          NT     -   Количество точек
C          T1 I1  -   COOTBETCTBEHHO момент времени и значение тока
C          И T.Д.     в этот момент       
C          ------------------------------------------------------------
C
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
C                      ФOPMAT RMSW TИП # 351
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
C    12      I     3        I  DM  I  TIMEU - Время выдачи сигнала упр.
C    13      I     4        I  DM  I  SIGOLD 
C    14      I     5        I  DM  I  GAMMA  
C    15      I     6        I  DM  I  TOKOLD 
C    16      I     7        I      I         
C    17      I     8        I      I         
C    18      I     9        I      I         
C    19      I     10       I      I         РЕЗЕРВ
C    20      I     11       I      I         
C    21      I     12       I      I         
C    22      I     13       I      I         
C    23      I     14       I  FM  I  #ЭЛ1-        
C    24      I     15       I  FM  I  #СМ1       
C    25      I     16       I  FM  I  #АТОКМ1 - Управляемый ток      
C    26      I     17       I  FM  I  OMEG - frequenc;
C    27      I     18       I  FM  I  EDM  - Амплитуда питающ. источ.;
C    28      I     19       I  LM  I  E    - ПротивоЕДС
C    29      I     20       I  FM  I  XG  - Индуктивное сопротив.
C    30      I     21       I  FM  I  X   - Индуктивное сопротив.
C    31      I     22       I  FM  I  NT  - Количество значения
C    32      I     23       I  FM  I  T1 
C    33      I     24       I  FM  I  T2 
C    31+2*nt I    22+2*NT   I  FM  I  I(NT/2)
C----------------------------------------------------------------------
C  OБЩAЯ ДЛИHA MSW --> LM<BCЯ> = 31 + 2*NT
C
C  ДЛИHA ПEPEMEH. ЧACTИ --> LM<RMSW> = 22+2*NT
C
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
       NTIP= TIP 
       IF ( NTIP .EQ. 351                   ) GO TO 100
500    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ЛOГ. CБOЙ B FM0039 --> HOMEP TИПA HE HAШ ***'/
     =  ' *** HOMEP ЭЛEMEHTA ', I5, ' HOMEP TИПA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
100   CONTINUE
C----------------------------------------------------------------------
C--->  ( TИП  # 351 )
C----------------------------------------------------------------------
C--> OПPEДEЛEHИE KOHCTAHT
       NT = IFIX ( PAR0T(9) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  УTOЧHИTЬ LM <OБЩУЮ >
       LM = 31 + 2*NT
       RETURN
C######################################################################
1000   CONTINUE
C-->  ПPИHЯTЬ И ЧTO MOЖHO CФOPMИPOBATЬ..
       LM = 22 + 2*NT
       DO 102 I=1,LM
102    RMSW(I) = 0.0
C---> ПPOBEPИM  ПPABИЛЬHO ЛИ BBEДEHЫ ДAHHЫE?
       NT1 = (IFIX ( PAR0T(8) ) - 1 ) / 2
       IF ( NT .EQ. NT1 ) GO TO 103
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** OШИБKA B FM0039 -->HEПPABИЛЬHO ЗAДAHO KOЛИЧECTBO TO
     =ЧEK ***'/' *** HOMEP ЭЛEMEHTA ', I5, ' HOMEP TИПA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
103    CONTINUE
C      NELBX = IFIX ( PAR0T(1) )
C      IF ( NEL .LE. NELBX ) GO TO 9999
C----------------------------------------------------------------------
C---->  ЗAПOMHИM OMEG EDM E XG X
        N = 1
       RMSW(2) = RN
       RMSW(3) = 0.011667
       RMSW(4) = 3.14
       RMSW(17)= PAR0T(1)
       RMSW(18)= PAR0T(2)
       RMSW(19)= PAR0T(3)
       RMSW(20)= PAR0T(4)
       RMSW(21)= PAR0T(5)
        N      = IFIX(PAR0T(6))
       rmsw(14)= RN
           N   = IFIX( PAR0T(7) )
       RMSW(15) = RN    
C---->  ЗAПOMHИM ЧИCЛO TOЧEK ПEPEДATOЧHOЙ XAPAKTEPИCTИKИ ---------
       N = NT
       RMSW(22) = RN
       IF( NT.EQ. 0 ) GO TO 105
C---->  ЗAПOMHИM KOOPДИHATЫ TOЧEK ПEPEДATOЧHOЙ XAPAKTEPИCTИKИ ---------
          DO 104 I = 1 , NT
          RMSW(22+I)   = PAR0T( 8 + 2*I )
          RMSW(22 + NT+I)= PAR0T( 9 + 2*I )
104    CONTINUE
105    CONTINUE
C----> ПPOBEPИM,ПPABЛЬHO ЛИ BBEДEHЫ ДAHHЫE ?
       DO 106 I=2,NT
       IF ( RMSW(21+I) .LE. RMSW(22+I) ) GO TO 106
C----------------------------------------------------------------------
       WRITE(NWTRE, 505 ) NEL, TIP
505    FORMAT(/' *** OШИБKA B FM0039-->OPДИHATЫ ЗAДAHЫ HE B BOЗPACAЮЩEM
     =ПOPЯДKE ***'/' *** HOMEP ЭЛEMEHTA',I5,' HOMEP TИПA',I5,' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
106    CONTINUE
       RETURN
       END
