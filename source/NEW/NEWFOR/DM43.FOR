         SUBROUTINE DM0043 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.1  (OT АВГУСТА   1992 Г.)------------
C   ПOДПPOГPAMMA:  DM0043 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C                ПPИЗHAK TIP # 4 -> ЛOГ. MAKPOMOДEЛЬ.
C                ( DM0043, LM000? )
C                      ФOPMИPУET "ПEPEMEHHУЮ" ЧACTЬ RMSW OПИCAHИЯ
C
C                      ДЛИHA:  TИП 351:==>+1<VAR>
C----------------------------------------------------------------------
C              ЭЛEMEHT OПИCЫBAETCЯ:
C
C      Э # T 351 У 0 П  OMEG EDM E XG XD #эл1 #СМ1
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
C          "ПOCTOЯHHAЯ" ЧACTЬ MSW TИПOB # 320,321,322,323,325:
C           --------------------------------------------------
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
C    13      I     4        I  DM  I  ALFOLD 
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
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR
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
         DATA PI / 3.1415292653 /         
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       NTIP = TIP 
       IF ( NTIP .EQ. 351                   ) GO TO 9991
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ЛOГ. CБOЙ B DM0025 - ЗAПPOC HA OБCЛУЖИBAHИE ЧУЖOГ
     =O TИПA ***'/
     =    ' *** ЭЛEMEHT # ', I10, ' TИП MOДEЛИ # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9991   CONTINUE
C--->  BЫБOP PEЖИMA ЗAПPOCA..
6666   GOTO ( 100, 200) , ZAPROS
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
C######################################################################
       ITOK = MSW(7)
C----------------------------------------------------------------------
C-->   Регулятор для моста Ларионова ( TИП 351 )
C----------------------------------------------------------------------
C  Надо ли вычислить новый сигнал управления ?
       TIMEU = RMSW(12)
       ALFOLD = RMSW(13)       
        TOKOLD = RMSW(15)
c        write(*,*) 'tokold',tokold
       OMEG = RMSW(26)
       EDM  = RMSW(27)
       E    = RMSW(28)
       XG   = RMSW(29)
       XD   = RMSW(30)
       TIMEIN = PI /(3.*OMEG) 
       XC = XD + 2.*XG
       X  = (XD + XG)/XC
       XC1 = XD + XG*1.5 
c Найдем значение управляемого тока в момент коммутации
       ALFOUT = ALFOLD
            IF( TN .LT. 0.0116)  GOTO 2000  
       TIMEKOM = TIMEU - TIMEIN + ALFOLD/OMEG + H0
C            write(*,*) 'alfaold',alfold,'timekom',timekomC
C            write(*,*)'timeu',timeu,'timein',timein
C            pause
         IF ( ABS(TN - TIMEKOM) .LE. H0) THEN
               TOKKOM = TOKN1( MSW(25) )
C             write(*,*) 'Старый In',tokold,'in',tokkom
               TOKOLD = TOKKOM - (SIN(ALFOLD - PI/6.)*EDM -
     -           ALFOLD*E) /(X*XC)
C             write(*,*) 'Новый In',tokold
C              pause
C Запомним точное значение In                              
                  RMSW(15) = TOKOLD
         ENDIF                            
       IF ( TN .LT. TIMEU )  GOTO 2000
C Черт побери , Надо формировать новый сигнал управления              
c Находим интервал переключения
       NMIN = MSW(11)
       GAMMA = RMSW(14)
       NT   = MSW(31)        
       TIMEU  = TIMEU + TIMEIN
C Ищем I(n+1)
       IF( (NMIN +1) .GT. NT) THEN
       ALFOUT =ALFOLD
       GOTO 1800
       ENDIF
       IF( (TN +TIMEIN/2.) .GT. RMSW(32 + NMIN )) NMIN = NMIN +1
       TIME1 = RMSW(NMIN + 31)
       TIME2 = RMSW(NMIN + 32)
       TOK1 =  RMSW(31 + NT + NMIN)
       TOK2 =  RMSW(32 + NT + NMIN)       
         FF   = ( SIN(GAMMA + PI/6) -  SIN(GAMMA - PI/6) /X) * EDM/XC 
     =  + (1/X - 1)*GAMMA*E/XC - E*PI/(3.*XC) 
C Какой угол ALFA ?
C Допустим ....
       ALFA = PI/6.
       DO 1050 I = 1,10
       TOKTN = (TOK2-TOK1)*(TN+ALFA/OMEG - TIME1)/(TIME2-TIME1)+TOK1
       TOKNEW = TOKTN - (SIN(ALFA - PI/6.)*EDM -  ALFA*E) /(X*XC)
       DTOK = TOKNEW - X*TOKOLD
C       write(*,*) 'dtok',dtok,'dtoknew',toknew,'tokold',tokold
C       WRITE(*,*) 'FF',FF
       IF( FF .LE. DTOK ) THEN
       ALFA = GAMMA
       GOTO 1100
       ENDIF
       ALFA1 = 0.
       ALFA2 = 2*PI/3.
       CALL FALFA(EDM,E, XC,X,ALFA1,ALFA2,DTOK )
       ALFA = ALFA1
1100    CONTINUE
       TOKTN = (TOK2-TOK1)*(TN+ALFA/OMEG - TIME1)/(TIME2-TIME1)+TOK1
       TOKNNN = TOKTN - (SIN(ALFA - PI/6.)*EDM -  ALFA*E) /(X*XC)
       deltat = abs(toknnn - toknew)
       write(*,*) 'deltat',deltat,'toknew',toknew
       pause
       if ( deltat .LT. (0.01*toknew) ) goto 1500
1050    continue        
1500   CONTINUE
C Как на счет GAMMA
       TOKNN = X*TOKOLD + SIN(ALFA+PI/6)*EDM/XC - (PI/3.+ALFA)*E/XC
       DBETA = 0.866*SIN(ALFA)*EDM/XC1 + COS(ALFA)*EDM/XG -
     -    E*ALFA/XC1 - 2.*TOKNN 
       BETA1 = 0.
       BETA2 = PI
       CALL FBETA(EDM,E, XC1,XG,BETA1,BETA2,DBETA )
       GAMMA = BETA1 - PI/3.
       IF(GAMMA .LT.0.) GAMMA = 0.
C       write(*,*) 'GAMMA',GAMMA,'DBETA',DBETA
       ALFOUT = ALFA        
1800   CONTINUE
C сохраним 
       MSW(11) = NMIN
       RMSW(12) = TIMEU
       RMSW(13) = ALFOUT
       RMSW(14) = GAMMA
       RMSW(15) = TOKNEW
2000   CONTINUE
       TOKN1(ITOK) = COS(ALFOUT)*14.99
C        WRITE(*,*) 'ALFAOLD',ALFOLD,'TOKN1',TOKN1(ITOK)
C        PAUSE
C-->  ЗAФИKCИP. KOHEЦ BЫЧИCЛEHИЙ       ( FQ ФЛAГ )
       MSW(10) = 1
       RETURN
C######################################################################
       END
