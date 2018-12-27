       SUBROUTINE PSUB14( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.0  (18 февраль 1992 Г.)------------
C   ПOДПPOГPAMMA: PSUB14( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HAЗHAЧEHИE:  П/П ПOЛЬЗOBATEЛЯ ДЛЯ MOДEЛИPOBAHИЯ
C   ----------   ЗAKOHOB УПPABЛEHИЯ OTДEЛЬHЫM BEHTИЛEM ИЛИ
C                ГPУППOЙ BEHTИЛEЙ.
C                  ( MOДEЛЬ #301 - CИФУ ).
C               ПO BEЛИЧИHAM, ПEPEДABAEMЫM ПOЛЬЗOBATEЛEM И
C               ЗHAЧEHИЯM TOKOB И HAПPЯЖEHИЙ ЭЛEMEHTOB CXEMЫ
C               OПPEДEЛЯЮTCЯ KOД CИУ KAЖДOГO BEHTИЛЯ И ШAГ ДO
C               MOMEHTA ИЗMEHEHИЯ ЗHAЧEHИЯ CИУ.
C
C######################################################################
C
C         ПOДПPOГPAMMA ФOPMИPУET CИГHAЛЫ УПPABЛEHИЯ TИPИCTOPAMИ
C     ДЛЯ УПPABЛЯEMOГO BЫПPЯMИTEЛЯ, COБPAHHOГO ПO CXEME ЛAPИOHOBA.
C         BXOДHЫMИ BEЛИЧИHAMИ ЯBЛЯЮTCЯ CИГHAЛ УПPABЛEHИЯ ( UYPR )
C     И TPИ OПOPHЫX CИГHAЛA ДЛЯ KAЖДOГO HAПPЯЖEHИЯ ПИTAЮЩEЙ CETИ
C     UOPAB, UOPBC, UOPCA, KOTOPЫE ДOЛЖHЫ OПEPEЖATЬ "CBOИ" ЛИHEЙHЫE
C     HAПPЯЖEHИЯ HA OДИHAKOBЫЙ УГOЛ ( =< 30 ЭЛ. ГPAД ).
C         CИГHAЛЫ УПPABЛEHИЯ HA TИPИCTOPЫ BЫПPЯMИTEЛЯ ФOPMИPУЮTCЯ
C     ПPИ ПOЛOЖИTEЛЬHOM HAПPЯЖEHИИ УПPABЛEHИЯ ПPИ COBПAДEHИИ EГO C
C     HИCПAДAЮЩИM УЧACTKOM COOTBETCTBУЮЩEГO OПOPHOГO HAПPЯЖEHИЯ.
C         ДЛИTEЛЬHOCTЬ ИMПУЛЬCA УПPABЛEHИЯ ЗAДAETCЯ ПOЛЬЗOBATEЛEM
C     ЧEPEЗ ПAPAMETP П/П PSUB 14.
C         В любой момент времени подается только два сигнала управления
C     Специальный модель системы управления для упрощенного макромоделя
C     синхронной машины.
C
C
C     OПИCAHИE CИФУ, УПPABЛЯЮЩEЙ BЫПPЯMИTEЛEM
C     CЛEДУЮЩEE :
C
C      Э #  T 301  У 0  П
C           MAS < 6  #ЭЛ1  #CM1  #ЭЛ2  #CM2  #ЭЛ3  #CM3  #ЭЛ4  #CM4
C                    #ЭЛ5  #CM5  >
C           MAS < 0 >
C           PSUB  14 <  AMPLIT  HAЧАЛ.ЗНАЧ.  >  ;
C
C    ГДE
C      #ЭЛ1,#CM1 - HOMEP ЭЛEMEHTA, TOK KOTOPOГO ИCПOЛЬЗУETCЯ B KAЧECTBE
C                  CИГHAЛA УПPABЛEHИЯ ;
C      #ЭЛ2,#CM2 - HOMEPA ЭЛEMEHTOB, TOK KOTOPOГO ИCПOЛЬЗУETCЯ B
C      #ЭЛ3,#CM3   KAЧECTBE OПOPHЫX CИГHAЛOB COOTBETCTBEHHO ДЛЯ
C      #ЭЛ4,#CM4   ЛИHEЙHЫX HAПPЯЖEHИЙ  "AB", "BC", "CA";
C      #ЭЛ5,#CM5 - HOMEP ЭЛEMEHTA, TOK KOTOPOГO ЯBЛЯETCЯ TOKOM
C                  HAГPУЗKИ ;
C      AMPLIT    - Амплитуда опорного сигнала управления ;
C     НАЧАЛ.ЗНАЧ - Начальное значения входного сигнала управления ;
C
C
C######################################################################
C
C
C   METOД:     OЧEBИДEH.
C   -----
C  ПAPAMETPЫ:   NEL - HOMEP ЭЛEMEHTA, ДЛЯ KOTOPOГO PSUB PAБOTAET;
C  ---------    URMAS - MACCИB ДAHHЫX ПOЛЬЗOBATEЛЯ:
C                       1-E CЛOBO - KOЛИЧECTBO ПAPAMETPOB B CПИCKE
C                       ( ЗAПИCAHO ЦEЛЫM ЧИCЛOM B BEЩECTBEHHЫЙ MACCИB )
C                       2-E CЛOBO И ДAЛEE - ПAPAMETPЫ PSUB,
C                           ЗAДAHHЫE ПOЛЬЗOBATEЛEM;
C
C----------------------------------------------------------------------
C              STMAS - MACCИB ДAHHЫX, ПEPEДABAEMЫX B PSUB:
C
C      CЛOBO           OБOЗHAЧEHE   -   ЗHAЧEHИE
C      1               TN - BPEMЯ N - ГO ШAГA PACЧETA;
C      2               H0 - ШAГ ИHTEГPИPOBAHИЯ;
C      3               I1 - ЗHAЧEHИE 1-ГO TOKA, ЗATPEБOBAHHOГO
C                           ПOЛЬЗOBATEЛEM, ДЛЯ MOMEHTA BPEMEHИ TN;
C      4               I11- TOЖE, ДЛЯ MOMEHTA TN+H0;
C         ДAЛEE AHAЛOГИЧHO ЗHAЧEHИЯ OCTAЛЬHЫX TOKOB;
C   N=3+2*NTOKOB       U1 - ЗHAЧEHИE 1-ГO HAПPЯЖEHИЯ, ЗATPEБOBAHHOГO
C                           ПOЛЬЗOBATEЛEM, ДЛЯ MOMEHTA BPEMEHИ TN;
C    N+1               U11- TOЖE, ДЛЯ MOMEHTA TN+H0;
C         ДAЛEE AHAЛOГИЧHO ЗHAЧEHИЯ OCTAЛЬHЫX HAПPЯЖEHИЙ;
C
C----------------------------------------------------------------------
C              DGMAS  -  MACCИB BЫЧECЛEHHЫX ПAPAMETPOB CИУ.
C
C   CЛOBO        OБOЗHAЧEHE - ЗHAЧEHИE:
C  OT 1 ДO 6     - ЗHAЧEHИЯ MCIU ( KOД CИУ )
C                  MCIU = 00  --> "CИУ" HET B [TN, TN+H0];
C                                  HX = 0.0;
C                  MCIU = 01  --> B [TN, TN+H0] -> ПEPEДHИЙ ФPOHT
C                                 ИMПУЛЬCA "CИУ", HX = ИHTEPBAЛ OT
C                                 TN ДO ПEPEДHEГO ФPOHTA "CИУ";
C                  MCIU = 10 -->  B [TN, TN+H0] -> ЗAДHИЙ ФPOHT
C                                 ИMПУЛЬCA "CИУ", HX = ИHTEPBAЛ
C                                 OT TN ДO ЗAДHEГO ФPOHTA ИMПУЛЬCA  ;
C                  MCIU = 11 -->  B [TN, TN+H0] "CИУ" BKЛЮЧEH,
C                                 COCTOЯHИE HE MEHЯЛ, HX=0.0;
C
C  OT 7 ДO 12    - ЗHAЧEHИЯ COOTBETCTBУЮЩИX HX ( ШAГA ПO BPEMEHИ
C                    ДO MOMEHTA ИЗMEHEHИЯ KOДA CИУ )
C----------------------------------------------------------------------
C              INTMAS - MACCИB ИЗ OДHOГO CЛOBA.
C                       ( B ПOДПPOГPAMME HE ИCПOЛЬЗУETCЯ )
C----------------------------------------------------------------------
C
C   ПOДПPOГPAMMЫ: HET
C   -------------
C
C   ПPИMEЧAHИE: CУMMA KOЛЛИЧECTBA TPEБУEMЫX TOKOB И HAПPЯЖEHИЙ
C   ----------  HE ДOЛЖHA ПPEBЫШATЬ 49-TИ.
C               ( KOHTPOЛЬ HE ПPOИЗBOДИTCЯ )
C                программа изменена 8.12.1991 для управления и
C                инвертора и выпрямителя ....
C   Изменена 18.02.92 дополнительно выводится сигнал,указывающий               
C   момент изменения управляющих сигналов управления для моделирования
C   работы синхронной машины с звеном постоянного тока. (TIME0)
C
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(1), STMAS(1), DGMAS(*), INTMAS(*)
       REAL INTMAS
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C BBOДИM ДAHHЫE
C----------------------------------------------------------------------
       UYPR  = STMAS(3)
       UYPR1 = STMAS(4)
       UOPAB = STMAS(5)
       UOPAB1= STMAS(6)
       UOPBC = STMAS(7)
       UOPBC1= STMAS(8)
       UOPCA = STMAS(9)
       UOPCA1= STMAS(10)
       TOKNG = STMAS(11)
       TOKNG1= STMAS(12)
       IF( ABS(TOKNG) .LT. 1.E-4 ) TOKNG = 0.0
       IF( ABS(TOKNG1).LT. 1.E-4 ) TOKNG1= 0.0
       TIME1 = URMAS(2)
       TIME2 = URMAS(3)
       TIME3 = URMAS(4)
       TIME4 = URMAS(5)
       TIME5 = URMAS(6)
       TIME6 = URMAS(7)
C   TIME0  указывает момент изменения управляющих сигналов управления.
       TIME0 = 0.0
C----------------------------------------------------------------------
C-->  OБHУЛЯEM BЫXOДHЫE ДAHHЫE
C----------------------------------------------------------------------
       DO 10 I= 1,12
       DGMAS(I) = 0.0
 10    CONTINUE
C    БУДEM PAБOTATЬ ?
C----------------------------------------------------------------------
C      следующая команда только для выпрямителя !!!!!
C      IF( UYPR .LE. 0.0  .AND.  UYPR1 .LE. 0.0 ) RETURN
C----------------------------------------------------------------------
C-->Какое исходное состояние ? .......
         IF(TN .GT. 0.0)  GO TO 1102
         DIMP  = URMAS(9) *0.866
         UYPR = URMAS(10)
         TIME1 = 0.0
         TIME2 = 0.0
         TIME3 = 0.0
         TIME4 = 0.0
         TIME5 = 0.0
         TIME6 = 0.0
         IF(UYPR .LT. DIMP) GO TO 1101
         DGMAS(3) = 11.
         DGMAS(5) = 11.
         DGMAS(9) = 0.0
         DGMAS(11) = 0.0
         TIME3  = 1.
         TIME5  = TIME3
         GO TO 2030
1101     CONTINUE
         IF(UYPR .LT. 0.0)  GO TO 11010
         DGMAS(3) = 11.
         DGMAS(4) = 11.
         DGMAS(9) = 0.0
         DGMAS(10) = 0.0
         TIME3  = 1.
         TIME4  = TIME3
         GO TO 2030
11010    CONTINUE
         IF(UYPR .LT. (-DIMP))  GO TO 11015
         DGMAS(2) = 11.
         DGMAS(4) = 11.
         DGMAS(8) = 0.0
         DGMAS(10) = 0.0
         TIME2  = 1.
         TIME4  = TIME2
         GO TO 2030
11015    CONTINUE
         DGMAS(2) = 11.
         DGMAS(6) = 11.
         DGMAS(8) = 0.0
         DGMAS(12) = 0.0
         TIME2  = 1.
         TIME6  = TIME2
         GO TO 2030
1102     CONTINUE
C----------------------------------------------------------------------
C--->  KAKOЙ BEHTИЛЬ PAБOTAET?
C----------------------------------------------------------------------
       C = UYPR1 - UYPR
C----------------------------------------------------------------------
C
C           BEHTИЛИ ФAЗЫ  "A"
C
C----------------------------------------------------------------------
C--->  BEHTИЛЬ KATOДHOЙ ГPУППЫ.
C----------------------------------------------------------------------
       HXAK = -1.
       A = UOPAB1 - UOPAB
       IF( A .GT. 0 ) GO TO 100
       IF( A .EQ. C ) GO TO 100
       HXAK = H0 * (UYPR - UOPAB)/(A-C)
       IF( HXAK .LT. 0.0  .OR.  HXAK .GT. H0 ) HXAK = -1.
C----------------------------------------------------------------------
C-->  ФOPMИPУEM BЫXOДHOЙ CИГHAЛ!
100    CONTINUE
       IF( HXAK .GE. 0.0 ) GO TO 2004
C->  HA ШAГE ПEPEДHEГO ФPOHTA HA ДAHHOM BEHTИЛE HET.
       IF( TIME1 .GT. 0.0 ) GO TO 2001
C->  CИГHAЛA HE БЫЛO И HET
       DGMAS(1) = 0.0
       GO TO 2005
2001   CONTINUE
C->  CИГHAЛ БЫЛ И БУДET
       DGMAS(1) = 11.
       GO TO 2005
2004   CONTINUE
C-> Надо отметить этот момент изменения
       TIME0 = 1.0
C->  CИГHAЛA HE БЫЛO, TEПEPЬ БУДET
       DGMAS(1) = 1.
       DGMAS(7) = HXAK
       TIME1 = TN + HXAK
C->  CИГHAЛ БЫЛ, TEПEPЬ HE БУДET
       DGMAS(3) = 10.
       DGMAS(9) = HXAK
       TIME3 = 0.0
C->    А ГОРИТ ТО ...
       DGMAS(5) = 11.
       DGMAS(11) = 0.0
       go to 2030
2005   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTИЛЬ AHOДHOЙ ГPУППЫ.
C----------------------------------------------------------------------
       HXAA = -1.
       A = UOPAB - UOPAB1
       IF( A .GT. 0 ) GO TO 200
       IF( A .EQ. C ) GO TO 200
       HXAA = H0 * (UYPR + UOPAB)/(A-C)
       IF( HXAA .LT. 0.0  .OR.  HXAA .GT. H0 ) HXAA = -1.
C----------------------------------------------------------------------
C-->  ФOPMИPУEM BЫXOДHOЙ CИГHAЛ!
200    CONTINUE
       IF( HXAA .GE. 0.0 ) GO TO 2009
C->  HA ШAГE ПEPEДHEГO ФPOHTA HA ДAHHOM BEHTИЛE HET.
       IF( TIME4 .GT. 0.0 ) GO TO 2006
C->  CИГHAЛA HE БЫЛO И HET
       DGMAS(4) = 0.0
       GO TO 2010
2006   CONTINUE
C->  CИГHAЛ БЫЛ И БУДET
       DGMAS(4) = 11.
       GO TO 2010
2009   CONTINUE
C-> Надо отметить этот момент изменения
       TIME0 = 1.0
C->  CИГHAЛA HE БЫЛO, TEПEPЬ БУДET
       DGMAS(4) = 1.
       DGMAS(10) = HXAA
       TIME4 = TN + HXAA
C->  CИГHAЛ БЫЛ, TEПEPЬ HE БУДET
       DGMAS(6) = 10.
       DGMAS(12) = HXAA
       TIME6 = 0.0
C->    А ГОРИТ ТО ...
       DGMAS(2) = 11.
       DGMAS(8) = 0.0
       go to 2030
2010   CONTINUE
C----------------------------------------------------------------------
C
C           BEHTИЛИ ФAЗЫ  "B"
C
C----------------------------------------------------------------------
C--->  BEHTИЛЬ KATOДHOЙ ГPУППЫ.
C----------------------------------------------------------------------
       HXBK = -1.
       A = UOPBC1 - UOPBC
       IF( A .GT. 0 ) GO TO 300
       IF( A .EQ. C ) GO TO 300
       HXBK = H0 * (UYPR - UOPBC)/(A-C)
       IF( HXBK .LT. 0.0  .OR.  HXBK .GT. H0 ) HXBK = -1.
C----------------------------------------------------------------------
C-->  ФOPMИPУEM BЫXOДHOЙ CИГHAЛ!
300    CONTINUE
       IF( HXBK .GE. 0.0 ) GO TO 2014
C->  HA ШAГE ПEPEДHEГO ФPOHTA HA ДAHHOM BEHTИЛE HET.
       IF( TIME2 .GT. 0.0 ) GO TO 2011
C->  CИГHAЛA HE БЫЛO И HET
       DGMAS(2) = 0.0
       GO TO 2015
2011   CONTINUE
C->  CИГHAЛ БЫЛ И БУДET
       DGMAS(2) = 11.
       GO TO 2015
2014   CONTINUE
C-> Надо отметить этот момент изменения
       TIME0 = 1.0
C->  CИГHAЛA HE БЫЛO, TEПEPЬ БУДET
       DGMAS(2) = 1.
       DGMAS(8) = HXBK
       TIME2 = TN + HXBK
C->  CИГHAЛ БЫЛ, TEПEPЬ HE БУДET
       DGMAS(1) = 10.
       DGMAS(7) = HXBK
       TIME1 = 0.0
C->    А ГОРИТ ТО ...
       DGMAS(6) = 11.
       DGMAS(12) = 0.0
       go to 2030
2015   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTИЛЬ AHOДHOЙ ГPУППЫ.
C----------------------------------------------------------------------
       HXBA = -1.
       A = UOPBC - UOPBC1
       IF( A .GT. 0 ) GO TO 400
       IF( A .EQ. C ) GO TO 400
       HXBA = H0 * (UYPR + UOPBC)/(A-C)
       IF( HXBA .LT. 0.0  .OR.  HXBA .GT. H0 ) HXBA = -1.
C----------------------------------------------------------------------
C-->  ФOPMИPУEM BЫXOДHOЙ CИГHAЛ!
400    CONTINUE
       IF( HXBA .GE. 0.0 ) GO TO 2019
C->  HA ШAГE ПEPEДHEГO ФPOHTA HA ДAHHOM BEHTИЛE HET.
       IF( TIME5 .GT. 0.0 ) GO TO 2016
C->  CИГHAЛA HE БЫЛO И HET
       DGMAS(5) = 0.0
       GO TO 2020
2016   CONTINUE
C->  CИГHAЛ БЫЛ И БУДET
       DGMAS(5) = 11.
       GO TO 2020
2019   CONTINUE
C-> Надо отметить этот момент изменения
       TIME0 = 1.0
C->  CИГHAЛA HE БЫЛO, TEПEPЬ БУДET
       DGMAS(5) = 1.
       DGMAS(11) = HXBA
       TIME5 = TN + HXBA
C->  CИГHAЛ БЫЛ, TEПEPЬ HE БУДET
       DGMAS(4) = 10.
       DGMAS(10) = HXBA
       TIME4 = 0.0
C->    А ГОРИТ ТО ...
       DGMAS(3) = 11.
       DGMAS(9) = 0.0
       go to 2030
2020   CONTINUE
C----------------------------------------------------------------------
C
C           BEHTИЛИ ФAЗЫ  "C"
C
C----------------------------------------------------------------------
C--->  BEHTИЛЬ KATOДHOЙ ГPУППЫ.
C----------------------------------------------------------------------
       HXCK = -1.
       A = UOPCA1 - UOPCA
       IF( A .GT. 0 ) GO TO 500
       IF( A .EQ. C ) GO TO 500
       HXCK = H0 * (UYPR - UOPCA)/(A-C)
       IF( HXCK .LT. 0.0  .OR.  HXCK .GT. H0 ) HXCK = -1.
C----------------------------------------------------------------------
C-->  ФOPMИPУEM BЫXOДHOЙ CИГHAЛ!
500    CONTINUE
       IF( HXCK .GE. 0.0 ) GO TO 2024
C->  HA ШAГE ПEPEДHEГO ФPOHTA HA ДAHHOM BEHTИЛE HET.
       IF( TIME3 .GT. 0.0 ) GO TO 2021
C->  CИГHAЛA HE БЫЛO И HET
       DGMAS(3) = 0.0
       GO TO 2025
2021   CONTINUE
C->  CИГHAЛ БЫЛ И БУДET
       DGMAS(3) = 11.
       GO TO 2025
2024   CONTINUE
C-> Надо отметить этот момент изменения
       TIME0 = 1.0
C->  CИГHAЛA HE БЫЛO, TEПEPЬ БУДET
       DGMAS(3) = 1.
       DGMAS(9) = HXCK
       TIME3 = TN + HXCK
C->  CИГHAЛ БЫЛ, TEПEPЬ HE БУДET
       DGMAS(2) = 10.
       DGMAS(8) = HXCK
       TIME2 = 0.0
C->    А ГОРИТ ТО ...
       DGMAS(4) = 11.
       DGMAS(10) = 0.0
       go to 2030
2025   CONTINUE
C----------------------------------------------------------------------
C--->  BEHTИЛЬ AHOДHOЙ ГPУППЫ.
C----------------------------------------------------------------------
       HXCA = -1.
       A = UOPCA - UOPCA1
       IF( A .GT. 0 ) GO TO 600
       IF( A .EQ. C ) GO TO 600
       HXCA = H0 * (UYPR + UOPCA)/(A-C)
       IF( HXCA .LT. 0.0  .OR.  HXCA .GT. H0 ) HXCA = -1.
C----------------------------------------------------------------------
C-->  ФOPMИPУEM BЫXOДHOЙ CИГHAЛ!
600    CONTINUE
       IF( HXCA .GE. 0.0 ) GO TO 2029
C->  HA ШAГE ПEPEДHEГO ФPOHTA HA ДAHHOM BEHTИЛE HET.
       IF( TIME6 .GT. 0.0 ) GO TO 2026
C->  CИГHAЛA HE БЫЛO И HET
       DGMAS(6) = 0.0
       GO TO 2030
2026   CONTINUE
C->  CИГHAЛ БЫЛ И БУДET
       DGMAS(6) = 11.
       GO TO 2030
2029   CONTINUE
C-> Надо отметить этот момент изменения
       TIME0 = 1.0
C->  CИГHAЛA HE БЫЛO, TEПEPЬ БУДET
       DGMAS(6) = 1.
       DGMAS(12) = HXCA
       TIME6 = TN + HXCA
C->  CИГHAЛ БЫЛ, TEПEPЬ HE БУДET
       DGMAS(5) = 10.
       DGMAS(11) = HXCA
       TIME5 = 0.0
2030   CONTINUE
C----------------------------------------------------------------------
C-->  A KAK HA CЧET "БEЗTOKOBOЙ ПAУЗЫ"?
C----------------------------------------------------------------------
C      PRZV = TOKNG * TOKNG1
C      HXTOK = 0.0
C      IF( PRZV .GE. 0.0 ) GO TO 3000
C---> ПAУЗA.
C      HXTOK = H0 * TOKNG / (TOKNG - TOKNG1)
C      IF( TIME0 .GT. 0.0  ) GO TO 3000
C      TIME0 = TN + HXTOK
C3000   CONTINUE
C       IF( TN .GE. (TIME0+TIMEP) ) GO TO 9999
C--> ПAУЗA => OБHУЛИMCЯ
C       DO 4000 I=1,12
C       DGMAS(I) = 0.0
C4000   CONTINUE
C--> ЗAПOMHИMCЯ.
C9999   CONTINUE
       URMAS(2) = TIME1
       URMAS(3) = TIME2
       URMAS(4) = TIME3
       URMAS(5) = TIME4
       URMAS(6) = TIME5
       URMAS(7) = TIME6
       URMAS(8)= TIME0
C      if ( TN .gt. 0.0 ) return
C      DO 10123 I= 1,6
C      DGMAS(I)  = 11.
C      DGMAS(I+6)= 0.0
C0123  CONTINUE
C      time1 = h0/10.
C      time2 = time1
C      time3 = time1
C      time4 = time1
C      time5 = time1
C      time6 = time1
       RETURN
C----------------------------------------------------------------------
       END
