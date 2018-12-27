         SUBROUTINE MSGOUT( MSG )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.0  (OT ДEKAБPЯ 1986 Г.)------------
C     ПOДПPOГPAMMA : MSGOUT ( MSG )
C     ------------
C     HAЗHAЧEHИE :  BЫBOД PACШИPEHHOГO TEKCTOBOГO COOБЩEHИЯ HA NWTR
C    ------------
C     METOД:        BЫБOP HOMEPOB ИЗ BXOДHOГO MACCИBA MSG И ПEЧATЬ.
C     -----
C     ПAPAMETPЫ:   MACCИB MSG => MSG(1) - KOЛИЧECTBO ЗAHЯTЫX ПOЗИЦИЙ
C     ---------                  (HOMEPOB COOБЩEHИЙ, KOTOPЫX HEOБXOДИMO
C                                 BЫBECTИ) B MSG(2) ... MSG( MSG(1) );
C                                MSG(J) => HOMEP COOБЩEHИЯ;
C    ПOДПPOГPAMMЫ:  COMMON / PROF1 /
C    ------------
C    ПPИMEЧAHИE: 1. ЖEЛATEЛЬHO OБECПEЧИTЬ ДЛИHУ CTPOKИ COOБЩEHИЯ HE
C    ----------     БOЛEE 80 CИMBOЛOB;
C                2. B TEKУЩEЙ BEPCИИ  ПPИДУCMOTPEHHO HE БOЛEE MMSG
C                   COOБЩEHИЙ [ 100 ];
C                3. PEAЛИЗOBAHO MAXMSG COOБЩEHИЙ  [ 33 ];
C                4. ДЛЯ ДO ЗAПИCИ HOMEPA COOБЩEHИЯ B MSG(*):
C                   I = MSG(1); MSG(I+2) = HOMEP; MSG(1) = I + 1;
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSG(1)
         COMMON /PROF1/ MASPRT(200)
        DATA MAXMSG/35/, MMSG/100/
C             *******************
         NWTR = MASPRT(13)
         NWTRE = MASPRT(14)
         MAX = MSG(1)
         IF ( MAX .EQ. 0 ) RETURN
         IF (MAX .LT. 0  .OR.  MAX .GT. 100 ) GO TO 667
         IMAX = MAX + 1
         DO 666 I=2,IMAX
         IF ( MSG(I) .GT. MAXMSG ) WRITE(NWTR,668) MSG(I)
C----------------------------------------------------------------------
         IF ( MSG(I) .EQ. 1 ) WRITE(NWTR,1)
         IF ( MSG(I) .EQ. 2 ) WRITE(NWTR, 2)
         IF ( MSG(I) .EQ. 3 ) WRITE(NWTR, 3)
         IF ( MSG(I) .EQ. 4 ) WRITE(NWTR, 4)
         IF ( MSG(I) .EQ. 5 ) WRITE(NWTR, 5)
         IF ( MSG(I) .EQ. 6 ) WRITE(NWTR, 6)
         IF ( MSG(I) .EQ. 7 ) WRITE(NWTR, 7)
         IF ( MSG(I) .EQ. 8 ) WRITE(NWTR, 8)
         IF ( MSG(I) .EQ. 9 ) WRITE(NWTR, 9)
         IF ( MSG(I) .EQ.10 ) WRITE(NWTR, 10)
         IF ( MSG(I) .EQ.11 ) WRITE(NWTR, 11)
         IF ( MSG(I) .EQ.12 ) WRITE(NWTR, 12)
         IF ( MSG(I) .EQ.13 ) WRITE(NWTR, 13)
         IF ( MSG(I) .EQ.14 ) WRITE(NWTR, 14)
         IF ( MSG(I) .EQ.15 ) WRITE(NWTR, 15)
         IF ( MSG(I) .EQ.16 ) WRITE(NWTR, 16)
         IF ( MSG(I) .EQ.17 ) WRITE(NWTR, 17)
         IF ( MSG(I) .EQ.18 ) WRITE(NWTR, 18)
         IF ( MSG(I) .EQ.19 ) WRITE(NWTR, 19)
         IF ( MSG(I) .EQ.20 ) WRITE(NWTR, 20)
         IF ( MSG(I) .EQ.21 ) WRITE(NWTR, 21)
         IF ( MSG(I) .EQ.22 ) WRITE(NWTR, 22)
         IF ( MSG(I) .EQ.23 ) WRITE(NWTR, 23)
         IF ( MSG(I) .EQ.24 ) WRITE(NWTR, 24)
         IF ( MSG(I) .EQ.25 ) WRITE(NWTR, 25)
         IF ( MSG(I) .EQ.26 ) WRITE(NWTR, 26)
         IF ( MSG(I) .EQ.27 ) WRITE(NWTR, 27)
         IF ( MSG(I) .EQ.28 ) WRITE(NWTR, 28)
         IF ( MSG(I) .EQ.29 ) WRITE(NWTR, 29)
         IF ( MSG(I) .EQ.30 ) WRITE(NWTR, 30)
         IF ( MSG(I) .EQ.31 ) WRITE(NWTR, 31)
         IF ( MSG(I) .EQ.32 ) WRITE(NWTR, 32)
         IF ( MSG(I) .EQ.33 ) WRITE(NWTR, 33)
         IF ( MSG(I) .EQ.34 ) WRITE(NWTR, 34)
         IF ( MSG(I) .EQ.35 ) WRITE(NWTR, 35)
C
C----------------------------------------------------------------------
666      CONTINUE
         MSG(1) = 0
         RETURN
667      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 500) MAX
500      FORMAT(' *** Лог. сбой при вызове MSGOUT, MSG(1)=', I12)
         CALL SUBERR(1)
         RETURN
668      FORMAT (' *** Cообщение # ', I4, ' не реализовано ***')
C######################################################################
1        FORMAT(' *** Hе опознано начало логической строки ***')
C---> 2
2        FORMAT(' *** Bводимое число слишком велико ***'/
     =          ' *** Bозможно - пропуск разделительного пробела ***')
C--->3
3        FORMAT(' *** Bводимое число содержит не допустимый символ ***')
C----->4
4        FORMAT(' *** Tип макромодели ошибочен или не реализован ***')
C-----> 5
5        FORMAT(' *** Hе определен номер макромодели ***'/
     =  ' *** описание списка "У" [ УЗЛЫ ]  принять невозможно ***')
C-----> 6
6        FORMAT(' *** Hе допустимый номер для обозначения узла ***')
C---->  7
7        FORMAT(' *** Hе определен номер макромодели ***'/
     = ' *** описание списка "П" [ ПAPAMETPЫ ] принять невозможно ***')
C----->8
8        FORMAT(' *** Данной макромодели параметры не нужны ***')
C---->9
9        FORMAT(' *** B логической строке описания указана не вся информ
     =ация ***')
C---->10
10       FORMAT(' *** Hарушена структура списка "П" - ожидается "MAS" **
     =*')
C---->11
11       FORMAT(' *** Hет "<" в списке параметров [ "MAS" ] ***')
C----->12
12     FORMAT(' *** Hарушена структура списка "П" - ожидается "FT" ***')
C------>13
13       FORMAT(' *** Bстроенная функция "FT" ?? не реализована ***')
C------>14
14    FORMAT(' *** B списке "П" описания "FT" нет ограничителя ">" ***')
C------>15
15       FORMAT(' *** B текущем списке много параметров ***')
C---->16
16       FORMAT(' *** Cписок FT # "<......>" - мал ***')
C---->17
17       FORMAT(' *** Hет "<" в списке параметров [ "FT" ] ***')
C---->18
18       FORMAT(' *** Hарушена структура списка "П" - ожижается "PSUB" *
     =**')
C---->19
19       FORMAT(' *** Hет "<" в списке параметров "PSUB" ***')
C---->20
20       FORMAT(' *** Hеожиданный конец логической строки - мало данных
     =в списке ***')
C---->21
21       FORMAT(' *** Hеопознано начало подсписка ***' / ' *** нет "V",
     = "U", "I", "GAMMA", "OMEGA", "MЭM", "MBH" ***')
C--->22
22       FORMAT(' *** Cписок "ЗAПPOC BЫBOДA" большой - усекается ***')
C--->23
23       FORMAT(' *** Oтмеченного узла в цепи нет ***')
C--->24
24       FORMAT(' *** Oтмеченного элемента цепь не содержит ***')
C---->25
25       FORMAT(' *** Oтмеченное "ЗHAЧEHИE" не допустимо ***')
C----> 26
26       FORMAT(' *** Oтмеченный элемент, запрошенных на вывод величин,
     = не содержит ***')
C--->27
27       FORMAT(' *** Hе определен номер звена  ***')
C--->28
28       FORMAT(' *** Hомер связи больше номера звена  ***')
C--->29
29       FORMAT(' *** Cвязь с звеном, имеющим больший номер ***')
C--->30
30       FORMAT(' *** Tип связи ошибочен  ***')
C--->31
31       FORMAT(' *** Cвязь не определена ***')
C--->32
32       FORMAT(' *** Bводим характеристики звена, не введя описания',
     *                ' связей *** ' )
C--->33
33       FORMAT(' *** Hарушен порядок следования параметров  ***')
C--->34
34       FORMAT(' *** Oтмеченный узел не сохраняется в #SPOOL ***')
C---> 35
35    FORMAT(' *** Oтмеченное "ЗНАЧЕНИЕ" не сохраняется в #SPOOL ***')
C
C
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
