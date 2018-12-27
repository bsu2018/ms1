          SUBROUTINE DRSPOL ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX----KOMПЛEKC "MS1" -->
C   ПOДПPOГPAMMA:   DRSPOL ( KAR, DAT, MSWALL , TOKN , VALLN )
C   ------------
C   HAЗHAЧEHИE:    Обработчик директивы #SPOOL
C   -----------     Формирует списки в COMMON/SERVZ8/ SPISNN, ...
C                    указывающие какие значения записывать в спулинг
C                    Возбуждает флаг FTRSPL==MASPRT(121)
C   METOД:         BBOД ДAHHЫX ( CM. INPUT1 )
C   -----
C*  ПAPAMETPЫ :    KAR,DAT - OПИCAHИE CTPOKИ ДИPEKTИBЫ;                *
C*  ---------       MSWALL - CПИCOK УПPABЛЯЮЩИX CЛOB ЭЛEMEHTOB;        *
C*                    TOKN - "TOKИ" ЭЛEMEHTOB;                         *
C*                   VALLN - "HAПPЯЖEHИЯ" ЭЛEMEHTOB.                   *
C
C
C
C   ПOДПPOГPAMMЫ:  1. COMMON PROF1, SERVZ1, 2, 3, 8
C   -------------  2. GETSPO
C   ПPИMEЧAHИE:    1. B CПИCKE HE БOЛEE 15-TИ ЗHAЧEHИЙ;
C   ----------     2. ФOPMAT SPISVL и  SPISTK
C
C                  ФOPMAT  SPISxx:
C                  -------------
C
C   +------------+----------+----------+    *   для VL - номер узла
C   I KOЛИЧECTBO I значение I значение I        для TK - адрес тока
C   I эначений   I    *     I    *     I ........        в масивах TOKN
C   +------------+----------+----------+
C   I 1 CЛOBO    I 1 CЛOBО  I 1- CЛOBО I .......
C   +------------+----------+----------+
C
C     P A З M E P H O C T Ь    SPIS  ==>  1 + 15 = 16  CЛOB;
C
C
CZZZ-------------------------------------------------------------------
      COMMON / PROF1 / MASPRT(200)
      COMMON/SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
      INTEGER        SPISNN, SPISVL,     SPISTK
      EQUIVALENCE ( NNTOK , MASPRT(46) )
      EQUIVALENCE ( FMSWAL , MASPRT(74) )
      EQUIVALENCE ( NMKNOT , MASPRT(7) )
      EQUIVALENCE ( MASPRT(15),  DIAL )
      EQUIVALENCE ( MASPRT(14),  NWTRE )
      EQUIVALENCE ( MASPRT(13),  NWTR )
      EQUIVALENCE ( MASPRT(121), FTRSPL )
      INTEGER FMSWAL,FTRSPL
C-->
      EQUIVALENCE ( NWTR , MASPRT(13) )
      EQUIVALENCE ( NRDR , MASPRT(12) )
      DIMENSION KAR(1),DAT(1),MSWALL(1),TOKN(1),VALLN(1)
      DIMENSION VT(16), UT(31), IT(31), SPIS(91)
      INTEGER VT, UT, SUM, PRT, SPIS, TABSPC, TIP, DIAL
C-->
      IF( FMSWAL.EQ.0 ) WRITE (NWTR,201)
201   FORMAT(/' *** HAPУШEH ПOPЯДOK CЛEДOBAHИЯ ДИPEKTИB.',
     *' ДИPEKTИBA ИГHOPИPУETCЯ ***'/)
      IF( FMSWAL.EQ.0 ) RETURN
         WRITE(NWTR, 202)
202   FORMAT(/' *** #SPOOL ПPИHЯTA:==>    количество сохраняемых ***'/
     *        ' *** эначений сокращается по запросу пользователя ***')
C
         FTRSPL = 0
C
         CALL GETSPO( MSWALL, SPIS,IRC )
         IF (IRC .EQ. 3 ) RETURN
         IF (IRC .EQ. 2 ) RETURN
C+++++++++++++++++++++++++++++++
         DO 1 i=1,SPISNN
             SPISVL(i) = 0
1            SPISTK(i) = 0
C**
         jv = 2
         jt = 2
         DO 2 ii=1, SPIS(1)
           i = (ii-1)*6+2
*  Узлы ******
           IF ( SPIS(i+0) .EQ. 1 ) THEN
               IF ( SPISVL(1) .EQ. 0 ) THEN
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               ELSE
                   DO 3 js = 2, SPISVL(jv-1)
                      IF ( SPISVL(js) .EQ. SPIS(i+1) ) GO TO 2
3                  CONTINUE
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               END IF
               GO TO 2
           ENDIF
*  разность узлов
           IF ( SPIS(i+0) .EQ. 2 ) THEN
*              первый узел
               IF ( SPISVL(1) .EQ. 0 ) THEN
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               ELSE
                   DO 31 js = 2, SPISVL(jv-1)
                      IF ( SPISVL(js) .EQ. SPIS(i+1) ) GO TO 2
31                 CONTINUE
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+1)
                   jv = jv + 1
               END IF
*              второй узел
               IF ( SPISVL(1) .EQ. 0 ) THEN
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+2)
                   jv = jv + 1
               ELSE
                   DO 32 js = 2, SPISVL(jv-1)
                      IF ( SPISVL(js) .EQ. SPIS(i+2) ) GO TO 2
32                 CONTINUE
                   SPISVL(1) = SPISVL(1) + 1
                   SPISVL(jv) = SPIS(i+2)
                   jv = jv + 1
               END IF
               GO TO 2
           ENDIF
*  токи ******
           IF ( SPIS(i+0) .EQ. 3 ) THEN
               IF ( SPISTK(1) .EQ. 0 ) THEN
                   SPISTK(1) = SPISTK(1) + 1
                   SPISTK(jt) = SPIS(i+1)
                   jt = jt + 1
               ELSE
                   DO 33 js = 2, SPISTK(jt-1)
                      IF ( SPISTK(js) .EQ. SPIS(i+1) ) GO TO 2
33                 CONTINUE
                   SPISTK(1) = SPISTK(1) + 1
                   SPISTK(jt) = SPIS(i+1)
                   jt = jt + 1
               END IF
               GO TO 2
           ENDIF
C********************
           WRITE(NWTRE,*) ' Нарушение структуры SPIS: программа DRSPOL'
C********************
2        CONTINUE
C*********************
         IF ( (SPISVL(1) + SPISTK(1)) .NE. 0 ) THEN
C
              FTRSPL = 1
C
              RETURN
         ELSE
              WRITE(NWTR,*) ' #SPOOL - ИГНОРИРУЕТСЯ !!!'
              RETURN
         ENDIF
         END
