         SUBROUTINE FALFA(EDM,E, ZC,X,R,WT,FIC,ALFA1,ALFA2,DTOK )
CXXX----KOMПЛEKC "MS1" -->
C   ПOДПPOГPAMMA:   FALFA(EDM,E, ZC,X,R,WT,FIC,ALFA1,ALFA2,DTOK )
C   ------------
C   HAЗHAЧEHИE:    Нахождение корень уравнения F(ALFA) - TOK = 0
C
C   METOД:         Ньютона
C
C   ПAPAMETPЫ:     EDM  - Амплитуда ЕДС питающей сети
C   ---------      E    - ПротивоЕДС
C                  XG   - Суммарное индуктивное сопротивление
C                  X    - Индуктивное сопротивление 
C                  ALFA1 - Максимальный Уголь отпирания очередного вентиля
C                  ALFA2 - Минимальный уголь отпирания очередного вентиля
C                  TOK  - Аппроксимированное среднее значение тока
C
C
c*****************************************************
         DATA PI /3.141593/ , EPS/0.001/
         DO 10 I = 1,100
         ALFA = ( ALFA1 + ALFA2)*0.5
         FF   = EXP(ALFA/WT)*(( COS(ALFA + PI/6 -FIC) -
     -   COS(ALFA - PI/6- FIC) /X) * EDM/ZC + (1/X - 1)*E/R )-DTOK 
         IF (ABS(FF) .LT. EPS)  RETURN
C            write(*,*) alfa,ff
C            pause
         IF ( FF.GT.0 )    ALFA1 = ALFA
         IF ( FF.LT.0 )    ALFA2 = ALFA
 10      CONTINUE
         WRITE(*,*) 'Решение уравнения управления falfa не сходится '
         WRITE(*,*) 'DTOK= ',dtok
         PAUSE
         RETURN
         END
