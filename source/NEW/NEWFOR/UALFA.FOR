           
         SUBROUTINE  UALFA(TOK1,WT,EDM,ZC,X,FIC,R,E,ALFA1,ALFA2)
CXXX----KOMПЛEKC "MS1" -->
C   ПOДПPOГPAMMA:    UALFA(TOK1,WT,EDM,ZC,X,FIC,R,E,ALFA1,ALFA2)
C   ------------
C   HAЗHAЧEHИE:    Нахождение корень уравнения U(ALFA) - TOK1 = 0
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
         FF   = (EXP(-PI/(3*WT))*(-EDM/ZC*COS(ALFA - PI/6-FIC)+E/R) +
     +   EDM/ZC*COS(ALFA+PI/6-FIC) - E/R)/(1- EXP(-PI/(3*WT))*X) 
     -  -  TOK1       
         IF (ABS(FF) .LT. EPS)  RETURN
C            write(*,*) alfa,ff
C            pause
         IF ( FF.GT.0 )    ALFA1 = ALFA
         IF ( FF.LT.0 )    ALFA2 = ALFA
 10      CONTINUE
         WRITE(*,*) 'Решение уравнения управления ualfa не сходится '
         WRITE(*,*) 'ALFA2 =',ALFA2,'ALFA1 = ',ALFA1,'FF',FF,TOK1
         PAUSE
         RETURN
         END
