         SUBROUTINE FBETA(EDM,E, XC1,XG,BETA1,BETA2,TOK,BETA )
CXXX----KOMПЛEKC "MS1" -->
C   ПOДПPOГPAMMA:   FBETA(EDM,E,XG,X,ALFA,TOK )
C   ------------
C   HAЗHAЧEHИE:    Нахождение корень уравнения F(BETA) - TOK = 0
C
C   METOД:         Ньютона
C
C   ПAPAMETPЫ:     EDM  - Амплитуда ЕДС питающей сети
C   ---------      E    - ПротивоЕДС
C                  XC1   - Суммарное индуктивное сопротивление
C                  XG    - Индуктивное сопротивление 
C                  BETA1 - Максимальный Уголь отпирания очередного вентиля
C                  BETA2 - Минимальный уголь отпирания очередного вентиля
C                  TOK  - Аппроксимированное среднее значение тока
C
C
c*****************************************************
         DATA PI /3.141593/ , EPS/0.001/
         DO 10 I = 1,100
         BETA = ( BETA1 + BETA2)*0.5
       FF = 0.866*SIN(BETA)*EDM/XC1 + COS(BETA)*EDM/XG -
     -    E*BETA/XC1 - TOK
         IF (ABS(FF) .LT. EPS)  RETURN
C            write(*,*) alfa,ff
C            pause
         IF ( FF.GT.0 )    BETA1 = BETA
         IF ( FF.LT.0 )    BETA2 = BETA
 10      CONTINUE
         WRITE(*,*) 'Решение уравнения управления ВП не сходится '
         WRITE(*,*) 'BETA2 =',BETA2,'BETA1=',BETA1,'FF',FF,TOK
         PAUSE
         RETURN
         END
