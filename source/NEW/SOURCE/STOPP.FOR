       SUBROUTINE STOPP(I)
CXXXX---KOMПЛEKC "MS1" --> BEPCИЯ 2.0
C   ПOДПPOГPAMMA:  STOPP(I)
C   -------------
C   HAЗHAЧEHИE:    Стоп в комплексеX ЗHAЧEHИЙ ДЛЯ MASPRT........
C   ----------
C   METOД:
C   -----
C   ПAPAMETPЫ:    i -  код завершениея
C   ---------
C   ПOДПPOГPAMMЫ:
C   ------------
C   ПPИMEЧAHИE:
C   ----------
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         IF ( I .EQ. 0 ) STOP 0
         IF ( I .EQ. 10 ) STOP 10
         IF ( I .EQ. 11 ) STOP 11
         IF ( I .EQ. 12 ) STOP 12
         IF ( I .EQ. 13 ) STOP 13
         IF ( I .EQ. 14 ) STOP 14
         IF ( I .EQ. 15 ) STOP 15
         IF ( I .EQ. 16 ) STOP 16
         IF ( I .EQ. 17 ) STOP 17
         IF ( I .EQ. 18 ) STOP 18
         IF ( I .EQ. 19 ) STOP 19
         IF ( I .EQ. 20 ) STOP 20
         STOP 666
         END
