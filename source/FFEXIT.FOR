       SUBROUTINE FFexit(I)
CXXXX---KOMПЛEKC "MS1" --> BEPCИЯ 2.0
C   ПOДПPOГPAMMA:  FFexit(I) exit - для gnu f77
C   -------------
C   HAЗHAЧEHИE:    Стоп в комплексе и передача кода завершения...
C   ----------
C   METOД:         это для WatCom Fortrana
C   -----
C   ПAPAMETPЫ:    i -  код завершениея
C   ---------
C   ПOДПPOГPAMMЫ:
C   ------------
C   ПPИMEЧAHИE:
C   ----------
CZZZ-------------------------------------------------------------------
C         include 'FSUBLIB.FI'
          CALL EXIT(i)
            
         END
