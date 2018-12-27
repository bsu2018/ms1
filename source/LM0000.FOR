       SUBROUTINE LM0000( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOMПЛEKC "MS1" --> BEPCИЯ 1.0  (OT ДEKAБPЯ 1986 Г.)------------
C   ПOДПPOГPAMMA: LM0000( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HAЗHAЧEHИE:   Заглушка
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       NWTR = MASPRT(13)
       WRITE(NWTR, 100)
100    FORMAT(//' *** LM0000: Покупайте мощные ЭВМ  # '//)
       CALL suberr(0)
       RETURN
       END
