         SUBROUTINE DM0000 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOMПЛEKC "MS1" -->
C   ПOДПPOГPAMMA:  DM0000 ( MSW, RMSW, YALL,       JALL, LOGYTR,
C   ------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HAЗHAЧEHИE:    Затычка имени фирмы  IBM
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
9999     CONTINUE
         NWTRE = MASPRT(14)
         NWTR  = MASPRT(13)
         WRITE(NWTRE,9998)  msw(2)
         WRITE(NWTR ,9998)  msw(2)
9998     FORMAT( ' ***  CБOЙ B DM0000 - Нет памяти для его подключен
     =ия     ***'/ 
     =           ' *** TIP = ',I8/)
         CALL SUBERR(0)
         RETURN
         END
