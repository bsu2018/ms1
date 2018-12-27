       SUBROUTINE FIDEF(MSWALL, TOKN, VALLN, KAR, DAT,irest)
CXXX----KOMПЛEKC "MS1" -->
C   ПOДПPOГPAMMA:   FIDEF(MSWALL, TOKN, VALLN,KAR, DAT, irest)
C   ------------
C   HAЗHAЧEHИE:    *** Открытие СИСТЕМНЫХ каналов ввода/вывода  ***
C   -----------         ПОСЛЕДОВАТЕЛЬНЫЙ ДОСТУП
C                       NRDR+NWTR+NWTRE+NSPOOL
C              kf ==>    5     3    6  ???9
C   METOД:         Формирование имени файла:
C  -------
C                      Jname{masprt(125)}//'.K'//xx
C
C            xx - номер канала --> kf
C
C   ПAPAMETPЫ:
C   ---------
C   ПOДПPOГPAMMЫ:  1. COMMON PROF1
C   ------------
C   ПPИMEЧAHИE:    KOHTPOЛЬ не проводится
C   ----------
CZZZ-------------------------------------------------------------------
         COMMON/ PROF1 / MASPRT(200)
         EQUIVALENCE( NWTR,   MASPRT(13))
         EQUIVALENCE( NRDR,   MASPRT(12))
         EQUIVALENCE( NWTRE,  MASPRT(14))
         EQUIVALENCE(NSPOOL,  MASPRT(18))
         EQUIVALENCE(NFILER,  MASPRT(83))
         EQUIVALENCE(NMAK,    MASPRT(89))
         EQUIVALENCE(NFCHEK,  MASPRT(95))
         EQUIVALENCE(NFACHK,  MASPRT(97))
         EQUIVALENCE(NREGX,   MASPRT(122))
         EQUIVALENCE(JNAME,   MASPRT(125))
         EQUIVALENCE(NZRDR,   MASPRT(127))
         DIMENSION MSWALL(*), TOKN(*), VALLN(*), KAR(1), DAT(1)
         CHARACTER*8 JNAME
         CHARACTER*8 NAME
         CHARACTER*1 BLANK
         CHARACTER*12 START_N
         CHARACTER*2 kanal, k_kk
         DATA START_N/'MS1_DATA.$$$'/, k_kk/'.K'/
         DATA BLANK/' '/
C----------------------------------------------------------------------
          DO 115 i=1,8
           NAME(i:i) = BLANK
115        JNAME(i:i) = BLANK
C----------------------------------------------------------------------
C      ОПИСАНИЕ ИСПОЛЬЗУЕМЫХ ФАЙЛОВ:
C----------------------------------------------------------------------
C----------------------------------------------------------------------
         OPEN (UNIT=99,FILE='CON')
C----------------------------------------------------------------------
         OPEN (UNIT=98,FILE=START_N,ERR=668,
     +         form='FORMATTED',iostat=iioo)
         READ(UNIT=98,FMT=9889,END=666,ERR=667)  NAME
C   irest  = 0  - рестарт не делать: = 1  - делать
         READ(UNIT=98,FMT=9888,END=666,ERR=667)  irest
9889     FORMAT(A8)
9888     FORMAT(i1)
         CLOSE (UNIT=98)
C********
         IF ( irest .EQ. 1 ) THEN
            CALL LOADMS(MSWALL, TOKN, VALLN, KAR, DAT, iii_rrr)
            IF ( iii_rrr .NE. 0 ) CALL STOPP(666)
         ENDIF
C********
C********
         IF ( irest .EQ. 0 ) THEN
            ii=1
            DO 111 i=1,8
               IF ( NAME(i:i) .EQ. BLANK ) GO TO 111
               JNAME(ii:ii) =  NAME(i:i)
               ii = ii + 1
111         CONTINUE
         ENDIF
C        OPEN (UNIT=03,FILE='MS1-1989.REZ')
         WRITE(UNIT=kanal,FMT=3333  ) NWTR
3333     FORMAT(I2.2)
         iiiiii = NWTR
         OPEN (UNIT=NWTR,FILE=JNAME//k_kk//kanal,ERR=555,
     +         form='FORMATTED',iostat=iioo)
C********
         IF ( irest .EQ. 1 ) THEN
333         read(NWTR,*,end=334)
            go to 333
334         backspace NWTR
         ENDIF
C********
C        OPEN (UNIT=06,FILE='MS1-1989.CON')
         WRITE(UNIT=kanal,FMT=3333  ) NWTRE
         iiiiii = NWTRE
         OPEN (UNIT=NWTRE,FILE=JNAME//k_kk//kanal,ERR=555,
     +         form='FORMATTED',iostat=iioo)
C********
         IF ( irest .EQ. 1 ) THEN
663         read(NWTRE,*,end=664)
            go to 663
664         backspace NWTRE
         ENDIF
C********
C        OPEN (UNIT=05,FILE='MS1-1989.JOB')
         WRITE(UNIT=kanal,FMT=3333  ) NRDR
         iiiiii = NRDR
         OPEN (UNIT=NRDR,FILE=JNAME//k_kk//kanal,ERR=555,
     +         form='FORMATTED',iostat=iioo)
C********
         IF ( irest .EQ. 1 ) THEN
            DO 5555 ii=1,NZRDR
5555        read(NRDR,*,end=6644)
         ENDIF
C********
C----------------------------------------------------------------------
6644   RETURN
666    WRITE(99,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(99,*) 'Файл MS1_DATA.$$$ не содержит имени файлов цепи'
       WRITE(99,*) '  *****  Выполнение работы прерывается ***** '
       STOP 666
C----------------------------------------------------------------------
667    WRITE(99,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(99,*) '       Файл MS1_DATA.$$$ не читается '
       WRITE(99,*) '  *****  Выполнение работы прерывается ***** '
       STOP 667
C----------------------------------------------------------------------
668    WRITE(99,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(99,*) '      Файл MS1_DATA.$$$ не открывается'
       WRITE(99,*) '  *****  Выполнение работы прерывается ***** '
       WRITE(99,*) '  *****     Iostat = ', iioo,     ' ***** '
       STOP 668
C----------------------------------------------------------------------
555    WRITE(99,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(99,*) '      Файл ',JNAME,k_kk,kanal,' не открывается'
       WRITE(99,*) '  *****  Выполнение работы прерывается ***** '
       WRITE(99,*) '  *****     Iostat = ', iioo,     ' ***** '
       WRITE(99,*) '  *****     iiiiii = ', iiiiii,   ' ***** '
       STOP 555
       END
