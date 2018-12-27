         SUBROUTINE KSOPEN(kf, forma, irc)
CXXX----KOMПЛEKC "MS1" -->
C   ПOДПPOГPAMMA:   KSOPEN(kf, forma, irc )
C   ------------
C   HAЗHAЧEHИE:    *** Открытие каналов ввода/вывода  ***
C   -----------         ПОСЛЕДОВАТЕЛЬНЫЙ ДОСТУП
C
C   METOД:         Формирование имени файла:
C  -------
C                      Jname{masprt(125)}//'.K'//xx
C
C            xx - номер канала --> kf
C
C   ПAPAMETPЫ:     kf -   Номер канала
C   ---------      form=0 - 'FORMATTED', =1 - 'UNFORMATTED'
C                  irc = 0 - все хорошо
C                      <> 0 - значение iostat
C   ПOДПPOГPAMMЫ:  1. COMMON PROF1
C
C   ПPИMEЧAHИE:    KOHTPOЛЬ не проводится
C   ----------
CZZZ-------------------------------------------------------------------
         CHARACTER*2 kanal, k_kk
         COMMON/ PROF1 / MASPRT(200)
         EQUIVALENCE(JNAME,   MASPRT(125))
         EQUIVALENCE( NWTR,   MASPRT(13))
         EQUIVALENCE( NWTRE,  MASPRT(14))
         CHARACTER*8 JNAME
         character*12 fullname, tempname
         INTEGER       forma
         DATA  k_kk/'.K'/
         irc = 0
!         write(*,*)'-- ksopen -- forma=', 
!     *     forma, ' JNAME=', JNAME
C********
C        OPEN (UNIT=03,FILE='MS1-1989.REZ')
         WRITE(UNIT=kanal,FMT=5555  ) kf
5555     FORMAT(I2.2)
!!!!!!!!!!
         call conact(8, JNAME, 2, k_kk, 12, tempname)
         call conact(12, tempname, 2, kanal, 12, fullname) 
!!!!         write(*,*)'--- kf fullname=', kf, ' ', fullname 
!!!!!!!!!! 




         IF ( forma .EQ. 0 ) THEN
!!!         OPEN (UNIT=kf,FILE=JNAME//k_kk//kanal,ERR=555,IOSTAT=irc,
         OPEN (UNIT=kf,FILE=fullname,ERR=555,IOSTAT=irc,
     *         FORM='FORMATTED')
C********
         IF(irc .NE. 0 )  GO TO 555
         RETURN
         ENDIF
         IF ( forma .EQ. 1 ) THEN
!!!         OPEN (UNIT=kf,FILE=JNAME//k_kk//kanal,ERR=555,IOSTAT=irc,
         OPEN (UNIT=kf,FILE=fullname,ERR=555,IOSTAT=irc,
     *         FORM='UNFORMATTED')
C********
         IF(irc .NE. 0 )  GO TO 555
         RETURN
         ENDIF
C----------------------------------------------------------------------
555    CONTINUE
       WRITE(*,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(*,*) '      Файл ',fullnamel,' не открывается'
       WRITE(*,*) '  ****** iostat = ',irc,'******************* '
C
       WRITE(NWTR,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(NWTR,*) '      Файл ',fullnamel,' не открывается'
       WRITE(NWTR,*) '  ****** iostat = ',irc,'******************* '
C
       WRITE(NWTRE,*) '        ***** !!!!! MS1 !!!!! ***** '
       WRITE(NWTRE,*) '      Файл ',fullnamel,' не открывается'
       WRITE(NWTRE,*) '  ****** iostat = ',irc,'******************* '
       RETURN
       END
