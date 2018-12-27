           SUBROUTINE MSGCON(I)
C  ВЫДача сообщения на консоль
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( TIME, MASPRT(29) )
C
           IF ( I .EQ. 1 ) THEN
           WRITE(99,*)  ''
           WRITE(99,*)   '[0;0HВнимание: создается контрольная точка длX
     Xя времени модели ',TIME, ' cek.'
           RETURN
           ENDIF
           IF ( I .EQ. 2 ) THEN
C          WRITE(99,*)  ''
           WRITE(99,*)   '[0;0HВнимание: контрольная точка  создана  длX
     Xя времени модели ',TIME, ' cek.'
           RETURN
           ENDIF
           END
