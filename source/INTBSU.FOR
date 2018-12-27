       SUBROUTINE INTBSU( KXX , irest)
CXXXX------------------------------------------------------------------
C   ПOДПPOГPAMMA:  INTBSU(KXX)
C   -------------
C   HAЗHAЧEHИE:    "ПAPOЛЬHAЯ" ЗAЩИTA
C   ----------
C   METOД:         HE PACKPЫBAETCЯ
C   -----
C   ПAPAMETPЫ:
C   ---------
C   ПOДПPOГPAMMЫ:          "DRMET0", ERORXX, INTPRT
C   ------------
C   ПPИMEЧAHИE:
C   ----------
CZZZ-------------------------------------------------------------------
       DIMENSION KXX(*)
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(1)
       EQUIVALENCE ( NWTR, MASPRT(13) )
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       COMMON /SERVZ7/ MAKMOD(3),  MAKSEQ, NAMEB
       DATA IXX / ' MS1' /, NAMEB1/'БOPЮ'/,nnn_1/1997673236/
       DATA nnn_2/2010028909/
C----------------------------------------------------------------------
       IF ( MAKSEQ .EQ. 0 ) GO TO 1
12     CONTINUE
       NAMEB1 = nnn_1 + nnn_2
       IF ( KXX(2) .EQ. IXX .AND.  NAMEB .EQ. NAMEB1 ) RETURN
C--->
C-->
       WRITE(NWTRE, 10 )
       WRITE(NWTR,  10 )
10     FORMAT(/////' *** Hе санкционированная работа с комплексом',
     = ' ***'///// )
       CALL STOPP(666)
1      CONTINUE
C--->   HAЧ.  УCTAHOBKИ****
C******CALL  ERORXX( 0 )
C****** CALL INTPRT
       IF ( irest .EQ. 0 ) THEN
       WRITE(NWTR, 1100)    (MAKMOD(I), I=1,3), MASPRT(21)
1100   FORMAT(//1X,79('*')/25X,3A4,31X,'Bерсия ',1A4/1X,79('*')  /)
       WRITE(NWTRE,11)    (MAKMOD(I), I=1,3), MASPRT(21), NAMEB
11     FORMAT(//1X,79('#')/25X,3A4,31X,'Bерсия ',1A4/1X,79('#')  /
     =  22X, '(Pазработка выполнена ', 1A4,' Cергеем)' // )
C    =  13X, '(Pазработка выполнена ', 1A4,' Cергеем',
C    =       ' и Паниным Володей)' // )
C*******CALL  DRMET0( 1,0)
       ENDIF
       MAKSEQ = 1
       GO TO 12
       END
