          SUBROUTINE end99
!!         write(99,*) char(27),'[2J'
         WRITE(*,1990)
1990     FORMAT(
     *1X, 78('�') /
     *' 께',74X,'께'/
     *' 께',11X,'       RESULTS OF CALCULATION WERE STORED  ',20X,'께'/
     *' 께',11X,'          IN FILES NAME.REZ & NAME.CON     ',20X,'께'/
     *' 께',74X,'께'/
     *' 께',10X,'      THANK  YOU  FOR  USE  OUR  SYSTEM  !!!',20X,'께'/
     *' 께',74X,'께'/
     *' 께',11X,'             GOOD  BYE  룼첓 ...           ',20X,'께'/
     *' 께',74X,'께'/
     *1X, 78('�') )
C######################################################################
         return
         end
