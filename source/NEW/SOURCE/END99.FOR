          SUBROUTINE end99
         write(99,*) char(27),'[2J'
         WRITE(99,1990)
1990     FORMAT(
     *1X, 78('�') /
     *' ��',74X,'��'/
     *' ��',11X,'       RESULTS OF CALCULATION WERE STORED  ',20X,'��'/
     *' ��',11X,'          IN FILES NAME.REZ & NAME.CON     ',20X,'��'/
     *' ��',74X,'��'/
     *' ��',10X,'      THANK  YOU  FOR  USE  OUR  SYSTEM  !!!',20X,'��'/
     *' ��',74X,'��'/
     *' ��',11X,'             GOOD  BYE  !!!                ',20X,'��'/
     *' ��',74X,'��'/
     *1X, 78('�') )
C######################################################################
         return
         end
