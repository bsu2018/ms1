          SUBROUTINE  beg99(irest,ndir)
          COMMON/PROF1/ MASPRT(200)
          CHARACTER*8 NNAME(5)
          EQUIVALENCE( MASPRT(128), FLGDIG  )
          INTEGER  FLGDIG
C**********************************************************************
         integer     HXX,MXX,SXX,MSXX
         integer     YYY,MYY,DYY
C######################################################################
         DATA NNAME/'#����','#�����','#����','#�����','        '/
         ndir1 = ndir
C====>  ��KCA��� �AT� � BPEMEH� BBO�A �H�OPMA���
         CALL GETTIM_(HXX,MXX,SXX,MSXX)
         CALL GETDAT_(YYY,MYY,DYY)
         YYY = MOD ( YYY,100 )
C######################################################################
C   99 ----> * !!!!!!!!!
         write(*,*) char(27),'[2J'
         WRITE(*,1989)
1989     FORMAT(
     *'[02;0H',1X, '�', 76('�'), '�',
     *'[03;0H',' ��',74X,'��',
     *'[04;0H',
     *' ��',11X,'  WELCOME  TO  *MS1*  MACROMODELING  SYSTEM',20X,'��',
     *'[05;0H', ' ��',74X,'��',
     *'[06;0H',
     *' ��',11X,'     (C) Boru S.,  1984-1990               ',20X,'��',
     *'[07;0H',
     *' ��',11X,'         Panin W., 1987-1990               ',20X,'��',
     *'[08;0H',
     *' ��',11X,'         All rights reserved.              ',20X,'��',
     *'[09;0H', ' ��',74X,'��',
     *'[10;0H', 1X, 78('�') )
C######################################################################
         IF ( irest .EQ. 0 ) THEN
            WRITE(*, 9700) DYY,MYY,YYY,HXX,MXX,SXX
9700        FORMAT( '[11;0H', '[K',
     =              '  START :    DATE ',2(I2.2,'/'),I2.2,
     =              '   TIME ',2(I2.2,':'),I2.2,'.')
C######################################################################
         ELSE
              IF (ndir1 .LE. 0  .OR. ndir1 .GT. 4) ndir1=5
                 WRITE(*, 9701) DYY,MYY,YYY,HXX,MXX,SXX,NNAME(ndir1)
9701             FORMAT( '[11;0H', '[K',
     =                   '  START :    DATE ',2(I2.2,'/'),I2.2,
     =                   '   TIME ',2(I2.2,':'),I2.2,'.',
     =                   '      �믮������ ��४⨢� ',A8        )
         ENDIF
         return
         end
