          SUBROUTINE  beg99(irest,ndir)
          COMMON/PROF1/ MASPRT(200)
          CHARACTER*8 NNAME(5)
          EQUIVALENCE( MASPRT(128), FLGDIG  )
          INTEGER  FLGDIG
C**********************************************************************
         integer     HXX,MXX,SXX,MSXX
         integer     YYY,MYY,DYY
C######################################################################
         DATA NNAME/'#ëóÖí','#èëóÖí','#ñÖèú','#ïñÖèú','........'/
         ndir1 = ndir
C====>  îàKCAñàü ÑATõ à BPEMEHà BBOÑA àHîOPMAñàà
         CALL GETTIM_(HXX,MXX,SXX,MSXX)
         CALL GETDAT_(YYY,MYY,DYY)
         YYY = MOD ( YYY,100 )
C######################################################################
C   99 ----> * !!!!!!!!!
!         write(*,*) char(27),'[2J'
         WRITE(*,1989)
1989     FORMAT(
     *'€', 76('€'), '€',/
     *'€€',74X,'€€',/
     *'€€',11X,'  WELCOME  TO  *MS1*  MACROMODELING  SYSTEM',20X,'€€',/
     *'€€',74X,'€€',                                               /
     *'€€',11X,'     (C) Boru S.,  1984-1990               ',20X,'€€',/
     *'€€',11X,'         Panin W., 1987-1990               ',20X,'€€',/
     *'€€',11X,'         All rights reserved.              ',20X,'€€',/
     *'€€',74X,'€€',                                               /
     * 78('€') )
C######################################################################
         IF ( irest .EQ. 0 ) THEN
            WRITE(*, 9700) DYY,MYY,YYY,HXX,MXX,SXX
9700        FORMAT( ' ', ' ',
     =              '  START :    DATE ',2(I2.2,'/'),I2.2,
     =              '   TIME ',2(I2.2,':'),I2.2,'.')
C######################################################################
         ELSE
              IF (ndir1 .LE. 0  .OR. ndir1 .GT. 4) ndir1=5
                 WRITE(*, 9701) DYY,MYY,YYY,HXX,MXX,SXX,NNAME(ndir1)
9701             FORMAT( ' ', !!!!! ' ',
     =                   '  START :    DATE ',2(I2.2,'/'),I2.2,
     =                   '   TIME ',2(I2.2,':'),I2.2,'.',     
     =                   '      ÇÎØÆ´≠•≠®• §®‡•™‚®¢Î ',A8        )
         ENDIF
         return
         end
