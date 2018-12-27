          SUBROUTINE beg99(irest,ndir)
          COMMON/PROF1/ MASPRT(200)
          CHARACTER*8 NNAME(5)
          EQUIVALENCE( MASPRT(128), FLGDIG  )
          INTEGER  FLGDIG
C**********************************************************************
         integer     HXX,MXX,SXX,MSXX
         integer     YYY,MYY,DYY
C######################################################################
         DATA NNAME/'#ëóÖí','#èëóÖí','#ñÖèú','#ïñÖèú','        '/
         ndir1 = ndir
C====>  îàKCAñàü ÑATõ à BPEMEHà BBOÑA àHîOPMAñàà
         CALL GETTIM(HXX,MXX,SXX,MSXX)
         CALL GETDAT(YYY,MYY,DYY)
         YYY = MOD ( YYY,100 )
C######################################################################
         print *,'[2J'
         WRITE(99,1989)
1989     FORMAT(1x,
     *'[2;10H','  *MS1*  MACROMODELING  SYSTEM .Version 1.1'
     *'[4;10H','Copyright (C) ÅÇè ,1984-1991 . All rights reserved.')     
C######################################################################
         IF ( irest .EQ. 0 ) THEN
            WRITE(99, 9700) DYY,MYY,YYY,HXX,MXX,SXX
9700        FORMAT(1x,'[6;0H', '[K',
     =              ' START : DATE ',2(I2.2,'/'),I2.2,
     =              '   TIME ',2(I2.2,':'),I2.2,'.')
C######################################################################
         ELSE
              IF (ndir1 .LE. 0  .OR. ndir1 .GT. 4) ndir1=5
                 WRITE(99, 9701) DYY,MYY,YYY,HXX,MXX,SXX,NNAME(ndir1)
9701             FORMAT(1x,'[6;0H', '[K',
     =                   '  START :    DATE ',2(I2.2,'/'),I2.2,
     =                   '   TIME ',2(I2.2,':'),I2.2,'.',
     =                  '      ÇÎØÆ´≠•≠®• §®‡•™‚®¢Î ',A8        )
         ENDIF
         return
         end
