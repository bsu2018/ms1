         SUBROUTINE picture(name )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:   picture(name )
C   -------------
C   HAáHAóEHàE:  copy screen to the file
C   ----------
C   METOÑ:  toolkit 's subroutine   
C   -----
C
C   èAPAMETPõ:  name -name .pcx  file
C
C   èOÑèPOÉPAMMõ:  pbfile
C
CZZZ-------------------------------------------------------------------
        integer*2 setup(6),error,name
        character*5 picname
        character*1 pnm
        pnm = name + 48
        picname = pnm//'.pcx'
c---- Save the screen to a .PCX file  
        setup(1)=0
        setup(2)=1  
        setup(3)=640        
        setup(4)=350
        call pbfile( picname, setup, error )
c---   
	if( error.NE.0 ) write(*,*) 'Error writing screen'

	return
	end
