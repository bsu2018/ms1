         SUBROUTINE picture(name )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   picture(name )
C   -------------
C   HA�HA�EH�E:  copy screen to the file
C   ----------
C   METO�:  toolkit 's subroutine   
C   -----
C
C   �APAMETP�:  name -name .pcx  file
C
C   �O��PO�PAMM�:  pbfile
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