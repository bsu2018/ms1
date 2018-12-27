               character*20 a,b,c
  
   1           do i=1,20
                 a(i:i)=' '
                 b(i:i)=' '  
               end do  
                
               write(*,*) 'a='
               read(*,*)a   
               write(*,*) 'b='
               read(*,*)b   

               write(*,*) 'a=<',a,'>' 
               write(*,*) 'b=<',b,'>' 
               write(*,*) 'a//b=<',a//b,'>' 
               call conact(20, a, 20, b, 20, c)     
               write(*,*) 'c=<',c,'>'  
               
               goto 1
               end
  