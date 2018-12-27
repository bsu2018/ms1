       integer a,b,c
!GCC$ ATTRIBUTES :: GFORTRAN_ERROR_BACKTRACE=yes
       real tarray(2)
        real  result
         call ETIME(tarray, result)
          write(*,*) tarray
       a=1; b=2; c=3
       call pgm(a)
       write(*,55) a, b  
55     format('a=',i4,' b=', i3 : ' c=', i2)
 
       do i=1,10
          call pgm(a)
          write(*,*) 'i=',i,' a=',a   
 !         CALL  backtrace
          call abort
        enddo
        stop
        end
       subroutine pgm(z)
        integer z, x
         data x /0/
!         save x
          save 
         x=x+1
         z=x
         return
         end

         
         