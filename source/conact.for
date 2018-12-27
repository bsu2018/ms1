           subroutine conact(k1, s1, k2, s2, k3, s3)
           character* (*) s1, s2, s3
!               s3=s1//s2 --> k декларированные размеры  сторок
!                пробелы в концах строк игнорируются... 
           integer i, l1, l2 ! l1 l2 len str s1 s2
           do i=1,k3
              s3(i:i)=' '
           end do  

           k=1
           l1=k1 
           do i=k1, 1, -1 
              if ( s1(i:i) .eq. ' ' ) then
                 l1=l1-1
                 cycle
              endif   
              exit 
           end do 
            
           l2=k2 
           do i=k2, 1, -1 
              if ( s2(i:i) .eq. ' ' ) then
                 l2=l2-1
                 cycle
              endif   
              exit 
           end do 
           do i=1,l1
              s3(k:k)=s1(i:i)
              k=k+1
              if ( k .gt. k3) exit
           end do  
           do i=1,l2
              if ( k .gt. k3) exit
              s3(k:k)=s2(i:i)
              k=k+1
           end do  
           return
           end
