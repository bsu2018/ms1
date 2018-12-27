          common /abc/ a, b, c 
          common /abc1/ x, y, z 
          character(32) ans
          call test_b(55555)
          write(*,*) 'a b c =', a, ' ', b, ' ' , c
          write(*,*) 'x y z =', x, ' ', y, ' ' , z
          !!!OPEN (UNIT=99) !,FILE='con:',recl=256)
          write(6,*) '999999999999999999999999999999999999999999999'
          read(*,*) ans
          write(*,*) 'ans=<', ans, '>' 

           stop
           end