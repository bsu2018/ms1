

         X = 0.314
         DO 10 I = 1,10
         ALFA = X*I
         TOK = COS(ALFA)*14.99
         WRITE(*,*) ALFA,TOK
  10     CONTINUE
         STOP
         END
