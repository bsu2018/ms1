@ECHO  OFF
@ECHO  Трансляция программы %1.FOR ........
e:\for77\bin\fl /4Y6 /AL /Ox   /FPc87 /c  %1.for
@IF ERRORLEVEL 1 goto err
@ECHO программa %1.FOR - O'KEY
@COPY %1.OBJ  \MS1\OBJ\%1.OBJ
@DEL %1.obj
goto end
:err
@ECHO  Ошибки в программе %1.FOR
rem@LIST   %1.lst
:end

