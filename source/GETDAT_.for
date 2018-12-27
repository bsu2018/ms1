          subroutine GETDAT_(YYY,MYY,DYY)  
C====>  ФИKCAЦИЯ ДATЫ И BPEMEHИ BBOДA ИHФOPMAЦИИ
         integer YYY,MYY,DYY 
!c===================================================
!c          дату и время системное получаем.....
!c====================================================
          CHARACTER(8) Date   ! The date in the form ccyymmdd: 
Cc                             !! century, year, month and day; 
          CHARACTER(9)  Time  ! The time in the form `hhmmss.ss':
Cc                            !! hours, minutes, seconds and milliseconds; 
          CHARACTER(9) Zone   ! The difference between local time and UTC (GMT)
Cc                            !! in the form Shhmm: sign, hours and minutes, 
Cc                             !!! e.g. `-0500' (winter in New York);

        CALL Date_and_Time(Date, Time, Zone)  
        read(Date,'(i4,i2,i2)' ) YYY,MYY,DYY 
         return 
        end