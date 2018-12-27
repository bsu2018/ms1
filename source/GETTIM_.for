           subroutine GETTIM_(HXX,MXX,SXX,MSXX)
           integer HXX,MXX,SXX,MSXX
!!         integer YYY,MYY,DYY 
!c===================================================
!c          время системное получаем.....
!c====================================================
          CHARACTER(8) Date   ! The date in the form ccyymmdd: 
Cc                             !! century, year, month and day; 
          CHARACTER(9)  Time  ! The time in the form `hhmmss.ss':
Cc                            !! hours, minutes, seconds and milliseconds; 
          CHARACTER(9) Zone   ! The difference between local time and UTC (GMT)
Cc                            !! in the form Shhmm: sign, hours and minutes, 
Cc                             !!! e.g. `-0500' (winter in New York);

          CALL Date_and_Time(Date, Time, Zone)  
          read( Time,'(i2,i2,i2,1x,i2)' ) HXX, MXX, SXX, MSXX
          return
          end 