         SUBROUTINE dater_(data)
         CHARACTER*8 data



         CHARACTER(8) Date   ! The date in the form ccyymmdd: 
                             !! century, year, month and day; 
         CHARACTER(9)  Time  ! The time in the form `hhmmss.ss':
                             !! hours, minutes, seconds and milliseconds; 
         CHARACTER(9) Zone   ! The difference between local time and UTC (GMT)
                             !! in the form Shhmm: sign, hours and minutes, 
                             !!! e.g. `-0500' (winter in New York);
      !! INTEGER*1  Values(8) ! The year, month of the year, day of the month,
                              !! time difference in minutes from UTC,
                              !!! hour of the day, minutes of the hour,
                              !!!! seconds of the minute, and milliseconds of the second 
                              !!!!! in successive values of the array.




         INTEGER YYY,MYY,DYY
C====>  ”ˆKCA–ˆŸ „AT› ˆ BPEMEHˆ BBO„A ˆH”OPMA–ˆˆ
*!!!!!!        CALL GETTIM(HXX,MXX,SXX,MSXX)
       !!!  CALL GETDAT(YYY,MYY,DYY)
        CALL Date_and_Time(Date, Time, Zone) !!!, Values)  
!        write(*,*) 'Date=<',Date ,'>'
        write(*,*) 'Time=<',Time ,'>'
!        write(*,*) 'Zone<',Zone  ,'>'
       !!!!! write(*,*) Values
        !return 

C    B›BO„ ‡AƒO‹OBKA.....
!!!         WRITE(UNIT=data, fmt=9000) DYY,MYY,YYY
c        WRITE(UNIT=FOR, 9700) MYY,DYY,YYY,HXX,MXX,SXX,MSXX
9000     FORMAT(i2,'/',i2,'/',i2)
!         data=Date(7:8) // '/' Date(5:6) // '/' Date(3:4) 
           WRITE(UNIT=data, fmt=9001) Date(7:8), Date(5:6), Date(3:4) 
9001        format(A2,'/',A2,'/',A2)
!!!              egetgtdwtgwdtg
         RETURN
          END
