         SUBROUTINE dater$(data)
         CHARACTER*8 data
         INTEGER YYY,MYY,DYY
C====>  ”ˆKCA–ˆŸ „AT› ˆ BPEMEHˆ BBO„A ˆH”OPMA–ˆˆ
*        CALL GETTIM(HXX,MXX,SXX,MSXX)
         CALL GETDAT(YYY,MYY,DYY)
         YYY = MOD ( YYY,100 )
C    B›BO„ ‡AƒO‹OBKA.....
         WRITE(UNIT=data, 9000) DYY,MYY,YYY
c        WRITE(UNIT=FOR, 9700) MYY,DYY,YYY,HXX,MXX,SXX,MSXX
9000     FORMAT(i2,'/',i2,'/',i2)
         RETURN
          END
