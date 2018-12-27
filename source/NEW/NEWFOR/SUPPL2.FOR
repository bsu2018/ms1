       SUBROUTINE  SUPPL2 (TJ1,TJ2 ,TJ3 ,TJ4,B,D)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (AT OCTOBER   1989 ƒ.)-----------
C   O„POƒPAMMA: SUPPL1(TJ1,TJ2, TJ3 ,TJ4,B,D)
C   -------------
C   HA‡HA—EHˆE:  B›—ˆC‹ŸET O™ˆ‰ ˆCTO—HˆK TOKA „B“X OC‹E„OBATE‹œHO
C   ----------   O„CE„ˆHEHH›X TPEX-O‹“CHˆKAMˆ C ˆCTO—HˆKAMˆ TOKA
C                TJ1 , TJ2  ˆ TJ3,TJ4
C                PE‡“‹œTAT B TJ1, TJ2
C                BCOMOƒATE‹œH›E ‡HA—EHˆŸ B,D
C                KOT-P›E B›—ˆC‹EH› B FM0035
C                OC‹“†ˆBAET O„POƒPAMMA DM0039
C
CZZZ--------------------------------------------------------------------
       TJ1 = TJ1 - B*(TJ2 + TJ3)
       TJ2 = TJ4 - D*(TJ2 + TJ3)
       RETURN
       END
