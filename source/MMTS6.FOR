       SUBROUTINE MMTS6 ( ABIG, AOBR )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)--
C    SUBROUTINE:   MMTS6 ( ABIG, AOBR )
C    ----------
C
C DIGIT FOR SIMETRIC MATRIX #ABIG# MATRIX #AOBR#:=#ABIG#**(-1)
C       DIMENSIONS FOR MATRIX IS 6.
C
CZZZ---------------------------------------------------------
       DIMENSION ABIG(21), AOBR(21)
       write(6,100)
100    format(' ���饭�� � �/� MMTS6 ')
       call suberr(10)
       RETURN
       END
