       SUBROUTINE MMTS5 ( ABIG, AOBR )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)--
C    SUBROUTINE:   MMTS5 ( ABIG, AOBR )
C    ----------
C
C DIGIT FOR SIMETRIC MATRIX #ABIG# MATRIX #AOBR#:=#ABIG#**(-1)
C       DIMENSIONS FOR MATRIX IS 5.
C
CZZZ---------------------------------------------------------
       DIMENSION ABIG(*), AOBR(*)
       write(6,100)
100    format(' é°‡†È•≠®• ™ Ø/Ø MMTS5 ')
       call suberr(10)
       RETURN
       END
