       SUBROUTINE MMTS2 ( ABIG, AOBR )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)--
C    SUBROUTINE:   MMTS2 ( ABIG, AOBR )
C   ----------
C
C DIGIT FOR SIMETRIC MATRIX #ABIG# MATRIX #AOBR#:=#ABIG#**(-1)
C       DIMENSIONS FOR MATRIX IS 2.
C
CZZZ---------------------------------------------------------
       DIMENSION ABIG(*), AOBR(*)
       A11 = ABIG( 1  )
       A12 = ABIG( 2  )
       A22 = ABIG( 3  )
C-------------------------------------------------------------
       XDEN = 1./(A22*A11-A12**2)
       AOBR( 1  ) = A22*XDEN
       AOBR( 2  ) = -A12*XDEN
       AOBR( 3  ) = A11*XDEN
       RETURN
       END
