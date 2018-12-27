       SUBROUTINE MMMS3 ( ABIG, AOBR )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)--
C    SUBROUTINE:   MMTS3 ( ABIG, AOBR )
C   ----------
C
C DIGIT FOR SIMETRIC MATRIX #ABIG# MATRIX #AOBR#:=#ABIG#**(-1)
C       DIMENSIONS FOR MATRIX IS 3.
C
CZZZ---------------------------------------------------------
       REAL*8 ABIG(*), AOBR(*)
       REAL*8 A11,A12,A13,A22,A23,A33,XDEN
       A11 = ABIG( 1  )
       A12 = ABIG( 2  )
       A13 = ABIG( 3  )
       A22 = ABIG( 4  )
       A23 = ABIG( 5  )
       A33 = ABIG( 6  )
C-------------------------------------------------------------
       XDEN = 1./(A33*A22*A11-A33*A12**2-A23**2*A11+2.*A23*A13*A12-A22*
     X A13**2)
       AOBR( 1  ) = XDEN*(A33*A22-A23**2)
       AOBR( 2  ) = XDEN*(-A33*A12+A23*A13)
       AOBR( 3  ) = XDEN*(A23*A12-A22*A13)
       AOBR( 4  ) = XDEN*(A33*A11-A13**2)
       AOBR( 5  ) = XDEN*(-A23*A11+A13*A12)
       AOBR( 6  ) = XDEN*(A22*A11-A12**2)
       RETURN
       END
