       SUBROUTINE PMTS4 ( ABIG, VEK, PVEK )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)--
C   SUBROUTINE:  PMTS4 ( ABIG, VEK, PVEK )
C   ----------
C
C DIGIT FOR SIMETRIC MATRIX #ABIG# AND VECTORS #VEK#
C  VECTORS #PVEK# := #ABIG# * #VEK#
C    DIMENSINS FOR MATRIX AND VECTORS IS 4.
C
CZZZ---------------------------------------------------------
        DIMENSION ABIG(*), VEK(*), PVEK(*)
       A11 = ABIG( 1  )
       A12 = ABIG( 2  )
       A13 = ABIG( 3  )
       A14 = ABIG( 4  )
       A22 = ABIG( 5  )
       A23 = ABIG( 6  )
       A24 = ABIG( 7  )
       A33 = ABIG( 8  )
       A34 = ABIG( 9  )
       A44 = ABIG( 10  )
       V1 = VEK( 1  )
       V2 = VEK( 2  )
       V3 = VEK( 3  )
       V4 = VEK( 4  )
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       PVEK( 1  ) = V4*A14+V3*A13+V2*A12+A11*V1
       PVEK( 2  ) = V4*A24+V3*A23+A22*V2+A12*V1
       PVEK( 3  ) = V4*A34+A33*V3+A23*V2+A13*V1
       PVEK( 4  ) = A44*V4+A34*V3+A24*V2+A14*V1
       RETURN
       END
