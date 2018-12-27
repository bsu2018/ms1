       SUBROUTINE PMTS6 ( ABIG, VEK, PVEK )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)--
C   SUBROUTINE:  PMTS6.( ABIG, VEK, PVEK )
C   ----------
C
C DIGIT FOR SIMETRIC MATRIX #ABIG# AND VECTORS #VEK#
C  VECTORS #PVEK# := #ABIG# * #VEK#
C    DIMENSINS FOR MATRIX AND VECTORS IS 6.
C
CZZZ---------------------------------------------------------
        DIMENSION ABIG(*), VEK(*), PVEK(*)
       A11 = ABIG( 1  )
       A12 = ABIG( 2  )
       A13 = ABIG( 3  )
       A14 = ABIG( 4  )
       A15 = ABIG( 5  )
       A16 = ABIG( 6  )
       A22 = ABIG( 7  )
       A23 = ABIG( 8  )
       A24 = ABIG( 9  )
       A25 = ABIG( 10  )
       A26 = ABIG( 11  )
       A33 = ABIG( 12  )
       A34 = ABIG( 13  )
       A35 = ABIG( 14  )
       A36 = ABIG( 15  )
       A44 = ABIG( 16  )
       A45 = ABIG( 17  )
       A46 = ABIG( 18  )
       A55 = ABIG( 19  )
       A56 = ABIG( 20  )
       A66 = ABIG( 21  )
       V1 = VEK( 1  )
       V2 = VEK( 2  )
       V3 = VEK( 3  )
       V4 = VEK( 4  )
       V5 = VEK( 5  )
       V6 = VEK( 6  )
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       PVEK( 1  ) = V6*A16+V5*A15+V4*A14+V3*A13+V2*A12+A11*V1
       PVEK( 2  ) = V6*A26+V5*A25+V4*A24+V3*A23+A22*V2+A12*V1
       PVEK( 3  ) = V6*A36+V5*A35+V4*A34+A33*V3+A23*V2+A13*V1
       PVEK( 4  ) = V6*A46+V5*A45+A44*V4+A34*V3+A24*V2+A14*V1
       PVEK( 5  ) = V6*A56+A55*V5+A45*V4+A35*V3+A25*V2+A15*V1
       PVEK( 6  ) = A66*V6+A56*V5+A46*V4+A36*V3+A26*V2+A16*V1
       RETURN
       END
