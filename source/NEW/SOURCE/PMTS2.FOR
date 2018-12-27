       SUBROUTINE PMTS2 ( ABIG, VEK, PVEK )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)--
C   SUBROUTINE:  PMTS2 ( ABIG, VEK, PVEK )
C   ----------
C
C DIGIT FOR SIMETRIC MATRIX #ABIG# AND VECTORS #VEK#
C  VECTORS #PVEK# := #ABIG# * #VEK#
C    DIMENSINS FOR MATRIX AND VECTORS IS 2.
C
CZZZ---------------------------------------------------------
        DIMENSION ABIG(*), VEK(*), PVEK(*)
       A11 = ABIG( 1  )
       A12 = ABIG( 2  )
       A22 = ABIG( 3  )
       V1 = VEK( 1  )
       V2 = VEK( 2  )
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       PVEK( 1  ) = V2*A12+A11*V1
       PVEK( 2  ) = A22*V2+A12*V1
       RETURN
       END
