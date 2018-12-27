         SUBROUTINE DRCIRC( KAR, DAT, MSWALL, IRCC)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)---IRCC !!!
C   O„POƒPAMMA:    DRCIRC( KAR, DAT, MSWALL)                 1991 £.
C   ------------
C   HA‡HA—EHˆE:    ##O™ˆ‰ MOHˆTOP OPAOTKˆ #–Eœ ***
C   ----------
C   METO„:         CM. / DRCIR0, DRCIR1, DRCIR2;
C   -----           B V.M = 2.0  -  PEA‹ˆ‡OBAHO CTATˆ—ECKOE B›„E‹EHˆE
C                   O.. O„ ˆH”OPMA–ˆOHH›E MACCˆB›:
C                  MACCˆB› B›„E‹ŸžTŸ, ˆX „ˆCKPˆTOP› ˆHˆ–ˆA‹ˆ‡ˆP“žTCŸ;
C                  B COOTBETCTBˆˆ C B›„E‹EHHO‰ O.. „E‹AETCŸ ‡AˆCœ B
C                  MASPRT CˆCOK “KA‡ATE‹E‰;
C                  (HEOXO„ˆMO EPEˆCATœ)
C
C   APAMETP›:     CM. DRCIR0, DRCIR1, DRCIR2;
C   ---------      IRCC = 0 ¯à¨ ¢å®¤¥ - § ¯à®á #–…œ
C                  IRCC = 1 ¯à¨ ¢å®¤¥ - § ¯à®á #X–…œ
C
C   O„POƒPAMM›:  1. COMMON PROF1, [ MEMORY, PONTER, PRINTN ]
C   ------------   2. DRCIR0, DRCIR1, DRCIR2  [REGCPU];
C   PˆME—AHˆE:    1. REGION "AAPAT" „O‹†EH ›Tœ ˆHˆ–ˆˆPOBA;
C   ----------     2. B KOH–E / COˆPAETCŸ "M“COP";
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/MEMORY/ NREG, REGION(1)
         COMMON/PONTER/ NPRT, PRT(1)
         COMMON/PRINTN/ NW6
         INTEGER PRT, REGION, FRCPU, BA0, DMSWAL, FMSWAL
         DIMENSION KAR(1), DAT(1)
C--->
         EQUIVALENCE ( MASPRT(73), FRCPU )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(13), NWTR  )
         EQUIVALENCE ( MASPRT(1), KBA0  )
         EQUIVALENCE ( MASPRT(2), NMBA0 )
         EQUIVALENCE ( MASPRT(4), N1BA0 )
         EQUIVALENCE ( MASPRT(5), KKNOT )
         EQUIVALENCE ( MASPRT(6), NDKNOT )
         EQUIVALENCE ( MASPRT(8), KPAR0  )
         EQUIVALENCE ( MASPRT(9), NDPAR0 )
         EQUIVALENCE ( MASPRT(19), KMSWAL )
         EQUIVALENCE ( MASPRT(20), DMSWAL )
         EQUIVALENCE ( MASPRT(74), FMSWAL )
C######################################################################
         DIMENSION BA0(800)
         DIMENSION KNOT(650)
         DIMENSION PAR0(1500)
         DIMENSION MSWALL(2000)
         FMSWAL = 0
         NW6 = NWTRE
C
C******  IRCC = 0
C
C----------------------------------------------------------------------
C  C T A T ˆ — E C K O E    B › „ E ‹ E H ˆ E    A M Ÿ T ˆ.
C----------------------------------------------------------------------
         NDKNOT = 650
         NDPAR0 = 1500
         NDBA0 = 800
         DMSWAL = 2000
C--->
         NMBA0 = NDBA0 / N1BA0
C--->
C--> —ˆCKˆ ˆ ˆHˆ–ˆˆPOBAHˆE MACCˆBOB:
C
         DO 1 I = 1, NDKNOT
1        KNOT(I) = -1
         DO 2 I = 1, NDPAR0
2        PAR0(I) = -1.0
         DO 3 I = 1, NDBA0
3        BA0(I) = -1
         DO 4 I = 1, DMSWAL
4        MSWALL(I) = -1
C-->  MAX PA‡MEPHOCTˆ:
         BA0(2) = NDBA0
         KNOT(2) = NDKNOT
         PAR0(2) = NDPAR0
         MSWALL(2) = DMSWAL
C--->  “KA‡ATE‹ˆ HA CBOO„H›E
         BA0(3) = 6
         KNOT(3) = 6
         PAR0(3) = 6.0
         MSWALL(3) = 6
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  O—ˆCTKA NXBA0[ MASPRT(3) ]
C  T.K INPUT0 MO†ET „OBBO„ˆTœ
       MASPRT(3) = 0
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C######################################################################
C -->  B›O‹HEHˆE „ˆPEKTˆB›  #–Eœ
C######################################################################
C   ---->  1991 £. <-----
        IF ( IRCC .EQ. 0 ) THEN
            IRCC = 0
            CALL DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRCC)
         ELSE
            IRCC = 0
            CALL DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRCC)
         END IF
         IF ( IRCC.EQ. 2 ) RETURN
C######################################################################
C        CALL PRINT0( BA0, KNOT, PAR0)
C######################################################################
         CALL DRCIR1( BA0, KNOT, PAR0,      IRCC)
         IF ( IRCC.EQ. 2 ) RETURN
C######################################################################
C        CALL PRINT0( BA0, KNOT, PAR0)
C######################################################################
         CALL DRCIR2(BA0, KNOT, PAR0, MSWALL )
C######################################################################
C        CALL PRINT0( BA0, KNOT, PAR0)
         IRCC = 0
         RETURN
         END
