         SUBROUTINE DRCIRC( KAR, DAT, MSWALL, IRCC)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)---IRCC !!!
C   èOÑPOÉPAMMA:    DRCIRC( KAR, DAT, MSWALL)
C   ------------
C   HAáHAóEHàE:    ##OÅôàâ MOHàTOP OÅPAÅOTKà #ñEèú ***
C   ----------
C   METOÑ:         CM. è/è DRCIR0, DRCIR1, DRCIR2;
C   -----           B V.M = 2.0  -  PEAãàáOBAHO CTATàóECKOE BõÑEãEHàE
C                   O.è. èOÑ àHîOPMAñàOHHõE MACCàBõ:
C                  MACCàBõ BõÑEãüûTü, àX ÑàCKPàèTOPõ àHàñàAãàáàPìûTCü;
C                  B COOTBETCTBàà C BõÑEãEHHOâ O.è. ÑEãAETCü áAèàCú B
C                  MASPRT CèàCOK ìKAáATEãEâ;
C                  (HEOÅXOÑàMO èEPEèàCATú)
C
C   èAPAMETPõ:     CM. DRCIR0, DRCIR1, DRCIR2;
C   ---------
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, [ MEMORY, PONTER, PRINTN ]
C   ------------   2. DRCIR0, DRCIR1, DRCIR2  [REGCPU];
C   èPàMEóAHàE:    1. REGION "AèèAPAT" ÑOãÜEH ÅõTú àHàñààPOBA;
C   ----------     2. B KOHñE è/è COÅàPAETCü "MìCOP";
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
         IRCC = 0
C----------------------------------------------------------------------
C  C T A T à ó E C K O E    B õ Ñ E ã E H à E   è A M ü T à.
C----------------------------------------------------------------------
         NDKNOT = 650
         NDPAR0 = 1500
         NDBA0 = 800
         DMSWAL = 2000
C--->
         NMBA0 = NDBA0 / N1BA0
C--->
C--> óàCKà à àHàñààPOBAHàE MACCàBOB:
C
         DO 1 I = 1, NDKNOT
1        KNOT(I) = -1
         DO 2 I = 1, NDPAR0
2        PAR0(I) = -1.0
         DO 3 I = 1, NDBA0
3        BA0(I) = -1
         DO 4 I = 1, DMSWAL
4        MSWALL(I) = -1
C-->  MAX PAáMEPHOCTà:
         BA0(2) = NDBA0
         KNOT(2) = NDKNOT
         PAR0(2) = NDPAR0
         MSWALL(2) = DMSWAL
C--->  ìKAáATEãà HA CBOÅOÑHõE
         BA0(3) = 6
         KNOT(3) = 6
         PAR0(3) = 6.0
         MSWALL(3) = 6
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C  OóàCTKA NXBA0[ MASPRT(3) ]
C  T.K INPUT0 MOÜET ÑOBBOÑàTú
       MASPRT(3) = 0
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C######################################################################
C -->  BõèOãHEHàE ÑàPEKTàBõ  #ñEèú
C######################################################################
         CALL DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRC )
         IF ( IRC .EQ. 2 ) RETURN
C######################################################################
C        CALL PRINT0( BA0, KNOT, PAR0)
C######################################################################
         CALL DRCIR1( BA0, KNOT, PAR0,      IRC )
         IF ( IRC .EQ. 2 ) RETURN
C######################################################################
C        CALL PRINT0( BA0, KNOT, PAR0)
         CALL DRCIR2(BA0, KNOT, PAR0, MSWALL )
C        CALL PRINT0( BA0, KNOT, PAR0)
         RETURN
         END
