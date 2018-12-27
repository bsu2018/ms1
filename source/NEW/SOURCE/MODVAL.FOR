         SUBROUTINE MODVAL( VALLN1, LOGYTR )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒAMMA:    MODVAL( VALLN1, LOGYTR )
C   -----------
C   HA‡HA—EHˆE:   - "PACTAHOBKA" “‡‹OB›X HAPŸ†EHˆ‰ COƒ‹ACHO
C   ----------      ˆH–ˆ„EHTH›X “‡‹OB O „AHH›M MACCˆBA LOGYTR.
C                   ˆH”OPMA–ˆŸ O PE˜EHˆˆ "TEHEBO‰" CˆCTEM›
C                   „O‹†HA ›Tœ ˆCO‹œ‡OBAHA ˆ COXPAHEHA -> TEPŸETCŸ !!!
C
C   METO„:         COƒ‹ACHO HA‡HA—EHˆŸ ˆ CTP“KT“P› LOGYTR:
C   -----          EC‹ˆ LOGYTR(K) > 0, TO VALLN1(K) = VALLN1(LOGYTR(K))
C                  EC‹ˆ LOGYTR(K) = 0, TO VALLN1(K) = 0.0
C                  EC‹ˆ LOGYTR(K) = -1, TO VALLN1(K) HE ˆ‡MEHŸETCŸ;
C   APAMETP›:
C   ----------     VALLN1 - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰;
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 6;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C   O„POƒPAMM›: 1. COMMON PROF1
C   ------------
C   PˆME—AHˆE: 1. KOHTPO‹œ HE POˆ‡BO„ˆTCŸ;
C   -----------
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION  VALLN1(1),  LOGYTR(1)
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(26), NSYS   )
         DO 100  K = 1, NSYS
         M = LOGYTR(K)
         IF ( M. LE. -1 ) GO TO 100
         IF ( M .NE. 0 ) GO TO 50
C-->  K ˆH–ˆ„EHTEH C H“‹EB›M “‡‹OM –Eˆ:
         VALLN1(K) = 0.0
         GO TO 100
50       CONTINUE
C        K - HOMEP “‡‹A, HAPŸ†EHˆE HA KOTOPOM PABHO HAPŸ†EHˆž
C        PAC—ˆTAHHOM“ „‹Ÿ “‡‹A  M...Pˆ PE˜EHˆˆˆ OCHOBHO‰ CˆCTEM›...
         VALLN1(K) = VALLN1( M )
C---->
100      CONTINUE
         RETURN
         END
