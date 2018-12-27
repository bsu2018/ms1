       SUBROUTINE APFSBR ( L , SIM , KSIM , KODFUN , K )
CXXX**-1KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)*************
C*                                                                     *
C*  èOÑèPOÉPAMMA :  APFSBR ( L , SIM , KSIM , KODFUN , K )             *
C*  ------------                                                       *
C*                                                                     *
C*  HAáHAóEHàE :    OTõCKAHàE àÑEHTàîàKATOPA àMEHà îìHKñàà B           *
C*  ----------      àHîàKCHOâ áAèàCà à ìCTAHOBãEHàE EMì B              *
C*                  COOTBETCTBàE èPàáHAKA îìHKñàà.                     *
C*                                                                     *
C*  METOÑ :         èOàCK COOTBETCTBàü B TAÅãàñE àMEH îìHKñàâ  KODFUN  *
C*  -----           ECãà TAKOE COOTBETCTBàE ECTú, TO CãEÑOBATEãúHO.    *
C*                  àMEEM îìHKñàû, A HE àÑEHTàîàKATOP.                 *
C*                                                                     *
C*  èAPAMETPõ :     L , L = 1  -  KOÑ  ->  OTPàñATEãúHõâ K O Ñ ,       *
C*  ---------           L = 2  -  OTPàñATEãúHõâ K O Ñ  ->  KOÑ .       *
C*                  SIM - MACCàB TEPMOB B àHîàKCHOM BàÑE .             *
C*                  KSIM - KOãàóECTBO ùãEMEHTOB B SIM .                *
C*                  KODFUN - MACCàB àMEH îìHKñàâ .                     *
C*                  K - óàCãO îìHKñàâ B KODFUN .                       *
C*                                                                     *
C*  èOÑèPOÉPAMMõ :  HET ; ( SUBERR(1) , COMMON : PRINTN )              *
C*  ------------                                                       *
C*                                                                     *
CZZZ********************************************************************
C*                                                                     *
       INTEGER SIM
       DIMENSION SIM(1) , KODFUN(1)
C-----------------------------------------------
       COMMON / PRINTN / NW6
C!!!-------------------------------------------------------------------
       GO TO ( 1 , 2 ) , L
C!!!-------------------------------------------------------------------
       WRITE(NW6,10) L
 10    FORMAT(1X,'***** CÅOâ èPà OÅPAôEHàà K APFSBR , L=',I10)
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
C*  èEPEKOÑàPOBKA àá èPOCTOÉO KOÑA B OTPàñATEãúHõâ KOÑ
 1     I = 1
C*  ECãà áHAK, TO OèìCKAEM EÉO
 4     IF ( SIM(I) .LT. 0 ) GO TO 11
C*  ECãà àÑEHTàîàKATOP àãà îìHKñàü , TO ...
       IF ( SIM(I) .GT. 0 ) GO TO 12
C*  àMEEM óàCãO. OèìCKAEM EÉO.
       I = I + 1
       GO TO 11
 12    CONTINUE
       DO 13 J=1,K
       IF ( SIM(I) .EQ. KODFUN(J) ) GO TO 23
       GO TO 13
C*  èEPEKOÑàPìEM îìHKñàû
 23    SIM(I) = -SIM(I)
       GO TO 11
 13    CONTINUE
 11    I = I + 1
       IF ( I .LE. KSIM ) GO TO 4
       RETURN
C*.....................................................................
C*  àá  OTPàñATEãúHõâ K O Ñ A  B èPOCTOâ KOÑ
 2     CONTINUE
       I = 1
 15    IF ( SIM(I) .GE. -100 ) GO TO 14
       SIM(I) = -SIM(I)
 14    I = I + 1
       IF ( I .LE. KSIM ) GO TO 15
       RETURN
       END
