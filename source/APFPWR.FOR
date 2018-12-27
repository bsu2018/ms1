       SUBROUTINE  APFPWR ( K , SIM , RSIM , KSIM , M , II )
CXXX****KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)*************
C*                                                                     *
C*   èOÑèPOÉPAMMA :  APFPWR ( K , SIM , RSIM , KSIM , M , II )         *
C*   ------------                                                      *
C*                                                                     *
C*   HAáHAóEHàE :  BõÑEãEHàE TEPMA "BOáBEÑEHàE B CTEèEHú" à ìèAKOBKA   *
C*   ----------    àãà PACèAKOBKA EÉO. ìèAKOBAHHAü OèEPAñàü "BOáBEÑE-  *
C*                 HàE B CTEèEHú" áAHàMAET OÑHO CãOBO (INTEGER*4) à    *
C*                 àMEET KOÑ (-90). PACèAKOBAHHOE "**" - ÑBOâHOE       *
C*                 CãOBO, KOÑ  - CMOTPà èOÑèPOÉPAMMì APFTRT.           *
C*                                                                     *
C*   METOÑ :       èOCãEÑOBATEãúHOE CKAHàPOBAHàE MACCàBA  SIM  à       *
C*   -----         èOàCK ìèAKOBAHHOÉO àãà PACèAKOBAHHOÉO áHAóEHàü "**".*
C*                                                                     *
C*   èAPAMETPõ :   K = 1  -  PACèAKOBAHHOE -> ìèAKOBAHHOE              *
C*   ---------     K = 2  -  ìèAKOBAHHOE   -> PACèAKOBAHHOE            *
C*                 SIM - MACCàB KOTOPõâ HEOÅXOÑàMO èPOCMOTPETú         *
C*                 RSIM - TO ÜE óTO à SIM , HO REAL*4                  *
C*                 KSIM - KOãàóECTBO ùãEMEHTOB B SIM                   *
C*                 M - KOÑ ìèAKOBAHHOÉO "**"                           *
C*                 II - KOÑ CàMBOãA "*"                                *
C*                                                                     *
C*   èOÑèPOÉPAMMõ : HET , ( SUBERR , COMMON / PRINTN / NW6 )           *
C*   ------------                                                      *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION SIM(1) , RSIM(1)
       INTEGER SIM
C!!!-------------------------------------------------------------------
       COMMON / PRINT / NW6
C!!!-------------------------------------------------------------------
       I = 1
       GO TO ( 1 , 2 ) , K
C!!!-------------------------------------------------------------------
       WRITE ( NW6 , 10 ) K
 10    FORMAT ( ' ***** CÅOâ èPà OÅPAôEHàà K  APFPWR ',I10 )
       CALL SUBERR ( 1 )
C!!!-------------------------------------------------------------------
C*   ìèAKOBKA  **  B OÑHO CãOBO
 1     CONTINUE
 4     IF ( SIM(I) .NE. II ) GO TO 3
       IF ( SIM(I+1) .NE. II ) GO TO 3
       SIM(I) = M
       KSIM = KSIM - 1
       I10 = I + 1
       DO 7 J=I10,KSIM
       SIM(J) = SIM(J+1)
 7     RSIM(J) = RSIM(J+1)
 3     I = I + 1
       IF ( I .LE. KSIM ) GO TO 4
       RETURN
C*   PACèAKOBKA  **  àá CãOBA B ÑBOâHOE CãOBO
C*
 2     IF ( SIM(I) .NE. M ) GO TO 5
       SIM(I) = II
       KSIM = KSIM + 1
       J = KSIM
 6     SIM(J) = SIM(J-1)
       J = J - 1
       IF ( J .GT. I ) GO TO 6
 5     I = I + 1
       IF ( I .LE. KSIM ) GO TO 2
       RETURN
       END
