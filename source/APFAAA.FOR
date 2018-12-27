       SUBROUTINE APFAAA ( KODFUN , KF , IFUN , NOMER )
CXXX****KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)*************
C*                                                                     *
C*  èOÑèPOÉPAMMA :  APFAAA ( KODFUN , KF , IFUN , NOMER )              *
C*  ------------                                                       *
C*                                                                     *
C*  HAáHAóEHàE :    OèPEÑEãEHàE èOPüÑKOBOÉO HOMEPA àMEHà îìHKñàà B     *
C*  ----------      TAÅãàñE àMEH îìHKñàâ.                              *
C*                                                                     *
C*  METOÑ :         èOCãEÑOBATEãúHõâ èPOCMOTP TAÅãàñõ àMEH îìHKñàâ.    *
C*  -----                                                              *
C*                                                                     *
C*  èAPAMETPõ :     KODFUN - TAÅãàñA PAáPEòEHHõX îìHKñàâ.              *
C*  ---------       KF - óàCãO îìHKñàâ.                                *
C*                  IFUN - àMü OèPEÑEãüEMOâ îìHKñàà.                   *
C*                  HOMEP - EE HOMEP B MACCàBE KODFUN.                 *
C*                                                                     *
C*  èOèPOÉPAMMõ :   HET ; ( SUBERR , COMMON : PRINTN )                 *
C*  -----------                                                        *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION KODFUN(1)
C!!!-------------------------------------------------------------------
       COMMON / PRINTN / NW6
C!!!-------------------------------------------------------------------
       DO 1 I=1,KF
       IKOD = -KODFUN(I)
       IF ( IFUN .NE. IKOD ) GO TO 1
       NOMER = I
       RETURN
 1     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NW6,10)
 10    FORMAT(1X,'***** CÅOâ B APFFFF . TAKOâ îìHKñàà HET .')
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
       RETURN
       END
