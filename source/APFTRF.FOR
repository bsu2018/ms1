       SUBROUTINE APFTRF ( KODFUN , K )
CXXX****KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)*************
C*                                                                     *
C*  èOÑèPOÉPAMMA :  APFTRF ( KODFUN , K )                              *
C*  ------------                                                       *
C*                                                                     *
C*  HAáHAóEHàE :    èEPEKOÑàPOBKA àá CàMBOãúHOÉO èPEÑCTABãEHàü         *
C*  ----------      (A-îOPMAT) BO BHìTPEHHEE èPEÑCTABãEHàE èPOÉPAMMõ   *
C*                  (F-îOPMAT,INTEGER*4) à èOCãEÑìûôAü ìèAKOBKA        *
C*                  àMEH îìHKñàâ. àMEHA îìHKñàâ áAòàTõ B               *
C*                  DATA - ÅãOKE èOÑèPOÉPAMMõ.                         *
C*                                                                     *
C*  METOÑ :         èOCàMBOãúHAü èEPEKOÑàPOBKA à ìèAKOBKA àMEHà.       *
C*  -----                                                              *
C*                                                                     *
C*  èAPAMETPõ :     KODFUN - MACCàB èEPEKOÑàPOBAHHõX à ìèAKOBAHHõX     *
C*  ---------                àMEH îìHKñàâ.                             *
C*                  K - KOãàóECTBO îìHKñàâ.                            *
C*                                                                     *
C*  èOÑèPOÉPAMMõ :  APFTRT , APFPAK                                    *
C*  ------------                                                       *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION KODFUN(1) , CHAR(4) , IFUN(4,12)
       INTEGER CHAR
       DATA      IFUN / 'L' , 'O' , 'G' , ' ' ,
     *                  'L' , 'G' , '1' , '0' ,
     *                  'E' , 'X' , 'P' , ' ' ,
     *                  'S' , 'Q' , 'R' , 'T' ,
     *                  'S' , 'I' , 'N' , ' ' ,
     *                  'C' , 'O' , 'S' , ' ' ,
     *                  'T' , 'A' , 'N' , ' ' ,
     *                  'C' , 'T' , 'A' , 'N' ,
     *                  'A' , 'S' , 'I' , 'N' ,
     *                  'A' , 'C' , 'O' , 'S' ,
     *                  'A' , 'T' , 'A' , 'N' ,
     *                  'A' , 'B' , 'S' , ' '    /
C*.....................................................................
       K = 12
       DO 1 I=1,K
       CHAR(1) = IFUN(1,I)
       CHAR(2) = IFUN(2,I)
       CHAR(3) = IFUN(3,I)
       CHAR(4) = IFUN(4,I)
       CALL APFTRT ( 1 , 4 , CHAR , CHAR )
       CALL APFPAK ( 1 , KODPAC , CHAR )
 1     KODFUN(I) = KODPAC
       RETURN
       END
