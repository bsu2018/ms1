         SUBROUTINE SEQNT0( TIP, KOD, NSM, IRC )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:   SEQNT0( TIP, KOD, NSM, IRC )
C   ------------
C   HAáHAóEHàE:    èO HOMEPì TàèA ùãEMEHTA TIP OèPEÑEãüET
C   -----------    CMEôEHàE NSM, COOTBETCTBìûôEE KOÑì
C                  àMEHà èEPEMEHHOâ KOD;
C   METOÑ:         èOàCK èO TAÅãàñE TABSPC;
C   ------
C
C   èAPAMETPõ:     TIP - Tàè MOÑEãà
C   ----------     KOD - KOÑ àMEHà èEPEMEHHOâ;
C                  NSM - "CMEôEHàE";
C                  IRC = 1  HAòãà,
C                  IRC = 2  OòàÅKA;
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, SERVZ1
C   ------------
C   èPàMEóAHàE:    1. KOÑõ àMEH îàKCàPOBAHõ COÉãACHO SERVZ TAÅãàñ;
C  -----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /SERVZ1/ TABSPC(1)
         INTEGER TABSPC, TIP, TIPX
         IRC = 2
         IX = 1
         TIPX = TIP
C--> èO TABSPC....
2        CONTINUE
         IF ( TABSPC(IX) .EQ. TIPX ) GO TO 1
         IF ( TABSPC(IX) .EQ. 0    ) RETURN
         IX = IX + 5
         GO TO 2
1        CONTINUE
C--> HET ãà èEPEAÑPECAñàà?
C--->   IX HAòA CTPOKA
         IF ( TABSPC(IX+1) .GT. 0 ) GO TO 3
C--->   èEPEAÑPECìEM à èOàôEM...
         TIPX = - TABSPC(IX+1)
         IX = 1
         GO TO 2
3        CONTINUE
C--->  èOàCK B CTPOKE
         IRC = 2
         IF ( TABSPC(IX+4) .EQ. KOD ) GO TO 4
C-->  MOÜET ÅõTú B ÑP. CTPOKE
         IX = IX + 5
         IF ( TABSPC(IX) .NE. TIPX ) RETURN
         IF ( TABSPC(IX) .EQ. 0 ) RETURN
         GO TO 3
4        CONTINUE
         IRC = 1
C-->  CTPOKì HAòãà, BõÅOP CMEôEHàü "OT"
         NSM = TABSPC(IX+2)
C--->  KOHTPOãú
         IF ( NSM. EQ. 1 ) RETURN
C!!!-------------------------------------------------------------------
         IF ( TABSPC(IX+2) .EQ. TABSPC(IX+3) ) RETURN
         NWTRE=MASPRT(14)
         WRITE(NWTRE,5) IX
5        FORMAT(' *** B TAÅãàñE TABSPC CTPOKA ', I4, ' áAèOãHEHA ',
     =   'HE KOPPEKTHO ***')
C!!!-------------------------------------------------------------------
         RETURN
         END
