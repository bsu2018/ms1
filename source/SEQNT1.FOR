         SUBROUTINE SEQNT1( TIP, KTIP, NSM, KNSM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:   SEQNT1( TIP, KTIP, NSM, KNSM )
C   ------------
C   HAáHAóEHàE:    èO HOMEPì TàèA ùãEMEHTA TIP à èO "CMEôEHàû"  NSM
C   ----------          H A X O Ñ à T :
C                  AÑPEC àMEHà TàèA MOÑEãà B TABTIP  ==> KTIP  à
C                  AÑPEC àMEHà èEPEMEHHOâ B TABTOK   ==> KNSM
C
C   METOÑ:         èOàCK èO TAÅãàñAM SERVZ  ÅãOKOB;
C   ------
C
C   èAPAMETPõ:     CM. HAáHAóEHàE;
C   ----------
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, SERVZ1, 2, 3
C   ------------
C   èPàMEóAHàE:    1. KOÑõ àMEH îàKCàPOBAHõ COÉãACHO SERVZ TAÅãàñ;
C  -----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /SERVZ1/ TABSPC(1)
         COMMON /SERVZ2/          TABTOK(1)
         COMMON /SERVZ3/          TABTIP(1)
         INTEGER TABSPC, TIP, TIPX, TABTOK, TABTIP
         KTIP = (13-1)* 5      + 1
         KNSM = (13-1)* 3      + 1
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
         KODN = TABSPC(IX+1)
         KTIP = (KODN-1) * 5     + 1
C--> èO NSM KOÑ àMEHà èEPEMEHHOâ KODP àôEM...
4        CONTINUE
         IF ( TABSPC(IX) .EQ. 0 ) RETURN
         IF ( TABSPC(IX) .NE. TIPX ) RETURN
         NS1 = TABSPC(IX+2)
         NS2 = TABSPC(IX+3)
C--->  HAòE CMEôEHàE MEÜÑì ?
         IF ( NS1 .LE. NSM  .AND.  NSM .LE. NS2 ) GO TO 5
C-->  èOèPOÅìEM B ÑP. CTPOKE...
         IX = IX + 5
         GO TO 4
5        CONTINUE
C-->  CTPOKì HAòãà, BõÅOP KOÑA àMEHà "TOKA":
         KODP = TABSPC(IX+4)
         KNSM = (KODP-1) * 3     + 1
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
