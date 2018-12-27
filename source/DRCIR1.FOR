         SUBROUTINE DRCIR1(BA0, KNOT, PAR0, IRC)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT ABÉìCTA 1987 É.)------------
C   èOÑèPOÉPAMMA:   DRCIR1(BA0, KNOT, PAR0, IRC)
C   ------------
C   HAáHAóEHàE:    COPTàPOBKA BA0 à OèPEÑEãEHàE PAáMEPA MSW_ALL
C   ----------
C   METOÑ:         COPTàPOBKA - CM. è/è TRIARB, èO èPàáHAKì TàèA MOÑEãà;
C   -----          PAáMEP MSWALL OèPEÑEãüETCü èO èOãüM MACTAB(8)
C                  èPà ùTOM:   +5 CãOB HA ÑàCKPàèTOP,
C                              +1*NXBA0 - HA ìKAáATEãà CãEÑ. MSW OèàCAHà
C                               (NXBA0 - KOãàóECTBO ùãEMEHTOB ñEèà)
C                               +1 CãOBO HA ìKAáATEãú KOHñA CèàCKA;
C                              èOÑPOÅHEE CM. CTPìKTìPì BA0 B è/è INPUT0;
C                 + LM = MACTAB(...+7) CãOB HA KAÜÑõâ ùãEMEHT àá BA0;
C                   ***  èPà  MACTAB(...+7) = +1(VAR) - áAèPOC
C                        óEPEá CALLFM èPà BOáÅìÜÑEHHOM îãAÉE FGMSW
C                        HA ìTOóHEHàE BCEâ ÑãàHõ MSW_ALL ÑAHHOÉO
C                        ùãEMEHTA.    ****
C
C
C   èAPAMETPõ:     BA0, KNOT, PAR0 - MACCàBõ OèàCAHàü ùãEMEHTOB;
C   ---------      IRC = 1 - áAÑAHàE BõèOãHàãà;
C                      = 2 - BOáMOÜHO TOãúKO èPà FRCPU = 0
C                            "AèèAPAT" REGION OTKãûóEH (CM. èPàMEóAHàE);
C
C   èOÑèPOÉPAMMõ:  1. COMMON PROF1, PROF2
C   ------------   2. *** SORTBA ******  ( OT ABÉìCTA 1987 É. )
C                  3.  CALLFM.
C   èPàMEóAHàE:    1. èPà FRCPU = 1 [MASPRT(73)] OèPEÑEãEHHõâ
C  -----------        PAáMEP MSWALL áAèàCõBAETCü B DMSWAL [MASPRT(20)]
C                     èPà FRCPU = 0 (CTATàóECKOE BõÑEãEHàE O.è.) -
C                     èPOBOÑàTCü CPABHEHàE HEOÅXOÑàMOÉO Ñãü ÑAãúHEâòEâ
C                     PAÅOTõ PAáMEPA MSWALL à áHAóEHàü DMSWAL àá MASPRT:
C                     B CãìóAE HE XBATKà O.è. ìCTAHABãàBAEàCü IRC=2 à
C                     ÉEHEPàPìETCü COOÅôEHàE;
C                   2.  !! CTPAXOBKA !! -->  èOãû NSYS èPàCBAàBAETCü
C                                            áHAóEHàE NMKNOT
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/PROF2/ MACTAB(800)
         INTEGER BA0, BA0CPL, AKKK, DMSWAL, FRCPU, FGMSW, TIP
         DIMENSION BA0(*), MSWT(50), KNOT(*), PAR0(*)
         EXTERNAL BA0CPL, BA0CNG
         EQUIVALENCE ( DMSWAL, MASPRT(20) )
         EQUIVALENCE ( FGMSW,  MASPRT(87) )
         EQUIVALENCE ( NMKNOT, MASPRT(7) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NXBA0,   MASPRT(3)  )
         EQUIVALENCE ( N1BA0,    MASPRT(4)  )
         EQUIVALENCE ( FRCPU,    MASPRT(73)  )
         EQUIVALENCE ( N1MACT,   MASPRT(17)  )
         EQUIVALENCE ( NSYS,     MASPRT(26)  )
         NSYS = NMKNOT
         IRC = 1
C--> SORT....
C!!!-------------------------------------------------------------------
C      CALL PRINT0( BA0, KNOT, PAR0)
C!!!-------------------------------------------------------------------
         CALL SORTBA(  BA0  )
C!!!-------------------------------------------------------------------
C      CALL PRINT0( BA0, KNOT, PAR0)
C!!!-------------------------------------------------------------------
C--->  OèPEÑEãüEM PAáMEP MSW_ALL
         MMSW = 5 + 1 * NXBA0 + 1
C-->  èOàCK èO MACTAB Ñãü BCEX ùãEMEHTOB:
         I0 = 6
         IMAX=BA0(3)
         DO 1 KKK = 1, NXBA0
C->  AÑPEC ùãEMEHTA KKK B BA0:
         AKKK = (KKK-1)*N1BA0 + I0
C-->  KOHTPOãú
         IF( AKKK+N1BA0  .GT.  IMAX ) CALL SUBERR(AKKK)
C-->  CMOTPàM MACTAB
         TIP = BA0(AKKK+1)
         IX = 1
4        CONTINUE
         IF(MACTAB(IX) .EQ. 0 ) GO TO 2
         IF(MACTAB(IX) .EQ. TIP ) GO TO 3
         IX = IX + N1MACT
         GO TO 4
2        CONTINUE
C-->  ãOÉ. OòàÅKA, TàèA HET...
         CALL SUBERR(TIP)
3        CONTINUE
C--> IX HAòA CTPOKA
C-->  KOHTPOãú
         IF ( MACTAB(IX+7) .LT. 0 ) CALL SUBERR(IX)
         IF ( MACTAB(IX+7) .EQ. +1 ) GO TO 60
C--->    ùãEMEHT HE C LM  -->  +1(VAR)
C ==> CóàTAEM
         MMSW = MMSW + MACTAB(IX+7)
       GO TO 1
C######################################################################
60     CONTINUE
C-->  ìTOóHàM LM.....
       FGMSW = 1
C      *********
C  AKKK - AÑPEC OèàCAHàü ùãEMEHTA B BA0, IKNOT à IPAR0 -> B KNOT à PAR0:
       IKNOT = BA0( AKKK+2 )
       IPAR0 = BA0( AKKK+3 )
C--->  HOMEP FM MOHàTOPA:
       NFM = MACTAB( IX+6 ) / 1000
C-->  ìTOóHàM
       LM = 10
       CALL CALLFM( NFM, BA0(AKKK), KNOT(IKNOT), PAR0(IPAR0), MSWT, LM )
C--> CÅPOCàM îãAÜEK...
       FGMSW = 0
C-->  CóàTAEM
       MMSW = MMSW + LM
C######################################################################
1        CONTINUE
C######################################################################
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 51) MMSW
51       FORMAT(' *** Ñ´Ô ·Ø®·™† MSW_ALL ≠•Æ°ÂÆ§®¨Æ ', I5, ' ·´Æ¢ O.è. *
     =**')
C!!!-------------------------------------------------------------------
C--->  óTO ì HAC ?
         IF ( FRCPU .EQ. 0 ) GO TO 5
C--> "APPARAT" REGION BKûóEH....
         DMSWAL = MMSW
         IRC = 1
         RETURN
C**********************************************************************
5        CONTINUE
         IRC = 1
C-->  CTATàóECKà BõÑEãàãà ÑOCTATOóHO ??
         IF ( DMSWAL.GE. MMSW ) RETURN
         IRC = 2
C-->  HET O.è.
C!!!-------------------------------------------------------------------
         IZZ = MMSW - DMSWAL
         WRITE(NWTRE, 50) IZZ, DMSWAL
50      FORMAT(/' *** Ñ´Ô MSW_ALL ≠•Â¢†‚†•‚ ', I5, ' ·´Æ¢ O.è. ***'/
     =  ' *** ®ß≠†Á†´‚≠Æ ØÆ§ MSW_ALL ¢Î§•´•≠Æ ', I5, ' ·´Æ¢ O.è. ***'/
     = ' *** ‡†°Æ‚† ¢®§®¨Æ °„§•‚ Ø‡•‡¢†≠† ***'///)
C!!!-------------------------------------------------------------------
         RETURN
         END
