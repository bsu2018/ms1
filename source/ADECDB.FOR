       SUBROUTINE ADECDB( KD, KB, IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT    MAü    1988 É.)-----------
C   èOÑèPOÉPAMMA: ADECDB( KD, KB, IER )
C   -------------
C   HAáHAóEHàE:  èPOÉPAMMA PAáÅàBAET ñEãOE ÑECüTàóHOE óàCãO  KD ,
C   ----------   èO CTEèEHüM ÑBOâKà .
C
C   METOÑ:         èEPEKOÑàPOBKA.
C   -----
C
C   èAPAMETPõ:  BXOÑHõE :
C  ----------      KD  - áAÑAHHOE ÑECüTàóHOE óàCãO ;
C
C               BõXOÑHõE :
C                KB(1) - PAáPüÑ "0" ÑBOàóHOÉO óàCãA ;
C                KB(2) - PAáPüÑ "1" ÑBOàóHOÉO óàCãA ;
C                KB(3) - PAáPüÑ "2" ÑBOàóHOÉO óàCãA ;
C                KB(4) - PAáPüÑ "3" ÑBOàóHOÉO óàCãA ;
C                KB(5) - PAáPüÑ "4" ÑBOàóHOÉO óàCãA ;
C                KB(6) - PAáPüÑ "5" ÑBOàóHOÉO óàCãA ;
C                KB(7) - PAáPüÑ "6" ÑBOàóHOÉO óàCãA ;
C                KB(8) - PAáPüÑ "7" ÑBOàóHOÉO óàCãA ;
C                KB(9) - PAáPüÑ "8" ÑBOàóHOÉO óàCãA ;
C                KB(10)- PAáPüÑ "9" ÑBOàóHOÉO óàCãA ;
C                KB(11)- PAáPüÑ "10" ÑBOàóHOÉO óàCãA ;
C                KB(12)- PAáPüÑ "11" ÑBOàóHOÉO óàCãA ;
C                KB(13)- PAáPüÑ "12" ÑBOàóHOÉO óàCãA ;
C                KB(14)- PAáPüÑ "13" ÑBOàóHOÉO óàCãA ;
C                KB(15)- PAáPüÑ "14" ÑBOàóHOÉO óàCãA ;
C                KB(16)- PAáPüÑ "15" ÑBOàóHOÉO óàCãA ;
C                  IER - îãAÉ OòàÅKà:
C                        IER=0 - OòàÅOK HET;
C                        IER=1 - áAÑAHHOE ÑECüTàóHOE óàCãO ÅOãúòE
C                                óàCãA "65535", óTO COOTBETCTBìET
C                                MAKCàMAãúHOMì óàCãì 16-Tà PAáPüÑHOÉO
C                                ÑBOàóHOÉO óàCãA .
C
C   èOÑèPOÉPAMMõ:  1. HET.
C   ------------
C
C   èPàMEóAHàE:    1. HET.
C   -----------
CZZZ-------------------------------------------------------------------
       INTEGER KB(16)
       IER = 0
       IF( KD .GT. 65535 ) GO TO 9999
C--> PAáOÅúEM èO PAáPüÑAM.
       KDP = KD
       DO 100 I=1,16
       I1 = 17 - I
       I2 = 2**(I1-1)
       KB(I1) = KDP / I2
       KDP = KDP - KB(I1) * I2
100    CONTINUE
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
       IER = 1
       RETURN
       END
