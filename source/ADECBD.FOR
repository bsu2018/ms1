       SUBROUTINE ADECBD( KD, KB, IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT   ABÉìCT  1988 É.)-----------
C   èOÑèPOÉPAMMA: ADECBD( KD, KB, IER )
C   -------------
C   HAáHAóEHàE:  èPOÉPAMMA èPEOÅPAáìET ÑBOâóHOE óàCãO B ÑECüTàóHOE
C   ----------
C
C   METOÑ:         èEPEKOÑàPOBKA.
C   -----
C
C
C   èAPAMETPõ:  BXOÑHõE :
C   ---------    KB(1) - PAáPüÑ "0" ÑBOàóHOÉO óàCãA ;
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
C
C              BõXOÑHõE:
C                 KD  -   èOãìóEHHOE ÑECüTàóHOE óàCãO   ;
C                  IER - îãAÉ OòàÅKà:
C                        IER=0 - OòàÅOK HET;
C                        IER=1 - PAáPüÑ ÑBOàóHOÉO óàCãA àMEET
C                                HEÑOèìCTàMOE áHAóEHàE ;
C
C
C
C   èOÑèPOÉPAMMõ:  1. HET.
C   ------------
C
C   èPàMEóAHàE:    1. HET.
C   -----------
CZZZ-------------------------------------------------------------------
       INTEGER KB(16)
       IER = 0
C--> CKãAÑõBAEM èO PAáPüÑAM.
       KD = 0
       DO 100 I=1,16
       I1 = 2**(I-1)
       IF( KB(I) .GT. 1 ) IER = 1
       KD = KD + KB(I) * I1
100    CONTINUE
       RETURN
       END
