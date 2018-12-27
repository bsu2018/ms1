       SUBROUTINE ADECDQ( KD, KQ, IER )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT    MAŸ    1988 ƒ.)-----------
C   O„POƒPAMMA: ADECDQ( KD, KQ, IER )
C   -------------
C   HA‡HA—EHˆE:  POƒPAMMA EPEBO„ˆT –E‹OE „BOˆ—HOE —ˆC‹O  KD ,
C   ----------   ‡A„AHHOE B "10" ”OPME B "8"  ”OPM“.
C
C   METO„:         EPEKO„ˆPOBKA.
C   -----
C
C   APAMETP›:  BXO„H›E :
C  ----------      KD  - ‡A„AHHOE „BOˆ—HOE —ˆC‹O B "10# ”OPME;
C
C               B›XO„H›E :
C                  KQ  - PEOPA‡OBAHHOE „BOˆ—HOE —ˆC‹O B "8"
C                        ”OPM“ ;
C                  IER - ”‹Aƒ O˜ˆKˆ:
C                        IER=0 - O˜ˆOK HET;
C                        IER=1 - ‡A„AHHOE BOCœMˆPˆ—HOE —ˆC‹O O‹œ˜E
C                                —ˆC‹A "177777", —TO COOTBETCTB“ET
C                                MAKCˆMA‹œHOM“ —ˆC‹“ 16-Tˆ PA‡PŸ„HOƒO
C                                „BOˆ—HOƒO —ˆC‹A .
C
C   O„POƒPAMM›:  1. HET.
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HEPABˆ‹œHOCTˆ ‡AˆCˆ BOCœMˆPˆ—HOƒO
C   -----------       —ˆC‹A HE POˆ‡BO„ˆTCŸ.
C
CZZZ-------------------------------------------------------------------
       IER = 0
       IF( KD .GT. 65535  ) GO TO 9999
C--> PA‡OœEM O PA‡PŸ„AM.
       KD5 = KD / 32768
       KDP = KD  - KD5 * 32768
       KD4 = KDP/ 4096
       KDP = KDP - KD4 * 4096
       KD3 = KDP/ 512
       KDP = KDP - KD3 * 512
       KD2 = KDP/ 64
       KDP = KDP - KD2 * 64
       KD1 = KDP/ 8
       KD0 = KDP - KD1 * 8
C--> PA‡ˆ‹ˆ O PA‡PŸ„AM, TEEPœ HA‰„EM „ECŸTˆ—HOE —ˆC‹O.
       KQ = KD0 + 10*KD1 + 100*KD2 + 1000*KD3 + 10000*KD4 + 100000*KD5
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
       IER = 1
       RETURN
       END
