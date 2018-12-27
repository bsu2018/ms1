       SUBROUTINE ADECQD( KQ, KD, KQ0, KQ1, KQ2, KQ3, KQ4, KQ5, IER )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT    MAŸ    1988 ƒ.)-----------
C   O„POƒPAMMA: ADECQD( KQ, KD, KQ0, KQ1, KQ2, KQ3, KQ4, KQ5, IER )
C   -------------
C   HA‡HA—EHˆE:  POƒPAMMA EPEBO„ˆT –E‹OE „BOˆ—HOE —ˆC‹O  KQ ,
C   ----------   ‡A„AHHOE B BOCœMˆPˆ—HO‰ ”OPME B „ECŸTˆ—H“ž ”OPM“
C                C O„HOBPEMEHHO‰ OPA‡PŸ„HO‰ PA‡ˆBKO‰ ‡A„AHHOƒO
C                BOCœMˆPˆ—HOƒO —ˆC‹A.
C
C   METO„:         EPEKO„ˆPOBKA.
C   -----
C
C   APAMETP›:  BXO„H›E :
C  ----------      KQ  - ‡A„AHHOE „BOˆ—HOE —ˆC‹O B BOCœMˆPˆ—HO‰ ”OPME;
C
C               B›XO„H›E :
C                  KD  - PEOPA‡OBAHHOE „BOˆ—HOE —ˆC‹O B „ECŸTˆ—H“ž
C                        ”OPM“ ;
C                  KQ0 - PA‡PŸ„ "0" BOCœMˆPˆ—HOƒO —ˆC‹A ;
C                  KQ1 - PA‡PŸ„ "1" BOCœMˆPˆ—HOƒO —ˆC‹A ;
C                  KQ2 - PA‡PŸ„ "2" BOCœMˆPˆ—HOƒO —ˆC‹A ;
C                  KQ3 - PA‡PŸ„ "3" BOCœMˆPˆ—HOƒO —ˆC‹A ;
C                  KQ4 - PA‡PŸ„ "4" BOCœMˆPˆ—HOƒO —ˆC‹A ;
C                  KQ5 - PA‡PŸ„ "5" BOCœMˆPˆ—HOƒO —ˆC‹A ;
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
       IF( KQ .GT. 177777 ) GO TO 9999
C--> PA‡OœEM O PA‡PŸ„AM.
       KQ5 = KQ / 100000
       KQP = KQ  - KQ5 * 100000
       KQ4 = KQP/ 10000
       KQP = KQP - KQ4 * 10000
       KQ3 = KQP/ 1000
       KQP = KQP - KQ3 * 1000
       KQ2 = KQP/ 100
       KQP = KQP - KQ2 * 100
       KQ1 = KQP/ 10
       KQ0 = KQP - KQ1 * 10
C--> PA‡ˆ‹ˆ O PA‡PŸ„AM, TEEPœ HA‰„EM „ECŸTˆ—HOE —ˆC‹O.
       KD = KQ0 + 8*KQ1 + 64*KQ2 + 512*KQ3 + 4096*KQ4 + 32768*KQ5
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
       IER = 1
       RETURN
       END
