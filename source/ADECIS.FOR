       SUBROUTINE ADECIS( KD, ADPROG, REG, IADRS, IDATA, T, IER )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT    MAŸ    1988 ƒ.)-----------
C   O„POƒPAMMA: ADECIS( KD, ADPROG, REG, IADRS, IDATA, T, IER )
C   -------------
C   HA‡HA—EHˆE:  OˆCK O KO„“ A„PECA–ˆˆ  KD ( „B“XPA‡PŸ„HOE Q-—ˆC‹O )
C   ----------   A„PECA ˆ „AHHOƒO B O‡“  ˆ‹ˆ B  POH .
C                ( „‹Ÿ KOMAH„, PAOTAž™ˆX CO C‹OBAMˆ )
C                ******************************************************
C
C   METO„:         EPEKO„ˆPOBKA, OPE„E‹EHˆE METO„A A„PECA–ˆˆ ˆ OˆCK.
C   -----
C
C   APAMETP›:  BXO„H›E :
C  ----------      KD     - KO„ A„PECA–ˆˆ ;
C                  REG(8) - MACCˆB PEƒˆCTPOB MˆKPOPO–ECCOPA ;
C
C               B›XO„H›E :
C                  IADRS  - A„PEC  ˆCKOMOƒO „AHHOƒO :
C                           EC‹ˆ IADRS >= 0 - A„PEC Ÿ—E‰Kˆ AMŸTˆ;
C                           EC‹ˆ IADRS <  0 - A„PEC PEƒˆCTPA .
C                                             ‹ˆO A„PEC OPTA B/B .
C                  IDATA  - —ˆC‹O, XPAHŸ™EECŸ O A„PEC“ 'IADRS';
C                  T      - BPEMŸ B›O‹HEHˆŸ KOMAH„› ;
C                  IER    - “KA‡ATE‹œ O˜ˆKˆ:
C                           IER=0  -  O˜ˆOK HE OHAP“†EHO ;
C                           IER=1  -  O˜ˆO—H›‰ KO„ A„PECA–ˆˆ ,
C                                     ‹ˆO B›XO„ ‡A PE„E‹› O‡“ ;
C                           IER=2  -  O‹“—EHH›‰ A„PEC Ÿ—E‰Kˆ AMŸTˆ
C                                     'IADRS' HE —ETH›‰ .
C
C   O„POƒPAMM›:  1. HET.
C   ------------
C
C   PˆME—AHˆE:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), ADPROG
       IER = 0
C--> KAKO‰ METO„ A„PECA–ˆˆ ?
       METOD = KD/8 + 1
       NREG  = KD - 8*(METOD - 1) + 1
       GO TO (100, 200, 300, 400, 500, 600, 700, 800 ), METOD
       GO TO 9990
C######################################################################
C
C               PŸMAŸ  A„PECA–ˆŸ
C
C######################################################################
100    CONTINUE
C--> PEƒˆCTPOB›‰ METO„ A„PECA–ˆˆ.
C----------------------------------------------------------------------
       IADRS = - NREG
       IDATA = REG(NREG)
       T = 0.
       RETURN
C----------------------------------------------------------------------
300    CONTINUE
C--> ABTOˆHKPEMEHTH›‰ METO„ A„PECA–ˆˆ
C----------------------------------------------------------------------
       IADRS = REG(NREG)
       T = 1.6E-6
       REG(NREG) = REG(NREG) + 2
       GO TO 900
C----------------------------------------------------------------------
500    CONTINUE
C--> ABTO„EKPEMEHTH›‰ METO„ A„PECA–ˆˆ
C----------------------------------------------------------------------
       REG(NREG) = REG(NREG) - 2
       IADRS = REG(NREG)
       T = 2.4E-6
       GO TO 900
C----------------------------------------------------------------------
700    CONTINUE
C--> ˆH„EKCH›‰ METO„ A„PECA–ˆˆ
C----------------------------------------------------------------------
       INDEX = MEM( REG(8) + 1)
       REG(8) = REG(8) + 2
       IADRS = REG(NREG) + INDEX
       T = 4.8E-6
       GO TO 900
C######################################################################
C
C            KOCBEHHAŸ  A„PECA–ˆŸ
C
C######################################################################
200    CONTINUE
C--> KOCBEHHO-PEƒˆCTPOB›‰ METO„ A„PECA–ˆˆ
C----------------------------------------------------------------------
       IADRS = REG(NREG)
       T = 1.6E-6
       GO TO 900
C----------------------------------------------------------------------
400    CONTINUE
C--> KOCBEHHO-ABTOˆHKPEMEHTH›‰ METO„ A„PECA–ˆˆ
C----------------------------------------------------------------------
       IADRS = MEM( REG(NREG)+1)
       T = 4.0E-6
       REG(NREG) = REG(NREG) + 2
       GO TO 900
C----------------------------------------------------------------------
600    CONTINUE
C--> KOCBEHHO-ABTO„EKPEMEHTH›‰ METO„ A„PECA–ˆˆ
C----------------------------------------------------------------------
       REG(NREG) = REG(NREG) - 2
       IADRS = MEM( REG(NREG)+1)
       T = 4.8E-6
       GO TO 900
C----------------------------------------------------------------------
800    CONTINUE
C--> KOCBEHHO-ˆH„EKCH›‰ METO„ A„PECA–ˆˆ
C----------------------------------------------------------------------
       INDEX = MEM( REG(8) + 1)
       REG(8) = REG(8) + 2
       IAD   =  REG(NREG)  + INDEX
       IADRS = MEM(IAD+1)
       T = 7.2E-6
C----------------------------------------------------------------------
900    CONTINUE
C--> POBEPˆM A„PEC ˆ ‡AOMHˆM „AHHOE.
       IF( IADRS .GE. 57344  .AND. IADRS .LT. 65536 ) GO TO 8888
       IF( IADRS .LT. 0      .OR.  IADRS .GT. ADPROG) GO TO 9990
       IDATA = MEM(IADRS+1)
       IF( MOD(IADRS,2) .GT. 0 ) GO TO 9999
       RETURN
C----------------------------------------------------------------------
8888   CONTINUE
C--> OPATˆ‹ˆCœ K OPT“ B/B.
       IADRS = - IADRS
       RETURN
C----------------------------------------------------------------------
9990   CONTINUE
       IER = 1
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
C--> CE–ˆA‹œHO „‹Ÿ KOMAH„ , PAOTAž™ˆX C A‰TAMˆ
       IDATA = MEM(IADRS)
       IADRS = IADRS - 1
       IER = 2
       RETURN
C----------------------------------------------------------------------
       END
