       SUBROUTINE ADEC07( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1, T, IER )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT SEPTEMBER 1989 ƒ.)-----------
C   O„POƒPAMMA: ADEC07( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1 , T, IER )
C
C   HA‡HA—EHˆE:  OPAOT—ˆK  KOMAH„
C   ----------   MˆKPOPO–ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH„A  "FADD" ( 07500R ):                          *
C              * C‹O†EHˆE C ‹ABAž™E‰ ‡AŸTO‰ .                       *
C              *------------------------------------------------------*
C              * KOMAH„A  "FSUB"  (07501R ):                          *
C              * B›—ˆTAHˆE C ‹ABAž™E‰ ‡AŸTO‰ .                      *
C              *------------------------------------------------------*
C              * KOMAH„A  "FMUL"  (07502R ):                          *
C              * “MHO†EHˆE C ‹ABAž™E‰ ‡AŸTO‰ .                      *
C              *------------------------------------------------------*
C              * KOMAH„A  "FDIV"  (07503R ):                          *
C              * „E‹EHˆE  C ‹ABAž™E‰ ‡AŸTO‰ .                       *
C              *------------------------------------------------------*
C              * KOMAH„A  "SOB"   (077RNN ):                          *
C              * B›—ˆTAHˆE E„EHˆ–› ˆ BETB‹EHˆE .                      *
C              *------------------------------------------------------*
C
C   METO„:      ‹Oƒˆ—ECKOE MO„E‹ˆPOBAHˆE.
C   -----
C
C   APAMETP›:  BXO„H›E :
C  ----------      KD     - KO„ KOMAH„›  ;
C                  REG(8) - MACCˆB PEƒˆCTPOB MˆKPOPO–ECCOPA ;
C                           REG(1) --> R0
C                           REG(2) --> R1
C                           REG(3) --> R2
C                           REG(4) --> R3
C                           REG(5) --> R4
C                           REG(6) --> R5
C                           REG(7) --> R6 ( “KA‡ATE‹œ CTEKA )
C                           REG(8) --> R7 ( C—ET—ˆK KOMAH„  )
C                  FLAG(6)- MACCˆB ”‹AƒOB CC MˆKPOPO–ECCOPA ;
C                           FLAG(1) --> ”‹Aƒ  C (EPEHOC)
C                           FLAG(2) --> ”‹Aƒ  V (APˆ”M. EPEO‹HEHˆE)
C                           FLAG(3) --> ”‹Aƒ  Z (H“‹EBO‰ PE‡“‹œTAT)
C                           FLAG(4) --> ”‹Aƒ  N (OTPˆ–AT.PE‡“‹œTAT)
C                           FLAG(5) --> ”‹Aƒ  T
C                           FLAG(6) --> ”‹Aƒ I/O(PˆOPˆTET)
C                  ADPRTO - A„PEC OPTA B›BO„A ;
C                  ADPRTI - A„PEC OPTA BBO„A ;
C                  ADPROG - MAKCˆMA‹œH›‰ A„PEC O‡“ ,B PE„E‹AX KOTOPOƒO
C                           PAOTAET “PAB‹Ÿž™AŸ POƒPAMMA ;
C                  IPORT  - CO„EP†ˆMOE OPTA B›BO„A ;
C                  IPORT1 - CO„EP†ˆMOE OPTA BBO„A ;
C
C
C               B›XO„H›E :
C                  T      - BPEMŸ B›O‹HEHˆŸ KOMAH„› ;
C                  IER    - “KA‡ATE‹œ O˜ˆKˆ:
C                           IER=0  -  O˜ˆOK HE OHAP“†EHO ;
C                           IER=1  -  O˜ˆO—HAŸ ‡AˆCœ KOMAH„› ‹ˆO
C                                     B›XO„ ‡A PE„E‹› O‡“ ;
C                           IER=2  -  POˆ‡O˜‹O OPA™EHˆE K Ÿ—E‰KE
C                                     AMŸTˆ, ˆMEž™E‰ HE—ETH›‰ A„PEC.
C                           IER=3  -  HE HA˜A KOMAH„A ;
C                           IER=4  -  OPA™EHˆE K OPT“ B/B
C                                     HE O TOM“ A„PEC“ ;
C                           IER=5  -  KOMAH„A OKA HE PEA‹ˆ‡OBAHA ;
C                           IER=6  -  KOMAH„A HE C“™ECTB“ET ;
C
C   O„POƒPAMM›:  1. ADECIS
C   ------------
C
C   PˆME—AHˆE:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG, KB(16), KB1(16)
       IER = 0
C--> —TO ‡A KOMAH„› ?
       KD1 = KD - 28672
       KOM = KD1 / 512 - 4
C--> PA‡O‰„EMCŸ O KOMAH„AM .
       GO TO (100, 9906, 77000 ), KOM
       GO TO 9903
100    CONTINUE
C----------------------------------------------------------------------
C              KOMAH„A "FADD"
C              KOMAH„A "FSUB"
C              KOMAH„A "FMUL"
C              KOMAH„A "FDIV"
C----------------------------------------------------------------------
C--> KOMAH„› OKA HE PEA‹ˆ‡OBAH›.
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAH„A "SOB"
C----------------------------------------------------------------------
77000  CONTINUE
C--> HOMEP PEƒˆCTPA.
       NR = ( KD - 32256 ) / 64 + 1
       ISM = KD - 32256 - (NR-1) * 64
C--> CME™AEMCŸ.
       REG(NR) = REG(NR) - 1
       IF( REG(NR) .NE. 0 ) GO TO 77010
C--> HE BETBˆMCŸ.
       T = 4.8E-6
       RETURN
77010  CONTINUE
C--> BETBˆMCŸ.
       REG(8) = REG(8) - 2 * ISM
       T = 5.6E-6
       RETURN
C----------------------------------------------------------------------
9903   CONTINUE
       IER = 3
       RETURN
C----------------------------------------------------------------------
9906   CONTINUE
       IER = 6
       RETURN
C----------------------------------------------------------------------
       END
