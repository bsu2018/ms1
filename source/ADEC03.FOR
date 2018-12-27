       SUBROUTINE ADEC03( KD, REG, FLAG, ADPRTO,ADPRTI,ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT CEHTŸPŸ  1989 ƒ.)-----------
C   O„POƒPAMMA: ADEC03( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )
C
C   HA‡HA—EHˆE:  OPAOT—ˆK  KOMAH„ BETB‹EHˆŸ
C   ----------   MˆKPOPO–ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH„A  "BNE"  ( 001000 ):                          *
C              * BETB‹EHˆE, EC‹ˆ HE PABHO H“‹ž.                       *
C              *------------------------------------------------------*
C              * KOMAH„A  "BEQ"  ( 001400 ):                          *
C              * BETB‹EHˆE, EC‹ˆ  PABHO H“‹ž.                         *
C              *------------------------------------------------------*
C              * KOMAH„A  "BGE"  ( 002000 ):                          *
C              * BETB‹EHˆE, EC‹ˆ O‹œ˜E ˆ‹ˆ PABHO H“‹ž.               *
C              *------------------------------------------------------*
C              * KOMAH„A  "BLT"  ( 002400 ):                          *
C              * BETB‹EHˆE, EC‹ˆ MEHœ˜E  H“‹Ÿ.                        *
C              *------------------------------------------------------*
C              * KOMAH„A  "BGT"  ( 003000 ):                          *
C              * BETB‹EHˆE, EC‹ˆ O‹œ˜E H“‹Ÿ.                         *
C              *------------------------------------------------------*
C              * KOMAH„A  "BLE"  ( 003400 ):                          *
C              * BETB‹EHˆE, EC‹ˆ MEHœ˜E ˆ‹ˆ PABHO H“‹ž.               *
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
C   O„POƒPAMM›:  1. HET.
C   ------------
C
C   PˆME—AHˆE:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI,ADPROG
       IER = 0
C--> PA‡O‰„EMCŸ O KOMAH„AM :
       KOM = KD / 256 - 1
       GO TO ( 1000, 1400, 2000, 2400, 3000, 3400 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C           KOMAH„A "BNE"
C----------------------------------------------------------------------
1000   CONTINUE
C--> BPEMŸ ?
       T = 4.0E-6
C--> CME™AEMCŸ ˆ‹ˆ HET ?
       IF( FLAG(3) .NE. 0 ) RETURN
C--> CME™EHˆE
       ISM = KD - 512
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME™AEMCŸ
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH„A "BEQ"
C----------------------------------------------------------------------
1400   CONTINUE
C--> BPEMŸ ?
       T = 4.0E-6
C--> CME™AEMCŸ ˆ‹ˆ HET ?
       IF( FLAG(3) .NE. 1 ) RETURN
C--> CME™EHˆE
       ISM = KD - 768
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME™AEMCŸ
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH„A "BGE"
C----------------------------------------------------------------------
2000   CONTINUE
C--> BPEMŸ ?
       T = 4.0E-6
C--> CME™AEMCŸ ˆ‹ˆ HET ?
       IF( FLAG(2) .NE. FLAG(4) ) RETURN
C--> CME™EHˆE
       ISM = KD - 1024
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME™AEMCŸ
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH„A "BLT"
C----------------------------------------------------------------------
2400   CONTINUE
C--> BPEMŸ ?
       T = 4.0E-6
C--> CME™AEMCŸ ˆ‹ˆ HET ?
       IF( ( FLAG(2)+FLAG(4) ).NE. 1 ) RETURN
C--> CME™EHˆE
       ISM = KD - 1280
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME™AEMCŸ
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH„A "BGT"
C----------------------------------------------------------------------
3000   CONTINUE
C--> BPEMŸ ?
       T = 4.0E-6
C--> CME™AEMCŸ ˆ‹ˆ HET ?
       IF( FLAG(3) .EQ. 1  .OR.  FLAG(2) .NE. FLAG(4)  ) RETURN
C--> CME™EHˆE
       ISM = KD - 1536
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME™AEMCŸ
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH„A "BLE"
C----------------------------------------------------------------------
3400   CONTINUE
C--> BPEMŸ ?
       T = 4.0E-6
C--> CME™AEMCŸ ˆ‹ˆ HET ?
       IF( FLAG(3) .EQ. 0  .AND.  ( FLAG(2)+FLAG(4) ) .NE. 1 ) RETURN
C--> CME™EHˆE
       ISM = KD - 1792
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME™AEMCŸ
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
9903   CONTINUE
       IER = 3
       RETURN
C----------------------------------------------------------------------
       END
