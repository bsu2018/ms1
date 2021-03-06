       SUBROUTINE ADEC07( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1, T, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT SEPTEMBER 1989 �.)-----------
C   �O��PO�PAMMA: ADEC07( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1 , T, IER )
C
C   HA�HA�EH�E:  O�PA�OT��K  KOMAH�
C   ----------   M�KPO�PO�ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH�A  "FADD" ( 07500R ):                          *
C              * C�O�EH�E C ��ABA��E� �A��TO� .                       *
C              *------------------------------------------------------*
C              * KOMAH�A  "FSUB"  (07501R ):                          *
C              * B���TAH�E C ��ABA��E� �A��TO� .                      *
C              *------------------------------------------------------*
C              * KOMAH�A  "FMUL"  (07502R ):                          *
C              * �MHO�EH�E C ��ABA��E� �A��TO� .                      *
C              *------------------------------------------------------*
C              * KOMAH�A  "FDIV"  (07503R ):                          *
C              * �E�EH�E  C ��ABA��E� �A��TO� .                       *
C              *------------------------------------------------------*
C              * KOMAH�A  "SOB"   (077RNN ):                          *
C              * B���TAH�E E�EH��� � BETB�EH�E .                      *
C              *------------------------------------------------------*
C
C   METO�:      �O���ECKOE MO�E��POBAH�E.
C   -----
C
C   �APAMETP�:  BXO�H�E :
C  ----------      KD     - KO� KOMAH��  ;
C                  REG(8) - MACC�B PE��CTPOB M�KPO�PO�ECCOPA ;
C                           REG(1) --> R0
C                           REG(2) --> R1
C                           REG(3) --> R2
C                           REG(4) --> R3
C                           REG(5) --> R4
C                           REG(6) --> R5
C                           REG(7) --> R6 ( �KA�ATE�� CTEKA )
C                           REG(8) --> R7 ( C�ET��K KOMAH�  )
C                  FLAG(6)- MACC�B ��A�OB CC� M�KPO�PO�ECCOPA ;
C                           FLAG(1) --> ��A�  C (�EPEHOC)
C                           FLAG(2) --> ��A�  V (AP��M. �EPE�O�HEH�E)
C                           FLAG(3) --> ��A�  Z (H��EBO� PE����TAT)
C                           FLAG(4) --> ��A�  N (OTP��AT.PE����TAT)
C                           FLAG(5) --> ��A�  T
C                           FLAG(6) --> ��A� I/O(�P�OP�TET)
C                  ADPRTO - A�PEC �OPTA B�BO�A ;
C                  ADPRTI - A�PEC �OPTA BBO�A ;
C                  ADPROG - MAKC�MA��H�� A�PEC O�� ,B �PE�E�AX KOTOPO�O
C                           PA�OTAET ��PAB����A� �PO�PAMMA ;
C                  IPORT  - CO�EP��MOE �OPTA B�BO�A ;
C                  IPORT1 - CO�EP��MOE �OPTA BBO�A ;
C
C
C               B�XO�H�E :
C                  T      - BPEM� B��O�HEH�� KOMAH�� ;
C                  IER    - �KA�ATE�� O���K�:
C                           IER=0  -  O���OK HE O�HAP��EHO ;
C                           IER=1  -  O���O�HA� �A��C� KOMAH�� ���O
C                                     B�XO� �A �PE�E�� O�� ;
C                           IER=2  -  �PO��O��O O�PA�EH�E K ��E�KE
C                                     �AM�T�, �ME��E� HE�ETH�� A�PEC.
C                           IER=3  -  HE HA�A KOMAH�A ;
C                           IER=4  -  O�PA�EH�E K �OPT� B/B
C                                     HE �O TOM� A�PEC� ;
C                           IER=5  -  KOMAH�A �OKA HE PEA���OBAHA ;
C                           IER=6  -  KOMAH�A HE C��ECTB�ET ;
C
C   �O��PO�PAMM�:  1. ADECIS
C   ------------
C
C   �P�ME�AH�E:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG, KB(16), KB1(16)
       IER = 0
C--> �TO �A KOMAH�� ?
       KD1 = KD - 28672
       KOM = KD1 / 512 - 4
C--> PA�O��EMC� �O KOMAH�AM .
       GO TO (100, 9906, 77000 ), KOM
       GO TO 9903
100    CONTINUE
C----------------------------------------------------------------------
C              KOMAH�A "FADD"
C              KOMAH�A "FSUB"
C              KOMAH�A "FMUL"
C              KOMAH�A "FDIV"
C----------------------------------------------------------------------
C--> KOMAH�� �OKA HE PEA���OBAH�.
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A "SOB"
C----------------------------------------------------------------------
77000  CONTINUE
C--> HOMEP PE��CTPA.
       NR = ( KD - 32256 ) / 64 + 1
       ISM = KD - 32256 - (NR-1) * 64
C--> CME�AEMC�.
       REG(NR) = REG(NR) - 1
       IF( REG(NR) .NE. 0 ) GO TO 77010
C--> HE BETB�MC�.
       T = 4.8E-6
       RETURN
77010  CONTINUE
C--> BETB�MC�.
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