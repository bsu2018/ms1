       SUBROUTINE ADEC04( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT CEHT��P�  1989 �.)-----------
C   �O��PO�PAMMA: ADEC04( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )
C
C   HA�HA�EH�E:  O�PA�OT��K  O�HOA�PECH�X KOMAH�
C   ----------   M�KPO�PO�ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH�A  "JSR"  ( 004RDD ):                          *
C              * O�PA�EH�E K �O��PO�PAMME .                           *
C              *------------------------------------------------------*
C              * KOMAH�A  "CLR"   (0050DD ):                          *
C              * O��CTKA .                                            *
C              *------------------------------------------------------*
C              * KOMAH�A  "COM"   (0051DD ):                          *
C              * �HBEPT�POBAH�E .                                     *
C              *------------------------------------------------------*
C              * KOMAH�A  "INC"   (0052DD ):                          *
C              * �P��AB�EH�E E��H��� .                                *
C              *------------------------------------------------------*
C              * KOMAH�A  "DEC"  (0053DD ):                           *
C              * B���TAH�E E��H��� .                                  *
C              *------------------------------------------------------*
C              * KOMAH�A  "NEG" ( 0054DD ):                           *
C              * ��MEHEH�E �HAKA .                                    *
C              *------------------------------------------------------*
C              * KOMAH�A  "ADC" ( 0055DD ):                           *
C              * �P��AB�EH�E �EPEHOCA .                               *
C              *------------------------------------------------------*
C              * KOMAH�A  "SBC" ( 0056DD ):                           *
C              * B���TAH�E �EPEHOCA .                                 *
C              *------------------------------------------------------*
C              * KOMAH�A  "TST"  ( 0057DD):                           *
C              * �POBEPKA .                                           *
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
       KOM = KD / 512 - 3
C--> PA�O��EMC� �O KOMAH�AM .
       GO TO ( 4000, 5 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAH�A "JSR"
C----------------------------------------------------------------------
4000   CONTINUE
C--> HOMEP PE��CTPA ?
       NR = ( KD - 2048 ) / 64 + 1
C---> �POBEP�EM  �PAB���H�� �� �A�AH NR ?
        IF ( NR.LT.1 .OR .NR .GT .6 ) GOTO 9901
C--> ��EM O�EPAH�  ( A�PEC--> IADRS,  ��C�O--> IDATA )
       KD0 =  KD - 2048  - 64 * ( NR - 1 )
       CALL ADECSI( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 4010
C--> �A�POC HA BBO� �AHH�X �� �OPTA BBO�A
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT1
4010   CONTINUE
C
       MEM( REG(7) - 1 ) = REG(NR)
       MEM( REG(7) - 2 ) = REG(NR) / 256
       REG(7) = REG(7) - 2
C
       REG(NR) = REG(8)
C
       REG(8) = IDATA
C--> BPEM� ?
       T = 4.0E-6 + TI
       RETURN
C----------------------------------------------------------------------
5      CONTINUE
       KD1 = KD - 2560
       KOM1= KD1 / 64 + 1
C--> ��EM O�EPAH�  ( A�PEC--> IADRS,  ��C�O--> IDATA )
       KD0 =  KD1 - 64 * ( KOM1 - 1 )
       CALL ADECIS( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 10
C--> �A�POC HA BBO� �AHH�X �� �OPTA B�BO�A
       IF( (-IADRS) .NE. ADPRTO ) GO TO 9904
       IDATA = IPORT
10     CONTINUE
       GO TO ( 5000, 5100, 5200, 5300, 5400, 5500, 5600, 5700 ), KOM1
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAH�A "CLR"
C----------------------------------------------------------------------
5000   CONTINUE
       INEW = 0
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = 0
       FLAG(2) = 0
       FLAG(3) = 1
       FLAG(4) = 0
C--> BPEM� ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "COM"
C----------------------------------------------------------------------
5100   CONTINUE
       INEW = 65535 - IDATA
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = 1
       FLAG(2) = 0
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "INC"
C----------------------------------------------------------------------
5200   CONTINUE
       INEW = IDATA + 1
       IF( INEW .GT. 65535 ) INEW = INEW - 65536
C--> �A�MEMC� ��A�AM�.
       FLAG(2) = 0
       IF( IDATA .EQ. 32767 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "DEC"
C----------------------------------------------------------------------
5300   CONTINUE
       INEW = IDATA - 1
       IF( INEW .LT. 0 ) INEW = 65536 + INEW
C--> �A�MEMC� ��A�AM�.
       FLAG(2) = 0
       IF( IDATA .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "NEG"
C----------------------------------------------------------------------
5400   CONTINUE
       INEW = 65536 - IDATA
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = 1
       IF( INEW .EQ. 0 ) FLAG(1) = 0
       FLAG(2) = 0
       IF( INEW .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "ADC"
C----------------------------------------------------------------------
5500   CONTINUE
       INEW = IDATA + FLAG(1)
       IF( INEW .GT. 65535 ) INEW = INEW - 65536
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = 0
       IF( INEW .EQ. 0  .AND.  IDATA .EQ. 65535  ) FLAG(1) = 1
       FLAG(2) = 0
       IF( INEW .EQ. 32768  .AND.  IDATA .EQ. 32767 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "SBC"
C----------------------------------------------------------------------
5600   CONTINUE
       INEW = IDATA - FLAG(1)
       IF( INEW .LT. 0 ) INEW = 65536 + INEW
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = 0
       IF( IDATA .EQ. 0  .AND.  INEW .EQ. 65535  ) FLAG(1) = 1
       FLAG(2) = 0
       IF( IDATA .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "TST"
C----------------------------------------------------------------------
5700   CONTINUE
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = 0
       FLAG(2) = 0
       FLAG(3) = 0
       IF( IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( IDATA .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 4.4E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
        RETURN
C######################################################################
9000   CONTINUE
C-->  �A��C�BAEM PE����TAT
       IF(IADRS .LT. 0 ) GO TO 92
       MEM(IADRS + 1) = INEW
       MEM( IADRS + 2) = INEW/256
       RETURN
92     CONTINUE
       IF(IADRS . LT. -8 ) GO TO 93
       REG(-IADRS) = INEW
       RETURN
93     CONTINUE
       IPORT = INEW
       RETURN
C----------------------------------------------------------------------
9901   CONTINUE
       IER = 1
       RETURN
C----------------------------------------------------------------------
9903   CONTINUE
       IER = 3
       RETURN
C----------------------------------------------------------------------
9904   CONTINUE
       IER = 4
       RETURN
C----------------------------------------------------------------------
       END
