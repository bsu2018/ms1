       SUBROUTINE ADEC08( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1 , T, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT SEPTEMBER 1989 �.)-----------
C   �O��PO�PAMMA: ADEC08( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1 , T, IER )
C
C   HA�HA�EH�E:  O�PA�OT��K  KOMAH� BETB�EH��
C   ----------   M�KPO�PO�ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH�A  "BPL"  ( 100000 ):                          *
C              * BETB�EH�E, EC�� ���C .                               *
C              *------------------------------------------------------*
C              * KOMAH�A  "BMI"  ( 100400 ):                          *
C              * BETB�EH�E, EC��  M�H�C .                             *
C              *------------------------------------------------------*
C              * KOMAH�A  "BHI"  ( 101000 ):                          *
C              * BETB�EH�E, EC�� �O���E .                             *
C              *------------------------------------------------------*
C              * KOMAH�A  "BLOS" ( 101400 ):                          *
C              * BETB�EH�E, EC�� MEH��E ��� PABHO .                   *
C              *------------------------------------------------------*
C              * KOMAH�A  "BVC"  ( 102000 ):                          *
C              * BETB�EH�E, EC�� HET AP��MET��ECKO�O �EPE�O�HEH�� .   *
C              *------------------------------------------------------*
C              * KOMAH�A  "BVS"  ( 102400 ):                          *
C              * BETB�EH�E, EC�� AP��MET��ECKOE �EPE�O�HEH�E .        *
C              *------------------------------------------------------*
C              * KOMAH�A  "BCC"  ( 103000 ):                          *
C              * BETB�EH�E, EC�� HET �EPEHOCA .                       *
C              *------------------------------------------------------*
C              * KOMAH�A  "BHIS" ( 103000 ):                          *
C              * BETB�EH�E, EC�� �O���E ��� PABHO .                   *
C              *------------------------------------------------------*
C              * KOMAH�A  "BCS"  ( 103400 ):                          *
C              * BETB�EH�E, EC�� �EPEHOC .                            *
C              *------------------------------------------------------*
C              * KOMAH�A  "BLO"  ( 103400 ):                          *
C              * BETB�EH�E, EC�� MEH��E .                             *
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
C   �O��PO�PAMM�:  1. HET.
C   ------------
C
C   �P�ME�AH�E:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG
       IER = 0
C--> PA�O��EMC� �O KOMAH�AM :
       KD1 = KD - 32768
       KOM = KD1 / 256 + 1
       GO TO ( 10, 400, 1000, 1400, 2000, 2400, 3000, 3400 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C           KOMAH�A "BPL"
C----------------------------------------------------------------------
10     CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( FLAG(4) .NE. 0 ) RETURN
C--> CME�EH�E
       ISM = KD1
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH�A "BMI"
C----------------------------------------------------------------------
400    CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( FLAG(4) .NE. 1 ) RETURN
C--> CME�EH�E
       ISM = KD1 - 256
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH�A "BHI"
C----------------------------------------------------------------------
1000   CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( ( FLAG(1)+FLAG(3) ) .NE. 0 ) RETURN
C--> CME�EH�E
       ISM = KD1 - 512
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH�A "BLOS"
C----------------------------------------------------------------------
1400   CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( ( FLAG(1)+FLAG(3) ) .EQ. 0 ) RETURN
C--> CME�EH�E
       ISM = KD1 - 768
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH�A "BVC"
C----------------------------------------------------------------------
2000   CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( FLAG(2) .NE. 0 ) RETURN
C--> CME�EH�E
       ISM = KD1 - 1024
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH�A "BVS"
C----------------------------------------------------------------------
2400   CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( FLAG(2) .NE. 1 ) RETURN
C--> CME�EH�E
       ISM = KD1 - 1280
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH�� "BCC" � "BHIS"
C----------------------------------------------------------------------
3000   CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( FLAG(1) .NE. 0 ) RETURN
C--> CME�EH�E
       ISM = KD1 - 1536
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
C           KOMAH�� "BCS" � "BLO"
C----------------------------------------------------------------------
3400   CONTINUE
C--> BPEM� ?
       T = 4.0E-6
C--> CME�AEMC� ��� HET ?
       IF( FLAG(1) .NE. 1 ) RETURN
C--> CME�EH�E
       ISM = KD1 - 1792
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
       RETURN
C----------------------------------------------------------------------
9903   CONTINUE
       IER = 3
       RETURN
C----------------------------------------------------------------------
       END
