       SUBROUTINE ADEC05( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT CEHT��P�  1989 �.)-----------
C   �O��PO�PAMMA: ADEC05( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )
C
C   HA�HA�EH�E:  O�PA�OT��K  O�HOA�PECH�X KOMAH�
C   ----------   M�KPO�PO�ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH�A  "ROR"  ( 0060DD ):                          *
C              * ��K���ECK�� C�B�� B �PABO.                           *
C              *------------------------------------------------------*
C              * KOMAH�A  "ROL"   (0061DD ):                          *
C              * ��K���ECK�� C�B�� B �EBO .                           *
C              *------------------------------------------------------*
C              * KOMAH�A  "ASR"   (0062DD ):                          *
C              * AP��MET��ECK�� C�B�� B �PABO.                        *
C              *------------------------------------------------------*
C              * KOMAH�A  "ASL"   (0063DD ):                          *
C              * AP��MET��ECK�� C�B�� B �EBO.                         *
C              *------------------------------------------------------*
C              * KOMAH�A  "MARK" (0064DD ):                           *
C              * BOCCTAHOB�EH�E "�C".                                 *
C              *------------------------------------------------------*
C              * KOMAH�A  "MFPI"( 0065DD ):                           *
C              *                                                      *
C              *------------------------------------------------------*
C              * KOMAH�A  "MTPI"( 0066DD ):                           *
C              *                                                      *
C              *------------------------------------------------------*
C              * KOMAH�A  "SXT" ( 0067DD ):                           *
C              * PAC��PEH�E �HAKA .                                   *
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
       KD1 = KD - 3072
       KOM = KD1 / 64 + 1
C--> ��EM O�EPAH�  ( A�PEC--> IADRS,  ��C�O--> IDATA )
       KD0 =  KD1 - 64 * ( KOM - 1 )
       CALL ADECIS( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 2
C--> �A�POC HA BBO� �AHH�X �� �OPTA B�BO�A
       IF( (-IADRS) .NE. ADPRTO ) GO TO 9904
       IDATA = IPORT
2      CONTINUE
C--> PA�O��EMC� �O KOMAH�AM .
       GO TO ( 6000, 6100, 6200, 6300, 6400, 6500, 6600, 6700 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAH�A "ROR"
C----------------------------------------------------------------------
6000   CONTINUE
C--> C�B��AEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6010 I=2,16
       KB1(I-1) = KB(I)
6010   CONTINUE
       KB1(16) = FLAG(1)
       CALL ADECBD( INEW, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = KB(1)
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEM� ?
       T = 6.8E-6 + TI
       IF( TI .EQ. 0 ) T = 6.0E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "ROL"
C----------------------------------------------------------------------
6100   CONTINUE
C--> C�B��AEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6110 I=1,15
       KB1(I+1) = KB(I)
6110   CONTINUE
       KB1(1) = FLAG(1)
       CALL ADECBD( INEW, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = KB(16)
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEM� ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "ASR"
C----------------------------------------------------------------------
6200   CONTINUE
C--> C�B��AEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6210 I=2,15
       KB1(I-1) = KB(I)
6210   CONTINUE
       KB1(16) = KB(16)
       CALL ADECBD( INEW, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = KB(1)
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEM� ?
       T = 7.2E-6 + TI
       IF( TI .EQ. 0 ) T = 6.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "ASL"
C----------------------------------------------------------------------
6300   CONTINUE
C--> C�B��AEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6310 I=1,15
       KB1(I+1) = KB(I)
6310   CONTINUE
       KB1(1) = 0
       CALL ADECBD( INEW, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> �A�MEMC� ��A�AM�.
       FLAG(1) = KB(16)
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEM� ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAH�A "MARK"
C----------------------------------------------------------------------
6400   CONTINUE
       ISM = KD - 3328
       REG(7) = REG(8) + 2 * ISM
       REG(8) = REG(6)
       REG(6) = MEM(REG(7) + 1 )
       REG(7) = REG(7) +2
C---> �POBEP�EM �KA�ATE�� CTEKA , HE B��E� �� �A �PE�E�A ....
       IF ( REG(7) .GT.1024 ) GO TO 9901
C--> BPEM� ?
       T = 13.2E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A "MFPI"
C              KOMAH�A "MTPI"
C----------------------------------------------------------------------
6500   CONTINUE
6600   CONTINUE
C--> KOMAH�� �OKA HE PEA���OBAH�.
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A "SXT"
C----------------------------------------------------------------------
6700   CONTINUE
       INEW = 0
       IF( FLAG(4) .EQ. 1 ) INEW = 65535
C--> �A�MEMC� ��A�AM�.
       FLAG(2) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
C--> BPEM� ?
       T = 7.6E-6 + TI
       IF( TI .EQ. 0 ) T = 6.8E-6
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