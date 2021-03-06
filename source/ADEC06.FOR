       SUBROUTINE ADEC06( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (AT SEPTEMBER 1989 �.)-----------
C   �O��PO�PAMMA: ADEC06( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1 , T , IER )
C
C   HA�HA�EH�E:  O�PA�OT��K  KOMAH�
C   ----------   M�KPO�PO�ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH�A  "MUL"  ( 070RSS ):                          *
C              * �MHO�EH�E .                                          *
C              *------------------------------------------------------*
C              * KOMAH�A  "DIV"   (071RSS ):                          *
C              * �E�EH�E .                                            *
C              *------------------------------------------------------*
C              * KOMAH�A  "ASH"   (072RSS ):                          *
C              * AP��MET��ECK�� C�B�� .                               *
C              *------------------------------------------------------*
C              * KOMAH�A  "ASCH"  (073RSS ):                          *
C              * AP��MET��ECK�� C�B�� �BO�HO�O C�OBA .                *
C              *------------------------------------------------------*
C              * KOMAH�A  "XOR"   (074RSS ):                          *
C              * �CK���A��EE ��� .                                    *
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
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG, KB(17), KB1(17)
       IER = 0
C--> �TO �A KOMAH�� ?
       KD1 = KD - 28672
       KOM = KD1 / 512 + 1
       NR  = ( KD1 - 512 * (KOM-1) ) / 64 + 1
C--> ��EM O�EPAH�  ( A�PEC--> IADRS,  ��C�O--> IDATA )
       KD0 =   KD1 - 512 * (KOM-1) - (NR - 1) * 64
       CALL ADECIS( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 2
C--> �A�POC HA BBO� �AHH�X �� �OPTA B/B
       IF( (-IADRS) .NE. ADPRTO.AND.(-IADRS).NE.ADPRTI ) GO TO 9904
       IDATA = IPORT
       IF( (-IADRS) .EQ. ADPRTI ) IDATA = IPORT1
2      CONTINUE
C--> PA�O��EMC� �O KOMAH�AM .
       GO TO (70000,71000,72000,73000,74000 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAH�A "MUL"
C----------------------------------------------------------------------
70000  CONTINUE
       INEW = REG(NR)*IDATA
C --> ��EM CTAP��E PA�P���
       INEW1 = INEW/65536
C --> ��EM M�A���E PA�P���
       INEW2 = INEW - INEW1*65536
C--> KAKO� PE��CTOP ?
       MD = MOD(NR,2)
       IF (MD.EQ.0) GO TO 70010
       REG(NR) = INEW1
       REG(NR+1) = INEW2
       GO TO 70020
70010  CONTINUE
       REG(NR) = INEW2
70020 CONTINUE
C-->   ��A�� ...
       IF(INEW.LT.(-32768).OR.INEW.GE.32768) FLAG(1) = 1
       FLAG(2) = 0
       FLAG(3) = 0
       IF ( INEW.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW.GE.32768) FLAG(4) = 1
C--> BPEM�  �OKA �P�MET�BHO ( = 55 MKC. )
       T = 55.E-6 + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A "DIV"
C----------------------------------------------------------------------
71000  CONTINUE
C--> KAKO� PE��CTOP ?
       MD = MOD(NR,2)
       IF (MD.EQ.0) GO TO 9901
       INEW = REG(NR)*65536 + REG(NR+1)
       IF (IDATA.EQ.0) GOTO 9901
C--> �ACTHOE
       INEW1 = INEW /IDATA
C--> OCTATOK
       INEW2 = INEW -INEW1*IDATA
       INEW3 = INEW1 /65536
       INEW4 = INEW1 - INEW3*65536
C--> PHUS INTO REGICTOR
       REG(NR) = INEW4
       REG(NR+1) = INEW2
C-->   ��A�� ...
       FLAG(1) = 0
       FLAG(3) = 0
       IF ( INEW4.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW4.GE.32768) FLAG(4) = 1
C--> BPEM�  �OKA �P�MET�BHO ( = 80 MKC. )
       T = 80.E-6 + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A "ASH"
C----------------------------------------------------------------------
72000  CONTINUE
       IF ( IDATA.GE.64 ) GOTO 9901
       NR0 = REG(NR)
C--> B KAK�� CTOPOH� ?
       IF ( IDATA.GE.32 ) GOTO 72010
C-->  C�B�� B�EBO
C-->  BPEM�
       TB = (2.8E-6)*IDATA
C-->  �XO��M EC�� O�H� H��� ...
       IF ( IDATA.GE.16 ) GOTO 72020
       INEW = REG(NR)*(2**IDATA)
       INEW1 = INEW /65536
       INEW  =  INEW -  INEW1*65536
C--> ��A� �EPEHOCA ?
       IC = MOD( INEW1,2)
       GO TO 72030
72010  CONTINUE
C--> C�B�� B�PABO
       IDATA = 64 - IDATA
C-->  BPEM�
       TB = (2.E-6)*IDATA
C--> COXPAH�ETC� CTAP��� PA�P��
       I15 = 0
       IF ( NR0.GT.32767 ) I15 = 32768
       IF ( NR0.GT.32767 ) REG(NR) = REG(NR) - 32768
       INEW1 = REG(NR)/(2**(IDATA-1))
       INEW  = INEW1/2 + I15
C--> ��A� �EPEHOCA ?
       IC = MOD( INEW1,2)
       GO TO 72030
72020  INEW = 0
       IC = 0
72030  CONTINUE
C-->   �A�OMH�M
       REG(NR) = INEW
C-->   ��A�� ...
       FLAG(1) = IC
       FLAG(2) = 0
       IF(NR0.GE.32768.AND.INEW.LT.32768) FLAG(2) =1
       IF(NR0.LT.32768.AND.INEW.GE.32768) FLAG(2) =1
       FLAG(3) = 0
       IF ( INEW.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW.GE.32768) FLAG(4) = 1
C--> BPEM�
       T = TB  + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A "ASCH"
C----------------------------------------------------------------------
73000  CONTINUE
       NR1 = REG(NR)
C--> KAKO� PE��CTOP �C�O����ETC� ?
       MD = MOD(NR,2)
       IF (MD.EQ.0) GO TO 73010
       NR0 = REG(NR)*65536 + REG(NR+1)
C--> B KAK�� CTOPOH� ?
       IF ( IDATA.GE.32 ) GOTO 73020
C-->  C�B�� B�EBO
C-->  BPEM�
       TB = (3.6E-6)*IDATA
       I32 = 0
       INEW = NR0*(2**IDATA)
       INEW0 = 0
CCCC!  INEW  =  INEW -  INEW0*(2**32)
       GO TO 73030
73020  CONTINUE
C--> C�B�� B�PABO
       IDATA = 64 - IDATA
C-->  BPEM�
       TB = (3.2E-6)*IDATA
C--> COXPAH�ETC� CTAP��� PA�P��
       I32 = 0
       IF ( REG(NR).GT.32767 ) I32 = 32768
       IF ( REG(NR).GT.32767 ) NR0 = NR0-65536*32768
       INEW1 = NR0/(2**(IDATA-1))
       INEW  = INEW1/2
73030  CONTINUE
       INEW1 = INEW /65536
       INEW2 =  INEW -  INEW1*65536
C--> ��A� �EPEHOCA ?
       IC = MOD( INEW0,2)
       REG(NR) = INEW1 + I32
       INEW = REG(NR)
       REG(NR+1) = INEW2
       GO TO 73050
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
73010 CONTINUE
C--> �HA��T ��K���ECK�� C�B��
C--> �PEO�PA��EM (REG) B �BO��H�� KO�
       CALL ADECDB(NR1,KB,IER )
       IF ( IER.NE.0) RETURN
       KB(17) = FLAG(1)
C--> B KAK�� CTOPOH� ?
       IF ( IDATA.GE.32 ) GOTO 73110
C-->  C�B�� B�EBO
C-->  BPEM�
       TB = (3.6E-6)*IDATA
       DO 73120 I = 1,17
       I1 = I + IDATA
       I2 = MOD(I1,17)
       IF(I2.EQ.0) I2 = 17
       KB1(I2) = KB(I)
73120  CONTINUE
       GO TO 73130
73110  CONTINUE
C--> C�B�� B�PABO
       IDATA = 64 - IDATA
C-->  BPEM�
       TB = (3.2E-6)*IDATA
       DO 73129 I = 1,17
       I1 =  18 -  I + IDATA
       I2 = MOD(I1,17)
       IF(I2.EQ.0) I2 = 17
       KB1(18 - I2) = KB(I)
73129  CONTINUE
73130  CONTINUE
C--> �PEO�PA��EM B �EC�T��HOM B��E
       CALL ADECBD(INEW,KB1,IER )
CCC    WRITE(6,*) KB1(17),INEW
       IF ( IER.NE.0) RETURN
       IC = KB1(17)
       REG(NR) = INEW
73050  CONTINUE
C-->   ��A�� ...
       FLAG(1) = IC
       FLAG(2) = 0
       IF(NR1.GE.32768.AND.INEW.LT.32768) FLAG(2) =1
       IF(NR1.LT.32768.AND.INEW.GE.32768) FLAG(2) =1
       FLAG(3) = 0
       IF ( INEW.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW.GE.32768) FLAG(4) = 1
C--> BPEM�
       T = TB  + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A "XOR"
C----------------------------------------------------------------------
74000  CONTINUE
C--> PA���BAEM CO�EP��MOE PE��CTPA.
       CALL ADECDB( REG(NR), KB, IER )
       IF( IER .NE. 0 ) RETURN
C--> PA���BAEM CO�EP��MOE �CTO�H�KA.
       CALL ADECDB( IDATA, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> O�EPA��� "�CK�. ���"
       DO 74010 I=1,16
       KB(I) = KB(I) + KB1(I)
       IF( KB(I) .GT. 1 ) KB(I) = 0
74010  CONTINUE
       CALL ADECBD( INEW, KB, IER )
       IF( IER .NE. 0 ) RETURN
C--> �A�MEMC� ��A�AM�.
       FLAG(2) = 0
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEM� ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0 ) T = 4.0E-6
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