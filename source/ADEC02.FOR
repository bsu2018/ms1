       SUBROUTINE ADEC02( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT CEHT��P�  1988 �.)-----------
C   �O��PO�PAMMA: ADEC02( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   ------------          IPORT1,T,IER )
C
C   HA�HA�EH�E:  O�PA�OT��K  KOMAH� ��PAB�EH��,�EPEC��K�
C   ----------   M�KPO�PO�ECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAH�A  "HALT" ( 000000 ):                          *
C              * OCTAHOB                                              *
C              *------------------------------------------------------*
C              * KOMAH�A  "WAIT"  (000001 ):                          *
C              * O���AH�E                                             *
C              *------------------------------------------------------*
C              * KOMAH�A  "RTI"   (000002 ):                          *
C              * BO�BPAT �� �PEP�BAH��                                *
C              *------------------------------------------------------*
C              * KOMAH�A  "BPT"   (000003 ):                          *
C              * KOMAH�HOE �PEP�BAH�E ��� OT�ATK�                     *
C              *------------------------------------------------------*
C              * KOMAH�A  "IOT"  (000004 ):                           *
C              * KOMAH�HOE �PEP�BAH�� ��� BBO�-B�BO�A                 *
C              *------------------------------------------------------*
C              * KOMAH�A  "RESET"( 000005 ):                          *
C              * CBPOC BHE�H�X �CTPO�CTB                              *
C              *------------------------------------------------------*
C              * KOMAH�A  "RTT" ( 000006 ):                           *
C              * BO�BPAT �� �PEP�BAH��                                *
C              *------------------------------------------------------*
C              * KOMAH�A  "JMP" ( 0001DD ):                           *
C              * �E��C�OBH�� �EPEXO� HA A�PEC <DD>                    *
C              *------------------------------------------------------*
C              * KOMAH�A  "RTS"  ( 00020R):                           *
C              * BO�BPAT �� �O��PO�PAMM�                              *
C              *------------------------------------------------------*
C              * KOMAH�A  "NOP" ( 000240):                            *
C              * ��CTA� KOMAH�A                                       *
C              *------------------------------------------------------*
C              * KOMAH�A "CLC"  ( 000241):                            *
C              * O��CTKA ��A�A �EPEHOC  C                             *
C              *------------------------------------------------------*
C              * KOMAH�A "CLV" ( 000242 ):                            *
C              * O��CTKA ��A�A  AP��MET��ECKOE �EPE�O�HEH�E V         *
C              *------------------------------------------------------*
C              * KOMAH�A "CLZ" ( 000244 ):                            *
C              * O��CTKA ��A�A H��EBO� PE����TAT Z                    *
C              *------------------------------------------------------*
C              * KOMAH�A "CLN"  (000250) :                            *
C              * O��CTKA ��A�A OTP��ATE��H�� PE����TAT N              *
C              *------------------------------------------------------*
C              * KOMAH�A "CCC"  (000257 ):                            *
C              * O��CTKA BCEX ��A�OB                                  *
C              *------------------------------------------------------*
C              * KOMAH�A "SEC"  (000261) :                            *
C              * �CTAHOBKA ��A�A �EPEHOC C                            *
C              *------------------------------------------------------*
C              * KOMAH�A "SEV"  ( 000262 ):                           *
C              * �CTAHOBKA ��A�A AP��MET��ECKOE �EPE�O�HEH�E V        *
C              *------------------------------------------------------*
C              * KOMAH�A "SEZ"  ( 000264):                            *
C              * �CTAHOBKA ��A�A H��EBO� PE����TAT Z                  *
C              *------------------------------------------------------*
C              * KOMAH�A  "SEN" ( 000270 ):                           *
C              * �CTAHOBKA ��A�A OTP��ATE��H�� PE����TAT  N           *
C              *------------------------------------------------------*
C              * KOMAH�A  "SCC"  ( 000277 )                           *
C              * �CTAHOBKA BCEX PA�P��OB                              *
C              *------------------------------------------------------*
C              * KOMAH�A  "SWAB" ( 0003DD ):                          *
C              * �EPECTAHOBKA �A�TOB <DD>                             *
C              *------------------------------------------------------*
C              * KOMAH�A  "BR" ( 000400):                             *
C              * BETB�EH�E �E��C�OBHOE                                *
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
C   �O��PO�PAMM�:  1. ADECSI, ADECDB, ADECBD ;
C   ------------
C
C   �P�ME�AH�E:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG, KB(16), KB1(16)
       IER = 0
C--> PA�O��EMC� �O KOMAH�AM :
       KOM = KD / 64 + 1
       GO TO ( 5,100,200,300,400,400,400,400 ), KOM
       GO TO 9903
5      CONTINUE
       KD0 = KD + 1
       GO TO ( 10,20,30,40,50,60,70,9906 ), KD0
C----------------------------------------------------------------------
C              KOMAH�A  "HALT"
C              KOMAH�A  "WAIT"
C----------------------------------------------------------------------
10     CONTINUE
20     CONTINUE
C --> HE PEA���OBA��HO
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "RTI"
C----------------------------------------------------------------------
30     CONTINUE
C--> HE PEA���OBAH T-PA�P��
C--->  BOCTAHAB��BAETC� CO�EP��MOE CK
       REG(8) = MEM(REG(7)+1)
       REG(7) = REG(7) + 2
C--> BOCTAHAB��BAETC� PE��CTP COCTO�H�� �PO�ECCOPA
       ICCP = MEM( REG(7) + 1 )
       REG(7) = REG(7) + 2
C---> �POBEP�EM �KA�ATE�� CTEKA ,HE B��E� �� �A �PE�E�A ...
        IF ( REG(7) .GT.1024 ) GO TO 9901
C--> O�PE�E��M ��A�� .
       CALL ADECDB( ICCP, KB, IER )
       IF( IER .NE. 0 ) RETURN
       FLAG(1) = KB(1)
       FLAG(2) = KB(2)
       FLAG(3) = KB(3)
       FLAG(4) = KB(4)
       FLAG(5) = KB(5)
       FLAG(6) = KB(8)
C--> BPEM� ?
       T = 10.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "BPT"
C              KOMAH�A  "IOT"
C              KOMAH�A  "RESET"
C----------------------------------------------------------------------
40     CONTINUE
50     CONTINUE
60     CONTINUE
C--> HE PEA���OBA��HO
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "RTT"
C----------------------------------------------------------------------
70     CONTINUE
C--> HE PEA���OBAH T-PA�P��
C--->  BOCTAHAB��BAETC� CO�EP��MOE CK
       REG(8) = MEM(REG(7)+1)
       REG(7) = REG(7) + 2
C--> BOCTAHAB��BAETC� PE��CTP COCTO�H�� �PO�ECCOPA
       ICCP = MEM( REG(7) + 1 )
       REG(7) = REG(7) + 2
C---> �POBEP�EM �KA�ATE�� CTEKA ,HE B��E� �� �A �PE�E�A ...
        IF ( REG(7) .GT.1024 ) GO TO 9901
C--> O�PE�E��M ��A�� .
       CALL ADECDB( ICCP, KB, IER )
       IF( IER .NE. 0 ) RETURN
       FLAG(1) = KB(1)
       FLAG(2) = KB(2)
       FLAG(3) = KB(3)
       FLAG(4) = KB(4)
       FLAG(5) = KB(5)
       FLAG(6) = KB(8)
C--> BPEM� ?
       T = 10.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "JMP"
C----------------------------------------------------------------------
100    CONTINUE
C-->�OKA HE �A�PE�AETC� PE��CTPOBA� A�PECA���  ....
C--> ��EM O�EPAH� �CTO�H�K ( A�PEC--> IADRS,  ��C�O--> IDATA )
       KD1 =  KD - 64
       CALL ADECSI( KD1, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 110
C--> �A�POC HA BBO� �AHH�X �� �OPTA BBO�A
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT1
110    CONTINUE
C---> �POBEP�EM �ETHOCT� A�PECA �EPEXO�A ...
       TMD = MOD(IDATA,2)
CCC    WRITE (6,*) TMD,IDATA
       IF ( TMD .NE. 0 ) GO TO 9902
       REG(8) = IDATA
C--> �OC��TAEM BPEM�
       T  = TI + 4.0E-6
       RETURN
C######################################################################
200    CONTINUE
       KD1 = KD - 128
       KD2 = KD1/8 + 1
       GO TO (207, 9906, 9906, 9906, 239, 249, 260, 269 ),KD2
C----------------------------------------------------------------------
C              KOMAH�A  "RTS"
C----------------------------------------------------------------------
207    CONTINUE
       KD1 = KD - 128
C--> �POBEP�EM A�PECAHT �B��ETC� PE��CTOPOM
       IF ( KD1 .GE. 8 ) GO TO 9901
       REG(8) = REG(KD1+1)
       REG(KD1+1) = MEM(REG(7)+1)
       REG(7) = REG(7) + 2
C---> �POBEP�EM �KA�ATE�� CTEKA ,HE B��E� �� �A �PE�E�A ...
        IF ( REG(7) .GT.1024 ) GO TO 9901
       T = 7.0E-6
       RETURN
C######################################################################
239    CONTINUE
       IF( KD .EQ. 160 ) GO TO 240
       IF( KD .EQ. 161 ) GO TO 241
       IF( KD .EQ. 162 ) GO TO 242
       IF( KD .EQ. 164 ) GO TO 244
       GO TO 9906
C----------------------------------------------------------------------
C              KOMAH�A  "NOP"
C----------------------------------------------------------------------
240    CONTINUE
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "CLC"
C----------------------------------------------------------------------
241    CONTINUE
       FLAG(1) = 0
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "CLV"
C----------------------------------------------------------------------
242    CONTINUE
       FLAG(2) = 0
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "CLZ"
C----------------------------------------------------------------------
244    CONTINUE
       FLAG(3) = 0
       T = 4.0E-6
       RETURN
249    CONTINUE
       IF( KD .EQ. 168) GO TO 250
       IF( KD .EQ. 175) GO TO 257
       GO TO 9906
C----------------------------------------------------------------------
C              KOMAH�A  "CLN"
C----------------------------------------------------------------------
250    CONTINUE
       FLAG(4) = 0
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "CCC"
C----------------------------------------------------------------------
257    CONTINUE
       DO 258 I=1,4
       FLAG(I) = 0
258    CONTINUE
       T = 4.0E-6
       RETURN
C######################################################################
260    CONTINUE
       IF( KD .EQ. 177) GO TO 261
       IF( KD .EQ. 178) GO TO 262
       IF( KD .EQ. 180) GO TO 264
       GO TO 9906
C----------------------------------------------------------------------
C           KOMAH�A "SEC"
C----------------------------------------------------------------------
261    CONTINUE
       FLAG(1) = 1
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "SEV"
C----------------------------------------------------------------------
262    CONTINUE
       FLAG(2) = 1
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "SEZ"
C----------------------------------------------------------------------
264    CONTINUE
       FLAG(3) = 1
       T = 4.0E-6
       RETURN
C######################################################################
269    CONTINUE
       IF( KD .EQ. 184 ) GO TO 270
       IF( KD .EQ. 191 ) GO TO 277
       GO TO 9906
C----------------------------------------------------------------------
C              KOMAH�A  "SEN"
C----------------------------------------------------------------------
270    CONTINUE
       FLAG(4) = 1
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAH�A  "SCC"
C----------------------------------------------------------------------
277    CONTINUE
       DO 278 I=1,4
       FLAG(I) = 1
278    CONTINUE
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C       KOMAH�A  "SWAB"
C----------------------------------------------------------------------
300    CONTINUE
C--> ��EM O�EPAH� �CTO�H�K ( A�PEC--> IADRS,  ��C�O--> IDATA )
       KD1 =  KD - 192
       CALL ADECIS( KD1, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 310
C--> �A�POC HA BBO� �AHH�X �� �OPTA BBO�A
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT
310    CONTINUE
       IDATA1 = IDATA/256
       IDATA2 = IDATA - IDATA1*256
C-->�OMEH�EM �A�T�...
       IDATA = IDATA2 * 256 + IDATA1
C--> �A��C�BAEM PE����TAT
       IF( IADRS.LT.0)  GO TO 340
       MEM( IADRS+1) = IDATA
       MEM( IADRS+2) = IDATA/256
       GO TO 350
340    CONTINUE
       IF( IADRS.LT.-8) GO TO 345
       REG(-IADRS) = IDATA
       GO TO 350
345    CONTINUE
       IPORT = IDATA
350    CONTINUE
C-->> �OC��TAEM BPEM�
       T = TI + 4.4E-6
       IF (TI.EQ.0) T= 4.8E-6
C--> �A�MEMC�  �P��HAKAM�
      FLAG(1) = 0
      FLAG(2) = 0
      FLAG(3) = 0
      IF(IDATA1.EQ.0) FLAG(3) = 1
      FLAG(4) = 0
      IF( IDATA1.GT.128 ) FLAG(4) = 1
      RETURN
C----------------------------------------------------------------------
C              KOMAH�A "BR"
C----------------------------------------------------------------------
400    CONTINUE
C--> CME�EH�E
       ISM = KD - 256
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CME�AEMC�
       REG(8) = REG(8) + 2*ISM
C--> BPEM� ?
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
9901   CONTINUE
       IER = 1
       RETURN
C----------------------------------------------------------------------
9902   CONTINUE
       IER = 2
CCCC   WRITE (6,*) IER
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
9906   CONTINUE
       IER = 6
       RETURN
       END
