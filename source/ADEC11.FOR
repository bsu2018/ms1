       SUBROUTINE ADEC11( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (AT OCTOBER  1989  �.)-----------
C   �O��PO�PAMMA: ADEC11( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )

C   HA�HA�EH�E:  O�PA�OT��K �B�XA�PECH�X KOMAH�
C   ----------   M�KPO�PO�ECCOPA  K1801BM1(2,3)  ( PA�OTA C �A�TAM� )
C              *------------------------------------------------------*
C              * KOMAH�A  "MOVB"( 11SSDD Q ):                          *
C              * O�EPAH� �CTO�H�K   <SS> �EPEC��AETC� �O A�PEC�       *
C              * O�EPAH�A �P�EMH�KA <DD> .                            *
C              *------------------------------------------------------*
C              * KOMAH�A  "CMPB"( 12SSDD Q ):                          *
C              * CPABH�BAETC� O�EPAH� �CTO�H�KA <SS> � O�EPAH�  �P�-  *
C              * EMH�KA <DD> � ��MEH��TC� �P��HAK�. O�A O�EPAH�A HE   *
C              * ��MEH��TC�.                                          *
C              *------------------------------------------------------*
C              * KOMAH�A  "BITB"( 13SSDD Q ):                          *
C              * B��O�H�ET �O���ECK�� ��HK���  "�" HA� O�EPAH�OM      *
C              * �CTO�H�KA <SS> � O�EPAH�OM  �P�EMH�KA <DD>, ��MEH��  *
C              * COOTBETCTB����M O�PA�OM �P��HAK�. O�A O�EPAH�A HE    *
C              * ��MEH��TC�.                                          *
C              *------------------------------------------------------*
C              * KOMAH�A  "BICB"( 14SSDD Q ):                          *
C              * O���AET KA���� PA�P�� O�EPAH�A �P�EMH�KA  <DD>,      *
C              * COOTBETCTB����� �CTAHOB�EHHOM� PA�P��� O�EPAH�A      *
C              * �CTO�H�KA <SS>. CO�EP��MOE  <SS>  HE ��MEH�ETC�.     *
C              *------------------------------------------------------*
C              * KOMAH�A  "BISB"( 15SSDD Q ):                          *
C              * B��O�H�ET �O���ECK�� O�EPA��� "���" HA� CO�EP��M�M   *
C              * O�EPAH�A �CTO�H�KA <SS> � O�EPAH�A �P�EMH�KA <DD> �  *
C              * �A��C�BAET PE����TAT �O A�PECC�  <DD>.               *
C              * CO�EP��MOE  <SS>  HE ��MEH�ETC�.                     *
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
C                  IBAI1 -  CTAP��� PA�P��
C                  IBAI2 -  M�A���� PA�P��
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
C
C
C   �O��PO�PAMM�:  1. ADECIS, ADECDB, ADECBD ;
C   ------------
C
C   �P�ME�AH�E:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6),  ADPROG, KB(16), KB1(16)
       INTEGER  ADPRTO, ADPRTI
       IER = 0
       KD = KD - 32768
       KOM = KD / 4096
C----------------------------------------------------------------------
C--> ��EM O�EPAH� �CTO�H�K ( A�PEC--> IADRS,  ��C�O--> IDATA )
       KD1 = ( KD - KOM*4096 ) / 64
       CALL ADECIS( KD1, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .EQ. 1 ) RETURN
       IER0 = IER
       IF( IADRS .GE.-8 ) GO TO 10
C--> �A�POC HA BBO� �AHH�X �� �OPTA BBO�A
CCCC   WRITE (6,*) ADPRTI,ADPRTO
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT1
10     CONTINUE
C----------------------------------------------------------------------
C--> ��EM O�EPAH� �P�EMH�K ( A�PEC--> IADRS1,  ��C�O--> IOLD  )
       KD1 = KD - KOM*4096 - KD1*64
       CALL ADECIS( KD1, ADPROG, REG, IADRS1, IOLD, TO, IER )
       IF( IER   .EQ. 1 ) RETURN
       IER1 = IER
       IF( IADRS1.GE.-8 ) GO TO 20
C--> �A�POC HA B�BO� �AHH�X B �OPT  B�BO�A
       IF( (-IADRS1) .NE. ADPRTO ) GO TO 9904
       IOLD = IPORT
20     CONTINUE
C----------------------------------------------------------------------
C -->  PA�O��EM C�OBO HA �A�T�
       IBAI1 = IDATA / 256
       IBAI2 = IDATA - IBAI1*256
C--> KAKO� �A�T O�PABAT�BAEM ?
       IDATA = IBAI2
       IF ( IER0.EQ.2 ) IDATA = IBAI1
C -->  PA�O��EM C�OBO HA �A�T�
       IBAI3 = IOLD / 256
       IBAI4 = IOLD - IBAI3*256
C--> KAKO� �A�T O�PABAT�BAEM ?
       IOLD = IBAI4
       IF ( IER1.EQ.2 ) IOLD = IBAI3
C--> PA�O��EMC� �O KOMAH�AM :
       GO TO ( 100,  200,  300,  400,  500, 9903, 9903, 9903, 9903,
     *        9903, 9903, 9903, 9903,9903, 9903 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAH�A  "MOVB"
C----------------------------------------------------------------------
100    CONTINUE
       IF( IADRS1 .LT. 0 ) GO TO 110
       IF( IER1.EQ. 2 ) GO TO 105
       MEM(IADRS1+1)=(MEM(IADRS1+1)/256)*256 + IDATA
       GO TO 120
105    CONTINUE
       MEM(IADRS1+1)= MEM(IADRS1+1) + (IDATA -
     * MEM(IADRS1+2))*256
       MEM(IADRS1+2)= IDATA
       GO TO  120
110    CONTINUE
       IF( IADRS1 .LT.-8 ) GO TO 115
       IREG1 = REG(-IADRS1)/32768
       TREG2 = REG(-IADRS1) - IREG1*32768
       REG(-IADRS1) = (IREG2 / 256)*256 + IDATA
       IF (IDATA.GT.128) REG(-IADRS1) = REG(-IADRS1)+32768
       GO TO 120
115    CONTINUE
       IPORT = (IPORT / 256) + IDATA
120    CONTINUE
       IER = 0
C--> �OC��TAEM BPEM�
       TB = 4.4E-6
       T = TB + TI + TO
C--> �A�MEMC� �P��HAKAM�.
       FLAG(2) = 0
       FLAG(3) = 0
       IF( IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( IDATA .GE. 32768 ) FLAG(4) = 1
       RETURN
C######################################################################
C----------------------------------------------------------------------
C              KOMAH�A  "CMPB"
C----------------------------------------------------------------------
200    CONTINUE
        IDATA = IDATA  - IOLD
C--> �OC��TAEM BPEM�
       TB = 3.2E-6
       IF( TI .EQ. 0 .AND. TO .EQ. 0 ) TB = 3.6E-6
       T = TB + TI + TO
C--> �A�MEMC� �P��HAKAM�.
       FLAG(1) = 0
       IF(IDATA .GE. 0 ) GO TO 205
       FLAG(1) = 1
       IDATA = 256 + IDATA
 205   CONTINUE
       FLAG(2) = 0
C      IF( IDATA.GE.32768.AND.IOLD.LT.
C    *32768.AND.INEW.LT.32768)    FLAG(2)=1
C      IF( IDATA.LT.32768.AND.IOLD.GE.
C    *32768.AND.INEW.GE.32768)    FLAG(2)=1
       FLAG(3) = 0
       IF(IDATA.EQ.0)  FLAG(3) = 1
       FLAG(4) = 0
C      IF(INEW.GE.32768)      FLAG(4)=1
       IER = 0
       RETURN
C######################################################################
C----------------------------------------------------------------------
C              KOMAH�A  "BITB"
C----------------------------------------------------------------------
300    CONTINUE
C--> PA�O��EM O�EPAH� �CTO�H�K �O CTE�EH�M �BO�K�
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
C--> PA�O��EM O�EPAH� �P�EMH�K �O CTE�EH�M �BO�K�
       CALL ADECDB( IOLD, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C----------------------------------------------------------------------
       ISUM = 0
       DO 310 I=1,8
       KB(I) = KB(I) * KB1(I)
       ISUM  = KB(I) + ISUM
310    CONTINUE
C--> �OC��TAEM BPEM�
       TB = 4.4E-6
       T = TB + TI + TO
C--> �A�MEMC� �P��HAKAM�.
       FLAG(2) = 0
       FLAG(3) = 0
       IF( ISUM .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = KB(8)
       IER = 0
       RETURN
C######################################################################
C______________________________________________________________________
C                    KOMAH�A "BICB"
C______________________________________________________________________
400    CONTINUE
C-->  PA�O��EM O�EPAH� �CTO�H�K �O CTE�EH�M �BO�K�
       CALL  ADECDB(IDATA,KB,IER)
       IF(IER.NE.0)  RETURN
C-->  PA�O��EM O�EPAH� �P�EMH�K �O CTE�EH�M �BO�K�
       CALL ADECDB(IOLD,KB1,IER)
       IF( IER. NE. 0 )  RETURN
C----------------------------------------------------------------------
       ISUM = 0
       DO 410  I=1,8
       IF( KB(I) .EQ. 1 ) KB1(I) = 0
       ISUM = ISUM + KB1(I)
410    CONTINUE
C-->   �PEO�PA��EM PE����TAT B �EC�T��HOM B��E
       CALL ADECBD(IDATA,KB1,IER)
C-->  �A��C�BAEM PE����TAT
       GO TO 100
C######################################################################
C______________________________________________________________________
C                    KOMAH�A "BISB"
C______________________________________________________________________
500    CONTINUE
C-->  PA�O��EM O�EPAH� �CTO�H�K �O CTE�EH�M �BO�K�
       CALL  ADECDB(IDATA,KB,IER)
       IF(IER.NE.0)  RETURN
C-->  PA�O��EM O�EPAH� �P�EMH�K �O CTE�EH�M �BO�K�
       CALL ADECDB(IOLD,KB1,IER)
       IF( IER. NE. 0 )  RETURN
C----------------------------------------------------------------------
       ISUM = 0
       DO 510  I=1,8
       IF( KB(I). EQ. 1 ) KB1(I) = 1
       ISUM = ISUM + KB1(I)
510    CONTINUE
C-->   �PEO�PA��EM PE����TAT B �EC�T��HOM B��E
       CALL ADECBD(IDATA,KB1,IER)
C-->  �A��C�BAEM PE����TAT
       GO TO 100
C######################################################################
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