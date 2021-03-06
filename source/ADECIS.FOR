       SUBROUTINE ADECIS( KD, ADPROG, REG, IADRS, IDATA, T, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT    MA�    1988 �.)-----------
C   �O��PO�PAMMA: ADECIS( KD, ADPROG, REG, IADRS, IDATA, T, IER )
C   -------------
C   HA�HA�EH�E:  �O�CK �O KO�� A�PECA���  KD ( �B�XPA�P��HOE Q-��C�O )
C   ----------   A�PECA � �AHHO�O B O��  ��� B  POH .
C                ( ��� KOMAH�, PA�OTA���X CO C�OBAM� )
C                ******************************************************
C
C   METO�:         �EPEKO��POBKA, O�PE�E�EH�E METO�A A�PECA��� � �O�CK.
C   -----
C
C   �APAMETP�:  BXO�H�E :
C  ----------      KD     - KO� A�PECA��� ;
C                  REG(8) - MACC�B PE��CTPOB M�KPO�PO�ECCOPA ;
C
C               B�XO�H�E :
C                  IADRS  - A�PEC  �CKOMO�O �AHHO�O :
C                           EC�� IADRS >= 0 - A�PEC ��E�K� �AM�T�;
C                           EC�� IADRS <  0 - A�PEC PE��CTPA .
C                                             ���O A�PEC �OPTA B/B .
C                  IDATA  - ��C�O, XPAH��EEC� �O A�PEC� 'IADRS';
C                  T      - BPEM� B��O�HEH�� KOMAH�� ;
C                  IER    - �KA�ATE�� O���K�:
C                           IER=0  -  O���OK HE O�HAP��EHO ;
C                           IER=1  -  O���O�H�� KO� A�PECA��� ,
C                                     ���O B�XO� �A �PE�E�� O�� ;
C                           IER=2  -  �O���EHH�� A�PEC ��E�K� �AM�T�
C                                     'IADRS' HE �ETH�� .
C
C   �O��PO�PAMM�:  1. HET.
C   ------------
C
C   �P�ME�AH�E:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), ADPROG
       IER = 0
C--> KAKO� METO� A�PECA��� ?
       METOD = KD/8 + 1
       NREG  = KD - 8*(METOD - 1) + 1
       GO TO (100, 200, 300, 400, 500, 600, 700, 800 ), METOD
       GO TO 9990
C######################################################################
C
C               �P�MA�  A�PECA���
C
C######################################################################
100    CONTINUE
C--> PE��CTPOB�� METO� A�PECA���.
C----------------------------------------------------------------------
       IADRS = - NREG
       IDATA = REG(NREG)
       T = 0.
       RETURN
C----------------------------------------------------------------------
300    CONTINUE
C--> ABTO�HKPEMEHTH�� METO� A�PECA���
C----------------------------------------------------------------------
       IADRS = REG(NREG)
       T = 1.6E-6
       REG(NREG) = REG(NREG) + 2
       GO TO 900
C----------------------------------------------------------------------
500    CONTINUE
C--> ABTO�EKPEMEHTH�� METO� A�PECA���
C----------------------------------------------------------------------
       REG(NREG) = REG(NREG) - 2
       IADRS = REG(NREG)
       T = 2.4E-6
       GO TO 900
C----------------------------------------------------------------------
700    CONTINUE
C--> �H�EKCH�� METO� A�PECA���
C----------------------------------------------------------------------
       INDEX = MEM( REG(8) + 1)
       REG(8) = REG(8) + 2
       IADRS = REG(NREG) + INDEX
       T = 4.8E-6
       GO TO 900
C######################################################################
C
C            KOCBEHHA�  A�PECA���
C
C######################################################################
200    CONTINUE
C--> KOCBEHHO-PE��CTPOB�� METO� A�PECA���
C----------------------------------------------------------------------
       IADRS = REG(NREG)
       T = 1.6E-6
       GO TO 900
C----------------------------------------------------------------------
400    CONTINUE
C--> KOCBEHHO-ABTO�HKPEMEHTH�� METO� A�PECA���
C----------------------------------------------------------------------
       IADRS = MEM( REG(NREG)+1)
       T = 4.0E-6
       REG(NREG) = REG(NREG) + 2
       GO TO 900
C----------------------------------------------------------------------
600    CONTINUE
C--> KOCBEHHO-ABTO�EKPEMEHTH�� METO� A�PECA���
C----------------------------------------------------------------------
       REG(NREG) = REG(NREG) - 2
       IADRS = MEM( REG(NREG)+1)
       T = 4.8E-6
       GO TO 900
C----------------------------------------------------------------------
800    CONTINUE
C--> KOCBEHHO-�H�EKCH�� METO� A�PECA���
C----------------------------------------------------------------------
       INDEX = MEM( REG(8) + 1)
       REG(8) = REG(8) + 2
       IAD   =  REG(NREG)  + INDEX
       IADRS = MEM(IAD+1)
       T = 7.2E-6
C----------------------------------------------------------------------
900    CONTINUE
C--> �POBEP�M A�PEC � �A�OMH�M �AHHOE.
       IF( IADRS .GE. 57344  .AND. IADRS .LT. 65536 ) GO TO 8888
       IF( IADRS .LT. 0      .OR.  IADRS .GT. ADPROG) GO TO 9990
       IDATA = MEM(IADRS+1)
       IF( MOD(IADRS,2) .GT. 0 ) GO TO 9999
       RETURN
C----------------------------------------------------------------------
8888   CONTINUE
C--> O�PAT���C� K �OPT� B/B.
       IADRS = - IADRS
       RETURN
C----------------------------------------------------------------------
9990   CONTINUE
       IER = 1
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
C--> C�E��A��HO ��� KOMAH� , PA�OTA���X C �A�TAM�
       IDATA = MEM(IADRS)
       IADRS = IADRS - 1
       IER = 2
       RETURN
C----------------------------------------------------------------------
       END
