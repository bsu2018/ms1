       SUBROUTINE ADECDB( KD, KB, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT    MA�    1988 �.)-----------
C   �O��PO�PAMMA: ADECDB( KD, KB, IER )
C   -------------
C   HA�HA�EH�E:  �PO�PAMMA PA���BAET �E�OE �EC�T��HOE ��C�O  KD ,
C   ----------   �O CTE�EH�M �BO�K� .
C
C   METO�:         �EPEKO��POBKA.
C   -----
C
C   �APAMETP�:  BXO�H�E :
C  ----------      KD  - �A�AHHOE �EC�T��HOE ��C�O ;
C
C               B�XO�H�E :
C                KB(1) - PA�P�� "0" �BO��HO�O ��C�A ;
C                KB(2) - PA�P�� "1" �BO��HO�O ��C�A ;
C                KB(3) - PA�P�� "2" �BO��HO�O ��C�A ;
C                KB(4) - PA�P�� "3" �BO��HO�O ��C�A ;
C                KB(5) - PA�P�� "4" �BO��HO�O ��C�A ;
C                KB(6) - PA�P�� "5" �BO��HO�O ��C�A ;
C                KB(7) - PA�P�� "6" �BO��HO�O ��C�A ;
C                KB(8) - PA�P�� "7" �BO��HO�O ��C�A ;
C                KB(9) - PA�P�� "8" �BO��HO�O ��C�A ;
C                KB(10)- PA�P�� "9" �BO��HO�O ��C�A ;
C                KB(11)- PA�P�� "10" �BO��HO�O ��C�A ;
C                KB(12)- PA�P�� "11" �BO��HO�O ��C�A ;
C                KB(13)- PA�P�� "12" �BO��HO�O ��C�A ;
C                KB(14)- PA�P�� "13" �BO��HO�O ��C�A ;
C                KB(15)- PA�P�� "14" �BO��HO�O ��C�A ;
C                KB(16)- PA�P�� "15" �BO��HO�O ��C�A ;
C                  IER - ��A� O���K�:
C                        IER=0 - O���OK HET;
C                        IER=1 - �A�AHHOE �EC�T��HOE ��C�O �O���E
C                                ��C�A "65535", �TO COOTBETCTB�ET
C                                MAKC�MA��HOM� ��C�� 16-T� PA�P��HO�O
C                                �BO��HO�O ��C�A .
C
C   �O��PO�PAMM�:  1. HET.
C   ------------
C
C   �P�ME�AH�E:    1. HET.
C   -----------
CZZZ-------------------------------------------------------------------
       INTEGER KB(16)
       IER = 0
       IF( KD .GT. 65535 ) GO TO 9999
C--> PA�O��EM �O PA�P��AM.
       KDP = KD
       DO 100 I=1,16
       I1 = 17 - I
       I2 = 2**(I1-1)
       KB(I1) = KDP / I2
       KDP = KDP - KB(I1) * I2
100    CONTINUE
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
       IER = 1
       RETURN
       END
