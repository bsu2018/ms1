       SUBROUTINE ADECBD( KD, KB, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT   AB��CT  1988 �.)-----------
C   �O��PO�PAMMA: ADECBD( KD, KB, IER )
C   -------------
C   HA�HA�EH�E:  �PO�PAMMA �PEO�PA��ET �BO��HOE ��C�O B �EC�T��HOE
C   ----------
C
C   METO�:         �EPEKO��POBKA.
C   -----
C
C
C   �APAMETP�:  BXO�H�E :
C   ---------    KB(1) - PA�P�� "0" �BO��HO�O ��C�A ;
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
C
C              B�XO�H�E:
C                 KD  -   �O���EHHOE �EC�T��HOE ��C�O   ;
C                  IER - ��A� O���K�:
C                        IER=0 - O���OK HET;
C                        IER=1 - PA�P�� �BO��HO�O ��C�A �MEET
C                                HE�O��CT�MOE �HA�EH�E ;
C
C
C
C   �O��PO�PAMM�:  1. HET.
C   ------------
C
C   �P�ME�AH�E:    1. HET.
C   -----------
CZZZ-------------------------------------------------------------------
       INTEGER KB(16)
       IER = 0
C--> CK�A��BAEM �O PA�P��AM.
       KD = 0
       DO 100 I=1,16
       I1 = 2**(I-1)
       IF( KB(I) .GT. 1 ) IER = 1
       KD = KD + KB(I) * I1
100    CONTINUE
       RETURN
       END
