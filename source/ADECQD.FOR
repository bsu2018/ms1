       SUBROUTINE ADECQD( KQ, KD, KQ0, KQ1, KQ2, KQ3, KQ4, KQ5, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT    MA�    1988 �.)-----------
C   �O��PO�PAMMA: ADECQD( KQ, KD, KQ0, KQ1, KQ2, KQ3, KQ4, KQ5, IER )
C   -------------
C   HA�HA�EH�E:  �PO�PAMMA �EPEBO��T �E�OE �BO��HOE ��C�O  KQ ,
C   ----------   �A�AHHOE B BOC�M�P��HO� �OPME B �EC�T��H�� �OPM�
C                C O�HOBPEMEHHO� �OPA�P��HO� PA���BKO� �A�AHHO�O
C                BOC�M�P��HO�O ��C�A.
C
C   METO�:         �EPEKO��POBKA.
C   -----
C
C   �APAMETP�:  BXO�H�E :
C  ----------      KQ  - �A�AHHOE �BO��HOE ��C�O B BOC�M�P��HO� �OPME;
C
C               B�XO�H�E :
C                  KD  - �PEO�PA�OBAHHOE �BO��HOE ��C�O B �EC�T��H��
C                        �OPM� ;
C                  KQ0 - PA�P�� "0" BOC�M�P��HO�O ��C�A ;
C                  KQ1 - PA�P�� "1" BOC�M�P��HO�O ��C�A ;
C                  KQ2 - PA�P�� "2" BOC�M�P��HO�O ��C�A ;
C                  KQ3 - PA�P�� "3" BOC�M�P��HO�O ��C�A ;
C                  KQ4 - PA�P�� "4" BOC�M�P��HO�O ��C�A ;
C                  KQ5 - PA�P�� "5" BOC�M�P��HO�O ��C�A ;
C                  IER - ��A� O���K�:
C                        IER=0 - O���OK HET;
C                        IER=1 - �A�AHHOE BOC�M�P��HOE ��C�O �O���E
C                                ��C�A "177777", �TO COOTBETCTB�ET
C                                MAKC�MA��HOM� ��C�� 16-T� PA�P��HO�O
C                                �BO��HO�O ��C�A .
C
C   �O��PO�PAMM�:  1. HET.
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� HE�PAB���HOCT� �A��C� BOC�M�P��HO�O
C   -----------       ��C�A HE �PO��BO��TC�.
C
CZZZ-------------------------------------------------------------------
       IER = 0
       IF( KQ .GT. 177777 ) GO TO 9999
C--> PA�O��EM �O PA�P��AM.
       KQ5 = KQ / 100000
       KQP = KQ  - KQ5 * 100000
       KQ4 = KQP/ 10000
       KQP = KQP - KQ4 * 10000
       KQ3 = KQP/ 1000
       KQP = KQP - KQ3 * 1000
       KQ2 = KQP/ 100
       KQP = KQP - KQ2 * 100
       KQ1 = KQP/ 10
       KQ0 = KQP - KQ1 * 10
C--> PA����� �O PA�P��AM, TE�EP� HA��EM �EC�T��HOE ��C�O.
       KD = KQ0 + 8*KQ1 + 64*KQ2 + 512*KQ3 + 4096*KQ4 + 32768*KQ5
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
       IER = 1
       RETURN
       END
