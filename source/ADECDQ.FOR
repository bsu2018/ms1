       SUBROUTINE ADECDQ( KD, KQ, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT    MA�    1988 �.)-----------
C   �O��PO�PAMMA: ADECDQ( KD, KQ, IER )
C   -------------
C   HA�HA�EH�E:  �PO�PAMMA �EPEBO��T �E�OE �BO��HOE ��C�O  KD ,
C   ----------   �A�AHHOE B "10" �OPME B "8"  �OPM�.
C
C   METO�:         �EPEKO��POBKA.
C   -----
C
C   �APAMETP�:  BXO�H�E :
C  ----------      KD  - �A�AHHOE �BO��HOE ��C�O B "10# �OPME;
C
C               B�XO�H�E :
C                  KQ  - �PEO�PA�OBAHHOE �BO��HOE ��C�O B "8"
C                        �OPM� ;
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
       IF( KD .GT. 65535  ) GO TO 9999
C--> PA�O��EM �O PA�P��AM.
       KD5 = KD / 32768
       KDP = KD  - KD5 * 32768
       KD4 = KDP/ 4096
       KDP = KDP - KD4 * 4096
       KD3 = KDP/ 512
       KDP = KDP - KD3 * 512
       KD2 = KDP/ 64
       KDP = KDP - KD2 * 64
       KD1 = KDP/ 8
       KD0 = KDP - KD1 * 8
C--> PA����� �O PA�P��AM, TE�EP� HA��EM �EC�T��HOE ��C�O.
       KQ = KD0 + 10*KD1 + 100*KD2 + 1000*KD3 + 10000*KD4 + 100000*KD5
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
       IER = 1
       RETURN
       END
