         BLOCK DATA
C===>  SERVZ3  <=======================================================
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C
C           TA����A �MEH MAKPOMO�E�E� "TABTIP"
C          COMMON / SERVZ3 /  TABTIP(1)
C
C+--------------+----------------------------------+
CI A�PEC BXO�A  I      �M� MO�E��                  I
CI              I                                  I
CI KO� �MEH�    I [ ���HA CTPOK� 5 C�OB ]          I
CI  MO�E��      I   XPAHEH�E ��OTHOE  (20 C�MBO�OB)I
C+--------------+----------------------------------+
CI              I    5   C�OB  B  CTPOKE           I
C+--------------+----------------------------------+
CI   1          I  CO�POT�B�EH�E                   I
CI   2          I  �H��KT�BHOCT�                   I
CI   3          I  EMKOCT�                         I
CI   4          I  �CTO�H�K �OCTO�HHO�O TOKA       I
CI   5          I  �.�.C. �OCTO�HHA�               I
CI   6          I  �.�.C. SIN - A�.                I
CI   7          I  RL - ��O�                       I
CI   8          I  ��O�                            I
CI   9          I  BEHT���                         I
CI   10         I  MOCT �AP�OHOBA                  I
CI   11         I  ��. MA��HA 3X3X0                I
CI   12         I  RL - BEHT���                    I
CI   13         I  [??]                            I
CI   14         I  ��EA��H�� K��� (PE�E)           I
CI   15         I  TPAHC�OPMATOP                   I
CI   16         I  HE��HE�HOE CO�POT�B�EH�E        I
CI   17         I  HE��HE�HA� �H��KT�BHOCT�        I
CI   18         I  PE����TOP BO�����EH�� 3X3X0     I
CI   19         I  BO�����TE�� 3X3X0 H��E 3�-HO�   I
CI   20         I  �AT��K HA�P��EH��               I
CI   21         I  �.�.C. 3-X �A�HA� SIN-�         I
CI   22         I  �.�.C. PE����TOPA BO�����EH��   I
CI   23         I  BEHT���H�� K���                 I
CI   24         I  BEHT���, ��PAB��EM�� C���       I
CI   25         I  �A�OB�� �B��ATE��               I
CI   26         I  C���                            I
CI   27         I  K��� , ��PAB��EM�� C���         I
CI   28         I  � - PE����TOP                   I
CI   29         I  � - PE����TOP                   I
CI   30         I  �HEP��OHHOE �BEHO               I
CI   31         I  � - PE����TOP                   I
CI   32         I  J  SIN - HA�                    I
CI   33         I  J  ��PAB�. �/� �O���OBATE��     I
CI   34         I  �� - PE����TOP                  I
CI   35         I  �� - PE����TOP                  I
CI   36         I  �A�OBPA�ATE��                   I
CI   37         I  KO��E�ATE��HOE �BEHO            I
CI   38         I  HE��HE�HOE �BEHO                I
CI   39         I  PE�E�HOE �BEHO                  I
CI   40         I  �BEHO "��CTOE �A�A���BAH�E"     I
CI   41         I  �MHO��TE��                      I
CI   42         I  �T�H                            I
CI   43         I  �H�H                            I
CI   44         I  ��EA��H�� O�EPA��OHH�� �C���TE��I
CI   45         I  ��EMEHT "�"                     I
CI   46         I  ��EMEHT "���"                   I
CI   47         I  ��EMEHT "�CK�.���"              I
CI   48         I  RS-TP���EP                      I
CI   49         I   D-TP���EP                      I
CI   50         I   T-TP���EP                      I
CI   51         I  KOM�APATOP                      I
CI   52         I  �B��ATE�� �OCTO�HHO�O TOKA      I
CI   53         I  3-X �A�H�� KOM�EHCATOP          I
CI   54         I  KBA�PATH�� KOPEH�               I
CI   55         I �EHEPATOP ���OO�PA�HO�O C��HA�A  I
CI   56         I �H�T                             I
CI   57         I M�KPO�PO�ECCOP K1801BM1(2,3)     I
CI   58         I MEXAH��ECKA� C�CTEMA             I
CI   59         I ��E�O-KOM�EHCATOP                I
CI              I                                  I
CI              I                                  I
CI              I                                  I
C+--------------+----------------------------------+
C �P� LTABNM = 5 �E�AT� �� TABNM(IX) B �OPMATE 5A4 [ 20 C�MBO�OB ]
C
C BXO� �O KOD_NAME :
C
C  IX = (KOD_NAME  -  1  ) * 5  + 1
C
CZZZ-------------------------------------------------------------------
         COMMON /SERVZ3/  TABTIP(300)
         DIMENSION TABTI1(50), TABTI2(50), TABTI3(50), TABTI4(50)
         DIMENSION TABTI5(50), TABTI6(50)
         EQUIVALENCE( TABTIP(1)  , TABTI1(1) )
         EQUIVALENCE( TABTIP(51) , TABTI2(1) )
         EQUIVALENCE( TABTIP(101), TABTI3(1) )
         EQUIVALENCE( TABTIP(151), TABTI4(1) )
         EQUIVALENCE( TABTIP(201), TABTI5(1) )
         EQUIVALENCE( TABTIP(251), TABTI6(1) )
C--->  HA  300 /  5  =  60  �MEH
         INTEGER TABTIP, TABTI1, TABTI2, TABTI3, TABTI4, TABTI5, TABTI6
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        DATA  TABTI1/ 'CO�P', 'OT�B', '�EH�', 'E   ', '    ',
     2                '�H��', 'KT�B', 'HOCT', '�   ', '    ',
     3                'EMKO', 'CT� ', '    ', '    ', '    ',
     4                '�CTO', '�H�K', ' �OC', 'T. T', 'OKA ',
     5                '�.�.', 'C. �', 'OCTO', '�HHA', '�   ',
     6                '�.�.', 'C. S', 'IN -', ' A� ', '    ',
     7                'RL_�', '�O� ', '    ', '    ', '    ',
     8                '��O�', '    ', '    ', '    ', '    ',
     9                'BEHT', '��� ', '    ', '    ', '    ',
     A                'MOCT', ' �AP', '�OHO', 'BA  ', '    ' /
       DATA  TABTI2/  '��. ', 'MA�.', ' 3X3', 'X0  ', '    ',
     2                'RL_B', 'EHT�', '��  ', '    ', '    ',
     3                '[??]', '[??]', '[??]', '[??]', '[??]',
     4                '��EA', '��H�', '� K�', '�� -', 'PE�E',
     5                'TPAH', 'C�OP', 'MATO', 'P   ', '    ',
     6                'HE��', 'H. C', 'O�PO', 'T�B�', 'EH�E',
     7                'HE��', 'H. �', 'H��K', 'T�BH', 'OCT�',
     8                'PE�.', ' BO�', '���.', ' 3X3', 'X0. ',
     9                'BO��', '���T', 'E�� ', 'H��E', '-3�0',
     A                '�AT�', '�K H', 'A�P�', '�EH�', '�.  '  /
C
       DATA TABTI3/ '�.�.', 'C. 3', '-� �', 'A�� ', 'SIN.',
     2              '�.�.', 'C. P', 'E�. ', 'BO��', '��-�',
     3              'BEHT', '���H', '�� K', '���-', '2_TP',
     4              'BEHT', '���,', ' ��P', 'AB�.', 'C���',
     5              '�A�O', 'B�� ', '�B��', 'ATE�', '�.  ',
     6              '( C�', '�� )', '    ', '    ', '    ',
     7              'K���', ', ��', 'PAB�', '�EM.', 'C���',
     8              '� - ', 'PE��', '��TO', 'P   ', '    ',
     9              '� - ', 'PE��', '��TO', 'P   ', '    ',
     A              '�HEP', '��OH', 'HOE ', '�BEH', 'O   ' /
       DATA TABTI4/ '� - ', 'PE��', '��TO', 'P   ', '    ',
     2              '�CTO', '�H�K', ' SIN', ' TOK', 'A   ',
     3              '�CT.', 'TOKA', '��P.', '�O��', '�OB.',
     4              '�� -', 'PE��', '��TO', 'P   ', '    ',
     5              '�� -', 'PE��', '��TO', 'P   ', '    ',
     6              '�A�O', 'BPA�', 'ATE�', '�   ', '    ',
     7              'KO�E', '�ATE', '��HO', 'E  �', 'BEHO',
     8              'HE��', 'HE�H', 'OE  ', '   �', 'BEHO',
     9              'PE�E', '�HOE', '    ', '   �', 'BEHO',
     A              '�BEH', 'O "�', '�CTO', 'E �A', '�." ' /
       DATA TABTI5/ '�MHO', '��TE', '��  ', '    ', '    ',
     2              '�CT.', 'TOKA', ' ��P', '.HA�', 'P.  ',
     3              '�CT.', 'HA�P', '.��P', '.HA�', 'P.  ',
     4              '��EA', '��H�', '� O�', '.�C�', '��T.',
     5              '�O�.', '��EM', 'EHT ', '"�" ', '    ',
     6              '�O�.', '��EM', 'EHT ', '"���', '"   ',
     7              '�O�.', '��EM', '."�C', 'K�.�', '��" ',
     8              'RS -', 'TP��', '�EP ', '    ', '    ',
     9              ' D -', 'TP��', '�EP ', '    ', '    ',
     A              ' T -', 'TP��', '�EP ', '    ', '    ' /
       DATA TABTI6/ 'KOM�', 'APAT', 'OP  ', '    ', '    ',
     2              '�B��', 'ATE�', '� �O', 'CT. ', 'TOKA',
     3              '3-X ', '�A�H', '.KOM', '�EHC', 'ATOP',
     4              'KBA�', 'PATH', '�� K', 'OPEH', '�   ',
     5              '�EHE', 'PATO', 'P ��', '.C��', 'HA�A',
     6              '�H�T', '    ', '    ', '    ', '    ',
     7              'M�K.', '�PO�', 'ECC.', ' K18', '01BM',
     8              'MEXA', 'H��E', 'CKA�', ' C�C', 'TEMA',
     9              '��E�', 'O-KO', 'M�EH', 'CATO', 'P   ',
     A              '[??]', '[??]', '[??]', '[??]', '[??]' /
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
