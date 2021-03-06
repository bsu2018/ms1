         BLOCK DATA
C====>  SERVZ2  <======================================================
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C
C  TA����A �MEH �EPEMEHH�X "TABTOK":
C   COMMON / SERVZ2 /   TABTOK(1)
C
C+------------+----------------------+
CI KO� BXO�A  I    �M� �EPEMEHHO�    I
CI            I[ PA�MEP 3 C�OBA  ]   I
CI KO� �MEH�  I                      I
CI            I( XPAHEH�E ��OTHOE )  I
CI �EPEMEHHO� I                      I
C+------------+----------------------+
CI            I     1   2   3        I
C+------------+----------------------+
CI    1       I        TOK           I
CI    2       I       GAMMA          I
CI    3       I       OMEGA          I
CI    4       I  ��.MA�.MOMEHT [M�M] I
CI    5       I  M_BH.               I
CI    6       I  C��HA� ��PAB�EH��   I
CI    7       I  U_ ��PAB. "PB"      I
CI    8       I  OME�A_CET�_O�OPHA�  I
CI    9       I  CKO���EH�E          I
CI   10       I  U_��P. Q_C�HXPOHHOE I
CI   11       I  U_��P. D_C�HXPOHHOE I
CI   12       I  TOK BEHT���         I
CI** 13 ****  I    [??]              I   !!!  13  BCE��A  "[??]" !!!
CI   14       I  HA�P��EH�E          I
CI   15       I  MO���� C�MM� -> ABC I
CI   16       I  ��O� �A�A "A"<->V_M.I
CI   17       I  V_X HA "A".         I
CI   18       I  V_Y HA "A+90 �PA�." I
CI   19       I  U_S_Q_O�OP. (��.MA�)I
CI   20       I  U_S_D_O�OP. (��.MA�)I
CI   21       I  I_S_Q_O�OP. (��.MA�)I
CI   22       I  I_S_D_O�OP. (��.MA�)I
CI   23       I  U_R_Q_O�OP. (��.MA�)I
CI   24       I  U_R_D_O�OP. (��.MA�)I
CI   25       I  I_R_Q_O�OP. (��.MA�)I
CI   26       I  I_R_D_O�OP. (��.MA�)I
CI   27       I  �.�.C. PE�. BO����. I
CI   28       I  OME�A MO���� -> ABC.I
CI   29       I  M�H. MO�HOCT�       I
CI   30       I  I_S_D_�A�._ PB.     I
CI   31       I  S_�A�._ PB.         I
CI   32       I  INT( ISD ) _ PB.    I
CI   33       I  INT( S ) _ PB.      I
CI   34       I  �EPEME�EH�E         I
CI   35       I  ��H. CKOPOCT�       I
CI   36       I  ��. MA�H. C��A      I
CI   37       I  BHE�H�� HA�P. C��A  I
CI   38       I  KO� C��             I
CI   39       I  �A� ��MEHEH�� C��   I
CI   40       I  B�XO�HO� C��HA�     I
CI   41       I  AKT�BHA� MO�HOCT�   I
CI            I  KOM�EHC�P�EMO� HA�P.I
CI   42       I  O�MEHHA� MO�HOCT�   I
CI            I  KOM�EHCATOPA        I
CI   43       I  BPEM� ��MEHEH�� B�X.I
CI            I  C��HA�A             I
CI   44       I  POH M�KPO�PO�ECCOPA I
CI   45       I  CC� M�KPO�PO�ECCOPA I
CI   46       I  CO�EP. �OPTA B/B    I
CI   47       I  O�O��EHHA� KOOP�.   I
C+------------+----------------------+
C    �E�AT� �� TABTOK(IX) �O �OPMAT� 3A4  [ 12 C�MBO�OB ]
C
C  BXO� B TABTOK �O KOD_NAME:
C
C  IX = (KOD_NAME - 1) * 3  +  1
CZZZ-------------------------------------------------------------------
         COMMON /SERVZ2/   TABTOK(180)
         DIMENSION TABTO1(45), TABTO2(45), TABTO3(45)
         DIMENSION TABTO4(45)
         EQUIVALENCE( TABTOK(1) , TABTO1(1) )
         EQUIVALENCE( TABTOK(46), TABTO2(1) )
         EQUIVALENCE( TABTOK(91), TABTO3(1) )
         EQUIVALENCE( TABTOK(136), TABTO4(1) )
         INTEGER TABTOK, TABTO1, TABTO2, TABTO3, TABTO4
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C--->  HA  90 / 3  = 30  �MEH                                    <-----
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
        DATA TABTO1/ ' TOK', '    ', '    ',
     =               'GAMM', 'A PO', 'TOPA',
     =               'OMEG', 'A PO', 'TOPA',
     =               ' M. ', '��-M', 'A�. ',
     =               ' M. ', 'BHE�', 'H�� ',
     =               'C��H', 'A� C', '��. ',
     =               'U ��', 'P. "', 'PB".',
     =               'OMEG', ' C�C', 'TEM�',
     =               'CKO�', '��EH', '�E. ',
     =               'U ��', 'P. Q', ' O�.',
     =               'U ��', 'P. D', ' O�.',
     =               'TOK ', 'BEHT', '���.',
     =               '[??]', '[??]', '[??]',
     =               'HA�P', '��EH', '�E. ',
     =               'MO��', '��->', ' ABC' /
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       DATA TABTO2/ '��O�', ' OT ', '�. A',
     =              'V_X ', 'HA �', '."A"',
     =              'V_Y ', 'HA A', '+90�',
     =              'U_S_', 'Q_O�', 'OP. ',
     =              'U_S_', 'D_O�', 'OP. ',
     =              'I_S_', 'Q_O�', 'OP. ',
     =              'I_S_', 'D_O�', 'OP. ',
     =              'U_R_', 'Q_O�', 'OP. ',
     =              'U_R_', 'D_O�', 'OP. ',
     =              'I_R_', 'Q_O�', 'OP. ',
     =              'I_R_', 'D_O�', 'OP. ',
     =              '�.�.', 'C.->', '"PB"',
     =              'OMEG', 'A.MO', '����',
     =              'M�H.', 'MO�H', 'OCT�',
     =              'ISD_', '�A�.', '"PB"'   /
C######################################################################
       DATA TABTO3/ 'S_�A', '�. "', 'PB".',
     =              'INT(', 'ISD)', '"PB"',
     =              'INT(', 'S) "', 'PB".',
     =              '�EPE', 'ME�E', 'H�E.',
     =              '��H.', 'CKOP', 'OCT�',
     =              '��. ', 'MA�.', 'C��A',
     =              'BH.H', 'A�P.', 'C��A',
     =              'KO� ', 'C�� ', '    ',
     =              '�A� ', '��M.', ' C��',
     =              'B�X.', ' C��', 'HA� ',
     =              'P KO', 'M�.H', 'A�P.',
     =              'Q O�', 'MEHH', 'A�  ',
     =              'BPEM', '� ��', 'M.C�',
     =              'POH ', 'M�K.', '�PO�',
     =              'CC� ', 'M�K.', '�PO�' /
C######################################################################
       DATA TABTO4/ 'CO�.', '�OPT', '.B/B',
     =              'O�O�', '�.KO', 'OP�.',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....',
     =              '....', '....', '....' /
C######################################################################
C
C
         END
