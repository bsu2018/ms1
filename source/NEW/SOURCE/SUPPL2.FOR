       SUBROUTINE  SUPPL2 (TJ1,TJ2 ,TJ3 ,TJ4,B,D)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (AT OCTOBER   1989 �.)-----------
C   �O��PO�PAMMA: SUPPL1(TJ1,TJ2, TJ3 ,TJ4,B,D)
C   -------------
C   HA�HA�EH�E:  B���C��ET O���� �CTO�H�K TOKA �B�X �OC�E�OBATE��HO
C   ----------   �O�CE��HEHH�X TPEX-�O��CH�KAM� C �CTO�H�KAM� TOKA
C                TJ1 , TJ2  � TJ3,TJ4
C                PE����TAT B TJ1, TJ2
C                BC�OMO�ATE��H�E �HA�EH�� B,D
C                KOT-P�E B���C�EH� B FM0035
C                O�C����BAET �O��PO�PAMMA DM0039
C
CZZZ--------------------------------------------------------------------
       TJ1 = TJ1 - B*(TJ2 + TJ3)
       TJ2 = TJ4 - D*(TJ2 + TJ3)
       RETURN
       END
