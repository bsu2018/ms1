       SUBROUTINE  SUPPL1 (N ,TJN1,TJN2 ,TJ1 ,TJ2, G )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (AT OCTOBER   1989 �.)-----------
C   �O��PO�PAMMA: SUPPL1(N ,TJN1,TJN2, TJ1 ,TJ2, G )
C   -------------
C   HA�HA�EH�E:  B���C��ET O���� �CTO�H�K TOKA 2**N �OC�E�OBATE��HO
C   ----------   �O�CE��HEHH�X TPEX-�O��CH�KAM� C �CTO�H�KAM� TOKA
C                TJN1 , TJN2
C                PE����TAT B TJ1, TJ2
C                BC�OMO�ATE��H�E �HA�EH�� G - MATP��A �POBO��MOCT�
C
C                O�C����BAET �O��PO�PAMMA DM0039
C
CZZZ--------------------------------------------------------------------
       DIMENSION TJN1(1), TJN2(1),G(1)
C--> �P���P�EM �O CTE�EH�M �BO�K�
       DO 10 K =1,N
       M = 2**(N-K)
       AB = G(2*K)/(2*G(2*K - 1))
       DO 10 I = 1,M
       A = AB*(TJN2(2*I-1) + TJN1(2*I))
       TJN1(I) = TJN1(2*I-1) - A
       TJN2(I) = TJN2(2*I) - A
10     CONTINUE
       TJ1 = TJN1(1)
       TJ2 = TJN2(1)
       RETURN
       END
