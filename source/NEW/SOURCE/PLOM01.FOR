       INTEGER FUNCTION PLOM01( I, J, RAB )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA_��HK���      PLOM01( I, J, RAB )
C   -----------
C   HA�HA�EH�E:   CPABHEH�E  I � J ��EMEHTA O��CAH�� MACC�B� "RAB":
C   -----------    ( CM. PLOMXY )  ->  �PEBECHA� COPT�POBKA MACC�ABA
C                                      OTO�PA�EH��;
C   METO�:        ��EMEHT CTP�KT�P� B RAB -> 2-C�OBA: IX,IY;
C   -----
C   �APAMETP�:    I, J - HOMEPA CPABH�BAEM�X   TO�EK (IX,IY) B RAB;
C   ---------      RAB - MACC�B OTO�PA�EH�� ��� PLOMXY;
C   �O��PO�PAMM�:  HET;
C   -------------
C   �P�ME�AH�E:   PLOM01 -> -1  (I<J); =0 (I=J);  =+1 (I>J);
C   ----------    CM.  "�PEB. COPT�POBKA";
CZZZ-------------------------------------------------------------------
       DIMENSION RAB(1)
       INTEGER RAB, XI, XJ
CVVVVV INTEGER  PLOM01
       XI = RAB( 2*I - 1 )
       XJ = RAB( 2*J - 1 )
        IF ( XI - XJ ) 1, 2, 3
1      PLOM01 = -1
       RETURN
2      PLOM01 = 0
       RETURN
3      PLOM01 = +1
       RETURN
       END
