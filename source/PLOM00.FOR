       SUBROUTINE PLOM00( I, J, RAB )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O�PO�PAMMA:  PLOM00( I, J, RAB )
C   -----------
C   HA�HA�EH�E:   �EPECTAHOBKA I � J ��EMEHTA O��CAH�� MACC�B� "RAB":
C   -----------    ( CM. PLOMXY )  ->  �PEBECHA� COPT�POBKA MACC�ABA
C                                      OTO�PA�EH��;
C   METO�:        ��EMEHT CTP�KT�P� B RAB -> 2-C�OBA: IX,IY;
C   -----
C   �APAMETP�:    I, J - HOMEPA �EPECTAB��EM�X TO�EK (IX,IY) B RAB;
C   ---------      RAB - MACC�B OTO�PA�EH�� ��� PLOMXY;
C   �O��PO�PAMM�:  HET;
C   -------------
C   �P�ME�AH�E:
C   ----------
CZZZ-------------------------------------------------------------------
       DIMENSION RAB(1)
       INTEGER RAB, XI, YI
       XI = RAB( 2*I - 1 )
       YI = RAB( 2*I )
C-->   "J"  B  "I":
       RAB(2*I-1) = RAB(2*J-1)
       RAB(2*I)   = RAB(2*J)
C--->   "I"   B  "J":
       RAB(2*J-1) = XI
       RAB(2*J)   = YI
       RETURN
       END
