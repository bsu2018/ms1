         SUBROUTINE SEQNT1( TIP, KTIP, NSM, KNSM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   SEQNT1( TIP, KTIP, NSM, KNSM )
C   ------------
C   HA�HA�EH�E:    �O HOMEP� T��A ��EMEHTA TIP � �O "CME�EH��"  NSM
C   ----------          H A X O � � T :
C                  A�PEC �MEH� T��A MO�E�� B TABTIP  ==> KTIP  �
C                  A�PEC �MEH� �EPEMEHHO� B TABTOK   ==> KNSM
C
C   METO�:         �O�CK �O TA����AM SERVZ  ��OKOB;
C   ------
C
C   �APAMETP�:     CM. HA�HA�EH�E;
C   ----------
C
C   �O��PO�PAMM�:  1. COMMON PROF1, SERVZ1, 2, 3
C   ------------
C   �P�ME�AH�E:    1. KO�� �MEH ��KC�POBAH� CO��ACHO SERVZ TA����;
C  -----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /SERVZ1/ TABSPC(1)
         COMMON /SERVZ2/          TABTOK(1)
         COMMON /SERVZ3/          TABTIP(1)
         INTEGER TABSPC, TIP, TIPX, TABTOK, TABTIP
         KTIP = (13-1)* 5      + 1
         KNSM = (13-1)* 3      + 1
         IX = 1
         TIPX = TIP
C--> �O TABSPC....
2        CONTINUE
         IF ( TABSPC(IX) .EQ. TIPX ) GO TO 1
         IF ( TABSPC(IX) .EQ. 0    ) RETURN
         IX = IX + 5
         GO TO 2
1        CONTINUE
C--> HET �� �EPEA�PECA���?
C--->   IX HA�A CTPOKA
         IF ( TABSPC(IX+1) .GT. 0 ) GO TO 3
C--->   �EPEA�PEC�EM � �O��EM...
         TIPX = - TABSPC(IX+1)
         IX = 1
         GO TO 2
3        CONTINUE
C--->  �O�CK B CTPOKE
         KODN = TABSPC(IX+1)
         KTIP = (KODN-1) * 5     + 1
C--> �O NSM KO� �MEH� �EPEMEHHO� KODP ��EM...
4        CONTINUE
         IF ( TABSPC(IX) .EQ. 0 ) RETURN
         IF ( TABSPC(IX) .NE. TIPX ) RETURN
         NS1 = TABSPC(IX+2)
         NS2 = TABSPC(IX+3)
C--->  HA�E CME�EH�E ME��� ?
         IF ( NS1 .LE. NSM  .AND.  NSM .LE. NS2 ) GO TO 5
C-->  �O�PO��EM B �P. CTPOKE...
         IX = IX + 5
         GO TO 4
5        CONTINUE
C-->  CTPOK� HA���, B��OP KO�A �MEH� "TOKA":
         KODP = TABSPC(IX+4)
         KNSM = (KODP-1) * 3     + 1
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
