         SUBROUTINE SEQNT0( TIP, KOD, NSM, IRC )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   SEQNT0( TIP, KOD, NSM, IRC )
C   ------------
C   HA�HA�EH�E:    �O HOMEP� T��A ��EMEHTA TIP O�PE�E��ET
C   -----------    CME�EH�E NSM, COOTBETCTB���EE KO��
C                  �MEH� �EPEMEHHO� KOD;
C   METO�:         �O�CK �O TA����E TABSPC;
C   ------
C
C   �APAMETP�:     TIP - T�� MO�E��
C   ----------     KOD - KO� �MEH� �EPEMEHHO�;
C                  NSM - "CME�EH�E";
C                  IRC = 1  HA���,
C                  IRC = 2  O���KA;
C   �O��PO�PAMM�:  1. COMMON PROF1, SERVZ1
C   ------------
C   �P�ME�AH�E:    1. KO�� �MEH ��KC�POBAH� CO��ACHO SERVZ TA����;
C  -----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /SERVZ1/ TABSPC(1)
         INTEGER TABSPC, TIP, TIPX
         IRC = 2
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
         IRC = 2
         IF ( TABSPC(IX+4) .EQ. KOD ) GO TO 4
C-->  MO�ET ��T� B �P. CTPOKE
         IX = IX + 5
         IF ( TABSPC(IX) .NE. TIPX ) RETURN
         IF ( TABSPC(IX) .EQ. 0 ) RETURN
         GO TO 3
4        CONTINUE
         IRC = 1
C-->  CTPOK� HA���, B��OP CME�EH�� "OT"
         NSM = TABSPC(IX+2)
C--->  KOHTPO��
         IF ( NSM. EQ. 1 ) RETURN
C!!!-------------------------------------------------------------------
         IF ( TABSPC(IX+2) .EQ. TABSPC(IX+3) ) RETURN
         NWTRE=MASPRT(14)
         WRITE(NWTRE,5) IX
5        FORMAT(' *** B TA����E TABSPC CTPOKA ', I4, ' �A�O�HEHA ',
     =   'HE KOPPEKTHO ***')
C!!!-------------------------------------------------------------------
         RETURN
         END
