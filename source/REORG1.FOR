         SUBROUTINE REORG1( K, L, ECHANG, COMPCL,  BA0)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   REORG1( K, L, ECHANG, COMPCL,  BA0)
C   -------------
C   HA�HA�EH�E:    "PEOP�AH��A���" COPT�P�EMO� �OC�E�OBATE��HOCT�
C   -----------     CO��ACHO A��OP�TMA �/� TRIARB.
C   METO�:         CM. TIRARB ��� TRIAR1;
C  -------
C   �APAMETP�:     CM. TRIARB ��� TRIAR1;
C   ---------
C   �O��PO�PAMM�:  CM. TRIARB ��� TRIAR1;
C   ------------
C   �P�ME�AH�E:     C���E�HA� �/� ��� COPT�POBK�;
C   -----------     TRIARB ��� TRIAR1
C
CZZZ-------------------------------------------------------------------
         EXTERNAL ECHANG, COMPCL
         INTEGER COMPCL
         DIMENSION BA0(1)
         INTEGER GAUCHE,  DROITE
C                C�EBA    C�PABA
         J = K
100      CONTINUE
         GAUCHE = 2*J
         IF( GAUCHE .GT. L ) GO TO 1000
         INDMAX = GAUCHE
         DROITE = GAUCHE + 1
         IF ( DROITE .GT. L ) GO TO 150
         IF ( COMPCL(GAUCHE, DROITE, BA0 ) .LT. 0 ) INDMAX = DROITE
150      CONTINUE
         IF ( COMPCL(INDMAX, J,      BA0 ) .LE. 0 ) GO TO 1000
         CALL ECHANG(J, INDMAX,  BA0)
         J = INDMAX
         GO TO 100
C--->
1000     RETURN
         END
