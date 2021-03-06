         SUBROUTINE GETLOG_( K, M, LOGYTR )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   GETLOG( K, M, LOGYTR )
C   -------------
C   HA�HA�EH�E:    B��O�HEH�E O�EPE�HO�O AKTA �OPM�POBAH��
C   ----------     MACC�BA LOGYTR K.�. ���OB.
C   METO�:         MO����KA��� "BO�HOBO�O" A��OP�TMA,
C   -----          OP�EH�POBAHHO�O HA "�O��EMEHTHOE" �OPM�POBAH�E
C                  MACC�BA LOGYTR;
C   �APAMETP�:     K, M - �H���EHTH�E ����;
C   ---------      LOGYTR - MACC�B K.�. ���OB;
C   �O��PO�PAMM�:  1. COMMON PROF1;
C   ------------
C   �P�ME�AH�E:    1. MACC�B LOGYTR �PE��O�O�AETC� �H����POBAHH�M;
C   -----------    2. OPEN6 � KNOT6M  TO �E ��E  �H����POBAH�;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION LOGYTR(1)
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( KNOT6M, MASPRT(58) )
         EQUIVALENCE ( OPEN6,  MASPRT(57) )
         INTEGER  OPEN6
         OPEN6 = OPEN6 + 1
         KHELP = NSYS
         MHELP = NSYS
         IF ( K .NE. 0 ) KHELP = K
         IF ( M .NE. 0 ) MHELP = M
         KNOT6M = MIN0( KNOT6M, KHELP, MHELP )
         MINKM = MIN0( K, M )
         LK = 0
         LM = 0
         IF ( K .NE. 0 ) LK = LOGYTR(K)
         IF ( M .NE. 0 ) LM = LOGYTR(M)
C$$$$$$$$ BETB�MC� $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         IF ( LK. EQ. -1  .AND.  LM .EQ. -1 ) GO TO 1
         IF ( LK. NE. -1  .AND.  LM .NE. -1 ) GO TO 2
         IF ( LK. EQ. -1 ) GO TO 3
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
4        CONTINUE
C---->  LK ^= -1,  LM = -1
         MING = MIN0( MINKM, LK )
         IF ( K .NE. 0 ) LOGYTR(K) = MING
         IF ( M .NE. 0 ) LOGYTR(M) = MING
C--->  H��HO �� MO������POBAT� LOGYTR ?
         IF ( LK .EQ. MING ) GO TO 666
C--> LOGYTR MO�. �O LK  �  MING
         MOLD = LK
         MNEW = MING
         ASSIGN 666 TO METKA
         GO TO 600
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
3        CONTINUE
C---> LK = -1,  LM ^= -1
         MING = MIN0( MINKM, LM )
         IF ( K .NE. 0 ) LOGYTR(K) = MING
         IF ( M .NE. 0 ) LOGYTR(M) = MING
         IF ( LM .EQ.  MING ) GO TO 666
         MOLD = LM
         MNEW = MING
         ASSIGN 666 TO METKA
         GO TO 600
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
2        CONTINUE
C--->  LK ^= -1,  LM ^= -1
         MING = MIN0( MINKM, LK, LM )
         IF ( K .NE. 0 ) LOGYTR(K) = MING
         IF ( M .NE. 0 ) LOGYTR(M) = MING
         IF ( LK .EQ. MING ) GO TO 20
C  -> MO�. �O LK..
         MOLD = LK
         MNEW = MING
         ASSIGN 20 TO METKA
         GO TO 600
20       CONTINUE
         IF ( LM .EQ. MING ) GO TO 666
C-->  MO�. �O LM ...
         MOLD = LM
         MNEW = MING
         ASSIGN 666 TO METKA
         GO TO 600
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
1        CONTINUE
C--->  LK = -1,   LM = -1
         IF ( K.NE. 0 ) LOGYTR(K) = MINKM
         IF ( M.NE. 0 ) LOGYTR(M) = MINKM
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      RETURN
C**********************************************************************
C**********************************************************************
C  �O��PO�PAMMA "MO������POBAT� LOGYTR"
C  �O �HA�EH��M "MOLD" � "MNEW"
C**********************************************************************
C**********************************************************************
600      CONTINUE
         DO 601  I = KNOT6M, NSYS
            IF ( LOGYTR(I) .NE. MOLD ) GO TO 601
C---> AKT "MO����KA���"
         LOGYTR(I) = MNEW
601      CONTINUE
         GO TO METKA, ( 666, 20 )
602      CALL SUBERR(20)
         END
