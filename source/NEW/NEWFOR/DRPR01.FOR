         SUBROUTINE DRPR01( KOD, TIME, TOK, VALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:   DRPR01( KOD, TIME, TOK, VALL )
C   -------------
C   HA�HA�EH�E:    �C�OH�TE�� ��PEKT�B� #�E�
C   ----------
C   METO�:         �O �H�OPMA��� �� COMMON SERVZ6 - �E�AT� �AHH�X;
C   -----
C              �EPEMEHHA� KXXX [MASPRT(99)] �C�O����ETC� KAK C.�.K.
C              �PO��CKAEM�X TO�EK ---> O�PA�OTKA �APAMETPA KOUT;
C
C             �P� FREST = 1 ( ��A� �A�POCA #�C�ET ) KXXX �P� KOD=1
C                            HE O�H���ETC�;
C
C
C   �APAMETP�:     KOD = 1 - OTKP�T� TA�����,  KXXX - O�H���T�;
C   ---------          = 2 - �E�AT� O�EPE�HO�;
C                      = 3 - �AKP�T� TA�����;
C                      = 4 �E�AT� O�EPE�HO� C �P��HAKOM KOMM�TA���;
C                      = 5 �E�AT� O�EPE�HO� (?) C �P��HAKOM #TECT_P�H�E#
C                      = 6 �E�AT� �AHH�X B MOMEHT CO��AH�� K.T. B KOH�E
C                             ( B�BO��TC� BCE��A )
C                      = 7 �E�AT� �AHH�X C �P��HAKOM �HTEP�O�. B�BO�A;
C                  TIME, TOK, VALL - MACC�BA O�PE�E�����E TO�K�;
C
C   �O��PO�PAMM�:  1. DRPR00
C   -------------  2. COMMON SERVZ6, PROF1
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( KXXX, MASPRT(99) )
       EQUIVALENCE( FREST,MASPRT(45) )
       INTEGER FREST
         COMMON/SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
         DIMENSION TOK(1), VALL(1)
         INTEGER SPISON
         GO TO (1,2,3,2,2,4,2 ), KOD
         CALL SUBERR(KOD)
1        CALL DRPR00(KOD, SPISON, TIME, TOK, VALL)
       IF ( FREST .EQ. 1 ) RETURN
         KXXX = 0
         RETURN
2        CONTINUE
         IF ( TIME .LT. TBEGON ) RETURN
         IF ( TIME .GT. TENDON ) RETURN
c         IF ( KOD .EQ. 4  .OR. KOD .EQ. 5 ) GO TO 4
c         IF ( KXXX .EQ. 0 ) CALL DRPR00( KOD, SPISON, TIME, TOK, VALL )
c         KXXX = KXXX + 1
c         IF ( KXXX .EQ. KOUTON ) KXXX = 0
         CALL DRPR00( KOD, SPISON, TIME, TOK, VALL )
         RETURN
C######################################################################
3        CONTINUE
C--->  �AKP�T� �E�AT�
         CALL DRPR00(KOD, SPISON, TIME, TOK, VALL )
         RETURN
C######################################################################
C--->  KOMM�TA��� B�BO��M BCE��A, �E�AT� TECT_P�H�E TO �E, K.T. TO �E...
4        CALL DRPR00(KOD, SPISON, TIME, TOK, VALL )
         RETURN
         END
