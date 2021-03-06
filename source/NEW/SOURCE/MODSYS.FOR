         SUBROUTINE MODSYS( YALL, JALL, LOGYTR )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�AMMA:    MODSYS( YALL, JALL, LOGYTR )
C   -----------
C   HA�HA�EH�E:   - O�'E��HEH�E CTPOK � CTO���OB B YALL � JALL ���
C   ----------         ��ETA B OCHOBHO� C�CTEME �H���EHTH�X ���OB;
C                 - �O��OTOBKA MECTA ��� "TEHEBO�" C�CTEM�;
C
C   METO�:        - C�O�EH�E CTPOK � CTO���OB C "�A�OB�M�" ���AM�;
C   -----         - O��CTKA CTO���OB C�CTEM� ��� HE "�A�OB�X" ���OB.
C
C   �APAMETP�:
C   ----------     YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 6;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C
C   �O��PO�PAMM�: 1. COMMON PROF1
C   ------------
C   �P�ME�AH�E:    1. OCHOBHA� C�CTEMA YALL � JALL C�OPM�POBAH�
C   -----------       �E� ��ETA OTKP�T�X BEHT��E�;
C                  2. LOGYTR - C�OPM�POBAH � MO������POBAH;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(1)
         COMMON /PROF3/ FUNTAB(1)
         DIMENSION  YALL(1), JALL(1), LOGYTR(1)
         INTEGER OPEN6
         REAL JALL
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(26), NSYS   )
         EQUIVALENCE ( MASPRT(23), NDALL )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
         EQUIVALENCE ( MASPRT(58), KNOT6M )
C===>    BCTPOEHHA� ��HK��� O�C����BAH�� �B�XMEPH�X MACC�BOB:
         ID(I,J) = NDALL * (J-1)  +  I
         DO 100  K = 1, NSYS
         M = LOGYTR(K)
         IF ( M. LE. -1 ) GO TO 100
C--->  EC�� LOGYTR(K) = 0, TO "�A�OB��" ��E� �TO� �P���� �H���EHTH�X
C      ���OB �B��ETC� "H��EB�M" ���OM, �O �TOM� HEO�XO��MO TO��KO
C      O��CT�T� K-� CTO��E�;
         IF ( M .EQ. 0 ) GO TO 200
C--->  KOHTPO��  LOGYTR(M) �O��HA ��T� PABHA "-1", �HA�E O���KA
C!!!-------------------------------------------------------------------
         IF ( LOGYTR(M) .EQ. -1 ) GO TO 1
         WRITE(NWTRE,4)
4        FORMAT(' *** �O���ECK�� C�O� B MODSYS ---> HAP��EH�E CTP�KT�P�
     = LOGYTR ***')
          WRITE(NWTRE, 2 ) ( I, LOGYTR(I), I=1,NSYS )
2        FORMAT( 5(1X,'LOGYTR(', I3, ')=', I4 ) )
         WRITE(NWTRE,  3 ) K  M,  OPEN6, KNOT6M
3      FORMAT(' K=', I4, ' M=', I4,' OPEN6 = ', I5, '  KNOT6M = ', I5 )
         CALL SUBERR(25)
1        CONTINUE
C!!!-------------------------------------------------------------------
C  K-� CTPOKA - �TO CTPOKA "TEHEBO�" C�CTEM�
C  M-� CTPOKA - �TO CTPOKA "OCHOBHO�" C�CTEM�
C----> �E�AEM:
C        YALL(M,*) = YALL(M,*) + YALL(K,*)
C        YALL(*,M) = YALL(*,M) + YALL(*,K)
C        JALL(M)   = JALL(M)   + JALL(K)
C        YALL(*,K) = 0.0
C---->
         DO 99 J = 1, NSYS
         IMJ = ID( M, J )
         IKJ = ID( K, J )
99       YALL( IMJ ) = YALL( IMJ ) + YALL( IKJ )
C********
         DO 98 J = 1, NSYS
         IJM = ID( J, M )
         IJK = ID( J, K )
98       YALL( IJM ) = YALL( IJM ) + YALL( IJK )
C--->    �CTO�H�K�....
         JALL( M ) = JALL( M ) + JALL( K )
200      CONTINUE
C--->  O��CKA K-X CTO���OB===> MECTO �O� "TEHEB��" C�CTEM�
         DO 97  J = 1, NSYS
         IJK = ID( J, K )
97       YALL(IJK) = 0.0
100      CONTINUE
         RETURN
         END
