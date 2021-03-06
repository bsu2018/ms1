         SUBROUTINE  GETSYS( MSWALL, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1  )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  GETSYS( MSWALL, YALL, JALL, LOGYTR,
C   ------------           TOKN, TOKN1, VALLN, VALLN1       )
C
C   HA�HA�EH�E:    �OPM�POBAH�E C�CTEM� ��HE�H�X A��E�PA��ECK�X
C  -----------     �PABHEH�� OTHOC�TE��HO HE��BECTHO�O
C                  BEKTOPA VALL - ���OB�X HA�P��EH��;
C                  C�CTEMA: [YALL] * [VALL] = [JALL]
C                  O�PE�E��ET �HA�EH�� VALLN1 ��� MOMEHTA
C                  BPEMEH�, PABHO�O TN + H0 ( T.E. ��� T(N+1) ).
C                  (�OPM�P�ETC� OCHOBHA� � "TEHEBA�" C�CTEMA );
C
C
C   METO�:         �O��EMEHTH�� BK�A� �HA�EH�� G-J �APAMETPOB
C  ------          BETBE�, �KB�BA�EHT�P����X BCE ��EMEHT�
C                  AHA����P�EMO� �E�� ( ZAPROS = 1 );
C                  O�PE�E�EH�E �T�X �APAMETPOB � �X �O�ABKA B
C                  YALL � JALL  �PO��BO��TC� �/� DM.....;
C                  ( "TEHEBA�" C�CTEMA �OPM�P�ETC� AHA�O���HO )
C
C   �APAMETP�:     MSWALL - ��PAB�����E C�OBA MAKPOMO�E�E�;
C   ---------      YALL   - MATP��A �POBO��MOCTE�;
C                  JALL   - BEKTOP �CTO�H�KOB TOKA BETBE�;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB;
C                  VALLN1 - BEKTOP ���OB�X HA�P��EH�� ( �CKOM�� );
C                  VALLN  - BEKTOP ���OB�X HA�P��EH�� B T = TN;
C                  TOKN - �HA�EH�E "TOKOB" � �P. �EPEMEHH�X COCTO�H��
C                         �O BCEM ��EMEHTAM B T = TN;
C                  TOKN1 - TO �E, HO B TN + H0;
C
C   �O��PO�PAMM�: 1. CALLDM - "B���BA����K" DM MOH�TOPOB;
C   ------------  2. COMMON PROF1; PRINT1, SUBERR, ENDLOG, MODSYS,
C                     DEPOSQ, PRINT4;
C  �P�ME�AH�E:    1. HA����E MOH�TOPOB DM..... HE �POBEP�ETC�;
C  ----------     2. YALL , JALL � LOGYTR - �H���A����P��TC�;
C                    �H��-TC� TAK �E OPEN6 � KNOT6M ( CM. MASPRT );
C                 3. PEA���OBAHO O�PA�EH�E K 25 �/� DM..... �EPE�
C                    CALLDM;
C                 4. �P� FTEST >=3 - �E�AT� "TEHEBO�" � "O��E�" C�CTEM;
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSWALL(1), YALL(1), JALL(1), LOGYTR(1)
         DIMENSION TOKN(1), TOKN1(1), VALLN(1), VALLN1(1)
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE ( MASPRT(37), FLOG )
         EQUIVALENCE ( MASPRT(28), ZAPROS )
         EQUIVALENCE ( MASPRT(29), TN )
         EQUIVALENCE ( MASPRT(38), FSYS )
         EQUIVALENCE ( MASPRT(23), NDALL )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
         EQUIVALENCE ( MASPRT(58), KNOT6M )
         EQUIVALENCE ( MASPRT(49), T6MSW )
         EQUIVALENCE ( MASPRT(60), FTEST )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(26), NSYS  )
         INTEGER OPEN6, ZAPROS, FSYS, NDALL, FLOG, FTEST
         REAL JALL
         ID(I,J) = NDALL * ( J - 1 )  + I
C===>  �CHAHOBKA "ZAPROS = 1"
         ZAPROS = 1
C===> C�POC ��A�A "C�CTEMA C�OPM�POBAHA"
         FSYS = 0
C===> �H���A���A��� MACC�BOB YALL, JALL, LOGYTR
         DO 500 I=1, NSYS
         LOGYTR(I) = -1
500      CONTINUE
C---->
         CALL DEPOSQ( 0.0, 0.0, YALL, JALL )
C---->
         OPEN6 = 0
         KNOT6M = NSYS
C===>   B��OB BCEX MOH�TOPOB*******************************************
         KF = 1
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODD )
C===> C�CTEMA C�OPM�POBAHA, LOGYTR - O�PE�E�EH....
         FSYS = 1
C---->  EC�� "TEHEBA�" C�CTEMA HE H��HA, TO BCE!
         IF ( FLOG .EQ. 0 ) GO TO 12345
C---> "A��APAT" FLOG BK��EH - �OPM�P�EM  "TEHEB��" C�CTEM�
C--> "OKOH�ATE��HOE" �OPM�POBAE�E LOGYTR
         CALL ENDLOG( LOGYTR )
C----> MO������POBAT� YALL ��� �OPM�POBAH�� "TEHEBO�" C�CTEM�
         CALL MODSYS( YALL, JALL, LOGYTR )
C===>  �OPM�POBAT�  "TEHEB��" C�CTEM�....
         KF = 3
         ZAPROS = 10
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODD )
C--->  TEHEBA� C�CTEMA C�OPM�POBAHA, HET �� �A�POCA HA KOHTP. �E�AT�
12345    IF ( FTEST .LT. 3 ) RETURN
C===> KOHTPO��H�E �E�AT�......
         NNN = MASPRT(7)
         WRITE ( NWTRE, 449 ) TN
449      FORMAT(/' *** FTEST_3: "OCHOBHA�" � "TEHEBA�" C�CTEM�',
     =    ' TN=', G12.5, ' ***')
C--->
       CALL PRINT4( YALL, JALL, LOGYTR )
C-->
C         DO 44 I=1, NNN
C44        WRITE(NWTRE, 445) ( I, J,YALL( ID(I,J) ), J=1,NNN       )
C445      FORMAT( 5(1X,'Y(',I3,',', I3, ')=', G12.5) )
C        WRITE(NWTRE, 446) (I,JALL(I), I=1, NNN       )
C446      FORMAT( 5(1X,'J(', I3, ')=', G12.5 ) )
C------
C         WRITE(NWTRE,447) ( I, LOGYTR(I), I=1,NNN)
C447      FORMAT( 5(1X,'LOGYTR(', I3, ')=', I4 ) )
C--->
         WRITE(NWTRE, 448) OPEN6, KNOT6M
448      FORMAT( ' OPEN6 = ', I5, '  KNOT6M = ', I5 )
C------
         RETURN
         END
