         SUBROUTINE FM0036( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT ������  1990 �.)------------
C
C
C   �O��PO�PAMMA:  FM0036( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�E� C�E�����X T��OB:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  66            �.�.C             ESG
C                              �������������
C                              C�H�CO��A��H��
C
C
C                  67          �������������       JSC
C                              C�H�CO��A��H��
C                              ���. ����
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 66 � K0 K1 �    R    L
C                                   MAS < NGARM1  E1  FI1
C                                         NGARM2  E2  FI2
C                                         .....   ..  ...  >  ;
C
C
C
C
C              � # T 67 � K0 K1 �  MAS < NGARM1  J1  FI1
C                                        NGARM2  J2  FI2
C                                        .....   ..  ...  >  ;
C
C
C
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E�E� �KA�AHH�X T��OB.
C
C   METO�:         B���C��ETC� � �A��C�BAETC� B MSW BC� HEO�XO��MA�
C   -----          �H�OPMA���, HEO�XO��MA� ��� �OC�E����E�O
C                  PAC�ETA MAKPOMO�E��. A�A�T�POBAHO K METO��
C                  ��H���EPA - B�O�����.
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW - �O�� SPM � SRAB;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C
C   �P�ME�AH�E:    1. KOHTPO�� HE �PO��BO��TC�;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C
C                  RMSW T��A # 66 [ �CTO�H�K SIN HA�P��EH�� ]
C                  -----------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II     I     I ���-�� I    I    I
C   ����:     II  R  I  L  I ������.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
C
C
C                  RMSW T��A # 67 [ �CTO�H�K SIN TOKA ]
C                  ------------------------------------
C
C             ++-----+-----+--------+----+----+
C             II  ��-I  ��-I ���-�� I    I    I
C   ����:     II ����I ����I ������.I G  I J  I
C  K0         II     I     I  K     I    I    I
C             ++-----+-----+--------+----+----+
C      K1     II  1  I  2  I   3    I 4  I  5 I
C             ++-----+-----+--------+----+----+
C  9   10     II 11  I 12  I  13    I 14 I 15 I
C             ++-----+-----+--------+----+----+
C
C             +-----+-----+-----+--->...+------+------+------+
C             I     I     I     I--->...I      I      I      I
C             I NG1 I E1  I FI1 I--->...I NGK  I EK   I FIK  I
C             I     I     I     I--->...I      I      I      I
C             +-----+-----+-----+--->...+------+------+------+
C             I  6  I  7  I  8  I--->...I 3+K*3I4+K*3 I 5+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C             I  16 I  17 I  18 I--->...I13+K*3I14+K*3I15+K*3I
C             +-----+-----+-----+--->...+------+------+------+
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         EQUIVALENCE ( N     , RN         )
         EQUIVALENCE ( FGMSW , MASPRT(87) )
         INTEGER BA0T, TIP, FGMSW
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 66 .OR. TIP .EQ. 67 ) GO TO 6667
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0036: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
6667   CONTINUE
         IF( TIP .EQ. 66 ) KGARM   = IFIX (PAR0T(3))
         IF( TIP .EQ. 67 ) KGARM   = IFIX (PAR0T(1))
         IF( MOD(KGARM,3) .NE. 0 ) GO TO 9999
         KGARM   = KGARM / 3
C--> ��� ����� ?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  �TO�H�T� LM <O���� >
       LM = 15 + 3*KGARM
       RETURN
C######################################################################
1000     CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
         LM = 5 + 3*KGARM
         DO 2 I=1,LM
2        RMSW(I) = 0.0
C----------------------------------------------------------------------
         IF( TIP .EQ. 67 ) GO TO 1100
         R = PAR0T(1)
         L = PAR0T(2)
         IF( R .EQ. 0.0  .AND. L .EQ. 0.0 ) GO TO 9000
1050     CONTINUE
         RMSW(1) = R
         RMSW(2) = L
         IPAR0   = 4
         GO TO 1200
1100     CONTINUE
         IPAR0   = 2
1200     CONTINUE
         N       = KGARM
         RMSW(3) = RN
         IMSW    = 6
         DO 1300 I=1,KGARM
         NG  = IFIX( PAR0T(IPAR0) )
         EG  = PAR0T(IPAR0+1)
         FIG = PAR0T(IPAR0+2)
         N = NG
         RMSW(IMSW) = RN
         RMSW(IMSW+1) = EG
         RMSW(IMSW+2) = FIG * 3.141592654 / 180.0
         IPAR0 = IPAR0 + 3
         IMSW  = IMSW  + 3
1300     CONTINUE
C
         RETURN
C---------------------------------------------------------------------
9000     WRITE (NWTR,505) NEL
         R = 1.0E-32
         GO TO 1050
C---------------------------------------------------------------------
505      FORMAT(' *** BH�TPEHHE CO�POT�B�EH�E �CTO�H�KA HA�P��EH�� ***'/
     =          ' *** ��EMEHTA HOMEP ', I10,  ' PABHO H���         ***'/
     =          ' *** �P�H�MAETC� PABH�M 1.0E-32  [OM]             ***')
C!!!-------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 555) TIP, NEL
555      FORMAT(' *** �O�. C�O� B FM0036: ����������� ������ ���-��
     =�������� ***'/' *** HOMEP T��A ',I10,' ��EMEHT HOMEP:',I10,'***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
         RETURN
C######################################################################
         END
