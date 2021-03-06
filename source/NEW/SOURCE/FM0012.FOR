         SUBROUTINE FM0012( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C
C
C   �O��PO�PAMMA:  FM0012( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�E� C�E�����X T��OB:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  63      3-X �A�HA� �.�.C        ES3
C                              C�H�CO��A��HO�
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 63 � K0 KA KB KC � R EMAX OMEGA FAZ0;
C
C     K0 - O���� ��E�, KA - �A�A A, KB - �A�A B, KC - �A�A C;
C
C---> B�BO�H�E �HA�EH��: TOK�: K ���AM KA, KB, KC, OT ���A K0;
C                              *                   **
C
C
C                  13      "��MEP�TE��"
C                           HA�P��EH��             V3
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 13 � K1 K2 K3;
C
C   B C��COK B��EC�EHH�X �HA�EH�� �OME�AET HA�P��EH�E ���OB K1, K2, K3;
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
C                  RMSW T�� # 13 [ ��MEP�TE�� HA�P��EH�� ]
C                  ----------------------------------------
C
C                  LM = 0, XPAH�TC� TO��KO ����;
C                  LM<�O�HA�> = 8+3=11;
C
C
C                  RMSW T��A # 63 [ �CTO�H�K SIN HA�P��EH�� - 3-� �A��]
C                  -----------------------------------------
C                    ( U1 = EM * SIN( OMEGA * T  +  FAZ0 )     LM = 8
C                    ( U2 = EM * SIN( OMEGA * T  +  FAZ0 - 120 )
C                    ( U3 = EM * SIN( OMEGA * T  +  FAZ0 + 120 )
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II �HA�.  I �HA�.I        I FAZ0 II      IJESIJESI PE  I
C   ����:     II  1/R   I EM/R I OMEGA  I  P   II JESA I   I   I �E  I
C  K0         II        I      I        I PA�. II      I B I C I PB  I
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 63 )   GO TO 63
         IF (TIP .EQ. 13 )   GO TO 13
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0012: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
13       CONTINUE
          LM = 0
C---> ��MEP�TE��
        RETURN
C----------------------------------------------------------------------
63     CONTINUE
       LM = 8
C######################################################################
C######################################################################
C---> SIN �CT. HA�P.
         R = PAR0T(1)
         EM = PAR0T(2)
         OMEGA = PAR0T(3)
         FAZA = PAR0T(4) * 3.141592654 / 180.0
         IF ( R .EQ. 0.0 ) GO TO 60
C
61       RMSW(1) = 1.0 / R
         RMSW(2) = EM / R
         RMSW(3) = OMEGA
         RMSW(4) = FAZA
         RMSW(5) = 0.0
         RMSW(6) = 0.0
         RMSW(7) = 0.0
         RMSW(8) = 0.0
C
C
         RETURN
60       WRITE (NWTR,505) NEL
         R = 1.0E-32
         GO TO 61
C----------------------------------------------------------------------
505      FORMAT(' *** BH�TPEHHE CO�POT�B�EH�E �CTO�H�KA HA�P��EH�� ***'/
     =          ' *** ��EMEHTA HOMEP ', I10,  ' PABHO H���         ***'/
     =          ' *** �P�H�MAETC� PABH�M 1.0E-32  [OM]             ***')
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
