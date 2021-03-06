         SUBROUTINE FM0003( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  FM0003( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E��:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  80       "��O� - RL"                D_RL
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E�� "RL _ ��O�";
C   -----
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� HA �HA�EH�� L(0) � L(@) HE �PO��BO��TC�;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C                  RMSW T��A # 80 [ RL _ ��O� ]
C                  --------------------------------
C                                                             LM = 10
C                                                             *******
C
C                    SPM                      SRAB
C          ++---+---+---+--++----+---+-------+------+--------+----+---+
C          II R I L I R I L II G I J I PE�E  I COCT.I BPEM�  I PE�EPB I
C          II 0 I 0 I @ I @ II R I R I  PB   I TEK. I �EPEK�.I        I
C          II   I   I   I   II L I L I       I "KD" I "TX"[*]I        I
C          ++---+---+---+---++---+---+-------+------+--------+--------+
C          II 1 I 2 I 3 I 4 II 5 I 6 I   7   I  8   I   9    I   10   I
C          ++---+---+---+---++---+---+-------+------+--------+--------+
C          II 11I 12I 13I 14II 15I 16I   17  I  18  I   19   I   20   I
C          ++---+---+---+---++---+---+-------+------+--------+----+---+
C
C  0 - ��O� OTKP�T(KD=1) ; @ - ��O� �AKP�T (KD=0) ;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 80 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0003: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> O�H��EH�E BCEX �O�E� RMSW ��� ��O�A
         LM = 10
         DO 100 I=1,LM
100      RMSW(I) = 0.0
C===> �P�H�T� �CXO�. �APAMETP�:
         RMSW(1) = PAR0T(1)
         RMSW(2) = PAR0T(2)
         RMSW(3) = PAR0T(3)
         RMSW(4) = PAR0T(4)
C---->
         IF ( RMSW(1) .EQ. 0.0 ) WRITE(NWTR, 2) NEL
         IF ( RMSW(1) .EQ. 0.0 ) RMSW(1) = 1.0E-32
2        FORMAT(' *** CO�POT�B�EH�E "OTKP�TO�O" ��EMEHTA # ', I4,
     = ' PABHO 0.0 ***'/' *** �P�H�MAETC� PABH�M 1.0E-32 [OM] ***')
C---->
         IF ( RMSW(3) .EQ. 0.0 ) WRITE(NWTR, 3) NEL
         IF ( RMSW(3) .EQ. 0.0 ) RMSW(3) = 1.0E+32
3        FORMAT(' *** CO�POT�B�EH�E "�AKP�TO�O" ��EMEHTA # ', I4,
     = ' PABHO 0.0 ***'/' *** �P�H�MAETC� PABH�M 1.0E+32 [OM] ***')
         RETURN
         END
