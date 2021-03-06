         SUBROUTINE FM0024( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT �EKA�P� 1987 �.)------------
C
C
C   �O��PO�PAMMA:  FM0024( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�E� C�E�����X T��OB:
C  ------------
C                +-----+--------------------------------------------+
C                I T�� I              �M� MO�E��                    I
C                I-----I--------------------------------------------I
C                I 15  I   ��PAB��EM�� HA�P��EH�EM �CTO�H�K TOKA    I
C                I-----I--------------------------------------------I
C                I 16  I   ��PAB��EM�� HA�P��EH�EM �CTO�H�K HA�P�-  I
C                I     I   �EH��                                    I
C                I-----I--------------------------------------------I
C                I 200 I   ��EA��H�� O�EPA��OHH�� �C���TE��         I
C                I-----I--------------------------------------------I
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
C                  RMSW T�� # 15 [ �T�H ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   �1   I   �2    I    G    II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  11    I   12    I    13   II
C             ++--------+---------+---------++
C
C
C                  RMSW T�� # 16 [ �H�H ]
C                  ----------------------
C
C      ++--------+---------+---------+---------+----------+
C      II        I         I         I         I          I
C      II   �1   I   �2    I   GBH   I    K    I G=GBH*K  I
C      II        I         I         I         I          I
C      ++--------+---------+---------+---------+----------+    LM = 5
C      II   1    I    2    I     3   I    4    I    5     I    ******
C      ++--------+---------+---------+---------+----------+
C      II  11    I   12    I    13   I   14    I   15     I
C      ++--------+---------+---------+---------+----------+
C
C
C                  RMSW T�� # 200 [ ��EA��H�� O�EPA��OHH�� �C���TE�� ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   K    I   GBX   I  GB�X   II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  13    I   14    I    15   II
C             ++--------+---------+---------++
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         EQUIVALENCE ( RN , N )
         INTEGER BA0T, TIP
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF( TIP .EQ. 15 ) GO TO 10
         IF( TIP .EQ. 16 ) GO TO 20
         IF( TIP .EQ. 200) GO TO 30
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0024: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
10       CONTINUE
C---> �CTO�H�K TOKA, ��PAB��EM�� HA�P��EH�EM
         N = IFIX( PAR0T(1) )
         RMSW(1) = RN
         N = IFIX( PAR0T(2) )
         RMSW(2) = RN
         G = PAR0T(3)
         RMSW(3) = G
C---> ���HA RMSW MO�E��
         LM = 3
         RETURN
C######################################################################
20       CONTINUE
C---> �CTO�H�K HA�P��EH��, ��PAB��EM�� HA�P��EH�EM
         N = IFIX( PAR0T(1) )
         RMSW(1) = RN
         N = IFIX( PAR0T(2) )
         RMSW(2) = RN
         R = PAR0T(3)
         IF( R .LE. 0.0 ) GO TO 25
         GBH = 1./R
21       RMSW(3) = GBH
         AK= PAR0T(4)
         RMSW(4) = AK
         G = AK * GBH
         RMSW(5) = G
C---> ���HA RMSW MO�E��
         LM = 5
         RETURN
25       WRITE(NWTR, 521) NEL
521    FORMAT(//' *** BH�TPEHHE CO�POT�B�EH�E �CTO�H�KA HA�P��EH�� ***'/
     =          ' *** ��EMEHTA HOMEP ', I10,  ' PABHO H���         ***'/
     =          ' *** �P�H�MAETC� PABH�M 1.0E-32  [OM]             ***')
         GBH = 1.0E+32
         GO TO 21
C######################################################################
30       CONTINUE
C---> ��EA��H�� O�EPA��OHH�� �C���TE��
         AK= PAR0T(1)
         RMSW(1) = AK
         RBX = PAR0T(2)
         IF( RBX .LE. 0.0 ) GO TO 35
         GBX = 1./ RBX
31       RMSW(2) = GBX
         RBIX = PAR0T(3)
         IF( RBIX .LE. 0.0 ) GO TO 37
         GBIX = 1./ RBIX
32       RMSW(3) = GBIX
C---> ���HA RMSW MO�E��
         LM = 3
         RETURN
35       WRITE(NWTR, 531) NEL
531    FORMAT(//' *** BXO�HOE CO�POT�B�EH�E O�EPA��OHHO�O �C���TE��***'/
     =          ' *** ��EMEHTA HOMEP ', I10,  ' OTP��ATE��HO       ***'/
     =          ' *** �P�H�MAETC� PABH�M �ECKOHE�HOCT�             ***')
         GBX = 0.0
         GO TO 31
37       WRITE(NWTR, 532) NEL
532    FORMAT(//' ***B�XO�HOE CO�POT�B�EH�E O�EPA��OHHO�O �C���TE��***'/
     =          ' *** ��EMEHTA HOMEP ', I10,  ' PABHO H���         ***'/
     =          ' *** �P�H�MAETC� PABH�M 1.E-32 [OM]               ***')
         GBIX = 1.0E+32
         GO TO 32
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
