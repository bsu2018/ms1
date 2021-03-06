         SUBROUTINE FM0001( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C
C
C   �O��PO�PAMMA:  FM0001( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�E� C�E�����X T��OB:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                   1      CO�POT�B�EH�E           R
C
C                   2      �H��KT�BHOCT�           L
C
C                   3      EMKOCT�                 C
C
C                          �CTO�H�K
C                   4      �OCTO�HHO�O             J
C                          TOKA
C
C                          �CTO�H�K
C                   5      �OCTO�HHO�O             E
C                          HA�P��EH��
C
C                          �CTO�H�K
C                   6      SIN                     ES
C                          HA�P��EH��(1)
C
C                          �CTO�H�K
C                   7      SIN                     ES
C                          HA�P��EH��(2)
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
C                  RMSW T�� # 1 [ CO�POT�B�EH�E ]
C                  ------------------------------
C
C                 SPM
C             ++--------++
C             II �HA�.  II
C             II  1/R   II
C             II        II            LM = 1
C             ++--------++            ******
C             II   1    II
C             ++--------++
C             II  11    II
C             ++--------++
C
C
C                  RMSW T��A # 2 [ �H��KT�BHOCT� ]
C                  -------------------------------
C
C                 SPM            SRAB
C              +--------++-----+-----+
C              I �HA�.  II     I     I
C              I   L    II  GL I  JL I
C              I        II     I     I
C              +--------++-----+-----+         LM = 3
C              I   1    II 2   I  3  I         ******
C              +--------++-----+-----+
C              I   11   II 12  I 13  I
C              +--------++-----+-----+
C
C
C
C
C                  RMSW T��A # 3 [ EMKOCT� ]
C                  -------------------------
C
C                 SPM            SRAB
C             ++--------++-----+-----+
C             II �HA�.  II     I     I
C             II   C    II  GC I  JC I
C             II        II     I     I           LM = 3
C             ++--------++-----+-----+           ******
C             II   1    II  2  I  3  I
C             ++--------++-----+-----+
C             II   11   II 12  I 13  I
C             ++--------++-----+-----+
C
C
C
C
C
C                  RMSW T��A # 4 [ �CTO�H�K �OCTO�HHO�O TOKA ]
C                  -------------------------------------------
C
C                 SPM
C             ++--------+
C             II �HA�.  I
C             II   J    I
C             II        I
C             ++--------+           LM = 1
C             II   1    I           ******
C             ++--------+
C             II   11   I
C             ++--------+
C
C
C             RMSW T��A # 5 [ �OCTO�HH�� �CTO�H�K HA�P��EH�� ]
C             ------------------------------------------------
C
C                   SPM
C             ++--------+------++
C             II �HA�.  I �HA�.II
C             II  1/R   I  E/R II
C             II        I      II
C             ++--------+------++
C             II   1    I  2   II        LM = 2
C             ++--------+------++        *******
C             II  11    I  12  II
C             ++--------+------++
C
C
C
C                  RMSW T��A # 6 [ �CTO�H�K SIN HA�P��EH��(1) ]
C                  --------------------------------------------
C                    ( U = EM * SIN( OMEGA * T  +  FAZA )     LM = 5
C                                                             ******
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+
C             II �HA�.  I �HA�.I        I FAZA II      I
C             II  1/R   I EM/R I OMEGA  I  P   II JES  I
C             II        I      I        I PA�. II      I
C             ++--------+------+--------+------++------+
C             II   1    I  2   I   3    I  4   II  5   I
C             ++--------+------+--------+------++------+
C             II  11    I  12  I  13    I  14  II  15  I
C             ++--------+------+--------+------++------+
C
C
C                  RMSW T��A # 7 [ �CTO�H�K SIN HA�P��EH��(2) ]
C                  --------------------------------------------
C                                                             LM = 7
C                                                             ******
C
C                      SPM                        SRAB
C     ++------+------+------+--------+------++------+------+
C     II      I      I �HA�.I        I FAZA II      I      I
C     II  R   I  L   I  EM  I OMEGA  I  P   II GES  I JES  I
C     II      I      I      I        I PA�. II      I      I
C     ++------+------+------+--------+------++------+------+
C     II  1   I  2   I  3   I   4    I  5   II  6   I   7  I
C     ++------+------+------+--------+------++------+------+
C     II  11  I  12  I  13  I  14    I  15  II  16  I  17  I
C     ++------+------+------+--------+------++------+------+
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
         GO TO (1, 2, 3, 4, 5, 6 , 7 ), TIP
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0001: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> CO�POT�B�EH�E
         R = PAR0T(1)
         IF ( R .EQ. 0.0 ) GO TO 10
C---> �A�OMH�M �APAMETP�.
11       RMSW(1) = 1.0 / R
C---> ���HA RMSW MO�E��
         LM = 1
         RETURN
10       WRITE(NWTR,501) NEL
501      FORMAT(' *** CO�POT�B�EH�E ��EMEHTA ', I5, ' PABHO H��� ***'/
     =          ' *** �P�H�MAETC� PABH�M 1.0E-32 [OM]            ***')
         R = 1.0E-32
         GO TO 11
C######################################################################
2        CONTINUE
C----> �H��KT�BHOCT�
         L = PAR0T(1)
         IF ( L .EQ. 0.0 ) GO TO 20
C---> �A�OMH�M �APAMETP�.
21       RMSW(1) = L
         RMSW(2) = 0.0
         RMSW(3) = 0.0
C---> ���HA RMSW MO�E��
         LM = 3
         RETURN
20       WRITE(NWTR, 521) NEL
521    FORMAT(' *** �H��KT�BHOCT ��EMEHTA HOMEP ', I5, ' PABHA H��� ***'
     =   /    ' *** �P�H�MAETC� PABHO� 1.0E-32 [�H]                 ***'
     =  )
         L = 1.0E-32
         GO TO 21
C######################################################################
3        CONTINUE
C--->  EMKOCT�
         C = PAR0T(1)
C---> �A�OMH�M �APAMETP�.
         RMSW(1) = C
         RMSW(2) = 0.0
         RMSW(3) = 0.0
C---> ���HA RMSW MO�E��
         LM = 3
         RETURN
C######################################################################
4        CONTINUE
C----> �CTO�H�K �OCTO�HHO�O TOKA.
         AJ = PAR0T(1)
C---> �A�OMH�M �APAMETP�.
         RMSW(1) = AJ
C---> ���HA RMSW MO�E��
         LM = 1
         RETURN
C######################################################################
5        CONTINUE
C---->  �CTO�H�K �OCT. HA�P.
         R = PAR0T(1)
         E = PAR0T(2)
         IF ( R .EQ. 0.0 ) GO TO 50
C---> �A�OMH�M �APAMETP�.
51       RMSW(1) = 1.0 / R
         RMSW(2) =   E / R
C---> ���HA RMSW MO�E��
         LM = 2
         RETURN
50       WRITE(NWTR,505) NEL
505      FORMAT(' *** BH�TPEHHE CO�POT�B�EH�E �CTO�H�KA HA�P��EH�� ***'/
     =          ' *** ��EMEHTA HOMEP ', I10,  ' PABHO H���         ***'/
     =          ' *** �P�H�MAETC� PABH�M 1.0E-32  [OM]             ***')
         R = 1.0E-32
         GO TO 51
C######################################################################
6        CONTINUE
C---> SIN �CT. HA�P.
         R = PAR0T(1)
         EM = PAR0T(2)
         OMEGA = PAR0T(3)
         FAZA = PAR0T(4) * 3.141592654 / 180.0
         IF ( R .EQ. 0.0 ) GO TO 60
C---> �A�OMH�M �APAMETP�.
61       RMSW(1) = 1.0 / R
         RMSW(2) = EM / R
         RMSW(3) = OMEGA
         RMSW(4) = FAZA
         RMSW(5) = 0.0
C---> ���HA RMSW MO�E��
         LM = 5
         RETURN
60       WRITE (NWTR,505) NEL
         R = 1.0E-32
         GO TO 61
C######################################################################
7        CONTINUE
C---> SIN �CT. HA�P��EH�� (2)
         ER = PAR0T(1)
         EL = PAR0T(2)
         AEM = PAR0T(3)
         OMEGA = PAR0T(4)
         FAZA = PAR0T(5) * 3.141592654 / 180.0
         IF( ER .NE. 0.0  .OR.  EL .NE. 0.0 ) GO TO 70
         ER = 1.0E-32
         WRITE(NWTR,505) NEL
70       CONTINUE
C---> �A�OMH�M �APAMETP�.
         RMSW(1) = ER
         RMSW(2) = EL
         RMSW(3) = AEM
         RMSW(4) = OMEGA
         RMSW(5) = FAZA
         RMSW(6) = 0.0
         RMSW(7) = 0.0
C---> ���HA RMSW MO�E��
         LM = 7
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
