          SUBROUTINE FM0014( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C
C   �O��PO�PAMMA:  FM0014( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E��:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  64      3-X �A�HA� �.�.C        E-PE�(1)
C                          PE����P�EMA� OT "PB"
C
C
C                  65      3-X �A�HA� �.�.C.,
C                          ��PAB��EMA� BHE�H�M     E-PE�(2)
C                          C��HA�OM .
C
C
C              ��EMEHT KO��P�ETC�:
C              -------------------
C
C              � # T 64 � K0 KA KB KC � T-BK�  R-BH�T #-��-TA  K-�C��;
C
C              � # T 65 � K0 KA KB KC � R-BH�T L-BH�T K-�C��  W  FI
C                                       #-��-TA  #-CME�.
C
C     K0 - O���� ��E�, KA - �A�A A, KB - �A�A B, KC - �A�A C;
C
C---> B�BO�H�E �HA�EH��: TOK�: K ���AM KA, KB, KC, OT ���A K0,
C                              *                   **
C                      �HA�EH�� �.�.C. E1, E2, E3;
C
C
C !!!!   �A�EP�KA HA O��H �A� �HTE�P�POBAH�� !!!
C        -----------------------------------
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
C
C
C                  RMSW T��A # 64 [ �CTO�H�K HA�P��EH�� OT "PE�.BO��" ]
C                  -----------------------------------------
C
C                    TOK(J) = K_�C. * E(J) / R_BH�T. + 1/R * U_N+1
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II �HA�.  I �HA�.I # ��-TAI A�P. II      IJESIJESI K   I
C   ����:     II  1/R   I T_BK�I "PB"   I  B   II JESA I   I   I �C��I
C  K0         II        I      I        I TOKN II      I B I C I EH��I
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
C                         RMSW T��A # 65
C                         --------------
C
C                             SPM                 SRAB
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C            II #   I #   IA�P.I   I   I K I   I   I   I    I    I    I
C  ����:     II��-TAICME�.I B  I R I L I�C�I W I FII G IJESAIJESBIJESCI
C K0         II     I     ITOKNI   I   I�EHI   I   I   I    I    I    I
C   K1       ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C     K2     II  1  I  2  I  3 I 4 I 5 I 6 I 7 I 8 I 9 I 10 I 11 I 12 I
C        K3  ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C 9 10 11 12 II 13  I 14  I 15 I16 I17 I18 I19 I20 I21 I 22 I 23 I 24 I
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         EQUIVALENCE ( N, RN )
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 64 )   GO TO 63
         IF (TIP .EQ. 65 )   GO TO 65
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0014: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
63     CONTINUE
       LM = 8
C----------------------------------------------------------------------
C---> SIN �CT. HA�P.
         R = PAR0T(2)
         IF ( R .GT. 0.0 ) GO TO 61
         WRITE (NWTR,505) NEL
         R = 1.0E-32
C--> �A�OM�HAEM
61       RMSW(1) = 1.0 / R
         RMSW(2) = PAR0T(1)
C--> HOMEP  ��-TA
         N = IFIX( PAR0T(3) )
         RMSW(3) = RN
         RMSW(4) = 0.0
         RMSW(5) = 0.0
         RMSW(6) = 0.0
         RMSW(7) = 0.0
         RMSW(8) = PAR0T(4)
C---> �O��TC� !
         RETURN
C######################################################################
C---> SIN �CT. HA�P.
65     CONTINUE
       LM = 12
C--> O�H���M .
       DO 67 I=1,LM
67     RMSW(I) = 0.0
C----------------------------------------------------------------------
C--> �A�OM�HAEM.
       N = IFIX( PAR0T(5) )
       RMSW(1) = RN
       N = IFIX( PAR0T(6) )
       RMSW(2) = RN
       R = PAR0T(1)
CCC       L = PAR0T(2)
        L = 0.0
       IF ( R .GT. 0.0  ) GO TO 69
       WRITE (NWTR,505) NEL
       R = 1.0E-32
       L = 0.0
69     RMSW(4) = R
       RMSW(5) = L
       RMSW(6) = PAR0T(2)
       RMSW(7) = PAR0T(3)
       RMSW(8) = 3.14159 * PAR0T(4) / 180.0
       RETURN
C----------------------------------------------------------------------
505      FORMAT(' *** BH�TPEHHE CO�POT�B�EH�E �CTO�H�KA HA�P��EH�� ***'/
     =          ' *** ��EMEHTA HOMEP ', I10,  ' PABHO H���         ***'/
     =          ' *** �P�H�MAETC� PABH�M 1.0E-32  [OM]             ***')
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
