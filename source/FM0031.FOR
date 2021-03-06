       SUBROUTINE FM0031( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT  MAPTA  1988 �.)------------
C   �O��PO�PAMMA  FM0031( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA�HA�EH�E:  1) HACTPO�KA MAKPOMO�E�E� �B��ATE�E� �OCTO�HHO�O TOKA
C   ----------        �P�  �=CONST.( T�� # 520 )
C                     �P�  �=CONST,C TAXO�EHEPATOPOM.( T�� # 521 )
C
C----------------------------------------------------------------------
C                  ��EMEHT O��C�BAETC�:
C
C           � ?? T 520 �  K1  K2
C                      �  R  L  K�  J
C                      MAS < NT T1 Y1 ....   > ;
C
C           ��E   R   -AKT�BHOE CO�POT�B�EH�E O�MOTK� �KOP�(OM);
C                 L   -�H��KT�BHOE CO�POT�B�EH�E O�MOTK� �KOP�(�H);
C                 K�  -KOHCTP�KT�BHA� �OCTO�HHA� �B��ATE�� (B*C);
C                 J   -MOMEHT(B K�*M)�HEP��� BPA�. �ACT� (K�*M**2);    ,
C              T1 Y1..-TA����HO �A�AHHA� �AB�C�MOCT� MBH(T) ;          ,
C
C           *----------------------------------------------------------
C
C           � ?? T 521 �  K1  K2
C                      �  R  L  K�  J  KT�  RT�
C                      MAS < NT T1 Y1 ....   > ;
C
C           ��E   R   -AKT�BHOE CO�POT�B�EH�E O�MOTK� �KOP�(OM);
C                 L   -�H��KT�BHOE CO�POT�B�EH�E O�MOTK� �KOP�(�H);
C                 K�  -KOHCTP�KT�BHA� �OCTO�HHA� �B��ATE�� (B*C);
C                 J   -MOMEHT(B K�*M)�HEP��� BPA�. �ACT� (K�*M**2);
C                 KT� -KO������EHT �EPE�A�� TAXO�EHEPATOPA (B/C**-1)
C                 RT� -BH�TPEHHEE CO�POT�B�EH�E O�MOTK� TAXO�EHEPATOPA;
C              T1 Y1..-TA����HO �A�AHHA� �AB�C�MOCT� MBH(T) ;
C
C
C----------------------------------------------------------------------
C            2) �P�  FGMSW = 1 -> �TO�H�ETC� �HA�EH�E LM -
C               O��E� ���H� MSW �AHHO�O T��A ( C ��ETOM MAS �AP-OB );
C
C   METO�:      �OPM�POBAH�E �EPEMEHHO� �ACT� MSW_ALL O��CAH�� ��-TA,
C   -----       B���C�EH�E BC�OMO�ATE��H�X �AHH�X, HEO�XO��M�X ���
C               �A��HE��E� PA�OT� MAKPOMO�E��;
C
C   �APAMETP�:  BA0T, KNOT, PAR0T - "OTPE�K�" MACC�BOB BA0, KNOT, PAR0
C   ---------   BBE�EHH�X �EPE� �/� INPUT0;
C               RMSW - �EPEMEHHA� �ACT� MSW_ALL O��CAH�� ��EMEHTA;
C               LM - �P� FGMSW = 0 => ���HA �EPEMEHHO� �ACT� MSW_ALL;
C                    �P� FGMSW = 1 => ���HA BCE�O MACC�BA    MSW_ALL;
C   �P�ME�AH�E: 1. BCE �O��  �AH����TC�
C   ----------
C
C              �OPMAT RMSW T�� # 520:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -1        I      I HOMEP ���A HA�A�A
C    10      I     0        I      I HOMEP ���A KOH�A
C----------------------------------------------------------------------
C    11      I     1        I  FM  I  R  - CO�POT�B�EH�E �KOP�;
C    12      I     2        I  FM  I  L  - �H��KT�BHOCT� �KOP�;
C    13      I     3        I  FM  I  K� - KOHCTP�KT�BH�� KO������EHT;
C    14      I     4        I  FM  I  JM - MOMEHT �HEP���;
C    15      I     5        I  FM  I        PE�EPB
C    16      I     6        I  FM  I  T� - �OCTO�HHA� �KOPHO� �E��;
C    17      I     7        I  FM  I  TM - �OCTO�HHA� MEXAH��ECKA�;
C    18      I     8        I  DM  I  MBH- �HA�EH�E MBH ��� T=TN;
C    19      I     9        I  DM  I  MBH1-�HA�EH�E MBH ��� T=TN1;
C    20      I    10        I  DM  I  G -  �HA�EH�E �POBO��MOCT�;
C    21      I    11        I  DM  I  J -  �HA�EH�E �CT. TOKA;
C    22      I    12        I  FM  I         PE�EPB
C    23      I    13        I  FM  I         PE�EPB
C    24      I    14        I  FM  I  NT - KO���ECTBO TO�EK MBH(T);
C    25      I    15        I  FM  I  T1 - BPEM�;                      ;
C    26      I    16        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 24 + 2*NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 14 + 2*NT
C
C----------------------------------------------------------------------
C
C
C
C              �OPMAT RMSW T�� # 521:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0031    I      I
C------------I--------------I------I-----------------------------------
C     9      I    -3        I      I HOMEP ���A HA�A�A O�MOTK� �KOP�
C    10      I    -2        I      I HOMEP ���A KOH�A  O�MOTK� �KOP�
C    11      I    -1        I      I HOMEP ���A HA�A�A O�MOTK� T�.
C    12      I     0        I      I HOMEP ���A KOH�A  O�MOTK� T�.
C----------------------------------------------------------------------
C    13      I     1        I  FM  I  R  - CO�POT�B�EH�E �KOP�;
C    14      I     2        I  FM  I  L  - �H��KT�BHOCT� �KOP�;
C    15      I     3        I  FM  I  K� - KOHCTP�KT�BH�� KO������EHT;
C    16      I     4        I  FM  I  JM - MOMEHT �HEP���;
C    17      I     5        I  FM  I  KTG- KO�����EHT �EPE�A�� T�.
C    18      I     6        I  FM  I  GTG- �POBO��MOCT� O�MOTK� T�.
C    19      I     7        I  FM  I  T� - �OCTO�HHA� �KOPHO� �E��;
C    20      I     8        I  FM  I  TM - �OCTO�HHA� MEXAH��ECKA�;
C    21      I     9        I  DM  I  MBH- �HA�EH�E MBH ��� T=TN;
C    22      I    10        I  DM  I  MBH1-�HA�EH�E MBH ��� T=TN1;
C    23      I    11        I  DM  I  GR-  �POBO��MOCT� �E�� �KOP�;
C    24      I    12        I  DM  I  JR-  �CT. TOKA    �E�� �KOP�;
C    25      I    13        I  DM  I  GRG- �POBO��MOCT� �E�� �KOP� - T�
C    26      I    14        I  DM  I  JRG- �CT. TOKA    �E�� �KOP� - T�
C    27      I    15        I  FM  I  NT - KO���ECTBO TO�EK MBH(T);
C    28      I    16        I  FM  I  T1 - BPEM�;                      ;
C    29      I    17        I  FM  I  MBH1-MOMEHT;
C ......     I ........     I  ..  I  .........................
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 27 + 2*NT
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 15 + 2*NT
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       INTEGER BA0T, FGMSW, TIP
       EQUIVALENCE ( FGMSW,  MASPRT(87) )
       EQUIVALENCE ( N    ,  RN         )
       REAL  JM, L, KF, KTG
C--->�TO �A ��EMEHT?
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP = TIP - 519
       GO TO (99999,99999),NTIP
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0031: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
99999  CONTINUE
C--->  HA��EM ���H� MSW
       INT = 6
       IF( NTIP .EQ. 2 ) INT = 8
       NT = IFIX( PAR0T( INT ) )
       NT1= ( IFIX( PAR0T( INT-1 ) ) - 1 ) / 2
       IF ( NT .EQ. NT1  ) GO TO 750
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 700) TIP, NEL
700      FORMAT(' *** O���KA �P� �A�AH�� KO�-BA ��EMEHTOB MACC�BA ***'/
     =   ' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
         RETURN
750    CONTINUE
C-->  �TO �POC�T?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  �TO�H�T� MSW_ALL PA�MEP
       LM = 24 + 2 * NT
       IF( NTIP .EQ. 2 ) LM = 27 + 2 * NT
       RETURN
C######################################################################
1000   CONTINUE
       IF( NTIP .EQ. 2 ) GO TO 3000
C######################################################################
C
C   �B��ATE�� �OCTO�HHO�O TOKA ( T�� # 520 )
C
C######################################################################
C-->  �OPM�POBAT� RMSW....
C-->  LM - ���HA "�EPEMEHHO�" �ACT� RMSW...
       LM = 14 + 2 * NT
C********************************************************************
C-> ��CTKA
       DO 1001 I=1,LM
       RMSW(I) = 0.0
1001   CONTINUE
C--------------------------------------------------------------------
C-->  �P�EM �OCTO�HH�X �APAMETPOB...
C--------------------------------------------------------------------
          R   = PAR0T(1)
          L   = PAR0T(2)
          KF  = PAR0T(3)
          JM  = PAR0T(4)
C----------------------------------------------------------------------
C--->  �A�O�HEH�E RMSW �OCTO�HH�M� �APAMETPAM�.
C----------------------------------------------------------------------
       RMSW (1) = R
       RMSW (2) = L
       RMSW (3) = KF
       RMSW (4) = JM
       RMSW (6) = L / R
       RMSW (7) = JM * R / KF**2
C----------------------------------------------------------------------
C--->  �A�O�HEH�E RMSW MACC�BOM.
C      ��HK��� MBH(T).
C----------------------------------------------------------------------
C      IPAR0 - C�ET��K MACC�BA PAR0T.
C--------------------------------------------------------------------
C
       RMSW ( 14 ) = 2 * FLOAT(NT)
       I1 = 15
       I2 = 14 + 2 * NT
       IPAR0 = 6
          DO 1100 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
1100      CONTINUE
       RETURN
C######################################################################
C
C   �B��ATE�� �OCTO�HHO�O TOKA C TAXO�EHEPATOPOM HA BA�� ( T�� # 521 )
C
C######################################################################
3000   CONTINUE
C-->  �OPM�POBAT� RMSW....
C-->  LM - ���HA "�EPEMEHHO�" �ACT� RMSW...
       LM = 15 + 2 * NT
C********************************************************************
C-> ��CTKA
       DO 3001 I=1,LM
       RMSW(I) = 0.0
3001   CONTINUE
C--------------------------------------------------------------------
C-->  �P�EM �OCTO�HH�X �APAMETPOB...
C--------------------------------------------------------------------
          R   = PAR0T(1)
          L   = PAR0T(2)
          KF  = PAR0T(3)
          JM  = PAR0T(4)
          KTG = PAR0T(5)
          RTG = PAR0T(6)
          IF( RTG .LT. 1.E-32 ) RTG = 1.E-32
C----------------------------------------------------------------------
C--->  �A�O�HEH�E RMSW �OCTO�HH�M� �APAMETPAM�.
C----------------------------------------------------------------------
       RMSW (1) = R
       RMSW (2) = L
       RMSW (3) = KF
       RMSW (4) = JM
       RMSW (5) = KTG
       RMSW (6) = 1./RTG
       RMSW (7) = L / R
       RMSW (8) = JM * R / KF**2
C----------------------------------------------------------------------
C--->  �A�O�HEH�E RMSW MACC�BOM.
C      ��HK��� MBH(T).
C----------------------------------------------------------------------
C      IPAR0 - C�ET��K MACC�BA PAR0T.
C--------------------------------------------------------------------
C
       RMSW ( 15 ) = 2 * FLOAT(NT)
       I1 = 16
       I2 = 15 + 2 * NT
       IPAR0 = 8
          DO 3100 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
3100      CONTINUE
       RETURN
C----------------------------------------------------------------------
       END
