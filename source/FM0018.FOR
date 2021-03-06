       SUBROUTINE FM0018( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA  FM0018( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA�HA�EH�E:  1) HACTPO�KA MAKPOMO�E�� M-�A�HO�O �A�OBO�O �B��ATE��
C   ----------      C MA�H�THO HE CB��AHH�M� O�MOTKAM�.
C                   ***************************************************
C                  T�� # 502 --> 2-X  �A�H�� BPA�. �A�OB�� �B��ATE��;
C                  T�� # 503 --> 3-X  �A�H�� BPA�. �A�OB�� �B��ATE��;
C                  T�� # 504 --> 4-X  �A�H�� BPA�. �A�OB�� �B��ATE��;
C                  T�� # 505 --> 5-T� �A�H�� BPA�. �A�OB�� �B��ATE��;
C                  T�� # 506 --> 6-T� �A�H�� BPA�. �A�OB�� �B��ATE��;
C                  T�� # 507 --> 7-M� �A�H�� BPA�. �A�OB�� �B��ATE��;
C                  T�� # 508 --> 8-M� �A�H�� BPA�. �A�OB�� �B��ATE��;
C                  T�� # 512 --> 2-X  �A�H�� ��H.  �A�OB�� �B��ATE��;
C                  T�� # 513 --> 3-X  �A�H�� ��H.  �A�OB�� �B��ATE��;
C                  T�� # 514 --> 4-X  �A�H�� ��H.  �A�OB�� �B��ATE��;
C                  T�� # 515 --> 5-T� �A�H�� ��H.  �A�OB�� �B��ATE��;
C                  T�� # 516 --> 6-T� �A�H�� ��H.  �A�OB�� �B��ATE��;
C                  T�� # 517 --> 7-M� �A�H�� ��H.  �A�OB�� �B��ATE��;
C                  T�� # 518 --> 8-M� �A�H�� ��H.  �A�OB�� �B��ATE��;
C
C
C
C                  ��EMEHT O��C�BAETC�:
C           � ?? T 5?? � #H1 #H2 ... #K1 #K2...
C                      �  M  P  R  J  MT  TL
C                      MAS < L0 L1 ....LN >
C                      MAS < T1 Y1 ....   > ;
C
C           ��E   M   -��C�O �A� �B��ATE��;
C                 P   -��C�O �AP �O��COB �B��ATE��;
C                 R   -AKT�BHOE CO�POT�B�EH�E O�MOTK�,(OM);
C                 J   -MOMEHT(B K�*M)�HEP��� BPA�. �ACT� (��� BPA�. ��),
C                      MACCA (B K�) �O�B��HO� �ACT� ( ��� ��H. �� );
C                 MT  -MOMEHT TPEH�� HA BA�� (B H*M)(��� BPA�. ��),
C                      C��A TPEH�� (B H) (��� ��H. ��);
C                 TL  -�EP�O� ��MEHEH�� �H��KT�BHOCT� �A��,
C                      TL = TAU (�PA�) ��� BPA�. ��,
C                      TL = TAU (B M ) ��� ��H.  ��;
C                 L0..-KO������EHT� �APMOH��ECKO�O P��A, O��C�BA��E�O
C                      �AB�C�MOCT� �H��KT�BHOCT� �A�� OT �EPEME�EH��:
C                      L(X)=L0+L1*COS(2*PI*X/TL)+L2*COS(4*PI*X/TL)...,
C                      ��E X - �EPEME�EH�E ��. �O��,
C                          X = XMEX * P;
C              T1 Y1..-TA����HO �A�AHHA� �AB�C�MOCT� MBH(T) ��� BPA�.��,
C                      TA����HO �A�AHHA� �AB�C�MOCT� FBH(T) ��� ��H.��;
C
C
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
C
C                       RMSW T�� # 502...518
C                      **********************
C
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  I     I     I     I     I      I      I       I         I     I
C  I  M  I  P  I  R  I  J  I  MT  I  TL  I PE�EPBI PE�EPB  I W0  I
C  I     I     I     I     I      I      I       I         I     I
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  I  1  I  2  I  3  I  4  I  5   I   6  I   7   I    8    I  9  I
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  I D1  I D1+1I D1+2I D1+3I D1+4 I D1+5 I  D1+6 I  D1+7   I D1+8I
C  +-----+-----+-----+-----+------+------+-------+---------+-----+--->
C  +D1=9+2*M
C  +DR1=1
C
C
C
C  +--------+--------+--------+--------+---------+----------+----------+
C  I        I  D L   I        I        I         I  NGARM,  I   NT,    I
C  I   L    I  ----- I   J0   I G(N+1) I J(N+1)  I L0,L1... I T1,Y1... I
C  I        I  D GAM I        I        I         I          I          I
C  I M C�OB I M C�OB I M C�OB I M C�OB I M C�OB  INMAS1 C�OBINMAS2 C�OBI
C  +--------+--------+--------+--------+---------+---------------------+
C  I DR2+M  I DR3+M  I DR4+M  I DR5+M  I DR6+M   I DR7+NMAS1I DR8+NMAS2I
C  +--------+--------+--------+--------+---------+----------+----------+
C  ID2..D3-1ID3..D4-1ID4..D5-1ID5..D6-1ID6...D7-1I D7..D8-1 I D8+NMAS2 I
C  +--------+--------+--------+--------+---------+----------+----------+
C  +D2=D1+9 +D3      +D4      +D5      +D6       +D7        +D8
C  +DR2=10  +DR3     +DR4     +DR5     +DR6      +DR7       +DR8
C
C   D@ -HOMEPA �O�E� MSW_ALL MACC�BA;
C   DR@-HOMEPA �O�E�  R_MSW  MACC�BA;
C
C                           DR1  DR2  DR3  DR4  DR5  DR6  DR7    DR8
C   ��� 2-X  �A�HO�O ��-->  1    10   12   14   16   18   20   20+NMAS1
C   ��� 3-X  �A�HO�O ��-->  1    10   13   16   19   22   25   25+NMAS1
C   ��� 4-X  �A�HO�O ��-->  1    10   14   18   22   26   30   30+NMAS1
C   ��� 5-T� �A�HO�O ��-->  1    10   15   20   25   30   35   35+NMAS1
C   ��� 6-T� �A�HO�O ��-->  1    10   16   22   28   34   40   40+NMAS1
C   ��� 7-M� �A�HO�O ��-->  1    10   17   24   31   38   45   45+NMAS1
C   ��� 8-M� �A�HO�O ��-->  1    10   18   26   34   42   50   50+NMAS1
C
C
C                           D1   D2   D3   D4   D5   D6   D7     D8
C   ��� 2-X  �A�HO�O ��-->  13   22   24   26   28   30   32   32+NMAS1
C   ��� 3-X  �A�HO�O ��-->  15   24   27   30   33   36   39   39+NMAS1
C   ��� 4-X  �A�HO�O ��-->  17   26   30   34   38   42   46   46+NMAS1
C   ��� 5-T� �A�HO�O ��-->  19   28   33   38   43   48   53   53+NMAS1
C   ��� 6-T� �A�HO�O ��-->  21   30   36   42   48   54   60   60+NMAS1
C   ��� 7-M� �A�HO�O ��-->  23   32   39   46   53   60   67   67+NMAS1
C   ��� 8-M� �A�HO�O ��-->  25   34   42   50   58   66   74   74+NMAS1
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       INTEGER D1 , D2 , D3 , D4 , D5 , D6 , D7 , D8
       INTEGER DR1, DR2, DR3, DR4, DR5, DR6, DR7, DR8
       INTEGER BA0T, FGMSW, TIP, M
       EQUIVALENCE ( FGMSW,  MASPRT(87) )
       EQUIVALENCE ( N    ,  RN         )
       REAL P, R, J, MT, TL
C--->�TO �A ��EMEHT?
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP = TIP - 501
       IF ( NTIP .GT. 10 ) NTIP = NTIP - 10
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 7 ) GO TO 99999
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0018: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
99999  CONTINUE
C--->  HA��EM D@  �  DR@
       M = PAR0T( 1 )
          IF ( M .EQ. NTIP + 1 ) GO TO 750
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 700) TIP, NEL, M
700      FORMAT(' *** O���KA �P� �A�AH�� �APAMETPA "M" ***'/
     =   ' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10,
     =   '     M =', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
         RETURN
750    CONTINUE
       D1 = 9 + 2 * M
       D7 = D1 + 9 + 5 * M
       DR1 = 1
       DR7 = DR1 + 9 + 5 * M
C-->  �TO �POC�T?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  �TO�H�T� MSW_ALL PA�MEP
       NMAS1 = IFIX ( PAR0T (7) )
       NMAS2 = IFIX ( PAR0T (8 + NMAS1 ) )
       D8 = D7 + NMAS1 + 1
       LM = D8 + NMAS2
       RETURN
C######################################################################
1000   CONTINUE
       TIP = BA0T(2)
C-->  �OPM�POBAT� RMSW....
C-->  LM - ���HA "�EPEMEHHO�" �ACT� RMSW...
       NMAS1 = IFIX ( PAR0T (7) )
       NMAS2 = IFIX ( PAR0T (8 + NMAS1 ) )
       DR8 = DR7 + NMAS1 + 1
       LM = DR8 + NMAS2
C********************************************************************
C-> ��CTKA
       DO 1 I=1,LM
       RMSW(I) = 0.0
1      CONTINUE
C--------------------------------------------------------------------
C-->  �P�EM �OCTO�HH�X �APAMETPOB...
C--------------------------------------------------------------------
          P   = PAR0T(2)
          R   = PAR0T(3)
          J   = PAR0T(4)
          MT  = PAR0T(5)
          TL  = PAR0T(6)
C----------------------------------------------------------------------
C--->  �A�O�HEH�E RMSW �OCTO�HH�M� �APAMETPAM�.
C----------------------------------------------------------------------
       N = M
       RMSW (1) = RN
       RMSW (2) = P
       RMSW (3) = R
       RMSW (4) = J
       RMSW (5) = MT
       RMSW (6) = TL
C----------------------------------------------------------------------
C--->  �A�O�HEH�E RMSW MACC�BAM�.
C        MACC�B KO��. �APMOH��ECKO�O P��A.
C----------------------------------------------------------------------
C      IPAR0 - C�ET��K MACC�BA PAR0T.
C--------------------------------------------------------------------
       N = NMAS1
       RMSW ( DR7 ) = RN
       I1 = DR7 + 1
       I2 = DR7 + NMAS1
       IPAR0 = 7
          DO 2000 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
2000      CONTINUE
C----------------------------------------------------------------------
C      MACC�B ��HK���   MBH (T)
C--------------------------------------------------------------------
       IPAR0 = IPAR0 + 1
       RN = FLOAT(NMAS2)
       RMSW ( DR8 ) = RN
       I1 = DR8 + 1
       I2 = DR8 + NMAS2
          DO 3000 I = I1, I2
          IPAR0 = IPAR0 + 1
          RMSW (I) = PAR0T (IPAR0)
3000      CONTINUE
       RETURN
       END
