         SUBROUTINE FM0033( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT CEHT��P� 1988 �.)------------
C   �O��PO�PAMMA:   FM0033( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�� TPAHC�OPMATOPOB
C  ------------    *************************************
C     KO�-BO O�-K  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C     K = 2        32      2-X O�M. TPAHC.       TP2
C     K = 3        33      3-X O�M. TPAHC.       TP3
C
C
C                  BBO� �O O��CAH��:
C
C           � # T 32 � @ @ ..  � SH UH PXX IXX PKZ UKZ
C             R1 R2   W1 W2  KY
C
C           � # T 33 � @ @ ..  � Rct Lct  L1  L2  L3
C             R1  R2  R3   W1 W2 W3   KY
C
C          ���� �O�K���EH��: <HA�A�O O�MOTOK> <KOH�� O�MOTOK>
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E��;
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
C   �P�ME�AH�E:    1. KOHTPO�� HE �PO��BO��TC�;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C
C                  RMSW T��OB # 32  33:
C                  --------------------
C
C
C  �OCT.  I �EPEMEHHA� RMSW-->
C +-------+--+--------+--------+--------+------+------+----------+...>
C I ����  I KI R �APA-I L �AP- I W �AP- I      I      I W1 MAT-  I...>
C I       I  I   MET- I   AME- I   AME- I R CT I L CT I    P�-   I...>
C I       I  I    P�  I   TP�  I   TP�  I      I      I    �A    I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I       I 1I K C�OB I K C�OB I K C�OB I  1   I  1   I K*(K+1)/2I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I 9-... I  I        I        I        I      I      I          I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I          I        I        I        I             I
C D0         D1      D2       D3       D4            D5
C DR0        DR1     DR2      DR3      DR4           DR5
C
C
C
C ...>+---------+--------+------+------+------+
C ...>I G MAT-  I J BEK- I      I      I      I
C ...>I   P��A  I   TOP  I JMU  ITOKMUXI  KY  I
C ...>I         I        I      I      I      I
C ...>+---------+--------+------+------+------+
C ...>IK*(K+1)/2I K C�OB I   1  I   1  I   1  I
C ...>+---------+--------+------+------+------+
C ...>I         I        I      I      I      I
C ...>+---------+--------+------+------+------+
C     I         I        I
C     D6       D7       D8
C     DR6      DR7      DR8
C
C
C
C
C               C M E � E H � � :
C               -----------------
C             T�� #32       T�� # 33
C   ----------------------------------
C   K--->       2              3
C   ----------------------------------
C   LM-->      32             44
C   ----------------------------------
C   D0-->       9             9
C   ----------------------------------
C   D1-->      14             16
C   ----------------------------------
C   D2-->      16             19
C   ----------------------------------
C   D3-->      18             22
C   ----------------------------------
C   D4-->      20             25
C   ----------------------------------
C   D5-->      22             27
C   ----------------------------------
C   D6-->      25             33
C   ----------------------------------
C   D7-->      28             39
C   ----------------------------------
C   D8-->      30             42
C   ----------------------------------
C   LMR->      20             30
C-------------------------------------
C
C   ��� �EPEMEHHO� �ACT� DR<*> = D<*> - D1 + 2
C                        $$$$$$$$$$$$$$$$$$$$$
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       DIMENSION MLM(2)
       INTEGER BA0T, TIP
       INTEGER  DR3, DR5
       REAL IH, IXX, LCT, L1, L2, L3
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE ( RN, N )
       DATA MLM / 32, 44 /
C-->  HA� �� T�� ?
       NEL = BA0T(1)
       TIP = BA0T(2)
       NWTR = MASPRT(13)
       IF( TIP .EQ. 32  .OR.  TIP .EQ. 33 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0033: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C----------------------------------------------------------------------
C--> HA�HEM
C----------------------------------------------------------------------
1        CONTINUE
C-->  K MO�E��
       K = TIP - 30
       K1 = K / 3 + 1
C-->  ���HA �EPEMEHHO� �ACT�
       LM = MLM(K1)  -  ( 8+2*K)
       DO 100 I=1, LM
100    RMSW(I) = 0.0
C-->  �A���EM  "K"
       N = K
       RMSW(1) = RN
C----------------------------------------------------------------------
C--> �P�EM �APAMETPOB...
C  �X KO�-BO...
       KPAR = 11
       IF( K .EQ. 3 ) KPAR = 12
C----------------------------------------------------------------------
C*** KOHTPO�� HA �O�O��TE��HOCT� !!!
C----------------------------------------------------------------------
       DO 101  I=1, KPAR
       IF( PAR0T(I) .LT. 0.0 ) GO TO 9999
101    CONTINUE
C----------------------------------------------------------------------
C--> �A���E MO�E�� OT���A�TC�
C----------------------------------------------------------------------
       IF( K .EQ. 3 ) GO TO 3000
C----------------------------------------------------------------------
C    O�HO�A�H�� �B�XO�MOTO�H�� TPAHC�OPMATOP (T�� # 32)
C----------------------------------------------------------------------
       SH = PAR0T(1)
       UH = PAR0T(2)
       IH = SH/UH
       PXX = PAR0T(3)
       IXX = PAR0T(4)*IH/100.
       PKZ = PAR0T(5)
       UKZ = PAR0T(6)*UH/100.
       R1  = PAR0T(7)
       R2  = PAR0T(8)
       RMSW(2) = R1
       RMSW(3) = R2
       W1  = PAR0T(9)
       W2  = PAR0T(10)
       RMSW(6) = W1
       RMSW(7) = W2
C--> �POBEP�M �AHH�E
       PKZMIN = IH**2 * ( R1+R2*W1**2/W2**2 )
       IF( PKZ .LE. PKZMIN ) GO TO 9995
C----------------------------------------------------------------------
C===>  PAC��T�BAEM �H��KT�BHOCT�...
C----------------------------------------------------------------------
       XXX = SQRT(UH**2*IXX**2-PXX**2)/IXX**2
       A = PXX/IXX**2 - R1
       RKZ = PKZ/IH**2
       XKZ = SQRT(UKZ**2*IH**2 - PKZ**2) / IH**2
       AKT= RKZ - R1
       R2 = R2*W1**2/W2**2
C ******************************************
C==>   BH�MAH�E ��EC� �P�BE�EHH�E �HA�EH��
C ******************************************
C==>  B���C���TC� �P�����EHO
       XL2 = XKZ/2.
       XL1 = XL2
       B = XXX - XL1
       RCT = B**2/A + A
       XLCT = RCT*A/B
       PI = 3.1415926
       L1 = XL1/(100.*PI)
       LCT = XLCT/(100.*PI*W1**2)
       RCT = RCT/W1**2
       L2= XL2/(100.*PI)*W2**2/W1**2
C==> �AKOH���� PAC�ET...
       RMSW(4) = L1
       RMSW(5) = L2
       RMSW(8) = RCT
       RMSW(9) = LCT
       RMSW(19)= SQRT(2.0) * IXX
       N = IFIX( PAR0T(11) )
       RMSW(20)= RN
       GO TO 5000
C----------------------------------------------------------------------
C    O�HO�A�H�� TPEXO�MOTO�H�� TPAHC�OPMATOP (T�� # 33)
C----------------------------------------------------------------------
3000   CONTINUE
       RCT = PAR0T(1)
       LCT = PAR0T(2)
       L1  = PAR0T(3)
       L2  = PAR0T(4)
       L3  = PAR0T(5)
       R1  = PAR0T(6)
       R2  = PAR0T(7)
       R3  = PAR0T(8)
       W1  = PAR0T(9)
       W2  = PAR0T(10)
       W3  = PAR0T(11)
C--> ����������� ���������
       R2 = R2*W2**2/W1**2
       R3 = R3*W3**2/W1**2
       L2 = L2*W2**2/W1**2
       L3 = L3*W3**2/W1**2
       LCT = LCT  /  W1**2
       RCT = RCT  /  W1**2
C==> �A������...
       RMSW(2) = R1
       RMSW(3) = R2
       RMSW(4) = R3
       RMSW(5) = L1
       RMSW(6) = L2
       RMSW(7) = L3
       RMSW(8) = W1
       RMSW(9) = W2
       RMSW(10)= W3
       RMSW(11)= RCT
       RMSW(12)= LCT
       RMSW(29)= PAR0T(12) / W1**2
       N = 1
       RMSW(30)= RN
C----------------------------------------------------------------------
C--> PAC��TAEM MATP���  W1
C----------------------------------------------------------------------
5000   CONTINUE
       DR3 = 2 + 2 * K
       DR5 = 4 + 3 * K
       IDR3 = DR3 - 1
       IDR5 = DR5 - 1
          DO 110 I=1,K
          DO 110 J=I,K
          WI = RMSW(IDR3 + I)
          WJ = RMSW(IDR3 + J)
          IDR5 = IDR5 + 1
          RMSW(IDR5) = WI * WJ
110    CONTINUE
       RETURN
C**********************************************************************
C----------------------------------------------------------------------
9995   CONTINUE
C!!!-------------------------------------------------------------------
        NWTRE = MASPRT(14)
        WRITE(NWTRE, 512) TIP, NEL
512   FORMAT(' *** �O�. C�O� B FM0033: O���KA B �CXO�H�X �AHH�X  *** ',
     =      /' *** �AHH�E O��TA KOPOTKO�O �AM�KAH�� HE �PAB���H� *** ',
     =      /' *** ��� �A�AHH�X �APAMETPOB MO�HOCT� PKZ  MA�A    *** ',
     =      /' *** HOMEP T��A ',I4,5X,' ��EMEHT HOMEP:',I4,5X,'  ***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 501) TIP, NEL
501      FORMAT(' *** �O�. C�O� B FM0033: �AHH�E HE �O�O��TE��H�  ***',
     =         /' *** HOMEP T��A ',I4,5X,' ��EMEHT HOMEP:',I4,5X,'***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
       RETURN
       END
