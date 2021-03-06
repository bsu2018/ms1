       SUBROUTINE FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT ��� 1992 �.)------------
C   �O��PO�PAMMA  FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA�HA�EH�E:  
C                     ���������� ��� ���   �����������
C
C
C                   1) HACTPO�KA MAKPOMO�E�� ��EKTP��ECKO�  MA��H� 3X3X0:
C   ----------      ***************************************************
C                  T�� # 11  --> "P��. BAP.", MB(_T_)
C                  ****************************************************
C                            ��� T��OB # 11 � 110:
C                            ---------------------
C                  ��EMEHT O��C�BAETC�:
C           � ?? T 11 � #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                     � RS RF LS  MS  LR  MR  MSR  PTAU GD**2
C                       MAS < T1 Y1 ....   > ;
C
C                     [ "P��HO�" B A P � A H T ]
C
C
C                            ��� T��OB # 14 � 140:
C                            ---------------------
C                  ��EMEHT O��C�BAETC�:
C           � ?? T 11 � #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                     � RS RF LS  MS  LR  MR  MSR  PTAU GD**2  #�
C                       MAS < T1 Y1 ....   > ;
C
C      #� -- �� �TO�O ��EMEHTA B��T� M_FT � B��ECT� �� M_BHE�.
C                                           *******
C                     [ "P��HO�" B A P � A H T ]
C
C                            ��� T��OB # 12 � 120:
C                            ---------------------
C                  ��EMEHT O��C�BAETC�:
C           � ?? T 11 � #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                     � RS RF LVS LVF MSS MFF     PTAU GD**2
C                       MAS < T1 Y1 ....   > ;
C
C@
C
C
C            2) �P�  FGMSW = 1 -> �TO�H�ETC� �HA�EH�E LM -
C               O��E� ���H� MSW �AHHO�O T��A ( C ��ETOM MAS �AP-OB );
C
C   METO�:      �OPM�POBAH�E �EPEMEHHO� �ACT� MSW_ALL O��CAH�� ��-TA,
C   -----       B���C�EH�E �O AHA��T��ECK�M �OPM��AM KO��-OB O�PATH�X
C               MATP�� � �P. BC�OMO�ATE��H�X �AHH�X, HEO�XO��M�X ���
C               �A��HE��E� PA�OT� MAKPOMO�E��;
C                �EPEBO� B C�CTEM� C� JMOM = GD**2/(4*GGRAV)
C
C   �APAMETP�:  BA0T, KNOT, PAR0T - "OTPE�K�" MACC�BOB BA0, KNOT, PAR0
C   ---------   BBE�EHH�X �EPE� �/� INPUT0;
C               RMSW - �EPEMEHHA� �ACT� MSW_ALL O��CAH�� ��EMEHTA;
C               LM - �P� FGMSW = 0 => ���HA �EPEMEHHO� �ACT� MSW_ALL;
C                    �P� FGMSW = 1 => ���HA BCE�O MACC�BA    MSW_ALL;
C   �P�ME�AH�E: 1. BCE �O��  �AH����TC�
C   ----------  2. �OPM��� ��� MACC�BA PARM -> �O���EH� �EPE� REDUCE;
C                  � B P��H��;
C               3. �OPMAT MSW T��A # 11, 110, 12, 120:
C
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 11:
C                ----------------------------------------------
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C              @ - O�PE�E��ETC� PA�MEPOM MAS <...> ==> ...+ NMAS + 1
C            *** - 22 - A�PEC B TOKN/N1 �HA�EH�� MFT ��� #� (T�� 14,140)
C
C
C
C
C
C      PAC���POBKA MACC�BOB PARD � T.�. CM. B �/� ME@N13 .....
C
C
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       INTEGER BA0T, FGMSW, TIP
       EQUIVALENCE ( FGMSW,  MASPRT(87) )
       EQUIVALENCE ( N    ,  RN         )
       REAL L0, LSD, LSQ, LF, LD, LQ, MF, MD, MQ, MR, R,
     =      RF, RD, RQ, OMEGA
C----------------------------------------------------------------------
       INTEGER D1
       INTEGER DR1
C
       DATA D1/   17  /
C
C--->  D@ -> HOMEPA �O�E� B MSW_ALL MACC�BA;
       DATA DR1/   1  /
C
       DATA FK/1.2247448 /
C
       TIP = BA0T(2)
         NEL = BA0T(1)
         IF ( TIP .EQ. 11  )   GO TO 99999
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0008: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
99999  CONTINUE
C-->  �TO �POC�T?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  �TO�H�T� MSW_ALL PA�MEP
       LM = 110
       RETURN
C######################################################################
1000   CONTINUE
       TIP = BA0T(2)
C-->  �OPM�POBAT� RMSW....
C-->  LM - ���HA "�EPEMEHHO�" �ACT� RMSW...
C
       LM = 95
C-> ��CTKA
       DO 1 I=1,LM
       RMSW(I) = 0.0
1      CONTINUE
C
C
13130     CONTINUE
C    P � � H O �  B A P � A H T..............
C-->  �P�EM �OCTO�HH�X �APAMETPOB...
C--------------------------------------------------------------------
          L0     = PAR0T(1)
          LSD    = PAR0T(2)
          LSQ    = PAR0T(3)
          LF     = PAR0T(4)
          LD     = PAR0T(5)
          LQ     = PAR0T(6)
          MF     = PAR0T(7)
          MD     = PAR0T(8)
          MQ     = PAR0T(9)
          MR     = PAR0T(10)
          R      = PAR0T(11)
          RF     = PAR0T(12)
          RD     = PAR0T(13)
          RQ     = PAR0T(14)
          OMEGA  = PAR0T(15)
C--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
C  *** BO�MO�EH KOHTPO��, �OKA HET   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C       NWTR = MASPRT(13)
C!!!-------------------------------------------------------------------
C--->  �A�O�HEH�E PARD....(DR2 - HA�A�O )
C----------------------------------------------------------------------
       RMSW(DR1 +0)  =   L0
       RMSW(DR1 +1)  =   LSD
       RMSW(DR1 +2)  =   LSQ
       RMSW(DR1 +3)  =   LF
       RMSW(DR1 +4)  =   LD
       RMSW(DR1 +5)  =   LQ
       RMSW(DR1 +6)  =   MF
       RMSW(DR1 +7)  =   MD
       RMSW(DR1 +8)  =   MQ
       RMSW(DR1 +9)  =   MR
       RMSW(DR1 +10) =   R
       RMSW(DR1 +11) =   RF
       RMSW(DR1 +12) =   RD
       RMSW(DR1 +13) =   RQ
       RMSW(DR1 +14) =   OMEGA
C----------------------------------------------------------------------
       RETURN
C......................................................................
       END
