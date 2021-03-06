       SUBROUTINE FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA  FM0008( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA�HA�EH�E:  1) HACTPO�KA MAKPOMO�E�� ��EKTP��ECKO�  MA��H� 3X3X0:
C   ----------      ***************************************************
C                  T�� # 11  --> "P��. BAP.", MB(_T_)
C                  T�� # 110 --> "P��. BAP.", MB(_OME�A_)
C                  T�� # 14  --> "P��. BAP.", MB(_T_) - M_FT( *#�* )
C                  T�� # 140 --> "P��. BAP.", MB(_OME�A_) - M_FT( *#�*)
C                  T�� # 12  --> "REDUCE."  , MB(_T_)
C                  T�� # 120 --> "REDUCE."  , MB(_OME�A_)
C                  ****************************************************
C                            ��� T��OB # 11 , # 12 � # 14:
C                            -----------------------------
C                   M_BHE�(T) - �A�AETC� K�CO�HO-��HE�HO� ��HK��E�
C                               �EPE� MACC�B MAS < T1 Y1 .... >
C                                ( CM. �/� KMAS02 );
C
C                      ��� T��OB # 110 , # 120 � # 140:
C                     ---------------------------------
C              M_BHE�(_OMEGA_) �A�AETC� KO��. �O��HOMA:
C              MAS < A0 A1 A2 ...>;
C
C                      MB := A0 + A1 * OMEGA + A2 * OMEGA**2 + ....
C
C
C                      ��� T��OB 14 � 140 �� O�PE�E�EHHO�O M_BH
C                      ------------------     B���TAETC� M_FT #�-TA;
C
C
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
C              RMSW T�� # 11, 110, 12, 120, 14, 140:
C              **************************************
C
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PE�EPB I"D0X"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 3,4,5  I ��� I      I      I       I       I       I       I     I
C  I 1,2 ���IMEX. I 9-T� I 9-T� I 36-T� I 36-T� I 6-T�  I 6-T�  I NMASI
C  IT-13,130I�P-� I C�OB I C�OB I C�OB  I C�OB  I C�OB  I C�OB  I C�OBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I  1 - 5 I  6  I7 - 15I16-24 I25-60  I61-96  I97-102 I103-108I109-@I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I ****** I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - O�PE�E��ETC� PA�MEPOM MAS <...> ==> ...+ NMAS + 1
C              ** - 1-> HOMEP #�, 2--> A�PEC MFT ==> ��� T. 14 � 140
C
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
       REAL LS, MS, LR, MR, MSR, JMOM, LS1, MS1, LR1, MR1, MSR1,
     =      LMSP, LMSM, LMRM, LMRP, MSR2D3
       REAL MSSSS0, MFFFF0, MSSFF0, LVSS0, LVFF0
C----------------------------------------------------------------------
       INTEGER D1,D2,D3,D4,D5,D6,D7,D8
       INTEGER DR1,DR2,DR3,DR4,DR5,DR6,DR7,DR8
C
       DATA D1, D2, D3, D4, D5, D6,  D7,  D8
     =  /   21, 27, 36, 45, 81, 117, 123, 129 /
C
C--->  D@ -> HOMEPA �O�E� B MSW_ALL MACC�BA;
       DATA DR1,  DR2,  DR3,  DR4,  DR5,  DR6,  DR7,  DR8
     =  /   1,    7,    16,   25,   61,   97,  103,   109 /
C
C--->  DR@ -> HOMEPA �O�E� B "OTPE�KE" �EPEMEHHO� �ACT� RMSW;
C
C      ���HA MSW_ALL ==> D8 + NMAS + 1; (+1 - HA ���H� TMAS );
C
C      ���HA "�EPEMEHHO�" �ACT� ==> DR8 + NMAS + 1;
C----------------------------------------------------------------------
       DATA KALL/6/, GGRAV / 9.81 /, SQRT3/1.732050808/
C
       TIP = BA0T(2)
         NEL = BA0T(1)
         IF ( TIP .EQ. 11 .OR. TIP .EQ. 110  .OR. TIP .EQ. 12
     =        .OR.  TIP .EQ. 120 .OR. TIP .EQ. 14  .OR. TIP .EQ. 140 )
     =      GO TO 99999
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
       IF ( TIP .EQ. 11  .OR.  TIP .EQ. 110 )
     = NMAS = IFIX( PAR0T(10) ) + 1
C             *********************
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     =   NMAS = IFIX ( PAR0T(9) ) + 1
       IF ( TIP .EQ. 14  .OR.  TIP .EQ. 140 )
     = NMAS = IFIX( PAR0T(11) ) + 1
C             *********************
       LM = D8 + NMAS
       RETURN
C######################################################################
1000   CONTINUE
       TIP = BA0T(2)
C-->  �OPM�POBAT� RMSW....
C-->  LM - ���HA "�EPEMEHHO�" �ACT� RMSW...
       IF ( TIP .EQ. 11  .OR. TIP .EQ. 110 )
     =   NMAS = IFIX ( PAR0T(10) ) + 1
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     =   NMAS = IFIX ( PAR0T(9) ) + 1
       IF ( TIP .EQ. 14  .OR. TIP .EQ. 140 )
     =   NMAS = IFIX ( PAR0T(11) ) + 1
C***********************************
C
       LM = DR8 + NMAS
C-> ��CTKA
       DO 1 I=1,LM
       RMSW(I) = 0.0
1      CONTINUE
C
       IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 ) GO TO 1001
C
       IF ( TIP .EQ. 14  .OR.  TIP .EQ. 140 ) GO TO 1013
C
C
13130     CONTINUE
C    P � � H O �  B A P � A H T..............
C-->  �P�EM �OCTO�HH�X �APAMETPOB...
C--------------------------------------------------------------------
          RS     = PAR0T(1)
C-
          RR     = PAR0T(2)
C-
          LS     = PAR0T(3)
          MS     = PAR0T(4)
          LR     = PAR0T(5)
          MR     = PAR0T(6)
          MSR    = PAR0T(7)
          PTAU   = PAR0T(8)
          JMOM   = PAR0T(9) / ( 4.0 * GGRAV )
C--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
C  *** BO�MO�EH KOHTPO��, �OKA HET   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C       NWTR = MASPRT(13)
C!!!-------------------------------------------------------------------
C--->  �A�O�HEH�E PARD....(DR2 - HA�A�O )
C----------------------------------------------------------------------
       RMSW(DR2 +0) = RS
       RMSW(DR2 +1) = RR
       RMSW(DR2 +2) = LS
       RMSW(DR2 +3) = MS
       RMSW(DR2 +4) = LR
       RMSW(DR2 +5) = MR
       RMSW(DR2 +6) = MSR
       RMSW(DR2 +7) = PTAU
       RMSW(DR2 +8) = JMOM
C----------------------------------------------------------------------
C--> �A�O�HEH�E PARM---> KO��. O�PATHO� MATP��� L(GAMMA):
C -->  CO��ACHO "P��HO�O" BAP�AHTA !!!!!!!!
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C****** ��EMEHT� O�PATHO� ******
       LMSP = LS + MS
       LMSM = LS - MS
       LMRP = LR + MR
       LMRM = LR - MR
       MSR2D3 = 3.0 * MSR * MSR
C------->  O�PE�E��TE��....
       ZLM = 4.0 * LMSM * LMRM  -  3.0 *MSR2D3
       LS1 = ( 4.0 * LMSP * LMRM  -  MSR2D3 ) / (ZLM*(LS+2.0*MS))
       MS1 =-( 4.0 *  MS  * LMRM  +  MSR2D3 ) / (ZLM*(LS+2.0*MS))
       LR1 = ( 4.0 * LMRP * LMSM  - MSR2D3  ) / (ZLM*(LR+2.0*MR))
       MR1 =-( 4.0 *  MR  * LMSM  + MSR2D3  ) / (ZLM*(LR+2.0*MR))
       MSR1 = -4.0 * MSR / ZLM
C--->  MACC�B PARM......
       RMSW(DR3+0) = LS1
       RMSW(DR3+1) = MS1
       RMSW(DR3+2) = LR1
       RMSW(DR3+3) = MR1
       RMSW(DR3+4) = MSR1
       RMSW(DR3+5) = LMSP
       RMSW(DR3+6) = LMSM
       RMSW(DR3+7) = MSR2D3
       RMSW(DR3+8) = PTAU / JMOM
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( TIP .EQ. 14  .OR. TIP .EQ. 140 ) RETURN
C      ---------------------------------------------
C--->  �A�O�HEH�E TMAS--->  MAS <....>
       DO 2 I=1, NMAS
       RMSW(DR8-1 + I ) = PAR0T(10-1 + I )
2      CONTINUE
       RETURN
1001   CONTINUE
C
C-->  �P�EM �OCTO�HH�X �APAMETPOB...   " R E D U C E ..."
C--------------------------------------------------------------------
          MSSSS0 = PAR0T(5)
          MFFFF0 = PAR0T(6)
          LVSS0  = PAR0T(3)
          LVFF0  = PAR0T(4)
          RSS0   = PAR0T(1)
          RFF0   = PAR0T(2)
          PTAU   = PAR0T(7)
          JMOM   = PAR0T(8) / ( 4.0 * GGRAV )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         MSSFF0 = SQRT( MSSSS0 * MFFFF0 )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C--------------------------------------------------------------------
C!!!-------------------------------------------------------------------
C  *** BO�MO�EH KOHTPO��, �OKA HET   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        NWTR = MASPRT(13)
       WRITE(NWTR,100) BA0T(1),  RSS0, RFF0, LVSS0, LVFF0,
     =                 MSSSS0, MFFFF0, MSSFF0, PTAU, JMOM
 100    FORMAT(/ ' *** ��EMEHT # ', I4, ' - ��EKTP��ECKA� MA��HA',
     = ' 3X_3X_0X ***'/
     = ' *** C��COK BBE�EHH�X �APAMETPOB O��CAH�� MAKPOMO�E��: ***'/
     = ' *** ', ' R_SS=',G12.5,' R_FF=', G12.5,' LV_SS=' ,G12.5, '***'/
     = ' *** ', 'LV_FF=',G12.5,' M_SS=', G12.5,'  M_FF=',G12.5,' ***'/
     = ' *** ', ' M_SF=',G12.5,' P_TAU=',G11.4,' J_MOM=',G12.5,' ***'/)
C
C
C
C!!!-------------------------------------------------------------------
C--->  �A�O�HEH�E PARD....(DR2 - HA�A�O )
C----------------------------------------------------------------------
       RMSW(DR2 +0) = RSS0
       RMSW(DR2 +1) = RFF0
       RMSW(DR2 +2) = LVSS0
       RMSW(DR2 +3) = LVFF0
       RMSW(DR2 +4) = MSSSS0
       RMSW(DR2 +5) = MFFFF0
       RMSW(DR2 +6) = MSSFF0
       RMSW(DR2 +7) = PTAU
       RMSW(DR2 +8) = JMOM
C----------------------------------------------------------------------
C--> �A�O�HEH�E PARM---> KO��. O�PATHO� MATP��� L(GAMMA):
C----------------------------------------------------------------------
C-->  O�PE�E��TE��:
      DEN      =4 /(LVFF0*LVSS0*(9 *MFFFF0**2*LVSS0**2+12 *MFFFF0*LVFF0
     X *LVSS0**2+18 *MSSFF0**2*LVFF0*LVSS0+9 *MSSSS0**2*LVFF0**2+12 *
     X MSSSS0*LVFF0**2*LVSS0+4 *LVFF0**2*LVSS0**2))
      RMSW(DR3)=(  DEN    *LVFF0*(9 *MFFFF0**2*LVSS0**2+12 *MFFFF0*
     X LVFF0*LVSS0**2+12 *MSSFF0**2*LVFF0*LVSS0+3 *MSSSS0**2*LVFF0**2+
     X 8 *MSSSS0*LVFF0**2*LVSS0+4 *LVFF0**2*LVSS0**2))/4
      RMSW(DR3+1)=(  DEN    *LVFF0**2*(3 *MSSFF0**2*LVSS0+3 *MSSSS0**2*
     X LVFF0+2 *MSSSS0*LVFF0*LVSS0))/4
      RMSW(DR3+2)=(DEN*LVSS0*(3 *MFFFF0**2*LVSS0**2+8 *MFFFF0*LVFF0
     X *LVSS0**2+12 *MSSFF0**2*LVFF0*LVSS0+9 *MSSSS0**2*LVFF0**2+12 *
     X MSSSS0*LVFF0**2*LVSS0+4 *LVFF0**2*LVSS0**2))/4
      RMSW(DR3+3)=(DEN*LVSS0**2*(3 *MFFFF0**2*LVSS0+2 *MFFFF0*LVFF0
     X *LVSS0+3 *MSSFF0**2*LVFF0))/4
      RMSW(DR3+4)=(DEN*MSSFF0*LVFF0*LVSS0*(-3 *MFFFF0*LVSS0-3 *
     X MSSSS0*LVFF0-2 *LVFF0*LVSS0))/2
      RMSW(DR3+5)=(DEN*MSSFF0*LVFF0*LVSS0*(3 *MFFFF0*LVSS0+3 *
     X MSSSS0*LVFF0+2 *LVFF0*LVSS0))/4
      RMSW(DR3+6)=(DEN *MSSFF0*LVFF0*LVSS0*SQRT3*(3 *MFFFF0*LVSS0+3
     X *MSSSS0*LVFF0+2 *LVFF0*LVSS0))/4
      RMSW(DR3+7)=(DEN*MSSFF0*LVFF0*LVSS0*SQRT3*(-3 *MFFFF0*LVSS0-
     X 3 *MSSSS0*LVFF0-2 *LVFF0*LVSS0))/4
      RMSW(DR3+8 )=PTAU/JMOM
C********************************************************************
C     #_13:  STATOR_3, ROTOR_3, DEMPF_0  G * 31.01.86 *
C*********************************************************************
C--->  �A�O�HEH�E TMAS--->  MAS <....>
       DO 20 I=1, NMAS
       RMSW(DR8-1 + I ) = PAR0T(09-1 + I )
20     CONTINUE
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C             NWTRE = MASPRT(14)
C           WRITE(NWTRE,667) DEN
C667      FORMAT(10X,' DEN=', G12.5 )
C       WRITE(NWTRE, 666 )   ( I,RMSW(I), I=1,LM)
C666     FORMAT(1X, 5( 'R(',I3,')=',G12.5, 1X ) )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       RETURN
C----------------------------------------------------------------------
1013   CONTINUE
C--->  �A�O�HEH�E TMAS--->  MAS <....>
       DO 30 I=1, NMAS
       RMSW(DR8-1 + I ) = PAR0T(11-1 + I )
30     CONTINUE
C****              HOMEP ��-TA CB���....
            N = PAR0T(10)
       RMSW ( 1)  =  RN
C-->  �A�EE TAK �E KAK ��� 11 ��� 110
       GO TO 13130
C......................................................................
       END
