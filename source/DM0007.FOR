         SUBROUTINE DM0007 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0007 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C   -----------    B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTA �E��
C
C                      �P� KODMUM = 1 --> BCE��A H1 � H2 B���PA�TC�
C                                         CO��ACHO MU �APAMETP�, A HE
C                                         �O TEK���M H1 � H2
C
C
C                   ��EKTP��ECKA�  MA��HA 3X3X0:
C                   ****************************
C                  T�� # 11  --> "P��. BAP.", MB(_T_)
C                  T�� # 110 --> "P��. BAP.", MB(_OME�A_)
C                  T�� # 14  --> "P��. BAP.", MB(_T_) - M_FT(#�)
C                  T�� # 140 --> "P��. BAP.", MB(_OME�A_) -  M_FT(#�)
C                  T�� # 12  --> "REDUCE."  , MB(_T_)
C                  T�� # 120 --> "REDUCE."  , MB(_OME�A_)
C                  ****************************************************
C                            ��� T��OB # 11 � # 12 � 14:
C                            ----------------------
C                   M_BHE�(T) - �A�AETC� K�CO�HO-��HE�HO� ��HK��E�
C                               �EPE� MACC�B MAS < T1 Y1 .... >
C                                ( CM. �/� KMAS02 );
C
C                      ��� T��OB # 110 � # 120 � 140:
C                     -------------------------
C              M_BHE�(_OMEGA_) �A�AETC� KO��. �O��HOMA:
C              MAS < A0 A1 A2 ...>;
C
C                      MB := A0 + A1 * OMEGA + A2 * OMEGA**2 + ....
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
C                            ��� T��OB # 14 � 140:
C                            ---------------------
C                  ��EMEHT O��C�BAETC�:
C           � ?? T 11 � #S1N #S2N #S3N #F1N #F2N #F3N
C                       #S1K #S2K #S3K #F1K #F2K #F3K
C                     � RS RF LS  MS  LR  MR  MSR  PTAU GD**2  #�
C                       MAS < T1 Y1 ....   > ;
C    #� - �HA�EH�E  MF_T �TO�O ��-TA B���TAETC� �� M_BHE�.
C
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
C               *** BO�MO�H� �TEPA��OHH�E �TO�HEH�� ***:
C                   �P� BO�����EH�� FITER [ MASPRT(65) ] --->
C                   �HA�EH�� "TOK"_� - �PO�HO� �BH�M METO�OM -
C                   HE B���C���TC�, A B���PA�TC� �� MACC�BA TOKN1;
C               ***
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
C                 - �P� �TOM KOHTPO��P��ETC� "PACXO��EH�E" ME���
C                   �HA�EH��M�, �O���EHH�M� �O �BHO� CXEME (�PO�HO�) �
C                   O�PE�E�EHH�M� �CXO�� �� B���C�EHH�X �HA�EH�� VALLN1;
C                   KOHTPO�� BE�ETC� �O HE �PEB��EH�� EITER[MASPRT(67)]
C                   OTHOC�TE��HO� �O�PE�HOCT�. B HE ��A�O�P��THOM C���AE
C                   B "MAXITR" �A��C�BAETC� MAX_�O�PE�HOCT� �
C                   B "NLITER" ��KC�P�ETC� HOMEP ��EMEHTA.....
C                   �APAMETP "KOD" = 1;
C
C
C                  C) ZAPROS = 3
C                     **********
C                     HE �O��EP��BAETC� ( PE�EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE �O��EP��BAETC�;
C
C   METO�:         ��CKPET��A��� �PABHEH�� COCTO�H�� MO�E�� �AHHO�O
C   -----          T��A HA OCHOBE METO�A ��H���EPA B�O�����:
C                  Y(N+1) = Y(N) + H1*F(N+1) + H2*F(N).
C
C   �APAMETP�:     MSW, RMSW - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------      ( "OTPE�OK" MACC�B� MSWALL -> C.M. �/� GETMSW );
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          B T = TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          B T = TN + H0;
C                  TOKN  �  TOKN1  -  �HA�EH�� "TOKOB" �P� T :=
C                                     TN  �  TN + H0;
C                                   PA�MEPHOCT� CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 5;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C                   ( =1 �P� �O���O� �OK. �O�PE�HOCT� );
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;  + PRINT5 - OT�A�KA;
C   ------------      DEPOSA, ME1N13, ME2N13, ME3N13,
C                     KMAS01, KMAS02,  MA1N13, MA2N13, MA3N13;
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C                3. �P� FTEST=-1 �E�AT� OT�. �H�. �EPE� PRINT5;
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 11, 110, 12, 120:
C                ----------------------------------------------
C                      +  14  � 140:
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C  +--------------------------------------------------------------+
C  I ���� �O�K���EH�� S1N S2N S3N F1N F2N F3N S1K S2K F1K F2K F3K I
C  I           12-T� C�OB: OT 9 �O 20                             I
C  +--------------------------------------------------------------+
C
C              RMSW T�� # 11, 110, 12, 120 [ ��. MA��HA 3X3X0 ] :
C              ************************************************
C         + 14  � 140
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I PE�EPB I"DOM"I PARD I PARM I PARGY I PARG1 I PARJY I PARJ0 I TMASI
C  I 3,4,5  I ��� I      I      I       I       I       I       I     I
C  I        IMEX. I 9-T� I 9-T� I 36-T� I 36-T� I 6-T�  I 6-T�  I NMASI
C  I        I�P-� I C�OB I C�OB I C�OB  I C�OB  I C�OB  I C�OB  I C�OBI
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  I 21 - 25I 26  I27-35 I36-44 I45-80  I81-116 I117-122I123-128I129-@I
C  I ****   I     I      I      I       I       I       I       I     I
C  +--------+-----+------+------+-------+-------+-------+-------+-----+
C  +D1            +D2    +D3    +D4     +D5     +D6     +D7     +D8   +
C
C              @ - O�PE�E��ETC� PA�MEPOM MAS <...> ==> ...+ NMAS + 1
C            *** - 22 - A�PEC B TOKN/N1 �HA�EH�� MFT ��� #� (T�� 14,140)
C
C
C
C--->  D@ -> HOMEPA �O�E� B MSW_ALL MACC�BA;
C
C
C
C
C
C      ���HA MSW_ALL ==> D8 + NMAS + 1; (+1 - HA ���H� TMAS );
C
C      ���HA "�EPEMEHHO�" �ACT� ==> DR8 + NMAS + 1;
C
C      PAC���POBKA MACC�BOB PARD � T.�. CM. B �/� ME@N13 ... � FM0008;
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C
C    OT 1 �O 6   - �HA�EH�� TOKOB �A� O�MOTOK: S1 S2 S3 F2 F2 F3;
C
C       7        -  �HA�EH�E GAMMA -> ��O� �O�O�EH�� POTOPA;
C
C       8        -  �HA�EH�E OMEGA -> ���OBA� �ACTOTA BPA�EH�� POTOPA
C                                     ( B O�HO�EP�O�HO� MO�E�� );
C       9        -  �HA�EH�E MFT - ��. MA�. MOMEHTA;
C
C      10        -  �HA�EH�E M_BHE�(T) - BHE�H�� HA�P��O�H�� MOMEHT;
C
C  �OPM���:
C  ********
C   ��HKT A) ( �E� ��A�A ):
C            �O �AHH�M �� TOKN � VALLN:
C            -1                          D L(N)
C   J0 = L(N)  *[ U(N) - ( R + OMEGA(N) *-------  ) * TOKN ]     (B MSW)
C                                        D GAMMA
C   MFTN = �� TOKN
C
C   MBHN = �� TOKN
C
C   DOMN = [PTAU/JMOM] * ( MBHN - MFTN )    (B MSW)
C
C   TOKN_Y(�BH��) = TOKN + H0 * J0    (B TOKN1)
C
C   GAM_Y(�BH��)  = GAM_N + H0 * OMEG_N  (B TOKN1)
C
C   OMEG_Y(�BH��) = OMEG_N + H0 * DOMN   (B TOKN1)
C
C
C   ��HKT A) ( �PO�O��EH�E, ��� �P� ��A�E, TO��KO �TO):
C   �O �AHH�M �� TOKN1, MSW � TOKN:
C
C   MFTN_1 = F(GAMN_1, TOKN_1) � �A��C� B TOKN1
C
C   MBHN_1 = HE C��TAETC�*** SUB( VALLN, TOKN_1, (TN+H0), ...)
C
C   DOMN_1 = HET **** [PTAU/JMOM] * ( MBHN_1 - MFTN_1 )
C
C   GAMN_1 = �� TOKN1   GAMN + H1 * OMEGN_1 + H2 * OMEGN
C
C   OMEGN_1 = �� TOKN1   OMEGN + H1 * DOMN_1 + H2 * DOMN
C
C   G0 = [ L(N+1) + H1*R + OMEGN_1 * D(L(N+1))/D(GAM) ] ** (-1)
C
C   GY = H1 * G0
C
C   G1 = G0 * L(N+1)
C
C   JY = G1 * [ TOKN + H2 * J0 ]
C
C------
C
C   ��HKT B) -->  PAC�ET TOKN1 � KOHTPO��:
C   �O �AHH�M �� TOKN1, MSW, VALLN1:
C
C   TOKX = GY * UL(N+1) + JY;  UL--> VALLN1(*) - VALLN1(*);
C
C   MFTX = F( GAMN_1, TOKX )
C
C   MBHX = SUB( VALLN1, TOKX, (TN+H0), ...) ��� T��OB 13 � 130
C                                           "-" MFT_��� #�.
C   DOMX = [PTAU/JMOM] * ( MBHX - MFTX )
C
C   OMEGX = OMEGN + H1 * DOMX + H2 * DOMN
C
C   GAMX =  GAMN +  H1 * OMEGX + H2 * GAMN
C
C   �O�CK  EPS:= [MAX] -> ABS(TOKX - TOKN1)/MAX(TOKX,TOKN1)
C
C   �P�  EPS > MAXITR --> ��KCA��� C�T�A���: NLITER, NSITER,...
C                         KOD = 1
C                         *******
C
C   ��KCA��� B TOKN1 �HA�EH�� TOKX, .....
C
C
CZZZ-------------------------------------------------------------------
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      DIMENSION G12(12,12), J12(12)
C      REAL J12
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         REAL JALL, ME3N13, MA3N13
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION UL(6),  TOKX(6)
         INTEGER ADRMSW
         INTEGER D1,D2,D3,D4,D5,D6,D7,D8
         INTEGER FTEST
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( FTEST,  MASPRT(60) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( MU,     MASPRT(31) )
         EQUIVALENCE ( H1OLD,  MASPRT(32) )
         EQUIVALENCE ( H2OLD,  MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( KODMUM, MASPRT(114) )
         INTEGER FITER,  ZAPROS, TIP
         REAL MAXITR, MFTN, MBHN, KMAS02, MFTX, MBHX, KMAS01, MU, MFTN1
         DATA KAKA9 / 9 /
C**********************************************************************
       DATA D1, D2, D3, D4, D5, D6,  D7,  D8
     =  /   21, 27, 36, 45, 81, 117, 123, 129 /
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C   BE����H�, �AB�C���E OT KOHKPETHO� MO�E��:
C 1) BCE D1,....  CM. B��E;
C 2) PA�MEPHOCT� PA�. MACC�BOB ( H��H� KXALL, A HE KALL):
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C 3) PA�MEPHOCT� �ACT� "GY", "JY" KXALL - KOTOP�� BK�A��BAEM:
       DATA KXALL/6/
C 4) "�O�H�E" PA�MEPHOCT� GY � JY:
       DATA KALL/6/
C 5) BC�OM. �KA�ATE��-> CME�EH�E B MSW ��� COOTB. MACC�BOB
C    ( HA "1" MEH��E COOTB. D1, D2,....):
       DATA IPARD, IPARM, IPARGY, IPARG1, IPARJY, IPARJ0, ITMAS
     =    /  26,    35,    44,     80,      116,    122,   128 /
C 6) �KA�ATE�� HA "DOM" C�OBO:
       DATA IDOM/26/
C 7) CME�EH�E B TOKN/TOKN1 HA "MEXAH��ECK�E" �EPEMEHH�E:
       DATA IGAM, IOMEG, IMFT, IMBH
     =    /  +6,   +7,    +8,   +9  /
C 8) �KA�ATE�� HA [PTAU/JMOM] OTHOC�TE��HO PARM( OT CME�EH�� D3 ):
       DATA IPTJ/+8/
C--->  ��HK��� "�B�XMEPH�X" MACC�BOB:
       IDGY(I,J) = IPARGY + (J-1)*KALL + I
       IDG1(I,J) = IPARG1 + (J-1)*KALL + I
       IDYALL(I,J) = (J-1) * NDALL + I
C      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       TIP = MSW(2)
C--
       IF ( TIP .EQ. 11  .OR. TIP .EQ.110.OR.TIP .EQ.14.OR.TIP.EQ.140
     =  .OR.  TIP .EQ. 12  .OR. TIP .EQ. 120       ) GO TO  99990
       GO TO 9999
99990  CONTINUE
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C--->  B��OP H1  � H2 --->
       H1 = H1OLD
       H2 = H2OLD
       IF ( KODMUM .EQ. 0 )  GO TO 3000
       H1 = H0 * ( 1.0 - MU )
       H2 = H0 * MU
3000   CONTINUE
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C-->  �KA�-�� HA TOK...
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
C--->  B��OP PE��MA �A�POCA..
       GO TO( 100, 200), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C-->  �A�EH�E HA�P��EH��:
       DO 102 I=1,KALL
       IF ( I .GT. KXALL ) GO TO 1020
C-->  ���� PEA��H�E, HE �EMP�EPH�E...
       K = MSW( I + 8 )
       M = MSW( 8 + KXALL + I )
       VK = 0.0
       IF ( K .NE. 0 ) VK = VALLN(K)
       VM = 0.0
       IF ( M .NE. 0 ) VM = VALLN(M)
       UL(I) = VK - VM
       GO TO 102
1020   CONTINUE
C-->  �EM��EPH�E ����...
       UL(I) = 0.0
102    CONTINUE
C--->
C-->  HE �POC�T �� �TEPA��� ?
       IF ( FITER .EQ. 1 ) GO TO 101
C-->  ��HKT A) --- HA�A�O -----(�E� ��A�A)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66666) ( IQ, RMSW(IQ), IQ=D1,D8 )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C66666  FORMAT(1X,5(1X,'R(',I3,')=', G12.5 ) )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  PAC�ET J0:
       GAMMA = TOKN(ITOK+IGAM)
       OMEGA = TOKN(ITOK+IOMEG)
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     = CALL ME2N13( RMSW(D2), RMSW(D3), OMEGA, GAMMA,
     =              TOKN(ITOK), UL, RMSW(D7) )
       IF ( TIP.EQ.11.OR.TIP.EQ.110.OR.TIP.EQ.14.OR.TIP.EQ.140 )
     = CALL MA2N13( RMSW(D2), RMSW(D3), OMEGA, GAMMA,
     =              TOKN(ITOK), UL, RMSW(D7) )
C??????????????????????????????????????????????????????????????????????
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66666) ( IQ, RMSW(IQ), IQ=D1,D8 )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  MFT_N  �  MBH_N
C*****
C      �MEET CM�C� �EPEC��TAT�, T.K. B C�. �HTEP�. TOK ��MEH.
C     MFT -->  MEH. O�EH� C��.
         MFTN = TOKN(ITOK+IMFT)
C      #########################
C@@    IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 )
C@@  =     MFTN = ME3N13( RMSW(D2), GAMMA, TOKN(ITOK) )
C@@    IF ( TIP.EQ.11.OR.TIP.EQ.110 .OR. TIP.EQ.14.OR.TIP.EQ.140)
C@@  =     MFTN = MA3N13( RMSW(D2), GAMMA, TOKN(ITOK) )
C-->
C66667  FORMAT('  MFTN=',G12.5,' MBHN=',G12.5,' G=',G12.5,' OM=',G12.5)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C   MBHN -->  MO�HO B���T� �� TOKN,
C   MO�HO �EPEC��TAT� ��� ��ETA F(OMEGA)
C***
C##    IF ( TIP .EQ. 11  .OR.   TIP .EQ. 12  .OR. TIP  .EQ. 14 )
C##  = MBHN = KMAS02( TN, RMSW(D8) )
C##
C##    IF ( TIP .EQ. 110  .OR.   TIP .EQ. 120  .OR. TIP .EQ. 140 )
C##  = MBHN = KMAS01(OMEGA,  RMSW(D8) )
C##
C@@@@@ TOKN(ITOK+IMFT) = MFTN
C###   TOKN(ITOK+IMBH) = MBHN
       MBHN = TOKN(ITOK + IMBH )
C      #########################
C
C??????????????????????????????????????????????????????????????????????
C-->  DOM_N -> MEX. XAP-KA..
       DOMN = RMSW(D3+IPTJ) * ( MBHN - MFTN )
       RMSW ( IDOM ) = DOMN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66667) MFTN, MBHN, GAMMA, OMEGA
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C--->  �PO�HO� "�BH�X" �HA�EH��
       DO 103 I=1,KALL
       TOKN1( ITOK1 + I ) = TOKN(ITOK1+I) + H0*RMSW(IPARJ0+I)
103    CONTINUE
       TOKN1(ITOK + IGAM ) = GAMMA + H0 * OMEGA
       TOKN1(ITOK + IOMEG ) = OMEGA + H0 * DOMN
C----------------------------------------------------------------------
C-->  ��HKT A) C ��A�OM....
101    CONTINUE
C**********************************************************************
C
C  -->  "�BH�E" ���  "�TEPA��OHH�E" GAMMA � OMEGA-->  OMEG1 � GAM1
       OMEG1 = TOKN1( ITOK + IOMEG )
       GAM1  = TOKN1( ITOK + IGAM  )
C**** �EPEC�ET MFT ��� �O���EP�K� T��OB 14 � 140:::>
C   ��� �HA�EH�� GAM1 � TOKN1  PAC�ET MFTN1 � �A��C� E�O B TOKN1:
C
C      #########################
       IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 )
     =     MFTN1 = ME3N13( RMSW(D2), GAM1,  TOKN1(ITOK) )
       IF ( TIP.EQ.11.OR.TIP.EQ.110 .OR. TIP.EQ.14.OR.TIP.EQ.140)
     =     MFTN1 = MA3N13( RMSW(D2), GAM1,  TOKN1(ITOK) )
       TOKN1(ITOK + IMFT ) = MFTN1
C    ############
C-->  GAMMA  �  OMEGA  B TO�EK  TN--->
       OMEGA = TOKN( ITOK + IOMEG )
       GAMMA = TOKN( ITOK + IGAM  )
C--> �O  �AHH�M �PO�HO�A  HAXO��M  GY  �  G1 --->
C
C-->  PAC�ET "�O�H�X" ( T.E KALL HA KALL ) GY � G1....
       IF ( TIP .EQ. 12  .OR. TIP .EQ. 120 )
     = CALL ME1N13( RMSW(D2), RMSW(D3), H1, OMEG1, GAM1, RMSW(D4),
     =                 RMSW(D5) )
       IF ( TIP .EQ. 11  .OR. TIP .EQ. 110 .OR.TIP.EQ.14.OR.TIP.EQ.140)
     = CALL MA1N13( RMSW(D2), RMSW(D3), H1, OMEG1, GAM1, RMSW(D4),
     =                 RMSW(D5) )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C      WRITE(6,66666) ( IQ, RMSW(IQ), IQ=D1,D8 )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C-->  PAC�ET JY...
       IF ( H2 .EQ. 0.0 ) GO TO  104
C-->  H2 HE 0.0,  ��ET J0...
       DO 105 I=1,KALL
       TOKX(I) = TOKN(ITOK1+I) + H2 * RMSW(IPARJ0+I)
105    CONTINUE
C-->  JY = G1 * TOKX
       DO 106 I=1,KALL
       SUM = 0.0
       DO 107 J=1,KALL
       INDEX = IDG1(I,J)
       SUM = SUM + RMSW(INDEX) * TOKX(J)
107    CONTINUE
       RMSW(IPARJY+I) = SUM
106    CONTINUE
       GO TO 1070
C**********************************************************************
104    CONTINUE
C-->  H2 = 0.0,  �O�ABK� HET
       DO 108 I=1,KALL
       SUM = 0.0
       DO 109 J = 1,KALL
       INDEX = IDG1(I,J)
       SUM = SUM + RMSW(INDEX) * TOKN(ITOK1 + J)
109    CONTINUE
       RMSW(IPARJY + I ) = SUM
108    CONTINUE
C----------------------------------------------------------------------
1070   CONTINUE
C
C   GY   �  JY   O�PE�E�EH�....
C---  BK�A��BAEM ----
       IIX = KAKA9 + KXALL
       CALL DEPOSA( MSW(KAKA9), MSW(IIX), KXALL, KALL,RMSW(D4),RMSW(D6),
     =              YALL,  JALL )
C
C--->   �� GY � JY  �O����M "PA�BEPH�T�E" MATP���--->
C$$$$$$CALL MOT005( RMSW(D4), G12,  RMSW(D6), J12 )
C-->  B�O��M B P��H��--->******
C
C   BK�A��
C        DO  552 I=1,12
C        DO  553 J1=1,12
C        IJ1XX = IDYALL( MSW(8+I), MSW(8+J1) )
C****
C553      YALL(        IJ1XX       )=YALL( IJ1XX )
C    =                              + G12( I, J1 )
C552      JALL(MSW(8+I))=JALL(MSW(8+I)) - J12(I)
C                                      $  -------- !!!
C@@@@
       KOD = 0
       IF ( FTEST .NE. -1 ) RETURN
C--->  OT�A�O�HA� �E�AT�:
       CALL PRINT5( TN, H1, H2, RMSW, TOKN, TOKN1, ITOK )
       RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C---- ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  �A�EH�E HA�P��EH��-->  HOBOE
       DO 201 I=1,KALL
       IF ( I .GT. KXALL ) GO TO 2010
C-->  ���� PEA��H�E, HE �EMP�EPH�E...
       K = MSW( I + 8 )
       M = MSW( 8 + KXALL + I )
       VK = 0.0
       IF ( K .NE. 0 ) VK = VALLN1(K)
       VM = 0.0
       IF ( M .NE. 0 ) VM = VALLN1(M)
       UL(I) = VK - VM
       GO TO 201
2010   CONTINUE
C-->  �EM��EPH�E ����...
       UL(I) = 0.0
201    CONTINUE
C-->  HA��EM TOK  �O  VALLN1...
       DO 202 I=1,KALL
       SUM = RMSW(IPARJY + I )
       DO 203 J=1,KALL
       INDEX = IDGY(I,J)
       SUM = SUM + RMSW(INDEX)*UL(J)
203    CONTINUE
       TOKX(I) = SUM
202    CONTINUE
C--->  MFT  �  MBH ...
C  -->  "�BH�E" ��� �TEP. �HA�EH�� ��� TN1 ( �PE�CKA�AH�E) -->
       GAM1 = TOKN1(ITOK + IGAM)
       OMEG1 = TOKN1(ITOK + IOMEG)
C######################################################################
C   ��� T��A 14/140 B��OP MFTN1  �� TOKN1(#�) ��� ��ETA B MBH(TN1)
       IF ( TIP.EQ.14.OR.TIP.EQ.140) MFTN1=TOKN1( MSW(D1+1) )
C######################################################################
C--->   �HA�EH��  GAMMA � OMEGA  B TN  TO�EK -->
       GAMMA = TOKN(ITOK+IGAM)
       OMEGA = TOKN(ITOK+IOMEG)
C-->  # HE �BH�� �A� ��� MEXAH�K� #####--->  ( MFT  �O HOB�M TOKAM )::>
       IF ( TIP .EQ. 12  .OR.  TIP .EQ. 120 )
     =          MFTX = ME3N13( RMSW(D2), GAM1, TOKX )
C
       IF ( TIP .EQ. 11  .OR.  TIP .EQ.110.OR.TIP.EQ.14.OR.TIP.EQ.140)
     =          MFTX = MA3N13( RMSW(D2), GAM1, TOKX )
C-->   M_BH  ���  TN+H0   :-->
C****
C  MBHX-->  C ��ETOM TOKX, GAM1, OMEG1---> ��� 14/140 ��ECT� MFTN1(#�)
C****
       TN1 = TN + H0
       IF ( TIP .EQ. 11  .OR.   TIP .EQ. 12  .OR. TIP .EQ. 14 )
     = MBHX = KMAS02( TN1, RMSW(D8) )
C
       IF ( TIP .EQ. 110  .OR.   TIP .EQ. 120  .OR. TIP .EQ. 140 )
     = MBHX = KMAS01( OMEG1,   RMSW(D8) )
C--
C######################################################################
       IF ( TIP .EQ. 14 .OR. TIP .EQ. 140 ) MBHX = MBHX - MFTN1
C######################################################################
       DOMX = RMSW(D3+IPTJ) * ( MBHX - MFTX )
       DOMN = RMSW(IDOM)
C--->  ####  B��O�HEH�E  HE�BHO�O  �A�A ( �TA� KOPPEK��� )::-->
       OMEGX = OMEGA + H1*DOMX + H2*DOMN
       GAMX = GAMMA + H1*OMEG1 + H2*OMEGA
C
C
C----------------------------------------------------------------------
C-->  O�EHKA �O�PE�HOCT�...
       EPSX = 0.0
       NSM = 0
       DO 204 I=1,KALL
       X1 = ABS( TOKN1(ITOK1+I) )
       X2 = ABS( TOKX(I) )
C++++
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP )  GO TO 2040
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1 - X2 ) / XZ
       GO TO 2041
2040   EPS = ABS( X1 - X2 )
2041   CONTINUE
C+++++
       IF ( EPS .LT. EPSX ) GO TO 204
C-->  ��KC�P�EM  B MO�E��...
       EPSX = EPS
       NSM = I
204    CONTINUE
C-->  ��� MEXAH�K�....
C----------------------------------------------------------------------
       X1 = ABS(GAM1)
       X2 = ABS(GAMX)
C++++
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP )  GO TO 2050
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1 - X2 ) / XZ
       GO TO 2051
2050   EPS = ABS( X1 - X2 )
2051   CONTINUE
C+++++
       IF ( EPS .LT. EPSX ) GO TO 205
C-->  ��KC�P�EM..
       EPSX = EPS
       NSM = IGAM + 1
205    CONTINUE
C----------------------------------------------------------------------
       X1 = ABS(OMEG1)
       X2 = ABS(OMEGX)
C++++
       IF ( X1 .LE. ZEROCP  .OR.  X2 .LE. ZEROCP )  GO TO 2060
       XZ = AMAX1( X1, X2 )
       EPS = ABS( X1 - X2 ) / XZ
       GO TO 2061
2060   EPS = ABS( X1 - X2 )
2061   CONTINUE
C+++++
       IF ( EPS .LT. EPSX ) GO TO 206
C-->  ��KC�P�EM..
       EPSX = EPS
       NSM = IOMEG + 1
206    CONTINUE
C--->  �EPE�A��C� TOKOB  B   TOKN1...
       DO 207 I=1,KALL
       TOKN1(ITOK1+I) = TOKX(I)
207    CONTINUE
       TOKN1(ITOK+ IGAM) = GAMX
       TOKN1(ITOK+ IOMEG) = OMEGX
       TOKN1(ITOK+ IMFT ) = MFTX
       TOKN1(ITOK+ IMBH ) = MBHX
C--->  ECT� �� "�O���A�" O���KA  ????
       KOD = 0
       IF ( EPSX .LT. EITER ) RETURN
       KOD = 1
C-->  O���KA ECT�, HE H��HO �� EE ��KC�POBAT�....
       IF ( EPSX .LT. MAXITR ) RETURN
       MAXITR = EPSX
       NLITER = MSW(1)
       NSITER = NSM
       RETURN
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0007 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
         END
