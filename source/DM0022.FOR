         SUBROUTINE DM0022 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0022 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C
C                   MAKPOMO�E�� M-�A�HO�O �A�OBO�O �B��ATE��
C                   C MA�H�THO HE CB��AHH�M� O�MOTKAM�.
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
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST, KMAS02;
C
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C                3. �P� FTEST=-1 �E�AT� OT�. �H�. �EPE� PRINT5;
C
C               "�OCTO�HHA�" �ACT� MSW T��OB # 502...518;
C                ----------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C  +------------------------------------------------------------+
C  I ���� �O�K���EH��  H1  H2 .... K1  K2 .....                 I
C  I               M C�OB: OT 9 �O ( 9 + M -1 )                 I
C  +------------------------------------------------------------+
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
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C
C    OT 1 �O M   - �HA�EH�� TOKOB �A� O�MOTOK;
C
C       M+1      -  �HA�EH�E GAMMA -> ��O� �O�O�EH�� POTOPA;
C
C       M+2      -  �HA�EH�E OMEGA -> ���OBA� �ACTOTA BPA�EH�� POTOPA
C
C       M+3      -  �HA�EH�E MFT - ��. MA�. MOMEHTA;
C
C       M+4      -  �HA�EH�E M_BHE�(T) - BHE�H�� HA�P��O�H�� MOMEHT;
C
C  �OPM���:
C  ********
C   ��HKT A) ( �E� ��A�A ):
C            �O �AHH�M �� TOKN � VALLN:
C
C   GAMEL(N) = GAM(N) * P ------> ( �� TOKN )
C
C   L(N) = F ( GAMEL(N) )
C
C   D L(N)
C   ------- = F ( GAMEL(N) )
C   D GAMEL
C
C            -1                          D L(N)
C   J0 = L(N)  *[ U(N)-(R + OMEG(N)* P *-------  ) * TOK(N) ] ->(B MSW)
C                                        D GAMEL
C
C   MFT(N) = �� TOKN
C
C   MBH(N) = �� TOKN
C
C   W0 = ( MFT(N) - MBH(N) - SIGN(OMEG(N) ) * MT ) / J ------->( B MSW )
C
C   TOK(N+1)(�BH��) = TOK(N) + H0 * J0 ----->( B TOKN1 )
C
C   OMEG(N+1)(�BH��) = OMEG(N) + H0 * W0 --->( B TOKN1 )
C
C   GAM(N+1) (�BH��) = GAM(N) + H2 * OMEG(N) + H1 * OMEG(N+1)->(B TOKN1)
C
C--------------------------------------------------------------------
C   ��HKT A) ( �P� ��A�E ):
C   �O �AHH�M �� TOKN1, MSW � TOKN:
C
C   GAMEL(N+1) = GAM(N+1) * P
C
C   L(N+1) = F ( GAMEL(N+1) ) --------> ( B MSW )
C
C   D L(N+1)
C   -------- = F ( GAMEL(N+1) ) ------> ( B MSW )
C   D GAMEL
C
C   MFTN1 = F( GAM(N+1),TOKN1 )
C                                             D L(N+1)
C   Z1 = [ L(N+1) + H1 * ( R + OMEG(N+1)* P * -------- ) ] ** (-1)
C                                             D GAMEL
C   G(N+1) = H1 * Z1 ----------------------------------> ( B MSW )
C
C   J(N+1) = Z1 * [ TOK(N) + H2 * J0 ] * L(N+1) -------> ( B MSW )
C
C--------------------------------------------------------------------
C   ��HKT B) -->  PAC�ET TOKN1 � KOHTPO��:
C   �O �AHH�M �� TOKN1, MSW, VALLN1:
C
C   TOKX = G(N+1) * UL(N+1) + J(N+1);  UL--> VALLN1(*) - VALLN1(*);
C
C   MFTX = F( GAM(N+1), TOKX )
C
C   OMEGX = OMEG(N)+H2*W0+H1*[MFTX+MBH(N+1)-SIGN(OMEG(N+1)*MT]/J
C
C   GAMX  = GAM(N) + H2 * OMEG(N) + H1 * OMEGX
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
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
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
         INTEGER D1 , D2 , D3 , D4 , D5 , D6 , D7
         INTEGER FITER,  ZAPROS, TIP
         INTEGER ADRMSW
         INTEGER FTEST
         REAL MAXITR,MFTN,MBHN,MFTX,MBHX,MU,MFTN1,LN,LN1,J,MT,J0,KMAS02
         DIMENSION UL(8), TOKX(8)
C--------------------------------------------------------------------
       PI=3.1415927
       TIP = MSW(2)
       NTIP = TIP - 501
       IF ( NTIP .GT. 10 ) NTIP = NTIP - 10
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 7 ) GO TO 10
       GO TO 9999
10     CONTINUE
C--------------------------------------------------------------------
C   BE����H�, �AB�C���E OT KOHKPETHO� MO�E��:
       M = NTIP + 1
       D1   = 9 + 2 * M
C 1) PA�MEPHOCT� PA�. MACC�BOB  KALL :
       KALL = M
C 2) BC�OM. �KA�ATE��-> CME�EH�E B MSW ��� COOTB. MACC�BOB
C    ( HA "1" MEH��E COOTB. D2,D3,....):
       ILN  = D1 + 8
       IDLN = ILN  + M
       IJ0  = IDLN + M
       IGN1 = IJ0  + M
       IJN1 = IGN1 + M
       ILGR = IJN1 + M + 1
       IMBHT= ILGR + MSW(ILGR) + 1
C 3) KO���ECTBO �APMOH�K B �PE�CTAB�EH�� �-���  L(GAMMA):
       NGARM = MSW(ILGR) - 1
C 4) CME�EH�E B TOKN/TOKN1 HA "MEXAH��ECK�E" �EPEMEHH�E:
       IGAM  = M
       IOMEG = M + 1
       IMFT  = M + 2
       IMBH  = M + 3
C--->  BBE�EM �OCTO�HH�E �APAMETP�.
       P  = RMSW( D1 + 1)
       R  = RMSW( D1 + 2)
       J  = RMSW( D1 + 3)
       MT = RMSW( D1 + 4)
       TL = RMSW( D1 + 5)
C----------------------------------------------------------------------
C--->  B��OP H1  � H2 --->
       H1 = H1OLD
       H2 = H2OLD
       IF ( KODMUM .EQ. 0 )  GO TO 30
       H1 = H0 * ( 1.0 - MU )
       H2 = H0 * MU
30     CONTINUE
C----------------------------------------------------------------------
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
       DO 110 I=1,KALL
       NN = MSW( 8 + I )
       NK = MSW( 8 + M + I )
       VN = 0.0
       IF ( NN .NE. 0 ) VN = VALLN(NN)
       VK = 0.0
       IF ( NK .NE. 0 ) VK = VALLN(NK)
       UL(I) = VN - VK
110    CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  HE �POC�T �� �TEPA��� ?
       IF ( FITER .EQ. 1 ) GO TO 175
C--------------------------------------------------------------------
C-->  ��HKT A) --- HA�A�O -----(�E� ��A�A)
C--------------------------------------------------------------------
       GAMN = TOKN(ITOK+IGAM)
       OMEGN = TOKN(ITOK+IOMEG)
C----------------------------------------------------------------------
C--->  O�PE�E��EM �HA�EH�E �H��KT�BHOCT� � EE �PO��BO�HO� ���  T = TN
C----------------------------------------------------------------------
          DO 150  I = 1, KALL
          LN  = RMSW ( ILGR + 1 )
          DLN = 0.0
          GAMEL = GAMN * P - FLOAT( I - 1 ) * TL / FLOAT( M )
             C1 = 2. * PI * GAMEL / TL
             DO 130 I1 = 1, NGARM
             AI1 = FLOAT( I1 )
             LN  = LN  + RMSW( ILGR + I1 +1 ) * COS( AI1 * C1 )
             C2  = AI1 * 2. * PI / TL
             DLN = DLN - RMSW( ILGR + I1 +1 ) * SIN( AI1 * C1 ) * C2
130          CONTINUE
C-->  PAC�ET J0:
         J0 = ( UL(I) - ( R + OMEGN* P * DLN ) * TOKN(ITOK1 + I) ) / LN
         RMSW ( IJ0 + I ) = J0
150      CONTINUE
C--------------------------------------------------------------------
C-->  MFT_N  �  MBH_N
C--------------------------------------------------------------------
         MFTN = TOKN( ITOK + IMFT )
         MBHN = TOKN( ITOK + IMBH )
C--------------------------------------------------------------------
C-->   W0----> MEX. XAP-KA..
       W0 = ( MFTN - MBHN - SIGN(1.,OMEGN) * MT ) / J
       RMSW ( ILN ) = W0
C--------------------------------------------------------------------
C--->  �PO�HO� "�BH�X" �HA�EH��
       DO 170 I=1,KALL
       TOKN1(ITOK1 + I) = TOKN(ITOK1+I) + H0 * RMSW( IJ0+I )
170    CONTINUE
       TOKN1(ITOK + IOMEG ) = OMEGN + H0 * W0
       TOKN1(ITOK + IGAM ) = GAMN  + H2 * OMEGN + H1 * OMEGN1
C**********************************************************************
C-->  ��HKT A) C ��A�OM....
175    CONTINUE
C**********************************************************************
C  -->  "�BH�E" ���  "�TEPA��OHH�E" GAMN1 � OMEGN1
       OMEGN1 = TOKN1( ITOK + IOMEG )
       GAMN1  = TOKN1( ITOK + IGAM  )
C----------------------------------------------------------------------
C--->  O�PE�E��EM �HA�EH�E �H��KT�BHOCT� � EE �PO��BO�HO� ���  T = TN+1
C----------------------------------------------------------------------
          DO 190  I = 1, KALL
          LN1  = RMSW ( ILGR + 1 )
          DLN1 = 0.0
          GAMEL = GAMN1 * P - FLOAT( I - 1 ) * TL / FLOAT( M )
             C1 = 2. * PI * GAMEL / TL
             DO 180 I1 = 1, NGARM
             AI1 = FLOAT( I1 )
             LN1 = LN1 + RMSW( ILGR + I1 +1 ) * COS( AI1 * C1 )
             C2  = AI1 * 2. * PI / TL
             DLN1= DLN1- RMSW( ILGR + I1 +1 ) * SIN( AI1 * C1 ) * C2
180          CONTINUE
         RMSW ( ILN + I ) = LN1
         RMSW ( IDLN + I ) = DLN1
190      CONTINUE
C--------------------------------------------------------------------
C**** �EPEC�ET MFT ��� �HA�EH�� GAMN1 � TOKN1
C     PAC�ET MFTN1 � �A��C� E�O B TOKN1:
C--------------------------------------------------------------------
         MFTN1 = 0.0
         DO 192 I = 1 , M
         MFTN1 = MFTN1 + TOKN1( ITOK1 + I )**2 * RMSW( IDLN + I )
192      CONTINUE
         MFTN1 = 0.5 * P * MFTN1
         TOKN1(ITOK + IMFT ) = MFTN1
C--------------------------------------------------------------------
C--> �O  �AHH�M �PO�HO�A  HAXO��M  G(N+1) � �A��C�BAEM E�O B MSW.
C--> �O  �AHH�M �PO�HO�A  HAXO��M  J(N+1) � �A��C�BAEM E�O B MSW.
C--------------------------------------------------------------------
         DO 195 I = 1 , M
         Z1=1./( RMSW(ILN+I) + H1 * ( OMEGN1 * P * RMSW(IDLN+I) + R ) )
         RMSW( IGN1 + I ) = H1 * Z1
         RMSW(IJN1 + I)=(TOKN(ITOK1+I)+H2*RMSW(IJ0+I))*Z1*RMSW(ILN+I)
195      CONTINUE
C--------------------------------------------------------------------
C   G(N+1)  �  J(N+1)  O�PE�E�EH�....
C---  BK�A��BAEM ----------------------------------------------------
       DO 197 I = 1 , M
       NN = 8 + I
       NK = 8 + M + I
       IG = IGN1 + I
       IJ = IJN1 + I
       CALL DEPOST( MSW( NN ), MSW( NK ), RMSW( IG ),RMSW( IJ ) ,
     =              YALL  ,JALL )
197    CONTINUE
       RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C---- ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  �A�EH�E HA�P��EH��-->  HOBOE
       DO 201 I=1,KALL
       NN = MSW( I + 8 )
       NK = MSW( 8 + M + I )
       VN = 0.0
       IF ( NN .NE. 0 ) VN = VALLN1(NN)
       VK = 0.0
       IF ( NK .NE. 0 ) VK = VALLN1(NK)
       UL(I) = VN - VK
201    CONTINUE
C--------------------------------------------------------------------
C-->  HA��EM TOK  �O  VALLN1...
C--------------------------------------------------------------------
       DO 202 I=1,KALL
       TOKX(I) = RMSW( IJN1 + I ) + RMSW( IGN1 + I ) * UL( I )
202    CONTINUE
C--------------------------------------------------------------------
C--->  MFT  �  MBH ...
C  -->  "�BH�E" ��� �TEP. �HA�EH�� ��� TN1 ( �PE�CKA�AH�E) -->
       GAMN1 = TOKN1(ITOK + IGAM)
       OMEGN1 = TOKN1(ITOK + IOMEG)
C--------------------------------------------------------------------
C--->   �HA�EH��  GAMMA � OMEGA  B TN  TO�EK -->
C--------------------------------------------------------------------
       GAMN  = TOKN(ITOK+IGAM)
       OMEGN = TOKN(ITOK+IOMEG)
C--------------------------------------------------------------------
C     PAC�ET MFTX
C--------------------------------------------------------------------
         MFTX  = 0.0
         DO 203 I = 1 , M
         MFTX = MFTX + TOKX( I )**2 * RMSW( IDLN + I )
203      CONTINUE
         MFTX = 0.5 * P * MFTX
C--------------------------------------------------------------------
C  MBHX-->  C ��ETOM TOKX, GAMN1, OMEGN1
C--------------------------------------------------------------------
       TN1 = TN + H0
       MBHX = KMAS02( TN1 , RMSW( IMBHT ) )
C--------------------------------------------------------------------
       W0  = RMSW( D1 + 8 )
       W01 = ( MFTX - MBHX - SIGN(1.,OMEGN1) * MT ) / J
C--->  ####  B��O�HEH�E  HE�BHO�O  �A�A ( �TA� KOPPEK��� )::-->
       OMEGX = OMEGN + H1*W01 + H2*W0
       GAMX = GAMN + H1*OMEGN1 + H2*OMEGN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C-->  O�EHKA �O�PE�HOCT�...
       EPSX = 0.0
       NSM = 0
       DO 204 I=1,KALL
       X1 = ABS( TOKN1(ITOK1+I) )
       X2 = ABS( TOKX(I) )
C++++
       IF ( X1 .LE. 0.0010  .OR.  X2 .LE. 0.0010 )  GO TO 2040
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
       X1 = ABS( GAMN1 )
       X2 = ABS( GAMX )
C++++
       IF ( X1 .LE. 1.E-05  .OR.  X2 .LE. 1.E-05 )  GO TO 2050
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
       X1 = ABS( OMEGN1 )
       X2 = ABS( OMEGX )
C++++
       IF ( X1 .LE. 0.001   .OR.  X2 .LE. 0.001  )  GO TO 2060
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
9998     FORMAT( ' *** �O�. C�O� B DM0022 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
         END
