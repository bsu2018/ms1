         SUBROUTINE DM0039 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (AT OCTOBER 1989 �.)------------
C   �O��PO�PAMMA:  DM0039 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    MOH�TOP MAKPOMO�E�� ��E�OKOM�EHCATOPA    (T�� 800)
C   -----------     ( FM0035, LM0014 )
C
C                  �E�� C PAC�PE�E�EHH�M� �APAMETPAM�
C                  �PECTAB��ETC� B C�TET��ECKO� CXEME
C
C@
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
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
C   METO�:
C   -----
C
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
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;SUPPL1, SUPPL2
C   ------------
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=  1, 2, 3, 4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST >=1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C
C
C                    "�OCTO�HHA�" �ACT� MSW
C                     ----------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I�HA�.  I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I        I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW T��A  # 800
C                  --------------------
C
C
C  �OCT.  I �EPEMEHHA� RMSW-->
C +----------+----+---+----+---+----+---+------+------+---+---+---+...>
C I ����I    I    I   I RI I   I    I   I      I      I   I   I   I...>
C I     I G1 I #��I#CMI    I GLI GC I KOI  KJP I G(20)IG11IG12IG22I...>
C I     I    I    I   I    I   I    I   I      I      I   I   I   I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I   3 I 1  I 1  I 1 I  1 I 1 I  1 I 1 I  1   I  20  I 1 I 1 I 1 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I 9-11I 12 I 13 I 14I 15 I16 I 17 I18 I 19   I 20-39I 40I41 I42 I...>
C +-----+----+----+---+----+---+----+---+------+------+---+---+---+...>
C I     I 1  I 2  I 3 I 4  I 5 I 6  I 7 I 8    I 9-28 I29 I30 I 31I
C
C
C
C ...>+---------+--------+------+
C ...>I G MAT-  I PE�EPB I      I
C ...>I   P��A  I        I ATOKMI
C ...>I BC�OMO�.I        I      I
C ...>+---------+--------+------+
C ...>I   16    I   1    I   1  I
C ...>+---------+--------+------+
C ...>I 43 - 58 I  59    I  60  I
C ...>+---------+--------+------+
C     I 32 - 47 I  48    I  49  I
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 60
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 49
C
C   BH�MAH�E           ��C�O ��E�O-�A�� CTPO�O PABHO  198
C   ----------
C               ��� KA��O� ��E�O-�A���  ��C�O �BEH�EB PABHO 1 (M)
C
C   ��OK RIEZO : TNL - TOK� B �H��KT�BHOCT�X   ( KH )
C                TNC - TOK� B EMKOCT�X          (KH )
C                UN0 - CTAPOE HA�P��EH�E       ( M/C)
C                UN1 - HOBOE  HA�P��EH�E       ( M/C)
C                TJNL - �CTO�H�K� TOKA B �H��KT�BHOCT�  ( KH)
C                TJNC - �CTO�H�K� TOKA B EMKOCT�        ( KH)
C                D11  - ��A�OHA��H�E ��EMEHT� MATP��.PA��O�EH��
C                            XO�E�KO�O
C                D21  - BHE-��A�OHA��H�E ��EMEHT� PA�. XO�E�KO�O
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
C----------------------------------------------------------------------
       COMMON /PIEZO/ TNL(200),TNC(200),UN0(200),UN1(200),
     * TJNL(200),TJNC(200),D11(200),D21(200)
C----------------------------------------------------------------------
C!!!!!!!!!!!!!!!!!!#########!!!!!!!!!!!!!!!!!--------------------------
         DIMENSION  G(20), TJN1(200), TJN2(200)
C----------------------------------------------------------------------
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER,  ZAPROS, TIP
         INTEGER ADRMSW
         REAL   KJP ,KO ,LI
C####################################################################
C      H0 = 0.13889E-5
C####################################################################
       TIP = MSW(2)
       IF ( TIP .NE. 800 ) GO TO 9999
C--------------------------------------------------------------------
       G1 = RMSW(12)
       GL = RMSW(16)
       GC = RMSW(17)
C      WRITE (6,*) G1,GL,GC
C--------------------------------------------------------------------
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
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  �APAMETP� CC
      RI = RMSW(15)
      KO = RMSW(18)
      KJP = RMSW(19)
      G11 = RMSW(40)
      G12 = RMSW(41)
      G22 = RMSW(42)
C--> KAKOE BXO�HOE HA�P��EH�E ?
      UBX = TOKN1(MSW(60))
CCC   WRITE (6,*) G11,G12,G22
      DO 10 I = 1,20
      G(I) = RMSW(19 + I)
10    CONTINUE
C--------------------------------------------------------------------
C--> �CTO�H�K� TOKA B �H��KT�BHOCT� � EMKOCT�
      DO 20 I =1,198
      X1 =  G1*(UN1(I) - UN1(I+1))
      X2 =  UN0(I) + UN1(I) - UN0(I+1) - UN1(I+1)
      TJNL(I) = TNL(I) + X1 + KO*UBX + X2*KJP*H0/2.
      TJNC(I) = TNC(I) + GC*UN1(I+1)
20    CONTINUE
CCC   WRITE(6,*) TJNL(198),TJNC(198)
C--> �CTO�H�K� TOKA B �-O�PA�HOM �BEHE
      TJN1(1) = - TJNL(1)
      TJN2(1) = TJNC(1)/2. + TJNL(1)
      DO 30 I =2,198
      TJN1(I) = TJNC(I-1)/2. - TJNL(I)
      TJN2(I) = TJNC(I)/2. + TJNL(I)
30    CONTINUE
      TJN2(198) = TJNC(198)/2. + TJN2(198)
C--------------------------------------------------------------------
C--> �P���P�EM J �O CTE�EH�M �BO�K�
      CALL SUPPL1(7 ,TJN1(1),TJN2(1),TJ1,TJ2,G )
      CALL SUPPL1(6 ,TJN1(129),TJN2(129),TJ3,TJ4,G )
      CALL SUPPL1(2 ,TJN1(193),TJN2(193),TJ5,TJ6,G )
      CALL SUPPL1(1 ,TJN1(197),TJN2(197),TJ7,TJ8,G )
C--> CK�A��BAEM ...
      CALL SUPPL2(TJ1,TJ2,TJ3,TJ4,RMSW(43),RMSW(44) )
      CALL SUPPL2(TJ1,TJ2,TJ5,TJ6,RMSW(45),RMSW(46) )
      CALL SUPPL2(TJ1,TJ2,TJ7,TJ8,RMSW(47),RMSW(48) )
CCC   WRITE (6,*) TJ1,TJ2
C----------------------------------------------------------------------
C  BK�A��BAEM B O���� MATP��� G-J
C----------------------------------------------------------------------
      KOD = 0
      G12 = - G12
      G11 = G11 + G12
      G22 = G22 + G12
      CALL DEPOST (MSW(9),MSW(10), G12,0.,YALL,JALL)
      CALL DEPOST (0 ,MSW(10), G22,TJ2,YALL,JALL)
      CALL DEPOST (0 ,MSW(9), G11,TJ1,YALL,JALL)
        RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C---- ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C--> BXO�HOE � B�XO�HOE HA�P��EH�� C�HTET��ECKO� CXEM�
       KN = MSW(9)
       KK = MSW(10)
       VN = VALLN1(KN)
       VK = VALLN1(KK)
C--> O�HOB��EM BH�TPEH�E HA�P��EH��
       DO 210 I = 1,199
       UN0(I) = UN1(I)
210     CONTINUE
C----------------------------------------------------------------------
C  ��EM HOB�E BH�TPEH�E HA�P��EH�� CC
C----------------------------------------------------------------------
       UN1(1) = VN
       UN1(199) = VK
C--> HA��EM J
       DO 220 I =2,198
       TJN1(I) = TJNL(I-1) + TJNC(I-1) - TJNL(I)
220     CONTINUE
       TJN1(2) = TJN1(2) + GL*VN
       TJN1(198) = TJN1(198) + GL*VK
C--> �C�O����ETC� PA��O�EH�E XO�E�KO�O
C--> D*TJN2 = J
       TJN2(2) = TJN1(2)/D11(2)
       DO 230 I =3,198
       TJN2(I) = (TJN1(I) - D21(I-1)*TJN2(I-1))/D11(I)
230     CONTINUE
C--> DT*UN1 = TJN2
       UN1(198) = TJN2(198)/ D11(198)
       DO 240 I =1,196
       UN1(198-I) = (TJN2(198-I)-D21(198-I)*UN1(199-I))/D11(198-I)
240     CONTINUE
C--> HOB�E BH�TPEH�E TOK�
       DO 250 I =1,198
       TNL(I) = G1*(UN1(I)-UN1(I+1)+UN0(I)-UN0(I+1))+ TNL(I)
       TNC(I) = GC*(UN1(I+1) - UN0(I+1))- TNC(I)
250     CONTINUE
       TOKN1(MSW(7)) = VK
       KOD = 0
       RETURN
C######################################################################
C######################################################################
9999     CONTINUE
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0039 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
C######################################################################
C        DEBUG INIT
         END
