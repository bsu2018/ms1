         SUBROUTINE DM0019 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0019 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:  MOH�TOP         MAKPOMO�E�� # T��A 92:
C  ------------  BEHT���H�� K��� ( �BA BEHT��� 1- OT "K" K "M" );
C                ************************************************
C                �PO��HAK T��A #6
C
C                   +----I<I-----+   *�EPB�� BEHT���_KATO�HA� �P���A*
C                   I            I
C  *K_��E�*---------+            +------*M_��E�*
C                   I            I
C                   +----I>I-----+   *BTOPO� BEHT���_AHO�HA� �P���A*
C
C
C     HA�P��EH�E O�OPHOE ��� 1-�O BEHT����� -->   V(�O1);
C     HA�P��EH�E O�OPHOE ��� 2-�O BEHT����� -->   V(�O2);
C     HA�P��EH�E ��PAB�EH�� ��� 1-�O BEHT��� --> V(��1) - V(��2)
C     HA�P��EH�E ��PAB�EH�� ��� 2-�O BEHT��� --> V(��2) - V(��1)
C     ( T.E. TO �TO ��� BEHT��� 1, HO C O�PATH�M �HAKOM );
C
C
C       PEA����ETC� "PA��E��HOE" ��PAB�EH�E BEHT���M�:
C                    �P� �O�ACAH�� O�HO�O -> �A�PET �O�A�� HA HE�O
C                                             �M����COB "C��" �
C                                             PA�PE�EH�E �O�A�� "C��"
C                                             HA �P��O�;
C                    �P� �O�ACAH�� O�O�X -> PA�PE�EH�E "C��" HA TOT,
C                                           ��� KOTOPO�O HA�P��EH�E
C                                           ��PAB�EH�� COOTB. H��HO�
C                                           �P���E �POBO��MOCT�;
C
C              ��EMEHT O��C�BAETC�:
C
C   � # T 92 � K  M  �  �O1  �O2   ��1  ��2  DTCIU  EYYV   EYYI;
C
C     ��E:     K, M  -  ���� �O�K���EH��,
C
C              �O1, �O2  -  ���� "O�OPHO�O" HA�P��EH��-> V(�01), V(�O2)
C
C              ��1, ��2  -  ���� "��PAB����E�O" HA�-� -> [(��1)-V(��2)]
C
C              DTCIU - ���TE��HOCT� �M����CA "C��" -> [ B ��. �PA��CAX ]
C                      �M����C� �EHEP�P��TC� CO��ACHO CPABHEH��
C                      HA�P. O�OPHO�O -> B���PAEMO�O �� MACC�BA
C                      VALLN/VALLN1 �  HA�P��EH�� "��PAB����E�O" ->
C                      B���PAEMO�O �� VALLN/VALLN1;
C
C  EYYV - MAX. �O�. AM���T��A C�� �P� B��P�M�TE��HM PE��ME PA�OT�;     �
C  EYYI - MAX. �O�. AM���T��A C�� �P� �HBEPTOPHOM   PE��ME PA�OT�;
C
C
C   !!! C��HA� "C��" �EHEP�P�ETC� B MOMEHT �EPECE�EH�� KP�BO�
C       ��PAB����E�O HA�P��EH�� BEHT��� C KP�BO� O�OPHO�O
C       HA�P��EH�� - H�C�A�A���� ��ACTOK.
C   !!!
C   !!!  ��� "ECTECTBEHHO�" PA�OT� K���A_2 B �A�E �E�� PEKOMEH��ETC�:
C
C        U_O�_1<KATO�> := - E_�A��;
C
C        U_O�_2<AHO�>  := + E_�A��;
C   !!!
C
C
C === B�BO�H�E �HA�EH��: I1         - TOK  BHE�. B�BO�OB, OT "K" K "M";
C                        I2, I3     - TOK� BEHT��E�,
C                        I3, I4     - "C��" HA BEHT���X;
C
C######################################################################
C    PAC�PE�E�EH�E HA�P��EH�� ��PAB�EH�� �O         AHO�HO-KATO�H�M
C                      BEHT���M
C
C                 EYY:=<HA�P��EH�E ��PAB�EH��>:= [V(��1) - V(��2)]
C                      --------------------------------------------
C
C    "KATO�H��" BEHT���                      "AHO�H��" BEHT���
C
C   BEHT. 01         -->  "-" EYY       BEHT. 02         --> "+" EYY
C
C                      O�OPH�E HA�P��EH��:
C
C   BEHT. 01   -->  V(�O1)              BEHT. 02          --> V(�O2)
C
C######################################################################
C-->  UYN/N1  - HA�P. ��PAB�EH��
C
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                 -  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE�
C                   �HA�EH�� "�POBO��MOCT�" �AKP�T�X BEHT��E� ( GVENT6 )
C                   [ B �AHHO� BEPC�� KOM��EKCA ];
C                  - �OPM�POBAH�E BEKTOPA K.�. ���OB "LOGYTR"
C                    ��� �H���EHTH�X ���OB --> �C�O����ETC�
C                     �/�  GETLOG --> OC��ECTB�T� "��EMEHTAPH��" BK�A�;
C
C                  1A) ZAPROS = 10
C                      ------------
C                      �OPM�POBAH�E "TEHEBO�" C�CTEM� �PABHEH��
C                       ( �O��EMEHTH�� BK�A� )
C
C                  B) ZAPROS = 20
C                     ------------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�. �P� �TOM TOK OTKP�T�X BEHT��E�:(���� K � M )
C                  <TOKN1>:= <VALLN1[K]> - <VALLN1[M]>;
C                   TOK �AKP�T�X BEHT��E� = 0.0;
C                  �A��C� �T�X TOKOB B MACC�B TOKN1;
C                   !!! B��O�H�T� �O MO����KA��� PE�EH�� VALLN1 !!!
C
C                  C) ZAPROS = 3
C                     -----------
C                      �O O�PE�E�EHH�M TOKAM BEHT���H�X "��E�"
C                      �OPM�P�ET C��HA�� "PA�PE�EH�E/�A�PET" CX � TX
C                      �CXO�� �� HA�P��EH�� ��PAB�EH�� � O�OPH�X
C                      HA�P��EH�� ( ���� KA, KB, KC ) -->
C                      O�PE�E��ET MOMEHT� "ECTECTBEHHO�" KOMM�TA���
C                      BEHT��E� ( CM. �/� MCIU50 ); HA OCHOBE �TO�O
C                     �O��BO��TC� AHA��� HA BO�MO�H�� CMEH� COCTO�H��
C                     BEHT��� [ OTKP�T <==> �AKP�T ]:
C                     �P� �TOM: FKOM = 0 - COCTO�H�E HE ��MEH��OC�;
C                               FKOM = 1 - HA TEK��EM �A�E COCTO�H�E
C                                         �O��HO "���O" ��MEH�T�C� B
C                                         B T = TX MOMEHT BPEMEH�;
C                                         PEA��HO B �PO�PAMME O�PE�E-
C                                         ��ETC� HX - �A� BO�MO�HO�
C                                         KOMM�TA���, �ATEM TX=TN+HX;
C                                         �P� HX < HXMIN [MASPRT(62)]
C                                         �P�H�MAETC� HX = HXMIN;
C                                         COCTO�H�E HX=0.0 B���BAET
C                                         �E�AT� COO��EH��;
C                                         �PO��BE�EH� COTBETCTB����E
C                                         ��MEHEH�� B MASPRT:
C                                          FKOM <== "1"
C                                         HXG = MIN( HX, HXG )
C                                         TXG = TN + HXG
C
C              ��ET: ICLOSE - MO���� TOKA "�AKP�T��" BEHT���;
C                    UOPEN  - MO���� HA�P��EH�� "OTKP�T��" BEHT���;
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX_BEHT���   -
C                      ��KC�P�ETC� CMEHA COCTO�H�� BEHT��� �APAMETPOM
C                      KD;
C                     PABEHCTBO �POBEP�ETC� C TO�. �O "EPSKOM".
C
C   METO�:         PA�HOB��HOCT� BO�HOBO�O A��OP�TMA ��� �OPM�POBAH��
C   -----          MACC�BA LOGYTR --> �E�E� �/� GETLOG;
C
C                  "TEHEBA�" C�CTEMA �OPM�P�ETC� "MAC�TA��POBAHHA�"
C                   TAK, �TO: V[X] - V[Y] = <TOK    X-Y   BETB�>;
C                   ����, OTME�EHH�E B LOGYTR(*) = 0 - "�A�OB�E"
C                   ��� KA��O� �H���EHTHO� �P���� OTKP�T�X BEHT��E�;
C
C   �APAMETP�:     MSW, RMSW - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------      ( "OTPE�OK" MACC�B� MSWALL -> C.M. �/� GETMSW );
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  T  =  TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  TN + H0;
C                  TOKN  � TOKN1 - �HA�EH�E �EPEMEHH�X "COCTO�H��"
C                                  MO�E�� �P� TN  �  TN + H0;
C                          ( PA�MEPHOCT� -> CM. MASPRT(46) = NNTOK )
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 6;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. DM00B9, SEQCIU, SEQMAX
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=1, 10,20,3,4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST = 1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C              �OPMAT MSW # T��A 92 [ BEHT���H�� K��� ]:
C              -------------------------------------------
C               "�OCTO�HHA�" �ACT�
C               ******************
C
C  +-------+--------+--------+--------+------+------+--------+----+
C  I HOMEP I HOMEP  I HOMEP  I  �P��. I KO�� I KO�. I A�P.   I PE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I �HA�.  I �E I
C  I HTA   I MO�E�� I PAC�ETAI (PE�.) I BO   I �HA� I "TOKN" I PB I
C  I       I        I DM.....I       .I ���OBI EH��.I "TOKN1"I    I
C  +-------+--------+--------+--------+------+------+--------+----+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7    I 8  I
C  +-------+--------+--------+--------+------+------+--------+----+
C
C
C              �EPEMEHHA� �ACT� MSW:
C              *********************
C
C  C�OBO     C�OBO     O�O�HA�EH� � �O���ECKOE �HA�EH�E
C  OT        OT
C  HA�A�A    �EPEMEH.
C  MSW       �ACT�
C
C  9          -        HOMEP ���A : 1 - BEHT��� --I>I--, 2 -� --I<I--
C  10         -        HOMEP ���A : 1 - BEHT��� --I<I--, 2 -� --I>I--
C
C-- -- - - - - --- - - - - - - - -
C  11, 12     1, 2     [F] HOMEPA ���OB �O1, �O2;
C
C  13, 14     3, 4     [F] HOMEPA ���OB ��1, ��2;
C
C  15          5       [F] DTCIU - ���T. �M����CA "C��"
C
C 16<->25     6<->15      MSWV1  -  C�OBO ��PAB�EH�� 1-M BEHT��EM
C      $$                 ( B 25 --->  EYYV )
C 26<->35    16<->25      MSWV2  -  C�OBO ��PAB�EH�� 2-M BEHT��EM
C      $$                 ( B 35 --->  EYYI )
C**********************************************************************
C
C   LM O��A� = 35  C�OB, LM_�OK. =  25
C
C       �OPMAT MSWV_EHT��� ��� DM00B9:
C       -------------------------------
C
C      C�OBO                   �HA�EH�E
C
C      1       KD - KO� COCTO�H��: =0 - �AKP�T, =1 - OTKP�T;
C      2       HX - �A� OT TN, BO�MO�HOE BPEM� �EPEK���EH��;
C
C ( B [*] HOMEPA MSW_VEHT��� ��� �/� "MCIU50" )
C
C  [1] 3       TS - BPEM� CTAPTA �M����CA( "E" KOMM�TA��� );
C  [2] 4       TK - BPEM� ��H��A �M����CA( "E" KOMM�TA��� );
C  [3] 5       CX - KO� PA�PE�EH��/�A�PETA "C��":
C                   =0 - C�� �A�PE�EHO �O�ABAT� HA BEHT���,
C                   =1 - C�� PA�PE�EHO �O�ABAT� HA BEHT���;
C [4] 6        TX - BPEM�, C KOTOPO�O �E�CTB�ET KO� "CX";
C
C
C [5] 7        MCIU - KO� �M����CA --->
C                     MCIU   = 00  --> "C��" HET B [TN, TN+H0];
C                                       HXCIU = 0.0;
C                     MCIU   = 01  --> B [TN, TN+H0] -> �EPE�H�� �POHT
C                                   �M����CA "C��", HXCIU = �HTEPBA� OT
C                                      TN �O �EPE�HE�O �POHTA "C��";
C                     MCIU   = 10 -->  B [TN, TN+H0] -> �A�H�� �POHT
C                                   �M����CA "C��", HXCIU = �HTEPBA�
C                                      OT TN �O �A�HE�O �POHTA �M����CA;
C                     MCIU   = 11 -->  B [TN, TN+H0] "C��" BK���EH,
C                                      COCTO�H�E HE MEH��, HXCIU=0.0;
C
C
C [6] 8        HXCIU - �A� OT TN, �O O�EPE�HO�O �POHTA "C��";
C (---)
C     9              - ��A� KD COCTO�H��;
C    10              - B 1-M --> EYYV,  BO 2-M --> EYYI
C
C********************************************************************
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADRMSW
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(59), GVENT6 )
         EQUIVALENCE ( MASPRT(94), DTMCIU )
         EQUIVALENCE ( MASPRT(60), FTEST )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
         INTEGER FTEST, OPEN6
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FKOM,   MASPRT(34) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( HXG,    MASPRT(61) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FKOM, ZAPROS
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
C----------------------------------------------------------------------
       DIMENSION TOKXN(2), TOKXN1(2),  THELP( 2), VHN(4), VHN1(4)
       DIMENSION KVENT( 2), MVENT( 2), UYN( 2), UYN1( 2),
     =           UON( 2), UON1( 2)
C
       INTEGER       CXA, CXK
       REAL                          IKN, IKN1, IAN, IAN1
C -->  AHO�H�E ���� ( HA�A�O BEHT��� ):
       DATA KVENT
     = /10,09/
C--> KATO�H�E ���� ( KOH�� BEHT��E� ):
       DATA MVENT
     = /09,10/
C
C----------------------------------------------------------------------
       KOD = 0
C--->  KOHTPO�� T��A
         IF ( MSW(2) .NE. 92 ) GO TO 9999
C
       TN1 = TN + H0
C---> B��OP PE��MA:
         IF ( ZAPROS .EQ. 1 )   GO TO 100
         IF ( ZAPROS .EQ. 10 )  GO TO 200
         IF ( ZAPROS .EQ. 20 )  GO TO 300
         IF ( ZAPROS .EQ. 3 )   GO TO 400
         IF ( ZAPROS .EQ. 4 )   GO TO 500
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0019 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
C
100    CONTINUE
101    CONTINUE
C-> ZAPROS = 1 -->                  -->  �OC�E�. B��OB MOH�TOPA BEH��E�
C-->  B COOTBETCTB�� C MACC�BAM� RMSW<50>, UYN, UYN1, UON, UON1
C
666    CONTINUE
       NEL = MSW(1)
       DO 102 NV = 1,2
C-->  A�PEC RMSW<50> BEHT���-->
       INV = (NV-1) * 10 + 16
C-->  ���� �O�K���EH��-->
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
C-->  HOMEP �O�. BEHT���
       NPEL = NEL*10000+NV
C-->  MOH�TOPHOE O�C����BAH�E-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
102    CONTINUE
C**********************************************************************
       RETURN
C**********************************************************************
C**********************************************************************
C**********************************************************************
200    CONTINUE
201    CONTINUE
C-->  ZAPROS=10-> TO �TO � ��� ZAPROS=1
       GO TO 666
C######################################################################
C######################################################################
C######################################################################
300    CONTINUE
C-->  ZAPROS=20 -->  TOK� BEHT��E�          O�PE�E��EM...>
C**********************************************************************
301    CONTINUE
C--> ZAPROS = 20  -> TOK� BEHT��E� B TOKN/N1-->
C      ITOKNV --> (ITOK-1) + 1 + NV = ITOK+0+NV
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       ITOK = MSW(7)
       NEL = MSW(1)
       DO 302 NV = 1,2
C-->  A�PEC RMSW<50> BEHT���-->
       INV = (NV-1) * 10 + 16
C-->  ���� �O�K���EH��-->
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
C-->  HOMEP �O�. BEHT���
       NPEL = NEL*10000+NV
C-->  MOH�TOPHOE O�C����BAH�E-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
C-->  PACTAB�T� O�PE�. �HA�EH�� TOKOB-->
       ITOKNV = ITOK     + NV
       TOKN1(ITOKNV) = TOKXN1(1)
       THELP(NV) = TOKXN1(1)
302    CONTINUE
C-->  "�TO�OB�E" TOK�-->
C-->  TOK OT  "K"  K  "M" -->
C
       TOKN1( ITOK ) = THELP(2) - THELP(1)
C
C-->  TOK� ��E, A "C��" E�E HE O�PE�E�EH�....
       RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
400    CONTINUE
C-->  ZAPROS=3  ->  AHA��� BO�MO�HO�O COCTO�H��/�EPEK���EH��
401    CONTINUE
C-->  ZAPROS=3 -  TOK� O�PE�E�EH�, HA��EM AHO�H�E � KATO�H�E TOK� B
C                 �A�AX BEHT��E� ��� B��CHEH�� CX, TX
C
C**********************************************************************
C----------------------------------------------------------------------
C
C-->  UON/N1  - O�OPH�E HA�P.
C-->  UYN/N1  - HA�P. ��PAB�EH��
C
C  VHN / N1 - "�CT�HH�E" HA�P��EH�� HA ���AX
C              �O1, �O2, ��1, ��2        B TN/TN+H0
C
C######################################################################
C    PAC�PE�E�EH�E HA�P��EH�� ��PAB�EH�� �O         AHO�HO-KATO�H�M
C                      BEHT���M
C
C                 EYY:=<HA�P��EH�E ��PAB�EH��>:= [V(��1) - V(��2)]
C                      --------------------------------------------
C
C    "KATO�H��" BEHT���                      "AHO�H��" BEHT���
C
C   BEHT. 01    -->  "-" EYY            BEHT. 02         --> "+" EYY
C
C                      O�OPH�E HA�P��EH��:
C
C   BEHT. 01   -->  V(�O1)              BEHT. 02          --> V(�O2)
C
C######################################################################
       DO 403 I=1,4
C-->  HOMEP ���A:
       K = MSW(I+10)
       VKN = 0.0
       VKN1 = 0.0
       IF ( K .EQ. 0 ) GO TO 404
       VKN  =  VALLN( K )
       VKN1 = VALLN1( K )
404    CONTINUE
       VHN(I)   = VKN
       VHN1(I)  = VKN1
403    CONTINUE
C-->  HA�P��EH�� ��PAB�EH��*******
C******* TN BPEM� *****************************************************
C   K A T O � H A �   � P � � � A:
       UYN(01) = - ( VHN(3) - VHN(4) )
C---            !!!!
C   A H O � H A �   � P � � � A:
       UYN(02) = + ( VHN(3) - VHN(4) )
C--
C----------------------------------------------------------------------
C******* TN + H0 BPEM� *************************************************
C   K A T O � H A �   � P � � � A:
       UYN1(01) =  - ( VHN1(3) - VHN1(4)  )
C---              !!!!
C   A H O � H A �   � P � � � A:
       UYN1(02) = + ( VHN1(3) - VHN1(4) )
C--
C----------------------------------------------------------------------
C***** O � O P H � E   H A � P � � E H � �:
C********TN BPEM�: ****************************************************
C-->
       UON(01) = VHN(1)
       UON(02) = VHN(2)
C----------------------------------------------------------------------
C********TN+H0  BPEM�: *************************************************
C-->
       UON1(01) =  VHN1(1)
       UON1(02) =  VHN1(2)
C-->
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --->  ��� KA��O�O BEHT��� CBOE CX < AHO�HOE>, CX <KATO�HOE> � TX:
C
       ITOK = MSW(7)
       IKN     = TOKN (ITOK + 01)
       IKN1    = TOKN1(ITOK + 01)
       IAN     = TOKN (ITOK + 02)
       IAN1    = TOKN1(ITOK + 02)
C----------------------------------------------------------------------
C--> HA�P��EH�E ��PAB�EH�E B TN � B   TN+H0 -->
       EYYN1 = VHN1( 3) -  VHN1( 4 )
       EYYN  = VHN ( 3) -  VHN ( 4 )
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  O�PAH��EH�E.....
       EYYV = RMSW(25)
       EYYI = RMSW(35)
       CALL SEQMAX( EYYN, IKN, IAN,  EYYV, EYYI )
       CALL SEQMAX( EYYN1, IKN1, IAN1,  EYYV, EYYI )
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C----------------------------------------------------------------------
       INVK = (1-1)*10 + 16
       INVA = (2-1)*10 + 16
       CXK = MSW(INVK+4)
       CXA = MSW(INVA+4)
C----------------------------------------------------------------------
       CALL SEQCIU( TXK, TXA, CXK, CXA, IKN, IKN1, IAN, IAN1,
     =  IFXK, IFXA,    EYYN, EYYN1 )
C
C$$$$$$GO TO 440
C
C-->  CMEHA CX � TX B MSW<50>  1     -> KATO�,  2    -> AHO�  :
C
       NV = 1
C-->  A�P. HA�A�A MSW<50>   KATO�H�� BEHT���
       INV = ( NV-1) * 10 + 16
       MSW(INV+4) = CXK
       IF ( IFXK  .EQ. 1 )    RMSW(INV+5) = TXK
C
       NV = 2
C-->  A�P. HA�A�A MSW<50>  AHO�H�� BEHT���
       INV = ( NV-1) * 10 + 16
       MSW(INV+4) =  CXA
       IF ( IFXA  .EQ. 1 )    RMSW(INV+5) = TXA
C----------------------------------------------------------------------
450    CONTINUE
C######################################################################
C######################################################################
C-->  B��O�H�M ZAPROS=3  ��� BEHT�E�***********************************
       NEL = MSW(1)
       ITOK = MSW(7)
       DTCIU = RMSW(15)
       DO 451 NV = 1,2
       INV = (NV-1) * 10 + 16
       K1 = KVENT(NV)
       M1 = MVENT(NV)
       K = MSW(K1)
       M = MSW(M1)
       NPEL = NEL*10000 + NV
C-->  PACTAB�M TOKXN/N1
       ITOKNV = ITOK  +  NV
       TOKXN(1) =  TOKN ( ITOKNV )
       TOKXN1(1) = TOKN1( ITOKNV )
C-->  BCE O�PE�E�EHO...
C-->  MOH�TOPHOE O�C����BAH�E-->
       CALL DM00B9(K, M, NPEL, MSW(INV), RMSW(INV), YALL, JALL, LOGYTR,
     =            TOKXN, TOKXN1, VALLN, VALLN1, KOD, UON(NV), UON1(NV),
     =            UYN(NV), UYN1(NV), DTCIU )
C- - - - - -
C-->  �P�MEM C��HA�� "C��"-->
       ICIUNV = ITOK + 2 +  NV
       TOKN1(ICIUNV) = TOKXN1(2)
451    CONTINUE
C-->  �A�POC� ...�
       RETURN
C----------------------------------------------------------------------
C######################################################################
500    CONTINUE
C-->  ZAPROS=4
C
C-->  ��� BEHT��E�  -->  AHA�O���HO ZAPROS=1-->
       GO TO 666
C
       END
