         SUBROUTINE DM0013 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EBPA�� 1989 �.)------------
C   �O��PO�PAMMA:  DM0013 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:  MOH�TOP         MAKPOMO�E�� # T��A 10:
C  ------------  MOCT �AP�OHOBA
C                ************************************************
C                �PO��HAK T��A #6
C          CB��� C BHE�HE� CPE�O�:
C
C            �A�A "A"       �A�A "B"     �A�A  "C"
C              ! I             ! I          ! I
C              ! V             ! V          ! V
C   +--< V1 ]--*---------------!------------!----< V4 ]----+
C   !           <-- IV1        !            !   <-- IV4    !
C   !                          !            !              !
C   *--< V2 ]------------------*------------!----< V5 ]----*
C   !           <-- IV2                     !   <-- IV5    !
C   !                                       !              !
C   +--< V3 ]-------------------------------*----< V6 ]----*
C   !           <-- IV3                                    !
C   !                                            <-- IV6   !
C   !    ---> TOK_A                         ----> TOK_K    !
C   +---------*                                 *----------+
C     "AHO�"  +                                 - "KATO�"
C
C
C
C     KO�-BO ���OB --- 5
C
C     B�BO�H�E �HA�EH��- 17 �T. :
C      TOK �A��-A, TOK �A��-B, TOK �A��-C, TOK AHO�A, TOK KATO�A,
C      TOK� V1, V2, V3, V4, V5, V6,
C      C��� V1, V2, V3, V4, V5, V6;
C
C   KO��POBKA:
C
C   � # T 10 �  1 2 3 4 5
C               � MAS < KOD [#��] >;
C
C ��E:  KOD = 0 EC�� MOCT HE ��PAB��EM��, TO��A #�� MO�HO HE KO��POBAT�;
C
C       KOD = 1 EC�� MOCT ��PAB��EM��, TO��A --->
C                #�� - HOMEP ��EMEHTA �E��, �B�����MC� C��� ��� MOCTA
C                      B TOKN/N1 CO�EP�ATC�: MMCIU - 6 �T�K.
C                                            HXCIU - 6 �T�K.
C                B COOTBETCTB�� CO CXEMO� � H�MEPA��E� BEHT��E�
C
C          MOH�TOP B���BAET DM00A9 (CM.)
C          B��O�H�� �A�O�HEH�E MSWX
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
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. DM00A9
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=1, 10,20,3,4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST = 1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C              �OPMAT MSW # T��A 10 [ MOCT �AP�OHOBA  ]:
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
C  MSW       �ACT�        ��� BEHT��E�:
C     ����                  ����:
C  9 - �A�A "A"           V1 --> <09> - <12>
C 10 - �A�A "B"           V2 --> <10> - <12>
C 11 - �A�A "C"           V3 --> <11> - <12>
C 12 - "A"- AHO�          V4 --> <13> - <09>
C 13 - "K" - KATO�        V5 --> <13> - <10>
C                         V6 --> <13> - <11>
C
C    14       1   [FM]  1 �P��HAK ��PAB�EH�� --> 0 HET,
C                                                1 - ��PAB�EH�E �O C��
C    15       2   [FM]  NPB - #��., �A��E�O C��
C    16       3   [LM]  ANPB - A�PEC #��. B MSWALL
C    17       4   [LM]  AUYP - A�PEC B TOKN/N1 �HA�EH�� C��
C    18       5   [ ]    PE�EPB
C    19       6   [ ]    PE�EPB
C    20-25    7-12    MSWX[1] V1-> #�O�.��; KD  HX  MMCIU  HXCIU  ��. KD
C    26-31   13-18    MSWX[2] V2-> #�O�.��; KD  HX  MMCIU  HXCIU  --
C    32-37   19-24    MSWX[3] V3-> #�O�.��; KD  HX  MMCIU  HXCIU  --
C    38-43   25-30    MSWX[4] V4-> #�O�.��; KD  HX  MMCIU  HXCIU  --
C    44-49   31-36    MSWX[5] V5-> #�O�.��; KD  HX  MMCIU  HXCIU  --
C    50-55   37-42    MSWX[6] V6-> #�O�.��; KD  HX  MMCIU  HXCIU  --
C
C  LM<O����> = 55;      LM<�OKA��H��>=42
C
C
C
C               PA�O��� MACC�B T��A  MSWX T��A # A9 ( ��� T��. #10)
C             -------------------------------------
C
C
C  +-------+--------+--------+--------+------+-------+
C  I HOMEP I  "KD"  I "HX"   I MMCIU  I  HX  I ��A�  I
C  I �O� - I �P��-  I BPEM�  I KO� ��PI CIU  I       I
C  I ��EME I HAK CO-I �PE�-O�I �M���� I �M-CAI "KD"  I
C  I HTA   I CTO�H��I KOM-���I  CA    I      I       I
C  +-------+--------+--------+--------+------+-------+
C  I  1    I   2    I   3    I    4   I   5  I       I
C  +-------+--------+--------+--------+------+-------+
C   MMCIU  �  HXCIU O�PE�E��ET MOH�TOP MAKPOMO�E�� BEHT���......
C
C
C    BEHT��� OTKP�T ==>  KD=1;;;;BEHT��� �AKP�T ==>  KD=0;
C
C********************************************************************
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL
       DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(59), GVENT6 )
         EQUIVALENCE ( MASPRT(94), DTMCIU )
         EQUIVALENCE ( MASPRT(60), FTEST )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
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
C----------------------------------------------------------------------
       DIMENSION TOKNX(2), TOKN1X(2)
       INTEGER VK, VM
       REAL CVN,CVN1
       DIMENSION VK(6), VM(6), TVN(6), TVN1(6), CVN(6), CVN1(6)
       DATA VK / 9, 10, 11, 13, 13, 13/
       DATA VM / 12,12,12,9,10,11/
C----------------------------------------------------------------------
       KOD = 0
C--->  KOHTPO�� T��A
         IF ( MSW(2) .NE. 10 ) GO TO 9999
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
9998     FORMAT( ' *** �O�. C�O� B DM0013 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
C---------------Z A P R O S = 10---------------------------------------
C---------------Z A P R O S = 20---------------------------------------
C---------------Z A P R O S = 3----------------------------------------
C---------------Z A P R O S = 4----------------------------------------
C
100    CONTINUE
200    CONTINUE
300    CONTINUE
400    CONTINUE
500    CONTINUE
C--> �EPE� MOH�TOP, �O�CTAB��EM TO��KO �HA�EH��.....
        ITOK=MSW(7)
C--> ��E C�� � HXCIU B ���OM ��EMEHTE
        ITOKZ=MSW(17)
C-- O���� ��� BCEX------
         DO 101 IV=1,6
C--> HOMEPA ���OB
        K1 = VK(IV)
        M1 = VM(IV)
         K = MSW(K1)
         M = MSW(M1)
C-->A�P. B HA�EM MSW HA�A�A MSVX[IV]...
        IIMSW = (IV-1)*6 + 20
C- �P� ZAPROS=3, TO BCTAB�T� MMCIU � HXCIU
C*******IF ( ZAPROS .NE. 3 ) GO TO  1010
C--> ��PAB. BEHT��� ????? EC�� �A, TO H� XEPA HE�E�AT�.....
         IF ( MSW(14) .EQ. 0 ) GO TO 1010
C--> K��A BCTAB�T� CIU � HXCIU
         KKII = 23 + (IV-1)*6
C--> OTK��A B��T�...
         KKJJ = ITOKZ + (IV-1)
C--> �EPE��C�BAEM B MSWX......
         MSW(KKII) = IFIX( TOKN1(KKJJ) )
         RMSW(KKII+1) = TOKN1(KKJJ+6)
C--> �EPE��CAT� B MSWX �HA�EH�� TOKOB
1010     TOKNX(1)  = TOKN (ITOK + 5 + IV - 1 )
         TOKN1X(1) = TOKN1(ITOK + 5 + IV - 1 )
C        CONTINUE
         CALL DM00A9(K,M,MSW(IIMSW),RMSW(IIMSW),YALL,JALL,LOGYTR,
     *               TOKNX,TOKN1X,VALLN,VALLN1,KOD)
C
C        IF ( ZAPROS .EQ. 3 ) CVN1(IV) = TOKN1X(2)
         CVN1(IV) = TOKN1X(2)
C        IF ( ZAPROS .EQ. 20 ) TVN1(IV) = TOKN1X(1)
         TVN1(IV) = TOKN1X(1)
101      CONTINUE
C**********************
C--> ��� ZAPROS = 1, 10, 4 --> �TO BCE......
C@@@@ @ IF ( ZAPROS .EQ. 1 ) RETURN
C@@@@ @@IF ( ZAPROS .EQ. 10 ) RETURN
C@@@@ @@IF ( ZAPROS .EQ. 4 ) RETURN
C-- PA��EPEMC� C ZAPROS=20 � 3....
C@@@@ @@ IF ( ZAPROS .NE. 3 ) GO TO 1011
C--> BCTAB�M CIU...
         DO 103 IV=1,6
         IICIU = ITOK + 11 + (IV-1)
103      TOKN1(IICIU) =CVN1(IV)
C--> ��� ZAPROS=20 �TO BCE...
C@@@@@@  RETURN
1011     CONTINUE
C  ZAPROS=3
C---->PAC��TAT� � PACCTAB�T� B�B. �HA�EH��.....
         DO 102 IV=1,6
C-->   A�PEC HA��X TOKN/TOKN1
         IITOK = ITOK + 5 + (IV-1)
         TOKN1(IITOK) = TVN1(IV)
102      CONTINUE
C---> B���C��M BHE�H�E TOK�......
C   ��� BPEMEH� TN1...
C
C.....�A�A "A"
        TOKN1(ITOK)=TVN1(1) - TVN1(4)
C.....�A�A "B"
        TOKN1(ITOK+1) = TVN1(2) - TVN1(5)
C.....�A�A "C"
        TOKN1(ITOK+2) = TVN1(3) - TVN1(6)
C...."AHO�"
        TOKN1(ITOK+3) = TVN1(1) + TVN1(2) + TVN1(3)
C...."KATO�"
        TOKN1(ITOK+4) = TVN1(4) + TVN1(5) + TVN1(6)
555       RETURN
          END
