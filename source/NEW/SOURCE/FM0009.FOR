         SUBROUTINE FM0009( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EBPA�� 1989 �.)------------
C   �O��PO�PAMMA:  FM0009( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E��:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  10       "MOCT �AP�OHOBA"          TM
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E�� "MOCT �AP�OHOBA"
C   -----
C
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
C     �C�O����ETC�: FM0009, LM0011, DM0013;
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. PEA���A��� ��� KOD  = 0  , KOD  = 1
C                     �P� O���KAX --> �P�H�MAETC� KOD  = 0;
C                     �HA�EH�� �APAMETPOB ---->
C
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
C    50-55   37-42    MSWX[6] V6-> #�O�.��; KD  HX  MMCIU  HXCIU  ---
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
C
C   MMCIU  �  HXCIU O�PE�E��ET MOH�TOP MAKPOMO�E�� BEHT���......
C
C
C    BEHT��� OTKP�T ==>  KD=1;;;;BEHT��� �AKP�T ==>  KD=0;
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         EQUIVALENCE ( NUM, RNUM )
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 10 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0009: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1        CONTINUE
C---> O�H��EH�E BCEX �O�E� RMSW ��� BEHT���  (INTEGER)
         LM = 42
         NUM = 0
         DO 100 I=1,LM
100      RMSW(I) = RNUM
C===> �P�H�T� �CXO�. �APAMETP�:
C-->  KOTPO��
         KKM = PAR0T(1)
C-->  B�� ��PAB�EH��..
         PMAS = PAR0T(2)
         IF ( PMAS .EQ. 0.0 ) GO TO 2
         IF ( PMAS .EQ. 1.0 ) GO TO 3
C----------------------------------------------------------------------
         WRITE(NWTR,4) NEL, PMAS
4        FORMAT(' *** T�� ��PAB�EH�� "C��" ��� ��EMEHTA #', I5, ' ***',
     =          ' *** PABH�� ', F5.0,' O���O�EH  --> �P�H�MAETC�',
     =    ' PABH�M H��� ***')
         GO TO 2
C----------------------------------------------------------------------
2        CONTINUE
C-->  PMAS = 0   HE ��PAB. BEHT���.....
C MMCIU=11,  HXCIU=0.0
         NUM=11
         DO 201 I=1,6
         JMSW =  (I-1)*6 + 10
         RMSW(JMSW) = RNUM
         RMSW(JMSW+1) = 0.0
201      CONTINUE
         GO TO 5
C######################################################################
3        CONTINUE
C---> PMAS = 1 --> ECT� ��PAB�EH�E (HOMEP ��-TA)
         NUM=INT(PAR0T(3))
         RMSW(2) = RNUM
C--> �P��HAK ��PAB�EH��....
         NUM = 1
         RMSW(1) = RNUM
         GO TO 5
C----------------------------------------------------------------------
5         CONTINUE
          RETURN
          END
