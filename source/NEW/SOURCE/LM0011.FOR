       SUBROUTINE LM0011( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EBPA�� 1989 �.)------------
C   �O��PO�PAMMA: LM0011( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�� T��A # 10
C   ----------     *** MOCT �AP�OHOBA ***
C
C
C              MCIU - KO� �M����CA --->
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
C
C                  ��EMEHT MO�ET ��T� �O���ECK� CB��AH C MAKPOMO�E��M�:
C                       T�� # 301 ( C��� )
C
C
C   METO�:        �O�CK B MSW_ALL O��CAH�� ��EMEHTA *C���
C   ------        B C���AE HAXO��EH�� -> �O�OPM�POBAH�E MSW_O��CAH��;
C                 B �TOM C���AE KOD = 0, �HA�E = 1;
C   �APAMETP�:    MSW(RMSW) - "OTPE�OK" MSW_MO�E�� T�� # 301;
C   ----------    ADR - A�PEC B MSW_ALL MSW_O��CAH�� �TO� MO�E��;
C                 MSWALL(RMSWAL) - BEC� C��COK MSW C�OB;
C                 KOD - = 0 - C�OPM�POBA�� MSW,
C                       = 1 - �O�. O���K�;
C                      ( HA NWTR COO��EH�E B�BO��TC� );
C   �O��PO�PAMM�: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C   �P�ME�AH�E:  �OPMAT� MSW_O��CAH�� ---->
C   ----------
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
C    20-25    7-12    MSWX[1] V1-> #�O�.��; KD  HX  MMCIU  HXCIU  ��. K�
C    26-31   13-18    MSWX[2] V2-> #�O�.��; KD  HX  MMCIU  HXCIU  ��. K�
C    32-37   19-24    MSWX[3] V3-> #�O�.��; KD  HX  MMCIU  HXCIU  ��. K�
C    38-43   25-30    MSWX[4] V4-> #�O�.��; KD  HX  MMCIU  HXCIU  ��. K�
C    44-49   31-36    MSWX[5] V5-> #�O�.��; KD  HX  MMCIU  HXCIU  ��. K�
C    50-55   37-42    MSWX[6] V6-> #�O�.��; KD  HX  MMCIU  HXCIU  ��. K�
C
C----------------------------------------------------------------------
C  LM<O����> = 55;      LM<�OKA��H��>=42
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 301, 9*0 /
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
       IF ( MSW(14) .EQ. 0 ) RETURN
C-->  HOMEP ��-TA C U. ��P
       NELMOT = MSW(15)
C-->
       NEL = MSW(1)
       TIP = MSW(2)
C-->
C-->  �CKAT� B MSWALL ��EMEHT C #��-TA = NELMOT:
       CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, KVIV, ATOKM )
C-->
       IF   ( IRC .EQ. 1 ) GO TO 1
C-->  ��EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 100) NEL, TIP, NELMOT
100    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     =  ' ( T�� # ', I3, ' ) ***'/
     =  ' *** C HE C��ECTB����M B �E�� ��EMEHTOM # ', I5,' ***'/
     =  ' *** O�PA�OTKA �E�� �PEKPA�AETC� *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HA���, �TO �A T�� TAM ��BET ?
       TIPX = MSWALL(AMOT + 1 )
       DO 2 I = 1, K10
       IF ( TIPX .EQ. LTAB(I) ) GO TO 3
2      CONTINUE
C-->  CB��� C HE HA��M T��OM.....
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,101) NEL, TIP, NELMOT, TIPX
101    FORMAT(//' *** �A�POC HA �O�. CB��� ��EMEHTA # ', I5,
     = ' ( T�� # ', I3, ' ) ***'//
     = ' *** C ��EMEHTOM # ', I5, ' ( T�� # ', I3, ' ) - O���O�EH',
     = ' ��� HE PEA���OBAH ***'/
     = ' *** O�PA�OTKA �E�� �PEKPA�AETC� ***'//)
       RETURN
3      CONTINUE
C-->  AMOT - A�P MSW ��EMEHTA, C KOTOP�M B CB���..
       MSW(16) = AMOT
C-->    ATOKM -->  A�PEC B�B. �HA�. �TO�O ��EMEHTA
       MSW(17) = ATOKM
       KOD = 0
C-->  C�OPM�POBAT� # �O���EMEHTOB
       NN = NEL * 1000
       DO 4 I=1,6
       J = 20 + (I-1)*6
       MSW(J) = NN + I
4      CONTINUE
       RETURN
       END
