       SUBROUTINE LM0002( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: LM0002( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C   HA�HA�EH�E:   �O���ECKA� HACTPO�KA MSW MAKPOMO�E�� T��A # 50
C   ----------     *** BO�����TE�� H��E 3�-0 (��� ��. MA�. 3X3X0) ****
C
C                  ��EMEHT MO�ET ��T� �O���ECK� CB��AH C MAKPOMO�E��M�:
C                       T�� # 40,  # 63 ( 3-X �A�HA� �.�.C.),
C                           # 13 ( "��MEP�TE�� HAP��EH��" );
C
C ===                  TP� �EPB�E �HA�EH�� �� TOKN/TOKN1 - �P�H�MA�TC�
C                      �A �HA�EH�� HA�P��EH�� ��PAB�EH��;
C ===
C
C   METO�:        �O�CK B MSW_ALL O��CAH�� ��EMEHTA *��. MA�.*
C   -----         ( �O�. T�� CO��ACHO BCTPOEHHO� TA�. "LTIP" )
C                 B C���AE HAXO��EH�� -> �O�OPM�POBAH�E MSW_O��CAH��;
C                 B �TOM C���AE KOD = 0, �HA�E = 1;
C   �APAAMETP�:   MSW(RMSW) - "OTPE�OK" MSW_MO�E�� T�� # 50;
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
C                      MAKPOMO�E�� # T��A 50:
C                BO�����TE�� H��E-3�_0 ( ��� ��. MA�. 3X3X0 )
C                ********************************************
C                �PO��HAK T��A #6, BEHT���H�� �PEO�PA�. �ACTOT�;
C
C              ��EMEHT O��C�BAETC�:
C
C   � 3 T 50 � KA KB KC KRA KRB KRC
C   � T_BK�  DTCIU #��_TA_PE����TOPA_BO�����EH��<�EHEPATOP
C                                                ��PAB����E�O
C                                                HA�P��EH��
C
C            KXA  KXB  KXC;
C     ��E:
C
C              KA, KB, KC  - ���� ��TA��E�O 3-X �A�HO�O HA�P��EH��;
C              KRA, KRB, KRC - ���� POTOPHO� O�MOTK� ��. MA��H� ->
C                              B�XO�HOE HA�P��EH�E �PEO�PA�OBATE��
C                              �ACTOT�;
C              DTCIU - ���TE��HOCT� �M����CA "C��" ->
C                      �M����C� �EHEP�P��TC� CO��ACHO CPABHEH��
C                      HA�P. O�OPHO�O -> B���PAEMO�O �� MACC�BA
C                      TOKN/TOKN1 ��EMEHTA #_��._PE�._BO����.
C                      ( TP� �EPB�E C�OBA ) � HA�P��EH�� ��TAH��
C                      �PEO�PA�OBATE��, T.E. HA ���AX KA, KB, KC;
C                      ( CM. DM0015 )
C                      KXA, KXB, KXC - ���� "O�OPHO�" C�CTEM�
C                                      HA�P��EH�� �A�A: "A", "B", "C";
C
C              #_��.BO��. - MO�E�� O�PE�E�EH�� HA�P��EH�� ��PAB�EH��:
C                         ( T�� #40, T�� # 63, T�� # 13 );
C                         ###############################
C
C === B�BO�H�E �HA�EH��: I1, I2, I3, I4, I5, I6, - TOK� BHE�. B�BO�OB,
C                        IV1, IV2, ..., IV18,    - TOK� BEHT��E�,
C                        CIU1, CIU2, ..., CIU18 - "C��" HA BEHT��E;
C
C              �OPMAT MSW # T��A 50 [ BO�����TE��, H��E ]:
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
C  9,10,11    -        KA, KB, KC, - HOMEPA ���OB �A� "��TAH��"
C  12,13,14   -        KRA< KRB, KRC - HOMEPA ���OB B�XO�HO�O
C                                      HA�P��EH�� H��E-3�_0
C-- -- - - - - --- - - - - - - - -
C   15         1       [F] NPB - HOMEP ��EMEHTA, �OCTAB����E�O
C                                HA�P��EH�E ��PAB�EH��;
C  16          2       [L] ANPB - A�PEC B MSW_ALL NPB ��EMEHTA;
C  17          3       [L] AUYP - A�PEC B TOKN/N1 NPB ��EMEHTA;
C  18          4       [F] TBKL - BPEM� BK�. H��E;
C  19          5       [F] DTCIU - ���T. �M����CA "C��"
C 20, 21     6, 7           PE�EPB;
C  22         8             "HX"  K���E� K.�. ���OB;
C 23<->25    9<->11  [F]     HOMEPA "O�OPH�X" ���OB;
C 26<->29    12<->15          PE�EPB;
C
C 30<->39    16<->25      MSWV1  -  C�OBO ��PAB�EH�� 1-M BEHT��EM
C 40<->49    26<->35      MSWV2  -  C�OBO ��PAB�EH�� 2-M BEHT��EM
C .....................................................................
C 200<->209   186<->195   MSWV18 -  C�OBO ��PAB�EH�� 18-M BEHT��EM
C**********************************************************************
C
C   LM O��A� = 209 C�OB, LM_�OK. = 195
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
C     9              - PE�EPB
C    10              - PE�EPB
C********************************************************************
C
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, PTIP, AMOT, APOT, ATOKM, TIPX
       DIMENSION LTAB(10)
       DATA LTAB/ 40, 63, 13, 7*0 /
       DATA K10 / 10 /
C----------------------------------------------------------------------
       KOD = 0
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
       RETURN
       END
