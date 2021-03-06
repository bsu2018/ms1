       SUBROUTINE FM0011( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: FM0011( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA�HA�EH�E:        HACTPO�KA MAKPOMO�E�� # T��A 50:
C  ------------  BO�����TE�� H��E-3�_0 ( ��� ��. MA�. 3X3X0 )
C                ********************************************
C                �PO��HAK T��A #6, BEHT���H�� �PEO�PA�. �ACTOT�;
C
C              ��EMEHT O��C�BAETC�:
C
C   � 3 T 50 � KA KB KC KRA KRB KRC
C   � T_BK�  DTCIU #��_TA_PE����TOPA_BO�����EH��<�EHEPATOP
C                                                ��PAB����E�O
C                                                HA�P��EH��>
C            KXA  KXB  KXC  EYYV  EYYI;
C     ��E:
C
C              KA, KB, KC  - ���� ��TA��E�O 3-X �A�HO�O HA�P��EH��;
C              KRA, KRB, KRC - ���� POTOPHO� O�MOTK� ��. MA��H� ->
C                              B�XO�HOE HA�P��EH�E �PEO�PA�OBATE��
C                              �ACTOT�;
C              DTCIU - ���TE��HOCT� �M����CA "C��" -> [ B ��. �PA��CAX ]
C                      �M����C� �EHEP�P��TC� CO��ACHO CPABHEH��
C                      HA�P. O�OPHO�O -> B���PAEMO�O �� MACC�BA
C                      TOKN/TOKN1 ��EMEHTA #_��._PE�._BO����.
C                      ( TP� �EPB�E C�OBA ) � HA�P��EH�� ��TAH��
C                      �PEO�PA�OBATE��, T.E. HA ���AX KA, KB, KC;
C                      ( CM. DM0015 )
C              #_��.BO��. - MO�E�� O�PE�E�EH�� HA�P��EH�� ��PAB�EH��:
C                         ( T�� #40, T�� # 63, T�� # 13 );
C                         ###############################
C                      KXA, KXB, KXC - ���� "O�OPHO�" C�CTEM�
C                                      HA�P��EH�� �A�A: "A", "B", "C";
C         EYYV - O�PAH��EH�E ��P. C��HA�A �P� B��P�M�TE��HOM PE��ME,
C         EYYI - O�PAH��EH�E ��P. C��HA�A �P� �HBEPCHOM      PE��ME;
C
C === B�BO�H�E �HA�EH��: I1, I2, I3, I4, I5, I6, - TOK� BHE�. B�BO�OB,
C                        IV1, IV2, ..., IV18,    - TOK� BEHT��E�,
C                        CIU1, CIU2, ..., CIU18 - "C��" HA BEHT��E;
C
C
C   METO�:       PA�METKA �O�E�, �A�AH�E CX_BEHT��� = 0( �A�PET    );
C   -----
C   �APAMETP�:    BA0T, KNOTT, PAR0T - "OTPE�KA" MACC�BOB �CX. �H�.
C   ---------
C                RMSW - "�EPEMEHA�" �ACT� MSW_��EMEHTA,
C                LM - EE ���HA B C�OBAX;
C   �O��PO�PAMM�:   HET
C   ------------
C   �P�ME�AH�E:  1. KOHTPO�� HE PEA���OBAH;
C  -----------      !!!  KOHTPO��P�ETC� TO��KO "�O�A�AH�E" HOMEPOB
C                        ���OB KXA, KXB, KXC B ��A�A�OH OT 0 �O
C                        MAKC�MA��HO �A�AHHO�O ( NMKNOT )
C                   !!!               � H A � E  --->  STOP 20
C                                                      -------
C                2. ��� OKOH�ATE��HO� HACTPO�K� ->
C                    C����T LM_MOH�TOP "LM0002";
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
C 20, 21     6, 7      [F] EYYV,  EYYI;
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
C     9              - ��A� KD COCTO�H��;
C    10              - PE�EPB
C********************************************************************
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( NMKNOT, MASPRT(7) )
       EQUIVALENCE( RK,  K )
       EQUIVALENCE ( NPB, RNPB   )
       EQUIVALENCE ( ID1, D1        )
       INTEGER NPB, TIP, BA0T
C
       DATA ID1 / 1 /
C
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 50 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0011 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
C-->
C  PAR0T -> TBKL, TCIU, NPB, KXA, KXB, KXC, EYYV, EYYI
C            1     2     3    4    5    6    7     8
C######################################################################
1      CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 195
C      *********
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C
       TBKL = PAR0T(1)
       RMSW(4) = TBKL
C
       NPB = IFIX( PAR0T(3) )
       RMSW(1) = RNPB
C
       DTCIU = PAR0T(2) / 1.8E+4
       RMSW(5) = DTCIU
C
       RMSW(6) = PAR0T(7)
       RMSW(7) = PAR0T(8)
C
C-->  PACTAB�T� B MSW_BEHT��E� CX=1 - PA�PE�EH�E  !! ��OK�POBAHO !!
C
C      DO 3 NV = 1,18
C      ICX = (NV-1)*10 + 16 + 4
C      RMSW(ICX) = D1
C3      CONTINUE
C----------------------------------------------------------------------
C-->  "O�OPH�E" ����
       K = IFIX( PAR0T(4) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(9) = RK
       K = IFIX( PAR0T(5) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(10) = RK
       K = IFIX( PAR0T(6) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(11) = RK
C-
       RETURN
C-
5      CONTINUE
       WRITE(NWTR,  502 ) NEL, K
       WRITE(NWTRE, 502 ) NEL, K
502    FORMAT(/' *** B ��EMEHTE # ', I6, ' HE �O��CT�M�� ',
     = '��E� # ', I5, ' ***'/
     = ' ***  PAC�ET �PEKPA�EH   [ FM0011 ] ***'//)
       STOP 20
       END
