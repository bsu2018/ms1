       SUBROUTINE FM0015( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: FM0015( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C   HA�HA�EH�E:        HACTPO�KA MAKPOMO�E�� # T��A 92:
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
C     HA�P��EH�E O�OPHOE ��� 1-�O BEHT��E� -->   V(�O1)
C     HA�P��EH�E O�OPHOE ��� 2-�O BEHT��E� -->   V(�O2)
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
C                                           ��PAB�EH�� �O�O��TE��HOE;
C
C
C              ��EMEHT O��C�BAETC�:
C
C   � # T 92 � K  M  �  �O1  �O2   ��1  ��2  DTCIU  EYYV  EYYI;
C
C     ��E:     K, M  -  ���� �O�K���EH��,
C
C              �O1, �O2  -  ���� "O�OPHO�O" HA�P��EH��-> V(�01),V(�O2)
C
C              ��1, ��2  -  ���� "��PAB����E�O" HA�-� ->[V(��1)-V(��2)]
C
C              DTCIU - ���TE��HOCT� �M����CA "C��" -> [ B ��. �PA��CAX ]
C                      �M����C� �EHEP�P��TC� CO��ACHO CPABHEH��
C                      HA�P. O�OPHO�O -> B���PAEMO�O �� MACC�BA
C                      VALLN/VALLN1 �  HA�P��EH�� "��PAB����E�O" ->
C                      B���PAEMO�O �� VALLN/VALLN1;
C
C  EYYV - O�PAH��EH�E "CBEPX�" ��P. C��HA�A;
C  EYYI - O�PAH��EH�E "CH���"  ��P. C��HA�A;
C
C
C === B�BO�H�E �HA�EH��: I1         - TOK  BHE�. B�BO�OB, OT "K" K "M";
C                        I2, I3     - TOK� BEHT��E�,
C                        I3, I4     - "C��" HA BEHT���X;
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
C                        BCEX ���OB          B ��A�A�OH OT 0 �O
C                        MAKC�MA��HO �A�AHHO�O ( NMKNOT )
C                   !!!               � H A � E  --->  STOP 20
C                                                      -------
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
C      $$                 ( B 25 - XPAH�T�C� EYYV )
C 26<->35    16<->25      MSWV2  -  C�OBO ��PAB�EH�� 2-M BEHT��EM
C      $$                 ( B 35 - XPAH�TC� EYYI )
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
C    10              - ��� 1-�O BEHT��� -> EYYV, ��� 2-�O --> EYYI;
C
C********************************************************************
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
       DATA ID1 / 1 /
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 92 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0015 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
C-->
C  PAR0T -> �O1,  �O2,  ��1,  ��2, DTCIU  EYYV  EYYI;
C            1     2     3    4    5        6    7
C######################################################################
1      CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 25
C      *********
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C
       DTCIU = PAR0T(5) / 1.8E+4
       RMSW(5) = DTCIU
C
C-->  PACTAB�T� B MSW_BEHT��E� CX=1 - PA�PE�EH�E     !! ��OK�POBAHO !!
C
C      DO 3 NV = 1,2
C      ICX = (NV-1)*10 +  6 + 4
C      RMSW(ICX) = D1
C3      CONTINUE
C----------------------------------------------------------------------
C-->  "O�OPH�E"  � "��P" ����
       DO 4 I = 1, 4
       K = IFIX( PAR0T(I) )
       IF ( K .LT. 0  .OR.  K  .GT. NMKNOT ) GO TO 5
       RMSW(I) = RK
4      CONTINUE
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       RMSW(15) =  PAR0T(6)
       RMSW(25) =  PAR0T(7)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       RETURN
C-
5      CONTINUE
       WRITE(NWTR,  502 ) NEL, K
       WRITE(NWTRE, 502 ) NEL, K
502    FORMAT(/' *** B ��EMEHTE # ', I6, ' HE �O��CT�M�� ',
     = '��E� # ', I5, ' ***'/
     = ' ***  PAC�ET �PEKPA�EH   [ FM0015 ] ***'//)
       STOP 20
       END
