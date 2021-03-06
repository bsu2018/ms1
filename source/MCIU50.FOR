       SUBROUTINE MCIU50( TN, H0, UOPN, UOPN1, UYPN, UYPN1,
     *                    DTCIU, DTMCIU, MSW50, RMSW50 )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: MCIU50( TN, H0, UOPN, UOPN1, UYPN, UYPN1,
C                         DTCIU, DTMCIU, MSW50, RMSW50 )
C   HA�HA�EH�E: "�EHEPATOP C��HA�OB C��" �O A��OP�TM�
C   -----------  "BEPT�KA��HO�O ��PAB�EH��" C ��ETOM �A�PETA-PA�PE�EH��
C                 �O�A�� C��HA�OB "C��" ( OT 5.08.86 );
C
C   METO�:    CPABHEH�E �B�X C��HA�OB, ��KCA��� �EPEXO�A �X
C   -----      "PA�HOCT�" �EPE� "HO��" OT OTP��ATE��HO�O �HA�EH��;
C              �O �T�M �AHH�M -> B���C�EH�E TS - BPEMEH� CTAPTA,
C              OCTA��HOE O�EB��HO;
C
C      !!! "C��" CTAPT��T B TO�KE �EPECE�EH�� U_��PAB�EH�� C
C                         "H�C�A�A���M�" ��ACTKAM� U_O�OPHOE.
C      !!!
C
C
C   �APAMETP�-O�O�HA�EH��:
C   ----------------------
C              UOPN, UOPN1 - O�OPH�E HA�P��EH��,
C              UYPN, UYPN1 - ��PAB�����E HA�P��EH��,
C                              B T=TN � T=TN+H0
C              TN - BPEM� N-�O �A�A, H0 - �A� �HTE�P.
C              DTCIU - ���TE��HOCT� �M����CA;
C              DIMCIU - M�H�MA��HA� ���TE��HOCT� �M����CA,
C                       KOTOP�� "��BCTB��T" BET���;
C              MSW50 (RMSW50) - MACC�BA O��CAH�� BEHT���:
C              *************
C
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
C     9              - ��A� KD COCTO�H��
C    10              - PE�EPB
C********************************************************************
C
C   �O��PO�PAMM�:   HET;
C   ------------
C   �P�ME�AH�E:   1.  H0  �O��EH ��T� MEH��E DTCIU;
C   ----------
C
CZZZ-------------------------------------------------------------------
       DIMENSION MSW50(*), RMSW50(*)
       INTEGER CX
C
       TX = RMSW50(4)
       CX = MSW50(3)
C
C-->  "ECTECTBEHH��" CTAPT.., ECT� �� OH B [TN, TN+H0 ] ?
       TN1 = TN+H0
       DUN = UOPN - UYPN
       DUN1 = UOPN1 - UYPN1
C-->  TS - O�EPE�HO� HE "XO�ET" ??
       IF ( DUN.LT.0.0 .AND. DUN1.GE.0.0 .AND. UOPN.GT.UOPN1 )  GO TO 1
       IF ( DUN.GT.0.0 .AND. DUN1.LE.0.0 .AND. UOPN.GT.UOPN1 )  GO TO 1
C-->  O�EPE�HO�O CTAPTA HE���O, �PE��O�O�AEM TS � TK O�PE�E�EHH�M�
C  PA��OP CX, TX �APAMETPOB
       GO TO 2
1      CONTINUE
C-->  "E" CTAPT HOBO�O �M�.
       HXCIUE = -( DUN / ( DUN1 - DUN ) ) * H0
       TSE = TN + HXCIUE
       TKE = TSE + DTCIU
C-->  ��KC�P�EM �TO..
       RMSW50(1) = TSE
       RMSW50(2) = TKE
2      CONTINUE
C-->  �OC�E�H�� B��AHH�� �M�. "C��" -->
       TSE = RMSW50(1)
       TKE = RMSW50(2)
C-->  TS  �  TK -- �M�. ��PAB�EH�� C ��ETOM CX,TX
C-->  CX=0 -> �A�PET, CX=1 - PA�PE�EH�E HA "C��", TX - KO��A
C
       IF ( CX .EQ. 1 ) GO TO 3
C-->  �E�CTB�ET �A�PET...
C-->  ��MEH�M, EC�� HA�O  BPEMEHA CTAPTA ��� ��H��A...
C-->  �E��O�.  �M�. HET..
       TS = -DTCIU
       TK = - DTCIU
       IF ( TX .LT. TSE ) GO TO 100
C-->  �PE�. C�� ECT�...
       TS = TSE
       TK = TKE
       IF ( TKE .LT. TX ) GO TO 100
C-->  B���MO C��  O�P�BAETC� �O CPE��HE �M����CA
C     TSE <= TX <= TKE
       TS = TSE
       TK = TX
       GO TO 100
C----------------------------------------------------------------------
3      CONTINUE
C-->  C��  PA�PE����...
C  �PE�  C�� ECT�
       TS = TSE
       TK = TKE
       IF ( TX .LE. TSE ) GO TO 100
C-->  �PE�. C�� HET
       TS = -DTCIU
       TK = -DTCIU
       IF ( TKE .LE. TX ) GO TO 100
C-->  B���MO PA�PE���� "�O CPE��HE"
       TS = TX
       TK = TKE
C**********************************************************************
C**********************************************************************
100    CONTINUE
C-->  TS  � TK  C ��ETOM  KODA CX  � TX BPEMEH�
C-->  KAPTA C��--->
       M1 = 0
       M2 = 0
C      MCIU -->  M1.M2
       IF ( TS .LE. TN   .AND.  TN  .LT. TK )   M1 = 1
       IF ( TS .LE. TN1  .AND.  TN1 .LE. TK )   M2 = 1
C-->  EC�� M1=0 � M2=0 -->  C�� HET.....
       IF ( M1 .EQ. 0  .AND.  M2 .EQ. 0 ) GO TO 200
C-->  M1 � M2  =1 C��  HE KOH�AET!!
       IF ( M1 .EQ. 1  .AND.  M2 .EQ. 1 ) GO TO 201
C-->  �O�. M1 = 1 (A M2=0 )  ->  �A�H�� �POHT..
       IF ( M1 .EQ. 1 ) GO TO 202
C--->  HE TAK,  M1=0, A M2 = 1       -->  �EPE�H�� �POHT
C -->  ���T. C��, KAKA� �O�����AC�..
       DTXXX = TK - TS
C-->  MO�ET "C��" O�EH� KOPOTK�� ?
       IF ( DTXXX .LE. DTMCIU ) GO TO 200
C-->  C�� BEHT��� �O��BCTB�ET..
       MCIU = 01
       HXCIU = TS - TN
       GO TO 300
C######################################################################
200    CONTINUE
C-->  M1=0, M2=0
       MCIU = 0
       HXCIU = 0
       GO TO 300
201    CONTINUE
C-->  M1, M2 = 1
       MCIU = 11
       HXCIU = 0.0
       GO TO 300
202    CONTINUE
C-->  M1 = 1, M2 = 0 -->  �A�OK..
       MCIU = 10
       HXCIU = TK - TN
C**********************************************************************
C**********************************************************************
300    CONTINUE
C-->  ��KC�P�EM
       RMSW50(6) = HXCIU
       MSW50(5) = MCIU
       RETURN
       END
