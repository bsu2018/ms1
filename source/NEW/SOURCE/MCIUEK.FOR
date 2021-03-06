       INTEGER FUNCTION MCIUEK( TN, H0, HX, F1, F2, TS, DTAU )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA_��HK���:  MCIUEK( TN, H0, HX, F1, F2, TS, DTAU )
C   --------------------
C   HA�HA�EH�E:  "�EHEPATOP" - "BEP���KATOP" �M����COB ��PAB�EH�� C��:
C   ----------
C               BAP�AHT "ECTECTBEHHA� KOMM�TA���" ( EK ):
C               -----------------------------------------
C
C  ��PAB�EH�E �A�AETC�:
C  -------------------
C
C  F1(T) - O�OPHOE HA�P��EH�E ( MO����P�EMOE ) -> BHE�H�� ��HK���;
C  F2(T) - HA�P��EH�E ��PAB�EH�� ( MO����P���EE ) -> BHE�. ��H-�;
C               O�O�HA�EH��:
C               ------------
C   DF12(T) :=: F1(T) - F2(T)
C   MCIUEK := (�HA�EH�E ��HK���) --> "��AH �M����CA ��PAB�EH��:
C
C            = 00 -->  HA [TN, TN+H0] �M����CA HET;
C            = 01 -->  HA [TN, TN+H0] �M����C �O�B��C�;
C            = 11 -->  HA [TN, TN+H0] �M����C HE �C�E�A�;
C            = 10 -->  HA [TN, TN+H0] �M����C �C�E�;
C
C   ( M1,M2 ) -->  B A��OP�TME COOTBETCTB��T �T�M �AHH�M;
C
C   DTAU - ���TE��HOCT� �M����CA;
C   TN -  BPEM� HA�A�A N-� TO�K�;
C   H0 - �A�, O�PE�E������ C�E�. BPEMEHH�� TO�K�;
C   TN1 = TN + H0;
C   HX - �A� OT TN  �O  TX MOMEHTA BPEMEH�, ��E
C   TX - BPEM� CMEH� COCTO�H�� �M����CA;
C   TS - MOMEHT BPEMEH� O�EPE�HO�O "�A��CKA" �M����CA C��;
C
C !!!!! �P� �EPBOM O�PA�EH�� K MCIUEK --> �A�AT� TS = -2*DTAU !!!!!!!!!!
C
C   METO�:  KOHTPO�� �A��CK/CH�T�E �M����CA C�� �O C�E�. A��OP�TM�:
C   -----   A) �P� DF12(TN) <= 0.0 � DF12(TN+H0) >= 0.0
C              C�� �A��CKAETC�, TS ��KC�P�ETC�, HX O�PE�E��ETC�;
C           B) �P� (TN+H0) > (TS+DTAU) --> �M����C CH�MAETC�,
C              HX O�PE�E��ETC�;
C   �APAMETP�: TN - BPEM� O�PE�E�EH�� C��HA�A;
C   ---------  H0 - OKPECHOCT� O�PE�E�EH�� C��HA�A;
C              HX - �A� OT TN �O TX - BPEMEH� �POXO��EH�� O�EPE�HO�O
C                   �POHTA �M����CA;
C              F1, F2 - BHE�H�E ��HK��� O�OPHO�O � ��PAB����E�O
C                       C��HA�OB;
C              TS - PAHEEE O�PE�E�EHH�� MOMEHT BPEMEH� CTAPTA �M����CA;
C              DTAU - �A�AHHA� ���TE��HOCT� �M����CA;
C
C
C  �HA�EH�E ��HK���:  MCIUEK = 00  --> "C��" HET B [TN, TN+H0];
C  ****************                     HX = 0.0;
C                     MCIUEK = 01  --> B [TN, TN+H0] -> �EPE�H�� �POHT
C                                      �M����CA "C��", HX = �HTEPBA� OT
C                                      TN �O �EPE�HE�O �POHTA "C��";
C                     MCIUEK = 10 -->  B [TN, TN+H0] -> �A�H�� �POHT
C                                      �M����CA "C��", HX = �HTEPBA�
C                                      OT TN �O �A�HE�O �POHTA �M����CA;
C                     MCIUEK = 11 -->  B [TN, TN+H0] "C��" BK���EH,
C                                      COCTO�H�E HE MEH��, HX=0.0;
C
C                !!!!!  �P� H0 > TIMP --->  BO�MO�HA �OPEP� "C��" !!!!!
C                      �PE��O�O�AETC�, �TO H0 < TIMP
C
C
C
C   �O��PO�PAMM�:  F1  �   F2  -  BHE�H�E, �A�AHH�E �P� B��OBE;
C   ------------
C   �P�ME�AH�E:
C   ----------
CZZZ-------------------------------------------------------------------
       EXTERNAL F1, F2
C-->  BPEM� KOH�A...
       TK = TS + DTAU
C-->  BPEM� C�. TO�K�
       TN1 = TN + H0
C-->  HX -->  CTAH�APTHO = 0.0
       HX = 0.0
C   A) ----------------------------------------------------------------
C-->  �OCTPOEH�E [M1, M2]-->
       M1 = 0
       IF ( TS .LE. TN  .AND.  TN .LE. TK ) GO TO 1
C   B) ----------------------------------------------------------------
C-->  AHA��� ��MEHEH�� ( M1=0 ), �O�CK M2 = ?
C--->  �A��CK CIU BO�MO�EH ?
       DF12N = F1(TN) - F2(TN)
       DF12N1 = F1(TN1) - F2(TN1)
       IF ( DF12N .LE. 0.0  .AND. DF12N1 .GE. 0.0 ) GO TO 2
C---> COCTO�H�E HE ��MEH��OC�, M2 = 0, �A��CKA C�� HET...
       MCIUEK = 00
       RETURN
C======================================================================
2      CONTINUE
C-->  �A��CK C�� ECT�(M1=0, M2=1), KO��A ?
       HX = -H0 * ( DF12N / (DF12N1 - DF12N) )
C-->  TS HOBOE...
       TS = TN + HX
       MCIUEK = 01
       RETURN
C======================================================================
1      CONTINUE
C--> M1=1, HA�T� M2 = ?
       IF ( TN1 .LE. TK ) GO TO 3
C-->  �M����C CH�MAETC�...  HA [TN, TN+H0], KO��A  ?
       HX = TK - TN
       MCIUEK = 10
       RETURN
C======================================================================
3      CONTINUE
C-->  M1  �   M2  = 1, ��MEHEH�� HET, �M����C �O�AH...
       MCIUEK = 11
       RETURN
       END
