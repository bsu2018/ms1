       SUBROUTINE PSUB9( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �HBAP�  1988 �.)------------
C   �O��PO�PAMMA: PSUB9( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   �AKOHOB ��PAB�EH�� OT�E��H�M BEHT��EM ���
C                �P���O� BEHT��E�.
C                  ( MO�E�� #301 - C��� ).
C               �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C               �HA�EH��M TOKOB � HA�P��EH�� ��EMEHTOB CXEM�
C               O�PE�E���TC� KO� C�� KA��O�O BEHT��� � �A� �O
C               MOMEHTA ��MEHEH�� �HA�EH�� C��.
C
C######################################################################
C
C         �O��PO�PAMMA �OPM�P�ET C��HA�� ��PAB�EH�� T�P�CTOPAM�
C     TPEX�A�HO-O�HO�A�HO�O �PEO�PA�OBATE�� �ACTOT� C HE�OCPE�CTBEH-
C     HO� CB���� � ECTECTBEHHO� KOMM�TA��E� �P� PA��E��HOM ��PAB�E-
C     H�� PA�OTO� AHO�HO� � KOTO�HO� �P���AM�.
C         BXO�H�M� BE����HAM� �B���TC� C��HA� ��PAB�EH�� ( UYPR )
C     � TP� O�OPH�X C��HA�A ��� KA��O� �A�� ��TA��E� CET�  UOPA, UOPB,
C     UOPC, KOTOP�E �O��H� O�EPE�AT� "CBO�" �A�H�E HA�P��EH�� HA
C     O��HAKOB�� ��O� ( =< 60 ��. �PA� ).
C         C��HA�� ��PAB�EH�� HA T�P�CTOP� KATO�HO� �P���� �OPM�P��TC�
C     �P� �O�O��TE��HOM HA�P��EH�� ��PAB�EH�� �P� COB�A�EH�� E�O C
C     H�C�A�A���M ��ACTKOM COOTBETCTB���E�O O�OPHO�O HA�P��EH��.
C     ��� AHO�HO� �P���� - TO�E �P� OTP��ATE��HOM HA�P��EH�� ��PAB�EH��.
C         ���TE��HOCT� �M����CA ��PAB�EH�� �A�AETC� �O���OBATE�EM
C     �EPE� �APAMETP �/� PSUB 9.
C         �P� �EPEXO�E TOKA HA�P��K� �EPE� HO�� �O��EP��BAETC�
C     �E�TOKOBA� �A��A, ���TE��HOCT� KOTOPO� �A�AETC� �O���OBATE�EM
C     �EPE� �APAMETP �/� PSUB 9.
C
C
C     O��CAH�E C���, ��PAB����E� TPEX�A�HO-O�HO�A�H�M �PEO�PA�OBATE�EM
C     C�E����EE :
C
C      � #  T 301  � 0  �
C           MAS < 5  #��1  #CM1  #��2  #CM2  #��3  #CM3  #��4  #CM4
C                    #��5  #CM5  >
C           MAS < 0 >
C           PSUB  9  <  DIMP   TPAUSE  >  ;
C
C    ��E
C      #��1,#CM1 - HOMEP ��EMEHTA, TOK KOTOPO�O �C�O����ETC� B KA�ECTBE
C                  C��HA�A ��PAB�EH�� ;
C      #��2,#CM2 - HOMEPA ��EMEHTOB, TOK KOTOPO�O �C�O����ETC� B
C      #��3,#CM3   KA�ECTBE O�OPH�X C��HA�OB COOTBETCTBEHHO ���
C      #��4,#CM4   �A� "A", "B", "C";
C      #��5,#CM5 - HOMEP ��EMEHTA, TOK KOTOPO�O �B��ETC� TOKOM
C                  HA�P��K� ;
C      DIMP      - ���TE��HOCT� �M����CA ��PAB�EH�� ( CEK.);
C      TPAUSE    - ���TE��HOCT� �E�TOKOBO� �A���    ( CEK.).
C
C
C######################################################################
C
C
C   METO�:     O�EB��EH.
C   -----
C  �APAMETP�:   NEL - HOMEP ��EMEHTA, ��� KOTOPO�O PSUB PA�OTAET;
C  ---------    URMAS - MACC�B �AHH�X �O���OBATE��:
C                       1-E C�OBO - KO���ECTBO �APAMETPOB B C��CKE
C                       ( �A��CAHO �E��M ��C�OM B BE�ECTBEHH�� MACC�B )
C                       2-E C�OBO � �A�EE - �APAMETP� PSUB,
C                           �A�AHH�E �O���OBATE�EM;
C
C----------------------------------------------------------------------
C              STMAS - MACC�B �AHH�X, �EPE�ABAEM�X B PSUB:
C
C      C�OBO           O�O�HA�EHE   -   �HA�EH�E
C      1               TN - BPEM� N - �O �A�A PAC�ETA;
C      2               H0 - �A� �HTE�P�POBAH��;
C      3               I1 - �HA�EH�E 1-�O TOKA, �ATPE�OBAHHO�O
C                           �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C      4               I11- TO�E, ��� MOMEHTA TN+H0;
C         �A�EE AHA�O���HO �HA�EH�� OCTA��H�X TOKOB;
C   N=3+2*NTOKOB       U1 - �HA�EH�E 1-�O HA�P��EH��, �ATPE�OBAHHO�O
C                           �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C    N+1               U11- TO�E, ��� MOMEHTA TN+H0;
C         �A�EE AHA�O���HO �HA�EH�� OCTA��H�X HA�P��EH��;
C
C----------------------------------------------------------------------
C              DGMAS  -  MACC�B B��EC�EHH�X �APAMETPOB C��.
C
C   C�OBO        O�O�HA�EHE - �HA�EH�E:
C  OT 1 �O 6     - �HA�EH�� MCIU ( KO� C�� )
C                  MCIU = 00  --> "C��" HET B [TN, TN+H0];
C                                  HX = 0.0;
C                  MCIU = 01  --> B [TN, TN+H0] -> �EPE�H�� �POHT
C                                 �M����CA "C��", HX = �HTEPBA� OT
C                                 TN �O �EPE�HE�O �POHTA "C��";
C                  MCIU = 10 -->  B [TN, TN+H0] -> �A�H�� �POHT
C                                 �M����CA "C��", HX = �HTEPBA�
C                                 OT TN �O �A�HE�O �POHTA �M����CA  ;
C                  MCIU = 11 -->  B [TN, TN+H0] "C��" BK���EH,
C                                 COCTO�H�E HE MEH��, HX=0.0;
C
C  OT 7 �O 12    - �HA�EH�� COOTBETCTB����X HX ( �A�A �O BPEMEH�
C                    �O MOMEHTA ��MEHEH�� KO�A C�� )
C----------------------------------------------------------------------
C              INTMAS - MACC�B �� O�HO�O C�OBA.
C                       ( B �O��PO�PAMME HE �C�O����ETC� )
C----------------------------------------------------------------------
C
C   �O��PO�PAMM�: HET
C   -------------
C
C   �P�ME�AH�E: C�MMA KO����ECTBA TPE��EM�X TOKOB � HA�P��EH��
C   ----------  HE �O��HA �PEB��AT� 49-T�.
C               ( KOHTPO�� HE �PO��BO��TC� )
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       REAL INTMAS
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C BBO��M �AHH�E
C----------------------------------------------------------------------
       UYPR = STMAS(3)
       UYPR1= STMAS(4)
       UOPA = STMAS(5)
       UOPA1= STMAS(6)
       UOPB = STMAS(7)
       UOPB1= STMAS(8)
       UOPC = STMAS(9)
       UOPC1= STMAS(10)
       TOKNG= STMAS(11)
       TOKNG1=STMAS(12)
       IF( ABS(TOKNG) .LT. 1.E-4 ) TOKNG = 0.0
       IF( ABS(TOKNG1).LT. 1.E-4 ) TOKNG1= 0.0
       DIMP = URMAS(2)
       TIMEP = URMAS(3)
       TIME1 = URMAS(4)
       TIME2 = URMAS(5)
       TIME3 = URMAS(6)
       TIME4 = URMAS(7)
       TIME5 = URMAS(8)
       TIME6 = URMAS(9)
       TIME0 = URMAS(10)
       IF( (TIME1 + DIMP) .LT. TN ) TIME1 = 0.0
       IF( (TIME2 + DIMP) .LT. TN ) TIME2 = 0.0
       IF( (TIME3 + DIMP) .LT. TN ) TIME3 = 0.0
       IF( (TIME4 + DIMP) .LT. TN ) TIME4 = 0.0
       IF( (TIME5 + DIMP) .LT. TN ) TIME5 = 0.0
       IF( (TIME6 + DIMP) .LT. TN ) TIME6 = 0.0
       IF( (TIME0 + TIMEP).LT. TN ) TIME0 = 0.0
C----------------------------------------------------------------------
C-->  O�H���EM B�XO�H�E �AHH�E
C----------------------------------------------------------------------
       DO 10 I= 1,12
       DGMAS(I) = 0.0
10     CONTINUE
C----------------------------------------------------------------------
C->   ��EM �EPE�H�� �POHT
C----------------------------------------------------------------------
C--->  KAKO� BEHT��� PA�OTAET?
C--->  MO�ET PA�OTAET BEHT��� B �A�E A?
       HXA = -1.
       A = UOPA1 - UOPA
       C = UYPR1 - UYPR
       IF( A .EQ. C ) GO TO 100
       HXA = H0 * (UYPR - UOPA)/(A-C)
       IF( HXA .LT. 0.0  .OR.  HXA .GT. H0 ) HXA = -1.
100    CONTINUE
C--->  MO�ET PA�OTAET BEHT��� B �A�E B?
       HXB = -1.
       A = UOPB1 - UOPB
       C = UYPR1 - UYPR
       IF( A .EQ. C ) GO TO 200
       HXB = H0 * (UYPR - UOPB)/(A-C)
       IF( HXB .LT. 0.0  .OR.  HXB .GT. H0 ) HXB = -1.
200    CONTINUE
C--->  MO�ET PA�OTAET BEHT��� B �A�E C?
       HXC = -1.
       A = UOPC1 - UOPC
       C = UYPR1 - UYPR
       IF( A .EQ. C ) GO TO 300
       HXC = H0 * (UYPR - UOPC)/(A-C)
       IF( HXC .LT. 0.0  .OR.  HXC .GT. H0 ) HXC = -1.
300    CONTINUE
C----------------------------------------------------------------------
C-->  BEHT��� KAKO� �P���� PA�OTAET?
C----------------------------------------------------------------------
       PRZV = TOKNG * TOKNG1
       IA =1
       IK =1
       IF( PRZV .LE. 0.0 ) GO TO 500
       IF( TOKNG .GT. 0.0 ) IA = 0
       IF( TOKNG .LT. 0.0 ) IK = 0
       GO TO 1000
C----------------------------------------------------------------------
500    CONTINUE
       HXTOK = 0.0
       IF( TOKNG .NE. TOKNG1 ) GO TO 600
       IF( UYPR1 .GE. 0.0    ) IA = 0
       IF( UYPR1 .LT. 0.0    ) IK = 0
       GO TO 1000
C----------------------------------------------------------------------
600    CONTINUE
       HXTOK = H0 * TOKNG / (TOKNG - TOKNG1)
       IF( UYPR1 .GE. 0.0    ) IA = 0
       IF( UYPR1 .LT. 0.0    ) IK = 0
       IF( TIME0 .GT. 0.0    ) GO TO 1000
       TIME0 = TN + HXTOK
1000   CONTINUE
C----------------------------------------------------------------------
C-->  �OPM�P�EM B�XO�HO� C��HA�!
C----------------------------------------------------------------------
C
C           BEHT��� �A��  "A"
C
C----------------------------------------------------------------------
C--->  BEHT��� KATO�HO� �P����.
C----------------------------------------------------------------------
       IF( IK .EQ. 1  .AND.  HXA .GE.0.0  .AND.  UOPA .GE. UOPA1 )
     * GO TO 2004
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME1 .GT. 0.0 ) GO TO 2001
C->  C��HA�A HE ���O � HET
       DGMAS(1) = 0.0
       GO TO 2005
2001   CONTINUE
C->  C��HA� ��� � ���ET
       IF( (TIME1 + DIMP) .LE. (TN+H0) ) GO TO 2002
       DGMAS(1) = 11.
       GO TO 2005
2002   CONTINUE
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(1) = 10.
       DGMAS(7) = TIME1 + DIMP - TN
       GO TO 2005
2004   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(1) = 1.
       DGMAS(7) = HXA
       TIME1 = TN + HXA
2005   CONTINUE
C----------------------------------------------------------------------
C--->  BEHT��� AHO�HO� �P����.
C----------------------------------------------------------------------
       IF( IA .EQ. 1  .AND.  HXA .GE.0.0  .AND.  UOPA .LE. UOPA1 )
     * GO TO 2009
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME4 .GT. 0.0 ) GO TO 2006
C->  C��HA�A HE ���O � HET
       DGMAS(4) = 0.0
       GO TO 2010
2006   CONTINUE
C->  C��HA� ��� � ���ET
       IF( (TIME4 + DIMP) .LE. (TN+H0) ) GO TO 2007
       DGMAS(4) = 11.
       GO TO 2010
2007   CONTINUE
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(4) = 10.
       DGMAS(10) = TIME4 + DIMP - TN
       GO TO 2010
2009   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(4) = 1.
       DGMAS(10) = HXA
       TIME4 = TN + HXA
2010   CONTINUE
C----------------------------------------------------------------------
C
C           BEHT��� �A��  "B"
C
C----------------------------------------------------------------------
C--->  BEHT��� KATO�HO� �P����.
C----------------------------------------------------------------------
       IF( IK .EQ. 1  .AND.  HXB .GE. 0.0  .AND.  UOPB .GE. UOPB1 )
     * GO TO 2014
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME2 .GT. 0.0 ) GO TO 2011
C->  C��HA�A HE ���O � HET
       DGMAS(2) = 0.0
       GO TO 2015
2011   CONTINUE
C->  C��HA� ��� � ���ET
       IF( (TIME2 + DIMP) .LE. (TN+H0) ) GO TO 2012
       DGMAS(2) = 11.
       GO TO 2015
2012   CONTINUE
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(2) = 10.
       DGMAS(8) = TIME2 + DIMP - TN
       GO TO 2015
2014   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(2) = 1.
       DGMAS(8) = HXB
       TIME2 = TN + HXB
2015   CONTINUE
C----------------------------------------------------------------------
C--->  BEHT��� AHO�HO� �P����.
C----------------------------------------------------------------------
       IF( IA .EQ. 1  .AND.  HXB .GE. 0.0  .AND.  UOPB .LE. UOPB1 )
     * GO TO 2019
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME5 .GT. 0.0 ) GO TO 2016
C->  C��HA�A HE ���O � HET
       DGMAS(5) = 0.0
       GO TO 2020
2016   CONTINUE
C->  C��HA� ��� � ���ET
       IF( (TIME5 + DIMP) .LE. (TN+H0) ) GO TO 2017
       DGMAS(5) = 11.
       GO TO 2020
2017   CONTINUE
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(5) = 10.
       DGMAS(11) = TIME5 + DIMP - TN
       GO TO 2020
2019   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(5) = 1.
       DGMAS(11) = HXB
       TIME5 = TN + HXB
2020   CONTINUE
C----------------------------------------------------------------------
C
C           BEHT��� �A��  "C"
C
C----------------------------------------------------------------------
C--->  BEHT��� KATO�HO� �P����.
C----------------------------------------------------------------------
       IF( IK .EQ. 1  .AND.  HXC .GE. 0.0  .AND.  UOPC .GE. UOPC1 )
     * GO TO 2024
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME3 .GT. 0.0 ) GO TO 2021
C->  C��HA�A HE ���O � HET
       DGMAS(3) = 0.0
       GO TO 2025
2021   CONTINUE
C->  C��HA� ��� � ���ET
       IF( (TIME3 + DIMP) .LE. (TN+H0) ) GO TO 2022
       DGMAS(3) = 11.
       GO TO 2025
2022   CONTINUE
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(3) = 10.
       DGMAS(9) = TIME3 + DIMP - TN
       GO TO 2025
2024   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(3) = 1.
       DGMAS(9) = HXC
       TIME3 = TN + HXC
2025   CONTINUE
C----------------------------------------------------------------------
C--->  BEHT��� AHO�HO� �P����.
C----------------------------------------------------------------------
       IF( IA .EQ. 1  .AND.  HXC .GE. 0.0  .AND.  UOPC .LE. UOPC1 )
     * GO TO 2029
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME6 .GT. 0.0 ) GO TO 2026
C->  C��HA�A HE ���O � HET
       DGMAS(6) = 0.0
       GO TO 2030
2026   CONTINUE
C->  C��HA� ��� � ���ET
       IF( (TIME6 + DIMP) .LE. (TN+H0) ) GO TO 2027
       DGMAS(6) = 11.
       GO TO 2030
2027   CONTINUE
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(6) = 10.
       DGMAS(12) = TIME6 + DIMP - TN
       GO TO 2030
2029   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(6) = 1.
       DGMAS(12) = HXC
       TIME6 = TN + HXC
2030   CONTINUE
C----------------------------------------------------------------------
C-->  A KAK HA C�ET "�E�TOKOBO� �A���"?
C----------------------------------------------------------------------
       IF( TN .GE. (TIME0+TIMEP) ) GO TO 9999
       DO 2050 I=1,12
       DGMAS(I) = 0.0
2050   CONTINUE
9999   CONTINUE
       URMAS(4) = TIME1
       URMAS(5) = TIME2
       URMAS(6) = TIME3
       URMAS(7) = TIME4
       URMAS(8) = TIME5
       URMAS(9) = TIME6
       URMAS(10)= TIME0
       RETURN
C----------------------------------------------------------------------
       END
