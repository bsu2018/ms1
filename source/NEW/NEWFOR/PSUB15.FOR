       SUBROUTINE PSUB15( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT ﭢ���  1992 �.)------------
C   �O��PO�PAMMA: PSUB15( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   �AKOHOB ��PAB�EH�� OT�E��H�M BEHT��EM    
C                  ( MO�E�� #301 - C��� ).
C               �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C               �HA�EH��M TOKOB � HA�P��EH�� ��EMEHTOB CXEM�
C               O�PE�E���TC� KO� C�� KA��O�O BEHT��� � �A� �O
C               MOMEHTA ��MEHEH�� �HA�EH�� C��.
C
C######################################################################
C
C         �O��PO�PAMMA �OPM�P�ET C��HA�� ��PAB�EH�� T�P�CTOPAM�
C     ��� ��PAB��EMO�O B��P�M�TE��, CO�PAHHO�O �O CXEME �AP�OHOBA.
C         BXO�H�M� BE����HAM� �B���TC� C��HA� ��PAB�EH�� ( UYPR )
C     � TP� O�OPH�X C��HA�A ��� KA��O�O HA�P��EH�� ��TA��E� CET�
C     UOPAB, UOPBC, UOPCA, KOTOP�E �O��H� O�EPE�AT� "CBO�" ��HE�H�E
C     HA�P��EH�� HA O��HAKOB�� ��O� ( =< 30 ��. �PA� ).
C         C��HA�� ��PAB�EH�� HA T�P�CTOP� B��P�M�TE�� �OPM�P��TC�
C     �� ���ਨ ���ᨬ��쭮�� ����த���⢨� 
C         � �� ������ �६��� �������� ⮫쪮 ��� ᨣ���� �ࠢ�����
C          �������� ALFA ��� ������� �����樨�
C
C     O��CAH�E C���, ��PAB����E� B��P�M�TE�EM
C     C�E����EE :
C
C      � #  T 301  � 0  �
C           MAS < 5  #��1  #CM1  #��2  #CM2  #��3  #CM3  #��4  #CM4
C                    #��5  #CM5  >
C           MAS < 0 >
C           PSUB  15 < AMPLIT HA�.���� T1 S1  T2 S2 .......  >  ;
C
C    ��E
C      #��1,#CM1 - HOMEP ��EMEHTA, TOK KOTOPO�O �C�O����ETC� B KA�ECTBE
C                  C��HA�A ��PAB�EH�� ;
C      #��2,#CM2 - HOMEPA ��EMEHTOB, TOK KOTOPO�O �C�O����ETC� B
C      #��3,#CM3   KA�ECTBE O�OPH�X C��HA�OB COOTBETCTBEHHO ���
C      #��4,#CM4   ��HE�H�X HA�P��EH��  "AB", "BC", "CA";
C      #��5,#CM5 - HOMEP ��EMEHTA, TOK KOTOPO�O �B��ETC� TOKOM
C                  HA�P��K� ;
C     AMPLIT    - ������㤠 ���୮�� ᨣ���� �ࠢ����� ;
C     �����.���� - ��砫쭮� ���祭�� �室���� ᨣ���� �ࠢ����� ;
C      T         - ������ ��������� ᨣ����   �ࠢ����� ;
C      S         - ����饥   ���祭�� �室���� ᨣ���� �ࠢ����� ;
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
C                �ணࠬ�� �������� 8.12.1991 ��� �ࠢ����� �
C                ������� � ���ﬨ⥫� ....
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(1), STMAS(1), DGMAS(*), INTMAS(*)
       REAL INTMAS
       EQUIVALENCE (NPAR,RN)
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C BBO��M �AHH�E
C----------------------------------------------------------------------
       UYPR  = STMAS(3)
       UYPR1 = STMAS(4)
       UOPAB = STMAS(5)
       UOPAB1= STMAS(6)
       UOPBC = STMAS(7)
       UOPBC1= STMAS(8)
       UOPCA = STMAS(9)
       UOPCA1= STMAS(10)
       TOKNG = STMAS(11)
       TOKNG1= STMAS(12)
       IF( ABS(TOKNG) .LT. 1.E-4 ) TOKNG = 0.0
       IF( ABS(TOKNG1).LT. 1.E-4 ) TOKNG1= 0.0
       TIME1 = URMAS(2)
       TIME2 = URMAS(3)
       TIME3 = URMAS(4)
       TIME4 = URMAS(5)
       TIME5 = URMAS(6)
       TIME6 = URMAS(7)
       TIME0 = URMAS(8)
C----------------------------------------------------------------------
C-->  O�H���EM B�XO�H�E �AHH�E
C----------------------------------------------------------------------
       DO 10 I= 1,12
       DGMAS(I) = 0.0
 10    CONTINUE
C    ���EM PA�OTAT� ?
C----------------------------------------------------------------------
C      ᫥����� ������� ⮫쪮 ��� ���ﬨ⥫� !!!!!
C      IF( UYPR .LE. 0.0  .AND.  UYPR1 .LE. 0.0 ) RETURN
C----------------------------------------------------------------------
C-->����� ��室��� ���ﭨ� ? .......
         IF(TN .GT. 0.0)  GO TO 1102
         DIMP  = URMAS(9) *0.866
         UYPR = URMAS(10)
         TIME1 = 0.0
         TIME2 = 0.0
         TIME3 = 0.0
         TIME4 = 0.0
         TIME5 = 0.0
         TIME6 = 0.0
         TIME0 = 0.
         IF(UYPR .LT. DIMP) GO TO 1101
         DGMAS(3) = 11.
         DGMAS(5) = 11.
         DGMAS(9) = 0.0
         DGMAS(11) = 0.0
         TIME3  = 1.
         TIME5  = TIME3
         GO TO 2030
1101     CONTINUE
         IF(UYPR .LT. 0.0)  GO TO 11010
         DGMAS(3) = 11.
         DGMAS(4) = 11.
         DGMAS(9) = 0.0
         DGMAS(10) = 0.0
         TIME3  = 1.
         TIME4  = TIME3
         GO TO 2030
11010    CONTINUE
         IF(UYPR .LT. (-DIMP))  GO TO 11015
         DGMAS(2) = 11.
         DGMAS(4) = 11.
         DGMAS(8) = 0.0
         DGMAS(10) = 0.0
         TIME2  = 1.
         TIME4  = TIME2
         GO TO 2030
11015    CONTINUE
         DGMAS(2) = 11.
         DGMAS(6) = 11.
         DGMAS(8) = 0.0
         DGMAS(12) = 0.0
         TIME2  = 1.
         TIME6  = TIME2
         GO TO 2030
1102     CONTINUE
C----> �����ࠥ� ����� ᨣ��� �ࠢ����� � ���
       RN =  URMAS(1)
       NUM  = IFIX (TIME0)
       NMAX = 2*NUM + 2
       IF ( NPAR .EQ. NMAX )     GO TO   1140
       IF ( TN .GE. URMAS(10 + NUM*2+1))  THEN
        TIME0 = TIME0 + 1.
        NUM = NUM + 1
        ENDIF
1140   CONTINUE
        UYPR = URMAS(10 + 2*NUM )
       UYPR1 = UYPR               
C---> �᫨ ᨣ��� �ࠢ����� ����� ��� ���୮�� ����殮���  �४�⨬
C---> ������ �� ������ .
       IF(UYPR1 .LT. 15. .AND. UYPR1 .GT. (-15.))  GO TO  1150
C---> ����塞 �� ��室�� ���祭��       
       TIME1   = 0.0
       TIME2   = 0.0
       TIME3   = 0.0
       TIME4   = 0.0
       TIME5   = 0.0
       TIME6   = 0.0
       GOTO 2030
1150   CONTINUE
C----------------------------------------------------------------------
C--->  KAKO� BEHT��� PA�OTAET?
C----------------------------------------------------------------------
       C = UYPR1 - UYPR
C----------------------------------------------------------------------
C
C           BEHT��� �A��  "A"
C
C----------------------------------------------------------------------
C--->  BEHT��� KATO�HO� �P����.
C----------------------------------------------------------------------
       HXAK = -1.
       A = UOPAB1 - UOPAB
       IF( A .GT. 0 ) GO TO 100
       IF( A .EQ. C ) GO TO 100
       HXAK = H0 * (UYPR - UOPAB)/(A-C)
       IF( HXAK .LT. 0.0  .OR.  HXAK .GT. H0 ) HXAK = -1.
C----------------------------------------------------------------------
C-->  �OPM�P�EM B�XO�HO� C��HA�!
100    CONTINUE
       IF( HXAK .GE. 0.0 ) GO TO 2004
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME1 .GT. 0.0 ) GO TO 2001
C->  C��HA�A HE ���O � HET
       DGMAS(1) = 0.0
       GO TO 2005
2001   CONTINUE
C->  C��HA� ��� � ���ET
       DGMAS(1) = 11.
       GO TO 2005
2004   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(1) = 1.
       DGMAS(7) = HXAK
       TIME1 = TN + HXAK
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(3) = 10.
       DGMAS(9) = HXAK
       TIME3 = 0.0
C->    � ����� �� ...
       DGMAS(5) = 11.
       DGMAS(11) = 0.0
       go to 2030
2005   CONTINUE
C----------------------------------------------------------------------
C--->  BEHT��� AHO�HO� �P����.
C----------------------------------------------------------------------
       HXAA = -1.
       A = UOPAB - UOPAB1
       IF( A .GT. 0 ) GO TO 200
       IF( A .EQ. C ) GO TO 200
       HXAA = H0 * (UYPR + UOPAB)/(A-C)
       IF( HXAA .LT. 0.0  .OR.  HXAA .GT. H0 ) HXAA = -1.
C----------------------------------------------------------------------
C-->  �OPM�P�EM B�XO�HO� C��HA�!
200    CONTINUE
       IF( HXAA .GE. 0.0 ) GO TO 2009
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME4 .GT. 0.0 ) GO TO 2006
C->  C��HA�A HE ���O � HET
       DGMAS(4) = 0.0
       GO TO 2010
2006   CONTINUE
C->  C��HA� ��� � ���ET
       DGMAS(4) = 11.
       GO TO 2010
2009   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(4) = 1.
       DGMAS(10) = HXAA
       TIME4 = TN + HXAA
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(6) = 10.
       DGMAS(12) = HXAA
       TIME6 = 0.0
C->    � ����� �� ...
       DGMAS(2) = 11.
       DGMAS(8) = 0.0
       go to 2030
2010   CONTINUE
C----------------------------------------------------------------------
C
C           BEHT��� �A��  "B"
C
C----------------------------------------------------------------------
C--->  BEHT��� KATO�HO� �P����.
C----------------------------------------------------------------------
       HXBK = -1.
       A = UOPBC1 - UOPBC
       IF( A .GT. 0 ) GO TO 300
       IF( A .EQ. C ) GO TO 300
       HXBK = H0 * (UYPR - UOPBC)/(A-C)
       IF( HXBK .LT. 0.0  .OR.  HXBK .GT. H0 ) HXBK = -1.
C----------------------------------------------------------------------
C-->  �OPM�P�EM B�XO�HO� C��HA�!
300    CONTINUE
       IF( HXBK .GE. 0.0 ) GO TO 2014
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME2 .GT. 0.0 ) GO TO 2011
C->  C��HA�A HE ���O � HET
       DGMAS(2) = 0.0
       GO TO 2015
2011   CONTINUE
C->  C��HA� ��� � ���ET
       DGMAS(2) = 11.
       GO TO 2015
2014   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(2) = 1.
       DGMAS(8) = HXBK
       TIME2 = TN + HXBK
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(1) = 10.
       DGMAS(7) = HXBK
       TIME1 = 0.0
C->    � ����� �� ...
       DGMAS(6) = 11.
       DGMAS(12) = 0.0
       go to 2030
2015   CONTINUE
C----------------------------------------------------------------------
C--->  BEHT��� AHO�HO� �P����.
C----------------------------------------------------------------------
       HXBA = -1.
       A = UOPBC - UOPBC1
       IF( A .GT. 0 ) GO TO 400
       IF( A .EQ. C ) GO TO 400
       HXBA = H0 * (UYPR + UOPBC)/(A-C)
       IF( HXBA .LT. 0.0  .OR.  HXBA .GT. H0 ) HXBA = -1.
C----------------------------------------------------------------------
C-->  �OPM�P�EM B�XO�HO� C��HA�!
400    CONTINUE
       IF( HXBA .GE. 0.0 ) GO TO 2019
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME5 .GT. 0.0 ) GO TO 2016
C->  C��HA�A HE ���O � HET
       DGMAS(5) = 0.0
       GO TO 2020
2016   CONTINUE
C->  C��HA� ��� � ���ET
       DGMAS(5) = 11.
       GO TO 2020
2019   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(5) = 1.
       DGMAS(11) = HXBA
       TIME5 = TN + HXBA
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(4) = 10.
       DGMAS(10) = HXBA
       TIME4 = 0.0
C->    � ����� �� ...
       DGMAS(3) = 11.
       DGMAS(9) = 0.0
       go to 2030
2020   CONTINUE
C----------------------------------------------------------------------
C
C           BEHT��� �A��  "C"
C
C----------------------------------------------------------------------
C--->  BEHT��� KATO�HO� �P����.
C----------------------------------------------------------------------
       HXCK = -1.
       A = UOPCA1 - UOPCA
       IF( A .GT. 0 ) GO TO 500
       IF( A .EQ. C ) GO TO 500
       HXCK = H0 * (UYPR - UOPCA)/(A-C)
       IF( HXCK .LT. 0.0  .OR.  HXCK .GT. H0 ) HXCK = -1.
C----------------------------------------------------------------------
C-->  �OPM�P�EM B�XO�HO� C��HA�!
500    CONTINUE
       IF( HXCK .GE. 0.0 ) GO TO 2024
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME3 .GT. 0.0 ) GO TO 2021
C->  C��HA�A HE ���O � HET
       DGMAS(3) = 0.0
       GO TO 2025
2021   CONTINUE
C->  C��HA� ��� � ���ET
       DGMAS(3) = 11.
       GO TO 2025
2024   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(3) = 1.
       DGMAS(9) = HXCK
       TIME3 = TN + HXCK
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(2) = 10.
       DGMAS(8) = HXCK
       TIME2 = 0.0
C->    � ����� �� ...
       DGMAS(4) = 11.
       DGMAS(10) = 0.0
       go to 2030
2025   CONTINUE
C----------------------------------------------------------------------
C--->  BEHT��� AHO�HO� �P����.
C----------------------------------------------------------------------
       HXCA = -1.
       A = UOPCA - UOPCA1
       IF( A .GT. 0 ) GO TO 600
       IF( A .EQ. C ) GO TO 600
       HXCA = H0 * (UYPR + UOPCA)/(A-C)
       IF( HXCA .LT. 0.0  .OR.  HXCA .GT. H0 ) HXCA = -1.
C----------------------------------------------------------------------
C-->  �OPM�P�EM B�XO�HO� C��HA�!
600    CONTINUE
       IF( HXCA .GE. 0.0 ) GO TO 2029
C->  HA �A�E �EPE�HE�O �POHTA HA �AHHOM BEHT��E HET.
       IF( TIME6 .GT. 0.0 ) GO TO 2026
C->  C��HA�A HE ���O � HET
       DGMAS(6) = 0.0
       GO TO 2030
2026   CONTINUE
C->  C��HA� ��� � ���ET
       DGMAS(6) = 11.
       GO TO 2030
2029   CONTINUE
C->  C��HA�A HE ���O, TE�EP� ���ET
       DGMAS(6) = 1.
       DGMAS(12) = HXCA
       TIME6 = TN + HXCA
C->  C��HA� ���, TE�EP� HE ���ET
       DGMAS(5) = 10.
       DGMAS(11) = HXCA
       TIME5 = 0.0
2030   CONTINUE
C----------------------------------------------------------------------
C-->  A KAK HA C�ET "�E�TOKOBO� �A���"?
C----------------------------------------------------------------------
C      PRZV = TOKNG * TOKNG1
C      HXTOK = 0.0
C      IF( PRZV .GE. 0.0 ) GO TO 3000
C---> �A��A.
C      HXTOK = H0 * TOKNG / (TOKNG - TOKNG1)
C      IF( TIME0 .GT. 0.0  ) GO TO 3000
C      TIME0 = TN + HXTOK
C3000   CONTINUE
C       IF( TN .GE. (TIME0+TIMEP) ) GO TO 9999
C--> �A��A => O�H���MC�
C       DO 4000 I=1,12
C       DGMAS(I) = 0.0
C4000   CONTINUE
C--> �A�OMH�MC�.
C9999   CONTINUE
       URMAS(2) = TIME1
       URMAS(3) = TIME2
       URMAS(4) = TIME3
       URMAS(5) = TIME4
       URMAS(6) = TIME5
       URMAS(7) = TIME6
       URMAS(8)=  TIME0
C      if ( TN .gt. 0.0 ) return
C      DO 10123 I= 1,6
C      DGMAS(I)  = 11.
C      DGMAS(I+6)= 0.0
C0123  CONTINUE
C      time1 = h0/10.
C      time2 = time1
C      time3 = time1
C      time4 = time1
C      time5 = time1
C      time6 = time1
       RETURN
C----------------------------------------------------------------------
       END
