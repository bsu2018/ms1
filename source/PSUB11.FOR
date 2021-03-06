        SUBROUTINE PSUB11( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT A�PE��  1988 �.)------------
C   �O��PO�PAMMA: PSUB11( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HA�HA�EH�E:  �/� �O���OBATE�� ��� MO�E��POBAH��
C   ----------   HE��HE�H�X ��EMEHTOB
C
C                  ( MO�E�� #17 )
C   BAP�AHT�:    BO�MO�H� �BA BAP�AHTA MO�E��:
C   ---------  1.�P��HAK MO�E�� = 0, MO�E�� ��HE�HA�,
C                �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C                �HA�EH��M TOKA  � HA�P��EH�� HE��HE�HO�O ��EMEHTA
C                �O MOMEHTA BPEMEH� TN BK����TE��HO B �O��PO�PAMME
C                O�PE�E���TC� �HA�EH�� G,J-�APAMETPOB MO�E�� ��� T=TN+1
C              2.�P��HAK MO�E�� = 1, MO�E�� �TEPA��OHHA�,
C                �O BE����HAM, �EPE�ABAEM�M �O���OBATE�EM �
C                �HA�EH��M TOKA  � HA�P��EH�� HE��HE�HO�O ��EMEHTA
C                �O MOMEHTA BPEMEH� TN+1 BK����TE��HO B �O��PO�PAMME
C                O�PE�E���TC� �HA�EH�� G,J-�APAMETPOB B MOMEHT TN+1
C                �TEPA��OHH�M C�OCO�OM
C   METO�:     O�EB��EH.
C   -----
C  �APAMETP�:   NEL - HOMEP ��EMEHTA, ��� KOTOPO�O PSUB PA�OTAET;
C  ---------    URMAS - MACC�B �AHH�X �O���OBATE��:
C                       1-E C�OBO - KO���ECTBO �APAMETPOB B C��CKE
C                       ( �A��CAHO �E��M ��C�OM B BE�ECTBEHH�� MACC�B);
C                       2-E C�OBO - �P��HAK MO�E�� ( CM.BAP�AHT� );
C                       3-E C�OBO - MOMEHT BK���EH�� MO�E��  T0;
C                       4-E C�OBO - KO� B��A HE��HE�HOCT� ( K )
C                       5-E C�OBO � �A�EE  - KOHCTAHT� AHA��T��ECK�X
C                           B�PA�EH��, O�PE�E��EM�X KO�OM ( K )
C----------------------------------------------------------------------
C              STMAS - MACC�B �AHH�X, �EPE�ABAEM�X B PSUB:
C
C      C�OBO           O�O�HA�EH�E   -   �HA�EH�E
C      1          TN - BPEM� N - �O �A�A PAC�ETA;
C      2          H0 - �A� �HTE�P�POBAH��;
C      3          I  - �HA�EH�E TOKA HE��HE�HO�O ��EMEHTA �ATPE�O-
C                      BAHHO�O  �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C      4          U  - �HA�EH�E HA�P��EH�� HE��HE�HO�O ��EMEHTA, �ATPE-
C                      �OBAHHO�O �O���OBATE�EM, ��� MOMEHTA BPEMEH� TN;
C----------------------------------------------------------------------
C              DGMAS(1) - �P�XO��T HOMEP �A�POCA,
C                         EC�� DGMAS(1)=1, TO �TO �EPB�� BXO� B
C                         �O��PO�PAMM� PSUB HA �AHHOM �A�E,
C                         EC�� DGMAS(1)=2, TO �TO HE �EPBA� �TEPA���
C                         B�XO��T �P��HAK MO�E��
C              DGMAS(2) - �P�XO��T �HA�EH�E TOKA MO�E�� B MOMEHT TN+1,
C                         EC�� HOMEP �A�POCA = 1 ( DGMAS(1) ), TO
C                         �TO �HA�EH�E TOKA MO�E��, B���C�EHHOE HA
C                         �PE�����EM �A�E,
C                         EC�� HOMEP �A�POCA = 2 ( DGMAS(1) ), TO
C                         �TO �HA�EH�E TOKA MO�E��, B���C�EHHOE HA
C                         �PE�����E� �TEPA��� �AHHO�O �A�A
C                         B�XO��T  J-�APAMETP (�CT.TOKA C�HTET��.CXEM�)
C              DGMAS(3) - G-�APAMETP (�POBO��MOCT� C�HTET��ECKO� CXEM�)
C              DGMAS(4) - = 0.
C              DGMAS(5) - = 0.       P  E  �  E  P  B
C
C----------------------------------------------------------------------
C              INTMAS - MACC�B �� �B�XCOT (200) ��EMEHTOB,
C                       B �O��PO�PAMME  H E   � C � O � � � � E T C �
C                                       ===============================
C----------------------------------------------------------------------
C
C   �O��PO�PAMM�: HET
C   -------------
C
C----------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( NWTR ,MASPRT(13) )
       EQUIVALENCE ( NWTRE,MASPRT(14) )
       EQUIVALENCE ( TMAX ,MASPRT(43) )
C
C
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
       I = STMAS(3)
       U = STMAS(4)
       T0 = URMAS(3)
       IF(URMAS(2).EQ.0.AND.DGMAS(1).NE.1) RETURN
       DGMAS(1) = URMAS(2)
       DGMAS(4) = 0.
       DGMAS(5) = 0.
       IF(TN.LT.T0) GO TO 102
C----------------------------------------------------------------------
       K=URMAS(4)
C--->  K - HOMEP B��A HE��HE�HOCT�
C======================================================================
C--> K=1        I = A * U**3
C          A - KOHCTAHTA, BBO��TC� URMAS(5)
C
C
C    B C��CKE �APAMETPOB PSUB  �OC�E CTAH�APTH�X �APAMETPOB C�E���T:
C    KOHCTAHT� AHA��T��ECK�X B�PA�EH�� ��� HE��HE�H�X ��EMEHTOB
C
C----------------------------------------------------------------------
      PI=3.141593
C======================================================================
C     �A��C� AHA��T��ECK�X B�PA�EH�� ��� HE��HE�H�X ��EMEHTOB
 101  GO TO (1),K
C----------------------------------------------------------------------
   1  A = URMAS(5)
      DGMAS(2) = 0.
      DGMAS(3) = A * U**2
C----------------------------------------------------------------------
      RETURN
 102  DGMAS(2) = 1.E-6
      DGMAS(3) = 1.E-6
      RETURN
      END
