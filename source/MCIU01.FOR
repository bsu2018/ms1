       INTEGER FUNCTION MCIU01( TN, H0, HX, TMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA_��HK��� MCIU01( TN, H0, HX, TMAS )
C   ---------------------
C   HA�HA�EH�E:        "�EHEPATOP" � "BEP���KATOP" �M����COB ��PAB�EH��
C  -----------         C�� ��� MO�E�� ��PAB. BEHT��� � ��EA��H�� K���.
C                         BAP�AHT # 01:
C                         -------------
C
C
C
C                         I<-HX->I        :<-HX->:
C                         $      $        :      $
C                       **********        :      ******
C                       **********        :      ******
C                       **********        :      ******
C  T0*--------I---------**********-----I--$------******----------------
C    I        I         I        I     I         I
C    I<-TZAD->I         I<-TIMP->I     I         I
C    I        I<-TALFA->I        I     I<-TALFA->I
C             I                        I
C             I<---------TCIU--------->I<-----------TCIU--------
C
C  B O��CAH�� �E�� �C�O����ETC� C�E�. �A��C�:
C
C   ... MAS < 1 TZAD  TCIU  TALFA  TIMP  TKCIU  DTALFA >
C
C  BO�MO�HO KO��POBAH�E:
C
C   ... MAS < 0 >, �TO COOTBETCTB�ET:
C
C       TZAD = 360 ��. �PA�.
C       TCIU = 60  ��. �PA�.
C       TAFLA= 30  ��. �PA�.
C       TIMP = 10  ��. �PA�.
C       TKCIU=  0  ��. �PA�.
C       DTALFA= 0  ��. �PA�.
C
C      ��EC� �C�O���OBAH� C�E�. O�O�HA�EH��:
C
C
C      TZAD  - BPEM� �A�EP�K� BK���EH�� "C��"    ( ��. �PA�. )
C      TCIU  - �EP�O� �OBTOPEH�� C��HA�OB "C��"  ( ��. �PA�. )
C      TALFA - �A�EP�KA �EPE�HE�O �POHTA C��HA�A
C              "C��" OT HA�A�A O�EPE�HO�O �EP�O�A
C              �OBTOPEH��                          ( ��. �PA�. )
C      TIMP - ���TE��HOCT� �M����CA "C��"          ( ��. �PA�. )
C      TKCIU - �EPE� CKO��KO �EP�O�OB �OBTOPEH��
C              C��HA�A "C��" ��MEH�T� TALFA HA
C      DTALFA - E��H�� - �A� ��MEHEH�� TALFA       ( ��. �PA�. )
C
C              - B C���AE, KO��A TEK��EE �HA�EH�E TALFA - "�A�EP�KA
C                �EPE�HE�O �POHTA C��" CTAHET �O���E TCIU - "�EP�O�A
C                �OBTOPEH�� C��" ( �TO BO�MO�HO �P� TKCIU � DTALFA
C                HE PABH�M� H��� ), �POBO��TC� #CTA�����A���#
C                                               ------------
C                �M����CA "C��":
C                ---------------
C                                            ******
C                                            ******
C                                            ******
C                                            ******
C   *----------------------*-----------------******--------------------
C   I(TGO)                 I(TGO+TCIU)       I
C   I                      I                 I
C   I                      I                 I
C   I<-------( TGO + TALFA<TEK��EE> )------->I
C   I                      I                 I
C------------<HOBOE TZAD>->I                 I
C                          I<-<HOBOE TALFA>->I
C                          I     PABHOE      I
C                              <�A�AHHOE>
C                              < TALFA  >
C
C
C              TGO - BPEM� HA�A�A O�EPE�HO�O �EP�O�A C��, ��� KOTOPO�O
C                    �A��KC�POBAHA C�T�A��� TALFA<TEK��EE> >= TCIU
C              TALFA = TALFA<�A�AHHOE> + [ NP/KCIU ] * DTALFA -
C                   TEK��EE �HA�EH�E "�A�AP�KA �EPE�HE�O �POHTA";
C              NP = [ (TN-TZAD) / TCIU ] - HOMEP �EP�O�A "C��"
C                   ( �P� NP < 0, NP = 0 );
C
C              #CTA�����A���# �AK���AET B C�E����EM:
C
C               TZAD <HOBOE>    = TGO + TCIU
C
C               TALFA < HOBOE > = TALFA <�A�AHHOE>
C
C               NP < HOBOE >    = 0 ( C�PAC�BAETC�).
C
C               #�EHEP�P�ETC� COO��EH�E HA NWTRE KAHA�....#
C
C
C
C
C   METO�:     "BOCTAHOB�EH�E" ��HK��� "C��"(T) � KOHTPO��
C   -----      HA����� "C��" �P� T = TN;
C   �APAMETP�: TN - BPEM� O�PE�E�EH�� HA����� �M����CA "C��";
C   ---------  H0 - "OKPECHOCT�" O�PE�E�EH�� C��
C              HX - "�A�" OT TN �O �POHTA �M����CA "C��"(CM. P�C.)
C
C
C              TMAS - "OTPE�OK" MACC�BA MSWALL - O��CAH�E "C��":
C
C  +------+------+-------+------+-------+--------+
C  I TZAD I TCIU I TALFA I TIMP I TKCIU I DTALFA I
C  +------+------+-------+------+-------+--------+
C  I  1   I  2   I   3   I  4   I   5   I    6   I
C  +------+------+-------+------+-------+--------+
C
C  �HA�EH�E ��HK���:  MCIU01 = 00  --> "C��" HET B [TN, TN+H0];
C  ****************                     HX = 0.0;
C                     MCIU01 = 01  --> B [TN, TN+H0] -> �EPE�H�� �POHT
C                                      �M����CA "C��", HX = �HTEPBA� OT
C                                      TN �O �EPE�HE�O �POHTA "C��";
C                     MCIU01 = 10 -->  B [TN, TN+H0] -> �A�H�� �POHT
C                                      �M����CA "C��", HX = �HTEPBA�
C                                      OT TN �O �A�HE�O �POHTA �M����CA;
C                     MCIU01 = 11 -->  B [TN, TN+H0] "C��" BK���EH,
C                                      COCTO�H�E HE MEH��, HX=0.0;
C
C                !!!!!  �P� H0 > TIMP --->  BO�MO�HA �OPEP� "C��" !!!!!
C                      �PE��O�O�AETC�, �TO H0 < TIMP
C
C
C   �O��PO�PAMM�:      PROF1
C   -------------
C
C   �P�ME�AH�E:        1. KOHTPO�� HE �POBO��TC�;
C  -----------         2. �M����C ECT�, EC��:
C                         .... <= TN <= .....
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       DIMENSION TMAS(*)
100     CONTINUE
       MCIU01 = 0
         HX = 0.0
C-->  BOCTAHOB�M �M����C....
C  NP - HOMEP �EP�O�A "C��", TGO -  BPEM� HA�A�A �EP�O�A "C��",
C        !!! BOCTAHOB�EH�E �O TN !!!
C
         TZAD = TMAS(1)
         TCIU = TMAS(2)
         TALFA = TMAS(3)
         TIMP = TMAS(4)
         KCIU = IFIX( TMAS(5) )
         DTALFA = TMAS(6)
C---->
         RNP = 0.0
         NP = 0
         T1 = TN - TZAD
         IF ( T1 .LT. 0.0 ) GO TO 1
         RNP = T1/ TCIU
         NP = INT( RNP )
         RNP = FLOAT( NP )
1        CONTINUE
C-->  BPEM� HA�A�A TEK. "C��"
         TGO = TZAD + RNP * TCIU
C-->  CME�EH�E BH�TP� �EP�O�A "C��" ..
         IF ( KCIU .EQ. 0  .OR.  DTALFA .EQ. 0.0 ) GO TO 2
C-->  TALFA ��MEH��AC�...
         IALFA = NP / KCIU
         TALFA = TALFA + FLOAT(IALFA) * DTALFA
C----------------------------------------------------------------------
C-->  HE H��HO �� CTA������POBAT� "C��" ?
        IF ( TALFA .LT. TCIU ) GO TO 2
C--->  CTA�����A���...
        TMAS(1) = TGO + TCIU
C---->
        WRITE(NWTRE, 3 ) TN, TGO, TMAS(1)
3      FORMAT(' *** �P� TN=', G12.5, ' B��O�HEHA "CTA�����A���"',
     =        ' C��HA�A "C��" ***'/
     =        ' *** TGO = ', G12.5, ', HOBOE BPEM� TZAD = ', G12.5,
     =        ' ***' )
       GO TO 100
2        CONTINUE
C--->  TI1 - �EPE�H�� �POHT, TI2 - �A�H�� �POHT...
         TI1 = TGO + TALFA
         TI2 = TI1 + TIMP
C--->  KAKO� �HTEPBA�...
C  M1 - �HA�EH�E "C��" ��� TN,
C  M2 - �HA�EH�E "C��" ��� TN + H0;
         M1 = 0
         M2 = 0
         TN1 = TN + H0
         IF ( TI1 .LE. TN  .AND.  TN .LT. TI2  )  M1 = 1
         IF ( TI1 .LE. TN1  .AND.  TN1 .LE. TI2  )  M2 = 1
C--->   M1 = 0 � M2 = 0  -->  �M����CA HET...
         IF ( M1 .EQ. 0  .AND.  M2 .EQ. 0 ) RETURN
C--> EC�M M1 � M2 --> 1,  �M����C HE "KOH���"....
         MCIU01 = 11
         IF ( M1 .EQ. 1  .AND.  M2 .EQ. 1 ) RETURN
C-->  �O��CT�M, �TO M1 = 0,  -->  �EPE�H�� �POHT...?
         MCIU01 = 01
         HX = TI1 - TN
C-->  TAK �� �TO?
         IF ( M1 .EQ. 0 ) RETURN
C-->  HE TAK....
         MCIU01 = 10
         HX = TI2 - TN
         RETURN
         END
