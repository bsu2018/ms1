         SUBROUTINE APFTEM ( KAR, KSIM, SIM, RSIM, ER, IER )
CXXX ---KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C      �O��PO�PAMMA: APFTEM ( KAR, KSIM, SIM, RSIM, ER, IER )
C      -------------
C      HA�HA�EH�E:   HA�A��HOE CKAH�POBAH�E TEKCTA � �PE�BAP�TE��HOE
C      -----------    B��E�EH�E TEPMOB � ��C�OB�X KOHCTAHT �E� �HAKA
C                     �� MACC�BA KAR ( A-KO� ) � �OPM�POBAH�E MACC�BA
C                     SIM ( F-KO� ) �O �O���EHH�M TEPMAM �� KAR;
C                     ��KCA��� �P���X O���OK B O��CAH�� B�PA�EH��.
C
C      METO�:        �O�CK, B��E�EH�E AP��M. B�PA�EH�� � �AKOBKA
C      -----         ��EHT���KATOPOB B SIM. �P� �TOM:
C                    "C���. C�MBO��" - �PE�CTAB�EH� CBO�M� F-KO�AM�, HO
C                                      C O�PATH�M� �HAKAM�, T.E. < 0;
C                    "��EHT���KATOP�" - �PE�CTAB�EH� O�HO� �O����E� B
C                                       SIM B C�ATOM F-KO�E. �P� �TOM
C                                       �C�O�����TC� �EPB�E �ET�PE
C                                       C�MBO�A, OCTA��H�E ��HOP�P��T-
C                                       C�.  �HA�EH�E BCE��A > 0.
C                    "��C�OB�E
C                     KOHCTAHT�" - �PE�CTAB�EH� B MACC�BE SIM
C                                  �B�M� �OC�E�OBATE��H�M� ��C�AM�
C                                  H��EM � �HA�EH�EM KOHCTAHT� B
C                                  �PE�CTAB�EH�� REAL*4.
C                                  T.E. B HA�A�E 0, �ATEM �A�EH�E > 0.
C
C      �APAMETP�:  KAR - MACC�B CTPOK� B A-KO�E ( PA�MEPHOCT� >= 72 ),
C      ----------        CO�EP�A��� �CXO�H�� ��� PA��OPA TEKCT.
C                         T�� INTEGER*4;
C                   KSIM - KO���ECTBO �AH�T�X �O����� B MACC�BE SIM,
C                          ( <= 100 ), INTEGER*4;
C                   SIM - MACC�B TEPMOB B F-KO�E ( PA�MEPHOCT� >=100 ).
C                          CM. METO�, INTEGER*4 + REAL*4;
C                   RSIM - TOT �E MACC�B �TO � SIM, HO O��CAH��
C                           B B���BA��E� �PO�PAMME KAK REAL*4;
C                           ��� �P� B��OBE APFTEM �KA�AT�:
C                           CALL APFTEM ( KAR, KSIM, SIM, SIM, ER,IER)
C                                                         ----
C
C                   ER  -  MACC�B PA�METK� O���OK ( 80 C�MBO�OB )
C                           B A - KO�E; INTEGER*4.
C                   IER - KO���ECTBO O���OK; �P� = 0 B MACC�BE ER
C                         HAXO��TC� �PO�E�� B A-KO�E;
C                           T��: INTEGER*4;
C      �O��PO�PAMM�: APFTRT, APFSQE, APFPAK; ( COMMON PRINTN );
C      ------------
C      �P�ME�AH�E: 1.  �� KAR B���PAETC� K72 = 72 �EPB�X C�MBO�OB;
C      ----------- 2.   B C E      �PO�E�� ��HOP�P��TC�;
C                       *****
CZZZ ------------------------------------------------------------------
C!!!-------------------------------------------------------------------
       COMMON /PRINTN/ NW6
C!!!-------------------------------------------------------------------
       DIMENSION KAR(1), SIM(1), RSIM(1), ER(1), FCAR(4)
       INTEGER KAR, SIM, ER, FLAG, FCAR
C------EQUIVALENCE ( RSIM(1),  SIM(1) )--------------------------------
       DATA K72 / 72 /
C �EPE��EM B F-KO�:
       CALL APFTRT ( 1, K72, KAR, KAR )
C �H���A���A���:
       IK=1
       IS=1
       FLAG=0
       IER=0
C B ER �PO�E�� B F=KO�E:
       DO 100 J=1,80
100    ER(J)=11
C   CKAH�P�EM KAR, B���PAEM:
2      CONTINUE
C   �PO�E�� ? --> 30  *** 01.02.86 ***
       IF ( KAR(IK) .EQ. 11 ) GO TO 30
C  C���E�H�E �HAK� ?  ---> 4
       IF ( KAR(IK) .GE. 12  .AND. KAR(IK)  .LE. 26 ) GO TO 4
C  �H��B���A��HO, <.> � <E> ? ---> 5  / <E> ��OK�POBAHO /
       IF ( KAR(IK) .EQ. 27                        ) GO TO 5
C  ���P� ? ---> 6
       IF ( KAR(IK) .GE. 1  .AND. KAR(IK) .LE. 10 ) GO TO 6
C  ��KB� ? ---> 7
       IF ( KAR(IK) .GE. 35  .AND. KAR(IK) .LE.  83 ) GO TO 7
C====== �O�O�H�TE��H�E C���E�H�E C�MBO�� ---> 4  / 01.02.86 /
       IF ( KAR(IK) .GE. 28  .AND. KAR(IK) .LE. 34 ) GO TO 4
C  �HA�E �BHA� O���KA, O�PA�OTAT� EE:
8      IER = IER + 1
C  �A��CAT� "�-�" $
       ER(IK) = 37
       FLAG = 0
       GO TO 3
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C  O���AETC� ���PA, ��� �PO�O��EH�E ��EHT���KATOPA
6      CONTINUE
       IF ( FLAG .EQ. 1 ) GO TO 7
C  <.>, <E>, <���PA> - �EPEKO��POBAT� KOHCTAHT�
5       CONTINUE
C   H��HO �� B�TO�KH�T� ��EHT���KATOP
       IF ( FLAG .EQ. 0 ) GO TO 9
C   B�TO�KH�T� FCAR
       FLAG = 0
C   ��AKOBAT�
       CALL APF PAK ( 1, IDKOD, FCAR )
       SIM(IS) = IDKOD
       IS = IS + 1
C   �P�H�T� KOHCTAHT�
9      N1 = IK
       N2 = K72 - IK + 1
       CALL APF SQE ( KAR, E0, N1, N2, IF1 )
       GO TO ( 11, 11,   11,   10, 10, 10 ), IF1
10     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NW6,12) IF1, N1, N2, IK, IS
12     FORMAT(' ***** C�O� B A��OP�TME APFTEM: IF1 =',I6,
     *        ' N1=',I6,' N2=',I6, ' IK=',I3,' IS=',I4)
C!!!-------------------------------------------------------------------
       CALL SUBERR(1)
C====>
C====>
C====>
C====>
C====>
C  �A��CAT� KOHCTAHT�
11     SIM(IS) = 0
       RSIM(IS + 1) = E0
       IS = IS + 2
C  KOPPEKT�P�EM C.�.K. IK � �PO�O��AEM
       IK = N1 - 1
       GO TO ( 3, 3, 8 ), IF1
C!!!-------------------------------------------------------------------
       GO TO 10
C!!!-------------------------------------------------------------------
C  �P�H�T� C���E�H�� �HAK
4      CONTINUE
C   B�TO�KH�T� FCAR, EC�� HEO�XO��MO
       IF ( FLAG .EQ. 0 ) GO TO 13
       FLAG = 0
       CALL APF PAK ( 1, IDKOD, FCAR )
       SIM(IS) = IDKOD
       IS = IS + 1
C
13     SIM(IS) = -KAR(IK)
       IS = IS + 1
       GO TO 3
C   O�PA�OTKA ��EHT���KATOPA
7      CONTINUE
       IF ( FLAG .EQ. 1 ) GO TO 14
C  �EPB�� BXO�: HA�A�O ��EHT���KATOPA
       FLAG = 1
       IC = 1
       FCAR(1) = 11
       FCAR(2) = 11
       FCAR(3) = 11
       FCAR(4) = 11
       GO TO 15
C  ��HOP�POBAT� ���HH�� K�COK ��EHT���KATOPA ( > 4 )
14     CONTINUE
       IF ( IC .GT. 4 ) GO TO 3
C �A��CAT� C�MBO�
15     FCAR(IC) = KAR(IK)
       IC =IC + 1
       GO TO 3
C======>  O��T� �PO�E��, HE KOHE� �� �TO ��EHT���KATOPA ? /01.02.86/
30      CONTINUE
         IF ( FLAG .EQ. 0 ) GO TO 3
         FLAG = 0
         CALL APF PAK (1, IDKOD, FCAR)
         SIM(IS) = IDKOD
         IS = IS + 1
         GO TO 3
C----------------------------------------------------------------------
C   KOH�OBKA O��E�O ��K�A  ********************************************
C----------------------------------------------------------------------
3      CONTINUE
       IK = IK + 1
       IF ( IK .LE. K72 ) GO TO 2
       IF ( FLAG .EQ. 0 ) GO TO 16
C  B KOH�E B�TO�KHEM FCAR
       FLAG = 0
       CALL APF PAK ( 1, IDKOD, FCAR )
       SIM(IS) = IDKOD
       IS = IS + 1
C  KOH�AEM
16     KSIM = IS - 1
       IF ( KSIM .LT. 0 ) KSIM = 0
C  �EPE��EM K A-KOD�
       CALL APF TRT ( 2, K72, KAR, KAR )
       CALL APF TRT ( 2,  80, ER, ER )
C!!!-------------------------------------------------------------------
       IF ( KSIM. GT. 100 ) WRITE ( NW6,17) KSIM
17     FORMAT(' ***** C�O� B APFTEM; KSIM=',I10 )
C!!!-------------------------------------------------------------------
       RETURN
       END
