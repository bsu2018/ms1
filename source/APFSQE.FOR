         SUBROUTINE APFSQE  ( KOD, E0, N1, N2, IF )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)----------
C    �O��PO�PAMMA    APFSQE(KOD,E0,N1,N2,IF)
C
C      HA�HA�EH�E: �EPEBO� ��C�A, �A��CAHHO�O ���POB�M� C�MBO�AM� B
C      ----------  �EC�T��HOE ��C�O C ��ABA��E� TO�KO� ( REAL*4 );
C
C      METO�:      �O��EMEHTHOE �PEOPA�OBAH�E (C�EBA HA�PABO) �CXO�H�X
C      -----       ���POB�X �AHH�X, �PE�CTAB�����X BHE�H�� �OPM� �A��C�
C                  �PEO�PA��EMO�O ��C�A;
C
C      �APAMETP�:  KOD - MACC�B �AHH�X ��� �PEO�PA�OBAH�� ( B F-KO�E.);
C      ---------   E0  - �PEO�PA�OBAHHOE �EC�T��HOE ��C�O ( REAL*4 );
C                  N1  - HOMEP ��EMEHTA B MACC�BE KOD HA�A�A �H�OPMA���
C                         �O��E�A�E� �PEO�PA�OBAH��;
C                  N2  - KO���ECTBO ��EMEHTOB �OCT��H�X ���
C                         �PEO�PA�OBAH��;
C                  IF  - �H��KATOP �PEO�PA�OBAH��:
C                      IF = 1  - �P� ��A�O�O���HOM �PEO�PA�OBAH��:
C                                �P� �TOM: N1 = N1 + N2; N2 = 0;
C
C                      IF = 2  MACC�B KOD HE KOH���C�, O�HAP��EH C�MBO�
C                              "HE ���PA",B N1 - HOMEP B MACC�BE KOD,
C                               COOTBETCTB����� O�HAP��EHHOM� HE ���-
C                               POBOM� C�MBO��; B N2 - ���HA ( KO�-BO
C                               ��EMEHTOB B MACC�BE KOD ) OCTAB�E�C�
C                               �ACT� - �E� C�MBO�A "HE ���PA" -
C                                B MACC�BE KOD;
C                               E0 - �PEO�PA�OBAHHOE ��C�O, �PE�CTAB�EH-
C                               HOE ���PAM� OT N1 �O C�MBO�A "HE ���PA";
C                     IF = 3 - �P� �PEO�PA�OBAH�� ��C�A �PO��O��O
C                              �EPE�O�HEH�E; �P� �TOM CTPOKA ( MACC�B
C                              KOD ) �PO�O��AET �POCMATP�BAT�C�.
C                         A)  �P� BCTPE�E C�MBO�A "HE ���PA" -
C                             N1 - A�PEC "HE ���PA" ( HOMEP B KOD ), A
C                              N2 - ���HA OCTAB�E�C� �ACT�, �E� C�MBO�A
C                                "HE ���PA";
C                         �)  EC�� C�T�A��� "HE ���PA" HE �A��KC�POBAHA,
C                             A �O�E ( MACC�B KOD ) �C�EP�AHO, TO:
C                              N1=N1+N2, N2=0.
C                          BO BCEX C���A�X IF=3, �P�H�MAETC� E0=MAX/MIN
C                          ( B �AB�C�MOCT� OT �HAKA �OP��KA ) �HA�EH��
C                          REAL*4 B �PE�CTAB�EH�� HA �C�O����EMO� �BM.
C
C                    IF = 4 - N2 �A�AH MEH��E H��� ( N2<0 ), �P� �TOM
C                          E0=0, N1 � N2 �E� ��MEHEH��
C                    IF = 5 - N2 �A�AHO PABHO� H��� ( N2=0 ), �P� �TOM
C                          E0=0, N1=N1+1, N2=0;
C                    IF = 6 - BO�H�KAET �P� �O��TKE �PEO�PA�OBAH��
C                         �O�� ( MACC�BA KOD ) COCTO��E�O �� O�H�X
C                         HE�HA�E��X �PO�E�OB ��� C�MBO�OB:
C                       <.>,<E>,<->,<+>; �P� �TOM: E0=0, N1=N1+N2, N2=0;
C
C      �O��PO�PAMM�: APFTRT;
C      ------------
C
C
C      ----------     CO��ACHO TA����E B �O��PO�PAMM� APFTRT .
C                    ( �P� �TOM <���PA> = FKOD(I) - 1 );
C
C
CZZZ -------------------------------------------------------------------
         INTEGER * 4  P, P1, TKOD, LIMITM, LIMITP
         DIMENSION KOD(1), TKOD(15)
         DATA IVV/0/, LIMITM/1677216/, LIMITP/72/
         DATA EMAX/0.99E+31/
         DATA TKOD/ '0','1','2','3','4','5','6','7','8','9',
     *             ' ', '+', '-', '.', 'E' /
         IF ( IVV .EQ. 1 ) GO TO 1
C  �EPB�� BXO�: �EPEKO��P. TKOD
         IVV=1
         CALL  APFTRT(1, 15,  TKOD,  TKOD  )
C----------------------------------------------------------------------
1        CONTINUE
         IF = 4
         IF ( N2 ) 100, 101, 2
2         NMAX = N1 + N2 -1
          IT=N1
4         CONTINUE
          IF ( KOD(IT) .NE. TKOD(11) ) GO TO 3
          IT = IT + 1
          IF ( IT - NMAX ) 4, 4, 108
3         IZM=+1
          IF ( KOD(IT) .EQ. TKOD(12) .OR.
     *         KOD(IT) .EQ. TKOD(13) .OR.
     *         KOD(IT) .EQ. TKOD(14) .OR.
     *         KOD(IT) .EQ. TKOD(15) )    GO TO 5
          GO TO 6
5         CONTINUE
          IF ( KOD(IT) .EQ. TKOD(15) ) GO TO 7
          IF ( KOD(IT) .EQ. TKOD(14) ) GO TO 8
          IF ( KOD(IT) .EQ. TKOD(13) ) IZM=-1
          IT = IT + 1
          IF ( IT - NMAX ) 6, 6, 108
7         M=1
          P=0
          P1=0
          IT = IT +1
          IF ( IT - NMAX ) 103, 103, 108
8         M=0
          P=0
          P1=0
          GO TO 104
6         M=0
          P=0
          P1=0
12        CONTINUE
          IF ( KOD(IT) . LT. TKOD(1) .OR.
     *         KOD(IT) . GT. TKOD(10)  )  GO TO  9
       IF ( M .LE. LIMITM) GO TO 10
          P = P + 1
          GO TO 11
10        M = M*10 + KOD(IT) - 1
11        IT = IT +1
          IF ( IT - NMAX ) 12, 12, 102
9         CONTINUE
          IF ( KOD(IT) .EQ. TKOD(14) ) GO TO 104
21        CONTINUE
          IF ( KOD(IT) .EQ. TKOD(15) ) GO TO 13
C$$$$$$$$$IF ( KOD(IT) .EQ. TKOD(12) .OR.
C$$$$$$$$$     KOD(IT) .EQ. TKOD(13)   )  GO TO 105
          IF = 2
          N1 = IT
          N2 = NMAX - IT
          GO TO 106
13        IT = IT + 1
          IF ( IT - NMAX ) 14, 14, 102
14        P1 = 0
C         PA��OP �OP��KA ( �HAK )
103       IZP= +1
          IF ( KOD(IT) .EQ. TKOD(12) .OR.
     *         KOD(IT) .EQ. TKOD(13)  )  GO TO  105
          GO TO 15
C         PA��OP �OP��KA  ( ��C�O )
105       CONTINUE
          IF ( KOD(IT) .EQ. TKOD(13) ) IZP= -1
          IT = IT + 1
          IF ( IT - NMAX ) 15, 15, 102
C         ��K� �O ���PAM �OP��KA
15        CONTINUE
          IF ( KOD(IT) .LT. TKOD(1) .OR.
     *         KOD(IT) .GT. TKOD(10)  )  GO TO 16
          IPP = IABS( P + IZP*P1)
          IF ( IPP - LIMITP ) 18,18,17
17        CONTINUE
C  �EPE�O�HEH�E �OP��KA, �POCMOTP �O KOH�A
       P = P + IZP*P1
53     IT= IT + 1
       IF ( IT - NMAX ) 50, 50, 51
51     N1 = N1 + N2
       N2 = 0
       GO TO 107
50     IF ( KOD(IT) .LT. TKOD(1) .OR. KOD(IT) .GT. TKOD(10) ) GO TO 52
       GO TO 53
52     N1 = IT
       N2 = NMAX - IT
          GO TO 107
C         �P�H�T� ���P� �OP��KA
18        P1 = P1*10 + KOD(IT) - 1
          IT = IT + 1
          IF ( IT - NMAX ) 15, 15, 19
19        P = P + IZP * P1
          GO TO 102
C         HE ���PA
16        P = P + IZP * P1
          IF = 2
          N1 = IT
          N2 = NMAX - IT
          GO TO 106
C         �POCMOTP �A TO�KO�
104       IT = IT + 1
          IF ( IT - NMAX ) 20, 20, 102
20        CONTINUE
          IF ( KOD(IT) .LT. TKOD(1) .OR.
     *         KOD(IT) .GT. TKOD(10)  )  GO  TO  21
          IF ( M .GT. LIMITM ) GO TO 22
          M = M*10 + KOD(IT) - 1
          P = P - 1
22        IT = IT + 1
          IF ( IT - NMAX ) 20,20, 102
C         �XO��:
101       IF = 5
          N1 = N1 + 1
          E0 = 0.0
          N2 = 0
          GO TO 100
C
108       IF=6
          N1=N1+1
          E0=0.0
          N2=0
          GO TO 100
C
102       IF = 1
          N1 = N1 + N2
          N2 = 0
C
106       CONTINUE
          IF ( IABS(P) .GT. LIMITP ) GO TO 107
          E0 = M
          P10 = 10.0**P
          E0 = IZM * E0 * P10
          GO TO 100
C
107       IF = 3
          E0 = IZM * EMAX
          IF ( P. LT. 0 ) E0 = 0.0
          GO TO 100
100       RETURN
          END
