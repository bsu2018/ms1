         SUBROUTINE APFSQI  ( KOD, N0, N1, N2, IF )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)----------
C    �O��PO�PAMMA    APFSQI(KOD,N0,N1,N2,IF)
C
C      HA�HA�EH�E: �EPEBO� ��C�A, �A��CAHHO�O ���POB�M� C�MBO�AM� B
C      ----------  �EC�T��HOE ��C�O C ��KC�POBAHHO� TO�KO� (INTEGER*4)
C
C      METO�:      �O��EMEHTHOE �PEOPA�OBAH�E (C�EBA HA�PABO) �CXO�H�X
C      -----       ���POB�X �AHH�X, �PE�CTAB�����X BHE�H�� �OPM� �A��C�
C                  �PEO�PA��EMO�O ��C�A;
C
C      �APAMETP�:  KOD - MACC�B �AHH�X ��� �PEO�PA�OBAH�� ( B F-KO�E.);
C      ---------   N0  - �PEO�PA�OBAHHOE �E�OE ��C�O ( INTEGER*4 );
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
C                               N0 - �PEO�PA�OBAHHOE ��C�O, �PE�CTAB�EH-
C                               HOE ���PAM� OT N1 �O C�MBO�A "HE ���PA";
C                               (��� PABHOE 0 );
C                     IF = 3 - �P� �PEO�PA�OBAH�� ��C�A �PO��O��O
C                              �EPE�O�HEH�E; �P� �TOM CTPOKA ( MACC�B
C                              KOD ) �PO�O��AET �POCMATP�BAT�C�.
C                         A)  �P� BCTPE�E C�MBO�A "HE ���PA" -
C                             N1 - A�PEC "HE ���PA" ( HOMEP B KOD ), A
C                             N2 - ���HA OCTAB�E�C� �ACT�, �E� C�MBO�A 0
C                                "HE ���PA";
C                         �)  EC�� C�T�A��� "HE ���PA" HE �A��KC�POBAHA,
C                             A �O�E ( MACC�B KOD ) �C�EP�AHO, TO:
C                              N1=N1+N2, N2=0.
C                          BO BCEX C���A�X IF=3, �P�H�MAETC� N0=MAX/MIN
C                          ( B �AB�C�MOCT� OT �HAKA )  �HA�EH��
C                       INTEGER*4 B �PE�CTAB�EH�� HA �C�O����EMO� �BM.
C
C                    IF = 4 - N2 �A�AH MEH��E H��� ( N2<0 ), �P� �TOM
C                          N0=0, N1 � N2 �E� ��MEHEH��
C                    IF = 5 - N2 �A�AHO PABHO� H��� ( N2=0 ), �P� �TOM
C                          N0=0, N1=N1+1, N2=0;
C                    IF = 6 - BO�H�KAET �P� �O��TKE �PEO�PA�OBAH��
C                         �O�� ( MACC�BA KOD ) COCTO��E�O �� O�H�X
C                         HE�HA�E��X �PO�E�OB ��� C�MBO�OB:
C                               <->,<+>; �P� �TOM: N0=0, N1=N1+N2, N2=0;
C
C      �O��PO�PAMM�: APFTRT;
C      ------------
C
C      �P�ME�AH�E:   �PE��O�O�AETC�, �TO MACC�B B F-KO�E ��OP��O�EH
C      ----------     CO��ACHO TA����E B �O��PO�PAMM� APFTRT .
C                    ( �P� �TOM <���PA> = FKOD(I) - 1 );
C
C
CZZZ ------------------------------------------------------------------_
         INTEGER * 4  P, P1, TKOD, LIMITM, LIMITP, LIMIT
         DIMENSION KOD(1), TKOD(13)
         DATA IVV/0/, LIMITM/-2147483647/, LIMITP/+2147483647/
         DATA                        LIMIT/214748364/
         DATA TKOD/'0','1','2','3','4','5','6','7','8','9',
     *             ' ', '+', '-' /
         IF ( IVV .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C �EPB�� BXO�: �O��PAB�M LIMITM
         LIMITM = LIMITM - 1
C  �EPB�� BXO�: �EPEKO��P. TKOD
         IVV=1
         CALL  APFTRT(1, 13,  TKOD,  TKOD  )
C----------------------------------------------------------------------
1        CONTINUE
         NMAX = N1 + N2 -1
         IF = 4
         N0 = 0
         IF ( N2 .LT. 0 ) GO TO 100
         IF ( N2 .NE. 0 ) GO TO 3
C=====>  N2 = 0
         N0 = 0
         N1 = N1 + 1
         N2 = 0
         IF = 5
         GO TO 100
C----------------------------------------------------------------------
C    ��EPEM �PO�E��
C----------------------------------------------------------------------
3        DO 4 I=N1,NMAX
         IT = I
         IF ( KOD(I) .NE. TKOD(11) ) GO TO 5
4        CONTINUE
C=====> O�H� �PO�E��
         N0 = 0
         IF = 6
         N1 = N1 + N2
         N2 = 0
         GO TO 100
C====> ECT� �� �HAK ?  (IZ=0 =EQ= "+")
5        CONTINUE
         IZ = 0
         IF ( KOD(IT) .EQ. TKOD(12)  .OR. KOD(IT) .EQ. TKOD(13) )
     =                       GO TO 60
         GO TO 6
60       CONTINUE
         IF ( KOD(IT) .EQ. TKOD(13) ) IZ = -1
C====> HA HOB�� C�MBO�
         IT = IT + 1
         IF ( IT .LE. NMAX ) GO TO 6
C=====> �O�E KOH���OC� TO��KO O��H �HAK...
         IF = 6
         N0 = 0
         N1 = N1 + N2
         N2 = 0
         GO TO 100
C
C  ==== ��K� �P�EMA ��CE� ====
C
6        CONTINUE
         ID = 0
         I1 = IT
         IF = 2
         N0 = 0
         DO 7 I=I1, NMAX
         IT = I
         IF ( KOD(I) .LT. TKOD(1)  .OR.  KOD(I) .GT. TKOD(10) )
     =                       GO TO 8
C========> HA�A ���PA...
         IF ( N0 .GT. LIMIT ) GO TO 9
         N0 = N0 * 10
         IF ( N0 .EQ. 10*LIMIT .AND. KOD(I) .GT. 7 .AND. IZ .EQ. 0 )
     =                       GO TO 9
         IF ( N0 .EQ. 10*LIMIT .AND. IZ .EQ. -1 ) GO TO 10
C========>  BCE HOPMA��HO
         N0 = N0 +(KOD(I) - 1)
         GO TO 7
C=======> �P�  OTP. ��C�E "B �P�T�K" K MIN. �O��CT�MOM�
10       ID = -1
         NX =(KOD(I) - 1) -1
         N0 = N0 + NX
7        CONTINUE
C======> �O�E KOH���OC�, BCE PA�O�PA��
         IF = 1
         N1 = N1 + N2
         N2 = 0
C======> KAK �HAK ?
         IF ( IZ .EQ. 0 ) GO TO 100
         N0 = - N0
         IF ( ID .EQ. -1 ) N0 = N0 -1
         GO TO 100
C**********************************************************************
C  OCO��E C���A�
C**********************************************************************
8        CONTINUE
         IF = 2
         N1 = IT
         N2 = NMAX - IT
         IF ( IZ .EQ. 0 ) GO TO 100
         N0 = -N0
         IF ( ID .EQ. -1 ) N0 = N0 -1
         GO TO 100
C====>  �EPE�O�HEH�E.....
9        CONTINUE
         N0 = LIMITP
         IF ( IZ .EQ. -1 ) N0 = LIMITM
         IF = 3
C====CKAH�P�EM �A���E....
         I1 = IT
         DO 12 I=I1, NMAX
         IT = I
         IF ( KOD(I) .LT. TKOD(1)  .OR.  KOD(I) .GT. TKOD(10) )
     =                            GO TO 11
12       CONTINUE
C======> �O�E KOH���OC�, HO �EPE�O�HEH�E....
         N1 = N1 + N2
         N2 = 0
         GO TO 100
C======> BCTET��AC� HE ���PA
11       N1 = IT
         N2 = NMAX - IT
         GO TO 100
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
100      R E T U R N
         END
