         SUBROUTINE APF GET ( ICAR, RCAR, IER, NRDR, NWTR, DIALOG)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C      �O��PO�PAMMA:  APF GET ( ICAR, RCAR, IER, NRDR, NWTR, DIALOG)
C      ------------
C      HA�HA�EH�E:    BBO� TEKCTOBO� �H�OPMA��� C KAHA�A NRDR,
C      ----------     PA���BKA BBE�EHHO� �H�OPMA��� HA �O���ECK�E
C                     TEPM�. �P� KA��OM O�PA�EH�� B��E��ETC� O��H
C                     TEPM. B C���AE �P���X O���OK - �E�AT� COO��EH��
C                     HE NWTR, B �OPMATE �AB�C��EM OT �HA�EH�� DIALOG.
C                     B �TOM C���AE IER = 1. �P� �OCT��EH�� KOH�A
C                     �A��A HA NRDR - IER = 2.
C      METO�:         �O��EMEHTHA� "�O�KA�KA" TEPMOB.
C      -----
C      �APAMETP�:    ICAR - O�EPE�HO� BBE�EHH�� C NRDR TEPM (INTEGER*4);
C      ---------     RCAR - O�EPE�HO� BBE�EHH�� C NRDR TEPM (REAL*4);
C                    IER  - = 0  -> �C�E�HOE B��E�EH�E TEPMA;
C                           = 1  -> ���� �P���E O���K�, �ACT� BBO��M�X
C                                   CTPOK ��HOP�POBA�OC�;
C                           = 2  -> HEO���AHH�� KOHE� �A��A HA NRDR;
C                     NRDR, NWTR - KAHA�� BBO�A/B�BO�A;
C                     DIALOG - = 0 -> �AKETH�� PE��M;
C                              = 1 -> �TEPAKT�BH�� PE��M;
C     �O��PO�PAMM�: APFTEM, APFTRT, APFSQE, APFPAK;
C     ------------
C     �P�ME�AH�E:  1. �PAB��O PA���EH�� HA TEPM� CM. B APFTEM;
C     ----------   2. �P� DIALOG = 1 ---> IER BCE��A = 0;
C                  3. BO�MO�HO O�PA�EH�E: CALL APFGET(ICAR, ICAR, ...
CZZZ-------------------------------------------------------------------
         EQUIVALENCE ( RSIM(1), SIM(1) )
         INTEGER DIALOG, ER, SIM, FLAG, TEKS, EORDR, BLANK
         DIMENSION KAR(80), SIM(80), RSIM(80), ER(80)
         DATA FLAG/0/, EORDR/0/, BLANK/' '/
         IER = 0
         IF ( EORDR .EQ. 1 ) GO TO 999
         IF ( FLAG  .EQ. 1 ) GO TO 100
C
C===> ��K� �TEH�� � B��E�EH�� TEPMOB
C
10       DO 1 I=1,80
1        KAR(I)=BLANK
         READ(NRDR,600, END=999) (KAR(I),I=1,80)
600      FORMAT(80A1)
         IF ( DIALOG .EQ. 0 ) WRITE(NWTR,601) (KAR(I),I=1,80)
601      FORMAT( 20X,80A1 )
C===== �EPE��EM B F-KO�:
         CALL APF TEM(KAR, MAXS, SIM, RSIM, ER, IX )
         IF ( IX .EQ. 0 ) GO TO 11
C==== �P���E O���K�
         IF ( DIALOG .EQ. 0 ) WRITE(NWTR,602) IX, (ER(I),I=1,80)
         IF ( DIALOG .EQ. 1 ) WRITE(NWTR,603) IX, (ER(I),I=1,80)
602      FORMAT(1X,'*** O���OK ', I3, ' ***=>',80A1)
603      FORMAT(80A1/' <== �C�PAB�TE O���K� B CTPOKE ==> ')
         IF ( DIALOG .EQ. 1 ) GO TO 10
         IER = 1
         GO TO 10
C===> O���OK HET, �CTAHOB�M ��A�A�OH CKAH�POBAH��
11       FLAG = 1
         TEKS = 1
         GO TO 100
C
C====> TEK���� BXO�
C
100      CONTINUE
C====  TEPM �OCT��EH? ��� HEO�XO��MO ��TAT� HOB�� �PO���...
         IF ( TEKS .GT. MAXS ) GO TO 10
         ICAR = SIM(TEKS)
         RCAR = RSIM(TEKS)
         TEKS = TEKS + 1
         RETURN
C
C===== HEO���AHH�� KOHE� �A��A HA NRDR
C
999      EORDR = 1
         IER = 2
         RETURN
         END
