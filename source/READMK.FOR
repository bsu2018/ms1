       SUBROUTINE READMK(KAR,IK,IKMAX,NRDRX,DSNAME,KFIZ,KODEOF)
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0
C   �O���PO�PAMMA: READMK(KAR,IK,IKMAX,NRDRX,DSNAME,KFIZ,KODEOF)
C   -------------
C   �A��A�E��E:    BBO� CTPOK� �A���X ��PEKT�B� #X�E�� �EPE�
C   ---------      KA�A� NRDRX B �OPMATE 80A1, �C�O����� �P� �TOM
C                  "MAKPO�E�EPATOP" BBO�A *** BEPC�� 0.0 ****
C                 TEKCT "MAKPOPAC��PE���" ��TAETC� �EPE� KA�A�
C                 NMAK [ MASPRT(89) ]
C
C                CTP�KT�PA �A��A NMAK:
C                --------------------
C
C              XXX_ZZZ_TT...TT
C->C 3-� �O�.  ___....TEKCT �O�CTA�OBK� ...(�O 70 �O�. )
C              ___    ----//----
C                ...........
C-> B KO��E:   000_000
C
C    ��E:
C              XXX - KO�-BO CTPOK C TEKCTOM �O�CTA�OBK�
C              ZZZ - ����A �ME�� TTT..TT "MAKPOKOMA���" --->
C                    B �A���X ��� "#X�E��" �TO �M� KO��P�ETC�
C                    �A���A� C C�MBO�A "M" ( MAKCIM), �O
C                    B �A��E NMAK C�MBO� "M" �E KO��P�ETC�
C
C   === �O��CKAETC� �C�O���OBAT� TAK-�E C�MBO� "�"  ===
C
C    "M" --->  MO�E��,            "�" --->  ��A�E���;
C
C
C                !! �OPMAT 1-CTPOK� KA��O�O "MAKPO_TE�A" ��KC�POBA� !!!!
C
C              -->  B KO��E �A��A -->  "000_000";
C              ��EC� "_" -->  O�O��A�AET �PO�E�;
C
C              B CTPOKAX "MAKPO_�O�CTA�OBOK" -> �OPMAT CBO�O����,
C                CO��AC�O C��TAKC�C� �A���X ��PEKT�B� "#�E��"
C                ( CM. �/�  INPUT0 );
C
C   METO�:  ( BEPC�� 0.0 ) --> �P�MOE KO��POBA��E;
C   -----
C@
C   �APAMETP�:   KAR - MACC�B BBE�E��O� ���OPMA��� ( �E �O�EE 80 );
C   ---------    IK, IKMAX - �PA���� "�O�E��O�" ���OPMA���;
C                NRDRX - KA�A� BBO�A;
C                DSNAME - MACC�B C DIM *(11) - �M� �A��A BBO�A
C                         ��� �PO�PAMM C�E�. O�C����BA���;
C                KODEOF = 0  -  CTPOKA BBE�E�A;
C                       = 1  -  C�T�A��� END OF FILE �A NRDRX;
C                KFIZ = 1  - "AKT" BBO�A ���-KO� CTPOK� COBEP�E�;
C                     = 0  - ���-K�� CTPOK� �E BBO����;
C
C
C   �O��PO�AMM�:  1. PROF1
C   -----------   2. SEQ0, MOVE0
C   �P�ME�A��E:   1. �P� O���KE CTP�KT�P� �A��A NMAK, ��� �P� E�O
C   ----------       C�OE --> COO��E��E � �CTA�OBKA KODEOF = 1;
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION   KAR(*), DSNAME(1), KARX(80)
       EQUIVALENCE ( NWTR, MASPRT(13) )
       EQUIVALENCE ( NMAK, MASPRT(89) )
       EQUIVALENCE ( NZRDR, MASPRT(127) )
       EQUIVALENCE ( NRDR, MASPRT(12) )
C
       DIMENSION NAME(72)
       INTEGER BLANK, TERM
       DATA KOD/0/, IFLAG/1/, MAKCIM/'M'/,K72/72/, K3/3/, K70/70/
       DATA MAKCIZ / '�' /, TERM / ';' /
       DATA BLANK/' '/, K80/80/, IPR/'%'/,IPL/'+'/, KODM/0/
C-->  IFLAG -->  �OMEP BXO�A....
       KODEOF = 0
       KFIZ = 0
       GO TO ( 1,2), IFLAG
       CALL SUBERR(IFLAG)
C-->  1-� BXO�..
1      CONTINUE
       IKX = 1
C$$$$$$IKMAXX = K80
       IKMAXX = K72
       IF ( KOD .EQ. 1 ) GO TO 9999
       READ( NRDRX, 500, END=9999) (KARX(I), I=1,K80 )
       IF ( NRDRX .EQ. NRDR ) NZRDR = NZRDR + 1
500    FORMAT(80A1)
       KFIZ = 1
23     CONTINUE
C-->  �CKAT� C�MBO� "MAKPO" -->  MAKCIM  �  MAKCIZ ....
       DO 12 I=IKX, IKMAXX
       IKMAK = I
       IF ( KARX(I) .EQ. MAKCIM ) GO TO 13
       IF ( KARX(I) .EQ. MAKCIZ ) GO TO 13
12     CONTINUE
C-->  �E �A��A, OT�AT� CTPOK�...
       GO TO 50
13     CONTINUE
C-->  �OXO�E �A�A�O..
C-->   LKMAK -> ����A �A "&", C�B�� �A "&"
       IKMAX  = IKMAK  - 1
       IKMAK = IKMAK + 1
       IF ( IKMAK .GT. IKMAXX ) GO TO 50
       LKMAK = 0
       DO 14 I=IKMAK, IKMAXX
       IF ( KARX(I) .EQ. BLANK ) GO TO 15
       IF ( KARX(I) .EQ. TERM  ) GO TO 15
       LKMAK = LKMAK + 1
14     CONTINUE
CC--
15     CONTINUE
C-->  IKMAK --> �A�A�O PAC��PE��� B KARX, LKMAK -->  E�O ����A...
       IF ( LKMAK .LE. 0 ) GO TO 50
C              ********
C--> ��TAEM NMAK...
10     CONTINUE
       REWIND NMAK
11     CONTINUE
       IF ( KODM .EQ. 1 ) GO TO 9998
       READ(NMAK,501, END=9998) NSTR, LNAME,(NAME(I), I=1,K72 )
501    FORMAT(I3, 1X, I3, 1X, 72A1 )
C--> �E KO����, �� O�..
       IF ( NSTR .EQ. 0 ) GO TO 50
C-->  �A� �� �POTOT�� ?
       IF ( LNAME .NE. LKMAK ) GO TO 17
       CALL SEQ0( KARX(IKMAK), NAME, LNAME, ISEQ0 )
       GO TO (16, 17 ), ISEQ0
       CALL SUBERR(ISEQ0)
17     CONTINUE
C  -->  PAC��PE�E �E �A���, �PO��CK  NSTR  CTPOK...
       IF ( KODM .EQ. 1 ) GO TO 9998
       DO 18 I=1,NSTR
       READ ( NMAK, 502, END=9998 )
502    FORMAT(80A1)
18     CONTINUE
C-->  �A �OB�� �O�CK..
       GO TO 11
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
16     CONTINUE
C-->  �POTOT�� O�O��A�...
       IKBEG = IKMAK
       IKEND = (IKBEG + LNAME )
C-->  �KA�ATE�� "OK�A" �POCMOTPA CTPOK� ��� INPUT0..
       IK = IKX
C-->  IKMAX -->  �A C�MBO�  �EPE� "&"
       DO 19 I=1, K80
19     KAR(I) = BLANK
C-->  �EPEC�AT� OT IK �O IKEND
       III=(IKEND - IK) + 1
       CALL MOVE0(KAR(IK), KARX(IK), III)
       IFLAG = 2
       RETURN
C**********************************************************************
50     CONTINUE
C-->  �� �E�O �E �A���....OT IKX �O IKMAXX -> �EPEC�AT� � OT�AT�..
       IK = IKX
       IKMAX = IKMAXX
C$$$$$$III=(IKMAX-IK) + 1
       III=(K80  -  IK) + 1
       DO 20 I= 1, K80
20     KAR(I) = BLANK
       CALL MOVE0( KAR(IK), KARX(IK), III )
       IFLAG = 1
       RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
2      CONTINUE
C-->  BTPO� BXO�..
       DO 21 I=1,K80
21     KAR(I) = BLANK
       IF ( NSTR . GT. 0 ) GO TO 22
C-->  BCE KO����OC�, E�EM �A���E...
       IFLAG = 1
       IKX = IKEND
C$$$$$$IKMAXX =  K80
       IKMAXX =  K72
       GO TO 23
C----------------------------------------------------------------------
22     CONTINUE
C-->  PAC��P�EM..
       IF ( KODM .EQ. 1 ) GO TO 9998
       READ(NMAK, 5020, END=9998 ) ( KAR(I), I=K3, K70 )
5020   FORMAT(3X,70A1)
C----------------------------------------------------------------------
       KAR(1) = IPL
       KAR(2) = IPR
       KAR(71) = IPR
       KAR(72) = IPL
       IK = K3
       IKMAX = K70
       IFLAG = 2
       NSTR = NSTR - 1
C      $$$$$$$$$$$$$$$$
       RETURN
C######################################################################
C######################################################################
C######################################################################
9999   CONTINUE
       KODEOF = 1
       KOD = 1
       RETURN
C######################################################################
9998   CONTINUE
C--> C NMAK �A�P��E�KA...
       WRITE(NWTR, 505 ) NMAK
505    FORMAT(/' *** C������ 䠩�� ��� "#X�E��" (����� # ',I2,') - ',
     =  '�訡�筠 ***'/)
       GO TO 9999
       END
