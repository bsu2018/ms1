      SUBROUTINE PLOMXY ( XY,NXY,KSX,KSY,ISX,ISY,NFOUT,RAB,IER)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C     �O��PO�PAMMA:  PLOMXY ( XY,NXY,KSX,KSY,ISX,ISY,NFOUT,RAB,IER)
C     ------------
C
C     HA�HA�EH�E:   B��EP��BAH�E �PA��KA �APAMETP��ECKO� �AB�C�MOCT�:
C     -----------       X=F(T), Y=�(T) => Y=FI(X), �A�AHHO� TA����HO;
C
C     METO�:        OTO�PA�EH�E R(X,Y) HA IR(JX,JY),�OPM�POBAH�E
C     ------         MHO�ECTBA IR,E�O COPT�POBKA � B���A���A���.
C
C
C
C     �APAMETP�:    XY - MACC�B �HA�EH�� X(T) � Y(T) (PA�MEPHOCT� 2*NXY)
C     ----------         XPAHEH�E �OC�E�OBATE��HOE X1,Y1,X2,Y2,.........
C                   NXY - KO���ECTBO TO�EK O�PE�E�EH�� Y=FI(X),
C                          PA�MEPHOCT� MACC�BA XY PABHA 2*NXY
C                   KSX - KO���ECTBO TO�EK �O X (KO�-BO CTPOK �PA��KA).
C                          (KSX<=NXY; �P� KSX<=0 � KSX>NXY --> KSX=NXY);
C                   KSY - KO���ECTBO C�MBO�OB B O�HO� CTPOKE Y:
C                          B 1-X 12-T� O���POBKA, KSY>=42,
C                          �P�  KSY<=15 ��� KSY>129 --> KSY=42 ��� 129;
C                   ISX - �HTEPBA� �E�AT� PA��E��TE��H�X OCE� �O X,
C                          �P� ISX > KSX �E�AT� ��OK�P�ETC�; =0 --> =6;
C                   IXY - �HTEPBA� �E�AT� PA��E��TE��H�X OCE� �O Y,
C                          �P� ISY > KSY �E�AT� ��OK�P�ETC�; =0 --> =10;
C                   NFOUT - HOMEP �A��A B�BO�A; �P� <=0 --> =6;
C                   RAB - PA�O��� MACC�B PA�MEPHOCT� <2*NXY>;
C                 !!!  �O��CKAETC� COBME�EH�E MACC�BOB RAB � XY:
C                      �P� �TOM �AHH�E B MACC�BE XY �OC�E PA�OT�
C                      �/� PLOMXY ����T �CKA�EH�;
C                 !!!  ��� �OBTOPHO� �E�AT� TEX �E �PA��KOB,
C                      �OCTATO�HO �P� �OBTOPHOM B��OAK PLOMXY
C                      �KA�AT� IER = -1 � �EPE�AT� �/� PLOMXY
C                      "CTAP�E" HE ��MEHEHH�E �HA�EH�� B MACC�BE RAB;
C
C                   IER - �P� BXO�E ::=> ���OE, HE PABHOE -1
C                                        �OCTPOEH�E �PA��KOB �O
C                                        �AHH�M MACC�BA "XY";
C                                   ::=> =-1 -> �O �AHH�M "RAB",
C                                        HE �EPB�� BXO� B PLOMXY;
C
C                   IER - �H��KATOP O���K�: IER= 0 - O���K� HET;
C                                           IER= 1 - O���KA �A�POCA;
C     �O��PO�PAMM�: TRIAR1, PLOM00, PLOM01 -> �PEBECHA� COPT�POBKA;
C     -------------  *****
C
C     �P�ME�AH�E:   O���KA OKP���EH�� �O OC�M = �A�� ��CKPET��A��� IR;
C     -----------
C
CZZZ ------------------------------------------------------------------
      DIMENSION XY(*),      RAB(*)
       EXTERNAL PLOM00,  PLOM01
       INTEGER PLOM01

      integer BLANK1,BLANK2,BLANK3,BLANK, PPX, CIM, ZERO, OUT, OUTB !!!!!!!! ?????

      DIMENSION BLANK3(10), OUT(117), PPX(10), OUTB(117)
      INTEGER RAB, U, T

      DATA BLANK1,BLANK2,BLANK/'-',':',' '/,CIM/'*'/,ZERO/'.'/
      DATA BLANK3/'0','1','2','3','4','5','6','7','8','9'/
      DATA PPX/'1','2','3','4','5','6','7','8','9','�'/
       IER1 = IER
       IER = 0
       IF ( IER1 .EQ. -1 ) GO TO 4
C                           *******
      KSYP=KSY
      KSXP=KSX
      ISXP=ISX
      ISYP=ISY
      NFOUTP=NFOUT
      IER=0
      IF(NXY.LE.1) IER=1
      IF(KSX.LE.0) KSXP=NXY
      IF(KSX.GT.NXY) KSXP=NXY
      IF(KSX.EQ.1) KSXP=2
      IF(KSY.LE.0) KSYP=123
      IF(KSY.LT.15) KSYP=42
      IF(KSY.GT.129) KSYP=129
      IF(ISX.LE.0) ISXP=6
      IF(ISY.LE.0) ISYP=10
      IF(NFOUT.LE.0) NFOUT=6
      IF(IER.EQ.1) GO TO 100
       NXY2 = 2*NXY
C     PA�MEP B C�MBO�AX �O Y:
      KSY1=KSYP-12
      KSY2=KSY1-1
C     HA��EM MIN � MAX -OKHO �PA��KA
      XMIN=XY(1)
      XMAX=XMIN
      YMIN=XY(2)
      YMAX=YMIN
      DO 1 I=2,NXY
      IF(XY(2*I-1) .LT. XMIN) XMIN=XY(2*I-1)
      IF(XY(2*I-1) .GT. XMAX) XMAX=XY(2*I-1)
      IF(XY(2*I  ) .LT. YMIN) YMIN=XY(2*I  )
      IF(XY(2*I  ) .GT. YMAX) YMAX=XY(2*I  )
    1 CONTINUE
C     �A�� B��OPA
      XSCAL=(XMAX-XMIN)/(FLOAT(KSXP-1))
      YSCAL=(YMAX-YMIN)/(FLOAT(KSY1-1))
      IF(XSCAL .EQ. 0.0 .OR. YSCAL .EQ. 0.0) IER=1
      IF(IER .EQ. 1) GO TO 100
C1*********************************************************************
C   EC�� BO�MO�HOCT� OTC�E��T� H���?   EC�� �A, TO K[X/Y]P0 = 1
       KXP0 = 0
       KYP0 = 0
       JXP0 = ( 0.0 - XMIN ) / XSCAL + 1
       JYP0 = ( 0.0 - YMIN ) / YSCAL + 1
       IF ( JXP0 .GE. 1   .AND.  JXP0 .LE. KSXP ) KXP0 = 1
       IF ( JYP0 .GE. 1   .AND.  JYP0 .LE. KSY1 ) KYP0 = 1
C1*********************************************************************
C     �O���EH�E MACC�BA OTO�PA�EH��:
      DO 2 I=1,NXY
C     KOOP��HATA �O X:
      JX=(XY(2*I-1)-XMIN)/XSCAL+1
C     ��HE�HA� �HTEP�O�����, XG-AP��MEHT �O X, Y-�O Y:
      XG=XMIN+XSCAL*(JX-1)
C     �P��O:
      YG=XY(2*I)
C     KOOP��HATA �O Y:
      JY=(YG-YMIN)/YSCAL+1
C     �A�OMH�M
      RAB(2*I-1) = JX
    2 RAB(2*I) = JY
C     COPT�P�EM �O JX
C----------------------------------------------------------------------
       CALL TRIAR1( NXY, PLOM00,  PLOM01,  RAB )
C-----------*******----------------------------------------------------
    4 CONTINUE
C     �PO��BO��M B�BO�
      WRITE(NFOUTP,600) XMIN,XSCAL,XMAX,YMIN,YSCAL,YMAX
  600 FORMAT(1X,'X MIN',6X,'�A� X',6X,'X MAX'/
     *       1X,3(G10.4,1X)/
     *       1X,'Y MIN',6X,'�A� Y',6X,'Y MAX'/
     *       1X,3(G10.4,1X))
C ISTR - �P��HAK HE B�BE�EHHO� CTPOK�, KSS- C.�.K. PA��E��TE�E�
C  JXXB - C.�.K. �OC�E�OBATE��H�X CTPOK, OUTB - "��CTA�" CTPOKA
        JXXB = 1
      ISTR=0
      KSS=1
C     O���POBKA HA�A�A, ISTR=0 CTPOK� HA B�BO� HET.
      DO 6 IX=1,KSY2,10
      DO 6 IZ=1,10
    6 OUT(IZ+IX-1)=BLANK3(IZ)
      WRITE(NFOUTP,601) (OUT(IZ),IZ=1,KSY1)
  601 FORMAT(12X,117A1)
C     B�BO� �PA��KA
      DO 7 NT=1, NXY
      IF(NT .NE. 1) GO TO 8
C     �O��OTOB�T� HOB�� CTPOK�
    9 DO 12 IX=1,KSY1
   12 OUT(IX)=BLANK
      IF(KSS .EQ. 1  .OR.  KSS .EQ. (ISXP+1)) GO TO 13
C     CETKA �O �OP��OHTA�� HE H��HA
      GO TO 14
   13 KSS=1
      DO 15 IX=1,KSY1
   15 OUT(IX)=BLANK1
C     CETKA �O BEPT�KA��
   14 DO 16 IX=1,KSY1,ISYP
   16 OUT(IX)=BLANK2
C2*********************************************************************
C  �OCTAB�T� ZERO �O BEPT�KA�� EC�� BO�MO�HO
       IF ( KYP0 .EQ. 1 ) OUT(JYP0) = ZERO
C2*********************************************************************
C     CTPOKA �O��OTOB�EHA, JXX-��KC�P. �O����� �O X
      JXX=RAB(2*NT-1)
C3*********************************************************************
C   �POBECT� ZERO �O �OP��OHTA�� EC�� BO�MO�HO
       IF ( KXP0 .EQ. 0  .OR. JXX .NE. JXP0 ) GO TO 19
C   �AHOC�M ZERO
       DO 20 IX=1, KSY1
20     OUT(IX) = ZERO
       DO 191 IX=1,KSY1,ISYP
191    OUT(IX)=BLANK2
19     CONTINUE
C3*********************************************************************
       JRAB2= RAB( 2*NT )
      OUT( JRAB2   )=CIM
      ISTR=1
      GO TO 7
C     HE �EPBA� TO�KA
    8 CONTINUE
      IF(JXX .EQ. RAB(2*NT-1)) GO TO 11
C     B�BECT� CTPOK�
   10 XPP=XMIN+(JXX-1)*XSCAL
      WRITE(NFOUTP,602) XPP,(OUT(IZ),IZ=1,KSY1)
  602 FORMAT(1X,G10.4,1X,117A1)
      ISTR=0
      KSS=KSS+1
      JXXB = JXXB + 1
C  H��HO �� B�BECT� "��CT��K�" ? JXNEXT - HOMEP C�E�. CTPOK�
         JXNEXT = RAB(2*NT-1)
C  KSTRN - KO�-BO "��CT��EK"
         KSTRN = JXNEXT - JXXB
         IF ( KSTRN .LE. 0 ) GO TO 9
C@@@
C  B�BECT� "��CT��K�"
         DO 21 KSTR = 1, KSTRN
C  �OPM�P�EM OUTB
         DO 22 IX = 1, KSY1
22       OUTB(IX) = BLANK
         IF ( KSS .EQ. 1  .OR.  KSS .EQ. (ISXP+1) ) GO TO 23
C   CETKA �O �OP��. HE H��HA
         GO TO 24
23       KSS = 1
         DO 25 IX = 1,KSY1
25       OUTB(IX) = BLANK1
C   CETKA �O BEPT�KA��
24       DO 26 IX = 1, KSY1, ISYP
26       OUTB(IX) = BLANK2
C  ZERO �O BEPT�KA��
         IF ( KYP0 .EQ. 1 ) OUTB(JYP0) = ZERO
C  ZERO �O �OP��OHT�
         IF ( KXP0 .EQ. 0  .OR.  JXXB .NE. JXP0 ) GO TO 29
         DO 30 IX = 1, KSY1
30       OUTB(IX) = ZERO
         DO 31 IX=1,KSY1, ISYP
31       OUTB(IX) = BLANK2
29       CONTINUE
C  B�BO� "��CT��K�"
         XPP = XMIN + (JXXB-1)*XSCAL
         WRITE(NFOUTP,602) XPP, ( OUTB(IZ), IZ=1,KSY1)
         KSS = KSS + 1
         JXXB = JXXB + 1
21       CONTINUE
C@@@@@@
      GO TO 9
C     B CTAPO� �O����� TO�KA ECT�,�O�AB�T� C�E������
   11 JP=RAB(2*NT)
C     OTC�E��T� �EPECE�EH��, MO�HO �AT�PAT�?
      IF(OUT(JP) .EQ. BLANK  .OR.  OUT(JP) .EQ. ZERO  .OR.
     *   OUT(JP) .EQ. BLANK1  .OR.
     *   OUT(JP) .EQ. BLANK2) GO TO 17
C     CT�PAT� HE����,�CTAHOB�T� �P�OP�TET �EPECE�EH��
      J11=0
      DO 18 J1=1,9
      IF(OUT(JP) .EQ. PPX(J1)) J11=J1
   18 CONTINUE
      OUT(JP)=PPX(J11+1)
      ISTR=1
      GO TO 7
   17 OUT(JP)=CIM
       ISTR = 1
      GO TO 7
C-----
    7 CONTINUE
C     �TO-TO OCTA�OC�?
      XPP=XMIN+(JXX-1)*XSCAL
      IF(ISTR .EQ. 1) WRITE(NFOUTP,602) XPP,
     *                               (OUT(IZ),IZ=1,KSY1)
C     �A�H�� O���POBKA
      DO 190 IX=1,KSY2,10
      DO 190 IZ=1,10
190   OUT(IZ+IX-1)=BLANK3(IZ)
      WRITE(NFOUTP,601) (OUT(IZ),IZ=1,KSY1)
C     MAC�TA� � �P�OP�TET�
      WRITE(NFOUTP,600) XMIN,XSCAL,XMAX,YMIN,YSCAL,YMAX
      WRITE(NFOUTP,603) (PPX(IZ),IZ=1,10)
  603 FORMAT(1X,'�P�OP�TET �EPECE�EH��: ',
     *         10(1A1,1X)/)
  100 RETURN
      END
