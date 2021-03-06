          SUBROUTINE PLOT2(X,Y,NXF,NYF,KSXF,KSYF,ISXF,ISYF,NFOUTF,MASF)
CXXXX---KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C    �O��PO�PAMMA:   PLOT2(X,Y,NXF,NYF,KSXF,KSYF,ISXF,ISYF,NFOUTF,MASF)
C    -------------
C
C    HA�HA�EH�E:  B��EP��BAH�E �PA��KOB B�A�MOHE�AB�C�M�X
C    -----------        �EPEMEHH�X Y OTHOC�TE��HO X, �A�AHH�X TA����HO;
C                 �O�PE�HOCT� B�BO�A TO�K� PABHA 0.5 ��CKPET. �A�A
C
C    �APAMETP�:
C   ----------
C
C       0---------1-----------2-------------3 -------------------> Y
C  X(I) I       Y(I)       Y(NX+I)        Y(NX+NX+I) � T.�.
C       I
C       I
C       V
C
C   X - MACC�B �A�OBO� �EPEMEHHO�, PA�MEPHOCT� NX HEO�PAH��EHHA;
C
C   Y - MACC�B OP��HAT, PA�MEPHOCT�� NX*NY;
C
C   NXF - KO�-BO ��EMEHTOB MACC�BA X  ( � O�HO� �EPEMEHHO� MACC�BA Y );
C
C   NYF - KO�-BO O�HOBPEMEHHO B�BO��M�X �PA��KOB < ��� = 9, HO > = 1;
C
C   KSXF - KO�-BO CTPOK B�BO��M�X HA �EPTE� �O OC� X ( �P� KSXF=0, =NXF)
C          ( �HA�EH�E OKP����ETC� B CTOPOH� BO�PACTAH��......)
C   KSYF - KO�-BO C�MBO�OB B O�HO� CTPOKE B�BO�A �O OC� Y,
C          KSYF < ��� = 129, 12 �EPB�X C�MBO�OB - O���POBKA, KSYF >= 42;
C      ( �P� KSY=0, KSY=123 )
C   ISXF - �HTEPBA� �E�AT� PA��E��TE��H�X OCE� �O X,
C         �P� ISXF > KSXF �E�AT� ��OK�P�ETC�;
C
C    ISYF - �HTEPBA� �E�AT� PA��E��TE��H�X OCE� �O Y,
C          �P� ISYF > KSYF �E�AT� ��OK�P�ETC�;
C
C     NFOUTF - HOMEP �A��A B�BO�A, �P� <=0, �P�H�MAETC� =6;
C
C     MASF - C�OCO� �A�AH�� MAC�TA�A:
C                   HE = 0 BCE �PA��K� B 1-M MAC�TA�E;
C                      =0  KA���� �PA��K B CBOEM MAC�TA�E;
C
C
C
C  MACC�B OP��HAT �OTOB�TC� CO��ACHO CXEM�:
C
C Y(1) Y(2)...Y(NX) Z(NX+1) Z(NX+2)...Z(NX+NX) Q(2*NX+1)...
C
C  Y - OP��HAT� 1-�O �PA��KA, Z - 2-�O, Q - 3-�O, ....
C
C  �P� KSXF,KSYF,ISXF,ISYF=0 B���PA�TC� CTAH�APTH�E �HA�EH�� �T�X �AP-B.
C
C
C        �O��PO�PAMM�:  HET
C        -------------
C
C
CZZZZ------------------------------------------------------------------
           DIMENSION OUT(117),YPR(15),ANG(9),PPX(8),BLANK3(10)
           DIMENSION YMIN1(9),YSCAL1(9),YMAX1(9), YSCAL2(9)
           DIMENSION X(1),Y(1)
         DATA BLANK1,BLANK2/'-',':'/
           DATA BLANK/' '/
           DATA ANG/'1','2','3','4','5','6','7','8','9'/
           DATA PPX/'$','#','*','@','%','�','�','?'/
           DATA BLANK3/'0','1','2','3','4','5','6','7','8','9'/
       NX=NXF
             NY=NYF
                   KSX=KSXF
                           KSY=KSYF
                                   ISX=ISXF
                                           ISY=ISYF
       NFOUT=NFOUTF
                   MAS=MASF
           IF ( KSX .LE. 0 ) KSX=NX
           IF ( KSX .EQ. 1 ) KSX=2
           IF ( KSY .LE. 0 ) KSY=123
           IF ( ISX .LE. 0 ) ISX=6
           IF ( ISY .LE. 0 ) ISY=10
           IF ( KSY .GT. 129 ) KSY=129
           IF ( KSY .LT. 15 ) KSY=42
           IF ( NFOUT .LE. 0 ) NFOUT=6
           IF (  NY   .LE. 1 ) NY=1
           IF (  NY   .GT. 9 ) NY=9
C--------------------
C
C  PA�MEP �O Y
C
         KSY1=KSY-12
C
C  �A� �O X
C
         XSCAL=( X(NX) - X(1) ) / ( FLOAT(KSX-1) )
         IFLAG1=0
C
C   �A� B��OPA �� MACC�BA X  �  Y
C
         NSTEP=NX/(KSX-1)
         IF ( NSTEP .LE. 0 ) NSTEP=1
C
C    �PAH��� OP��HAT
C
         M1=1
         M2=NX*NY
         YMIN=Y(M1)
         YMAX=Y(M1)
         DO 40 J=M1,M2
         IF ( Y(J) - YMIN ) 28,26,26
26       IF ( Y(J) - YMAX ) 40,40,30
28       YMIN=Y(J)
         GO TO 40
30       YMAX=Y(J)
40       CONTINUE
C-----
          IF ( MAS .NE. 0 ) GO TO 405
C     B��PAT� �H��B. MAC�.
          DO 406 JM=1,NY
          M1=(JM-1)*NX+1
          M2=NX*JM
          YMIN1(JM)=Y(M1)
          YMAX1(JM)=Y(M1)
          DO 140 J=M1,M2
          IF ( Y(J) - YMIN1(JM) ) 128,126,126
126       IF ( Y(J) - YMAX1(JM) ) 140,140,130
128       YMIN1(JM)=Y(J)
          GO TO 140
130       YMAX1(JM)=Y(J)
140       CONTINUE
406       CONTINUE
C-------
405       CONTINUE
C
C   �A� B��OPA �O Y
C
         YSCAL=( YMAX - YMIN )/( FLOAT(KSY1-1) )
         YSCALM=YSCAL
         IF ( YSCAL .EQ. 0.0 ) YSCAL=1.0
C
         IF ( MAS .NE. 0 ) GO TO 407
C  B��PAT� �H��B��. �A� �O Y
         DO 408 JM=1,NY
         YSCAL1(JM)=( YMAX1(JM) - YMIN1(JM) ) / ( FLOAT(KSY1-1) )
         YSCAL2(JM)=YSCAL1(JM)
         IF ( YSCAL1(JM) .EQ. 0.0 ) YSCAL1(JM)=1.0
408      CONTINUE
407      CONTINUE
C     OP�AH��OBAT� B�BO�
C
         IF ( MAS .NE. 0 ) GO TO 409
C
C    �H��B���A��HO B�BECT�
C
         WRITE(NFOUT,600) X(1),XSCAL,X(NX)
600      FORMAT(1X,'X MIN',6X,'�A� X',6X,'X MAX'/ 1X,3( G10.4,1X))
         DO 410 JM=1,NY
410      WRITE(NFOUT,601) ANG(JM),YMIN1(JM),YSCAL2(JM),YMAX1(JM)
601      FORMAT(1X,'C�MBO� ',1A1,1X,'Y MIN',6X,'�A� Y',6X,'Y MAX'/
     *           10X,3(G10.4,1X) )
         GO TO 4100
C----
409       CONTINUE
C********************************************************************
C   B�BO� OCHOBH�X �APAMETPOB
C
          WRITE(NFOUT,8) X(1),XSCAL,X(NX),YMIN,YSCALM,YMAX
8         FORMAT(1X,'X MIN',6X,'�A� X',6X,'X MAX'/
     *           1X,3(G10.4,1X)/
     *           1X,'Y MIN',6X,'�A� Y',6X,'Y MAX'/
     *           1X,3(G10.4,1X)
     *           )
C
4100         CONTINUE
C
C
C  OCHOBHO� ��K�
C
         XB=X(1)
         L=1
         MY=NY
         I=1
         KSS=1
C----
45       F=I-1.
C
C     HOBA� TO�KA B X
C
         XPR= X(I)
C        XPR=XB+F*XSCAL
C  �A�AT� CETK�
C
50       DO 55 IX=1,KSY1
55       OUT(IX)=BLANK
C---
         IF ( KSS .EQ. 1 .OR. KSS .EQ. (ISX+1) ) GO TO 103
C
C  �HA�E CETKA HE H��HA
C
          GO TO 102
103       KSS=1
          DO 100 IX=1,KSY1
100       OUT(IX)=BLANK1
C
C  �A�O�HEH�E CETK� �O Y
C
102       DO 101 IX=1,KSY1,ISY
101       OUT(IX)=BLANK2
          IF ( IFLAG1 .NE. 0 ) GO TO 400
          KSY2=KSY1-1
          DO 401 IX=1,KSY2,10
          DO 401 IZ=1,10
401       OUT(IZ+IX-1)=BLANK3(IZ)
          IFLAG1=1
C-----
          WRITE(NFOUT,7) ( OUT(IZ),IZ=1,KSY1)
          DO 403  IX=1,KSY1
403       OUT(IX)=BLANK1
          DO 404 IX=1,KSY1,ISY
404       OUT(IX)=BLANK2
C
C
C  H��HO �� HAHOC�T� TO�K�
400       CONTINUE
C
C  TO�K� HAHECT�
C
          DO 60 J=1,MY
C  �H�EKC B Y
          LL = L + (J-1)*NX
C  �H�EKC HA B�BO� Y
          JP=( ( (Y(LL)-YMIN)/YSCAL) +1.0 )
C----
         IF ( MAS .EQ. 0 ) JP=( ( ( Y(LL)-YMIN1(J) )/YSCAL1(J) ) +1.0 )
C
C----
C
C  �A�O�H�T� C�MBO�OM-��O�PA�EH�EM
C
         IF ( OUT(JP) .EQ. BLANK .OR.
     *        OUT(JP) .EQ. BLANK1 .OR.
     *        OUT(JP) .EQ. BLANK2 ) GO TO 500
C  ���T� �A �EPECE�EH��M�
         J11=0
         DO 501 J1=1,7
         IF ( OUT(JP) .EQ. PPX(J1) ) J11=J1
501      CONTINUE
         OUT(JP)=PPX(J11+1)
         GO TO 502
C---
500       OUT(JP)=ANG(J)
502      CONTINUE
60       C O N T I N U E
         L = L + NSTEP
61       CONTINUE
C----
         WRITE(NFOUT,2) XPR,(OUT(IZ),IZ=1,KSY1)
2        FORMAT(1X,G10.4,1X,117A1)
C----
         KSS=KSS+1
80       I=I+NSTEP
         IF ( I-KSX ) 45,84,86
84       XPR=X(NX)
         KSS=1
         GO TO 50
C---
86       CONTINUE
         KSY2=KSY1-1
          DO 503 IX=1,KSY2,10
          DO 503 IZ=1,10
503       OUT(IZ+IX-1)=BLANK3(IZ)
         WRITE(NFOUT,7) ( OUT(IZ),IZ=1,KSY1)
7        FORMAT(12X,117A1)
C----
         IF ( MAS .NE. 0 ) GO TO 411
C--
         WRITE(NFOUT,600) X(1),XSCAL,X(NX)
         DO 412 JM=1,NY
412      WRITE(NFOUT,601) ANG(JM),YMIN1(JM),YSCAL2(JM),YMAX1(JM)
         GO TO 413
C===
411      CONTINUE
C---------
         WRITE(NFOUT,8) X(1),XSCAL,X(NX),YMIN,YSCALM,YMAX
C---------
413      CONTINUE
         WRITE(NFOUT,9) ( PPX(IZ),IZ=1,8)
9        FORMAT(1X,'�P�OP�TET �EPECE�EH��: ',
     *              8(1A1,1X)/ )
         RETURN
         END
