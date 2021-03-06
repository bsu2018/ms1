       SUBROUTINE CUBU(A,B,C,X)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT MAPTA    1989 �.)------------
C   �O��PO�PAMMA:  CUBU(A,B,C,X)
C  -------------
C
C   HA�HA�EH�E:    �O��PO�PAMMA PE�EH�� K����HO�O �PABHEH��
C   -----------    METO�OM : TP��OHOMETP��ECKOE PE�EH�E,KOTOPOE
C                  �O�POBHO O��CAHHO B C�PABO�H�KE �O MATEMAT�KE
C                  ABTOPAM� �.KOPH � T.KOPH .CTPAH��A  44
C                  �APA�PA� 1.8-4
C                     ----------
C                  ��EC� �AET TO��KO �E�CTB�TE��H�E KOPH�
C                  MH�M�E KOPH� �P�PABH��T H����...
C                  �OKA �PO�PAMMA C��TAETC� HE B �BO�HO�
C                  TO�HOCT�.....
C                  *********************
C     �PABHEH�E �MEET B��    X**3 + A*X**2 + B*X + C = 0
C
C     BXO�H�E �HA�EH��  A,B,C
C   A B�XO�H�E PE�EH�� : MACC�B X(3)
C
C **********************************************************************
       DIMENSION X(3)
C --> �P�BO��TC� K B��� : Y**3 + P*Y + Q = 0
       P =-(A**2/3.)+ B
       Q = 2.*(A/3.)**3 - A*B/3. + C
       PQ = (P/3.)**3 + (Q/2.)**2
       PI = 3.1415926
       X(1) = 0.
       X(2) = 0.
       X(3) = 0.
C --> PE�AEM ...PACCMOTP�M P � Q
       IF(P.NE.0.0) GO TO 30
C --> �HA��T P = 0    A    Q ?
       IF (Q.NE .0.0) GOTO 13
C ��EC� P=Q=0
       X(1) = -A/3.
       RETURN
 13    CONTINUE
C --> A T�T P=0 HO Q HE PABHO 0
       TE = ABS(-Q)
       X(1)= (TE)**(1./3.)
       IF ( Q.GT.0.0) X(1)=-X(1)
       X(1) = X(1) -A/3.
       RETURN
 30    CONTINUE
C--> �HA��T P HE PABHO 0 �OCMOTP�M Q ?
       IF (Q.NE.0.) GOTO 35
C--> ��EC� Q=0 A P HE PABHO 0
C --> �PABHEH�E �MEET B�� X**3 +X*P =0  O�HO TP�B�A��HOE PE�EH�E X=0
       X(1)= -A/3.
       IF(P.GT.0.) RETURN
C --> �HA��T P<0  X = +(-)SQRT(-P) -A/3.
       X(2) = SQRT(-P) - A/3.
       X(3) = - SQRT(-P) - A/3.
       RETURN
 35    CONTINUE
C --> � P � Q HE PABHO 0 ....
       IF(PQ.GE.0.0) GO TO 10
C--> ��EC� PQ<0 �HA��T P<0 �MEETC� 3 �E�CTB�TE��H�X PE�EH�� ...
       COALFA = -Q/(2.*SQRT(-(P/3.)**3))
       ALFA = ACOS(COALFA)
       X(1)= 2.*SQRT(-P/3.)*COS(ALFA/3.)  -A/3.
       X(2)= -2.*SQRT(-P/3.)*COS(ALFA/3.+PI/3.) -A/3.
       X(3)= -2.*SQRT(-P/3.)*COS(ALFA/3.-PI/3.) -A/3.
       RETURN
10     CONTINUE
C--> PQ>0 PACCMOTP�M P ?...�O���AETC� O�HO �E�CTB�TE��HOE PE�EH�E
       IF (P.LT.0.0)  GOTO 20
C --> ��EC� P>0
       TBETA = 2.*(P/3.)**(1.5)/Q
       BETA = ATAN(TBETA)
       TE = TAN(BETA/2)
       TALFA = ABS(TE)**(1./3.)
       IF (TE.LT.0.) TALFA =-TALFA
       ALFA  = ATAN(TALFA)
       X(1) = -2.*SQRT(P/3.)/TAN(ALFA*2.)-A/3.
       RETURN
 20    CONTINUE
C--> ��EC� P<0 ... �OC�E�H�� C���A�
       SBETA = 2.*(-P/3.)**(1.5)/Q
       BETA = ASIN(SBETA)
       TE = TAN(BETA/2.)
       TALFA = ABS(TE)**(1./3.)
       IF ( TE.LT.0.) TALFA = -TALFA
       ALFA  = ATAN (TALFA)
       X(1) = -2.*SQRT(-P/3.)/SIN(ALFA*2.) - A/3.
       RETURN
       END
