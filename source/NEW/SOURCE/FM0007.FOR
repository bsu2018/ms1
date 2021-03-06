         SUBROUTINE FM0007( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  FM0007( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�E�:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  71      "HE��H. CO�POT�B�EH�E"     RN
C
C                  72      "HE��H. �H��KT�BHOCT�"     LN
C
C                  720     "HE��H. �H��KT�BHOCT�" LN �A�AHHA� �EPE�
C                                                 L_ ����. OT I_TOKA;
C
C       HE��HE�HOCT� ��EMEHTA �A�AETC� TA����HO, �EPE� MAS <...>:
C
C       ���  "RN" ==>  BO��T-AM�EPHA� X-KA:
C
C             MAS < I1 U1  I2 U2  ... ...   I_N U_N >;
C
C       ���  "LN" ==> BE�EP-AM�EPHA� X-KA: ( T�� #72 ):
C
C             MAS < I1 �1  I2 �2  ... ...   I_N �_N >;
C
C       ���  "LN" ==> �EPE� L_���. OT TOKA:
C
C             MAS < I1 L_�1 I2 L_�2 .....           >;
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E��, C�A�H PA��O�EH�E
C   -----          TA����HO �A�AHH�X HE��HE�H�X �AB�C�MOCTE�.
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW �P� FGMSW = 0;
C                  LM - ���HA BCE�O MSW O��CAH��      �P� FGMSW = 1;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------   2. SPLIN0
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. HEO�XO��MO KO��POBAT� HE MEHEE �B�X TO�EK BAX.
C                     �A�AHHA� Y=F(X) HE �O��HA �MET� �PO��BO�H��
C                     PABH�� H��� --> �HA�E C�O� �P� PAC�ETE.......!!!!!
C
C                  4. H��E �P�BO��TC� �OPMAT� RMSW:
C
C                  RMSW T��A # 71, 72 � 720 [ HE��HE�H�E R � L ]:
C                  ----------------------------------------------
C                                                        LM=<�EPEMEHHA�>
C                                                          =+1<VAR>
C                                                  (OT KO�-AB TO�EK BAX)
C                    SPM
C +-----+----+-----------------------+
C I  G  I  J I  XY_MAS               I
C I     I    I MACC�B TO�EK BAX.     I    MAC�B� "B", "C", "D" -->
C I     I    I N,(X,Y).."B","C","D"  I    C��A�H PA��O�EH�E BAX:
C +-----+----+-----------------------+    CM. �/� SPLIN0;
C I  1  I 2  I 3 ... 3+(5*N+1)       I
C +-----+----+-----------------------+
C I  11 I 12 I 13 ...12+(5*N+1)      I
C +-----+----+-----------------------+
C
C   N -> KO���ECTBO TO�EK BAX.
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         EQUIVALENCE( FGMSW, MASPRT(87) )
         EQUIVALENCE( N,       RN       )
         INTEGER BA0T, TIP, FGMSW
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 71  .OR.  TIP .EQ. 72  .OR. TIP.EQ.720) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 501) TIP, NEL
501      FORMAT(' *** �O�. C�O� B FM0007: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> �TO HA�O?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C--> �A�POC "�TO�H�T� ���H� MSW"
C      LM:=8 + 2<HOMEPA ���OB> + 2<G-J �APAMETP�> + 5*N+1<MACC�B XYMAS>
C          N := KO�-BO TO�EK BAX (X,Y)
C
           NMAS = IFIX( PAR0T(1) )
           N  = NMAS/2
           LM = 8 + 2 +2 + 5*N + 1
C----------------------------------------------------------------------
         RETURN
1000     CONTINUE
C---> �OPMAT�POBAT� RMSW <OTPE�OK>;
C-->  CKO��KO ��CE�...
       NMAS=IFIX( PAR0T(1) )
C--> CKO��KO TO�EK..
       N = NMAS/2
C-->  �O��CT�MO, �� �TO ?
       IF ( N .GE. 2 ) GO TO 1001
C-->  PA�OTAT� HE����...
       NWTR = MASPRT(13)
       WRITE(NWTR,500) NEL
500    FORMAT(/' ***  ��� ��EMEHTA # ', I5, ' MA�O TO�EK ��� B.A.X.',
     = ' ***'/ ' *** PAC�ET �PEP�BAETC� ***'//)
       STOP 666
1001   CONTINUE
       LM = 2 + 5*N + 1
       DO 1002 I=1,LM
1002   RMSW(I) = 0.0
C-->  �P�H�T� MACC�B�...
       RMSW(3) = RN
C       MSW(3) = N
C
       DO 1003 I=1, NMAS
       RMSW(3+I) = PAR0T(1+I)
1003   CONTINUE
C--->  HA��EM C��A�H PA��O�EH�E....
       CALL SPLIN0( RMSW(3) )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       RETURN
       END
