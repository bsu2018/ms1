         SUBROUTINE DM0029 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT �EKA�P� 1987 �.)------------
C   �O��PO�PAMMA:  DM0029 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA�HA�EH�E:
C   -----------              MOH�TOP MO�E�E�:
C               +-----+--------------------------------------+
C               I     I                                      I
C               I T�� I    HA�MEHOBAH�E ��EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 330 I ���POBO� �O���ECK�� ��EMEHT "�"      I
C               +-----+--------------------------------------+
C               I 331 I ���POBO� �O���ECK�� ��EMEHT "���"    I
C               +-----+--------------------------------------+
C               I 332 I ���POBO� �O���ECK�� ��EMEHT"�CK�.���"I
C               +-----+--------------------------------------+
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                   HE �E�AET H��E�O
C
C
C                  B) ZAPROS = 2
C                     ----------
C                   HE �E�AET H��E�O
C
C                                                                      )
C                  C) ZAPROS = 3
C                     -----------
C                     �O��BO��TC� PAC�ET �HA�EH�� B�XO�H�X C��HA�OB
C                     �O ��BECTH�M BXO�H�M C��HA�AM.
C                     �P� �TOM: FKOM = 0 - COCTO�H�E B�XO�H�X
C                                          C��HA�OB HE ��MEH��OC�;
C                               FKOM = 1 - HA TEK��EM �A�E COCTO�H�E
C                                         �O��HO "���O" ��MEH�T�C� B
C                                         B T = TX MOMEHT BPEMEH�;
C                                         PEA��HO B �PO�PAMME O�PE�E-
C                                         ��ETC� HX-�A� BO�MO�HO� CME-
C                                         H� COCTO�H��, �ATEM TX=TN+HX;
C                                         �P� HX < HXMIN [MASPRT(62)]
C                                         �P�H�MAETC� HX = HXMIN;
C                                         COCTO�H�E HX=0.0 B���BAET
C                                         �E�AT� COO��EH��;
C                                         �PO��BE�EH� COTBETCTB����E
C                                         ��MEHEH�� B MASPRT:
C                                          FKOM <== "1"
C                                         HXG = MIN( HX, HXG )
C                                         TXG = TN + HXG
C
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX[RMSW(10)] -
C               ( PABEHCTBO �POBEP�ETC� C TO�. �O "EPSKOM"[MASPRT(39)])
C                      ��KC�P�ETC� CMEHA COCTO�H�� B�XO�HO�O C��HA�A,
C                      B �POT�BHOM C���AE, �HA�EH�E B�XO�HO�O C��HA�A
C                      BOCCTAHAB��BAETC�;
C
C   METO�:         �O���ECKOE MO�E��POBAH�E
C   -----
C
C   �APAMETP�:     MSW, RMSW - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��
C   ---------      ( "OTPE�OK" MACC�B� MSWALL -> C.M. �/� GETMSW );
C                  YALL - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                  JALL - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  VALLN - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  T  =  TN;
C                  VALLN1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                          �P�  TN + H0;
C                  TOKN  � TOKN1 - �HA�EH�E �EPEMEHH�X "COCTO�H��"
C                                  MO�E�� �P� TN  �  TN + H0;
C                          ( PA�MEPHOCT� -> CM. MASPRT(46) = NNTOK )
C                  LOGYTR - BEKTOP �P��HAKOB K.�. ���OB:
C                  �C�O����ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 6;
C                  PA�MEPHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK��A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  KOD - �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. SEQSOS
C
C   �P�ME�AH�E:  1.CTP�KT�P� MSW C�OBA MAKPOMO�E�� �AHHO�O T��A CM. H��E
C   ----------   2. �P� ZAPROS ^=  1, 2, 3, 4 - B��A�A COO��EH�� � �XO�
C                3. �P� FTEST >=1 -> �E�AT� HA WTRE AKTA CMEH� COCTO�H��
C
C
C                    "�OCTO�HHA�" �ACT� MSW
C                     ----------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I�HA�.  I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I BO   I �HA� I"TOKN" IPB I
C  I       I        I DM.....I        I ���OBI EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C              �OPMAT RMSW T�� # 330,331,332:
C              ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0025    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I         PE�EPB
C            I              I      I
C    11      I     2        I  FM  I  NBXOD-KO���ECTBO TOKOB, �C�O��-
C            I              I      I        ��EM�X KAK BXO�H�E C��HA��;
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C .......    I   .......    I  ... I  ..........................
C  9+I*3     I     I*3      I  FM  I  #��I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 11 + 3*NBXOD
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 2 + 3*NBXOD
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         REAL JALL, JRL, L, IRLT, IRLN
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
               DIMENSION MSWALL(*), RMSWAL(*)
               INTEGER ADRMSW
         EQUIVALENCE ( MASPRT(62), HXMIN )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(60), FTEST )
         INTEGER FTEST
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( HXG,    MASPRT(61) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FKOM,   MASPRT(34) )
         EQUIVALENCE ( TXG,    MASPRT(35) )
         EQUIVALENCE ( NTXG,   MASPRT(36) )
         EQUIVALENCE ( EPSKOM, MASPRT(39) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         EQUIVALENCE ( ICLOSE, MASPRT(77) )
         EQUIVALENCE ( UOPEN,  MASPRT(78) )
         REAL ICLOSE
         INTEGER FKOM, ZAPROS, TIP, SIG1, SIG11, SIG2, SIG21
C--->  KOHTPO�� T��A
         TIP = MSW(2)
         NTIP = TIP - 329
         IF ( NTIP .GE. 1  .AND. NTIP .LE. 3 ) GO TO 11
C######################################################################
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), TIP
9998     FORMAT( ' *** �O�. C�O� B DM0029 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C######################################################################
11       CONTINUE
C---> B��OP PE��MA:
         GO TO ( 667, 666, 600, 500       ), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C======================================================================
C---------------Z A P R O S = 1----------------------------------------
667      CONTINUE
         KOD = 0
         RETURN
C======================================================================
C---------------Z A P R O S = 2----------------------------------------
666      CONTINUE
C---> "C�POC" �HA�EH�� TX - BO�MO�HO�O BPEMEH� �EPEK���EH��.....
         RMSW(10) = TN + H0
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 O�PE�E�EH�-------
600      CONTINUE
C==>  �TO �A T��?
         GO TO ( 610 , 620 , 630 ), NTIP
C-->  O���KA. KOH�AEM...
         CALL SUBERR(TIP)
         RETURN
C######################################################################
610      CONTINUE
C======================================================================
C     ���POBO� �O���ECK�� ��EMEHT "�"
C----------------------------------------------------------------------
         NBXOD = MSW(11)
         TIME  = TN + H0
         IMSW  = 11
         DO 615 I = 1,NBXOD
         IMSW = IMSW + 3
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         IF( TIME .GT. TIME1 ) TIME = TIME1
615      CONTINUE
         IMSW = 11
         DO 617 I = 1,NBXOD
         IMSW = IMSW + 3
         IBX  = IFIX (TOKN1( MSW(IMSW) ) )
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         PARAM =  TOKN( MSW(IMSW) )
         IF( TIME1 .GT. TIME ) IBX = IFIX( PARAM )
         IF( IBX .EQ. 0  ) GO TO 663
617      CONTINUE
         GO TO 662
C######################################################################
620      CONTINUE
C======================================================================
C     ���POBO� �O���ECK�� ��EMEHT "���"
C----------------------------------------------------------------------
         NBXOD = MSW(11)
         TIME  = TN + H0
         IMSW  = 11
         DO 625 I = 1,NBXOD
         IMSW = IMSW + 3
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         IF( TIME .GT. TIME1 ) TIME = TIME1
625      CONTINUE
         IMSW = 11
         DO 627 I = 1,NBXOD
         IMSW = IMSW + 3
         PARAM = TOKN1( MSW(IMSW) )
         IBX  = IFIX ( PARAM )
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         PARAM = TOKN( MSW(IMSW) )
         IF( TIME1 .GT. TIME ) IBX = IFIX( PARAM )
         IF( IBX .EQ. 1  ) GO TO 662
627      CONTINUE
         GO TO 663
C######################################################################
630      CONTINUE
C======================================================================
C     ���POBO� �O���ECK�� ��EMEHT "�CK�. ���"
C----------------------------------------------------------------------
         NBXOD = MSW(11)
         TIME  = TN + H0
         IMSW  = 11
         DO 635 I = 1,NBXOD
         IMSW = IMSW + 3
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         IF( TIME .GT. TIME1 ) TIME = TIME1
635      CONTINUE
         IMSW = 11
         IBIX = 0
         DO 637 I = 1,NBXOD
         IMSW = IMSW + 3
         PARAM = TOKN1( MSW(IMSW) )
         IBX  = IFIX ( PARAM )
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         PARAM = TOKN( MSW(IMSW) )
         IF( TIME1 .GT. TIME ) IBX = IFIX( PARAM )
         IBIX = IBIX + IBX
637      CONTINUE
         IF( IBIX .EQ. 0  .OR.  IBIX .EQ. NBXOD ) GO TO 663
C######################################################################
C
C       BO�MO�H�E CTAH�APTH�E C�T�A���.
C
C######################################################################
662      CONTINUE
C-->   C��HA� HA B�XO�E �P�  T=TIME  CTA� PABH�M  "1".
C----------------------------------------------------------------------
         RMSW(10) = TIME
         ITOK = MSW(7)
         TOKN1(ITOK) = 1.
         TOKN1(ITOK + 1 ) = 0.
         TOKN1(ITOK + 2 ) = TIME
         KOD = 0
         RETURN
C======================================================================
663      CONTINUE
C-->   C��HA� HA B�XO�E �P�  T=TIME  CTA� PABH�M  "0".
C----------------------------------------------------------------------
         RMSW(10) = TIME
         ITOK = MSW(7)
         TOKN1(ITOK) = 0.
         TOKN1(ITOK + 1 ) = 1.
         TOKN1(ITOK + 2 ) = TIME
         KOD = 0
         RETURN
C######################################################################
500      CONTINUE
C===> ��KCA��� CMEH� COCTO�H�� B�XO�HO�O C��HA�A......ZAPROS = 4
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        KOD = 0
C       EC�� TXG   "�P�MEPHO" PABHO TX=MO�E��, TO CMEHA H��HA!
         TX   = RMSW(10)
         EPST = ABS( TX   - TXG )
C----->   (TX - TXG) = [ (TN+HX) - (TN+HXG) ]
         IF ( EPST .GT. EPSKOM ) GO TO 510
C===> �HA�E CMEH�M COCTO�H�E....
         ITOK = MSW(7)
         TOKN1(ITOK+2) = TXG
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) MSW(1),TOKN1(ITOK),TN,TXG
501     FORMAT(' ***FTEST_1: B ��-TE #',I5, ' C��HA�  CTA� =',F2.0,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
510      CONTINUE
C--->  B�X. C��HA� HE ��MEH��C�.
         ITOK = MSW(7)
         TOKN1(ITOK) = TOKN(ITOK)
         TOKN1(ITOK+1) = 0.0
         IF( TOKN1(ITOK) .LE. 0.5 ) TOKN1(ITOK+1) = 1.0
         TOKN1(ITOK+2) = TXG
         RETURN
C----------------------------------------------------------------------
         END
