         SUBROUTINE DM0020 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT �EKA�P� 1987 �.)------------
C   �O��PO�PAMMA:  DM0020 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA�HA�EH�E:
C   -----------              MOH�TOP MO�E�E�:
C               +-----+--------------------------------------+
C               I     I                                      I
C               I T�� I    HA�MEHOBAH�E ��EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 333 I ���POBO�  RS-TP���EP                 I
C               +-----+--------------------------------------+
C               I 334 I ���POBO�   D-TP���EP                 I
C               +-----+--------------------------------------+
C               I 335 I ���POBO�   T-TP���EP                 I
C               +-----+--------------------------------------+
C               I 360 I KOM�APATOP(1)                        I
C               +-----+--------------------------------------+
C               I 361 I KOM�APATOP(2)                        I
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
C              �OPMAT RMSW T�� # 33X � 360:
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  TX -BPEM� BO�MO�HO�O �EPEK���EH��
C            I              I      I
C    11      I     2        I  DM  I  ��A� KD =0 - B�X. C��HA� HE ��MEH.
C            I              I      I          =1 - B�X. C��HA� ��MEH��.
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C    15      I     6        I  FM  I  #��2 -      -"-
C    16      I     7        I  FM  I  #CM2 -      -"-
C    17      I     8        I  LM  I  ATOKM2 -    -"-
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 17
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 8
C
C----------------------------------------------------------------------
C
C
C
C
C              �OPMAT RMSW T�� # 335:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  TX -BPEM� BO�MO�HO�O �EPEK���EH��
C            I              I      I
C    11      I     2        I  DM  I  ��A� KD =0 - B�X. C��HA� HE ��MEH.
C            I              I      I          =1 - B�X. C��HA� ��MEH��.
C    12      I     3        I  FM  I  #��1 - HOMEP ��EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ��EMEHTA #��1;
C    14      I     5        I  LM  I  ATOKM1 - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ��EMEHTA #��1;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 14
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 5
C
C----------------------------------------------------------------------
C
C
C
C                   �OPMAT RMSW T�� # 361:
C                   ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  TX- BPEM� BO�MO�HO�O �EPEK���EH��
C            I              I      I
C    11      I     2        I  DM  I  ��A� KD =0 - B�X. C��HA� HE ��MEH.
C            I              I      I          =1 - B�X. C��HA� ��MEH��.
C    12      I     3        I  FM  I  �1   - HOMEP ���A;
C    13      I     4        I  FM  I  �2   - HOMEP ���A;
C    14      I     5        I  FM  I  �3   - HOMEP ���A;
C    15      I     6        I  FM  I  �4   - HOMEP ���A;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 15
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 6
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
         NTIP = TIP - 332
         IF ( NTIP .GE. 1  .AND. NTIP .LE. 3 ) GO TO 11
         IF ( TIP .EQ. 360 .OR. TIP .EQ. 361 ) GO TO 11
C######################################################################
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), TIP
9998     FORMAT( ' *** �O�. C�O� B DM0020 - �A�POC HA O�C����BAH�E ���O�
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
         MSW(11) = 0
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 O�PE�E�EH�-------
600      CONTINUE
C==>  �TO �A T��?
         GO TO ( 610 , 620 , 630 ), NTIP
         IF( TIP .EQ. 360 ) GO TO 640
         IF( TIP .EQ. 361 ) GO TO 650
C-->  O���KA. KOH�AEM...
         CALL SUBERR(TIP)
         RETURN
C######################################################################
610      CONTINUE
C======================================================================
C     ���POBO� RS-TP���EP
C----------------------------------------------------------------------
         SIG1 = IFIX( TOKN ( MSW(14) ) + 0.5 )
         SIG11= IFIX( TOKN1( MSW(14) ) + 0.5 )
         TIME1 = TOKN1( MSW(14) + 3 - MSW(13) )
         SIG2 = IFIX( TOKN ( MSW(17) ) + 0.5 )
         SIG21= IFIX( TOKN1( MSW(17) ) + 0.5 )
         TIME2 = TOKN1( MSW(17) + 3 - MSW(16) )
         IF( SIG1 .EQ. SIG11 .AND. SIG2 .EQ. SIG21 ) GO TO 617
C---> �A�MEMC� B�XO�H�M C��HA�OM...
         IF( TIME1 .GT. TIME2 ) GO TO 615
C----------------------------------------------------------------------
C---> C��HA� HA BXO�E  "S" ��MEH�ETC� PAH��E.
         TIME = TIME1
         IF( SIG2 .EQ.   1   ) GO TO 663
         GO TO 662
615     CONTINUE
C----------------------------------------------------------------------
C---> C��HA� HA BXO�E  "R" ��MEH�ETC� PAH��E.
         TIME = TIME2
         IF( SIG1 .EQ.   1   ) GO TO 662
         GO TO 663
C----------------------------------------------------------------------
617      CONTINUE
         IF( SIG1 .EQ. 1 ) GO TO 618
         IF( SIG2 .EQ. 0 ) GO TO 661
         GO TO 663
618      CONTINUE
         IF( SIG2 .EQ. 1 ) GO TO 661
         GO TO 662
C######################################################################
620      CONTINUE
C======================================================================
C     ���POBO� D-TP���EP
C----------------------------------------------------------------------
         SIG1 = IFIX( TOKN ( MSW(14) ) + 0.5 )
         SIG11= IFIX( TOKN1( MSW(14) ) + 0.5 )
         TIME1 = TOKN1( MSW(14) + 3 - MSW(13) )
         SIG2 = IFIX( TOKN ( MSW(17) ) + 0.5 )
         SIG21= IFIX( TOKN1( MSW(17) ) + 0.5 )
         TIME2 = TOKN1( MSW(17) + 3 - MSW(16) )
C---> �A�MEMC� B�XO�H�M C��HA�OM...
         IF( TIME1 .GT. TIME2 ) GO TO 625
C---> C��HA� HA BXO�E  "D" ��MEH�ETC� PAH��E.
         TIME = TIME1
         IF( SIG2 .EQ. 0 ) GO TO 661
         IF( SIG11.EQ. 0 ) GO TO 663
         GO TO 662
625     CONTINUE
C---> C��HA� HA BXO�E  "C" ��MEH�ETC� PAH��E.
         TIME = TIME2
         IF( SIG21.EQ. 0 ) GO TO 661
         IF( SIG1 .EQ. 0 ) GO TO 663
         GO TO 662
C######################################################################
630      CONTINUE
C======================================================================
C     ���POBO� T-TP���EP
C----------------------------------------------------------------------
         SIG1 = IFIX( TOKN ( MSW(14) )  )
         SIG11= IFIX( TOKN1( MSW(14) )  )
         TIME = TOKN1( MSW(14) + 3 - MSW(13) )
C---> �A�MEMC� B�XO�H�M C��HA�OM...
         IF( SIG1 .EQ. 0 ) GO TO 661
         IF( SIG11.EQ. 1 ) GO TO 661
         ITOK = MSW(7)
         IF( TOKN(ITOK) .LE. 0.5 ) GO TO 662
         GO TO 663
C######################################################################
640      CONTINUE
C======================================================================
C     KOM�APATOP(1)
C----------------------------------------------------------------------
         SIGIN = TOKN( MSW(14) )
         SIGIN1= TOKN1( MSW(14) )
         SIGPR = TOKN ( MSW(17) )
         SIGPR1= TOKN1( MSW(17) )
641      CONTINUE
C---> �A�MEMC� B�XO�H�M C��HA�OM...
C----O�PE�E��M BXO�H�E C��HA��  (?)------------------------------------
C     �C�O����EM ��HE�H�� A��POKC�MA��� C��H��OB HA �P�MOM (PR)
C     � �HBEPCHOM (IN) BXO�AX KOM�APATOPA � HAXO��M BPEM� COB�A�EH��
C     �T�X C��HA�OB.
C         SIGPR = A * T + B
C         SIGIN = C * T + D
C----------------------------------------------------------------------
          A = (SIGPR1 - SIGPR) / H0
          C = (SIGIN1 - SIGIN) / H0
          IF( A .EQ. C ) GO TO 661
          B = SIGPR
          D = SIGIN
          HX = ( D - B ) / ( A - C )
          IF( HX .LT. 0.0  .OR.  HX .GE. H0 ) GO TO 661
C-->  HA �A�E �O��HO �PO��O�T� ��MEHEH�E C��HA�A.
          IF( HX .LT. HXMIN ) HX = HXMIN
          RMSW(10) = TN + HX
          MSW(11)  = 1
C--->  EC�� TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMET�T� �TO B "FKOM"....
         FKOM = 1
C--->   PAC��TAT� � �A�OMH�T� B�XO�HO� C��HA� � BPEM� E�O �O�B�EH��
         ITOK = MSW(7)
         TOKN1(ITOK) = 0.0
         IF( SIGIN1 .GE. SIGPR1 ) TOKN1(ITOK) = 1.0
         TOKN1(ITOK+1) = 0.0
         IF( TOKN1(ITOK) .LE. 0.5 ) TOKN1(ITOK+1) = 1.0
         TOKN1(ITOK+2) = TN + HX
         KOD = 0
         RETURN
C######################################################################
650      CONTINUE
C======================================================================
C     KOM�APATOP(2)
C----------------------------------------------------------------------
         NY1 = MSW(12)
         NY2 = MSW(13)
         NY3 = MSW(14)
         NY4 = MSW(15)
C--> O�PE�E��M BXO�H�E HA�P��EH�� ���  T=TN
         VNY1 = 0.0
         IF( NY1 .GT. 0 ) VNY1 = VALLN(NY1)
         VNY2 = 0.0
         IF( NY2 .GT. 0 ) VNY2 = VALLN(NY2)
         SIGIN = VNY1 - VNY2
         VNY3 = 0.0
         IF( NY3 .GT. 0 ) VNY3 = VALLN(NY3)
         VNY4 = 0.0
         IF( NY4 .GT. 0 ) VNY4 = VALLN(NY4)
         SIGPR = VNY3 - VNY4
C--> O�PE�E��M BXO�H�E HA�P��EH�� ���  T=TN+H0
         VNY1 = 0.0
         IF( NY1 .GT. 0 ) VNY1 = VALLN1(NY1)
         VNY2 = 0.0
         IF( NY2 .GT. 0 ) VNY2 = VALLN1(NY2)
         SIGIN1 = VNY1 - VNY2
         VNY3 = 0.0
         IF( NY3 .GT. 0 ) VNY3 = VALLN1(NY3)
         VNY4 = 0.0
         IF( NY4 .GT. 0 ) VNY4 = VALLN1(NY4)
         SIGPR1 = VNY3 - VNY4
C----------------------------------------------------------------------
C   �A��HE���� PAC�ET ��� MO�E�� KOM�APATOPA(2) �PO��BO��TC� TAK�E
C   KAK � ��� MO�E�� KOM�APATOPA(1), �O�TOM� �EPEXO��M HA METK� 641.
C----------------------------------------------------------------------
         GO TO 641
C######################################################################
C
C       BO�MO�H�E CTAH�APTH�E C�T�A���.
C
C######################################################################
661      CONTINUE
C--->  B�XO�HO� C��HA� HA �A�E HE ��MEH�ETC�.
C----------------------------------------------------------------------
         RMSW(10) = TN + H0
         ITOK = MSW(7)
         TOKN1(ITOK) = TOKN(ITOK)
         TOKN1(ITOK+1) = 0.0
         IF( TOKN1(ITOK) .LE. 0.5 ) TOKN1(ITOK+1) = 1.0
         TOKN1(ITOK+2) = TN + H0
         MSW(11) = 0
         KOD = 0
         RETURN
C======================================================================
662      CONTINUE
C-->   C��HA� HA B�XO�E �P�  T=TIME  CTA� PABH�M  "1".
C----------------------------------------------------------------------
         RMSW(10) = TIME
         ITOK = MSW(7)
         TOKN1(ITOK) = 1.
         TOKN1(ITOK + 1 ) = 0.
         TOKN1(ITOK + 2 ) = TIME
         MSW(11) = 1
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
         MSW(11) = 1
         KOD = 0
         RETURN
C######################################################################
500      CONTINUE
C===> ��KCA��� CMEH� COCTO�H�� B�XO�HO�O C��HA�A......ZAPROS = 4
C---->  HET �� HEO�XO��MOCT� �PO��BECT� CMEH� COCTO�H�� B�X.C��HA�A..?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        KOD = 0
        IF ( MSW(11) .EQ. 0 ) GO TO 510
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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