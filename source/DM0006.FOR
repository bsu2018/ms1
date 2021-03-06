         SUBROUTINE DM0006 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0006 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA�HA�EH�E:    MOH�TOP MO�E�� R_L ��O�A [ MO�E�� # 80, �.T�� # 5 ]
C   -----------    ***************************************************
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C                  �OPM�POBAH�E BK�A�OB B YALL MATP��� �POBO��MOCTE� �
C                  JALL - BEKTOP �CTO�H�KOB TOKA �HA�EH�� �APAMETPOB
C                  �KB�BA�EHTH�X  G - J BETBE�  MO�E�� ��EMEHTA �E��
C                           T��A # 80 [ RL _ ��O� ];
C                           ***********************
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
C                    ( �E� "A��APATA" LOGYTR )
C
C                  C) ZAPROS = 3
C                     -----------
C                     �O��BO��TC� AHA��� HA BO�MO�H�� CMEH� COCTO�H��
C                     ��O�A [ OTKP�T <==> �AKP�T ]:
C                     �P� �TOM: FKOM = 0 - COCTO�H�E HE ��MEH��OC�;
C                               FCOM = 1 - HA TEK��EM �A�E COCTO�H�E
C                                         �O��HO "���O" ��MEH�T�C� B
C                                         B T = TX MOMEHT BPEMEH�;
C                                         PEA��HO B �PO�PAMME O�PE�E-
C                                         ��ETC� HX - �A� BO�MO�HO�
C                                         KOMM�TA���, �ATEM TX=TN+HX;
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
C              ��ET: ICLOSE - MO���� TOKA "�AKP�T��" BEHT���;
C                    UOPEN  - MO���� HA�P��EH�� "OTKP�T��" BEHT���;
C                             �T� �HA�EH�� ���T�BA�TC� KAK � �P�
C                             CMEHE COCTO�H�� ��EMEHTA, TAK � �P�
C                            O�PE�E�EH�� BO�MO�H�X BPEMEH �EPEK���EH��;
C
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - �P� TX_G  [MASPRT(35)] .EQ. TX[RMSW(19)] -
C                      ��KC�P�ETC� CMEHA COCTO�H�� "��O�A" �APAMETPOM
C                      KD [MSW(18)];
C                     PABEHCTBO �POBEP�ETC� C TO�. �O "EPSKOM".
C
C   METO�:         ��CKPET��A��� �PABHEH�� COCTO�H�� MO�E�� �AHHO�O
C   -----          T��A HA OCHOBE METO�A ��H���EPA B�O�����:
C                  Y(N+1) = Y(N) + H1*F(N+1) + H2*F(N).
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
C                4. �O��CKAETC� �A�AH�E L[0] � L[@] = 0.0;
C
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 80:
C                --------------------------------
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
C
C                  RMSW T��A # 80 [ RL _ ��O� ]
C                  --------------------------------
C
C
C
C                    SPM                      SRAB
C +----+---++---+---+---+--++----+---+-------+------+--------+--------+
C I��- I��-II R I L I R I L II G I J I ��A�  I TEK .I BPEM�  I PE�EPB I
C IE�  IE� II 0 I 0 I @ I @ II R I R I  KD   I COCT.I �EPEK�.I        I
C I"K" I"M"II   I   I   I   II L I L I       I "KD" I "TX"[*]I        I
C +----+---++---+---+---+---++---+---+-------+------+--------+--------+
C I 9  I 10II 11I 12I 13I 14II 15I 16I   17  I  18  I   19   I   20   I
C +----+---++---+---+---+---++---+---+-------+------+--------+--------+
C
C  0 - ��O� OTKP�T (KD=1); @ - ��O� �AKP�T (KD=0);
C [*] - XPAH�TC� �HA�EH�E PEA��HO B��O�HEHHO�O �A�A HX ==> TX = TN + HX
C ��A� KD - PA�PE�EH�E HA CMEH� COCTO�H��;
C
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
         INTEGER FKOM, ZAPROS
C===>    BCTPOEHHA� ��HK��� O�C����BAH�� �B�XMEPH�X MACC�BOB:
         ID(I,J) = NDALL * (J-1)  +  I
C---->   ���� �O�K���EH��:
         K = MSW(9)
         M = MSW(10)
C--->  KOHTPO�� T��A
         IF ( MSW(2) .NE. 80 ) GO TO 9999
C---> B��OP PE��MA:
         GO TO ( 667, 666, 600, 500       ), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
667      CONTINUE
C---------------Z A P R O S = 1----------------------------------------
C---> "C�POC" �HA�EH�� HX - BO�MO�HO�O BPEMEH� �EPEK���EH��.....
         RMSW(19) =      H0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(17) = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===> �TO �A COCTO�H�E ? (KD=0 -- �AKP�T, KD=1 -- OTKP�T)?
         IF (  MSW(18) .EQ. 1 ) GO TO 698
C===> ��O� �AKP�T==> KD=0 <======
         R = RMSW(13)
         L = RMSW(14)
         GO TO 555
698      CONTINUE
C===> ��O� OTKP�T ==> KD=1 <======
         R = RMSW(11)
         L = RMSW(12)
555      CONTINUE
C---> TE�EP� BCE E��HO....
C====> PAC�ET GRL  � JRL     �OPM��� ��MEHEH�:= MO�HO L = 0.0
         GRL = H1 / ( L + H1 * R )
C--->   TOK HA N - OM  �A�E
         ITOK = MSW(7)
         TOK  = TOKN( ITOK )
C--->
         JRL = (L*TOK) / (L+H1*R)
         IF ( H2 .EQ. 0.0 ) GO TO 668
C===>  HA�P��EH�E HA N-OM �A�E.
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
C--->
         JRL = JRL + H2*( (UN-R*TOK)/(L+H1*R) )
668      CONTINUE
C===> �A�OMH�T� G � J �APAMETP� ��O�A;
         RMSW(15) = GRL
         RMSW(16) = JRL
C--->
C--->   �O��EMEHTH�� BK�A�:
C---->
C======================================================================
         KOD = 0
C--->
         CALL DEPOST( K, M, GRL, JRL, YALL, JALL )
C--->
         RETURN
C======================================================================
C$$$$$$ZAPROS = 2$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
666      CONTINUE
C===> PAC��TAT� TOK�
         GRL = RMSW(15)
         JRL = RMSW(16)
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
C---> TOK B T(N+1) MOMEHT BPEMEH�:
         IRLT = GRL * UT  + JRL
C===>        �A��C� TOKOB
         ITOK = MSW(7)
         TOKN1( ITOK ) = IRLT
C********
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
600      CONTINUE
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 O�PE�E�EH�-------
C----O�PE�E��M TEK. � �PE�. COCTO�H�� (?)------------------------------
         ITOK = MSW(7)
C===>
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN1(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN1(M)
         UT = VK - VM
         IRLT = TOKN1( ITOK )
C--> A PAH��E ���O...
         VK = 0.0
         IF ( K .NE. 0 ) VK = VALLN(K)
         VM = 0.0
         IF ( M .NE. 0 ) VM = VALLN(M)
         UN = VK - VM
         IRLN = TOKN( ITOK )
C----�A�MEMC� COCTO�H��M�----------------------------------------------
C!!!-------------------------------------------------------------------
C      KD = MSW(18)
C      CALL SEQSOS( MSW(1), TN, KD, IRLN, UN )
C      MSW(18) = KD
C!!!-------------------------------------------------------------------
         KD = MSW(18)
         IF ( KD .EQ. 0 ) GO TO 601
C---> ��O� OTKP�T (KD=1), HE XO�ET �� OH �AKP�T�C�?
C        IF (  UN .GE 0.0  .AND.  IRLN .GE. 0.0
C    =                     .AND.  IRLT .LT. 0.0  ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
       IF (
     =                          IRLT .LE. -ICLOSE ) GO TO 602
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C---> �HA�E COCTO�H�E ��O�A HE ��MEH��OC�....
         KOD = 0
         RETURN
602      CONTINUE
C--->  ��O� #�AKP��C�#  B TE�EH�� BPEMEH� OT TN �O TN+H0....
C      KO��A �TO MO��O C����TC�?
C !!! �OPM��A C�PABE���BA �P�  IRLN > (-ICLOSE) !!!
         HX =    + H0 * ( (-ICLOSE - IRLN ) /(IRLT-IRLN)  )
         IF ( HX .LT. HXMIN ) HX = HXMIN
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
         IF (  HX .LE. 0.0 ) WRITE (NWTRE, 604 ) MSW(1), KD, TN, H0, HX
604     FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "RL-��O�" ==> COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     =   ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RMSW(19) = HX
C--->  EC�� TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMET�T� �TO B "FKOM"....
         FKOM = 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(17) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         KOD = 0
         RETURN
C----------------------------------------------------------------------
601      CONTINUE
C===> ��O� �AKP�T (KD=0), HE �E�AET �� OH OTKP�T�C�?....
C        IF ( UT .GT. 0.0 ) GO TO 603
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
       IF ( UT .GT. UOPEN ) GO TO 603
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C===>  HET, HE BO��E�A�, COCTO�H�E HE MEH�ETC�...
         KOD = 0
         RETURN
603      CONTINUE
C===>   ��O� B TE�EH�� TN  <-->  TN+H0 �O��EH OTKP�T�C� ...
C       KO��A ??? +!!!!!!!  C�PABE���BO ���  UN < UOPEN !!!!!!!!
         HX =    + H0 * ( (UOPEN - UN) /(UT - UN)  )
         IF ( HX .LT. HXMIN ) HX = HXMIN
C$$$$$$$$TX = TN + HX
C!!!-------------------------------------------------------------------
        IF ( HX   .LE. 0.0 ) WRITE (NWTRE, 605 ) MSW(1), KD, TN, H0, HX
605     FORMAT(' *** ��EMEHT # ', I5, ' MO�E�� "RL-��O�" ==> COMHEH�� ?'
     = , ' ***'/' *** KD = ', I2, ' TN = ', E12.5, ' H0 = ', E12.5,
     = ' HX = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RMSW(19) = HX
         HXG =  AMIN1( HX, HXG )
C               ****************
         TXG = TN + HXG
         FKOM = 1
         KOD = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         MSW(17) = -1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         RETURN
C----------------------------------------------------------------------
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
500      CONTINUE
         KOD = 0
C===> ��KCA��� CMEH� COCTO�H�� MO�E��.................ZAPROS = 4
C---->  HET �� HEO�XO��MOCT� �PO��BECT� CMEH� COCTO�H�� ��O�A...?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        IF ( MSW(17) .NE. -1 ) RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C       EC�� TXG   "�P�MEPHO" PABHO TX=MO�E��, TO CMEHA H��HA!
         HX   = RMSW(19)
         EPST = ABS( HX   - HXG )
C----->   (TX - TXG) = [ (TN+HX) - (TN+HXG) ]
         IF ( EPST .GT. EPSKOM ) RETURN
C===> �HA�E CMEH�M COCTO�H�E....
         KD = MSW(18)
         KD = KD + 1
         IF ( KD. EQ. 2 ) KD = 0
         MSW(18) = KD
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) MSW(1), KD, TN, TXG
501     FORMAT(' ***FTEST_1: B ��-TE #',I5, ' KD CTA�O =',I2,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0006 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
         END
