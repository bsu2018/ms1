         SUBROUTINE GETNEW( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
     *          YALL, JALL, LOGYTR, WORKX, IRC, YALLHP, WORKY )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  GETNEW( MSWALL, TOKN, TOKN1, VALLN, VALLN1,
C   ------------    YALL, JALL, LOGYTR, WORKX, IRC, YALLHP, WORKY )
C
C   HA�HA�EH�E:    - B���C�EH�E O�EPE�HO� TO�K� "COCTO�H��" �E��:
C   -----------    *** ��� T = TN + HXG O�PE�E���TC� TOKN1 � VALLN1 ***
C
C              == �P� IPOL = 0 --> O���HA� �HTEP�O�����, ��HE�HA�;
C                 �P� IPOL = 1 --> C�E��A��HA�, ��� METO�A ��H.-B�O�.;
C ***�ETO 1987********** BO�EBOE PE�EH�E: �P� DX<ZEROCP, DX=0.0 *****
C
C                      - �P� �A�POCE (FRUN0 = 1) ---> B��O�H�ETC�
C                        TECT P�H�E, �P� �TOM �O��CKAETC�
C                        HE �O�EE NRUNG "�PO��EH��" H0DATA;
C                         HXMIN "�PO��TC�" C�HXPOHHO;
C                      - B C���AE, H0 > H0_OLD -> TECT �POBO��TC� BHE
C                        �AB�C�MOCT� OT FRUN0;
C
C                      - �P� PONRUN = 1 (CM. MASPRT) B�BO��TC� C�E����EE
C                        TN  <CTAPTOBA� TO�KA>
C                        TN1 <"CTAPA�" TO�KA �P� TN+H0>
C                        TNX1 <HOBA� TO�KA  �P� TN+H0/2>
C                        TNX2 <HOBA� TO�KA  �P� TN+H0/2+H0/2>
C
C
C                      *** �P� FRUNGE = 0 TECT H� KO��A HE �POBO��TC� **
C                      *** �P� FRUNGE = 2 TECT �POBO��TC� HA KA��OM   **
C                                               �A�E                ****
C
C
C              == �P� KODSYS = 1 --> PE�EH�E C�A� �E� �TEPA���;
C
C              == �P� KODSYS = 2 --> PE�EH�E C�A� C   �TEPA���M�;
C                                    EPSSYS--> TO�HOCT�;
C                      ( "K�COK" -->  "C@C�A�@" );
C
C
C                  - CMEHA COCTO�H�� BEHT���H�X ��EMEHTOB, BO�����EH�E
C                    ��A�A FKOM B C���AE HA����� KOMM�TA��� BEHT��E�...
C                    [ �P� �OBTOPE PAC�ETA C TO�K� TN, HEO�XO��MO HE
C                      ��MEH�� �HA�EH�� FKOM, HXG � HX B MSWALL MO�E�E�
C                      "BEPH�T�" BEHT��� B COCTO�H�E �P� T = TN,
C                      ��� �E�O BOC�O���OBAT�C� �/� RESETX..].
C
C                  - �P� HEO�XO��MOCT� B TN = T0 ( CM. MASPRT )
C                    �POBE�EH�E �POCECCA "O�PE�E�EH�� �CXO�HO�O
C                    COCTO�H�� BEHT���H�X ��EMEHTOB", A TAK�E B C���AE
C                    TXG == TN, T.E. �P� HXG = 0.0 ==> �PO��BO��TC�
C                    SEQTIM BO�BPATOB B "CTAPTOBY�" TO�KY TN PAC�ETA:
C                    �P� �TOM -> B�BO� �H�OPMA��OHHO�O COO��EH��;
C                   ( C.�.K. SEQTIM - KOHTPO�� "�A��K��BAH��" );
C                    �T� C�T�A��� BO�MO�H� �P� HXMIN = 0.0;
C
C                  - �P� B�PO��EH�� C�CTEM� - �PO�CXO��T 10-�
C                    KPATHA� �O��TKA ���AB�TC� OT �TO� C�T�A���;
C                    �TO BO�MO�HO �P� BK���EH�� ��A�A SEQTRX � �P�
C                    �C�OB�� HE PABEHCTBA 0.0 �POBO��MOCT� GVENT6;
C                    BHE �AB�C�MOCT� OT ��A�A SEQTRX, �P� PAC�ETE TO�K�
C                    T = T0, KOHTPO�� B�PO��EHHOCT� C�CTEM� BK��AETC�;
C
C                  - �P� �A�POCAX OT DM... MOH�TOPOB �TEPA��OHH�X
C                    �TO�HEH�� �EPEMEHH�X COCTO�H�� [ TOKN1 ],
C                    B��O�H�ETC� HE �O�EE MITER �TEPA���;
C
C                - KA���� DM MOH�TOP �.T��A 2 & 3 �P� ZAPROS=2 B��O�H�ET
C                  CPABHEH�E �PE�CKA�AHHO�O �HA�EH�� BEKTOPA "TOK"
C                  (XPAH�TC� B "TOKN1") C B���C�EHH�M �CXO�� ��
C                  �HA�EH�� VALLN1;
C                B C���AE, KO��A EPS=ABS(TOK_� - TOKN1)/MAX(TOK_�,TOKN1)
C                  > EITER --> KOD=1 � �P� MAXITR<EPS -->
C                 ��KC�P�ETC� HOBOE �HA�EH�E MAXITR=EPS � NLITER=N_��-TA
C
C                - **** �TO�HEH�E COCTO�H�� ***: �TO-->
C                  BO�����EH�E ��A�A FITER, �TO TPE��ET OT DM MOH�TOPOB
C                  �.T��A 2 & 3 �P� B���C�EH�� G-J �APAMETPOB �C�PO��-
C                  �OBAT� HE �PE�CKA�AHH�E �HA�EH�� "TOK", A �HA�EH��
C                  "TOK", XPAH���EC� B MACC�BE "TOKN1";
C
C                - �P� MITER = 0  -->  KOHTPO�� HA KOD ^=0 HET !!!!!!!!!
C
C
C   METO�:         - ��CKPET��A��� �PABHEH�� COCTO�H�� MAKPOMO�E�E�
C   -----            HA OCHOBE METO�A ��H���EPA-B�O����� (H2^=0) ���
C                    HE �BHO�O METO�A ���EPA (H2=0), COCTAB�EH�E C�CTEM�
C                    ��HE�H�X A��E�. �P-�� � �X PE�EH�E;
C                    ��PAB�EH�E COCTO�H�EM BEHT���H�X ��EMEHTOB HA
C                    OCHOBE �H�OPMA��� ��� TN � TN+H0 MOMEHTA BPEMEH�;
C                    �HTEP�O����� �P� HEO�XO��MOCT� TOKOB � HA�P��EH��;
C
C                 - �P� �POBE�EH�� TECTA P�H�E -> �C�O����ETC�
C                   PA�O��� �A�� HA KAHA�E NFILER; CTEK ���
C                   �B�X TO�EK PE�EH�� �P� T = TN  � T = TN1;
C                   KOHTPO�� �OK. O���K� - �O BEKTOPAM ���. HA�P��EH��;
C
C
C                  - "B�BO�" BEHT��E� B HA�A��HOE COCTO�H�E
C                    OC��ECTB��ETC� B TOM C���AE, KO��A PE��M AHA��-
C                    ��P�EMO� �E��, �A�AHH�� �O���OBATE�EM, HE COOTBET-
C                    CTB�ET "HA�A��HOM�" COCTO�H�� BEHT��E�.
C                    ( T.E. EC�� HA �A�E OT "TN=T0" �O "TN+H0"
C                      HEKOTOP�E BEHT��� ��MEH��T CBOE COCTO�H�E
C                      TAK, �TO TX (BPEM� BO�MO�HO� KOMM�TA���) = T0,
C                      TO B��O�H�ETC� "HACTPO�KA" �T�X BEHT���H�X
C                      ��EMEHTOB, KOTOPA� �AK���AETC� B CMEHE �X
C                      COCTO�H��; �ATEM TO�KA TN+H0 PAC��T�BAETC�
C                      �AHOBO. �TA �PO�E��PA B��O�H�ETC� HE     O��H
C                      PA�, HO TO��KO B TO�KE T = T0 ).
C
C          ==     B �AB�C�MOCT� OT METVET[MASPRT(107)] HXG O�PE�E��ETC�:
C              =0 - �HTEP�O����� K TXG ME��� "TN"  �  "TN1 =TN+H0";
C              =1 - �EPEC�ET K TXG OT TN C H0=HXG;
C                   �P� �TOM HXMIN - MIN BO�MO�H�� �A�;
C              =2 - "C���AH�E" "TN1" C "TXG",
C                   �P� �TOM CMEHA COCTO�H�� BEHT��E� C
C                    EPSKOM = TN1 - TXG
C              >2 - TO �E, �TO � =2, HO EPSKOM HE MO������P�ETC�;
C
C          ==  �P� METVET=1 � HXMIN=0 --> PAC�ET KAK ��� METVET=0  ===
C
C
C   �APAMETP�:     MSWALL - C�OPM�POBAHH�� C��COK C�OB COCTO�H��
C   ---------               MO�E�E� ��EMEHTOB �E��;
C                  VALLN, TOKN - HA�P��EH�� � "TOK�" �E�� �P�
C                                T = T0, T.E. B HA�A��H�� MOMEHT BPEMEH�
C                                B �OC�E�CTB�� - B TEK���� "CTAPTOB��"
C                                MOMEHT BPEMEH� T.E. B T = TN;
C                  VALLN1 � TOKN1 - TO �E ���  TN = TN + HXG  ( H0 );
C                  YALL - MATP��� ���OB�X �POBO��MOCTE�;
C                  JALL - BEKTOP �CTO�H�KOB TOKA;
C                  LOGYTR - MACC�B �P��HAKOB K.�. ���OB;
C                           ( ��� MAKPOMO�E�E� ��EA��H�X BEHT��E� );
C                 WORKX - PA�. MACC�B, PA�MEPHOCT�� HE MEHEE NSYS;
C                  IRC = 1 TO�KA PAC��TAHHA, �P� FKOM = 1 COCTO�H��
C                          BEHT��E� ��MEHEH�;
C                      = 2 ��� MO�E�E� C �.T��OM #2 �/��� #3 �A MITER
C                          �TEPA��� HE �OCT��H�TA TO�HOCT� EITER;
C                          ( �TA C�T�A��� B V.M=2.0 ��HOP�P�ETC� )
C                      = 3 - TECT P�H�E HE B��O�HEH, TO�HOCT� HE
C                            �OCT��H�TA �A�E �OC�E NRUNGE
C                            PA� "�PO��EH��" H0DATA;
C
C                      �P� KODSYS = 2 HEO�XO��MO:
C                      WORKY --> PA�MEPHOCT�  <= NSYS;
C                      YALLHP --> PA�MEPHOCT� NDALL * NDALL;
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, PROF3;
C   -------------  2. GETSYS,   MODVAL,  DECOM0+SOLVE0
C                  3. PRINT4, RSLMC, FACTR;
C   �P�ME�AH�E:
C   ----------
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(1)
         COMMON /PROF3/ FUNTAB(1)
         DIMENSION MSWALL(1), TOKN(1), TOKN1(1), VALLN(1), VALLN1(1)
         DIMENSION YALL(1), JALL(1), LOGYTR(1)
         REAL JALL, MU
         INTEGER FEND, FKOM, ZAPROS, T5MSW, T6MSW, PON, FLOG
         EQUIVALENCE ( MASPRT(29), TN ),
     =               ( MASPRT(30), H0 ),
     =               ( MASPRT(31), MU ),
     =               ( MASPRT(32), H1 ),
     =               ( MASPRT(33), H2 ),
     =               ( MASPRT(50), ZEROCP ),
     =               ( MASPRT(13), NWTR ),
     =               ( MASPRT(14), NWTRE ),
     =               ( MASPRT(35), TXG ),
     =               ( MASPRT(23), NDALL ),
     =               ( MASPRT(37), FLOG ),
     =               ( MASPRT(26), NSYS ),
     =               ( MASPRT(34), FKOM ),
     =               ( MASPRT(41), T5MSW ),
     =               ( MASPRT(49), T6MSW ),
     =               ( MASPRT(46), NNTOK ),
     =               ( MASPRT(56), PON   ),
     =               ( MASPRT(42), T0    ),
     =               ( MASPRT(28), ZAPROS )
         EQUIVALENCE ( HXG, MASPRT(61) )
         EQUIVALENCE ( GVENT6, MASPRT(59) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( MITER,  MASPRT(66) )
         INTEGER FITER, SEQTRX
         EQUIVALENCE ( SEQTRX, MASPRT(64) )
         EQUIVALENCE ( SEQTIM, MASPRT(71) )
         EQUIVALENCE ( NDIVH0, MASPRT(84) )
         EQUIVALENCE ( NFILER, MASPRT(83) )
         EQUIVALENCE ( FRUN0,  MASPRT(82) )
         EQUIVALENCE ( NRUNGE, MASPRT(70) )
         EQUIVALENCE ( ERUNGE, MASPRT(69) )
         EQUIVALENCE ( H0OLD,  MASPRT(81) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( FRUNGE, MASPRT(68) )
         EQUIVALENCE ( H0DATA, MASPRT(40) )
         EQUIVALENCE ( PONRUN, MASPRT(86) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( PPVENT, MASPRT(106) )
         EQUIVALENCE ( EPSKOM, MASPRT(39)  )
         EQUIVALENCE ( HXMIN,  MASPRT(62)  )
         EQUIVALENCE ( METVET, MASPRT(107)  )
         EQUIVALENCE ( IPOL,   MASPRT(108)  )
         EQUIVALENCE ( KODSYS, MASPRT(109)  )
         EQUIVALENCE ( EPSSYS, MASPRT(110)  )
         EQUIVALENCE ( KODMU,  MASPRT(113)  )
         EQUIVALENCE ( KODMUM,  MASPRT(114)  )
         INTEGER PPVENT
         DIMENSION WORKX(1), WORKY(1), YALLHP(1)
         REAL MAXITR
         INTEGER CASE, FRUN0, FRUNGE, PONRUN, CASE1
         INTEGER SEQTIM
C
C--->  �P� SEQTRX = 1  -  BK���AETC� KOHTPO�� HA B�PO��EHHOCT�;
C
         ID(I,J) = NDALL * (J-1) + I
C====>
         DATA IHELPV/ 0 /, IHELP6/0/, K10/10/
         MAXITR = 0.0
         NLITER = 0
        CASE = 0
        CASE1 = 0
        FKOM = 0
C       ********
100    CONTINUE
         IHELP6 = 0
         IHELPV = 0
C--->  KOHTPO�� BK���M:
         COND = 0.0
C-->  �P� SEQTRX = 0, B�K����M:
         IF ( SEQTRX .EQ. 0 ) COND = -1.0
C--> �P� TN = T0, KOHTPO�� BK����M �P�H���TE��HO...
         IF ( T0 .EQ. TN ) COND = 0.0
C---> NITER - C.�.K. �TEPA���, FITER - ��A� �TEPA���....
         NITER = 0
         FITER = 0
         IRC = 1
C======================================================================
C--->   �A��KC�P�EM BBE�EHHOE �HA�EH�E GVENT6.......
         HELP6 = GVENT6
         GVENT6 = 0.0
52       CONTINUE
C----------------------------------------------------------------------
C==>  C�OPM�POBAT� C�CTEM�
         CALL GETSYS( MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1 )
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C--->>>   C � A �  <<<-------------------------------------------------
C --  METKA  201  --> �TEP. METO�.
       GO TO ( 200, 201 ), KODSYS
       CALL SUBERR(KODSYS)
200    CONTINUE
C
C   PE��T� C�CTEM� < DECOM0 + SOLVE0 >
C===> IPVT  <==> VALLN1
C===> WORK  <==> WORKX
         CALL DECOM0( NDALL, NSYS, YALL, COND, VALLN1, WORKX  )
C
         IF ( COND .NE. COND+1. ) GO TO 1
C--->>  ABAP��....
202    CONTINUE
C---> EC�� MO�EM - �TO �E�O ��TAEMC� �C�PAB�T�....
         IF ( HELP6 .EQ. 0.0 ) GO TO 15
C---->  EC�� ��EA��H�X BEHT��E� HET, TO � �C�PAB��T� HE�E�O.....
         IF ( FLOG .EQ. 0 ) GO TO 15
C---> "B�KHEM"
         T1HELP = TN + H0
         WRITE(NWTRE, 16 )         TN, H0,  T1HELP
16       FORMAT(' *** B TO�KE: TN = ', G12.5, ' H0 = ', G12.5,
     =           ' TN1 = ', G12.5, '     ***'/
     =' ***  C�CTEMA [Y] * [V] = [J] B_�_P_O_�_�_E_H_A ',24X,  ' ***')
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( IHELP6 .GT. K10 ) GO TO 15
         IHELP6 = IHELP6 + 1
         RHELP6 = IHELP6
         GVENT6 = RHELP6 * HELP6
         WRITE(NWTRE,14) GVENT6
14       FORMAT(' *** �POBO��MOCT� �AKP�T�X BEHT��E� �P�H�MA�TC� PABH�M�
     =', E12.5, ' [C�M]***' )
         GO TO 52
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
15       CONTINUE
C!!!-------------------------------------------------------------------
C  C�CTEMA B�PO��EHA �ATA��HO...............
         WRITE(NWTRE,10) TN, H0
         WRITE(NWTR ,10) TN, H0
10       FORMAT(//' *** C�CTEMA B�PO��EHA  �_A_T_A_�_�_H_O   ***'/
     =          ' *** TN = ', E12.5, '  H0 = ', G12.5, ' ***')
C-->
C----------------------------------------------------------------------
C==>  C�OPM�POBAT� C�CTEM�  ��� �E�AT�....
         CALL GETSYS( MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1 )
C----------------------------------------------------------------------
C-->
       CALL PRINT4( YALL, JALL, LOGYTR )
C-->
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
1        CONTINUE
C===> PE��T� C�CTEM�
         CALL SOLVE0( NDALL, NSYS, YALL, JALL, VALLN1, IRC )
C---> IRC - �OKA HE AHA����P�EM.....
C===> PE�EH�E B VALLN1.....
         DO 20 I=1,NSYS
           VALLN1(I) = JALL(I)
20       CONTINUE
C
C--->> C�A� PE�EH�.........
203    CONTINUE
C--->  BOCTAHOB�T� CTAPOE GVENT6
         GVENT6 = HELP6
C===> B���EH �� A��APAT FLOG ?
         IF ( FLOG .EQ. 0 ) GO TO 60
C---> O�PE�E��T� TOK� OTKP�T�X BEHT��E�... ( B TOKN1 )
         KF = 3
         ZAPROS = 20
         CALL CALLDM(KF, MSWALL, YALL, JALL, LOGYTR,
     =               TOKN, TOKN1, VALLN, VALLN1, KODMD )
C--->
C===> MO������P�EM PE�EH�E CO��ACHO LOGYTR.......
         CALL MODVAL( VALLN1, LOGYTR )
60       CONTINUE
C==> O�PE�E��M HOB�E TOK� HA ��EMEHTAX "�OCTO�HHO�" CTP�KT�P�.....
         ZAPROS = 2
C===> O�POC BCEX DM...., KPOME ��EA��H�X BEHT��E�
         KF = 5
         CALL CALLDM(KF, MSWALL, YALL, JALL, LOGYTR,
     =               TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
C--->  �P� MITER = 0, KOHTPO�� HET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       IF ( MITER .EQ. 0 ) GO TO 61
C--->
C---> ECT� �� �A�POC� HA "�TEPA���"
         IF ( KODMD . EQ. 0 ) GO TO 61
C---> �POC�T �TEPA���.....
C======================================================================
C    MAKPOMO�E�� �.T��A #2 �/��� #3   �A�POC��� �O�ABO�H�E
C              �TEPA��� ��� �TO�HEH�� �PE�CKA�AHH�X �HA�EH�� "TOK"
C              B �TOM C���AE KODMD ^= 0
C
C        HEO�XO��MO:
C O�POC�T� DM... MOH�TOP� C ZAPROS = 1 �P� BO�����EH�� FITER = 1 ��A�A;
C DM... MOH�TOP� B �TOM C���AE �O��H� �C�O���OBAT� HE �PE�CKA�AHH�E
C �M� �HA�EH�� "TOK", A �HA�EH�� �� MACC�BA "TOKN1"
C
C --->  BK���AEM FITER
         FITER = 1
C--->  ��KC�P�EM �TEPA���
         NITER = NITER + 1
C--->  �OBTOP�EM, EC�� �TO BO�MO�HO....
         IF ( NITER .LE.  MITER ) GO TO 52
C--> �P�EXA��.....
         WRITE(NWTRE, 62 ) MITER, EITER, TN, H0
62     FORMAT(' *** B MO�E��X �.T��A 2 �/��� 3 �A ', I3,' �TEPA��� ',
     =     25X, '***'/
     =' *** TO�HOCT� ', G12.5, ' HE �OCT��H�TA ( TN=', G10.5,
     = ' H0=',G12.5, ' ***')
       WRITE(NWTRE,63) MAXITR, NLITER, NSITER
63     FORMAT(' *** MAX. �O�PE�HOCT� ', G12.5,' BHOC�T ��EMEHT # ',
     =  I5, ' (CM.#', I3, ') ', 5X, '***' )
C--->  �TO �OKA ��HOP�P�EM....!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IRC = 2
C---> DIGITA - MO�ET �OBTOP�T� PAC�ET �P� MEH��EM H0..........
C
C======================================================================
C
61       CONTINUE
C--->  ��A�EK, TO C�POC�M...
         FITER = 0
C
C===> TOKN  � TOKN1 O�PE�E�EH� ��� BCEX ��EMEHTOB...�P� T => TN I TN1
C      VALLN  �  VALLN1 ��BECTH�.....
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C   T_E_C_T_____P_�_H_�_E
C
700    CONTINUE
       GO TO ( 701, 704 ), CASE
C-->  H��EH �� TECT?
C
C***>  �P� FRUNGE = 2 --->  TECT�POBAH�E HA KA��OM �A�E !!!!!!!!!!!!!!!
C
C
       IF ( FRUNGE .EQ. 0 ) GO TO 799
       IF ( FRUNGE .EQ. 2 ) GO TO 710
C-->  �P� FRUN0 = 1 ��� �P� H0 > H0OLD  TECT�P�EM....
       IF ( FRUN0 .EQ. 0  .AND.  H0 .LE. H0OLD ) GO TO 799
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
710    CONTINUE
C-->  ��KC. �EPEMEHH�E
       H0OLD = H0
       H1OLD = H1
       H2OLD = H2
       TNOLD = TN
       TN1OLD = TN + H0
C----------------------------------------------------------------------
        KAKA5=5
       IF ( PONRUN .EQ. 1  .AND.  PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5,  TNOLD, TOKN, VALLN )
       IF ( PONRUN .EQ. 1  .AND.  PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5,  TN1OLD, TOKN1, VALLN1 )
C----------------------------------------------------------------------
C-->  ��KC. TO�K� B CTEKE HA KAHA�E NFILER...
       REWIND NFILER
       WRITE(NFILER) TN1OLD, ( VALLN1(I), I=1,NSYS)
       WRITE(NFILER) TN1OLD, ( TOKN1(I), I=1,NNTOK)
       WRITE(NFILER) TNOLD, ( VALLN(I), I=1,NSYS)
       WRITE(NFILER) TNOLD, (  TOKN(I), I=1,NNTOK)
       END FILE NFILER
C-->  ��MEH�EM �A�
C-->
       H0 = H0OLD / 2.0
       H1 = H1OLD / 2.0
       H2 = H2OLD / 2.0
C-->  C�ET HOBO� TO�K�  ��� TN + H0/2, ��KCA��� B VALLN1 � TOKN1...
       CASE = 1
       GO TO 100
701    CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C--->  TO�KA ��� T = TN + H0/2  ---> B VALLN1 � TOKN1
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  ��KC�P�EM
       TN = TN + H0
       DO 702 I = 1, NSYS
702    VALLN(I) = VALLN1(I)
       DO 703 I = 1, NNTOK
703    TOKN(I) = TOKN1(I)
C----------------------------------------------------------------------
       IF ( PONRUN .EQ. 1  .AND. PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5, TN,      TOKN, VALLN )
C----------------------------------------------------------------------
C-->  PAC�ET ��� T = TN + H0/2 + H0/2  "HOB��" B�P�AHT..
       CASE = 2
       GO TO 100
704    CONTINUE
        CASE = 0
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C   TO�KA  ���  T = TN +H0/2 +H0/2  -->  B VALLN1 � TOKN1
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
        TN1X = TN + H0
C----------------------------------------------------------------------
       IF ( PONRUN .EQ. 1  .AND. PON .EQ. 1 )
     =                      CALL DRPR01(KAKA5,   TN1X,  TOKN1, VALLN1 )
C----------------------------------------------------------------------
C--->  B���C��M �O�PE�HOCT�...
        REWIND NFILER
C-->  ��TAEM B JALL  CTAPOE VALLN1...
       READ(NFILER) TXX, ( JALL(I), I=1,NSYS)
C-->  CPABHEH��
       EPSMAX = 0.0
       IMAX = 0
       DO 714 I = 1, NSYS
       X1 = ABS( VALLN1(I) )
       X2 = ABS( JALL  (I) )
       IF ( X1 .LE. ZEPOCP  .OR.  X2 .LE. ZEROCP ) GO TO 780
       XZ = AMAX1( X1, X2 )
       EPS = ABS(X1 - X2) / XZ
       GO TO 781
780    CONTINUE
C-->  O�HO �� ��CE� ����KO K H���.....
       EPS = ABS( X1 - X2 )
781    CONTINUE
       IF ( EPS .LT. EPSMAX ) GO TO 714
C-->  ��KC�P�EM...
       IMAX = I
       EPSMAX = EPS
714    CONTINUE
C-->           �TO�OB�� KOHTPO��....
       IF (  EPSMAX .LE. ERUNGE ) GO TO 705
C-->  TECT HE �PO�E�.....
       WRITE( NWTRE, 706 ) TNOLD, TN1OLD, IMAX, EPSMAX, ERUNGE
706    FORMAT(' *** HA [', G12.5, ' -:- ', G12.5, ' ]  ��� ���A # ',
     =  I5, ' ***'/
     = ' *** �OK. �O�PE�HOCT� ', G10.5, ' �O���E �A�AHHO� ', G10.5,
     =  ' ***' )
C-->CMEHA �A�A � TO�K�, MAKET HOBO� TO�K� ��� T = TN(CTAPOE) + H0OLD/2
C-->
       DO 707 I=1,NSYS
707    VALLN1(I) = VALLN(I)
       DO 708 I=1, NNTOK
708    TOKN1(I) = TOKN(I)
C-->  BOCTAHOB�M "CTAP�E" TOKN � VALLN �� NFILER_A
C-->  �PO��CT�M �A��C� O� TOKN1
       READ(NFILER)
C-->  BOCTAHAB��BAEM...
       READ(NFILER) TXX, (VALLN(I), I=1, NSYS )
       READ(NFILER) TXX, (TOKN(I), I=1, NNTOK )
       TN = TNOLD
C--> �O��PEP��M "C�����" BPEMEH�....
       HXG = H0
       TXG = TN + H0
C-->  �MEH���M H0DATA....
       H0DATA = H0DATA / 2.0
       HXMIN = HXMIN / 2.0
C--> H1, H0, H2 --> ��E ��MEHEH�..
       XX = H0DATA * 1.8E+4
       WRITE(NWTRE, 709 ) H0DATA, XX
709    FORMAT(' *** �A�AHH�� �A� H0DATA ��MEHEH HA ', G12.5,
     = ' (',G10.5,' ) ***')
C-->  KOHTPO�� �A��K�.
       NDIVH0 = NDIVH0 + 1
       IF ( NDIVH0 .LE. NRUNGE ) GO TO 710
C-->  �P�EXA��.....
       WRITE(NWTRE, 711) NRUNGE, H0DATA
711    FORMAT( /' *** �PEB��EHO �O�. KO�-BO "�PO��EH��" �A�A H0DATA ',
     = I5, ' ***'/
     = ' *** "TECT P�H�E" OTK���AETC�, PAC�ET BE�ETC� C H0DATA = ',
     = G12.5, ' ***'/)
       WRITE(NWTR, 711) NRUNGE, H0DATA
       FRUNGE = 0
       IRC = 3
C-->  MAKET -->  TECT �PO�E�....
       GO TO 705
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
705    CONTINUE
C-->  TECT �PO�E�....
C-->  BOCTAHOB�EH�E "��O�A��HOE"
       REWIND NFILER
       READ(NFILER) XX, ( VALLN1(I), I=1,NSYS)
       READ(NFILER) XX, ( TOKN1 (I), I=1,NNTOK)
       READ(NFILER) XX, ( VALLN (I), I=1,NSYS)
       READ(NFILER) XX, ( TOKN  (I), I=1,NNTOK)
C-->
       TN = TNOLD
       H0 = H0OLD
       H1 = H1 OLD
       H2 = H2 OLD
       HXG = H0
       TXG = TN + H0
C-->  BCE....
799    CONTINUE
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C==> ���A �� KOMM�TA���?( EC�� T5MSW � T6MSW = 0, TO HET HEO�XO��MOCT� )
C    �EPEO�PE�E��T� TX_G BPEM� PEA��HO�O �A�A...)
C
         IF ( T5MSW .EQ. 0  .AND. T6MSW .EQ. 0 ) GO TO 3
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       FKOM = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C===> TX_G  HA�T�...
C--->  ��EM KOM-�...
         ZAPROS = 3
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
3        CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       GO TO ( 851 ), CASE1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C====>  ���A  ��  K O M � T A � � � ?????
         IF ( FKOM .EQ. 0 ) GO TO 40
C######################################################################
C  O � P A � O T K A   K O M � T A � � � ..............................
C######################################################################
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  OT 21.10.86.
       IF ( METVET .EQ. 0 ) GO TO 820
       IF ( METVET .EQ. 1 ) GO TO 850
       IF ( METVET .EQ. 2 ) GO TO 880
C@@@ �HA�E KAK  = 2, HO �E� MO�. EPSKOM @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       GO TO 890
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
820    CONTINUE
C***** � H T E P � O � � � � �   **************************************
C===>  ��MEH�T� COCTO�H�E BEHT��E�...
         ZAPROS = 4
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
C******> �CTAHOB�T� "HE�BH�� ���EP", T.K. B �TOM C���AE FKOM = 1
C  �P� �C�OB�� KODMU=0 ...........
         IF ( KODMU .EQ. 1 ) GO TO 54321
         H1 = H0
         H2 = 0.0
54321    CONTINUE
C
C
C
C
C**********************************************************************
C**********************************************************************
C====> "HACTPO�KA" BEHT��E�====>  TO��KO ��� �A�A T0 <===> T0+H0
C====>  EC�� TX = T0, TO �O��TKA PAC�ETA �E��  OT CTAPO�
C====> "CTAPTOBO�" TO�K�, �P� �TOM COCTO�H�� BEHT��E� ��E ��MEHEH�;
C====>  �TA �O�E��PA B��O�H�ETC� #HE CTPO�O# O��H PA� ��� TN = T0 �
C====>  BCE�O SEQTIM PA� ��� TN ^= T0:
C######################################################################
C######################################################################
         IF ( ABS(TXG-T0) .GT. ZEROCP ) GO TO 50
C===>   #�OMO�AEM#  � �OBTOP�EM, EC�� �TO �P� T  = TN
         TXG = T0 + H0
         HXG = H0
         GO TO 52
50       CONTINUE
C!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( HXG .NE. 0.0 ) GO TO 55
C---> "�P��KCOB�BAH�E" !!!COO��EH�E ��OK�POBAHO (13.01.87)!!!
         TN1 = TN + H0
C        WRITE(NWTRE, 56 ) TN1,     TN
56       FORMAT(            ' *** �P� TN1=', G12.5, ' "C�POC" B "CTAPTOB
     =��" TO�K� TN=',G12.5,' ***')
         IHELPV = IHELPV + 1
         TXG = TN + H0
         HXG = H0
         IF ( IHELPV .LE. SEQTIM ) GO TO 52
C-@@@@@@@
         WRITE(NWTRE, 57) SEQTIM, TN, METVET
         WRITE(NWTR,  57) SEQTIM, TN, METVET
57    FORMAT(//' *** B��O�HEHO ', I5,' "C�POCOB" PAC�ETA B "CTAPTOB��"',
     =  ' TO�K� TN=', G12.5, ' ***'/
     = ' *** PAC�ET �PEP�BAETC� -> METVET=', I3, '     ***'///)
         CALL SUBERR(0)
55       CONTINUE
C!!!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C**********************************************************************
C@@@@
       IF ( METVET .GE. 2 ) GO TO 856
C          @@@@@@@@@@@@@@@
C----------------------------------------------------------------------
C**********************************************************************
C===>   �HTEP�O����� [TOKN]  <=> [TOKN1]  B  [TOKN1]
C===>   �HTEP�O����� [VALLN] <=> [VALLN1] B  [VALLN1]
C
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       IF ( IPOL .EQ. 0 ) GO TO 900
C-->> C�E�. �HTEP�O�. �� TN <-> TN1 B TXG ( XPAH�T� B "N1" )
       D1X = (H1 - HXG) / H0
       D2X = (H2 - HXG) / H0
       DO 901 I = 1, NSYS
       VN1 = VALLN1(I)
       VN  = VALLN (I)
       VALLN1(I) = D1X * VN1 + D2X * VN  - HXG * ( VN1 - VN )
901    CONTINUE
C---
       DO 902 I=1, NNTOK
       TTN1 = TOKN1(I)
       TTN  = TOKN (I)
       TOKN1(I) = D1X * TTN1 + D2X * TTN - HXG * ( TTN1 - TTN )
902    CONTINUE
       GO TO 903
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
900    CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C  KOHTPO�� �HTEP�O����� ...
       IF ( HXG .GT. H0 ) GO TO 800
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C*******>DTX = (TX - TN ) / H0
         DTX = HXG / H0
         DO 4 I = 1, NSYS
         VN = VALLN(I)
         DVN1 = VALLN1(I) - VN
C******** BO�EBOE PE�EH�E:
         IF ( ABS(DVN1) .LT. ZEROCP ) DVN1 = 0.0
         VALLN1(I) = VN + DVN1 * DTX
4        CONTINUE
C----------------------------------------------------------------------
         DO 5 I=1,NNTOK
         TTN = TOKN(I)
         DTN1 = TOKN1(I) - TTN
C******** BO�EBOE PE�EH�E:
         IF ( ABS(DTN1) .LT. ZEROCP ) DTN1 = 0.0
         TOKN1(I) = TTN + DTN1 * DTX
5        CONTINUE
C*******BCE********
903    CONTINUE
802    CONTINUE
C!!!-------------------------------------------------------------------
         TN1 = TN + HXG
         IF ( PON .EQ. 1  .AND. PPVENT .EQ. 1 )
     =                     CALL DRPR01( 4, TN1, TOKN1, VALLN1 )
C!!!-------------------------------------------------------------------
         RETURN
40       CONTINUE
C===> KOM�TA��� HE ���O....
C     TOKN1  �   VALLN1  ���  T  =  TN + HXG  O�PE�E�EH�
         RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
800    CONTINUE
       TXG = TN + HXG
       WRITE(NWTRE,801)  TN, H0, TXG, HXG, METVET
801    FORMAT( ' *** ABAP�� B ��OKE �HTEP�O�����...[GETNEW]:',
     =  5X, '(METVET=', I2, 10X, ' ***'/
     = ' *** TN=',E12.5,' H0=',E12.5,' TXG=',E12.5,' HXG=',E12.5,
     = ' ***'  )
       GO TO 802
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
850    CONTINUE
C@@@ �EPEC�ET....
C@@  COCTO�H�E HE MEH�EM, A C��TAEM C H0 = HXG.....
       IF ( HXG .EQ. 0.0 ) GO TO 820
       CASE1 = 1
       H0M1 = H0
       H0 = HXG
       TXG = TN + H0
       GO TO 52
C-->  �O CASE1 ....
851    CONTINUE
       H0 = H0M1
C-->  �EPEC��TA�� K TO�KE TN+HXG ....
       IF ( FKOM .EQ. 1 ) GO TO 852
C***
       WRITE(NWTRE, 85344 ) TN, HXG, METVET, HXMIN, EPSKOM
85344  FORMAT(' *** �PO��CK KOMM�TA��� TN=', G12.5, ' HXG=',G12.5,
     = ' ***'/
     = ' *** METVET=', I3, ' HXMIN=',G12.5, ' EPSKOM=', G12.5, ' ***')
       GO TO 853
C-->  ��MEH�M COCTO�H�E BEHT��E�...
852    CONTINUE
C=
         ZAPROS = 4
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
853    CONTINUE
C-->  �E�AT� "KOMM�TA���" :::
       TN1 = TN + HXG
       IF ( PON .EQ. 1 .AND. PPVENT .EQ. 1 )
     =                 CALL DRPR01( 4, TN1, TOKN1, VALLN1 )
       CASE1 = 0
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
880    CONTINUE
C-->  "C���AH�E"
C-->  HOB�� EPSKOM....
       EPSOLD = EPSKOM
C-->   EPSKOM = (TN+H0) - (TN+HXG)
       EPSKOM = H0 - HXG
       IF ( EPSKOM .LT. 0.0 ) WRITE(NWTRE,8805) TN, H0, HXG, EPSKOM
8805   FORMAT(' *** TN=',G12.5,' H0=',G12.5,' HXG=',G12.5,
     =         ' EPSKOM=', G12.5, ' ***')
885    CONTINUE
C===>  ��MEH�T� COCTO�H�E BEHT��E�...
         ZAPROS = 4
         KF = 2
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODMD )
C
       EPSKOM = EPSOLD
C-->  �A�EE TAK �E KAK ��� METVET=0    KPOME �HTEP�O�����..
       GO TO 50
856    CONTINUE
C-->  B "N1" PE����TAT ... �E�ATAEM...
       TN1 = TN + HXG
       IF ( PON .EQ. 1 .AND. PPVENT .EQ. 1 )
     =      CALL DRPR01( 4, TN1, TOKN1, VALLN1 )
C@@@@@ C � � � A H � E  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       HXG = H0
       TXG = TN + HXG
       RETURN
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
890    CONTINUE
C-->  �E� MO�. EPSKOM_A...
       EPSOLD = EPSKOM
       GO TO 885
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C
C@C�A�@ ---------------------------------------------------------------
201    CONTINUE
C-->  C�A� C �TEP. �TO�.
C  YALLHP  --> �CXO�HA�, YALL - �EKOM�O��POBAHA�...
C
C-->  XPAH�M...
       DO 205 I=1,NSYS
       DO 206 J=1,NSYS
       IDHH = ID(I,J)
       YALLHP(IDHH) = YALL(IDHH)
206    CONTINUE
205    CONTINUE
C-->  YALL = L * U,  PER -> WORKY
       CALL FACTR( YALL, WORKY, NSYS, NDALL, IERFAC )
C
       IF ( IERFAC .EQ. 0 ) GO TO 207
C-->  TO�HOE B�PA��EH�E
       WRITE(NWTRE, 2000) TN
2000   FORMAT(' *** YALL HE �AKTOP���ETC� TN=',G12.5, ' ***' )
C
       GO TO 202
207    CONTINUE
C-->  �PO��EM PE�AT�..
       EPSXXX = EPSSYS
       CALL RSLMC(YALLHP, YALL, JALL, VALLN1, NSYS, EPSXXX,
     =            IERRSL, NDALL, WORKX, WORKY )
C
C-->  ???
       IF ( IERRSL .EQ. 0 ) GO TO 203
C
       GO TO (211, 212, 213, 214 ), IERRSL
       CALL SUBERR(IERRSL )
211    CONTINUE
C-->  TO�HA TO��KO HOPMA..
C@@@@@@WRITE(NWTRE,2001)     TN
2001   FORMAT(' *** B C�A� TO�HOCT� -> TO��KO ���',
     =  ' HOPM� PE�EH�� TN=',G12.5,  ' ***' )
C-->  ��HOP�P�EM..
       GO TO 203
212    CONTINUE
C-->  TO�HOCT� HOPM� H��E �A�.
       WRITE(NWTRE,2002)         EPSXXX, TN
2002   FORMAT(' *** B C�A�',
     =  ' PE�EH�E C TO�HOCT�� =',G12.5, ' TN=',G12.5,   ' ***' )
C-->  IGNORE
       GO TO 203
213    CONTINUE
C-->  HET CM�C�A..
       WRITE(NWTRE,2003) TN
2003   FORMAT(' *** PE�EH�E C�A� HE �MEET CM�C�A TN=', G12.5, ' ***' )
       GO TO 202
C
214    CONTINUE
C-->  YALL = L*U  -->  B U --> 0-�� B ��A�OHA�E...
       WRITE(NWTRE,2004) TN
2004   FORMAT(' *** C�A� -> H��� B ��A�OHA�� [U] TN=',G12.5, ' ***')
       GO TO 202
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         END
