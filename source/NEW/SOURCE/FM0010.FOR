       SUBROUTINE FM0010(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: FM0010(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:   HACTPO�KA MAKPOMO�E�� T��A # 40
C   ----------   PE����TOP BO�����EH�� 3X3X0 ��. MA��H�;
C                ***************************************
C                �.TIP # 4 -> �O�. MAKPOMO�E��.
C                ( DM0014, LM0001 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C                      ( ���HA :==> +1(VAR) )
C
C
C              ��EMEHT O��C�BAETC�:
C
C      � # T 40 � KA KB KC
C          � T_BK� U_�OTO�. #��-TA_��._MA��H�_3X3X0
C              PSUB #P < .........>;
C
C              �EPE� A��APAT PSUB �A�A�TC� �AKOH� ��PA��EH�� "H��E";
C
C      ��E
C              KA, KB, KC - ���� �E��, �P�H�T�E �A O�OPH�� C�CTEM�
C                           HA�P��EH�� ( CET� �ECKOHE�HO� MO�HOCT� );
C              T_BK� - BPEM�, HA��HA� C KOTOPO�O BK�. PE�. BO����.
C              U_�OT. - �OTO�O�HOE HA�P��EH�E HA B�XO�E PE����TOPA
C                      ( O�PAH���BA�TC� "C�H�CO���" B�X. HA�P��EH�� );
C              #��. MA�. - HOMEP ��EMEHTA, O��C�BA���� ��. MA��H�,
C                          KOTOP�� PE����P�EM;
C
C      B�BO�H�E BE����H� B MACC�BE TOKN/TOKN1:
C
C      U_R_��_A, U_R_��_B, U_R_��_C,  -->  HA�P��EH�� ��PAB�EH��
C                                          B C�CTEME OCE� POTOPA;
C     OME�A_O�POHA�, S_CKO���EH�E,
C
C    U_Q_��P.  U_D_��P;
C
C   I_S_D_�A�,   S_CKO���EH�E_�A�,  INT(ISD-ISD_ZAD),  INT(S-S_ZAD),
C    (#16)       (#17)               (#18)               (#19)
C    PE�EPB;
C    (#20)                                 -->   BCE�O 20 �HA�EH��;
C
C*****  �AHHA� MO�E�� O�C����BAET MAKPOMO�E�� C T��.# 11, 110, 12, 120;
C
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E��
C   -----
C                      ��� PSUB2 --> "��BA�BAET" BBE�EHH�E �O���OBATE�EM
C                                     �HA�EH�� ��� KTI � KTS;
C
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW �P� FGMSW = 0;
C                  LM - ���HA BCE�O MSW O��CAH��      �P� FGMSW = 1;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. KO�-BO �APAMETPOB B �/� PSUB HE KOHTPO��P.
C                     HET HA �TO BPEMEH� ( MOE�O !!!)
C
C                4.��� O��CAH�� �AKOHOB ��PAB�EH�� PEA���OBAHA PSUB1-->
C
C
C      1) .........��� O��CAH�� �AKOHOB ��PAB�EH�� PEA���OBAHA PSUB1-->
C
C      PSUB 1 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD XAF  >;
C              ( 8-M� �APAMETPOB )
C
C
C      2) .........��� O��CAH�� �AKOHOB ��PAB�EH�� PEA���OBAHA PSUB2-->
C
C      PSUB 2 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD   XAF
C
C      KTI  TI1 ISZAD_1  TI2 ISZAD_2 .....
C                        I         I
C                        I<---*--->I
C
C      KTS  TS1 S_1      TS2    S_2  ...... >;
C                        I         I
C                        I         I
C                        I<---*--->I
C
C
C    ��E:
C          -*-  �BA �HA�EH��, O��C�BA���E O�H� TO�K� O�PE�E�EH��
C               COOTBETCTB���E� ��HK��OHA��HO� �AB�C�MOCT� -->
C               OHA �A�AETC� KAK K�CO�HO-��HE�HA� ��HK���;
C
C       KTI  - KO�-BO TO�EK ��� O�PE�E�EH�� �AB�C�MOCT� ISZAD OT "TN";
C
C       KTS  - KO�-BO TO�EK ��� O�PE�E�EH�� �AB�C�MOCT� SZAD  OT "TN";
C
C
C  PSUB1 --->  B���C��ET:
C  *****
C     UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* ( KE+1 ) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]* ( KE+1 ) - KE* XAF*IRQ0;
C
C  ��E:  INT - �HTE�PA�;
C
C
C  PSUB2 --->  B���C��ET:
C  *****
C     UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* ( KE+1 ) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]* ( KE+1 ) - KE* XAF*IRQ0;
C
C  ��E:  INT - �HTE�PA�, A ISDZAD � SZAD - K�CO�HO-��HE�HA� ��HK���
C                                                     B P E M E H �
C                                                     *************
C
C                  5. H��E �P�BO��TC� �OPMAT� RMSW:
C
C              �OPMAT RMSW T�� # 40:
C              ---------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I       O�O�HA�EH�E - �HA�EH�E
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I
C O��CAH��   I    FM0010    I      I
C            I              I      I
C------------I--------------I------I-----------------------------------
C     9      I    -2        I      I HOMEP ���A KA ( �A�A A )
C    10      I    -1        I      I HOMEP ���A KB ( �A�A B )
C    11      I     0        I      I HOMEP ���A KC ( �A�A C )
C----------------------------------------------------------------------
C    12      I     1        I  DM  I  FQ - ��A� HOB�E �HA�. B���C�EH�
C            I              I      I   ( ��� C�HXPO. MAKPO. P.TIP # 4
C    13      I     2        I  FM  I  NELT - HOMEP ��. PE����P. ��. MA�.
C    14      I     3        I  LM  I  AMOT - A�P. B MSWALL O��CAH��
C            I              I      I         PE�. ��. MA��H�
C    15      I     4        I  LM  I  APOT - A�PEC B MSWALL ���OB MA��H�
C    16      I     5        I  LM  I  ATOKM - A�P B TOKN/N1 BE����H,
C            I              I      I          PAC��T. ��� ��. MA�.
C    17      I     6        I  FM  I  TBKL - BPEM�, HA��HA� C KOTOPO�O
C            I              I      I         BK���AETC� PE����TOP;
C    18,19   I     7,8      I      I  PE�EPB.
C    20      I     9        I  FM  I  UPOT - �OTO�O�HOE HA�P��EH�E
C    21      I     10       I  FM  I  NSUB - HOMEP �/� �O���OBATE��
C22,23,24,25 I11,12,13,14,15I      I  PA�. MECTO ��� �HT. 1 � 2 B TOKN;
C  27        I    16        I  FM  I  NPAR - KO�-BO �APAMETPOB,
C            I              I      I         BBE�EHH�X B PSUB<..>
C OT 28 �O   I OT 17 �O     I FM   I  �APAMETP� PSUB, �A�AHH�E
C 28 + NPAR  I 17 + NPAR    I      I  �O���OBATE�EM
C            I 26 -->       I FM   I  KTI ��� PSUB2 ....
C            I 27+2*KTI --> I FM   I  KTS ��� PSUB2 ....
C            I              I      I
C            I              I      I
C            I              I      I
C----------------------------------------------------------------------
C
C  O��A� ���HA MSW --> LM<BC�> = 28+NPAR
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 17+NPAR
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RNELT, NELT )
       EQUIVALENCE ( RNPSUB,  NPSUB )
       EQUIVALENCE ( RNPAR,   NPAR  )
       INTEGER NETL, NPSUB, NPAR, TIP, BA0T, FGMSW, AKTI, AKTS
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 40 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0010 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  �TO�H�T� LM <O���� >
C  PAR0T -> TBKL, UPOT, #��. -NSUB, NPAR, ..�APAMETP�..... KTI     KTS
C            1     2     3     4     5     6....           15 .....
       LM = 28 + IFIX( PAR0T(5) )
       RETURN
C######################################################################
1000   CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       NPAR = IFIX( PAR0T(5) )
       LM = 17 + NPAR
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C--->
       RMSW(16) = FLOAT( NPAR )
C-->   !!!!!!!  = RNPAR
       NELT = IFIX( PAR0T(3) )
       RMSW(2) = RNELT
C-->
       UPOT = PAR0T(2)
       RMSW(9) = UPOT
C-->
       TBKL = PAR0T(1)
       RMSW(6) = TBKL
C-->
       NPSUB =  - IFIX( PAR0T(4) )
       RMSW(10) = RNPSUB
C-->  �APAMETP� PSUB--->
       DO 3 I=1, NPAR
3      RMSW(17-1 +I) = PAR0T( 6-1 + I)
C
C
C######################################################################
       IF ( NPSUB .EQ. 1 ) GO TO 20
       IF ( NPSUB .EQ. 2 ) GO TO 10
C!!!-------------------------------------------------------------------
       WRITE(NWTR,  502 ) NEL, TIP, NPSUB
       WRITE(NWTRE, 502 ) NEL, TIP, NPSUB
502    FORMAT(/' *** ��� ��EMEHTA ', I5, ' ( T�� ', I3, ' ) PSUB ', I2,
     =   ' HE PEA���OBAHA ***'/ )
       STOP 667
C!!!-------------------------------------------------------------------
10     CONTINUE
C-->  ��BO�M KTI  � KTS -->
       IF( NPAR .LE. 9 ) GO TO 20
       KTI = IFIX( PAR0T(15) )
       KTI2 = 2*KTI
       RMSW(26) = FLOAT( KTI2 )
C-->  ��E ��BET KTS....
       KTS = IFIX( PAR0T( 16 + 2*KTI ) )
       KTS2 = KTS * 2
       RMSW(27 + 2*KTI ) = FLOAT( KTS2 )
C######################################################################
20     CONTINUE
       NPAR17 = NPAR + 17 - 1
       WRITE(NWTR,300) NEL, TIP, NPSUB, NPAR, (RMSW(I), I=17, NPAR17)
300    FORMAT(/' *** ��EMEHT ', I5, '   T�� ', I3, '   PSUB ', I2,
     =  22X,   '     ***'/
     = ' *** �A�AH� �APAMETP� B KO���ECTBE ', I3, ' �T. :', 21X,'***'/
     = ' ***      KE=', G12.5, ' URD00=', G12.5, ' KI=', G12.5,
     = '     ***'/
     = ' *** ISD_ZAD=', G12.5, ' URQ00=', G12.5, ' KS=', G12.5,
     = '     ***'/
     =' *** BETA=',G12.5,'  S_ZAD=',G12.5,' XAF=',G12.5, 6X,'***'/
     = ' ***', 20X,'�O�O�H�TE��HO �A�AHO:', 20X, '***'/
     =     5(1X, G12.5, 1X) )
       WRITE(NWTR, 301 )
301    FORMAT(/)
C----------------------------------------------------------------------
       RETURN
       END
