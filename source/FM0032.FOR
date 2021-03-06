       SUBROUTINE FM0032(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT    MA�    1988 �.)-----------
C   �O��PO�PAMMA: FM0032(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA�HA�EH�E:  HACTPO�KA MAKPOMO�E�� M�KPO�PO�ECCOPHO� C�CTEM�,
C   ----------   �OCTPOEHHO� HA �A�E M�KPO�PO�ECCOPA CEP��
C                K1801BM1(2,3), �ME��E�O C�CTEM� KOMAH� ��PM� "DEC".
C                ******************************************************
C                �P��HAK TIP # 5 -> �O�. MAKPOMO�E��.
C                ( DM0036,LM0013 )
C                      �OPM�P�ET "�EPEMEHH��" �ACT� RMSW O��CAH��
C
C                 ���HA  MSW  ==>+56
C----------------------------------------------------------------------
C
C        OP�AH��A��� �AM�T� B PEA��HO� M�KPO�PO�ECCOPHO� C�CTEME
C        � B MO�E��P���E� EE �PO�PAMME (MACC�B� MEM � MEM1)
C        -------------------------------------------------------
C
C        OP�AH��A��� �AM�T� B PEA��HO� M�KPO�PO�ECCOPHO� C�CTEME
C
C               C�OBO
C           �A�T      �A�T                      �A�T
C        *---------+---------*               *---------*
C        ! CTAP��� ! M�A���� ! 000000        ! M�A���� ! 000000
C        ! CTAP��� ! M�A���� ! 000002        ! CTAP��� ! 000001
C        ! CTAP��� ! M�A���� ! 000004        ! M�A���� ! 000002
C        ! CTAP��� ! M�A���� ! 000006        ! CTAP��� ! 000003
C        ! CTAP��� ! M�A���� ! 000010        ! M�A���� ! 000004
C        ! CTAP��� ! M�A���� ! 000012        ! CTAP��� ! 000005
C        ! CTAP��� ! M�A���� ! 000014        ! M�A���� ! 000006
C        ! ................. ! ......        ! CTAP��� ! 000007
C        ! ................. ! ......        ! ....... ! ......
C        ! ................. ! ......        ! ....... ! ......
C        ! CTAP��� ! M�A���� ! 003774        ! M�A���� ! 003776
C        ! CTAP��� ! M�A���� ! 003776        ! M�A���� ! 003777
C        *---------+---------*               *---------*
C
C
C        OP�AH��A��� �AM�T� B MO�E��P���E� �PO�PAMME
C            (MACC�B�   MEM  �  MEM1 )
C     *-------------------------------------*
C     ! C�OBO C A�PECOM 000000              ! 0001
C     ! CTAP��� �A�T C�OBA C A�PECOM 000000 ! 0002
C     ! C�OBO C A�PECOM 000002              ! 0003
C     ! CTAP��� �A�T C�OBA C A�PECOM 000002 ! 0004
C     ! C�OBO C A�PECOM 000004              ! 0005
C     ! CTAP��� �A�T C�OBA C A�PECOM 000004 ! 0006
C     ! C�OBO C A�PECOM 000006              ! 0007
C     ! CTAP��� �A�T C�OBA C A�PECOM 000006 ! 0008
C     ! C�OBO C A�PECOM 000010              ! 0009
C     ! CTAP��� �A�T C�OBA C A�PECOM 000010 ! 0010
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! C�OBO C A�PECOM 003776              ! 2047
C     ! CTAP��� �A�T C�OBA C A�PECOM 003776 ! 2048
C     *-------------------------------------*
C
C
C
C
C----------------------------------------------------------------------
C              ��EMEHT O��C�BAETC�:
C
C         � # T 380 � 0 �  #�� #CM
C                          FTAKT  ADRPPTI  ADRPRTO ADRPROG  P  ;
C
C         ��E
C          #�� #CM- HOMEP ��EMEHTA � HOMEP TOKA ,KO-P�� �O�K���AET
C                   K M�KPO�PO�ECCOP� ,KAK BXO�HO� C��HA�  ;
C          FTAKT  - TAKTOBA� �ACTOTA M�KPO�PO�ECCOPHO� C�CTEM� ;
C          ADRPRTI- A�PEC �OPTA BBO�A  B BOC�M����HOM B��E ;
C          ADRPRTO- A�PEC �OPTA B�BO�A B BOC�M����HOM B��E ;
C          ADRPROG- A�PEC ��E�K� O�� (B BOC�M����HOM B��E), �A���E
C                   KOTOPO�O HE PA�ME�A�TC� H� ��PAB����A� �PO�PAMMA
C                   H� �AHH�E ;
C          P   -    ��A� OT�A�O�HO� �E�AT�:
C                   P=0 - OT�A�O�HA� �E�AT� OTMEH�ETC� ,
C                   P=1 - OT�A�O�HA� �E�AT� �A�PE�AETC� - �OC�E
C                         B��O�HEH�� KA��O�O �A�A �PO�PAMM� B B�XO�HO�
C                         HA�OP �AHH�X B�BO��TC� CO�EP��MOE  POH
C                         CC�  �  �OPTA BBO�A-B�BO�A C A�PECOM ADRPORT;
C----------------------------------------------------------------------
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E��
C   -----
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
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C
C                   �OPMAT RMSW T�� # 380:
C                  -----------------------
C+-----------I--------------I------I-----------------------------------
C  # C�OBA   I    # C�OBA B I KTO  I
C OT HA�A�A  I    OTPE�KE   IO�PE�.I
C BCE�O MSW  I    RMSW B    I      I       O�O�HA�EH�E - �HA�EH�E
C O��CAH��   I    FM0032    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ���A ( BCE��A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I TX - BPEM� BO�MO�HO�O �EPEK���EH��;
C    11      I     2        I  DM  I KX - �KA�ATE�� ��MEHEH�� COCTO�H��
C            I              I      I      �OPTA  B/B HA �A�E ;
C    12      I     3        I  FM  I TTAKT - �EP�O� TAKTOBO� �ACTOT� ;
C    13      I     4        I  FM  I P - ��A� OT�A�O�HO� �E�AT�:
C            I              I      I     P=0 - OT�A�O�HA� �E�AT� OTK�.
C            I              I      I     P=1 - OT�A�O�HA� �E�AT� BK�.
C    14      I     5        I  FM  I           PE�EPB
C    15      I     6        I  FM  I           PE�EPB
C    16      I     7        I  DM  I KX - �KA�ATE�� ��MEHEH�� COCTO�H��
C            I              I      I      �OPTA B/B HA �A�E ;
C    17      I     8        I  DM  I TT - TEK��EE BPEM� ;
C    18      I     9        I  DM  I R0 - CO�EP��MOE POH R0 �P� T=TT ;
C    19      I    10        I  DM  I R1 - CO�EP��MOE POH R1 �P� T=TT ;
C    20      I    11        I  DM  I R2 - CO�EP��MOE POH R2 �P� T=TT ;
C    21      I    12        I  DM  I R3 - CO�EP��MOE POH R3 �P� T=TT ;
C    22      I    13        I  DM  I R4 - CO�EP��MOE POH R4 �P� T=TT ;
C    23      I    14        I  DM  I R5 - CO�EP��MOE POH R5 �P� T=TT ;
C    24      I    15        I  DM  I R6 - CO�EP��MOE POH R6 �P� T=TT ;
C    25      I    16        I  DM  I R7 - CO�EP��MOE POH R7 �P� T=TT ;
C    26      I    17        I  DM  I FLC-�HA�EH�E ��A�A "C" CC� �P� T=TT
C    27      I    18        I  DM  I FLV-�HA�EH�E ��A�A "V" CC� �P� T=TT
C    28      I    19        I  DM  I FLZ-�HA�EH�E ��A�A "Z" CC� �P� T=TT
C    29      I    20        I  DM  I FLN-�HA�EH�E ��A�A "N" CC� �P� T=TT
C    30      I    21        I  DM  I FLT-�HA�EH�E ��A�A "T" CC� �P� T=TT
C    31      I    22        I  DM  I FLIO-�HA�EH�E ��A�A "I/O" CC�
C            I              I      I             �P� T=TT ;
C    32      I    23        I  DM  I IPORT  -CO�EP��MOE �OPTA B�BO�A
C    33      I    24        I  DM  I IPORT1 -CO�EP��MOE �OPTA BBO�A
C            I              I      I             �P� T=TT ;
C    34      I    25        I  FM  I ADRPROG-MAKC�MA��H�� A�PEC O�� ;
C    35      I    26        I  FM  I ADRPRTO-A�PEC �OPTA B�BO�A;
C    36      I    27        I  FM  I ADRPRTI-A�PEC �OPTA BBO�A ;
C    37      I    28        I  DM  I TO - CTAPOE BPEM� ;
C    38      I    29        I  DM  I R0 - CO�EP��MOE POH R0 �P� T=TO ;
C    39      I    30        I  DM  I R1 - CO�EP��MOE POH R1 �P� T=TO ;
C    40      I    31        I  DM  I R2 - CO�EP��MOE POH R2 �P� T=TO ;
C    41      I    32        I  DM  I R3 - CO�EP��MOE POH R3 �P� T=TO ;
C    42      I    33        I  DM  I R4 - CO�EP��MOE POH R4 �P� T=TO ;
C    43      I    34        I  DM  I R5 - CO�EP��MOE POH R5 �P� T=TO ;
C    44      I    35        I  DM  I R6 - CO�EP��MOE POH R6 �P� T=TO ;
C    45      I    36        I  DM  I R7 - CO�EP��MOE POH R7 �P� T=TO ;
C    46      I    37        I  DM  I FLC-�HA�EH�E ��A�A "C" CC� �P� T=TO
C    47      I    38        I  DM  I FLV-�HA�EH�E ��A�A "V" CC� �P� T=TO
C    48      I    39        I  DM  I FLZ-�HA�EH�E ��A�A "Z" CC� �P� T=TO
C    49      I    40        I  DM  I FLN-�HA�EH�E ��A�A "N" CC� �P� T=TO
C    50      I    41        I  DM  I FLT-�HA�EH�E ��A�A "T" CC� �P� T=TO
C    51      I    42        I  DM  I FLIO-�HA�EH�E ��A�A "I/O" CC�
C            I              I      I             �P� T=TO ;
C    52      I    43        I  DM  I IPORT  -CO�EP��MOE �OPTA B�BO�A
C    53      I    44        I  DM  I IPORT1 -CO�EP��MOE �OPTA BBO�A
C            I              I      I             �P� T=TO ;
C    54      I    45        I  FM  I  #�� - HOMEP ��EMEHTA;
C    55      I    46        I  FM  I  #CM - HOMEP TOKA ��EMEHTA #�� ;
C    56      I    47        I  LM  I  ATOKM - A�P B TOKN/N1 �HA�EH��
C            I              I      I           TOKA HOMEPA #CM ;
C----------------------------------------------------------------------
C  O��A� ���HA MSW --> LM<BC�> = 56
C
C  ���HA �EPEMEH. �ACT� --> LM<RMSW> = 47
C
C   BH�MAH�E �O�ABK�*** :HA�A�H�� A�PEC CTEKA B MEM-MACC�BE 2046
C   ----------
C               �OBAB�� LAN  11/06/89.
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW, ADPRTI,ADPRTO, ADPROG
       NEL = BA0T(1)
       TIP = BA0T(2)
       IF (  TIP .EQ. 380 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** �O�. C�O� B FM0032 --> HOMEP T��A HE HA� ***'/
     =  ' *** HOMEP ��EMEHTA ', I5, ' HOMEP T��A ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->       M�KPO�PO�ECCOPHA� C�CTEMA HA �A�E M�KPO�PO�ECCOPA
C --->       CEP�� K1801BM1(2,3)
C----------------------------------------------------------------------
         IF ( FGMSW.EQ. 0 ) GOTO 11
C--> �TO�H�T� LM <O����>
         LM = 56
         RETURN
C----------------------------------------------------------------------
 11      CONTINUE
C-->  �P�H�T� � �TO MO�HO C�OPM�POBAT�..
       LM = 47
       DO 10 I=1,LM
10     RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> BBE�EM �AHH�E
           N = IFIX ( PAR0T(1) )
        NCM  =  IFIX ( PAR0T(2) )
       FTAKT = PAR0T(3)
       ADPRTI = IFIX ( PAR0T(4) )
       ADPRTO = IFIX ( PAR0T(5) )
       ADPROG = IFIX ( PAR0T(6) )
       P =  PAR0T(7)
C---> �POBEP�M � �A�OMH�M �AHH�E.
C
C--> �POBEP�M HOMEP #�� ..
       IF ( N .GE. NEL ) GO TO 9999
       RMSW( 45) = RN
       N = NCM
       RMSW(46) = RN
C---> �POBEP�M �ACTOT�.
       IF( FTAKT .GT. 0.0 ) GO TO 15
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 551 ) NEL, TIP
551    FORMAT(/' *** O���KA B FM0032 --> �ACTOTA HE �O�O��TE��HA  ***'/
     =         ' *** ��EMEHT  ',   I5,'      HOMEP T��A ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
15     CONTINUE
       RMSW(3) = 1./FTAKT
C--> �POBEP�M A�PEC �OPTA BBO�A
C--> �POBEP�M �PAB���HOCT� �A�AH�� KA��O�O PA�P��A A�PECA
CCCC   WRITE (6,*) ADPRTI
       CALL ADECQD(ADPRTI, IADD, I0, I1, I2, I3, I4, I5, IER )
       IF( IER .NE. 0 ) GO TO 20
       IF( I0  .LT. 0 .OR. I0 .GT. 7 ) GO TO 20
       IF( I1  .LT. 0 .OR. I1 .GT. 7 ) GO TO 20
       IF( I2  .LT. 0 .OR. I2 .GT. 7 ) GO TO 20
       IF( I3  .LT. 0 .OR. I3 .GT. 7 ) GO TO 20
       IF( I4  .LT. 0 .OR. I4 .GT. 7 ) GO TO 20
       IF( I5  .LT. 0 .OR. I5 .GT. 7 ) GO TO 20
       IF( ADPRTI .LT. 160000 .OR. ADPRTI .GT. 177777 ) GO TO 20
C--> �A�OMH�M..
       N = IADD
       RMSW(27) = RN
       GO TO 22
20     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 552 ) NEL, TIP
552    FORMAT(/' *** O���KA B FM0032 --> HE�PAB���HO �A�AH A�PEC  ***'/
     =         ' *** �OPTA BBO�A                           ***'/
     =         ' *** ��EMEHT  ',   I5,'      HOMEP T��A ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
22     CONTINUE
C--> �POBEP�M A�PEC �OPTA B�BO�A
C--> �POBEP�M �PAB���HOCT� �A�AH�� KA��O�O PA�P��A A�PECA
       CALL ADECQD(ADPRTO, IADD, I0, I1, I2, I3, I4, I5, IER )
       IF( IER .NE. 0 ) GO TO 21
       IF( I0  .LT. 0 .OR. I0 .GT. 7 ) GO TO 21
       IF( I1  .LT. 0 .OR. I1 .GT. 7 ) GO TO 21
       IF( I2  .LT. 0 .OR. I2 .GT. 7 ) GO TO 21
       IF( I3  .LT. 0 .OR. I3 .GT. 7 ) GO TO 21
       IF( I4  .LT. 0 .OR. I4 .GT. 7 ) GO TO 21
       IF( I5  .LT. 0 .OR. I5 .GT. 7 ) GO TO 21
       IF( ADPRTO .LT. 160000 .OR. ADPRTO .GT. 177777 ) GO TO 21
C--> �A�OMH�M..
       N = IADD
       RMSW(26) = RN
       GO TO 25
21     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 553 ) NEL, TIP
553    FORMAT(/' *** O���KA B FM0032 --> HE�PAB���HO �A�AH A�PEC  ***'/
     =         ' *** �OPTA B�BO�A                           ***'/
     =         ' *** ��EMEHT  ',   I5,'      HOMEP T��A ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
 25    CONTINUE
C--> �POBEP�M MAKC�MA��H�� A�PEC O��
C--> �POBEP�M �PAB���HOCT� �A�AH�� KA��O�O PA�P��A A�PECA
       CALL ADECQD(ADPROG, IADPRG, I0, I1, I2, I3, I4, I5, IER )
       IF( IER .NE. 0 ) GO TO 30
       IF( I0  .LT. 0 .OR. I0 .GT. 7 ) GO TO 30
       IF( I1  .LT. 0 .OR. I1 .GT. 7 ) GO TO 30
       IF( I2  .LT. 0 .OR. I2 .GT. 7 ) GO TO 30
       IF( I3  .LT. 0 .OR. I3 .GT. 7 ) GO TO 30
       IF( I4  .LT. 0 .OR. I4 .GT. 7 ) GO TO 30
       IF( I5  .LT. 0 .OR. I5 .GT. 7 ) GO TO 30
       IF( ADPROG .LT. 0 .OR. ADPROG .GT. 3777 ) GO TO 30
C--> �A�OMH�M..
       N = IADPRG
       RMSW(25) = RN
C----------------------------------------------------------------------
C---> �CTAHAB��BAEM HA�A�. �HA�EH�E �KA�ATE�� CTEKA
       RMSW(15) = RN
       GO TO 35
30     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 5522) NEL, TIP
5522   FORMAT(/' *** O���KA B FM0032 --> HE�PAB���HO �A�AH        ***'/
     =         ' *** MAKC�MA��H�� A�PEC O��                       ***'/
     =         ' *** ��EMEHT  ',   I5,'      HOMEP T��A ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
35     CONTINUE
C--> �PO�O���M..
       N = IFIX(P)
       RMSW(4) = RN
C======================================================================
C--> TE�EP� BBE�EM B O�� M�KPO�PO�ECCOPHO� C�CTEM� ��PAB������
C--> �PO�PAMM� B BOC�M�P��HOM KO�E, A �ATEM �PEO�PA��EM EE B
C--> �EC�T��H�� �OPM�.
C======================================================================
C--> O�H���M
       DO 80 I=1,2048
       MEM1(I) = 0
80     MEM(I)  = 0
C--> NPROG - KAHA� BBO�A �PO�PAMM�.
       NPROG = 22
C--> BBO��M
C***   READ(NPROG,600,END=100) (MEM1(I),I=1,2048)
       DO 400 I=1,2048
       READ(NPROG,600,END=100) MEM1(I)
600    FORMAT(8X,I6)
C      IF(MEM1(I).EQ.0)  I1=I1+1
C      IF(I1.GT.0) GOTO 100
400    CONTINUE
       I = 2049
100    CONTINUE
       WRITE (6,600) I
       WRITE (6,600) (MEM1(K),K=1,I)
       IK = I - 1
       IF( (2*IK-1) .GT. IADPRG ) GO TO 200
C--> �EPEBO��M B �EC�T��H�� �OPM�
       DO 150 I=1,IK
       KQ = MEM1(I)
       CALL ADECQD(KQ, KD, KQ0, KQ1, KQ2, KQ3, KQ4, KQ5, IER)
       IF( IER. NE. 0 ) GO TO 210
       IF( KQ0  .LT. 0 .OR. KQ0 .GT. 7 ) GO TO 210
       IF( KQ1  .LT. 0 .OR. KQ1 .GT. 7 ) GO TO 210
       IF( KQ2  .LT. 0 .OR. KQ2 .GT. 7 ) GO TO 210
       IF( KQ3  .LT. 0 .OR. KQ3 .GT. 7 ) GO TO 210
       IF( KQ4  .LT. 0 .OR. KQ4 .GT. 7 ) GO TO 210
       IF( KQ5  .LT. 0 .OR. KQ5 .GT. 7 ) GO TO 210
C--> �A�OMH�M BCE C�OBO
       MEM(2*I-1) = KD
C--> �A�OMH�M CTAP��� �A�T
       MEM(2*I) = KD/256
150    CONTINUE
CCCC   WRITE (6,*) RMSW(15),RMSW(3)
       RETURN
200    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 554 ) NEL, TIP
554    FORMAT(/' *** O���KA B FM0032 --> ��PAB����A� �PO�PAMMA    ***'/
     =         ' *** �AH�MAET O��ACT� �AM�T� �O���E �A�AHHO�      ***'/
     =         ' *** ��EMEHT  ',   I5,'      HOMEP T��A ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
210    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) I, NEL, TIP
555    FORMAT(/' *** O���KA B FM0032 --> O���O�HA� KOMAH�A B      ***'/
     =         ' *** �PO�PAMME ��� M�KPO�PO�ECCOPA B CTPOKE',I4,' ***'/
     =         ' *** ��EMEHT  ',   I5,'      HOMEP T��A ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
CCCC   WRITE (6,*) RMSW,MSW
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 556 ) NEL, TIP
556    FORMAT(/' *** O���KA B FM0032 -->HE�PAB���HO �A�AH� BXO�H�E �AHH�
     =E   ***'/' *** HA BXO� ��EMEHTA ', I5, ' HOMEP T��A ', I5,' ***',
     =         ' *** �O�AETC� TOK ��EMEHTA, �ME��E�O �O����� �OP��KOB��
     =HOMEP   *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
CCCCC  DEBUG INIT
       END
