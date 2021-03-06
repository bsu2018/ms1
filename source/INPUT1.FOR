         SUBROUTINE INPUT1( MSWALL, VT, UT, IT, IRC )
CXXX----KOM��EKC "MS1" -->
C
C      �O��PO�PAMMA: INPUT1( MSWALL, VT, UT, IT, IRC )
C     --------------
C
C      HA�HA�EH�E:   BBO� �CXO�HO� �H�OPMA��� "�A�POC HA B�BO�",
C      ----------    �A�O�HEH�E MACC�BOB VT, UT, IT;
C
C                    *** O�PA�OT��K �AHH�X ��PEKT�B #TAB, #�E�, #�PA�,
C                                                              #�PA�1;
C                    �OPMAT �O���ECKO� CTPOK� O��CAH��:
C                            ( BBO� �E� �OPMATH�� )
C
C                  "V" K1 K2 ... "U" K1  K2  ...
C                   "I" N1 N2 ... N5"."S1 .... N6"."S2 ....
C                  "GAMMA" N1 N2 ... "OMEGA" N1 N2 .....
C                   "M�M" N1 N2 ... "MBH" N1 N2 .......";"
C                   "C��"  N1  N2 .... ;
C
C                  (��KB� B " " KO��P��TC� KAK �OKA�AHO)
C
C
C                  N1 N2 ...- HOMEPA ��EMEHTOB  B �E��;
C
C                  K1 K2 ... - HOMEPA ���OB �O�K���EH��;
C
C                  S1 S2 ... - "CME�EH�E" HOMEPA �EPEMEHHO� B
C                               MACC�BE TOKN � TOKN1 ��� �KA�AHHO�O
C                               ��EMEHTA;
C
C                     B O � M O � H O :
C
C                  A) �A�AT� HE �O�EE IRC-T� BE�E��H ��� B�BO�A �O
C                     ��PEKT�BAM #�E�, "�PA�, #TA�   �  HE �O�EE 2-X -
C                     �O �EPEKT�BE #�PA�1;
C
C     METO�:       C�HTAKC��ECK�� � CEMAHT��ECK��  KOHTPO�� BBO��M�X
C     -----        �AHH�X, �POBEPKA  COOTBETCTB�� �H�OPMA�OHH�X
C                  O��CAH�� �O��CT�M�M.
C                  �OPM�POBAH�E � B��A�A �H�OPMA��OHH�X COO��EH��;
C
C    �APAMETP�:   C���E�H�E �OKA��H�E MACC�B�:
C    ---------    VT - HOMEPA ���OB ( 1 + 1*9 = 10 );
C                 UT - HOMEPA �AP ���OB ( 1 + 2*9 = 19 );
C                 IT - HOMEPA "��EMEHT"_"CME�EH�E" ( 1 + 2*9 = 19 );
C                  *** �EPB�� ��EMEHT �T�X MACC�BOB - KO�-BO �AH�T�X ***
C
C                  �P� BXO�E:
C                  IRC = MAX. KO���ECTBO �AHH�X B �A�POCE ( OT 1 �O 9 );
C                  �P� B�XO�E:
C                 IRC = 1 --> O���OK HET;
C                     = 2 --> �ACT� O��CAH�� ��HOP�POBA�OC� ��-�A
C                             O���OK;
C                     = 3 --> KOHE� �AHH�X HA NRDR;
C
C                  OCTA��H�E �APAMETP� B���PA�TC� �� O���X ��OKOB
C                  PROF1, PROF2, PROF3;
C
C    �O��PO�PAMM�: 1. HA�OP �O��PO�PAMM C �PE��KCOM APF...;
C    ------------  2. COMMON: PROF1, PROF2, PROF3;
C                  3. MSGOUT, SEQMS0, SEQNT0, APFTRT, APFSQI;
C
C   05.04.89 --> �E�AT� �P� DIAL=1 !!!
C   �P�ME�AH�E:1.�P� O���KE B O��CAH�� - ��HOP. �ACT� �O�. �O� CTPOK�;
C   ---------- 2.�P� PA�OTE B �TEPAKT�BHOM PE��ME - �PO��BO��TC� �A�POC
C                HA �OBTOP O���O�HO� �O�. CTPOK�;
C              2A. �C�P. APFSQ. - BBO� ��CT. (MUDATSKAQ PROGAMMA INPUT0)
C              ****��� 1990 �.�஢�ઠ �� ���. ���祭�� �� ���ᨢ��
C                  �� /SERVZ8/ �� �� �㫥��� ���祭�� 䫠��
C                  MASPRT(121) <==> FTRSPL
C
c
C              3.PAC�O�O�EH�E �H�OPMA��� B MACC�BAX:
C
C
C                            VT � UT
C                            -------
C +------------+------------------------.....
C ! KO���ECTBO !  HOMEP   ! HOMEP      !.....
C !�C�O����EM�X! ���A     !  ���A      !.....
C ! C�OB       !          !            !.....
C +------------+------------------------.....
C !     1      !    1     !   2        !.....
C !------------!----------!------------!.....
C
C                             IT
C                             ---
C    +---------+---------------------------+--------------------+...
C    ! KO�-BO  !  HOMEP ��EMEHTA           !  �HA�EH�E          !...
C    ! �C�O�.  !                           ! "CME�EH��"         !...
C    ! C�OB    !                           !                    !...
C    +---------+---------------------------+--------------------!...
C    !    1    !             2             !          3         !...
C    +---------+---------------------------+----------+---------!...
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
         DIMENSION VT(1), UT(1), IT(1)
         INTEGER VT, UT, FLAGX, DIAL, EOFRDR
         EQUIVALENCE ( NWTR,  MASPRT(13) ),
     =               ( NWTRE, MASPRT(14) ),
     =               ( NRDR,  MASPRT(12) ),
     =               ( DIAL,  MASPRT(15) ),
     =               ( NMKNOT, MASPRT(7) ),
     =               ( NZRDR, MASPRT(127) ),
     =               ( FTRSPL, MASPRT(121))
         INTEGER FKAR, SUM, PRT90, ER, BLANK, POINT, CIU
         DIMENSION KAR(80), FKAR(80), ER(80), LG(5), LO(5),
     =             LMFT(3), LMBH(3), MSG(25), CIU(3)
         DIMENSION MSWALL(1)
         INTEGER SUMER, TIP
         INTEGER FTRSPL, SPISNN, SPISVL, SPISTK
C---->
         DATA K9/9/, K72/72/, K2/2/
         DATA LV/'V'/, LU/'U'/, LI/'I'/, CIU/'C','�','�'/
         DATA  LG/'G', 'A', 'M', 'M', 'A'/
         DATA  LO/'O', 'M', 'E', 'G', 'A'/
         DATA  LMFT/ 'M', '�', 'M' /
         DATA  LMBH/ 'M', 'B', 'H' /
         DATA LGOPA/'$'/, LKON/';'/, BLANK/' '/, POINT/'.'/
         DATA EOFRDR/0/
         SUMER = 0
         KK_19 = IRC*2+1
C----> HA�A��H�E �CTAHOBK�
         DO 10001 I = 1,KK_19
         UT(I) = 0
         IT(I) = 0
         IF ( I .GT. IRC+1 ) GO TO 10001
         VT(I) = 0
10001    CONTINUE
C---> C.�.K. *****
         SUM = 0
         KSUM = IRC
C        IF ( KSUM .GT. K9 ) KSUM = K9
         IF ( KSUM .LE. 0 ) KSUM = K2
         DO 10000 I = 1,80
10000    ER(I) = BLANK
         MSG(1) =0
         IPRT = 0
         IPRTX = 0
         IGNORE = 0
         IK = K72 + 1
         IER = 0
         PRT90 = 0
         IRC = 1
C--->
C######################################################################
C######################################################################
C
C  B � � T �
C  O�EPE�HO�            METKA  10
C  C�MBO�               ---------
C
C######################################################################
C######################################################################
10       CONTINUE
         IF ( KAR(IK) .EQ. LKON ) GO TO 9998
         IF ( IGNORE .EQ. 1 ) IPRT = 0
         IK = IK + 1
         IF ( IK .LE. K72 ) GO TO 1001
C----> EC�� ���� O���K�  --> CKA�EM O� �TOM
         IF ( IER .EQ. 0 ) GO TO 1002
C=======> ��O�A��H�� C.�.K. O���OK
         SUMER = SUMER + IER
C---> �E�AT� O���OK
         IF ( DIAL .EQ. 1 ) WRITE ( NWTR, 1003 ) (ER(I), I=1,80)
1003     FORMAT(80A1/' *** �O�C��COK  -  ��HOP�P�ETC� ***')
         IF ( DIAL .EQ. 0 ) WRITE (NWTR, 1004 ) IER, (ER(I),I=1,80)
1004     FORMAT(1X,'*** O���OK ', I2, ' ***=>', 80A1/
     * ' *** �O�C��COK  -  ��HOP�P�ETC� ***')
         CALL MSGOUT( MSG )
         IRC = 2
         IF ( DIAL .EQ. 1 ) IRC = 1
         DO 1005 I=1,80
1005     ER(I) = BLANK
         IER = 0
1002     CONTINUE
C-------- BBO� CTPOK� -------------------------------------------------
         IF ( EOFRDR .EQ. 1 ) GO TO 9999
         READ( NRDR, 1006, END=9999) (KAR(I), I=1,80)
         NZRDR = NZRDR + 1
C--->
1006     FORMAT(80A1)
         IF (DIAL .EQ. 0 ) WRITE( NWTR, 1007) (KAR(I), I=1,80)
         IF (DIAL .EQ. 1 ) WRITE( NWTR, 10070) (KAR(I), I=1,80)
1007     FORMAT(20X, 80A1)
10070    FORMAT(     80A1)
C???????????????????????????????????????????????????????????????????????
         CALL APFCAS ( 1, 80,KAR, KAR )
C???????????????????????????????????????????????????????????????????????
C===>  "�APA�E��H��" MACC�B FKAR B F-KO�:
         CALL APF TRT( 1, K72, KAR, FKAR )
         IK = 1
C----------------------------------------------------------------------
1001     CONTINUE
C---> HE �AKOH��T� ��?
         IF ( KAR(IK) .EQ. LKON ) GO TO 9998
C---> ��PA�� �PO�.
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
C--->  HE MHO�O �� ?
         IF ( SUM .GT. KSUM ) GO TO 10
C---> HE �POBEPKA �� �POCTO
         IF ( PRT90 .EQ. 1 ) GO TO 1008
C--->  B ��OK� �A�POCOB....
         GO TO (1, 2, 3,   4,     5,    6,   7,  8   ), IPRT
C--->           V  U  I  GAMMA  OMEGA  MFT  MBH  C��
C   # C � T O #
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
1008     CONTINUE
         IPRTX = 0
         IF ( KAR(IK) .EQ. LV ) IPRTX = 1
         IF ( KAR(IK) .EQ. LU ) IPRTX = 2
         IF ( KAR(IK) .EQ. LI ) IPRTX = 3
         IF ( KAR(IK) .EQ. LG(1) .AND. KAR(IK+1) .EQ. LG(2)  .AND.
     =      KAR(IK+2) .EQ. LG(3) .AND. KAR(IK+3) .EQ. LG(4)  .AND.
     =      KAR(IK+4) .EQ. LG(5) ) IPRTX=4
         IF ( KAR(IK) .EQ. LO(1) .AND. KAR(IK+1) .EQ. LO(2)  .AND.
     =      KAR(IK+2) .EQ. LO(3) .AND. KAR(IK+3) .EQ. LO(4)  .AND.
     =      KAR(IK+4) .EQ. LO(5) ) IPRTX=5
         IF ( KAR(IK) .EQ. LMFT(1) .AND. KAR(IK+1) .EQ. LMFT(2)  .AND.
     =      KAR(IK+2) .EQ. LMFT(3) ) IPRTX = 6
         IF ( KAR(IK) .EQ. LMBH(1) .AND. KAR(IK+1) .EQ. LMBH(2)  .AND.
     =      KAR(IK+2) .EQ. LMBH(3) ) IPRTX = 7
         IF ( KAR(IK) .EQ. CIU (1) .AND. KAR(IK+1) .EQ. CIU (2)  .AND.
     =      KAR(IK+2) .EQ. CIU (3) ) IPRTX = 8
C
         IF ( PRT90 .EQ. 1  .AND.  IPRTX .EQ. 0 ) GO TO 9010
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF ( IPRTX .EQ. 0 ) GO TO 1010
C---> C�TO HA��O.., CMECT�MC� �A <�M�>-1 C�MBO�....
         IGNORE = 0
         IPRT = IPRTX
         PRT90 = 0
         IF ( IPRT .EQ. 1 ) IK = IK + 0
         IF ( IPRT .EQ. 2 ) IK = IK + 0
         IF ( IPRT .EQ. 3 ) IK = IK + 0
         IF ( IPRT .EQ. 4 ) IK = IK + 4
         IF ( IPRT .EQ. 5 ) IK = IK + 4
         IF ( IPRT .EQ. 6 ) IK = IK + 2
         IF ( IPRT .EQ. 7 ) IK = IK + 2
         IF ( IPRT .EQ. 8 ) IK = IK + 2
         GO TO 10
1010     CONTINUE
C--->  O���KA, EC�� HE ��HOP�P�EM...
         IF ( IGNORE .EQ. 1 ) GO TO 10
C--->  #O���KA#
         IGNORE = 1
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 21
         MSG(1) = I+1
         GO TO 10
C**********************************************************************
C######################################################################
C######################################################################
C   BBECT�
C   �E�OE       M E T K A   90
C   ��C�O       --------------
C######################################################################
C######################################################################
90       CONTINUE
         PRT90 = 1
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO  9998
C--->  �POBEP�M HET �� C�E�. O��CATE��...
         GO TO 1008
C--->  BEPHEMC�, EC�� HET....
9010     CONTINUE
         PRT90 = 0
         N2 = K72 - IK + 1
         N2OLD = N2
         CALL APF SQI( FKAR, N0, IK, N2, IFI )
         GO TO ( 9001, 9002, 9003, 9004, 9005, 9006 ), IFI
9004     CONTINUE
9005     CONTINUE
C!!!-------------------------------------------------------------------
         WRITE( NWTRE, 9007 ) IPRT, IFI
9007     FORMAT(' *** �O�. C�O� B INPUT1: IPRT=', I12,' IFI=',I10)
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
9006     CONTINUE
C===> TO��KO �HAK ��� O�H� �PO�E��, �O�POC�M HOB�� CTPOK�
         PRT90 = 1
         GO TO 10
9003     CONTINUE
C---> �EPE�O�HEH�E
         IK = IK - 1
         IGNORE = 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 2
         MSG(1) = I + 1
         IER = IER + 1
         GO TO 10
9002     CONTINUE
C===>  EC�� N2 =(N2OLD-1)==> TO CPA�� BCTET��C� HE C�MBO�, ��CE� HE���O!
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( N2 .EQ. (N2OLD-1) ) GO TO 9009
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C----> BBE��, BCTPET��� HE ���P� => IK HA �TOM C�MBO�E OCTAB�M
C----> �O��CT�M �OC�E ���P� TO��KO �PO�E� , ";"  ��� "."
       IF ( KAR(IK) .EQ. BLANK .OR. KAR(IK) .EQ. LKON ) GO TO 9001
       IF ( KAR(IK) .EQ. POINT .AND. IPRT .EQ. 3 ) GO TO 9001
C===> OKOH�AH�E �OC�E ���P� O���O�HOE, BCTAHEM HA HE�O....
9009     IK = IK - 1
         IGNORE = 1
         ER(IK+1) = LGOPA
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I + 1
         IER = IER + 1
         GO TO 10
9001     CONTINUE
C=====> BCE O"KEY   BEPHEMC� B ��OK �A�POCA
         GO TO ( 100,  200,  300,  400,  500,  600,  700, 800 ), IPRT
C---????
         GO TO 9004
C######################################################################
C######################################################################
1        CONTINUE
C--->  "V" BBO��M...
         IGNORE = 0
         GO TO 90
100      CONTINUE
C--> �O��CT�M �� HOMEP ���A ?
         IF ( N0 .GE. 0  .AND.  N0 .LE. NMKNOT ) GO TO 101
C-->  HOMEP O���O�EH...
103      IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 23
         MSG(1) = I + 1
         GO TO 10
C--->
C-->  HOMEP O���O�EH...��� �  #SPOOL
66661    IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 34
         MSG(1) = I + 1
         GO TO 10
C--->
101      CONTINUE
         IF ( FTRSPL .EQ. 0 ) GO TO 66663
C    �஢�ਬ �� �����⨬���� ����஢
         DO 66660 i66660=1,SPISVL(1)
            IF ( N0 .EQ. SPISVL(i66660+1) ) GO TO 66663
66660    CONTINUE
          GO TO 66661
C#############
66663  CONTINUE
C--> ��E� HA�, ECT� K��A �P�H�T� ?
         SUM = SUM + 1
         IF ( SUM .LE. KSUM ) GO TO 102
C--->  ��E MHO�O ��CE� B �A�POCE
104      IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 22
         MSG(1) = I + 1
         GO TO 10
102      CONTINUE
C---> �P�H�T� ��E�
         KVT = VT(1) + 1
         VT(KVT+1) = N0
         VT(1) = KVT
         GO TO 10
C######################################################################
2        CONTINUE
C--->  "U"
         IGNORE = 0
         IUU = 0
         GO TO 90
200      CONTINUE
         IF ( IUU .EQ. 1 ) GO TO 201
C--> B���� �EPB�� ��E�..
         N0U1 =  N0
         IF ( N0U1 .GE. 0  .AND.  N0U1 .LE. NMKNOT ) GO TO 202
C-->  ��E� O���O�EH
         GO TO 103
202      CONTINUE
C#############
         IF ( FTRSPL .EQ. 0 ) GO TO 66664
C    �஢�ਬ �� �����⨬���� ����஢
         DO 66662 i66660=1,SPISVL(1)
            IF ( N0U1 .EQ. SPISVL(i66660+1) ) GO TO 66664
66662    CONTINUE
          GO TO 66661
66664     CONTINUE
C-->  H��EH �P. ��E�...
         IUU = 1
         GO TO 90
201      CONTINUE
C-->  C�E�. ��E� B����
         IUU = 0
         IF ( N0 .GE. 0  .AND.  N0 .LE. NMKNOT ) GO TO 203
         GO TO 103
203      CONTINUE
         IF ( FTRSPL .EQ. 0 ) GO TO 66665
C    �஢�ਬ �� �����⨬���� ����஢
         DO 66666 i66660=1,SPISVL(1)
            IF ( N0 .EQ. SPISVL(i66660+1) ) GO TO 66665
66666    CONTINUE
          GO TO 66661
66665     CONTINUE
C-->  ���� B HA�����
         KUT = UT(1) + 1
         SUM = SUM + 1
         IF ( SUM .GT. KSUM ) GO TO 104
         UT(KUT+1) = N0U1
         UT(KUT+2) = N0
         UT(1) = KUT + 1
         GO TO 10
C######################################################################
3        CONTINUE
C--->  C��COK "I"
C---> NSM0 - CME�EH�E
         NSM0 = 1
         IGNORE = 0
         II= 0
         GO TO 90
300      CONTINUE
         IF ( II .EQ. 1 ) GO TO 301
         NEL = N0
C--->  ECT� �� TAKO� ��EMEHT ?
         CALL SEQMS0( MSWALL, NEL, IMSW, IR0, NSM, ITOK )
         GO TO( 302, 303 ), IR0
303      CONTINUE
C--->  HOMEPA HET...
         IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 24
         MSG(1) = I + 1
         GO TO 10
302      CONTINUE
C-->   HOMEP ��-TA HA�, BBO��TC� �� CME�EH�E?
         IF ( KAR(IK) .NE. POINT ) GO TO 304
C--> BBO��M "CME�EH�E"
         IK = IK + 1
         II = 1
         GO TO 90
C--->
301      CONTINUE
         II = 0
         NSM0 = N0
304      CONTINUE
C-->  �O��CT�MO �� CME�EH�E....
         IF ( NSM0 .GE. 1  .AND.  NSM0  .LE. NSM ) GO TO 305
C---> CME�EH�E O���O�HOE
         IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 25
         MSG(1) = I + 1
         GO TO 10
C--->
305      CONTINUE
C--->  �P�H�T� NEL  �  NSM0
         IF ( FTRSPL .EQ. 0 ) GO TO 66613
C    �஢�ਬ �� �����⨬���� ����஢ #SPOOL
C   NEL + NSM0
C --> A�PECA...
C---> �POCMOTP MSWALL...
         CALL SEQMS0( MSWALL, NEL, IMSW66, IR660, NSM66, ITOK66 )
         GO TO (66610,66611), IR660
         CALL SUBERR(IR660)
66611       CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE,66612) IR660, NEL
66612       FORMAT(' *** �O�. C�O� B INPUT1 "����� � MSWALL": IR660=',
     =   I5, ' NEL = ', I6, ' ***' )
C!!!--------------------------------- ---------------------------------
         CALL SUBERR(66623)
66610       CONTINUE
         TIP = MSWALL(IMSW+1)
         iii666 = ITOK66 + ( NSM0-1 )
         DO 66614 i66660=1,SPISTK(1)
            IF ( iii666 .EQ. SPISTK(i66660+1) ) GO TO 66613
66614    CONTINUE
C         GO TO 66604
C---> ������ ��� � ������ #SPOOL
66604    IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 35
         MSG(1) = I + 1
         GO TO 10
66613     CONTINUE
         KIT = IT(1) + 1
         SUM = SUM + 1
         IF ( SUM .GT. KSUM ) GO TO 104
         IT(KIT+1) = NEL
         IT(KIT+2) = NSM0
         IT(1) = KIT + 1
         GO TO 10
C######################################################################
C######################################################################
4        CONTINUE
C-->  C��COK "GAMMA", �M� �EPEMEHHO� C KO�OM #2#  HA�O HA�T�...
         KODSM = 2
410      IGNORE = 0
         GO TO 90
400      CONTINUE
411      NEL = N0
C--->  ECT� �� �TOT ��EMEHT?
         CALL SEQMS0(MSWALL, NEL, IMSW, IR0, NSM,  ITOK )
         GO TO ( 401, 303 ), IR0
         CALL SUBERR(IR0)
401      CONTINUE
         TIP = MSWALL(IMSW+1)
C--> �O HOMEP� T��A HA�T� "CME�EH�E" C KO�OM  �MEH� "KODSM"
         CALL SEQ NT0( TIP, KODSM, NSM0, IR0 )
         GO TO ( 402, 403 ), IR0
         CALL  SUBERR(IR0)
403      CONTINUE
C--->  ��EMEHT HE CO�EP��T TAKO�O "CME�EH��"
         IGNORE = 1
         IK = IK - 1
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 26
         MSG(1) = I + 1
         GO TO 10
C--->
402      CONTINUE
C--> NSM0 - �CKOMOE "CME�EH�E", NEL - HOMEP ��EMEHTA
         GO TO 305
C######################################################################
C######################################################################
5        CONTINUE
C--->  C��COK "OMEGA"
         KODSM = 3
         GO TO 410
500      CONTINUE
         KODSM = 3
         GO TO 411
C######################################################################
6        CONTINUE
C--->  C��COK "M�M",  KO� �MEH� �EPEMEHHO� = 4
         KODSM = 4
         GO TO 410
600      CONTINUE
         KODSM = 4
         GO TO 411
C######################################################################
7        CONTINUE
C-- C��COK "MBH",  KO� �MEH� �EPEMEHHO� 5
         KODSM = 5
         GO TO 410
700      CONTINUE
         KODSM = 5
         GO TO 411
C######################################################################
8        CONTINUE
C--->  C��COK "C��",  KO� �EPEMEHHO� 6
         KODSM = 6
         GO TO 410
800      CONTINUE
         KODSM = 6
         GO TO 411
C######################################################################
C######################################################################
C######################################################################
C  HEO���AHH��
C    KOHE�             M E T K A   # 9999
C  �A��A NRDR          ------------------
C######################################################################
C######################################################################
9999     CONTINUE
         EOFRDR = 1
         IRC = 3
         WRITE(NWTR,776)  NRDR
776      FORMAT(' *** KOHE� BBO��MO� �H�OPMA��� HA KAHA�E BBO�A # ',
     =           I3, ' ***'///)
         RETURN
C######################################################################
C######################################################################
9998     CONTINUE
C--->  ";"  O�HAP��EH...
         IF ( IER .EQ. 0 ) GO TO 803
         SUMER = SUMER + IER
C-->  CKA�AT� O� O���KAX
C---> �E�AT� O���OK
         IF ( DIAL .EQ. 1 ) WRITE ( NWTR, 1003 ) (ER(I), I=1,80)
C
         IF ( DIAL .EQ. 0 ) WRITE (NWTR, 1004 ) IER, (ER(I),I=1,80)
         CALL MSGOUT( MSG )
         IRC = 2
803      CONTINUE
         IF ( SUMER .EQ. 0 ) RETURN
         IF ( DIAL .EQ. 0 ) RETURN
         I = KSUM - SUM
         IER = 0
         IF ( I .LE. 0 ) RETURN
         WRITE(NWTR, 801) I
801      FORMAT(' *** BBE��TE ";" ��� O��CAH�� ��� ', I2, ' B�BO��M�X BE
     =����H ***' )
         SUMER = 0
         DO 802  I= 1,80
802      ER(I) = BLANK
         IRC = 1
         IGNORE = 0
         MSG(1) = 0
         IPRT = 0
         PRT90 = 0
         IER = 0
         IK = K72 + 1
         GO TO 10
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
