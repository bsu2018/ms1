         SUBROUTINE GETMSW( BA0, KNOT, PAR0, MSWALL, IER )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C
C      �O��PO�PAMMA: GETMSW( BA0, KNOT, PAR0, MSWALL, IER )
C      ------------
C
C      HA�HA�EH�E: -  �OPM�P�ET C��COK ��PAB�����X C�OB MAKPOMO�E�E�
C      -----------    MSWALL  [ M_ACRO S_TATUS W_ORD ],
C                     CO�EP�A�E�O BH�TPEHHEE O��CAH�E MAKPOMO�E�E�;
C                  - ��KC�P�ET HOMEP HA�A�A �EPBO�O O��CAH�� B MSWALL
C                    ��EMEHTOB C �P��HAKOM T��A MO�E�� PABH�M # 5
C                    ( T5MSW = MASPRT(41) );
C                  - ��KC�P�ET HOMEP HA�A�A �EPBO�O O��CAH�� B MSWALL
C                    ��EMEHTOB C �P��HAKOM T��A MO�E�� PABH�M # 6
C                    ( T6MSW = MASPRT(49) );
C                  - O�PE�E��ET KO���ECTBO B�BO�H�X �HA�EH�� TOKOB �
C                    C�E�. �EPEMEHH�X ( CO��ACHO �O�� MACTAB(6) ),
C                    �O���EHHOE �HA�EH�E   ===>  �A��C�BAETC� B
C                    NNTOK �O�E MASPRT(46);
C                  - �P� HA����� ��EMEHTOB, MO�E��P�EM�X MAKPO-
C                    MO�E��M� C ��EA��H�M� BEHT���M� ( �.T�� # 6 ) -
C                    BO�����AET ��A�  FLOG  ( MASPRT(37) <-- 1 );
C                    !!! BH�MAH�E !!! �PE��O�O�AETC�, �TO �H�OPMA���
C                     B MSWALL OTCOPT�POBAHA �O �O�� "�P��HAK T��A";
C
C      METO�:       �A�O�HEH�E MSW CO��ACHO �H�OPMA��OHH�X MAC�BOB
C      -----        BA0, KNOT � PAR0; �ETA��HA� HACTPO�KA
C                   KOHKPETHO� MAKPOMO�E�� �PO��BO��C� �/�
C                   C �MEHAM� FM_XXXX ( CM. TA�. MACTAB )
C                   HACTPO�KA �AK���AETC� B B���C�EH�� BCEX
C                   HEO�XO��M�X KOHCTAHT, �C�O����EM�X B
C                   �/� DM_XXXX - PAC�ETA MAKPOMO�E��;
C
C                   ��� �O���ECKO� CB��� MCW C�OB--> �P� HEO�XO��MOCT�
C                       B���BAET  CALLLM - �/� �POCMOTPA MSW_ALL
C                       � B��OBA �P� �A�POCE ( �EPE� MACTAB O��CAH�E )
C                       COOTBET. LM..@..  MOH�TOPOB HACTPO�K�..
C
C
C
C     �APAMETP�:    �H�OPMA��OHH�E MAC�B�:
C     ---------     BA0 - MACC�B C��CKA ��EMEHTOB;
C                   KNOT - MACC�B C��CKA ���OB �O�K���EH��;
C                   PAR0 - MACC�B C��CKA �APAMETPOB;
C                          ( CM. INPUT0 );
C                   MSWALL - MACC�B ��PAB�����X C�OB MAKPOMO�E��;
C                   IER -  = 0  BCE T��� MO�E�E� �OCT��H�;
C                          = N  T�� HOMEP N HE �OCT��EH,
C                               (�A��HE���� �POCMOTP BA0
C                                �PEKPA�EH);
C                          = 1 - LM MOH�TOP B��B�� �O���ECK�� O���K�;
C                          ******************************************
C
C
C     �O��PO�PAMM�: 1. CALLFM - "B���BA����K" MOH�TOPOB FM..@...
C     ------------
C                   2. COMMON PROF1, PROF2, PROF3
C                   3.         SUBERR;
C                   4. CALL_LM - "B���BA����K" MOH�TOPOB LM..@..
C
C
C    �P�ME�AH�E:    1. �OC�E �C�E�HO�O �OPM�POBAH�� MSWALL
C    ----------        MACC�B� BA0, KNOT � PAR0 HE H��H�;
C                   2. CTP�KT�PA B���BA��E�O �PE��O�EH�� FM.( B CALLFM):
C                      CALL FM.....( BA0T, KNOTT, PAR0T,
C                                    MSW<�EPEMEHHA� �ACT�>,
C                                    LM );
C                      ��EC�:
C                      ...T - TEK���E ��EMEHT� MACC�BOB,
C                      MSW<�EP. �ACT�> - TEK��EE MECTO B MSWALL,
C                      HO !!!BH�MAH�E!!! HA��HA� C �O�� "SMP"
C                       ( M�H�� C��COK ���OB );
C                      LM - ���HA �EPEMEHHO� �ACT� O��CAH�� MSW;
C                      ( ���HA TO��KO �O�E� SPM � SRAB );
C
C                   3. �PE��O�O�AETC�, �TO BA0 OTCOPT�POBAH �O
C                      BO�PACTAH�� �P��HAKA T��A MAKPOMO�E��;
C
C
C                  CTP�KT�PA MACC�BA  MSWALL:
C                  --------------------------
C
C  +-------------...+------+------------+------------+...+----------+
C  I            I   I A�P. I"�OCTO�HHA�"I"�EPEMEHHA�"I   IA�P. C�E�.I
C  I ��CKP��TOP I   I C�E�.I �ACT� MSW  I �ACT� MSW  I   I   ���    I
C  I            I   I MSW  I (INTEGER*4)I            I   I  HO�� [0]I
C  +------------+...+------+------------+------------+...+----X-----+
C  I1 2 3 4 5   I   I  1   I   8 C�OB   I    ????    I        I
C  +------------+   +------+------------+------------+        I
C                      I                                      I
C                      V                                      I
C                      +>------------------------------------>+
C
C
C               "�OCTO�HHA�" �ACT� MSW:
C                ----------------------
C
C  +-------+--------+--------+--------+------+------+----------+------+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI KO�� I KO�. I A�P. B�B.I PE   I
C  I ��EME I T��A   I �/�    I T��A   I �ECT I B�B. I �HA�. B  I �E   I
C  I HTA   I MO�E�� I PAC�ETAI        I BO   I �HA� I "TOKN" � I PB   I
C  I       I        I DM.....I        I ���OBI EH��.I B "TOKN1"I      I
C  +-------+--------+--------+--------+------+------+----------+------+
C  I  1    I   2    I   3    I    4   I   5  I  6   I    7     I  8   I
C  +-------+--------+--------+--------+------+------+----------+------+
C
C
C               "�EPEMEHHA�" �ACT� MSW:
C               -----------------------
C
C  +----------++-------------+-------------+
C  I C��COK   II C��COK      I PA�O�A�     I
C  I ���OB    II BH�TPEHH�X  I O��ACT�     I
C  I �O�K��-  II �APAMETPOB  I MAKPOMO�E�� I
C  I �EH��    II MAKPOMO�E�� I             I
C  I ��EMEHTA II             I             I
C  I          II             I             I
C  +----------++-------------+-------------I
C  I 9+K"���OBII   SPM       I  SRAB       I
C  +----------++-------------+-------------+
C
C  �EPEMEHHA� �ACT� �O�HOCT� �A�O�H�ETC� �/� FM.... HA OCHOBE "OTPE�KOB"
C  �H�OPMA��OHH�X MACC�BOB BA0, KNOT, PAR0
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/PROF2/ MACTAB(800)
         COMMON/PROF3/ FUNTAB(50)
         DIMENSION BA0(*), KNOT(*), PAR0(*), MSWALL(*)
         INTEGER BA0, FM, TIP, TKEL, FMM, DM, ADRMSW, T5MSW
         INTEGER PTIP, T6MSW, ADRTOK, T4MSW
         IER = 0
C===> KAHA�� �E�AT�
         NWTR = MASPRT(13)
         NWTRE = MASPRT(14)
C===>  �KA�ATE��
         NNTOK = 0
         MMSWA = MSWALL(2)
         T5MSW = 0
         T6MSW = 0
         T4MSW = 0
C---> �KA�ATE�� CBO�O�. MECTA B TOKN � TOKN1
         ADRTOK = 1
         IMSWA = 6
         MSWALL(IMSWA) = 0
         IBA0 = 6
C===> KEL - KO�-BO ��EMEHTOB O��CAH�� B BA0, TKEL - C.�.K.
         KEL = MASPRT(3)
         TKEL = 0
C######################################################################
C  P A � � O P   O � E P E � H O � O  O � � C A H � �
C######################################################################
1        CONTINUE
C===> HE �EPE�O�H���C� �� M�?
         IF ( IBA0 .LE. BA0(3) ) GO TO 100
105      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 500) IBA0, TKEL
500      FORMAT(' *** �O�. C�O� B GETMSW: IBA0=', I10, ' TKEL=',I10)
C******* CALL PRINT0( BA0, KNOT, PAR0 )
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
100      CONTINUE
C===>  �O�E �O� A�P. C�E�.
         ADRMSW = IMSWA
         MSWALL(ADRMSW) = 0
C===> O�PE�E�EH�E �OCTO�HHO� �ACT� TEK��E�O MSW, �CTAH. HA�. E�O:
         IMSWA = IMSWA + 1
C===> A�P. HA�A�A MSW �E� A�P. C�E�.
         IMSWA0 = IMSWA
         MSWALL( IMSWA + 0 ) = BA0( IBA0 )
         MSWALL( IMSWA + 1 ) = BA0( IBA0 + 1 )
         MSWALL( IMSWA + 2 ) = -1
C===> KO�-BO ���OB:
         MSWALL( IMSWA + 4 ) = BA0( IBA0 + 7 )
C===> �A��C� C��CKA ���OB IMP �T�K;
         IMP = BA0( IBA0 + 7 )
         IK = BA0( IBA0 + 2 )
         DO  101  I=1, IMP
101      MSWALL( IMSWA+ 8 + (I-1) ) = KNOT( IK + (I-1) )
C===> �KA�ATE�� -> HA HA�A�O �EPEMEHHO� �ACT�, �A C��CKOM ���OB
         IMSWA = IMSWA + 8 + IMP
C==> �OTOB�MC� K �OPM�POBAH�� �EPEMEHHO� �ACT�:
         TIP = BA0( IBA0 + 1 )
         PTIP = BA0( IBA0 + 4 )
         MSWALL( IMSWA0 + 3 ) = PTIP
         IKNOT = BA0( IBA0 + 2 )
         IPAR0 = BA0( IBA0 + 3 )
C===> �POBEP�M HA����E PEA���A��� FM .... & DM....
         I1 = 1
104      CONTINUE
         IF ( MACTAB(I1) .EQ. 0  ) GO TO 102
         IF ( MACTAB(I1) .EQ. TIP ) GO TO 103
         I1 = I1 + MASPRT(17)
         GO TO 104
102      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 501) TIP
501      FORMAT(' *** �O�. C�O�--> T��A # ', I10, ' HET B MACTAB')
C!!!-------------------------------------------------------------------
         GO TO 105
103      CONTINUE
C===> ECT� �� PEA���A���?
         IF ( MACTAB(I1+7) .GT. 0 ) GO TO 106
C===>  HET !!!
         WRITE(NWTR, 502) TIP
502      FORMAT(//' *** MAKPOMO�E�� T�� HOMEP: ', I5,
     =          ' �OKA HE �O��EP��BAETC� ***'/
     =          ' *** OTC�TCB�ET �PO�PAMMHA� PEA���A��� ***'/
     =          ' *** O��CAH�E �E�� C�PAC�BAETC� ***'///)
         IER = TIP
         RETURN
C######################################################################
106      CONTINUE
C===> *********** KO�-BO B�BO�H�X �HA�EH��
         IKK = MACTAB( I1+5 )
         IKK1 = IKK / 100
         IKK2 = IKK - IKK1 * 100
         MSWALL(IMSWA0 + 5) = IKK2
C===>   �A�OMH�M �TO KO�-BO B NNTOK--->
         NNTOK = NNTOK + IKK2
C===> O�PE�E��EM A�PECA B TOKN � TOKN1.....
         MSWALL( IMSWA0 + 6 ) = ADRTOK
C---> B PE�EPB -1....
         MSWALL( IMSWA0 + 7 ) = -1
C===> HOBOE CBO�. MECTO B TOKN � TOKN1....
         ADRTOK = ADRTOK + IKK2
C   ( ADRTOK  <==>  NNTOK + 1  B KOH�E �O�ECCA )
C===> B��E��M FM � DM:
         KODFM = MACTAB(I1+6)
         FM = KODFM/1000
         DM = KODFM - FM*1000
         MSWALL(IMSWA0 + 2 ) = DM
C**********************************************************************
C  B���BAEM FM@@@@.....
C  IMSWA - �KA�ATE�� TEK. �O�� B MSWALL;   BO�BPAT� HA METK� 1000
C######################################################################
          CALL CALLFM( FM, BA0(IBA0), KNOT(IKNOT), PAR0(IPAR0),
     =                 MSWALL(IMSWA), LM )
C######################################################################
1000    CONTINUE
C----> ��� O�EPE�HO�O ��EMEHTA C�OPM�POBA�� "�EPEMEHH��" �ACT� MSW
C                          ( EE ���HA LM C�OB )
         TKEL = TKEL + 1
         IMSWA = IMSWA + LM
C---> HE �EPE�O�H���C� �� M� ��E???
         IF ( IMSWA .GT. MMSWA ) GO TO 9999
C---> �A���EM B HA�A�O O��CAH�� A�P. C�E�.
         MSWALL( ADRMSW ) = IMSWA
       IF ( PTIP .NE. 4 ) GO TO 155
C-->  ��KCA��� ��-TA C �.T��OM # 4
       IF ( T4MSW .EQ. 0 ) T4MSW = ADRMSW
155    CONTINUE
         IF ( PTIP .NE. 5 ) GO TO 150
C===>  ��KCA��� ��EMEHTOB C �P��HAKOM T��A MAKPOMO�E�� # 5
         IF ( T5MSW .EQ. 0 ) T5MSW = ADRMSW
150      CONTINUE
         IF ( PTIP .NE. 6 ) GO TO 151
C==> ��KCA��� ��EMEHTA C �.T��A # 6
         IF ( T6MSW .EQ. 0 ) T6MSW = ADRMSW
151      CONTINUE
C---> �KA�. C�. O�H���M
         MSWALL( IMSWA ) = 0
C----> CMECT�MC� B BA0
         IBA0 = IBA0 + MASPRT(4)
C---->  �O��EP�KA ��CKP��TOPA..
         MSWALL(3) = IMSWA
         IF ( TKEL .LT. KEL ) GO TO 1
C---> K O H � A E M:
         MSWALL(3) = IMSWA
         MASPRT(41) = T5MSW
         MASPRT(49) = T6MSW
         MASPRT(93) = T4MSW
         MASPRT(37) = 0
         IF ( T6MSW .NE. 0 ) MASPRT(37) = 1
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 504) KEL, IMSWA, T4MSW, T5MSW, T6MSW
504      FORMAT(/' *** HACTPO�KA MAKPOMO�E�E� ��� ', I5, ' ��EMEHTOB',
     = ' �ABEP�EHA ***'/
     =' *** ( �AH�TO ', I10, ' C�OB B C��CKE M_S_W_ALL )        ***'/
     =' *** [ T4MSW = ',I5,'  T5MSW = ',I5,' T6MSW = ',I5 ,' ]       ***
     ='/ )
C!!!-------------------------------------------------------------------
C====> B MASPRT(46) - KO�-BO  B�B. �HA�EH��  TOKA � C�E�. �EPEMEHH�X.
         MASPRT(46) = NNTOK
C===>  CAMO �POBEPKA...
         IF ( ADRTOK .EQ. (NNTOK+1) ) GO TO 152
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 153 ) NNTOK, ADRTOK
153      FORMAT(' *** C�O� B KOH�E  GETMSW: NNTOK = ', I5,
     =          ' ADRTOK = ', I5, ' ***' )
C!!!-------------------------------------------------------------------
         CALL  SUBERR(1)
152      CONTINUE
C######################################################################
C   O K O H � A T E � � H A �  HACTPO�KA �EPE� LM_MOH�TOP�:
C
       CALL CALLLM( MSWALL, KOD )
       IER = KOD
C----------------------------------------------------------------------
         RETURN
9999     CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 505 ) TKEL, LM, ADRMSW , FM
505   FORMAT(' *** HET MECTA B M_S_W_ALL ���...', I4, ' ��EMEHTA ***'/
     =       ' *** ���HA �A�PO�EHHO� �. �ACT� MSW ', I10, ' C�OB ***'/
     =       ' *** HA�A�O O��CAH�� B MSWALL.......', I10, ' C�OBA ***'/
     =       ' *** HOMEP MOH�TOPA FM..............', I10 /)
C----------------------------------------------------------------------
         CALL SUBERR(0)
         RETURN
         END
