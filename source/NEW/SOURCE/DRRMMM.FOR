      SUBROUTINE DRRMMM(MSWALL,TOKN)
CXXX****KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)************
C �O��PO�PAMMA:    DRRMMM(MSWALL,TOKN)
C--------------
C
C  HA�HA�EH�E:    O�PA�OTKA �EPEKT�B�
C ------------    #MPE��M
C                 < �M� MO�E�� >  #  ISQ  ISD  IRQ  IRD  OMEGA(;)
C
C               �A�AH�E HA�A��HO�O PE��MA PA�OT� ��. MA��H�:
C                 < �M� MO�E�� > - M3X3X0 - PEA���OBAHO;
C                              # - HOMEP ��EMEHTA B �E��;
C                            ISQ - �POEK��� TOKA CTATOPA HA OC� Q
C                                  O�OPHO� C�CTEM�;
C                            ISD - �POEK��� TOKA CTATOPA HA OC� D
C                                  O�OPHO� C�CTEM�;
C                            IRQ - �POEK��� TOKA POTOPA HA OC� Q
C                                  O�OPHO� C�CTEM�;
C                            IRD - �POEK��� TOKA POTOPA HA OC� D
C                                  O�OPHO� C�CTEM�;
C                          OMEGA - ���OBA� CKOPOCT� BPA�EH�� POTOPA;
C
C     !!! BH�MAH�E !!!  O�OPHA� C�CTEMA Q-D OCE� COB�A�AET C OC��
C                       �A�� "A" CTATOPA ( OC� Q ) ,
C                       OC� D O�EPE�AET OC� Q HA +90 �PA�.;
C                       C�CTEMA CTATOPH�X O�MOTOK COBME�EHA C
C                       C�CTEMO� POTOPH�X O�MOTOK , T.E. GAMMA=0;
C     !!!!!!!!!!!!!!!!
C
C    METO�:        BBO� � �POEK��� Q-D �HA�EH�� HA A-B-C OC�.
C   -------
C
C  �APAMETP�:      MSWALL -C��COK C�OB COCTO�H�� ��EMEHTOB;
C -----------        TOKN -MACC�B HA�A��H�X �HA�EH�� �EPEMEHH�X
C                          COCTO�H��;
C
C  �O��PO�PAMM�:   1.PROF1 , SUBERR
C --------------   2.GETDAT,SEQMS0
C
C  �P�ME�AH�E:     1.�P� �EPEXO�E �� Q-D B A-B-C , C��TAETC�,
C ------------       �TO "A"+"B"+"C"=0
C                    ( KO������EHT 2/3 HE BBO��TC� );
C                  2.�A��C� ...,,...  �KB�BA�EHTHA ...,0,...;
C                  3.KOHE� �AHH�X �O ";" ��� �OC�E BBO�A
C                    BCEX �HA�EH��;
C                  4.�P� O���KE BCE O�H���ETC�.
C
CZZZ*******************************************************************
      COMMON/PROF1/ MASPRT(200)
      EQUIVALENCE (NWTR, MASPRT(13))
      EQUIVALENCE (NWTRE,MASPRT(14))
      EQUIVALENCE (NRDR, MASPRT(12))
      EQUIVALENCE (KAR1(2),KAR(1))
      EQUIVALENCE (DIAL,MASPRT(15))
      DIMENSION MSWALL(1),TOKN(1),KAR(80),LTAB(10)
      DIMENSION DAT(21),M3X3X0(6),KAR1(81),PAR(6)
      INTEGER TERM,DIAL,BLANK
      REAL ISA,ISB,ISC,IRA,IRB,IRC
      REAL ISQ,ISD,IRQ,IRD
      INTEGER AMOT,ATOKM,TIPX
C
      DATA M3X3X0/'M','3','X','3','X','0'/
      DATA R120/2.0943951/,TERM/';'/
      DATA LTAB/11,110,12,120,6*0/,KLTAB/4/
      DATA KAR1/81*' '/,PAR/6*0.0/
      DATA BLANK/' '/,GAMMA/0.0/
C
      A(Q,D)=Q*COS(0.0*R120)+D*SIN(0.0*R120)
      B(Q,D)=Q*COS(1.0*R120)+D*SIN(1.0*R120)
      C(Q,D)=Q*COS(2.0*R120)+D*SIN(2.0*R120)
C--->  C�K BBE�EHH�X
      KPAR=0
      DO 2000 I=1,81
 2000 KAR1(I)=BLANK
      DO 2001 I=1,21
 2001 DAT(I)=0.0
C--->  BBO� ��CE�.....
      READ (NRDR,1,END=666)(KAR(I),I=1,80)
      IF(DIAL.EQ.0) WRITE(NWTR,1001)(KAR(I),I=1,80)
 1001 FORMAT(/20X,80A1)
 1    FORMAT(80A1)
C--->  �M� ?
      DO 2 I=1,6
      IF(M3X3X0(I).NE.KAR(I)) GO TO 667
 2    CONTINUE
C--->  HA�E.....
 100  CONTINUE
C--->  B��T� ��C�O
      CALL GETDAT(KAR,DAT,IRL)
C--->  CKO��KO ?
      KKK=IFIX(DAT(1))
C--->  HAKO��T�.....
      IF(KKK.GT.6) KKK=6
      DO 3 I=1,KKK
 3    PAR(KPAR+I)=DAT(1+I)
      KPAR=KPAR+KKK
 5    CONTINUE
C--->  H��HO �� E�E ?
      IF(KPAR.GE.6) GO TO 10
      DO 4 I=7,72
      IF(KAR(I).EQ.TERM) GO TO 10
 4    CONTINUE
C--->  �OBBO�.....
      READ (NRDR,1,END=666)(KAR(I),I=1,80)
      IF(DIAL.EQ.0) WRITE(NWTR,1002)(KAR(I),I=1,80)
C--->  ����KA...��CT�M 73 �O����� KAR.....
      KAR1(72)=BLANK
 1002 FORMAT(1X,20X,80A1)
C--->  �O����M ��C�A , B KAR1(1) �PO�E�
      CALL GETDAT(KAR1,DAT,IRC1)
      KKK1=IFIX(DAT(1))
C--->  O�PAH���MC�.....
      IF(KKK1.GT.(6-KPAR)) KKK1=6-KPAR
      IF(KKK1.LT.0) CALL SUBERR(KKK1)
C--->  �P�EM ��CE�
      DO 40 I=1,KKK1
 40   PAR(KPAR+I)=DAT(I+1)
      KPAR=KPAR+KKK1
C--->  E�E H��HO ?
      GO TO 5
C**********************************************************************
 10   CONTINUE
      ISQ=PAR(2)
      ISD=PAR(3)
      IRQ=PAR(4)
      IRD=PAR(5)
      OMEGA=PAR(6)
      NELM=IFIX(PAR(1))
C--->  �CKAT� B MSWALL ��EMEHT C HOMEPOM NELM:
      CALL SEQMS0(MSWALL,NELM,AMOT,IRC0,KVTV,ATOKM)
      IF(IRC0.EQ.1) GO TO 11
C--->  ��EMEHTA HET.
      WRITE (NWTR,1005)NELM
 1005 FORMAT(/' *** �E�� HE CO�EP��T ��EMEHTA # ',I5,' ***'/
     *' *** �EPEKT�BA ��HOP�P�ETC� ***')
      RETURN
 11   CONTINUE
C--->  �TO �A T�� ?
      TIPX=MSWALL(AMOT+1)
      DO 12 I=1,KLTAB
      IF(TIPX.EQ.LTAB(I)) GO TO 13
 12   CONTINUE
C--->  HE HA� T��......
      WRITE (NWTR,1006) NELM,TIPX,(M3X3X0(I),I=1,6)
 1006 FORMAT(/' *** ��EMEHT �E�� # ',I5,' ( T�� ',I4,') ***'/
     *' *** HE MO�E�� ��. MA��H� ',6A1,' ***'/
     *' *** ��PEKT�BA ��HOP�P�ETC� ***'/)
      RETURN
 13   CONTINUE
C--->  BCE O'KE�....
      WRITE (NWTR,1007) NELM,TIPX,(M3X3X0(I),I=1,6),
     *                  ISQ,ISD,IRQ,IRD,OMEGA,GAMMA
 1007 FORMAT(/' *** ��� ��EMEHTA �E�� # ',I5,' ( T�� ',I4,')',
     = 19X, ' ***'/
     *' *** ��. MA��H� ',6A1,' �CTAHOB�EHO:', 26X, ' ***'/
     *' ***   ISQ=',G12.5,'   ISD=',G12.5,    19X, ' ***'/
     *' ***   IRQ=',G12.5,'   IRD=',G12.5,    19X, ' ***'/
     *' *** OMEGA=',G12.5,' GAMMA=',G12.5,    19X, ' ***')
      ISA=A(ISQ,ISD)
      ISB=B(ISQ,ISD)
      ISC=C(ISQ,ISD)
C**********************************************************************
      IRA=A(IRQ,IRD)
      IRB=B(IRQ,IRD)
      IRC=C(IRQ,IRD)
      WRITE (NWTR,1008) ISA,ISB,ISC,IRA,IRB,IRC
 1008 FORMAT(' *** ��� ��� �A�H�X �HA�EH�� ( OC� Q COB�A�AET',
     *' C �A�O� "A" ): ***'/
     *' *** ISA=',G12.5,' ISB=',G12.5,' ISC=',G12.5,'       ***'/
     *' *** IRA=',G12.5,' IRB=',G12.5,' IRC=',G12.5,'       ***'/)
C--->  PACTAB�T� �HA�EH��.
      TOKN(ATOKM+0)=ISA
      TOKN(ATOKM+1)=ISB
      TOKN(ATOKM+2)=ISC
      TOKN(ATOKM+3)=IRA
      TOKN(ATOKM+4)=IRB
      TOKN(ATOKM+5)=IRC
      TOKN(ATOKM+6)=GAMMA
      TOKN(ATOKM+7)=OMEGA
      RETURN
C**********************************************************************
 667  CONTINUE
      WRITE (NWTR,1009)(M3X3X0(I),I=1,6),(LTAB(I),I=1,KLTAB)
 1009 FORMAT(/' *** ��PEKT�BA " #MPE��M" PEA���OBAHA ��� MO�E�E� ***'/
     *' *** �M� MO�E��:',6A1,' HOMEPA T��OB: ***'/
     *' *** ',10I6,' ***')
      WRITE (NWTR,1010)
 1010 FORMAT(' *** ��HOP�P�ETC� ***'/)
      RETURN
 666  CONTINUE
      WRITE (NWTR,1011) NRDR
 1011 FORMAT(/' *** KOHE� HA KAHA�E BBO�A ',I2,' [ DRRMMM ] ***'/
     *' *** PA�OTA �PEP�BAETC� ***'//)
      CALL SUBERR(0)
      RETURN
      END
