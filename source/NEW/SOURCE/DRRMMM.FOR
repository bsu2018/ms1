      SUBROUTINE DRRMMM(MSWALL,TOKN)
CXXX****KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)************
C O„POƒPAMMA:    DRRMMM(MSWALL,TOKN)
C--------------
C
C  HA‡HA—EHˆE:    OPAOTKA „EPEKTˆB›
C ------------    #MPE†ˆM
C                 < ˆMŸ MO„E‹ˆ >  #  ISQ  ISD  IRQ  IRD  OMEGA(;)
C
C               ‡A„AHˆE HA—A‹œHOƒO PE†ˆMA PAOT› ‹. MA˜ˆH›:
C                 < ˆMŸ MO„E‹ˆ > - M3X3X0 - PEA‹ˆ‡OBAHO;
C                              # - HOMEP ‹EMEHTA B –Eˆ;
C                            ISQ - POEK–ˆŸ TOKA CTATOPA HA OCœ Q
C                                  OOPHO‰ CˆCTEM›;
C                            ISD - POEK–ˆŸ TOKA CTATOPA HA OCœ D
C                                  OOPHO‰ CˆCTEM›;
C                            IRQ - POEK–ˆŸ TOKA POTOPA HA OCœ Q
C                                  OOPHO‰ CˆCTEM›;
C                            IRD - POEK–ˆŸ TOKA POTOPA HA OCœ D
C                                  OOPHO‰ CˆCTEM›;
C                          OMEGA - “ƒ‹OBAŸ CKOPOCTœ BPA™EHˆŸ POTOPA;
C
C     !!! BHˆMAHˆE !!!  OOPHAŸ CˆCTEMA Q-D OCE‰ COBA„AET C OCœž
C                       ”A‡› "A" CTATOPA ( OCœ Q ) ,
C                       OCœ D OEPE†AET OCœ Q HA +90 ƒPA„.;
C                       CˆCTEMA CTATOPH›X OMOTOK COBME™EHA C
C                       CˆCTEMO‰ POTOPH›X OMOTOK , T.E. GAMMA=0;
C     !!!!!!!!!!!!!!!!
C
C    METO„:        BBO„ ˆ POEK–ˆŸ Q-D ‡HA—EHˆ‰ HA A-B-C OCˆ.
C   -------
C
C  APAMETP›:      MSWALL -CˆCOK C‹OB COCTOŸHˆ‰ ‹EMEHTOB;
C -----------        TOKN -MACCˆB HA—A‹œH›X ‡HA—EHˆ‰ EPEMEHH›X
C                          COCTOŸHˆŸ;
C
C  O„POƒPAMM›:   1.PROF1 , SUBERR
C --------------   2.GETDAT,SEQMS0
C
C  PˆME—AHˆE:     1.Pˆ EPEXO„E ˆ‡ Q-D B A-B-C , C—ˆTAETCŸ,
C ------------       —TO "A"+"B"+"C"=0
C                    ( KO””ˆ–ˆEHT 2/3 HE BBO„ˆTCŸ );
C                  2.‡AˆCœ ...,,...  KBˆBA‹EHTHA ...,0,...;
C                  3.KOHE– „AHH›X O ";" ˆ‹ˆ OC‹E BBO„A
C                    BCEX ‡HA—EHˆ‰;
C                  4.Pˆ O˜ˆKE BCE OH“‹ŸETCŸ.
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
C--->  C—K BBE„EHH›X
      KPAR=0
      DO 2000 I=1,81
 2000 KAR1(I)=BLANK
      DO 2001 I=1,21
 2001 DAT(I)=0.0
C--->  BBO„ —ˆCE‹.....
      READ (NRDR,1,END=666)(KAR(I),I=1,80)
      IF(DIAL.EQ.0) WRITE(NWTR,1001)(KAR(I),I=1,80)
 1001 FORMAT(/20X,80A1)
 1    FORMAT(80A1)
C--->  ˆMŸ ?
      DO 2 I=1,6
      IF(M3X3X0(I).NE.KAR(I)) GO TO 667
 2    CONTINUE
C--->  HA˜E.....
 100  CONTINUE
C--->  B‡ŸTœ —ˆC‹O
      CALL GETDAT(KAR,DAT,IRL)
C--->  CKO‹œKO ?
      KKK=IFIX(DAT(1))
C--->  HAKOˆTœ.....
      IF(KKK.GT.6) KKK=6
      DO 3 I=1,KKK
 3    PAR(KPAR+I)=DAT(1+I)
      KPAR=KPAR+KKK
 5    CONTINUE
C--->  H“†HO ‹ˆ E™E ?
      IF(KPAR.GE.6) GO TO 10
      DO 4 I=7,72
      IF(KAR(I).EQ.TERM) GO TO 10
 4    CONTINUE
C--->  „OBBO„.....
      READ (NRDR,1,END=666)(KAR(I),I=1,80)
      IF(DIAL.EQ.0) WRITE(NWTR,1002)(KAR(I),I=1,80)
C--->  ‹ž˜KA...—ˆCTˆM 73 O‡ˆ–ˆž KAR.....
      KAR1(72)=BLANK
 1002 FORMAT(1X,20X,80A1)
C--->  O‹“—ˆM —ˆC‹A , B KAR1(1) POE‹
      CALL GETDAT(KAR1,DAT,IRC1)
      KKK1=IFIX(DAT(1))
C--->  OƒPAHˆ—ˆMCŸ.....
      IF(KKK1.GT.(6-KPAR)) KKK1=6-KPAR
      IF(KKK1.LT.0) CALL SUBERR(KKK1)
C--->  PˆEM —ˆCE‹
      DO 40 I=1,KKK1
 40   PAR(KPAR+I)=DAT(I+1)
      KPAR=KPAR+KKK1
C--->  E™E H“†HO ?
      GO TO 5
C**********************************************************************
 10   CONTINUE
      ISQ=PAR(2)
      ISD=PAR(3)
      IRQ=PAR(4)
      IRD=PAR(5)
      OMEGA=PAR(6)
      NELM=IFIX(PAR(1))
C--->  ˆCKATœ B MSWALL ‹EMEHT C HOMEPOM NELM:
      CALL SEQMS0(MSWALL,NELM,AMOT,IRC0,KVTV,ATOKM)
      IF(IRC0.EQ.1) GO TO 11
C--->  ‹EMEHTA HET.
      WRITE (NWTR,1005)NELM
 1005 FORMAT(/' *** –Eœ HE CO„EP†ˆT ‹EMEHTA # ',I5,' ***'/
     *' *** „EPEKTˆBA ˆƒHOPˆP“ETCŸ ***')
      RETURN
 11   CONTINUE
C--->  —TO ‡A Tˆ ?
      TIPX=MSWALL(AMOT+1)
      DO 12 I=1,KLTAB
      IF(TIPX.EQ.LTAB(I)) GO TO 13
 12   CONTINUE
C--->  HE HA˜ Tˆ......
      WRITE (NWTR,1006) NELM,TIPX,(M3X3X0(I),I=1,6)
 1006 FORMAT(/' *** ‹EMEHT –Eˆ # ',I5,' ( Tˆ ',I4,') ***'/
     *' *** HE MO„E‹œ ‹. MA˜ˆH› ',6A1,' ***'/
     *' *** „ˆPEKTˆBA ˆƒHOPˆP“ETCŸ ***'/)
      RETURN
 13   CONTINUE
C--->  BCE O'KE‰....
      WRITE (NWTR,1007) NELM,TIPX,(M3X3X0(I),I=1,6),
     *                  ISQ,ISD,IRQ,IRD,OMEGA,GAMMA
 1007 FORMAT(/' *** „‹Ÿ ‹EMEHTA –Eˆ # ',I5,' ( Tˆ ',I4,')',
     = 19X, ' ***'/
     *' *** ‹. MA˜ˆH› ',6A1,' “CTAHOB‹EHO:', 26X, ' ***'/
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
 1008 FORMAT(' *** ˆ‹ˆ „‹Ÿ ”A‡H›X ‡HA—EHˆ‰ ( OCœ Q COBA„AET',
     *' C ”A‡O‰ "A" ): ***'/
     *' *** ISA=',G12.5,' ISB=',G12.5,' ISC=',G12.5,'       ***'/
     *' *** IRA=',G12.5,' IRB=',G12.5,' IRC=',G12.5,'       ***'/)
C--->  PACTABˆTœ ‡HA—EHˆŸ.
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
 1009 FORMAT(/' *** „ˆPEKTˆBA " #MPE†ˆM" PEA‹ˆ‡OBAHA „‹Ÿ MO„E‹E‰ ***'/
     *' *** ˆMŸ MO„E‹ˆ:',6A1,' HOMEPA TˆOB: ***'/
     *' *** ',10I6,' ***')
      WRITE (NWTR,1010)
 1010 FORMAT(' *** ˆƒHOPˆP“ETCŸ ***'/)
      RETURN
 666  CONTINUE
      WRITE (NWTR,1011) NRDR
 1011 FORMAT(/' *** KOHE– HA KAHA‹E BBO„A ',I2,' [ DRRMMM ] ***'/
     *' *** PAOTA PEP›BAETCŸ ***'//)
      CALL SUBERR(0)
      RETURN
      END
