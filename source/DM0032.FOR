         SUBROUTINE DM0032 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  DM0032 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA�HA�EH�E:    MOH�TOP MAKPOMO�E�� MEXAH��ECKO� C�CTEM� (T�� 999)
C   -----------
C
C                  B �AB�C�MOCT� OT �HA�EH�� ��A�A ZAPROS [MASPRT(28)]:
C                  B��O�H�ET C�E�����E ��HK���:
C
C                  A) ZAPROS = 1
C                     ----------
C
C
C
C
C
C
C               *** BO�MO�H� �TEPA��OHH�E �TO�HEH�� ***:
C                   �P� BO�����EH�� FITER [ MASPRT(65) ] --->
C                   �HA�EH�� "TOK"_� - �PO�HO� �BH�M METO�OM -
C                   HE B���C���TC�, A B���PA�TC� �� MACC�BA TOKN1;
C               ***
C                  B) ZAPROS = 2
C                     ----------
C                  B���C�EH�E �HA�EH�� TOKOB B BETB�X B T(N+1) MOMEHT
C                  BPEMEH�   �CXO�� �� G � J �APAMETPOB � �HA�EH��
C                  ���OB�X HA�P��EH�� HA ��EMEHTE PAC��TAHH�X ��� T(N+1)
C                    � �A��C� (��KCA���) �T�X �HA�EH�� B "TOK T(N+1)";
C                 - �P� �TOM KOHTPO��P��ETC� "PACXO��EH�E" ME���
C                   �HA�EH��M�, �O���EHH�M� �O �BHO� CXEME (�PO�HO�) �
C                   O�PE�E�EHH�M� �CXO�� �� B���C�EHH�X �HA�EH�� VALLN1;
C                   KOHTPO�� BE�ETC� �O HE �PEB��EH�� EITER[MASPRT(67)]
C                   OTHOC�TE��HO� �O�PE�HOCT�. B HE ��A�O�P��THOM C���AE
C                   B "MAXITR" �A��C�BAETC� MAX_�O�PE�HOCT� �
C                   B "NLITER" ��KC�P�ETC� HOMEP ��EMEHTA.....
C                   �APAMETP "KOD" = 1;
C
C    METO�:                                                          METO��:
C   -----                                                            -----
C
C
C   �APAMETP�:                                                       �APAMETP�:
C   ---------     RMSW - ��PAB����EE C�OBO COCTO�H�� MAKPOMO�E��     ---------
C                 PE�OK" MACC�B� MSWALL -> C.M. �/� GETMSW );
C                 - MATP��A �P0BO��MOCT� AHA����P�EMO� �E��;
C                 - BEKTOP �CTO�H�KOB TOKA AHA����P�EMO� �E��;
C                  - BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                    B T = TN;
C                 1- BEKTOP ���OB�X HA�P��EH�� AHA����P�EMO� �E��:
C                    B T = TN + H0;
C                  �  TOKN1  -  �HA�EH�� "TOKOB" �P� T :=
C                               TN  �  TN + H0;
C                             PA�MEPHOCT� CM. MASPRT(46)=NNTOK;
C                 R - BEKTOP �P��HAKOB K.�. ���OB:
C                 ���ETC� B MAKPOMO�E��X C �P��HAKOM T��A = 5;
C                 PHOC� MACC�BOB �P���� "ALL" O�PE�E���TC�:
C                 DIMENSION ---> NDALL = MASPRT(23)
C                 �A�_PA�MEPHOCT� ---> NSYS = MASPRT(26)
C                  �APAMETP O�PATHO� CB��� C MOH�TOPOM DM....
C                  �P� �O���O� �OK. �O�PE�HOCT� );
C
C
C   �O��PO�PAMM�
C   ------------  MMON PROF1, PROF2, PROF3;
C           ���
C          � "MOVE" TPE���TC� C�E�����E �O��PO�PAMM�:
C          MOVE - �POBEPKA CTP�KT�P �AHH�X ;
c          GRZ  - B���C�EH�E MATP�� �OBOPOTOB  �  SIN
c                 � COS  O�O��EHH�X KOOP��HAT
C          KST  - B���C�EH�E �POEK��� BEKTOPOB � TEH�OPOB
C      �HEP��� B HE�O�B��HO� C�CTEME KOOP��HAT  DM000840
C     1. CHMOVE - �POBEPKA CTP�KT�P �AHH�X ;
C     2. PAGRZ  - B���C�EH�E MATP�� �OBOPOTOB  �  SIN
C                 � COS  O�O��EHH�X KOOP��HAT
C     3. NEKST  - B���C�EH�E �POEK��� BEKTOPOB � TEH�OPOB
C                  4. OME    - B���C�EH�E �POEK��� BEKTOPOB B
C                              HE�O�B��HO� C�CTEME KOOP��HAT
C                  5. DVVVV  - B���C�EH�E AHA�O�OB CKOPOCTE� �EHTPOB
C                              MACC �BEH�EB
C                  6. DAMATR - B���C�EH�E MATP��� �HEP���
C                  7. DSUMMA - B���C�EH�E �POEK��� BEKTOPOB  �
C                              HEB��OK �PABHEH�� ��HAM�K�
C                  8. DAPGRS - B���C�EH�E MATP���  S
C                  9. GRAV   - B���C�EH�E BHE�H�X MOMEHTOB � C��
C                              �E�CTB����X HA �EHTP� MACC �BEH�EB.
C                              (C��TAETC�, �TO �OTEH��A��H�E C���
C                               �E�CTB��T �O OC�  Y0)
C                 10. SYSER  - COO��EH�� O �ATA��H�X � �H�OPMA��OHH�X
C                              O���KAX
C
C
C
C   �P�ME�AH�E:  1. �P� ZAPROS ^=   1 ��� 2 - COO��EH�E � �XO�;
C   ----------
C
C               "�OCTO�HHA�" �ACT� MSW T��A # 999;
C                ---------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I �P��HAKI  HE  I KO�. IA�P.   IPE I
C  I ��EME I T��A   I �/�    I T��A   I �C�O-I B�B. IHA�.   I�E I
C  I HTA   I MO�E�� I PAC�ETAI MO�E�� I ����-I �HA� I"TOKN" IPB I
C  I       I        I DM.....I       .I ETC� I EH��.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  0    I  999   I  32    I    3   I   0  I  3*N I  ???  I -1I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C                       RMSW T�� # 999;
C                      ****************
C
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II KO�-BO I KO�-BO I # �BEHAI # ��-TAI # CME- I A�PEC  I ...==>
C     II �BEH�EBI CB��E� I MEX.   I ��.    I �EH��  I   B    I ...==>
C     II        I C ��.�.I �E��   I �E��   I        I TOKN/N1I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   1    I    2   I   3    I   4    I   5    I   6    I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   9    I   10   I   11   I   12   I   13   I   14   I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C
C      BCE �APAMETP� �EPE�A�TC� �P� �OMO�� "COMMON" ��OKOB.
C
C     B MACC�BE TOKN � TOKN1 CO�EP��TC�:
C
C
C     TOKN(1)     - TOKN(N)   - O�O��EHH�E KOOP��H�T� C�CTEM�
C
C     TOKN(N+1)   - TOKN(2*N) - O�O��EHH�E CKOPOCT� C�CTEM�
C
C     TOKN(2*N+1) - TOKN(3*N) - O�O��EHH�E �CKOPEH�� C�CTEM�
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
C----------------------------------------------------------------------
         COMMON /BLQ/ Q(30),QP(30),Q2P(100)
         COMMON /BBB/ B(60,120)
         COMMON /BLSPK/ QQ(270)
         COMMON /BLMATR/ AMATR(60,60),V(2700),RNEV(100)
         COMMON /BLINT/ N,N3,N9,M(990),NSAIT,IS(29)
         COMMON /SYS1/ KEY(20),LIM
         COMMON /BLDATE/ I1,I2,I3,I4,I5,KUK
C----------------------------------------------------------------------
      COMMON/BLKUST/RKST(90),ROKST(90),AIKST(270)
      COMMON/BLNEK/E(90),R(90),RO(540),RM(30)
C!!!!!!!!!!!!!!!!!!#########!!!!!!!!!!!!!!!!!--------------------------
C??????  DIMENSION  X0(1),XP0(1),X2P0(1),P0(1),PP0(1)
         DIMENSION  X0(10),XP0(10),X2P0(10),P0(10),PP0(10)
         DIMENSION X2P(100),PP(100),P(100)
C----------------------------------------------------------------------
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER,  ZAPROS, TIP
         INTEGER ADRMSW
         REAL MAXITR
C####################################################################
C   NITER - KO�-BO �TEPA���
C   JR    - �P��HAK �C�O���OBAH�� �/� CHMOVE ( �POBEPKA �AHH�X )
C   LL    - �P��HAK �O�O�H�TE��H�X CB��E� (�C�O����ETC� B �/� SWITCH)
C           �KA��BAET, ��MEH��C� �� B �PO�ECCE PA�OT�  MACC�B
C           �P��HAKOB  IP(N) ( B COMMON/BLSAIT/ )
C           LL = 0 - C�CTEMA �O�O�H�TE��H�X CB��E� HE ��MEH��AC�;
C           LL =-1 - XOT� �� O�HA CB��� OTK�����AC�;
C           LL = 1 - �O�AB��AC� XOT� �� O�HA CB��� ( ��AP );
         DATA NITER,JR,LL/3,0,0/
         NWTRE = MASPRT(14)
C####################################################################
       TIP = MSW(2)
       IF ( TIP .NE. 999 ) GO TO 9999
C--------------------------------------------------------------------
C-->  �KA�-�� HA TOK...
       ITOK = MSW(7)
C--->  B��OP PE��MA �A�POCA..
       GO TO( 100, 200), ZAPROS
C===> �HA�E O���KA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOH�TOP #', I4, ' �A�POC O�C����BAH�� O���O�EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ��HOP�P�ETC� ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  HE �POC�T �� �TEPA��� ?
C????? IF ( FITER .EQ. 1 ) GO TO ????
C--------------------------------------------------------------------
C    AHA�O� �/�  "MOVE" KOM��EKCA  "CADR"
C--------------------------------------------------------------------
C           O��CAH�E ��EHT���KATOPOB
C--------------------------------------------------------------------
C   X0  - MACC�B O�O��EHH�X KOOP��HAT
C   XP0 - MACC�B O�O��EHH�X CKOPOCTE�
C   X2P0- MACC�B O�O��EHH�X �CKOPEH��
C   P0  - MACC�B �HA�EH�� �O�O�H�TE��H�X �EPEMEHH�X
C   PP0 - MACC�B �HA�EH�� �PO��BO�H�X �O�O�H�TE��H�X �EPEMEHH�X
C   NP  - ��C�O �O�O�H�TE��H�X �EPEMEHH�X
C   T0  - BPEM� HA�A��HOE
C   T1  - BPEM� KOHE�HOE
C   NSOLU-��C�O �A�OB �HTE�P�POBAH��
C   NMATR-�APAMETP, O�PE�E������ �EPE� CKO��KO �A�OB �HTE�P�POBAH��
C         ���ET B��O�H�T�C� �EPEC�ET MATP��� 'A'
C   NDRK -�APAMETP, O�PE�E������ �EPE� CKO��KO �A�OB �HTE�P�POBAH��
C         B��AETC� �E�AT� PE����TATOB
C####################################################################
C   �/�  'MOVE' B���BAETC�:
C   SUBROUTINE MOVE(X0,XP0,X2P0,P0,PP0,NP,T0,T1,NSOLU,NMATR,NDRK)
C####################################################################
C--> �A�AEM BXO�H�E �APAMETP�
      DO 900 I=1,N
      X0(I)  = TOKN(ITOK+I-1)
      XP0(I) = TOKN(ITOK+I-1+N)
      X2P0(I)= TOKN(ITOK+I-1+2*N)
900   CONTINUE
      T0 = TN
      T1 = TN+H0
      NSOLU = 1
      NMATR = 10
      NDRK  = 1
C--------------------------------------------------------------------
      I1=NP
      I2=NSOLU
      I3=NITER
      I4=NMATR
      I5=NDRK
C--------------------------------------------------------------------
      DO 1001 I=1,N
      Q(I)=X0(I)
      QP(I)=XP0(I)
      Q2P(I)=X2P0(I)
      Q2P(I+N)=0.
      X2P0(I+N)=0.
1001  CONTINUE
C--> �POBEP�M �AHH�E
      IF (JR.EQ.0) CALL CHMOVE(JR,NSOLU,NMATR,T0,T1,NDRK)
C--------------------------------------------------------------------
C  H - �A� �HTE�P�POBAH��
C  T - �PE�����EE TEK��EE BPEM�
C  TT- �OC�E����EE TEK��EE BPEM�
C  KUK-HOMEP TEK��E�O �A�A �HTE�P�POBAH��
C--------------------------------------------------------------------
      H=(T1-T0)/NSOLU
      H2=.5*H
      H26=H*H/6.
      T=T0
      TT=T+H
      IF( TN .EQ. 0.) KUK=-1
      KUK=KUK+1
1030  IF(LL.NE.0) GOTO 1020
      IF(MOD(KUK,NMATR).NE.0) GO TO 1018
C--------------------------------------------------------------------
C   �PO��BO��M �EPEC�ET MATP���  'A'
C   � PAC��T�BAEM �HA�EH�� O�O��EHH�X �CKOPEH�� ��� T=TN
C--------------------------------------------------------------------
1020  CALL PAGRZ
      CALL NEKST
      CALL OME
      CALL DVVVV
      CALL DAMATR
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(11).EQ.1)CALL DCMTR
C     IF(KEY(7).EQ.1)CALL UDCMTR
C     IF(KEY(2).EQ.1)CALL FORSE(T,P)
C--------------------------------------------------------------------
      IF(KEY(2).EQ.0)CALL GRAV
      CALL CONTRL(T,P)
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(11).EQ.1)CALL SAITE
C     IF(KEY(7).EQ.1)CALL USAITE
C--------------------------------------------------------------------
      CALL DSUMMA
      NM=N+NSAIT
      CALL DAPGRS(NM,II)
      IF(II.NE.0) RETURN
      DO 1005 I=1,NM
      SS=0.
      DO 1004 J=1,NM
      SS=SS+B(I,J)*RNEV(J)
1004  CONTINUE
      X2P0(I)=X2P0(I)-SS
      Q2P(I)=X2P0(I)
1005  CONTINUE
      IF(KUK.NE.0) GO TO 1018
C--------------------------------------------------------------------
C--> �A�OM�HAEM �HA�EH�� O�O��EHH�X �CKOPEH�� ��� T=T0
C--------------------------------------------------------------------
       DO 1150 I=1,N
       TOKN(ITOK + 2*N + I - 1) = X2P0(I)
1150   CONTINUE
       WRITE(NWTRE,1989)
     * T,X0(1),X0(2),XP0(1),XP0(2),X2P0(1),X2P0(2),QQ(181),QQ(182)
1989   FORMAT( 1X, 79('-')/
     *         1X, ' BPEM�        ', G12.5 /
     *         1X, ' �EPEME�EH��  ', G12.5, 5X, G12.5 /
     *         1X, ' CKOPOCT�     ', G12.5, 5X, G12.5 /
     *         1X, ' �CKOPEH��    ', G12.5, 5X, G12.5 /
     *         1X, ' O�O��. C���  ', G12.5, 5X, G12.5 /
     *         1X, 79('-') )
1018  IF(NP.EQ.0) GOTO 1019
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(1).EQ.1)  CALL DRIVER(P0,T,PP0)
C--------------------------------------------------------------------
      DO 1006 I=1,NP
      PP(I)=PP0(I)
1006  CONTINUE
C--------------------------------------------------------------------
C--> HA��HAEM �TEPA��OHH�� PAC�ET O�O��EHH�X
C    �EPEMEHH�X ��� C�E����E� BPEMEHHO� TO�K�
C--------------------------------------------------------------------
1019  DO 1012 L=1,NITER
      DO 1007 I=1,N
      Q(I)=X0(I)+XP0(I)*H+(2.*X2P0(I)+Q2P(I))*H26
      QP(I)=XP0(I)+(X2P0(I)+Q2P(I))*H2
1007  CONTINUE
      IF(NP.EQ.0) GOTO 1009
      DO 1008 I=1,NP
      P(I)=P0(I)+(PP0(I)+PP(I))*H2
1008  CONTINUE
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(1).EQ.1)CALL DRIVER(P,TT,PP)
C--------------------------------------------------------------------
1009  CALL PAGRZ
      CALL NEKST
      CALL OME
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(2).EQ.1)CALL FORSE(TT,P)
C--------------------------------------------------------------------
      IF(KEY(2).EQ.0) CALL GRAV
      CALL CONTRL(TT,P)
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(11).EQ.1)CALL SAITE
C     IF(KEY(7).EQ.1)CALL USAITE
C--------------------------------------------------------------------
      CALL DSUMMA
      DO 1011 I=1,NM
      SS=0.
      DO 1010 J=1,NM
      SS=SS+B(I,J)*RNEV(J)
1010  CONTINUE
      Q2P(I)=Q2P(I)-SS
1011  CONTINUE
1012  CONTINUE
C--------------------------------------------------------------------
C--> �TEPA��� �AKOH����C�.
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(3).EQ.1)CALL SWITCH(X0,XP0,X2P0,T,TT,LL)
C--------------------------------------------------------------------
      IF(LL.NE.1) GOTO 1025
C--------------------------------------------------------------------
C--> B �PO�ECCE PA�OT� MEXAH��ECKO� C�CTEM� �O�AB��AC�
C    MEXAH��ECKA� CB���(��AP). �EPEC��TAEM MATP��� 'A' �
C    O�O��EHH�E �EPEMEHH�E
C--------------------------------------------------------------------
      CALL PAGRZ
      CALL NEKST
      CALL OME
      CALL DVVVV
      CALL DAMATR
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(11).EQ.1)CALL DCMTR
C     IF(KEY(7).EQ.1)CALL UDCMTR
C--------------------------------------------------------------------
      NM=N+NSAIT
      CALL DAPGRS(NM,II)
      IF(II.NE.0)RETURN
      DO 1022 I=1,N
      SS=0.
      DO 1021 J=1,N
      SS=SS+AMATR(I,J)*QP(J)
1021  CONTINUE
      X2P(I)=SS
1022  CONTINUE
      DO 1024 I=1,N
      SS=0.
      DO 1023 J=1,N
      SS=SS+B(I,J)*X2P(J)
1023  CONTINUE
      QP(I)=SS
1024  CONTINUE
C--------------------------------------------------------------------
C--> �EPE�P�CBO�M PAC��TAHH�E O�O��EHH�E �EPEMEHH�E
C    � �A�OMH�M �X.
C--------------------------------------------------------------------
1025  DO 1013 I=1,N
      X0(I)=Q(I)
      XP0(I)=QP(I)
1013  CONTINUE
      DO 1014 I=1,N
      IF(ABS(Q2P(I)).GT.10000.) GO TO 9991
      X2P0(I)=Q2P(I)
1014  CONTINUE
      IF(NSAIT.EQ.0) GO TO 1051
      J=N+1
      DO 1050 I=J,NM
      X2P0(I)=Q2P(I)
1050  CONTINUE
1051  IF(NP.EQ.0)GOTO 1016
      DO 1015 I=1,NP
      PP0(I)=PP(I)
      P0(I)=P(I)
1015  CONTINUE
1016  T=TT
      TT=T+H
      IF(MOD(KUK+1,NDRK).NE.0) GOTO 1017
C--------------------------------------------------------------------
C--> B �AHHOM PE��ME C�E�����E �/� HE �C�O�����TC�
C     IF(KEY(6).EQ.1)CALL FUNK(T,P0,PP0,NP)
C     IF(KEY(5).EQ.1.OR.KEY(10).EQ.1)CALL DRES(T,P0,PP0,NP)
C--------------------------------------------------------------------
       DO 1250 I=1,N
       TOKN1(ITOK       + I - 1) =   X0(I)
       TOKN1(ITOK +   N + I - 1) =  XP0(I)
1250   TOKN1(ITOK + 2*N + I - 1) = X2P0(I)
       WRITE(NWTRE,1989)
     * T,X0(1),X0(2),XP0(1),XP0(2),X2P0(1),X2P0(2),QQ(181),QQ(182)
1017    LL=0
        RETURN
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C---- ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C----------------------------------------------------------------------
C----------------------------------------------------------------------
C--->  �EPE�A��C� TOKOB  B   TOKN1...
       DO  250 I=1,N
       TOKN1(ITOK       + I - 1) =   X0(I)
       TOKN1(ITOK +   N + I - 1) =  XP0(I)
       TOKN1(ITOK + 2*N + I - 1) = X2P0(I)
250    CONTINUE
       RETURN
C######################################################################
9991     CONTINUE
         WRITE(NWTRE,9992) MSW(1), MSW(2)
9992     FORMAT( ' *** ��C�EHHA� HECXO��MOCT� �P� MO�E��POBAH�� MEX.C�CT
     =EM� ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C######################################################################
9999     CONTINUE
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** �O�. C�O� B DM0032 - �A�POC HA O�C����BAH�E ���O�
     =O T��A ***'/
     =    ' *** ��EMEHT # ', I10, ' T�� MO�E�� # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
C######################################################################
C        DEBUG INIT
         END
