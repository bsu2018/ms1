         SUBROUTINE DM0032 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:  DM0032 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HAáHAóEHàE:    MOHàTOP MAKPOMOÑEãà MEXAHàóECKOâ CàCTEMõ (Tàè 999)
C   -----------
C
C                  B áABàCàMOCTà OT áHAóEHàü îãAÉA ZAPROS [MASPRT(28)]:
C                  BõèOãHüET CãEÑìûôàE îìHKñàà:
C
C                  A) ZAPROS = 1
C                     ----------
C
C
C
C
C
C
C               *** BOáMOÜHõ àTEPAñàOHHõE ìTOóHEHàü ***:
C                   èPà BOáÅìÜÑEHàà FITER [ MASPRT(65) ] --->
C                   áHAóEHàü "TOK"_ü - èPOÉHOá üBHõM METOÑOM -
C                   HE BõóàCãüûTCü, A BõÅàPAûTCü àá MACCàBA TOKN1;
C               ***
C                  B) ZAPROS = 2
C                     ----------
C                  BõóàCãEHàE áHAóEHàâ TOKOB B BETBüX B T(N+1) MOMEHT
C                  BPEMEHà   àCXOÑü àá G à J èAPAMETPOB à áHAóEHàü
C                  ìáãOBõX HAèPüÜEHàâ HA ùãEMEHTE PACóàTAHHõX Ñãü T(N+1)
C                    à áAèàCú (îàKCAñàü) ùTàX áHAóEHàâ B "TOK T(N+1)";
C                 - èPà ùTOM KOHTPOãàPìûETCü "PACXOÜÑEHàE" MEÜÑì
C                   áHAóEHàüMà, èOãìóEHHõMà èO üBHOâ CXEME (èPOÉHOá) à
C                   OèPEÑEãEHHõMà àCXOÑü àá BõóàCãEHHõX áHAóEHàâ VALLN1;
C                   KOHTPOãú BEÑETCü èO HE èPEBõòEHàû EITER[MASPRT(67)]
C                   OTHOCàTEãúHOâ èOÉPEòHOCTà. B HE ÅãAÉOèPàüTHOM CãìóAE
C                   B "MAXITR" áAèàCõBAETCü MAX_èOÉPEòHOCTú à
C                   B "NLITER" îàKCàPìETCü HOMEP ùãEMEHTA.....
C                   èAPAMETP "KOD" = 1;
C
C    METOÑ:                                                          METOûú:
C   -----                                                            -----
C
C
C   èAPAMETPõ:                                                       èAPAMETPõ:
C   ---------     RMSW - ìèPABãüûôEE CãOBO COCTOüHàü MAKPOMOÑEãà     ---------
C                 PEáOK" MACCàBõ MSWALL -> C.M. è/è GETMSW );
C                 - MATPàñA èP0BOÑàMOCTà AHAãàáàPìEMOâ ñEèà;
C                 - BEKTOP àCTOóHàKOB TOKA AHAãàáàPìEMOâ ñEèà;
C                  - BEKTOP ìáãOBõX HAèPüÜEHàâ AHAãàáàPìEMOâ ñEèà:
C                    B T = TN;
C                 1- BEKTOP ìáãOBõX HAèPüÜEHàâ AHAãàáàPìEMOâ ñEèà:
C                    B T = TN + H0;
C                  à  TOKN1  -  áHAóEHàü "TOKOB" èPà T :=
C                               TN  à  TN + H0;
C                             PAáMEPHOCTà CM. MASPRT(46)=NNTOK;
C                 R - BEKTOP èPàáHAKOB K.á. ìáãOB:
C                 úáìETCü B MAKPOMOÑEãüX C èPàáHAKOM TàèA = 5;
C                 PHOCà MACCàBOB ÉPìèèõ "ALL" OèPEÑEãüûTCü:
C                 DIMENSION ---> NDALL = MASPRT(23)
C                 ôAü_PAáMEPHOCTú ---> NSYS = MASPRT(26)
C                  èAPAMETP OÅPATHOâ CBüáà C MOHàTOPOM DM....
C                  èPà ÅOãúòOâ ãOK. èOÉPEòHOCTà );
C
C
C   èOÑèPOÉPAMMõ
C   ------------  MMON PROF1, PROF2, PROF3;
C           Ñãü
C          õ "MOVE" TPEÅìûTCü CãEÑìûôàE èOÑèPOÉPAMMõ:
C          MOVE - èPOBEPKA CTPìKTìP ÑAHHõX ;
c          GRZ  - BõóàCãEHàE MATPàñ èOBOPOTOB  à  SIN
c                 à COS  OÅOÅôEHHõX KOOPÑàHAT
C          KST  - BõóàCãEHàE èPOEKñàâ BEKTOPOB à TEHáOPOB
C      àHEPñàà B HEèOÑBàÜHOâ CàCTEME KOOPÑàHAT  DM000840
C     1. CHMOVE - èPOBEPKA CTPìKTìP ÑAHHõX ;
C     2. PAGRZ  - BõóàCãEHàE MATPàñ èOBOPOTOB  à  SIN
C                 à COS  OÅOÅôEHHõX KOOPÑàHAT
C     3. NEKST  - BõóàCãEHàE èPOEKñàâ BEKTOPOB à TEHáOPOB
C                  4. OME    - BõóàCãEHàE èPOEKñàâ BEKTOPOB B
C                              HEèOÑBàÜHOâ CàCTEME KOOPÑàHAT
C                  5. DVVVV  - BõóàCãEHàE AHAãOÉOB CKOPOCTEâ ñEHTPOB
C                              MACC áBEHúEB
C                  6. DAMATR - BõóàCãEHàE MATPàñõ àHEPñàà
C                  7. DSUMMA - BõóàCãEHàE èPOEKñàâ BEKTOPOB  à
C                              HEBüáOK ìPABHEHàâ ÑàHAMàKà
C                  8. DAPGRS - BõóàCãEHàE MATPàñõ  S
C                  9. GRAV   - BõóàCãEHàE BHEòHàX MOMEHTOB à Càã
C                              ÑEâCTBìûôàX HA ñEHTPõ MACC áBEHúEB.
C                              (CóàTAETCü, óTO èOTEHñàAãúHõE Càãõ
C                               ÑEâCTBìûT èO OCà  Y0)
C                 10. SYSER  - COOÅôEHàü O îATAãúHõX à àHîOPMAñàOHHõX
C                              OòàÅKAX
C
C
C
C   èPàMEóAHàE:  1. èPà ZAPROS ^=   1 àãà 2 - COOÅôEHàE à ìXOÑ;
C   ----------
C
C               "èOCTOüHHAü" óACTú MSW TàèA # 999;
C                ---------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I èPàáHAKI  HE  I KOã. IAÑP.   IPE I
C  I ùãEME I TàèA   I è/è    I TàèA   I àCèO-I BõB. IHAó.   IáE I
C  I HTA   I MOÑEãà I PACóETAI MOÑEãà I ãúáì-I áHAó I"TOKN" IPB I
C  I       I        I DM.....I       .I ETCü I EHàâ.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  0    I  999   I  32    I    3   I   0  I  3*N I  ???  I -1I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C                       RMSW Tàè # 999;
C                      ****************
C
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II KOã-BO I KOã-BO I # áBEHAI # ùã-TAI # CME- I AÑPEC  I ...==>
C     II áBEHúEBI CBüáEâ I MEX.   I ùã.    I ôEHàü  I   B    I ...==>
C     II        I C ùã.ñ.I ñEèà   I ñEèà   I        I TOKN/N1I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   1    I    2   I   3    I   4    I   5    I   6    I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   9    I   10   I   11   I   12   I   13   I   14   I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C
C      BCE èAPAMETPõ èEPEÑAûTCü èPà èOMOôà "COMMON" ÅãOKOB.
C
C     B MACCàBE TOKN à TOKN1 COÑEPÜàTCü:
C
C
C     TOKN(1)     - TOKN(N)   - OÅOÅôEHHõE KOOPÑàHàTõ CàCTEMõ
C
C     TOKN(N+1)   - TOKN(2*N) - OÅOÅôEHHõE CKOPOCTà CàCTEMõ
C
C     TOKN(2*N+1) - TOKN(3*N) - OÅOÅôEHHõE ìCKOPEHàü CàCTEMõ
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
C   NITER - KOã-BO àTEPAñàâ
C   JR    - èPàáHAK àCèOãúáOBAHàü è/è CHMOVE ( èPOBEPKA ÑAHHõX )
C   LL    - èPàáHAK ÑOèOãHàTEãúHõX CBüáEâ (àCèOãúáìETCü B è/è SWITCH)
C           ìKAáõBAET, àáMEHàãCü ãà B èPOñECCE PAÅOTõ  MACCàB
C           èPàáHAKOB  IP(N) ( B COMMON/BLSAIT/ )
C           LL = 0 - CàCTEMA ÑOèOãHàTEãúHõX CBüáEâ HE àáMEHàãACú;
C           LL =-1 - XOTü Åõ OÑHA CBüáú OTKãûóàãACú;
C           LL = 1 - ÑOÅABàãACú XOTü Åõ OÑHA CBüáú ( ìÑAP );
         DATA NITER,JR,LL/3,0,0/
         NWTRE = MASPRT(14)
C####################################################################
       TIP = MSW(2)
       IF ( TIP .NE. 999 ) GO TO 9999
C--------------------------------------------------------------------
C-->  ìKAá-ãú HA TOK...
       ITOK = MSW(7)
C--->  BõÅOP PEÜàMA áAèPOCA..
       GO TO( 100, 200), ZAPROS
C===> àHAóE OòàÅKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHàTOP #', I4, ' áAèPOC OÅCãìÜàBAHàü OòàÅOóEH
     = ***'/ ' *** ZAPROS = ', I5, ' - àÉHOPàPìETCü ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  HE èPOCüT ãà àTEPAñàà ?
C????? IF ( FITER .EQ. 1 ) GO TO ????
C--------------------------------------------------------------------
C    AHAãOÉ è/è  "MOVE" KOMèãEKCA  "CADR"
C--------------------------------------------------------------------
C           OèàCAHàE àÑEHTàîàKATOPOB
C--------------------------------------------------------------------
C   X0  - MACCàB OÅOÅôEHHõX KOOPÑàHAT
C   XP0 - MACCàB OÅOÅôEHHõX CKOPOCTEâ
C   X2P0- MACCàB OÅOÅôEHHõX ìCKOPEHàâ
C   P0  - MACCàB áHAóEHàâ ÑOèOãHàTEãúHõX èEPEMEHHõX
C   PP0 - MACCàB áHAóEHàâ èPOàáBOÑHõX ÑOèOãHàTEãúHõX èEPEMEHHõX
C   NP  - óàCãO ÑOèOãHàTEãúHõX èEPEMEHHõX
C   T0  - BPEMü HAóAãúHOE
C   T1  - BPEMü KOHEóHOE
C   NSOLU-óàCãO òAÉOB àHTEÉPàPOBAHàü
C   NMATR-èAPAMETP, OèPEÑEãüûôàâ óEPEá CKOãúKO òAÉOB àHTEÉPàPOBAHàü
C         ÅìÑET BõèOãHüTúCü èEPECóET MATPàñõ 'A'
C   NDRK -èAPAMETP, OèPEÑEãüûôàâ óEPEá CKOãúKO òAÉOB àHTEÉPàPOBAHàü
C         BõÑAETCü èEóATú PEáìãúTATOB
C####################################################################
C   è/è  'MOVE' BõáõBAETCü:
C   SUBROUTINE MOVE(X0,XP0,X2P0,P0,PP0,NP,T0,T1,NSOLU,NMATR,NDRK)
C####################################################################
C--> áAÑAEM BXOÑHõE èAPAMETPõ
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
C--> èPOBEPàM ÑAHHõE
      IF (JR.EQ.0) CALL CHMOVE(JR,NSOLU,NMATR,T0,T1,NDRK)
C--------------------------------------------------------------------
C  H - òAÉ àHTEÉPàPOBAHàü
C  T - èPEÑõÑìôEE TEKìôEE BPEMü
C  TT- èOCãEÑìûôEE TEKìôEE BPEMü
C  KUK-HOMEP TEKìôEÉO òAÉA àHTEÉPàPOBAHàü
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
C   èPOàáBOÑàM èEPECóET MATPàñõ  'A'
C   à PACóàTõBAEM áHAóEHàü OÅOÅôEHHõX ìCKOPEHàâ Ñãü T=TN
C--------------------------------------------------------------------
1020  CALL PAGRZ
      CALL NEKST
      CALL OME
      CALL DVVVV
      CALL DAMATR
C--------------------------------------------------------------------
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
C     IF(KEY(11).EQ.1)CALL DCMTR
C     IF(KEY(7).EQ.1)CALL UDCMTR
C     IF(KEY(2).EQ.1)CALL FORSE(T,P)
C--------------------------------------------------------------------
      IF(KEY(2).EQ.0)CALL GRAV
      CALL CONTRL(T,P)
C--------------------------------------------------------------------
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
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
C--> áAèOMàHAEM áHAóEHàü OÅOÅôEHHõX ìCKOPEHàâ Ñãü T=T0
C--------------------------------------------------------------------
       DO 1150 I=1,N
       TOKN(ITOK + 2*N + I - 1) = X2P0(I)
1150   CONTINUE
       WRITE(NWTRE,1989)
     * T,X0(1),X0(2),XP0(1),XP0(2),X2P0(1),X2P0(2),QQ(181),QQ(182)
1989   FORMAT( 1X, 79('-')/
     *         1X, ' BPEMü        ', G12.5 /
     *         1X, ' èEPEMEôEHàü  ', G12.5, 5X, G12.5 /
     *         1X, ' CKOPOCTà     ', G12.5, 5X, G12.5 /
     *         1X, ' ìCKOPEHàü    ', G12.5, 5X, G12.5 /
     *         1X, ' OÅOÅô. Càãõ  ', G12.5, 5X, G12.5 /
     *         1X, 79('-') )
1018  IF(NP.EQ.0) GOTO 1019
C--------------------------------------------------------------------
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
C     IF(KEY(1).EQ.1)  CALL DRIVER(P0,T,PP0)
C--------------------------------------------------------------------
      DO 1006 I=1,NP
      PP(I)=PP0(I)
1006  CONTINUE
C--------------------------------------------------------------------
C--> HAóàHAEM àTEPAñàOHHõâ PACóET OÅOÅôEHHõX
C    èEPEMEHHõX Ñãü CãEÑìûôEâ BPEMEHHOâ TOóKà
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
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
C     IF(KEY(1).EQ.1)CALL DRIVER(P,TT,PP)
C--------------------------------------------------------------------
1009  CALL PAGRZ
      CALL NEKST
      CALL OME
C--------------------------------------------------------------------
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
C     IF(KEY(2).EQ.1)CALL FORSE(TT,P)
C--------------------------------------------------------------------
      IF(KEY(2).EQ.0) CALL GRAV
      CALL CONTRL(TT,P)
C--------------------------------------------------------------------
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
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
C--> àTEPAñàà áAKOHóàãàCú.
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
C     IF(KEY(3).EQ.1)CALL SWITCH(X0,XP0,X2P0,T,TT,LL)
C--------------------------------------------------------------------
      IF(LL.NE.1) GOTO 1025
C--------------------------------------------------------------------
C--> B èPOñECCE PAÅOTõ MEXAHàóECKOâ CàCTEMõ ÑOÅABàãACú
C    MEXAHàóECKAü CBüáú(ìÑAP). èEPECóàTAEM MATPàñì 'A' à
C    OÅOÅôEHHõE èEPEMEHHõE
C--------------------------------------------------------------------
      CALL PAGRZ
      CALL NEKST
      CALL OME
      CALL DVVVV
      CALL DAMATR
C--------------------------------------------------------------------
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
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
C--> èEPEèPàCBOàM PACóàTAHHõE OÅOÅôEHHõE èEPEMEHHõE
C    à áAèOMHàM àX.
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
C--> B ÑAHHOM PEÜàME CãEÑìûôàE è/è HE àCèOãúáìûTCü
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
C--->  èEPEáAèàCú TOKOB  B   TOKN1...
       DO  250 I=1,N
       TOKN1(ITOK       + I - 1) =   X0(I)
       TOKN1(ITOK +   N + I - 1) =  XP0(I)
       TOKN1(ITOK + 2*N + I - 1) = X2P0(I)
250    CONTINUE
       RETURN
C######################################################################
9991     CONTINUE
         WRITE(NWTRE,9992) MSW(1), MSW(2)
9992     FORMAT( ' *** óàCãEHHAü HECXOÑàMOCTú èPà MOÑEãàPOBAHàà MEX.CàCT
     =EMõ ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C######################################################################
9999     CONTINUE
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ãOÉ. CÅOâ B DM0032 - áAèPOC HA OÅCãìÜàBAHàE óìÜOÉ
     =O TàèA ***'/
     =    ' *** ùãEMEHT # ', I10, ' Tàè MOÑEãà # ', I10, ' ***'//)
         CALL SUBERR(0)
         RETURN
C######################################################################
C        DEBUG INIT
         END
