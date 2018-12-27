         SUBROUTINE DM0029 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT „EKAPŸ 1987 ƒ.)------------
C   O„POƒPAMMA:  DM0029 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA‡HA—EHˆE:
C   -----------              MOHˆTOP MO„E‹E‰:
C               +-----+--------------------------------------+
C               I     I                                      I
C               I Tˆ I    HAˆMEHOBAHˆE ‹EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 330 I –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆ"      I
C               +-----+--------------------------------------+
C               I 331 I –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆ‹ˆ"    I
C               +-----+--------------------------------------+
C               I 332 I –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT"ˆCK‹.ˆ‹ˆ"I
C               +-----+--------------------------------------+
C
C                  B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C                  B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                   HE „E‹AET Hˆ—EƒO
C
C
C                  B) ZAPROS = 2
C                     ----------
C                   HE „E‹AET Hˆ—EƒO
C
C                                                                      )
C                  C) ZAPROS = 3
C                     -----------
C                     Oˆ‡BO„ˆTCŸ PAC—ET ‡HA—EHˆ‰ B›XO„H›X CˆƒHA‹OB
C                     O ˆ‡BECTH›M BXO„H›M CˆƒHA‹AM.
C                     Pˆ TOM: FKOM = 0 - COCTOŸHˆE B›XO„H›X
C                                          CˆƒHA‹OB HE ˆ‡MEHˆ‹OCœ;
C                               FKOM = 1 - HA TEK“™EM ˜AƒE COCTOŸHˆE
C                                         „O‹†HO "›‹O" ˆ‡MEHˆTœCŸ B
C                                         B T = TX MOMEHT BPEMEHˆ;
C                                         PEA‹œHO B POƒPAMME OPE„E-
C                                         ‹ŸETCŸ HX-˜Aƒ BO‡MO†HO‰ CME-
C                                         H› COCTOŸHˆŸ, ‡ATEM TX=TN+HX;
C                                         Pˆ HX < HXMIN [MASPRT(62)]
C                                         PˆHˆMAETCŸ HX = HXMIN;
C                                         COCTOŸHˆE HX=0.0 B›‡›BAET
C                                         E—ATœ COO™EHˆŸ;
C                                         POˆ‡BE„EH› COTBETCTB“ž™ˆE
C                                         ˆ‡MEHEHˆŸ B MASPRT:
C                                          FKOM <== "1"
C                                         HXG = MIN( HX, HXG )
C                                         TXG = TN + HXG
C
C
C
C                  D) ZAPROS = 4
C                     **********
C                   - Pˆ TX_G  [MASPRT(35)] .EQ. TX[RMSW(10)] -
C               ( PABEHCTBO POBEPŸETCŸ C TO—. „O "EPSKOM"[MASPRT(39)])
C                      ”ˆKCˆP“ETCŸ CMEHA COCTOŸHˆŸ B›XO„HOƒO CˆƒHA‹A,
C                      B POTˆBHOM C‹“—AE, ‡HA—EHˆE B›XO„HOƒO CˆƒHA‹A
C                      BOCCTAHAB‹ˆBAETCŸ;
C
C   METO„:         ‹Oƒˆ—ECKOE MO„E‹ˆPOBAHˆE
C   -----
C
C   APAMETP›:     MSW, RMSW - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------      ( "OTPE‡OK" MACCˆB› MSWALL -> C.M. / GETMSW );
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  T  =  TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          Pˆ  TN + H0;
C                  TOKN  ˆ TOKN1 - ‡HA—EHˆE EPEMEHH›X "COCTOŸHˆŸ"
C                                  MO„E‹ˆ Pˆ TN  ˆ  TN + H0;
C                          ( PA‡MEPHOCTˆ -> CM. MASPRT(46) = NNTOK )
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 6;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C                  KOD - APAMETP OPATHO‰ CBŸ‡ˆ C MOHˆTOPOM DM....
C
C
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------   2. SEQSOS
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=  1, 2, 3, 4 - B›„A—A COO™EHˆŸ ˆ “XO„
C                3. Pˆ FTEST >=1 -> E—ATœ HA WTRE AKTA CMEH› COCTOŸHˆŸ
C
C
C                    "OCTOŸHHAŸ" —ACTœ MSW
C                     ----------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I‡HA—.  I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I        I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I   7   I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C              ”OPMAT RMSW Tˆ # 330,331,332:
C              ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0025    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I         PE‡EPB
C            I              I      I
C    11      I     2        I  FM  I  NBXOD-KO‹ˆ—ECTBO TOKOB, ˆCO‹œ-
C            I              I      I        ‡“EM›X KAK BXO„H›E CˆƒHA‹›;
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C .......    I   .......    I  ... I  ..........................
C  9+I*3     I     I*3      I  FM  I  #‹I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 11 + 3*NBXOD
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 2 + 3*NBXOD
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
         INTEGER FKOM, ZAPROS, TIP, SIG1, SIG11, SIG2, SIG21
C--->  KOHTPO‹œ TˆA
         TIP = MSW(2)
         NTIP = TIP - 329
         IF ( NTIP .GE. 1  .AND. NTIP .LE. 3 ) GO TO 11
C######################################################################
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), TIP
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0029 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C######################################################################
11       CONTINUE
C---> B›OP PE†ˆMA:
         GO TO ( 667, 666, 600, 500       ), ZAPROS
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
C======================================================================
C---------------Z A P R O S = 1----------------------------------------
667      CONTINUE
         KOD = 0
         RETURN
C======================================================================
C---------------Z A P R O S = 2----------------------------------------
666      CONTINUE
C---> "CPOC" ‡HA—EHˆŸ TX - BO‡MO†HOƒO BPEMEHˆ EPEK‹ž—EHˆŸ.....
         RMSW(10) = TN + H0
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 OPE„E‹EH›-------
600      CONTINUE
C==>  —TO ‡A Tˆ?
         GO TO ( 610 , 620 , 630 ), NTIP
C-->  O˜ˆKA. KOH—AEM...
         CALL SUBERR(TIP)
         RETURN
C######################################################################
610      CONTINUE
C======================================================================
C     –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆ"
C----------------------------------------------------------------------
         NBXOD = MSW(11)
         TIME  = TN + H0
         IMSW  = 11
         DO 615 I = 1,NBXOD
         IMSW = IMSW + 3
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         IF( TIME .GT. TIME1 ) TIME = TIME1
615      CONTINUE
         IMSW = 11
         DO 617 I = 1,NBXOD
         IMSW = IMSW + 3
         IBX  = IFIX (TOKN1( MSW(IMSW) ) )
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         PARAM =  TOKN( MSW(IMSW) )
         IF( TIME1 .GT. TIME ) IBX = IFIX( PARAM )
         IF( IBX .EQ. 0  ) GO TO 663
617      CONTINUE
         GO TO 662
C######################################################################
620      CONTINUE
C======================================================================
C     –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆ‹ˆ"
C----------------------------------------------------------------------
         NBXOD = MSW(11)
         TIME  = TN + H0
         IMSW  = 11
         DO 625 I = 1,NBXOD
         IMSW = IMSW + 3
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         IF( TIME .GT. TIME1 ) TIME = TIME1
625      CONTINUE
         IMSW = 11
         DO 627 I = 1,NBXOD
         IMSW = IMSW + 3
         PARAM = TOKN1( MSW(IMSW) )
         IBX  = IFIX ( PARAM )
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         PARAM = TOKN( MSW(IMSW) )
         IF( TIME1 .GT. TIME ) IBX = IFIX( PARAM )
         IF( IBX .EQ. 1  ) GO TO 662
627      CONTINUE
         GO TO 663
C######################################################################
630      CONTINUE
C======================================================================
C     –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT "ˆCK‹. ˆ‹ˆ"
C----------------------------------------------------------------------
         NBXOD = MSW(11)
         TIME  = TN + H0
         IMSW  = 11
         DO 635 I = 1,NBXOD
         IMSW = IMSW + 3
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         IF( TIME .GT. TIME1 ) TIME = TIME1
635      CONTINUE
         IMSW = 11
         IBIX = 0
         DO 637 I = 1,NBXOD
         IMSW = IMSW + 3
         PARAM = TOKN1( MSW(IMSW) )
         IBX  = IFIX ( PARAM )
         TIME1 = TOKN1( MSW(IMSW) + 3 - MSW(IMSW-1) )
         PARAM = TOKN( MSW(IMSW) )
         IF( TIME1 .GT. TIME ) IBX = IFIX( PARAM )
         IBIX = IBIX + IBX
637      CONTINUE
         IF( IBIX .EQ. 0  .OR.  IBIX .EQ. NBXOD ) GO TO 663
C######################################################################
C
C       BO‡MO†H›E CTAH„APTH›E CˆT“A–ˆˆ.
C
C######################################################################
662      CONTINUE
C-->   CˆƒHA‹ HA B›XO„E Pˆ  T=TIME  CTA‹ PABH›M  "1".
C----------------------------------------------------------------------
         RMSW(10) = TIME
         ITOK = MSW(7)
         TOKN1(ITOK) = 1.
         TOKN1(ITOK + 1 ) = 0.
         TOKN1(ITOK + 2 ) = TIME
         KOD = 0
         RETURN
C======================================================================
663      CONTINUE
C-->   CˆƒHA‹ HA B›XO„E Pˆ  T=TIME  CTA‹ PABH›M  "0".
C----------------------------------------------------------------------
         RMSW(10) = TIME
         ITOK = MSW(7)
         TOKN1(ITOK) = 0.
         TOKN1(ITOK + 1 ) = 1.
         TOKN1(ITOK + 2 ) = TIME
         KOD = 0
         RETURN
C######################################################################
500      CONTINUE
C===> ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ B›XO„HOƒO CˆƒHA‹A......ZAPROS = 4
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        KOD = 0
C       EC‹ˆ TXG   "PˆMEPHO" PABHO TX=MO„E‹ˆ, TO CMEHA H“†HA!
         TX   = RMSW(10)
         EPST = ABS( TX   - TXG )
C----->   (TX - TXG) = [ (TN+HX) - (TN+HXG) ]
         IF ( EPST .GT. EPSKOM ) GO TO 510
C===> ˆHA—E CMEHˆM COCTOŸHˆE....
         ITOK = MSW(7)
         TOKN1(ITOK+2) = TXG
C!!!-------------------------------------------------------------------
         IF ( FTEST .GE. 1 ) WRITE(NWTRE,501) MSW(1),TOKN1(ITOK),TN,TXG
501     FORMAT(' ***FTEST_1: B ‹-TE #',I5, ' CˆƒHA‹  CTA‹ =',F2.0,
     =          ' TN = ', E12.5, ' TXG = ', E12.5, ' ***')
C!!!-------------------------------------------------------------------
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
510      CONTINUE
C--->  B›X. CˆƒHA‹ HE ˆ‡MEHˆ‹CŸ.
         ITOK = MSW(7)
         TOKN1(ITOK) = TOKN(ITOK)
         TOKN1(ITOK+1) = 0.0
         IF( TOKN1(ITOK) .LE. 0.5 ) TOKN1(ITOK+1) = 1.0
         TOKN1(ITOK+2) = TXG
         RETURN
C----------------------------------------------------------------------
         END
