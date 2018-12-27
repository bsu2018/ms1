         SUBROUTINE DM0020 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT „EKAPŸ 1987 ƒ.)------------
C   O„POƒPAMMA:  DM0020 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA‡HA—EHˆE:
C   -----------              MOHˆTOP MO„E‹E‰:
C               +-----+--------------------------------------+
C               I     I                                      I
C               I Tˆ I    HAˆMEHOBAHˆE ‹EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 333 I –ˆ”POBO‰  RS-TPˆƒƒEP                 I
C               +-----+--------------------------------------+
C               I 334 I –ˆ”POBO‰   D-TPˆƒƒEP                 I
C               +-----+--------------------------------------+
C               I 335 I –ˆ”POBO‰   T-TPˆƒƒEP                 I
C               +-----+--------------------------------------+
C               I 360 I KOMAPATOP(1)                        I
C               +-----+--------------------------------------+
C               I 361 I KOMAPATOP(2)                        I
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
C              ”OPMAT RMSW Tˆ # 33X ˆ 360:
C              ----------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  TX -BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ
C            I              I      I
C    11      I     2        I  DM  I  ”‹Aƒ KD =0 - B›X. CˆƒHA‹ HE ˆ‡MEH.
C            I              I      I          =1 - B›X. CˆƒHA‹ ˆ‡MEHˆ‹.
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C    15      I     6        I  FM  I  #‹2 -      -"-
C    16      I     7        I  FM  I  #CM2 -      -"-
C    17      I     8        I  LM  I  ATOKM2 -    -"-
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 17
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 8
C
C----------------------------------------------------------------------
C
C
C
C
C              ”OPMAT RMSW Tˆ # 335:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  TX -BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ
C            I              I      I
C    11      I     2        I  DM  I  ”‹Aƒ KD =0 - B›X. CˆƒHA‹ HE ˆ‡MEH.
C            I              I      I          =1 - B›X. CˆƒHA‹ ˆ‡MEHˆ‹.
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 14
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 5
C
C----------------------------------------------------------------------
C
C
C
C                   ”OPMAT RMSW Tˆ # 361:
C                   ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0016    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  TX- BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ
C            I              I      I
C    11      I     2        I  DM  I  ”‹Aƒ KD =0 - B›X. CˆƒHA‹ HE ˆ‡MEH.
C            I              I      I          =1 - B›X. CˆƒHA‹ ˆ‡MEHˆ‹.
C    12      I     3        I  FM  I  “1   - HOMEP “‡‹A;
C    13      I     4        I  FM  I  “2   - HOMEP “‡‹A;
C    14      I     5        I  FM  I  “3   - HOMEP “‡‹A;
C    15      I     6        I  FM  I  “4   - HOMEP “‡‹A;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 15
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 6
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
         NTIP = TIP - 332
         IF ( NTIP .GE. 1  .AND. NTIP .LE. 3 ) GO TO 11
         IF ( TIP .EQ. 360 .OR. TIP .EQ. 361 ) GO TO 11
C######################################################################
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), TIP
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0020 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
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
         MSW(11) = 0
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- Z A P R O S = 3 -----TOKN, TOKN1, VALLN, VALLN1 OPE„E‹EH›-------
600      CONTINUE
C==>  —TO ‡A Tˆ?
         GO TO ( 610 , 620 , 630 ), NTIP
         IF( TIP .EQ. 360 ) GO TO 640
         IF( TIP .EQ. 361 ) GO TO 650
C-->  O˜ˆKA. KOH—AEM...
         CALL SUBERR(TIP)
         RETURN
C######################################################################
610      CONTINUE
C======================================================================
C     –ˆ”POBO‰ RS-TPˆƒƒEP
C----------------------------------------------------------------------
         SIG1 = IFIX( TOKN ( MSW(14) ) + 0.5 )
         SIG11= IFIX( TOKN1( MSW(14) ) + 0.5 )
         TIME1 = TOKN1( MSW(14) + 3 - MSW(13) )
         SIG2 = IFIX( TOKN ( MSW(17) ) + 0.5 )
         SIG21= IFIX( TOKN1( MSW(17) ) + 0.5 )
         TIME2 = TOKN1( MSW(17) + 3 - MSW(16) )
         IF( SIG1 .EQ. SIG11 .AND. SIG2 .EQ. SIG21 ) GO TO 617
C---> ‡A‰MEMCŸ B›XO„H›M CˆƒHA‹OM...
         IF( TIME1 .GT. TIME2 ) GO TO 615
C----------------------------------------------------------------------
C---> CˆƒHA‹ HA BXO„E  "S" ˆ‡MEHŸETCŸ PAHœ˜E.
         TIME = TIME1
         IF( SIG2 .EQ.   1   ) GO TO 663
         GO TO 662
615     CONTINUE
C----------------------------------------------------------------------
C---> CˆƒHA‹ HA BXO„E  "R" ˆ‡MEHŸETCŸ PAHœ˜E.
         TIME = TIME2
         IF( SIG1 .EQ.   1   ) GO TO 662
         GO TO 663
C----------------------------------------------------------------------
617      CONTINUE
         IF( SIG1 .EQ. 1 ) GO TO 618
         IF( SIG2 .EQ. 0 ) GO TO 661
         GO TO 663
618      CONTINUE
         IF( SIG2 .EQ. 1 ) GO TO 661
         GO TO 662
C######################################################################
620      CONTINUE
C======================================================================
C     –ˆ”POBO‰ D-TPˆƒƒEP
C----------------------------------------------------------------------
         SIG1 = IFIX( TOKN ( MSW(14) ) + 0.5 )
         SIG11= IFIX( TOKN1( MSW(14) ) + 0.5 )
         TIME1 = TOKN1( MSW(14) + 3 - MSW(13) )
         SIG2 = IFIX( TOKN ( MSW(17) ) + 0.5 )
         SIG21= IFIX( TOKN1( MSW(17) ) + 0.5 )
         TIME2 = TOKN1( MSW(17) + 3 - MSW(16) )
C---> ‡A‰MEMCŸ B›XO„H›M CˆƒHA‹OM...
         IF( TIME1 .GT. TIME2 ) GO TO 625
C---> CˆƒHA‹ HA BXO„E  "D" ˆ‡MEHŸETCŸ PAHœ˜E.
         TIME = TIME1
         IF( SIG2 .EQ. 0 ) GO TO 661
         IF( SIG11.EQ. 0 ) GO TO 663
         GO TO 662
625     CONTINUE
C---> CˆƒHA‹ HA BXO„E  "C" ˆ‡MEHŸETCŸ PAHœ˜E.
         TIME = TIME2
         IF( SIG21.EQ. 0 ) GO TO 661
         IF( SIG1 .EQ. 0 ) GO TO 663
         GO TO 662
C######################################################################
630      CONTINUE
C======================================================================
C     –ˆ”POBO‰ T-TPˆƒƒEP
C----------------------------------------------------------------------
         SIG1 = IFIX( TOKN ( MSW(14) )  )
         SIG11= IFIX( TOKN1( MSW(14) )  )
         TIME = TOKN1( MSW(14) + 3 - MSW(13) )
C---> ‡A‰MEMCŸ B›XO„H›M CˆƒHA‹OM...
         IF( SIG1 .EQ. 0 ) GO TO 661
         IF( SIG11.EQ. 1 ) GO TO 661
         ITOK = MSW(7)
         IF( TOKN(ITOK) .LE. 0.5 ) GO TO 662
         GO TO 663
C######################################################################
640      CONTINUE
C======================================================================
C     KOMAPATOP(1)
C----------------------------------------------------------------------
         SIGIN = TOKN( MSW(14) )
         SIGIN1= TOKN1( MSW(14) )
         SIGPR = TOKN ( MSW(17) )
         SIGPR1= TOKN1( MSW(17) )
641      CONTINUE
C---> ‡A‰MEMCŸ B›XO„H›M CˆƒHA‹OM...
C----OPE„E‹ˆM BXO„H›E CˆƒHA‹›  (?)------------------------------------
C     ˆCO‹œ‡“EM ‹ˆHE‰H“ž APOKCˆMA–ˆž CˆƒHˆ‹OB HA PŸMOM (PR)
C     ˆ ˆHBEPCHOM (IN) BXO„AX KOMAPATOPA ˆ HAXO„ˆM BPEMŸ COBA„EHˆŸ
C     TˆX CˆƒHA‹OB.
C         SIGPR = A * T + B
C         SIGIN = C * T + D
C----------------------------------------------------------------------
          A = (SIGPR1 - SIGPR) / H0
          C = (SIGIN1 - SIGIN) / H0
          IF( A .EQ. C ) GO TO 661
          B = SIGPR
          D = SIGIN
          HX = ( D - B ) / ( A - C )
          IF( HX .LT. 0.0  .OR.  HX .GE. H0 ) GO TO 661
C-->  HA ˜AƒE „O‹†HO POˆ‡O‰Tˆ ˆ‡MEHEHˆE CˆƒHA‹A.
          IF( HX .LT. HXMIN ) HX = HXMIN
          RMSW(10) = TN + HX
          MSW(11)  = 1
C--->  EC‹ˆ TX .LT. TX_G  ==> TO B TX_G  <=== TX...
         HXG  =  AMIN1( HX, HXG )
C                ****************
         TXG = TN + HXG
C====>  OTMETˆTœ TO B "FKOM"....
         FKOM = 1
C--->   PAC—ˆTATœ ˆ ‡AOMHˆTœ B›XO„HO‰ CˆƒHA‹ ˆ BPEMŸ EƒO OŸB‹EHˆŸ
         ITOK = MSW(7)
         TOKN1(ITOK) = 0.0
         IF( SIGIN1 .GE. SIGPR1 ) TOKN1(ITOK) = 1.0
         TOKN1(ITOK+1) = 0.0
         IF( TOKN1(ITOK) .LE. 0.5 ) TOKN1(ITOK+1) = 1.0
         TOKN1(ITOK+2) = TN + HX
         KOD = 0
         RETURN
C######################################################################
650      CONTINUE
C======================================================================
C     KOMAPATOP(2)
C----------------------------------------------------------------------
         NY1 = MSW(12)
         NY2 = MSW(13)
         NY3 = MSW(14)
         NY4 = MSW(15)
C--> OPE„E‹ˆM BXO„H›E HAPŸ†EHˆŸ „‹Ÿ  T=TN
         VNY1 = 0.0
         IF( NY1 .GT. 0 ) VNY1 = VALLN(NY1)
         VNY2 = 0.0
         IF( NY2 .GT. 0 ) VNY2 = VALLN(NY2)
         SIGIN = VNY1 - VNY2
         VNY3 = 0.0
         IF( NY3 .GT. 0 ) VNY3 = VALLN(NY3)
         VNY4 = 0.0
         IF( NY4 .GT. 0 ) VNY4 = VALLN(NY4)
         SIGPR = VNY3 - VNY4
C--> OPE„E‹ˆM BXO„H›E HAPŸ†EHˆŸ „‹Ÿ  T=TN+H0
         VNY1 = 0.0
         IF( NY1 .GT. 0 ) VNY1 = VALLN1(NY1)
         VNY2 = 0.0
         IF( NY2 .GT. 0 ) VNY2 = VALLN1(NY2)
         SIGIN1 = VNY1 - VNY2
         VNY3 = 0.0
         IF( NY3 .GT. 0 ) VNY3 = VALLN1(NY3)
         VNY4 = 0.0
         IF( NY4 .GT. 0 ) VNY4 = VALLN1(NY4)
         SIGPR1 = VNY3 - VNY4
C----------------------------------------------------------------------
C   „A‹œHE‰˜ˆ‰ PAC—ET „‹Ÿ MO„E‹ˆ KOMAPATOPA(2) POˆ‡BO„ˆTCŸ TAK†E
C   KAK ˆ „‹Ÿ MO„E‹ˆ KOMAPATOPA(1), OTOM“ EPEXO„ˆM HA METK“ 641.
C----------------------------------------------------------------------
         GO TO 641
C######################################################################
C
C       BO‡MO†H›E CTAH„APTH›E CˆT“A–ˆˆ.
C
C######################################################################
661      CONTINUE
C--->  B›XO„HO‰ CˆƒHA‹ HA ˜AƒE HE ˆ‡MEHŸETCŸ.
C----------------------------------------------------------------------
         RMSW(10) = TN + H0
         ITOK = MSW(7)
         TOKN1(ITOK) = TOKN(ITOK)
         TOKN1(ITOK+1) = 0.0
         IF( TOKN1(ITOK) .LE. 0.5 ) TOKN1(ITOK+1) = 1.0
         TOKN1(ITOK+2) = TN + H0
         MSW(11) = 0
         KOD = 0
         RETURN
C======================================================================
662      CONTINUE
C-->   CˆƒHA‹ HA B›XO„E Pˆ  T=TIME  CTA‹ PABH›M  "1".
C----------------------------------------------------------------------
         RMSW(10) = TIME
         ITOK = MSW(7)
         TOKN1(ITOK) = 1.
         TOKN1(ITOK + 1 ) = 0.
         TOKN1(ITOK + 2 ) = TIME
         MSW(11) = 1
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
         MSW(11) = 1
         KOD = 0
         RETURN
C######################################################################
500      CONTINUE
C===> ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ B›XO„HOƒO CˆƒHA‹A......ZAPROS = 4
C---->  HET ‹ˆ HEOXO„ˆMOCTˆ POˆ‡BECTˆ CMEH“ COCTOŸHˆŸ B›X.CˆƒHA‹A..?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        KOD = 0
        IF ( MSW(11) .EQ. 0 ) GO TO 510
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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