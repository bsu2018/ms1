         SUBROUTINE DM0023 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                       MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAŸ   1987 ƒ.)------------
C   O„POƒPAMMA:  DM0023 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                            MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  MO„E‹œ #301--> CˆCTEMA ˆM“‹œCHO-”A‡OBOƒO “PAB‹EHˆŸ
C                                    ( Cˆ”“ )
C
C
C              ‹EMEHT OˆC›BAETCŸ:
C
C       # T 301 “ 0 
C              MAS < NTOKOB  #‹1 #CM1  #‹2 #CM2 ... >
C              MAS < NHAP   “‡E‹N1 “‡E‹K1  “‡E‹N2 “‡E‹K2 ... >
C              PSUB #P < .........>;
C
C              —EPE‡ AAPAT PSUB ‡A„AžTCŸ ‡AKOH› “PAB‹EHˆŸ ;
C
C      ƒ„E
C          NTOKOB -   KO‹ˆ—ECTBO TOKOB ‹EMEHTOB CXEM› , KOTOP›E
C                     ˆCO‹œ‡“žTCŸ Pˆ PAC—ETE CˆƒHA‹OB “PAB‹EHˆŸ(Cˆ“);
C          #‹I , #CMI     - HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                            ˆCO‹œ‡“EMOƒO Pˆ PAC—ETE Cˆ“;
C          NHAP           - KO‹ˆ—ECTBO HAPŸ†EHˆ‰ ME†„“ “‡‹AMˆ CXEM› ,
C                            ˆCO‹œ‡“EM›X Pˆ PAC—ETE Cˆ“;
C          “‡E‹NI , “‡E‹KI - HOMEPA “‡‹OB , HAPŸ†EHˆŸ ME†„“ KOTOP›Mˆ
C                            ˆCO‹œ‡“žTCŸ Pˆ PAC—ETE Cˆ“;
C          #P              - HOMEP / O‹œ‡OBATE‹Ÿ ( PSUB#P ) ,
C                            B KOTOPO‰ POˆ‡BO„ˆTCŸ PAC—ET Cˆ“;
C
C
C                  A) ZAPROS = 1
C                  -------------
C                    CPOC ”‹AƒA FQ - BE‹ˆ—ˆH› “P. APAM. OPE„E‹EH›;
C                  === Hˆ —EƒO HE PAC—ˆT›BAEM ==
C
C                  B) ZAPROS = 2
C                     ----------
C                      == B C‹“—AE H“‹EBOƒO ‡HA—EHˆŸ ”‹AƒA FQ -->
C                  B›—ˆC‹EHˆE
C
C
C              FQ - PAC—ET “P. HAPŸ†EHˆ‰ B›O‹HEH -->
C                  MEXAHˆ‡M TOƒO ”‹AƒA O‡BO‹ŸET CˆHXPOHˆ‡ˆPOBATœ
C                  B›O‹HEHˆŸ PA‡‹ˆ—H›X DM_MOHˆTOPOB C .TˆA # 4;
C
C                  ‡AKOH› “PAB‹EHˆŸ --> —EPE‡ O„POƒPAMM“ O‹œ‡OBATE‹Ÿ
C
C
C                  C) ZAPROS = 3
C                     **********
C                     HE O„„EP†ˆBAETCŸ ( PE‡EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE O„„EP†ˆBAETCŸ;
C
C   METO„:     OPE„E‹EHˆE CˆƒHA‹OB “PAB‹EHˆŸ O ‡A„AHHOM“ ‡AKOH“
C   -----      B ‡ABˆCˆMOCTˆ OT TOKOB ˆ HAPŸ†EHˆ‰ ‹EMEHTOB CXEM›
C
C   APAMETP›:     MSW, RMSW - “PAB‹Ÿž™EE C‹OBO COCTOŸHˆŸ MAKPOMO„E‹ˆ
C   ---------      ( "OTPE‡OK" MACCˆB› MSWALL -> C.M. / GETMSW );
C                  YALL - MATPˆ–A P0BO„ˆMOCTˆ AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA AHA‹ˆ‡ˆP“EMO‰ –Eˆ;
C                  VALLN - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          B T = TN;
C                  VALLN1- BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ AHA‹ˆ‡ˆP“EMO‰ –Eˆ:
C                          B T = TN + H0;
C                  TOKN  ˆ  TOKN1  -  ‡HA—EHˆŸ "TOKOB" Pˆ T :=
C                                     TN  ˆ  TN + H0;
C                                   PA‡MEPHOCTˆ CM. MASPRT(46)=NNTOK;
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB:
C                  ˆCO‹œ‡“ETCŸ B MAKPOMO„E‹ŸX C Pˆ‡HAKOM TˆA = 5;
C                  PA‡MEPHOCˆ MACCˆBOB ƒP“› "ALL" OPE„E‹ŸžTCŸ:
C                   MAX_DIMENSION ---> NDALL = MASPRT(23)
C                   TEK“™AŸ_PA‡MEPHOCTœ ---> NSYS = MASPRT(26)
C                  KOD - APAMETP OPATHO‰ CBŸ‡ˆ C MOHˆTOPOM DM....
C                   ( =1 Pˆ O‹œ˜O‰ ‹OK. OƒPE˜HOCTˆ );
C                 MSWALL( (RMSWAL ) - BECœ CˆCOK MSW_ALL,
C                 ADRMSW - A„PEC TEK. C‹OAB MSW ( RMSW );
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOST, CALL_PS --> B›‡.  PSUB /;
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=   1 ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 301:
C                ---------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. IHA—.   I‡E I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C              ”OPMAT RMSW Tˆ # 301:
C              ----------------------
C
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0019    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NTOKOB -KO‹ˆ—ECTBO TOKOB, ˆCO‹œ-
C            I              I      I          ‡“EM›X Pˆ PAC—ETE Cˆ“;
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C  9+I*3     I     I*3      I  FM  I  #‹I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  NHAP -KO‹ˆ—ECTBO HAPŸ†EHˆ‰,
C            I              I      I         ˆC. „‹Ÿ PAC—ETA Cˆ“;
C13+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  “‡E‹N1 -“‡E‹ HA—A‹A HAPŸ†EHˆŸ;
C14+NTOKOB*3 I  5+NTOKOB*3  I  FM  I  “‡E‹K1 -“‡E‹ KOH–A  HAPŸ†EHˆŸ;
C .........  I ............ I .... I  ...........................
C            I              I      I
C13+NTOKOB*3+I 3+NTOKOB*3+  I      I  NPSUB - HOMEP / O‹œ‡OBATE‹Ÿ
C NHAP*2    I  NHAP*2     I  FM  I         ( PSUB ) B KOTOPO‰ POˆ‡-
C            I              I      I         BO„ˆTCŸ PAC—ET Cˆ“;
C (...)+1    I  (...)+1     I  FM  I  NPAR - KO‹ˆ—ECTBO APAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  APAMETP› PSUB , ‡A„AHH›E
C„O(...)+NPARI „O(...)+NPAR I      I  O‹œ‡OBATE‹EM ‹žC BOCEMœ
C  + 8       I   + 8        I      I  ‹EMEHTOB (HA BCŸKˆ‰ C‹“—A‰)
C----------------------------------------------------------------------
C
C     B MACCˆBE TOKN ˆ TOKN1 CO„EP†ˆTCŸ:
C     **********************************
C    OT 1 „O 6   - ‡HA—EHˆŸ MCIU;
C                  MCIU = 00  --> "Cˆ“" HET B [TN, TN+H0];
C                                  HX = 0.0;
C                  MCIU = 01  --> B [TN, TN+H0] -> EPE„Hˆ‰ ”POHT
C                                 ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹ OT
C                                 TN „O EPE„HEƒO ”POHTA "Cˆ“";
C                  MCIU = 10 -->  B [TN, TN+H0] -> ‡A„Hˆ‰ ”POHT
C                                 ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹
C                                 OT TN „O ‡A„HEƒO ”POHTA ˆM“‹œCA  ;
C                  MCIU = 11 -->  B [TN, TN+H0] "Cˆ“" BK‹ž—EH,
C                                 COCTOŸHˆE HE MEHŸ‹, HX=0.0;
C    OT 7 „O 12  - ‡HA—EHˆŸ HX
C          
C            13  - ‘“ = 1.0 --> ¢ ¬®¬¥­â ¨§¬¥­¥­¨ï ¨¬¯ã«ìá®¢ ã¯à ¢«¥­¨ï   
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
c??????? DIMENSION INTMAS(1), STMAS(100), DGMAS(12)
         DIMENSION INTMAS(10), STMAS(100), DGMAS(12)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         INTEGER ADRMSW
         REAL JALL, JNEL, MAXITR,INTMAS
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NNTOK,  MASPRT(46) )
         EQUIVALENCE ( NSYS,   MASPRT(26) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( FITER,  MASPRT(65) )
         EQUIVALENCE ( EITER,  MASPRT(67) )
         EQUIVALENCE ( NLITER, MASPRT(79) )
         EQUIVALENCE ( MAXITR, MASPRT(80) )
         EQUIVALENCE ( NSITER, MASPRT(88) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER FITER, TIP, ZAPROS
       KOD = 0
C**********************************************************************
       TIP = MSW(2)
       IF ( TIP .EQ. 301 ) GO TO 9900
C######################################################################
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0023 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
C######################################################################
9900   CONTINUE
C--->  B›OP PE†ˆMA ‡APOCA..
6666   GO TO( 100, 200), ZAPROS
C----------------------------------------------------------------------
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- ZAPROS = 1 ------------------------------------------------------
100    CONTINUE
C==> CPOC ”‹AƒA       OCHOBHO‰ PAC—ET B›O‹HEH;
       MSW(10) = 0
       RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-----ZAPROS = 2 ------------------------------------------------------
200    CONTINUE
C-->  H“†HO ‹ˆ PAOTATœ ?
       IF ( MSW(10) .EQ. 1 ) RETURN
C----------------------------------------------------------------------
C-->  Pˆ„ETCŸ .............................
C-->  ZAPROS = 2 --> PAC—ˆTATœ ‡HA—EHˆŸ....
       STMAS( 1) = TN
       STMAS( 2) = H0
       ISTMAS = 1
C----------------------------------------------------------------------
C--> EPE„AEM TOKˆ
       NTOKOB = MSW(11)
       IF( NTOKOB .EQ. 0 ) GO TO 16
          DO 15 I= 1,NTOKOB
          ISTMAS = ISTMAS + 2
          ITOKN = MSW( 3*I + 11 )
          STMAS(ISTMAS) = TOKN(ITOKN)
          STMAS(ISTMAS+1) = TOKN1(ITOKN)
15     CONTINUE
16     CONTINUE
C----------------------------------------------------------------------
C--> EPE„AEM HAPŸ†EHˆŸ
C----------------------------------------------------------------------
       NNAPR = MSW (12 + 3*NTOKOB )
       IF( NNAPR .EQ. 0 ) GO TO 18
       IMSWV = 11 + 3*NTOKOB
          DO 17 I= 1,NNAPR
          ISTMAS = ISTMAS + 2
          IMSWV = IMSWV + 2
          NA = MSW( IMSWV )
          KO = MSW( IMSWV + 1 )
          VNA = 0.0
          IF( NA .GT. 0 ) VNA = VALLN(NA)
          VNA1= 0.0
          IF( NA .GT. 0 ) VNA1= VALLN1(NA)
          VKO = 0.0
          IF( KO .GT. 0 ) VKO = VALLN(KO)
          VKO1= 0.0
          IF( KO .GT. 0 ) VKO1= VALLN1(KO)
          STMAS(ISTMAS) = VNA - VKO
          STMAS(ISTMAS+1) = VNA1 - VKO1
17     CONTINUE
18     CONTINUE
C----------------------------------------------------------------------
C-->  URMAS <--> MSW(14 + 3*NTOKOB + 2*NNAPR)
C----------------------------------------------------------------------
       INPSUB = 13 + 3*NTOKOB + 2*NNAPR
       NPSUB = MSW(INPSUB)
       NEL = MSW(1)
C----------------------------------------------------------------------
C--> PAC—ˆT›BAEM Cˆ“
C----------------------------------------------------------------------
       CALL CALLPS( NPSUB, NEL, RMSW(INPSUB+1), STMAS, DGMAS, INTMAS )
C----------------------------------------------------------------------
C -->  ‡AˆCœ B TOKN1
C----------------------------------------------------------------------
       ITOK = MSW(7)
          DO 25 I= 1 , 12
          TOKN1( ITOK + I - 1 ) = DGMAS( I )
25     CONTINUE
          TOKN1( ITOK + 12 ) = RMSW( INPSUB + 8 )  
C----------------------------------------------------------------------
C-->  ‡A”ˆKCˆP. KOHE– B›—ˆC‹EHˆ‰       ( FQ ”‹Aƒ )
       MSW(10) = 1
       RETURN
       END
