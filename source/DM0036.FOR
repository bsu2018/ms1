         SUBROUTINE DM0036 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT   MAŸ   1988 ƒ.)------------
C   O„POƒPAMMA:  DM0036 ( MSW, RMSW, YALL, JALL, LOGYTR,
C   -------------            TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C   HA‡HA—EHˆE:
C   -----------
C   HA‡HA—EHˆE:  MOHˆTOP   MAKPOMO„E‹ˆ MˆKPOPO–ECCOPHO‰ CˆCTEM›,
C   ----------   OCTPOEHHO‰ HA A‡E MˆKPOPO–ECCOPA CEPˆˆ
C                K1801BM1(2,3), ˆMEž™EƒO CˆCTEM“ KOMAH„ ”ˆPM› "DEC".
C                ******************************************************
C                Pˆ‡HAK TIP # 5 -> ‹Oƒ. MAKPOMO„E‹œ.
C
C
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
C   ------------   2. SEQSOS, ADEC**
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
C
C                   ”OPMAT RMSW Tˆ # 380:
C                  -----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0032    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I TX - BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ;
C    11      I     2        I  DM  I KX - “KA‡ATE‹œ ˆ‡MEHEHˆŸ COCTOŸHˆŸ
C            I              I      I      OPTA  B/B HA ˜AƒE ;
C    12      I     3        I  FM  I TTAKT - EPˆO„ TAKTOBO‰ —ACTOT› ;
C    13      I     4        I  FM  I P - ”‹Aƒ OT‹A„O—HO‰ E—ATˆ:
C            I              I      I     P=0 - OT‹A„O—HAŸ E—ATœ OTK‹.
C            I              I      I     P=1 - OT‹A„O—HAŸ E—ATœ BK‹.
C    14      I     5        I  FM  I           PE‡EPB
C    15      I     6        I  FM  I           PE‡EPB
C    16      I     7        I  DM  I KX - “KA‡ATE‹œ ˆ‡MEHEHˆŸ COCTOŸHˆŸ
C            I              I      I      OPTA B/B HA ˜AƒE ;
C    17      I     8        I  DM  I TT - TEK“™EE BPEMŸ ;
C    18      I     9        I  DM  I R0 - CO„EP†ˆMOE POH R0 Pˆ T=TT ;
C    19      I    10        I  DM  I R1 - CO„EP†ˆMOE POH R1 Pˆ T=TT ;
C    20      I    11        I  DM  I R2 - CO„EP†ˆMOE POH R2 Pˆ T=TT ;
C    21      I    12        I  DM  I R3 - CO„EP†ˆMOE POH R3 Pˆ T=TT ;
C    22      I    13        I  DM  I R4 - CO„EP†ˆMOE POH R4 Pˆ T=TT ;
C    23      I    14        I  DM  I R5 - CO„EP†ˆMOE POH R5 Pˆ T=TT ;
C    24      I    15        I  DM  I R6 - CO„EP†ˆMOE POH R6 Pˆ T=TT ;
C    25      I    16        I  DM  I R7 - CO„EP†ˆMOE POH R7 Pˆ T=TT ;
C    26      I    17        I  DM  I FLC-‡HA—EHˆE ”‹AƒA "C" CC Pˆ T=TT
C    27      I    18        I  DM  I FLV-‡HA—EHˆE ”‹AƒA "V" CC Pˆ T=TT
C    28      I    19        I  DM  I FLZ-‡HA—EHˆE ”‹AƒA "Z" CC Pˆ T=TT
C    29      I    20        I  DM  I FLN-‡HA—EHˆE ”‹AƒA "N" CC Pˆ T=TT
C    30      I    21        I  DM  I FLT-‡HA—EHˆE ”‹AƒA "T" CC Pˆ T=TT
C    31      I    22        I  DM  I FLIO-‡HA—EHˆE ”‹AƒA "I/O" CC
C            I              I      I             Pˆ T=TT ;
C    32      I    23        I  DM  I IPORT  -CO„EP†ˆMOE OPTA B›BO„A
C    33      I    24        I  DM  I IPORT1 -CO„EP†ˆMOE OPTA BBO„A
C            I              I      I             Pˆ T=TT ;
C    34      I    25        I  FM  I ADRPROG-MAKCˆMA‹œH›‰ A„PEC O‡“ ;
C    35      I    26        I  FM  I ADRPRTO-A„PEC OPTA B›BO„A;
C    36      I    27        I  FM  I ADRPRTI-A„PEC OPTA BBO„A ;
C    37      I    28        I  DM  I TO - CTAPOE BPEMŸ ;
C    38      I    29        I  DM  I R0 - CO„EP†ˆMOE POH R0 Pˆ T=TO ;
C    39      I    30        I  DM  I R1 - CO„EP†ˆMOE POH R1 Pˆ T=TO ;
C    40      I    31        I  DM  I R2 - CO„EP†ˆMOE POH R2 Pˆ T=TO ;
C    41      I    32        I  DM  I R3 - CO„EP†ˆMOE POH R3 Pˆ T=TO ;
C    42      I    33        I  DM  I R4 - CO„EP†ˆMOE POH R4 Pˆ T=TO ;
C    43      I    34        I  DM  I R5 - CO„EP†ˆMOE POH R5 Pˆ T=TO ;
C    44      I    35        I  DM  I R6 - CO„EP†ˆMOE POH R6 Pˆ T=TO ;
C    45      I    36        I  DM  I R7 - CO„EP†ˆMOE POH R7 Pˆ T=TO ;
C    46      I    37        I  DM  I FLC-‡HA—EHˆE ”‹AƒA "C" CC Pˆ T=TO
C    47      I    38        I  DM  I FLV-‡HA—EHˆE ”‹AƒA "V" CC Pˆ T=TO
C    48      I    39        I  DM  I FLZ-‡HA—EHˆE ”‹AƒA "Z" CC Pˆ T=TO
C    49      I    40        I  DM  I FLN-‡HA—EHˆE ”‹AƒA "N" CC Pˆ T=TO
C    50      I    41        I  DM  I FLT-‡HA—EHˆE ”‹AƒA "T" CC Pˆ T=TO
C    51      I    42        I  DM  I FLIO-‡HA—EHˆE ”‹AƒA "I/O" CC
C            I              I      I             Pˆ T=TO ;
C    52      I    43        I  DM  I IPORT  -CO„EP†ˆMOE OPTA B›BO„A
C    53      I    44        I  DM  I IPORT1 -CO„EP†ˆMOE OPTA BBO„A
C            I              I      I             Pˆ T=TO ;
C    54      I    45        I  FM  I  #‹ - HOMEP ‹EMEHTA;
C    55      I    46        I  FM  I  #CM - HOMEP TOKA ‹EMEHTA #‹ ;
C    56      I    47        I  LM  I  ATOKM - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEPA #CM ;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 56
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 47
C----------------------------------------------------------------------
C
C
C
C        OPƒAHˆ‡A–ˆŸ AMŸTˆ B PEA‹œHO‰ MˆKPOPO–ECCOPHO‰ CˆCTEME
C        ˆ B MO„E‹ˆP“ž™E‰ EE POƒPAMME (MACCˆB› MEM ˆ MEM1)
C        -------------------------------------------------------
C
C        OPƒAHˆ‡A–ˆŸ AMŸTˆ B PEA‹œHO‰ MˆKPOPO–ECCOPHO‰ CˆCTEME
C
C
C               C‹OBO
C           A‰T      A‰T                      A‰T
C        *---------+---------*               *---------*
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000000        ! M‹A„˜ˆ‰ ! 000000
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000002        ! CTAP˜ˆ‰ ! 000001
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000004        ! M‹A„˜ˆ‰ ! 000002
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000006        ! CTAP˜ˆ‰ ! 000003
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000010        ! M‹A„˜ˆ‰ ! 000004
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000012        ! CTAP˜ˆ‰ ! 000005
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000014        ! M‹A„˜ˆ‰ ! 000006
C        ! ................. ! ......        ! CTAP˜ˆ‰ ! 000007
C        ! ................. ! ......        ! ....... ! ......
C        ! ................. ! ......        ! ....... ! ......
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 003774        ! M‹A„˜ˆ‰ ! 003776
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 003776        ! M‹A„˜ˆ‰ ! 003777
C        *---------+---------*               *---------*
C
C
C        OPƒAHˆ‡A–ˆŸ AMŸTˆ B MO„E‹ˆP“ž™E‰ POƒPAMME
C            (MACCˆB›   MEM  ˆ  MEM1 )
C     *-------------------------------------*
C     ! C‹OBO C A„PECOM 000000              ! 0001
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000000 ! 0002
C     ! C‹OBO C A„PECOM 000002              ! 0003
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000002 ! 0004
C     ! C‹OBO C A„PECOM 000004              ! 0005
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000004 ! 0006
C     ! C‹OBO C A„PECOM 000006              ! 0007
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000006 ! 0008
C     ! C‹OBO C A„PECOM 000010              ! 0009
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000010 ! 0010
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! C‹OBO C A„PECOM 003776              ! 2047
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 003776 ! 2048
C     *-------------------------------------*
C
C
C----------------------------------------------------------------------
C
C
C     B›BO„ˆM›E ‡HA—EHˆŸ:
C
C     I #.1 - I #.8  -  POH MˆKPOPO–ECCOPA ( B „ECŸTˆ—HOM Bˆ„E ) ;
C     I #.9          -  CC MˆKPOPO–ECCOPA ( B „ECŸTˆ—HOM Bˆ„E ) ;
C     I #.10         -  CO„EP†ˆMOE OPTA B›BO„A( B „ECŸTˆ—HOM Bˆ„E ) ;
C     I #.11         -  BPEMŸ, „‹Ÿ KOTOPOƒO XPAHŸTCŸ “KA‡AHH›E ‡HA—EHˆŸ;
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         COMMON /MEMER/ MEM(2048), MEM1(2048)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         DIMENSION MSWALL(*), RMSWAL(*)
         INTEGER ADPORT, ADPROG
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
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
         EQUIVALENCE ( RN, N )
         INTEGER FKOM, ZAPROS, TIP
         NOUT = 10
C--->  KOHTPO‹œ TˆA
         TIP = MSW(2)
         IF ( TIP .EQ. 380 ) GO TO 11
C######################################################################
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), TIP
9998     FORMAT( ' *** ‹Oƒ. CO‰ B DM0036 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
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
C---> CPOC BPEMEHˆ  TX - BO‡MO†HOƒO EPEK‹ž—EHˆŸ ...
         MSW(11) = 0
         KOD = 0
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---- Z A P R O S = 3 ---- TOKN1  ˆ  VALLN1  OPE„E‹EH› ---------------
600      CONTINUE
C---> ‡AOMHˆM CTAP“ž TO—K“.
         RMSW(37) = RMSW(17)
         DO 602 I=1,16
         MSW(37+I) = MSW(17+I)
602      CONTINUE
         IADPRG = MSW(34)
         DO 605 I=1,IADPRG
         MEM1(I) = MEM(I)
605      CONTINUE
C-->     —TO B OPT“ BBO„A ?...
         MSW(33) = IFIX( TOKN1(MSW(56)))
C--> PO„O‹†ˆM B›O‹HŸTœ “PAB‹Ÿž™“ž POƒPAMM“.
         TN1 = TN + H0
CCCC     WRITE (6,*) MSW(33)
C400     FORMAT (8X,E15.6)
         CALL ADEC00( RMSW(17), TN1, MSW(13), NOUT, MSW(11), MSW(18),
     *   MSW(26), MSW(32), MSW(33),MSW(35),MSW(36), MSW(34) , IER )
         IF( IER .NE. 0 ) GO TO 9999
         RMSW(10) = RMSW(17)
         DO 422 I=1,8
         TOKN1(ITOK+I-1) =  MSW(17+I)*1.
422      CONTINUE
         ICCP = MSW(31)*128 + MSW(30)*16 + MSW(29)*8 +
     +          MSW(28)*4   + MSW(27)*2  + MSW(26)
         TOKN1(ITOK+8) =  ICCP*1.
         TOKN1(ITOK+9) = MSW(32)*1.
         TOKN1(ITOK+10)= RMSW(10)
CCCCC    WRITE (6,430) (TOKN1(ITOK+I-1),I=1,11)
C430     FORMAT (5X,F12.8)
         IF( MSW(11) .NE. 1 ) RETURN
         FKOM = 1
         HX = RMSW(10) - TN
         HXG = MIN( HX,HXG )
         TXG = TN + HXG
         RETURN
C######################################################################
500      CONTINUE
C--- Z A P R O S = 4  --- ”ˆKCA–ˆŸ CMEH› COCTOŸHˆŸ B›XO„HOƒO CˆƒHA‹A.
C----------------------------------------------------------------------
         KOD = 0
         MSW(33) = IFIX(TOKN1(MSW(56)))
         IF( MSW(11) .EQ. 0 ) GO TO 520
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C---> EC‹ˆ TXG  "PˆMEPHO" PABHO  TX MO„E‹ˆ, TO CMEHA H“†HA !
         TX = RMSW(10)
         EPST = ABS( TX - TXG )
         IF( EPST .GT. EPSKOM ) GO TO 510
C---> ˆHA—E CMEHˆM COCTOŸHˆE...
         GO TO 520
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
510      CONTINUE
C---> B CˆCTEME POˆ‡O˜‹A KOMM“TA–ˆŸ PAHœ˜E, —EM “ HAC. BEPHEMCŸ !!!
         RMSW(17) = RMSW(37)
         DO 515 I=1,16
         MSW(17+I) = MSW(37+I)
515      CONTINUE
         IADPRG = MSW(34)
         DO 516 I=1,IADPRG
         MEM(I) = MEM1(I)
516      CONTINUE
C-->  B›O‹HˆM “PAB‹Ÿž™“ž POƒPAMM“ „O MOMEHTA BPEMEHˆ  T=TXG
         CALL ADEC00( RMSW(17), TXG, MSW(13), NOUT, MSW(11), MSW(18),
     *   MSW(26), MSW(32), MSW(33),MSW(35),MSW(36), MSW(34) , IER )
         IF( IER .NE. 0 ) GO TO 9999
         RMSW(10) = RMSW(17)
C---> B›BE„EM TEK“™EE COCTOŸHˆE M-CˆCTEM›
520      CONTINUE
         ITOK = MSW(7)
         DO 522 I=1,8
         TOKN1(ITOK+I-1) =  MSW(17+I)*1.
522      CONTINUE
         ICCP = MSW(31)*128 + MSW(30)*16 + MSW(29)*8 +
     +          MSW(28)*4   + MSW(27)*2  + MSW(26)
         TOKN1(ITOK+8) =  ICCP*1.
         TOKN1(ITOK+9) = MSW(32)*1.
         TOKN1(ITOK+10)= RMSW(10)
C****    WRITE (6,530) (TOKN1(ITOK+I-1),I=1,11)
C530     FORMAT (5X,F12.8)
         RETURN
C######################################################################
C-->  E—ATœ COO™EHˆ‰ O O˜ˆKAX.
9999     CONTINUE
         NWTRE = MASPRT(14)
         GO TO ( 9991, 9992, 9993, 9994, 9995, 9996 ),IER
         RETURN
C----------------------------------------------------------------------
9991     CONTINUE
         WRITE(NWTRE,99991) MSW(1), TIP
99991    FORMAT( ' *** ‹Oƒˆ—ECKˆ‰ CO‰ B “PAB‹Ÿž™E‰ POƒPAMME „‹Ÿ ',
     =   'MˆKPOPO–ECCOPHO‰ CˆCTEM› '/
     =   ' *** O˜ˆO—HAŸ ‡AˆCœ KOMAH„›,‹ˆO B›XO„ ‡A PE„E‹› O‡“ ***'/
     =   ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9992     CONTINUE
         WRITE(NWTRE,99992) MSW(1), TIP
99992    FORMAT( ' *** ‹Oƒˆ—ECKˆ‰ CO‰ B “PAB‹Ÿž™E‰ POƒPAMME „‹Ÿ ',
     =   'MˆKPOPO–ECCOPHO‰ CˆCTEM› '/
     =   ' *** OPA™EHˆE K Ÿ—E‰KE AMŸTˆ,ˆMEž™E‰ HE—ETH›‰ A„PECC  ***'/
     =   ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9993     CONTINUE
         WRITE(NWTRE,99993) MSW(1), TIP
99993    FORMAT( ' *** ‹Oƒˆ—ECKˆ‰ CO‰ B “PAB‹Ÿž™E‰ POƒPAMME „‹Ÿ ',
     =   'MˆKPOPO–ECCOPHO‰ CˆCTEM› '/
     =   ' *** O˜ˆO—HAŸ ‡AˆCœ KOMAH„›                           ***'/
     =   ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9994     CONTINUE
         WRITE(NWTRE,99994) MSW(1), TIP
99994    FORMAT( ' *** ‹Oƒˆ—ECKˆ‰ CO‰ B “PAB‹Ÿž™E‰ POƒPAMME „‹Ÿ ',
     =   'MˆKPOPO–ECCOPHO‰ CˆCTEM› '/
     =   ' *** ‡APOC HA BBO„ „AHH›X ˆ‡ OPTA BBO„A-B›BO„A        ***'/
     =   ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9995     CONTINUE
         WRITE(NWTRE,99995) MSW(1), TIP
99995    FORMAT( ' *** ‹Oƒˆ—ECKˆ‰ CO‰ B “PAB‹Ÿž™E‰ POƒPAMME „‹Ÿ ',
     =   'MˆKPOPO–ECCOPHO‰ CˆCTEM› '/
     =   ' *** „AHHAŸ KOMAH„A OKA HE PEA‹ˆ‡OBAHA                 ***'/
     =   ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
9996     CONTINUE
         WRITE(NWTRE,99996) MSW(1), TIP
99996    FORMAT( ' *** ‹Oƒˆ—ECKˆ‰ CO‰ B “PAB‹Ÿž™E‰ POƒPAMME „‹Ÿ ',
     =   'MˆKPOPO–ECCOPHO‰ CˆCTEM› '/
     =   ' *** KO„ KOMAH„› OTC“TCTB“ET B EPE—HE KOMAH„           ***'/
     =   ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(1)
         RETURN
C----------------------------------------------------------------------
         END
