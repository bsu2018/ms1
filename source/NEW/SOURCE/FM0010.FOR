       SUBROUTINE FM0010(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: FM0010(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:   HACTPO‰KA MAKPOMO„E‹ˆ TˆA # 40
C   ----------   PEƒ“‹ŸTOP BO‡“†„EHˆŸ 3X3X0 ‹. MA˜ˆH›;
C                ***************************************
C                .TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0014, LM0001 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C                      ( „‹ˆHA :==> +1(VAR) )
C
C
C              ‹EMEHT OˆC›BAETCŸ:
C
C       # T 40 “ KA KB KC
C           T_BK‹ U_OTO‹. #‹-TA_‹._MA˜ˆH›_3X3X0
C              PSUB #P < .........>;
C
C              —EPE‡ AAPAT PSUB ‡A„AžTCŸ ‡AKOH› “PA›‹EHˆŸ "H—E";
C
C      ƒ„E
C              KA, KB, KC - “‡‹› –Eˆ, PˆHŸT›E ‡A OOPH“ž CˆCTEM“
C                           HAPŸ†EHˆŸ ( CETœ ECKOHE—HO‰ MO™HOCTˆ );
C              T_BK‹ - BPEMŸ, HA—ˆHAŸ C KOTOPOƒO BK‹. PEƒ. BO‡“„.
C              U_OT. - OTO‹O—HOE HAPŸ†EHˆE HA B›XO„E PEƒ“‹—TOPA
C                      ( OƒPAHˆ—ˆBAžTCŸ "CˆH“CO‰„›" B›X. HAPŸ†EHˆŸ );
C              #‹. MA˜. - HOMEP ‹EMEHTA, OˆC›BAž™ˆ‰ ‹. MA˜ˆH“,
C                          KOTOP“ž PEƒ“‹ˆP“EM;
C
C      B›BO„H›E BE‹ˆ—ˆH› B MACCˆBE TOKN/TOKN1:
C
C      U_R_“_A, U_R_“_B, U_R_“_C,  -->  HAPŸ†EHˆŸ “PAB‹EHˆŸ
C                                          B CˆCTEME OCE‰ POTOPA;
C     OMEƒA_OPOHAŸ, S_CKO‹œ†EHˆE,
C
C    U_Q_“P.  U_D_“P;
C
C   I_S_D_‡A„,   S_CKO‹œ†EHˆE_‡A„,  INT(ISD-ISD_ZAD),  INT(S-S_ZAD),
C    (#16)       (#17)               (#18)               (#19)
C    PE‡EPB;
C    (#20)                                 -->   BCEƒO 20 ‡HA—EHˆ‰;
C
C*****  „AHHAŸ MO„E‹œ OC‹“†ˆBAET MAKPOMO„E‹ˆ C Tˆ.# 11, 110, 12, 120;
C
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ
C   -----
C                      „‹Ÿ PSUB2 --> "“„BAˆBAET" BBE„EHH›E O‹œ‡OBATE‹EM
C                                     ‡HA—EHˆŸ „‹Ÿ KTI ˆ KTS;
C
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW Pˆ FGMSW = 0;
C                  LM - „‹ˆHA BCEƒO MSW OˆCAHˆŸ      Pˆ FGMSW = 1;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ B MˆHˆMA‹œHOM O"EME;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. KO‹-BO APAMETPOB B / PSUB HE KOHTPO‹ˆP.
C                     HET HA TO BPEMEHˆ ( MOEƒO !!!)
C
C                4.„‹Ÿ OˆCAHˆŸ ‡AKOHOB “PAB‹EHˆŸ PEA‹ˆ‡OBAHA PSUB1-->
C
C
C      1) .........„‹Ÿ OˆCAHˆŸ ‡AKOHOB “PAB‹EHˆŸ PEA‹ˆ‡OBAHA PSUB1-->
C
C      PSUB 1 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD XAF  >;
C              ( 8-Mœ APAMETPOB )
C
C
C      2) .........„‹Ÿ OˆCAHˆŸ ‡AKOHOB “PAB‹EHˆŸ PEA‹ˆ‡OBAHA PSUB2-->
C
C      PSUB 2 < KE URD00 KI ISDZAD URQ00 KS BETA SZAD   XAF
C
C      KTI  TI1 ISZAD_1  TI2 ISZAD_2 .....
C                        I         I
C                        I<---*--->I
C
C      KTS  TS1 S_1      TS2    S_2  ...... >;
C                        I         I
C                        I         I
C                        I<---*--->I
C
C
C    ƒ„E:
C          -*-  „BA ‡HA—EHˆŸ, OˆC›BAž™ˆE O„H“ TO—K“ OPE„E‹EHˆŸ
C               COOTBETCTB“ž™E‰ ”“HK–ˆOHA‹œHO‰ ‡ABˆCˆMOCTˆ -->
C               OHA ‡A„AETCŸ KAK K“CO—HO-‹ˆHE‰HAŸ ”“HK–ˆŸ;
C
C       KTI  - KO‹-BO TO—EK „‹Ÿ OPE„E‹EHˆŸ ‡ABˆCˆMOCTˆ ISZAD OT "TN";
C
C       KTS  - KO‹-BO TO—EK „‹Ÿ OPE„E‹EHˆŸ ‡ABˆCˆMOCTˆ SZAD  OT "TN";
C
C
C  PSUB1 --->  B›—ˆC‹ŸET:
C  *****
C     UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* ( KE+1 ) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]* ( KE+1 ) - KE* XAF*IRQ0;
C
C  ƒ„E:  INT - ˆHTEƒPA‹;
C
C
C  PSUB2 --->  B›—ˆC‹ŸET:
C  *****
C     UYD0 = [ URD00 - KI*INT(ISD0-ISDZAD) ]* ( KE+1 ) - KE* XAF* IRD0;
C
C
C  UYQ0 = [URQ00 - KS *S - BETA*INT(S-SZAD) ]* ( KE+1 ) - KE* XAF*IRQ0;
C
C  ƒ„E:  INT - ˆHTEƒPA‹, A ISDZAD ˆ SZAD - K“CO—HO-‹ˆHE‰HAŸ ”“HK–ˆŸ
C                                                     B P E M E H ˆ
C                                                     *************
C
C                  5. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C              ”OPMAT RMSW Tˆ # 40:
C              ---------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I       OO‡HA—EHˆE - ‡HA—EHˆE
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I
C OˆCAHˆŸ   I    FM0010    I      I
C            I              I      I
C------------I--------------I------I-----------------------------------
C     9      I    -2        I      I HOMEP “‡‹A KA ( ”A‡A A )
C    10      I    -1        I      I HOMEP “‡‹A KB ( ”A‡A B )
C    11      I     0        I      I HOMEP “‡‹A KC ( ”A‡A C )
C----------------------------------------------------------------------
C    12      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4
C    13      I     2        I  FM  I  NELT - HOMEP ‹. PEƒ“‹ˆP. ‹. MA˜.
C    14      I     3        I  LM  I  AMOT - A„P. B MSWALL OˆCAHˆŸ
C            I              I      I         PEƒ. ‹. MA˜ˆH›
C    15      I     4        I  LM  I  APOT - A„PEC B MSWALL “‡‹OB MA˜ˆH›
C    16      I     5        I  LM  I  ATOKM - A„P B TOKN/N1 BE‹ˆ—ˆH,
C            I              I      I          PAC—ˆT. „‹Ÿ ‹. MA˜.
C    17      I     6        I  FM  I  TBKL - BPEMŸ, HA—ˆHAŸ C KOTOPOƒO
C            I              I      I         BK‹ž—AETCŸ PEƒ“‹ŸTOP;
C    18,19   I     7,8      I      I  PE‡EPB.
C    20      I     9        I  FM  I  UPOT - OTO‹O—HOE HAPŸ†EHˆE
C    21      I     10       I  FM  I  NSUB - HOMEP / O‹œ‡OBATE‹Ÿ
C22,23,24,25 I11,12,13,14,15I      I  PA. MECTO „‹Ÿ ˆHT. 1 ˆ 2 B TOKN;
C  27        I    16        I  FM  I  NPAR - KO‹-BO APAMETPOB,
C            I              I      I         BBE„EHH›X B PSUB<..>
C OT 28 „O   I OT 17 „O     I FM   I  APAMETP› PSUB, ‡A„AHH›E
C 28 + NPAR  I 17 + NPAR    I      I  O‹œ‡OBATE‹EM
C            I 26 -->       I FM   I  KTI „‹Ÿ PSUB2 ....
C            I 27+2*KTI --> I FM   I  KTS „‹Ÿ PSUB2 ....
C            I              I      I
C            I              I      I
C            I              I      I
C----------------------------------------------------------------------
C
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 28+NPAR
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 17+NPAR
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RNELT, NELT )
       EQUIVALENCE ( RNPSUB,  NPSUB )
       EQUIVALENCE ( RNPAR,   NPAR  )
       INTEGER NETL, NPSUB, NPAR, TIP, BA0T, FGMSW, AKTI, AKTS
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 40 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0010 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C-->  “TO—HˆTœ LM <O™“ž >
C  PAR0T -> TBKL, UPOT, #‹. -NSUB, NPAR, ..APAMETP›..... KTI     KTS
C            1     2     3     4     5     6....           15 .....
       LM = 28 + IFIX( PAR0T(5) )
       RETURN
C######################################################################
1000   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       NPAR = IFIX( PAR0T(5) )
       LM = 17 + NPAR
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C--->
       RMSW(16) = FLOAT( NPAR )
C-->   !!!!!!!  = RNPAR
       NELT = IFIX( PAR0T(3) )
       RMSW(2) = RNELT
C-->
       UPOT = PAR0T(2)
       RMSW(9) = UPOT
C-->
       TBKL = PAR0T(1)
       RMSW(6) = TBKL
C-->
       NPSUB =  - IFIX( PAR0T(4) )
       RMSW(10) = RNPSUB
C-->  APAMETP› PSUB--->
       DO 3 I=1, NPAR
3      RMSW(17-1 +I) = PAR0T( 6-1 + I)
C
C
C######################################################################
       IF ( NPSUB .EQ. 1 ) GO TO 20
       IF ( NPSUB .EQ. 2 ) GO TO 10
C!!!-------------------------------------------------------------------
       WRITE(NWTR,  502 ) NEL, TIP, NPSUB
       WRITE(NWTRE, 502 ) NEL, TIP, NPSUB
502    FORMAT(/' *** „‹Ÿ ‹EMEHTA ', I5, ' ( Tˆ ', I3, ' ) PSUB ', I2,
     =   ' HE PEA‹ˆ‡OBAHA ***'/ )
       STOP 667
C!!!-------------------------------------------------------------------
10     CONTINUE
C-->  “„BOˆM KTI  ˆ KTS -->
       IF( NPAR .LE. 9 ) GO TO 20
       KTI = IFIX( PAR0T(15) )
       KTI2 = 2*KTI
       RMSW(26) = FLOAT( KTI2 )
C-->  ƒ„E †ˆBET KTS....
       KTS = IFIX( PAR0T( 16 + 2*KTI ) )
       KTS2 = KTS * 2
       RMSW(27 + 2*KTI ) = FLOAT( KTS2 )
C######################################################################
20     CONTINUE
       NPAR17 = NPAR + 17 - 1
       WRITE(NWTR,300) NEL, TIP, NPSUB, NPAR, (RMSW(I), I=17, NPAR17)
300    FORMAT(/' *** ‹EMEHT ', I5, '   Tˆ ', I3, '   PSUB ', I2,
     =  22X,   '     ***'/
     = ' *** ‡A„AH› APAMETP› B KO‹ˆ—ECTBE ', I3, ' ˜T. :', 21X,'***'/
     = ' ***      KE=', G12.5, ' URD00=', G12.5, ' KI=', G12.5,
     = '     ***'/
     = ' *** ISD_ZAD=', G12.5, ' URQ00=', G12.5, ' KS=', G12.5,
     = '     ***'/
     =' *** BETA=',G12.5,'  S_ZAD=',G12.5,' XAF=',G12.5, 6X,'***'/
     = ' ***', 20X,'„OO‹HˆTE‹œHO ‡A„AHO:', 20X, '***'/
     =     5(1X, G12.5, 1X) )
       WRITE(NWTR, 301 )
301    FORMAT(/)
C----------------------------------------------------------------------
       RETURN
       END
