       SUBROUTINE LM0007( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT   MAü   1987 É.)------------
C   èOÑèPOÉPAMMA: LM0007( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HAáHAóEHàE:   ãOÉàóECKAü HACTPOâKA MSW MAKPOMOÑEãEâ TàèOB
C   ---------- 310,311,312,313,314,315,316,317,320,321,322:
C     +-----+-----------------------------------+--------------------I
C     I     I                                   I     èEPEÑATOóHAü   I
C     I Tàè I       HAàMEHOBAHàE áBEHA          I                    I
C     I     I                                   I       îìHKñàü      I
C     I-----I-----------------------------------I--------------------I
C     I     I  èPOèOPñàOHAãúHO-CìMMàPìûôEE      I                    I
C     I 310 I       ( è - PEÉìãüTOP )           I          K         I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I         àHTEÉPàPìûôEE             I                    I
C     I 311 I       ( à - PEÉìãüTOP )           I        1/(TS)      I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I                                   I                    I
C     I 312 I         àHEPñàOHHOE               I        1/(TS+1)    I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I         ÑàîîEPEHñàPìûôEE          I                    I
C     I 313 I       ( Ñ - PEÉìãüTOP )           I      TS/(TS+1)     I
C     I     I                                   I                    I
C     I-----I-----------------------------------I--------------------I
C     I     I   èPOèOPñàOHAãúHO-àHTEÉPàPìûôEE   I    TAU1*S + 1      I
C     I 314 I         ( èà - PEÉìãüTOP )        I   -------------    I
C     I     I                                   I      TAU2*S        I
C     I-----I-----------------------------------I--------------------I
C     I     I èPOèOPñàOHAãúHO-ÑàîîEPEHñàPìûôEE  I    TAU1*S + 1      I
C     I 315 I         ( èÑ - PEÉìãüTOP )        I   -------------    I
C     I     I                                   I    TAU2*S + 1      I
C     I-----I-----------------------------------I--------------------I
C     I     I                                   I    1 - TAU*S       I
C     I 316 I       îAáOBPAôATEãú               I   -------------    I
C     I     I                                   I    1 + TAU*S       I
C     I-----I-----------------------------------I--------------------I
C     I     I                                   I       W**2         I
C     I 317 I       KOããEÅATEãúHOE              I  ----------------- I
C     I     I                                   I  S**2+2*E*W*S+W**2 I
C     +-----+-----------------------------------+--------------------I
C     I     I                                   I          I Y    *  I
C     I     I                                   I          I     *   I
C     I     I        HEãàHEâHOE áBEHO           I          I*  *     I
C     I 320 I  (èEPEÑATOóHAü îìHKñàü EÉO        I   ------*+-------> I
C     I     I  áAÑAHHO HAÅOPOM TOóEK)           I      *   I       X I
C     I     I                                   I *  *     I         I
C     I     I                                   I          I         I
C     +-----+-----------------------------------+--------------------+
C     I     I                                   I         !Y         I
C     I     I                                   I   !--<--*----!-----I
C     I     I                                   I   !     !ì2  !     I
C     I     I                                   I   !     !    !    XI
C     I 321 I       PEãEâHOE áBEHO              I---*-----I----*-----I
C     I     I                                   I   !X1   !    !X2   I
C     I     I                                   I   !     !    !     I
C     I     I                                   I---!-----*-->-!     I
C     I     I                                   I         !ì1        I
C     +-----+-----------------------------------+--------------------+
C     I     I                                   I !X        *   *    I
C     I     I                                   I !    *             I
C     I     I                                   I !*              T  I
C     I     I                                   I *----------------- I
C     I 322 I  áBEHO "óàCTOE áAèAáÑõBAHàE"      I !ì                 I
C     I     I                                   I !             *   *I
C     I     I                                   I !TAU     *         I
C     I     I                                   I !<->!*          T  I
C     I     I                                   I *---*------------- I
C     +-----+-----------------------------------+--------------------+
C
C
C   METOÑ:        èOàCK B MSW_ALL OèàCAHàà ùãEMEHTOB , AÑPECOB áAÑAHHõX
C   -----         TOKOB ùãEMEHTOB B MACCàAX TOKN/TOKN1.
C                 B CãìóAE HAXOÜÑEHàü -> ÑOîOPMàPOBAHàE MSW_OèàCAHàü;
C                 B ùTOM CãìóAE KOD = 0, àHAóE = 1;
C
C   èAPAMETPõ:    MSW(RMSW) - "OTPEáOK" MSW_MOÑEãà Tàè # 301;
C   ----------    ADR - AÑPEC B MSW_ALL MSW_OèàCAHàü ùTOâ MOÑEãà;
C                 MSWALL(RMSWAL) - BECú CèàCOK MSW CãOB;
C                 KOD - = 0 - CîOPMàPOBAãà MSW,
C                       = 1 - ãOÉ. OòàÅKà;
C                      ( HA NWTR COOÅôEHàE BõBOÑàTCü );
C   èOÑèPOÉPAMMõ: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C              îOPMAT RMSW Tàè # 310:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # CãOBA   I    # CãOBA B I KTO  I
C OT HAóAãA  I    OTPEáKE   IOèPEÑ.I
C BCEÉO MSW  I    RMSW B    I      I       OÅOáHAóEHàE - áHAóEHàE
C OèàCAHàü   I    FM0020    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ìáãA ( BCEÉÑA 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - îãAÉ HOBõE áHAó. BõóàCãEHõ
C            I              I      I   ( Ñãü CàHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NBXOD-KOãàóECTBO TOKOB, àCèOãú-
C            I              I      I        áìEMõX KAK BXOÑHõE CàÉHAãõ;
C    12      I     3        I  FM  I  #ùã1 - HOMEP ùãEMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ùãEMEHTA #ùã1;
C    14      I     5        I  FM  I  K1   - KOùîîàñàEHT èPOèOPñàOHAãú-
C            I              I      I         HOCTà TOKA #CM1 ùã-TA #ùã1;
C    15      I     6        I  LM  I  ATOKM1 - AÑP B TOKN/N1 áHAóEHàü
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ùãEMEHTA #ùã1;
C  8+I*4     I  -1+I*4      I  FM  I  #ùãI -      -"-
C  9+I*4     I     I*4      I  FM  I  #CMI -      -"-
C 10+I*4     I   1+I*4      I  FM  I  KI   -      -"-
C 11+I*4     I   2+I*4      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NBXOD*4  I  3+NBXOD*4   I  FM  I  NGARM -KOãàóECTBO ÉAPMOHàK, àCè.
C            I              I      I         Ñãü OèàCAHàü áAÑ. CàÉHAãA;
C 13+NBXOD*4 I  4+NBXOD*4   I  FM  I  W  -   óACTOTA OCH. ÉAPMOHàKà(Éñ);
C 14+NBXOD*4 I  5+NBXOD*4   I  FM  I  A0  -  AMèãàTìÑA èOCT. COCTABãü-
C            I              I      I         ûôEâ áAÑ. CàÉHAãA;
C 15+NBXOD*4 I  6+NBXOD*4   I  FM  I  A1  -  AMèãàTìÑA 1-Oâ ÉAPMOHàKà
C            I              I      I         áAÑAHHOÉO CàÉHAãA;
C 16+NBXOD*4 I  7+NBXOD*4   I  FM  I  FI1 -  HAó. îAáA 1-Oâ ÉAPMOHàKà;
C 17+NBXOD*4 I  8+NBXOD*4   I  FM  I  A2  -  AMèãàTìÑA 2-Oâ ÉAPMOHàKà;
C 18+NBXOD*4 I  9+NBXOD*4   I  FM  I  FI2 -  HAó. îAáA 2-Oâ ÉAPMOHàKà;
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C
C       îOPMAT èEPBõX èüTà CãOB RMSW, OÑàHAKOBõâ Ñãü  TàèOB:
C       Tàè # 311,312,313,314,315,316,317,320,321,322,330,331,332:
C+-----------I--------------I------I-----------------------------------
C  # CãOBA   I    # CãOBA B I KTO  I
C OT HAóAãA  I    OTPEáKE   IOèPEÑ.I
C BCEÉO MSW  I    RMSW B    I      I       OÅOáHAóEHàE - áHAóEHàE
C OèàCAHàü   I    FM0020/21 I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ìáãA ( BCEÉÑA 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - îãAÉ HOBõE áHAó. BõóàCãEHõ
C            I              I      I   ( Ñãü CàHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I         PEáEPB
C            I              I      I
C    12      I     3        I  FM  I  #ùã  - HOMEP ùãEMEHTA;
C    13      I     4        I  FM  I  #CM  - HOMEP TOKA ùãEMEHTA #ùã1;
C    14      I     5        I  LM  I  ATOKM  - AÑP B TOKN/N1 áHAóEHàü
C            I              I      I           TOKA HOMEP #CM
C            I              I      I           ùãEMEHTA #ùã;
C   ......   I     .....    I  ..  I ...............................
C   ......   I     .....    I  ..  I ...............................
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM
C----------------------------------------------------------------------
C-->  HAóAãà...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
       IF( TIP .EQ. 310 ) GO TO 100
       GO TO 200
C----------------------------------------------------------------------
C---> èPOèOPñàOHAãúHO CìMMàPìûôEE áBEHO ( Tàè  # 310 )
C----------------------------------------------------------------------
100   CONTINUE
C----------------------------------------------------------------------
C--> CKOãúKO ùãEMEHTOB?
C----------------------------------------------------------------------
       NBXOD = MSW(11)
       IF( NBXOD .EQ. 0 ) RETURN
C----------------------------------------------------------------------
C--> Ñãü KAÜÑOÉO ùãEMEHTA àôEM...
C----------------------------------------------------------------------
          DO 15 I= 1 , NBXOD
          NELMOT = MSW(8 + 4*I)
          NSM = MSW(9 + 4*I)
C----------------------------------------------------------------------
C-->  àCKATú B MSWALL ùãEMEHT C #ùã-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HAòãà àãà HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C-->  ùãEMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 500) NEL, TIP, NELMOT
500    FORMAT(//' *** áAèPOC HA ãOÉ. CBüáú ùãEMEHTA # ', I5,
     =  ' ( Tàè # ', I3, ' ) ***'/
     =  ' *** C HE CìôECTBìûôàM B ñEèà ùãEMEHTOM # ', I5,' ***'/
     =  ' *** OÅPAÅOTKA ñEèà èPEKPAôAETCü *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HAòãà, ECTú ãà ì ùãEMEHTA TPEÅìEMõâ TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 3
C----------------------------------------------------------------------
C-->  CBüáú C TOKOM , KOTOPOÉO HE CìôECTBìET ì áAÑAHHOÉO ùãEMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,501) NEL, TIP, NELMOT, NSM
501    FORMAT(//' *** áAèPOC HA ãOÉ. CBüáú ùãEMEHTA # ', I5,
     = ' ( Tàè # ', I3, ' ) ***'//
     = ' *** C ùãEMEHTOM # ', I5, ' ( CMEôEHàE',I3, ' ) - OòàÅOóEH',
     = ' ùãEMEHT HE àMEET TPEÅìEMOÉO TOKA ***'/
     = ' *** OÅPAÅOTKA ñEèà èPEKPAôAETCü ***'//)
       RETURN
3      CONTINUE
          MSW(11+4*I) = ATOKM + NSM - 1
15     CONTINUE
C----------------------------------------------------------------------
C--> áAKOHóàãà!
       KOD = 0
       RETURN
C======================================================================
C----------------------------------------------------------------------
C---> àHTEÉPàPìûôEE     áBEHO                 ( Tàè  # 311 )
C---> àHEPñàOHHOE       áBEHO                 ( Tàè  # 312 )
C---> ÑàîîEPEHñàPìûôEE  áBEHO                 ( Tàè  # 313 )
C---> èPOèOPñàOHAãúHO-àHTEÉPàPìûôEE     áBEHO ( Tàè  # 314 )
C---> èPOèOPñàOHAãúHO-ÑàîîEPEHñàPìûôEE  áBEHO ( Tàè  # 315 )
C---> îAáOBPAôATEãú                           ( Tàè  # 316 )
C---> KOããEÅATEãúHOE    áBEHO                 ( Tàè  # 317 )
C---> HEãàHEâHOE        áBEHO                 ( Tàè  # 320 )
C---> PEãEâHOE          áBEHO                 ( Tàè  # 321 )
C---> ìMHOÜàTEãú                              ( Tàè  # 322 )
C----------------------------------------------------------------------
200   CONTINUE
          NELMOT = MSW(12)
          NSM = MSW(13)
C----------------------------------------------------------------------
C-->  àCKATú B MSWALL ùãEMEHT C #ùã-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HAòãà àãà HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 4
C----------------------------------------------------------------------
C-->  ùãEMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 500) NEL, TIP, NELMOT
       RETURN
C----------------------------------------------------------------------
4      CONTINUE
C-->  MSW - HAòãà, ECTú ãà ì ùãEMEHTA TPEÅìEMõâ TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 6
C----------------------------------------------------------------------
C-->  CBüáú C TOKOM , KOTOPOÉO HE CìôECTBìET ì áAÑAHHOÉO ùãEMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,501) NEL, TIP, NELMOT, NSM
       RETURN
6      CONTINUE
          MSW(14) = ATOKM + NSM - 1
C----------------------------------------------------------------------
C--> áAKOHóàãà!
       KOD = 0
       RETURN
       END
