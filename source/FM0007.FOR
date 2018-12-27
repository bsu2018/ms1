         SUBROUTINE FM0007( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  FM0007( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹E‰:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                  71      "HE‹ˆH. COPOTˆB‹EHˆE"     RN
C
C                  72      "HE‹ˆH. ˆH„“KTˆBHOCTœ"     LN
C
C                  720     "HE‹ˆH. ˆH„“KTˆBHOCTœ" LN ‡A„AHHAŸ —EPE‡
C                                                 L_ „ˆ””. OT I_TOKA;
C
C       HE‹ˆHE‰HOCTœ ‹EMEHTA ‡A„AETCŸ TA‹ˆ—HO, —EPE‡ MAS <...>:
C
C       „‹Ÿ  "RN" ==>  BO‹œT-AMEPHAŸ X-KA:
C
C             MAS < I1 U1  I2 U2  ... ...   I_N U_N >;
C
C       „‹Ÿ  "LN" ==> BEEP-AMEPHAŸ X-KA: ( Tˆ #72 ):
C
C             MAS < I1 ”1  I2 ”2  ... ...   I_N ”_N >;
C
C       „‹Ÿ  "LN" ==> —EPE‡ L_„ˆ”. OT TOKA:
C
C             MAS < I1 L_„1 I2 L_„2 .....           >;
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹ˆ.
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹ˆ, C‹A‰H PA‡‹O†EHˆE
C   -----          TA‹ˆ—HO ‡A„AHH›X HE‹ˆHE‰H›X ‡ABˆCˆMOCTE‰.
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW Pˆ FGMSW = 0;
C                  LM - „‹ˆHA BCEƒO MSW OˆCAHˆŸ      Pˆ FGMSW = 1;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------   2. SPLIN0
C
C   PˆME—AHˆE:    1. KOHTPO‹œ B MˆHˆMA‹œHOM O"EME;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. HEOXO„ˆMO KO„ˆPOBATœ HE MEHEE „B“X TO—EK BAX.
C                     ‡A„AHHAŸ Y=F(X) HE „O‹†HA ˆMETœ POˆ‡BO„H“ž
C                     PABH“ž H“‹ž --> ˆHA—E CO‰ Pˆ PAC—ETE.......!!!!!
C
C                  4. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C                  RMSW TˆA # 71, 72 ˆ 720 [ HE‹ˆHE‰H›E R ˆ L ]:
C                  ----------------------------------------------
C                                                        LM=<EPEMEHHAŸ>
C                                                          =+1<VAR>
C                                                  (OT KO‹-AB TO—EK BAX)
C                    SPM
C +-----+----+-----------------------+
C I  G  I  J I  XY_MAS               I
C I     I    I MACCˆB TO—EK BAX.     I    MACˆB› "B", "C", "D" -->
C I     I    I N,(X,Y).."B","C","D"  I    C‹A‰H PA‡‹O†EHˆE BAX:
C +-----+----+-----------------------+    CM. / SPLIN0;
C I  1  I 2  I 3 ... 3+(5*N+1)       I
C +-----+----+-----------------------+
C I  11 I 12 I 13 ...12+(5*N+1)      I
C +-----+----+-----------------------+
C
C   N -> KO‹ˆ—ECTBO TO—EK BAX.
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         EQUIVALENCE( FGMSW, MASPRT(87) )
         EQUIVALENCE( N,       RN       )
         INTEGER BA0T, TIP, FGMSW
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 71  .OR.  TIP .EQ. 72  .OR. TIP.EQ.720) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 501) TIP, NEL
501      FORMAT(' *** ‹Oƒ. CO‰ B FM0007: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> —TO HA„O?
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C--> ‡APOC "“TO—HˆTœ „‹ˆH“ MSW"
C      LM:=8 + 2<HOMEPA “‡‹OB> + 2<G-J APAMETP›> + 5*N+1<MACCˆB XYMAS>
C          N := KO‹-BO TO—EK BAX (X,Y)
C
           NMAS = IFIX( PAR0T(1) )
           N  = NMAS/2
           LM = 8 + 2 +2 + 5*N + 1
C----------------------------------------------------------------------
         RETURN
1000     CONTINUE
C---> ”OPMATˆPOBATœ RMSW <OTPE‡OK>;
C-->  CKO‹œKO —ˆCE‹...
       NMAS=IFIX( PAR0T(1) )
C--> CKO‹œKO TO—EK..
       N = NMAS/2
C-->  „O“CTˆMO, ‹ˆ TO ?
       IF ( N .GE. 2 ) GO TO 1001
C-->  PAOTATœ HE‹œ‡Ÿ...
       NWTR = MASPRT(13)
       WRITE(NWTR,500) NEL
500    FORMAT(/' ***  „‹Ÿ ‹EMEHTA # ', I5, ' MA‹O TO—EK „‹Ÿ B.A.X.',
     = ' ***'/ ' *** PAC—ET PEP›BAETCŸ ***'//)
       call STOPP(28)
1001   CONTINUE
       LM = 2 + 5*N + 1
       DO 1002 I=1,LM
1002   RMSW(I) = 0.0
C-->  PˆHŸTœ MACCˆB›...
       RMSW(3) = RN
C       MSW(3) = N
C
       DO 1003 I=1, NMAS
       RMSW(3+I) = PAR0T(1+I)
1003   CONTINUE
C--->  HA‰„EM C‹A‰H PA‡‹O†EHˆE....
       CALL SPLIN0( RMSW(3) )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       RETURN
       END
