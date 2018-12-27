         SUBROUTINE DM0037 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT CEHTŸPŸ 1988 ƒ.)------------
C   O„POƒPAMMA:  DM0037 ( MSW, RMSW, YALL, JALL, LOGYTR
C  -------------             TOKN, TOKN1, VALLN, VALLN1, KOD,
C                              MSWALL, RMSWAL, ADRMSW )
C
C   HA‡HA—EHˆE:    B ‡ABˆCˆMOCTˆ OT ‡HA—EHˆŸ ”‹AƒA ZAPROS [MASPRT(28)]:
C   -----------    B›O‹HŸET C‹E„“ž™ˆE ”“HK–ˆˆ:
C
C                  A) ZAPROS = 1
C                     ----------
C                  ”OPMˆPOBAHˆE BK‹A„OB B YALL MATPˆ–“ POBO„ˆMOCTE‰ ˆ
C                  JALL - BEKTOP ˆCTO—HˆKOB TOKA ‡HA—EHˆ‰ APAMETPOB
C                  KBˆBA‹EHTH›X  G - J BETBE‰  MO„E‹ˆ ‹EMEHTA –Eˆ
C                  TˆA # 32  33  -->  TPAHC”OPMATOP› ;
C                  *********************
C     KO‹-BO O-K  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C     K = 2        32      2-X OM. TPAHC.       TP2
C     K = 3        33      3-X OM. TPAHC.       TP3
C
C!!!!!
C    B H ˆ M A H ˆ E :   K  OPE„E‹ŸETCŸ ˆCXO„Ÿ ˆ‡ HOMEPA TˆA:
C    ****************
C                          K  = TIP - 30
C
C!!!!
C                  BBO„ O OˆCAHˆž:
C
C            # T 3@ “ @ @ ..   R1 R2 .. L1 L2 .. W1 W2 .. RCT  LCT ;
C
C          “‡‹› O„K‹ž—EHˆŸ: <HA—A‹O OMOTOK> <KOH–› OMOTOK>
C
C
C                  B) ZAPROS = 2
C                     ----------
C                  B›—ˆC‹EHˆE ‡HA—EHˆ‰ TOKOB B BETBŸX B T(N+1) MOMEHT
C                  BPEMEHˆ   ˆCXO„Ÿ ˆ‡ G ˆ J APAMETPOB ˆ ‡HA—EHˆŸ
C                  “‡‹OB›X HAPŸ†EHˆ‰ HA ‹EMEHTE PAC—ˆTAHH›X „‹Ÿ T(N+1)
C                    ˆ ‡AˆCœ (”ˆKCA–ˆŸ) TˆX ‡HA—EHˆ‰ B "TOK T(N+1)";
C
C                  C) ZAPROS = 3
C                     **********
C                     HE O„„EP†ˆBAETCŸ ( PE‡EPB );
C
C                  D) ZAPROS = 4
C                     **********
C                     HE O„„EP†ˆBAETCŸ;
C
C   METO„:         „ˆCKPETˆ‡A–ˆŸ “PABHEHˆ‰ COCTOŸHˆŸ MO„E‹ˆ „AHHOƒO
C   -----          TˆA HA OCHOBE METO„A ‹ˆHˆ„†EPA BˆO‹“ƒˆ:
C                  Y(N+1) = Y(N) + H1*F(N+1) + H2*F(N).
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
C
C
C
C
C   O„POƒPAMM›:  1. COMMON PROF1, PROF2, PROF3;
C   ------------      DEPOSA,
C
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=   1 ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 32, 33
C                ------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   IPE-I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. IHA—.   I‡E-I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IPB I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I"TOKN1"I   I
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C
C
C                  RMSW TˆOB # 32  33:
C                  --------------------
C
C
C  OCT.  I EPEMEHHAŸ RMSW-->
C +-------+--+--------+--------+--------+------+------+----------+...>
C I “‡‹›  I KI R APA-I L AP- I W AP- I      I      I W1 MAT-  I...>
C I       I  I   MET- I   AME- I   AME- I R CT I L CT I    Pˆ-   I...>
C I       I  I    P›  I   TP›  I   TP›  I      I      I    –A    I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I       I 1I K C‹OB I K C‹OB I K C‹OB I  1   I  1   I K*(K+1)/2I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I 9-... I  I        I        I        I      I      I          I...>
C +-------+--+--------+--------+--------+------+------+----------+...>
C I          I        I        I        I             I
C D0         D1      D2       D3       D4            D5
C DR0        DR1     DR2      DR3      DR4           DR5
C
C
C ...>+---------+--------+------+------+------+
C ...>I G MAT-  I J BEK- I      I      I      I
C ...>I   Pˆ–A  I   TOP  I JMU  ITOKMUXI  KY  I
C ...>I         I        I      I      I      I
C ...>+---------+--------+------+------+------+
C ...>IK*(K+1)/2I K C‹OB I   1  I   1  I   1  I
C ...>+---------+--------+------+------+------+
C ...>I         I        I      I      I      I
C ...>+---------+--------+------+------+------+
C     I         I        I
C     D6       D7       D8
C     DR6      DR7      DR8
C
C
C
C
C
C
C
C
C
C
C               C M E ™ E H ˆ Ÿ :
C               -----------------
C             Tˆ #32       Tˆ # 33
C   -----------------------------------
C   K--->       2              3
C   -----------------------------------
C   LM-->      32             44
C   -----------------------------------
C   D0-->       9             9
C   -----------------------------------
C   D1-->      14             16
C   -----------------------------------
C   D2-->      16             19
C   -----------------------------------
C   D3-->      18             22
C   -----------------------------------
C   D4-->      20             25
C   -----------------------------------
C   D5-->      22             27
C   -----------------------------------
C   D6-->      25             33
C   -----------------------------------
C   D7-->      28             39
C   -----------------------------------
C   D8-->      30             42
C   -----------------------------------
C   LMR->      20             30
C--------------------------------------
C
C   „‹Ÿ EPEMEHHO‰ —ACTˆ DR<*> = D<*> - D1 + 2
C                        $$$$$$$$$$$$$$$$$$$$$
C
C@@
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         DIMENSION STR(3), TTN(3), ULN(3), Z(9), Z1(9), G0(9)
         DIMENSION MSWALL(*), RMSWAL(*)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
          DIMENSION MD0(2), MD1(2), MD2(2), MD3(2), MD4(2)
          DIMENSION MD5(2), MD6(2), MD7(2), MD8(2)
         INTEGER D0, D1, D2, D3, D4, D5, D6, D7, D8
         INTEGER ADRMSW
         INTEGER TIP,  ZAPROS
         REAL JALL, JMU, LCT
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         DATA MD0 /  9,   9  /
         DATA MD1 / 14,  16  /
         DATA MD2 / 16,  19  /
         DATA MD3 / 18,  22  /
         DATA MD4 / 20,  25  /
         DATA MD5 / 22,  27  /
         DATA MD6 / 25,  33  /
         DATA MD7 / 28,  39  /
         DATA MD8 / 30,  42  /
C######################################################################
C  BCTPOEHHAŸ ”“HK–ˆŸ  NIJ(I,J) OPE„E‹ŸET HOMEP ‹EMEHTA B BEKTOPE ,
C  B KOTOPOM XPAHˆTCŸ BEPXHETPE“ƒO‹œHAŸ —ACTœ CˆMMETPˆ—HO‰ KBA„PATHO‰
C  MATPˆ–› (O CTPOKAM) PA‡MEPHOCTœž  K .
C
       NIJ(I,J) = K*(I-1) - I*(I-1)/2 + J
C      **********************************
C######################################################################
C  BCTPOEHHAŸ ”“HK–ˆŸ  TDK(I,J) OPE„E‹ŸET HOMEP ‹EMEHTA B BEKTOPE ,
C  B KOTOPOM XPAHˆTCŸ KBA„PATHAŸ MATPˆ–A (O CTO‹–AM) PA‡MEPHOCTœž K.
C
       IDK(I,J) = K*(J-1) + I
C      **********************
C######################################################################
C--->  KOHTPO‹œ TˆA
       TIP = MSW(2)
       IF ( TIP .EQ. 32  .OR. TIP .EQ. 33  ) GO TO 9999
C----------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT(/' *** ‹Oƒ. CO‰ B DM0037 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
C----------------------------------------------------------------------
9999     CONTINUE
C######################################################################
       K = TIP - 30
       K1 = K / 3 + 1
       K2 = K * (K+1) / 2
C-->  CME™EHˆŸ
       D0 = MD0(K1)
       D1 = MD1(K1)
       D2 = MD2(K1)
       D3 = MD3(K1)
       D4 = MD4(K1)
       D5 = MD5(K1)
       D6 = MD6(K1)
       D7 = MD7(K1)
       D8 = MD8(K1)
C--> APAMETP›
       RCT = RMSW(D4)
       LCT = RMSW(D4+1)
C######################################################################
C---> B›OP PE†ˆMA:
         GO TO ( 667, 666                 ), ZAPROS
C===> ˆHA—E O˜ˆKA
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9997) MSW(3), ZAPROS
9997     FORMAT( ' *** DM MOHˆTOP #', I4, ' ‡APOC OC‹“†ˆBAHˆŸ O˜ˆO—EH
     = ***'/ ' *** ZAPROS = ', I5, ' - ˆƒHOPˆP“ETCŸ ***')
         RETURN
667      CONTINUE
C--------------------ZAPROS=1------------------------------------------
C--->  B›—ˆC‹EHˆE G - J APAMETPOB ˆ ˆX ‡AˆCœ A MSW:
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C-->  HAXO„ˆM CTAP›E TOKˆ.
C----------------------------------------------------------------------
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
       DO 10 I=1,K
       TTN(I) = TOKN(ITOK1+I)
10     CONTINUE
       TOKMU = TOKN(ITOK+K)
C----------------------------------------------------------------------
C-->  HAXO„ˆM CTAP›E HAPŸ†EHˆŸ.
C----------------------------------------------------------------------
       DO 20 I=1,K
       IMSW = D0 + I - 1
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       ULN(I) = VN - VK
20     CONTINUE
C----------------------------------------------------------------------
C--> KOHCTAHT›  ( T1 = H1 + LCT/RCT  )
C----------------------------------------------------------------------
       T1 = H1 + LCT / RCT
C----------------------------------------------------------------------
C--> OPE„E‹ˆM MATPˆ–“ "Z" ( Z = L + H1*R + H1*LCT*W1/T1 )
C----------------------------------------------------------------------
       A = H1 * LCT / T1
       DO 30 I=1,K2
       Z(I) = A * RMSW(D5+I-1)
30     CONTINUE
       DO 40 I=1,K
       I1 = I-1
       IZ = NIJ(I,I)
       Z(IZ) = Z(IZ) + RMSW(D2+I1) + H1*RMSW(D1+I1)
40     CONTINUE
C----------------------------------------------------------------------
C--> OPE„E‹ˆM  "JMU" ( JMU = [ (LCT/RCT - H2)*IMUN + H2*WT*I ] / T1 )
C----------------------------------------------------------------------
       JMU = TOKMU * ( LCT / RCT - H2 )
       A = 0
       DO 50 I=1,K
       A = A + TTN(I) * RMSW(D3+I-1)
50     CONTINUE
       JMU = ( JMU + H2 *A ) / T1
C--> ‡AOMHˆM "JMU"
       RMSW(D8) = JMU
C----------------------------------------------------------------------
C--> OPA™AEM MATPˆ–“ "Z"
C----------------------------------------------------------------------
       IF( K .EQ. 2 ) CALL MMTS2( Z , Z1 )
       IF( K .EQ. 3 ) CALL MMTS3( Z , Z1 )
C----------------------------------------------------------------------
C--> OPE„E‹ˆM BEKTOP "J"
C  J = Z1 * [ H2*UN + (L-H2*R-H2*RCT*W1)*IN + (H1*JMU + H2*IWUN)*RCT*W
C----------------------------------------------------------------------]
       A = -H2 * RCT
       DO 60 I=1,K2
       Z(I) = A * RMSW(D5+I-1)
60     CONTINUE
       DO 70 I=1,K
       I1 = I-1
       IZ = NIJ(I,I)
       Z(IZ) = Z(IZ) + RMSW(D2+I1) - H2 * RMSW(D1+I1)
70     CONTINUE
       IF( K .EQ. 2 ) CALL PMTS2( Z , TTN , STR )
       IF( K .EQ. 3 ) CALL PMTS3( Z , TTN , STR )
       A = RCT * ( H1*JMU + H2*TOKMU )
       DO 80 I=1,K
       TTN(I) = STR(I) + H2 * ULN(I) + A * RMSW(D3+I-1)
80     CONTINUE
       IF( K .EQ. 2 ) CALL PMTS2( Z1 , TTN , RMSW(D7) )
       IF( K .EQ. 3 ) CALL PMTS3( Z1 , TTN , RMSW(D7) )
C----------------------------------------------------------------------
C--> OPE„E‹ŸEM MATPˆ–“ "G"  ( G = H1 * Z1 )
C----------------------------------------------------------------------
       I1 = 1
       DO 90 I=1,K
       DO 90 J=I,K
       A = H1 * Z1(I1)
       RMSW(D6+I1-1) = A
       IND0 = IDK(I,J)
       IND1 = IDK(J,I)
       G0(IND0) = A
       G0(IND1) = A
       I1 = I1 + 1
90     CONTINUE
C----------------------------------------------------------------------
C-->  BK‹A„›BAEM
C----------------------------------------------------------------------
       KKON = 9 + K
C
C
       CALL DEPOSA( MSW(9), MSW(KKON), K, K, G0,
     =              RMSW(D7), YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
C######################################################################
666    CONTINUE
C-->  ZAPROS = 2 ------------------------------------------------------
C######################################################################
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
C----------------------------------------------------------------------
C-->  HAXO„ˆM HOB›E HAPŸ†EHˆŸ.
C----------------------------------------------------------------------
       DO 100 I=1,K
       IMSW = D0 + I - 1
       KN = MSW(IMSW)
       KK = MSW(IMSW+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN1(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN1(KK)
       ULN(I) = VN - VK
100    CONTINUE
C----------------------------------------------------------------------
C-->  “MHO†ˆM  G * ULN1
C----------------------------------------------------------------------
       IF( K .EQ. 2 ) CALL PMTS2( RMSW(D6), ULN, TTN )
       IF( K .EQ. 3 ) CALL PMTS3( RMSW(D6), ULN, TTN )
C----------------------------------------------------------------------
C-->  ‡AˆCœ B TOKN1 = G * ULN1 + J
C----------------------------------------------------------------------
       S = 0
       DO 110 I=1,K
       A = TTN(I) + RMSW( D7 - 1 + I )
       TOKN1(ITOK1 + I) = A
       S = S + A * RMSW(D3-1+I)
110    CONTINUE
       TOKN1(ITOK+K) = S * H1 / (H1+LCT/RCT) + RMSW(D8)
       KOD = 0
       KY = MSW(D8+2)
       IF( KY .EQ. 0 ) RETURN
C----------------------------------------------------------------------
C-->   KY = 1 ==> ‡A„AH PE†ˆM ABTOMATˆ—ECKOƒO B›XO„A HA “CT. PE†ˆM
C--> ˆ™EM MOMEHT „OCTˆ†EHˆŸ KPˆBO‰ HAPŸ†EHˆŸ U1 H“‹EBOƒO ‡HA—EHˆŸ
C----------------------------------------------------------------------
       KN = MSW(D0)
       KK = MSW(D0+K)
       VN = 0.0
       IF( KN .NE. 0 ) VN = VALLN(KN)
       VK = 0.0
       IF( KK .NE. 0 ) VK = VALLN(KK)
       U1N = VN - VK
       U1N1= ULN(1)
       IF( U1N1*U1N .GE.  0.0 ) RETURN
       TOKMUX = RMSW(D8+1)
       IF( U1N1 .GT. U1N ) TOKMUX = - TOKMUX
C----------------------------------------------------------------------
C--> “CTAHAB‹ˆBAEM PE†ˆM XO‹OCTOƒO XO„A
C----------------------------------------------------------------------
       TOKN (ITOK)   = 0.99 * TOKMUX
       TOKN1(ITOK)   = 0.99 * TOKMUX
          DO 120 I=2,K
          TOKN (ITOK+I-1) = 0.0
          TOKN1(ITOK+I-1) = 0.0
120    CONTINUE
       TOKN (ITOK+K) = 0.99 * TOKMUX * RMSW(D3)
       TOKN1(ITOK+K) = 0.99 * TOKMUX * RMSW(D3)
       MSW(D8+2) = 0
       RETURN
C****************
       END
