         SUBROUTINE DM0011 ( MSW, RMSW, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1, KOD,
     *                         MSWALL, RMSWAL, ADRMSW )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  DM0011 ( MSW, RMSW, YALL, JALL, LOGYTR
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
C                  TˆA # 22  23  24  25  26  -->  ˆH„“K. CBŸ‡. OMOTKˆ
C                  ***************************
C     KO‹-BO O-K  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C     K = 2        22      2-X OM. TPAHC.        L2
C     K = 3        23      3-X OM. TPAHC.        L3
C     K = 4        24      4-X OM. TPAHC.        L4
C     K = 5        25      5-X OM. TPAHC.        L5
C     K = 6        26      6-X OM. TPAHC.        L6
C
C!!!!!
C    B H ˆ M A H ˆ E :   K  OPE„E‹ŸETCŸ ˆCXO„Ÿ ˆ‡ HOMEPA TˆA:
C    ****************
C                          K  = TIP - K20
C
C!!!!
C                  BBO„ O OˆCAHˆž:
C
C            # T 2@ “ @ @ ..   R1 R2 .. L11 M12 M13..L22 M23 ...
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
C   ------------      DEPOSA, MMAT2, 3,4,5,6; PMAT2,3,4,5,6
C                             I<-- REDUCE PRODUCT ------->I
C   PˆME—AHˆE:  1.CTP“KT“P“ MSW C‹OBA MAKPOMO„E‹ˆ „AHHOƒO TˆA CM. Hˆ†E
C   ----------   2. Pˆ ZAPROS ^=   1 ˆ‹ˆ 2 - COO™EHˆE ˆ “XO„;
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW TˆA # 22, 23, 24, 25, 26:
C                ------------------------------------------------------
C
C  +-------+--------+--------+--------+------+------+-------+---+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. IA„P.   I"K"I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. IHA—.   IKO-I
C  I HTA   I MO„E‹ˆ I PAC—ETAI MO„E‹ˆ I BO   I ‡HA— I"TOKN" IBO I
C  I       I        I DM.....I       .I “‡‹OBI EHˆ‰.I"TOKN1"IO__ MOTOK
C  +-------+--------+--------+--------+------+------+-------+---+
C  I  1    I   2    I   3    I    4   I   5  I  6   I  7    I 8 I
C  +-------+--------+--------+--------+------+------+-------+---+
C
C@@
C                  RMSW TˆOB # 22  23  24  25  26
C                  --------------------------------
C            LM<EPEMEHHAŸ>=+K + K*(K+1)/2 + K*(K+1)/2 + K = 2*K+K(K+1)
C              ***********   R     L           G         J   ----------
C
C            LM<O™AŸ> = 4*K + K*(K+1) + 8
C
C  OCTOŸHHAŸ I EPEMEHHAŸ RMSW-->
C +---+-------+--+-------+----------+---------+----------+
C I K I “‡‹›  I KI R AP-I L AP-   I GY MAT- I JY BEK-  I
C I   I       I  I   AME-I   AME-   I    Pˆ-  I    TOP   I
C I   I       I  I   TP› I   TP›    I    –A   I          I
C +---+-------+--+-------+----------+---------+----------+
C I 8 I 2*K   I@ I K-C‹OBIK*(K+1)/2 IK*(K+1)/2I K C‹OB   I
C +---+-------I--+-------+----------+---------+----------+
C             I          I          I         I
C             I          I          I         I
C             I          I          I         I
C            D1         D2         D3         D4
C            DR1        DR2        DR3        DR4
C
C
C               C M E ™ E H ˆ Ÿ :
C               -----------------
C             Tˆ #22       Tˆ # 23   Tˆ # 24   Tˆ # 25   Tˆ # 26
C   -------------------------------------------------------------------
C   K--->       2              3          4           5          6
C   -------------------------------------------------------------------
C   LM-->       23            33         45          59         75
C   -------------------------------------------------------------------
C   D0-->       9             9           9           9         9
C   -------------------------------------------------------------------
C   D1-->      13             15         17          19         21
C   -------------------------------------------------------------------
C   D2-->      16             19         22          25         28
C   -------------------------------------------------------------------
C   D3-->      19             25         32          40         49
C   -------------------------------------------------------------------
C   D4-->      22             31         42          55         70
C   -------------------------------------------------------------------
C   LMR->      15             19         29          41         55
C   -------------------------------------------------------------------
C
C   „‹Ÿ EPEMEHHO‰ —ACTˆ DR<*> = D<*> - D1 + 1
C                        $$$$$$$$$$$$$$$$$$$$$$
C
       DIMENSION MD0(5), MD1(5), MD2(5), MD3(5), MD4(5), MLM(5)
C
C@@
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
       DIMENSION TTX(6), UL(6), YMAT(36), G0(36)
       INTEGER DX2, D1, DX3, D0X, D0XK, D2, D4, D3, D4X
       DIMENSION MSWALL(*), RMSWAL(*)
       INTEGER ADRMSW
       DIMENSION TTY(6)
         DIMENSION MSW(*), RMSW(*), YALL(*), JALL(*), LOGYTR(*)
         DIMENSION VALLN(*), RMASPT(1), VALLN1(*), TOKN(*), TOKN1(*)
         EQUIVALENCE ( MASPRT(1), RMASPT(1) )
         REAL JALL
         EQUIVALENCE ( ZAPROS, MASPRT(28) )
         EQUIVALENCE ( NDALL,  MASPRT(23) )
         EQUIVALENCE ( TN,     MASPRT(29) )
         EQUIVALENCE ( H0,     MASPRT(30) )
         EQUIVALENCE ( H1,     MASPRT(32) )
         EQUIVALENCE ( H2,     MASPRT(33) )
         EQUIVALENCE ( ZEROCP, MASPRT(50) )
         INTEGER TIP,  ZAPROS
       DATA K6/6/, K2/2/, K22/22/, K26/26/, K20 /20/
       DATA MLM / 23, 33, 45, 59, 75 /
       DATA MD0 / 9,  9,  9,  9,  9  /
       DATA MD1 / 13,  15,  17,  19,  21 /
       DATA MD2 / 16,  19,  22,  25,  28 /
       DATA MD3 / 19,  25,  32,  40,  49 /
       DATA MD4 / 22,  31,  42,  55,  70 /
C
       IDK(I,J) = K*(J-1) + I
C      **********************
C--->  KOHTPO‹œ TˆA
       TIP = MSW(2)
         IF ( TIP    .LT.K22  .OR. TIP     .GT.  K26 ) GO TO 9999
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
       K = TIP - K20
       K1 = K - 1
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C$$$$$$IF ( K .LT. K2  .OR.  K  .GT.  K6 ) CALL SUBERR(K)
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C-->  CME™EHˆŸ
       DX2 = MD2(K1) - 1
       D1 =  MD1(K1)
       DX3 = MD3(K1) - 1
C-->  B "TOK"-->
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
C--
C-->  YMAT = L + H1 * R
       KX = K*(K+1)
       KX = KX/2
       DO 2 I=1,KX
       YMAT(I) = RMSW( DX2+I )
2      CONTINUE
       J = 1
       I = 1
3      YMAT(I) = YMAT(I) + H1*RMSW( D1 + J )
       I = I + ( K-J) + 1
       J = J + 1
       IF ( J .LE. K ) GO TO 3
C-->  G0 := [ L + H1*R ] **(-1)
       GO TO ( 4, 5, 6, 7, 8 ), K1
       CALL SUBERR(K1)
4      CALL MMTS2( YMAT, G0 )
       GO TO 9
5      CALL MMTS3( YMAT, G0 )
       GO TO 9
6      CALL MMTS4( YMAT, G0 )
       GO TO 9
7      CALL MMTS5( YMAT, G0 )
       GO TO 9
8      CALL MMTS6( YMAT, G0 )
C----------------------------------------------------------------------
C----------------------------------------------------------------------
9      CONTINUE
C-->  ‡AˆCœ  GY  =  H1 * G0
       DO 10  I=1,KX
       RMSW( DX3 + I ) = G0(I) * H1
10     CONTINUE
C-->  PAC—ET ˆCTO—HˆKOB TOKA...
C     ******
C-->  JY = G0 * [ L*TOKN + H2*TTX ]
C     ƒ„E  TTX = (UL(N) - R * TOKN)
C======================================================================
       IF ( H2 .EQ. 0.0 ) GO TO 11
C-->  HA‰Tˆ UL
       D0X = MD0(K1) - 1
       D0XK = D0X + K
       DO 1 I=1,K
       KKK = MSW( D0X + I )
       MMM = MSW( D0XK + I )
       VK = 0.0
       VM = 0.0
       IF ( KKK .NE. 0 ) VK = VALLN(KKK)
       IF ( MMM .NE. 0 ) VM = VALLN(MMM)
       UL(I) = VK - VM
1      CONTINUE
C-->  HA‰Tˆ TTX
       DO 20 I=1, K
       TTX(I) = H2*( UL(I) - RMSW(D1+I)*TOKN(ITOK1+I) )
20     CONTINUE
C-->  HA‰Tˆ L*TOKN -->  B  TTY
       D2 = MD2(K1)
       GO TO ( 22, 23, 24, 25, 26 ), K1
       CALL SUBERR(K1)
22     CALL PMTS2(RMSW(D2), TOKN(ITOK), TTY )
       GO TO 27
23     CALL PMTS3(RMSW(D2), TOKN(ITOK), TTY )
       GO TO 27
24     CALL PMTS4(RMSW(D2), TOKN(ITOK), TTY )
       GO TO 27
25     CALL PMTS5(RMSW(D2), TOKN(ITOK), TTY )
       GO TO 27
26     CALL PMTS6(RMSW(D2), TOKN(ITOK), TTY )
C----------------------------------------------------------------------
C----------------------------------------------------------------------
27     CONTINUE
C-->  HA‰Tˆ  TTX = TTX + TTY
       DO 28  I=1,K
28     TTX(I) = TTX(I) + TTY(I)
47     CONTINUE
C-->  HA‰Tˆ  JY = G0 * TTX
       D4 = MD4(K1)
       GO TO ( 32, 33, 34, 35, 36 ), K1
       CALL SUBERR(K1)
C----------------------------------------------------------------------
32     CALL PMTS2( G0, TTX, RMSW(D4) )
       GO TO 37
33     CALL PMTS3( G0, TTX, RMSW(D4) )
       GO TO 37
34     CALL PMTS4( G0, TTX, RMSW(D4) )
       GO TO 37
35     CALL PMTS5( G0, TTX, RMSW(D4) )
       GO TO 37
36     CALL PMTS6( G0, TTX, RMSW(D4) )
C----------------------------------------------------------------------
C----------------------------------------------------------------------
37     CONTINUE
C-->  GY  ˆ  JY  B  RMSW
C-->  BK‹A„›BAEM..., HO B HA—A‹E ˆ‡ "TPE“ƒO‹œHO‰" GY B O›—H“ž YMAT..
C======================================================================
C   B RMSW(D3)  GY, DX3 CME™EHˆE, KX „‹ˆHA RMSW(D3)....
C----------------------------------------------------------------------
C-->  OT 9.10.86.......
       KXX=1
       DO 710  I1 =1,K
              DO  711  J1=I1,K
                  IND0 = IDK( I1, J1 )
                  YMAT(IND0) = RMSW(DX3+KXX)
                  IND1 = IDK( J1, I1 )
                  YMAT(IND1) = RMSW(DX3+KXX)
C                 --------------------------
                  KXX = KXX + 1
711           CONTINUE
710    CONTINUE
C----------------------------------------------------------------------
C      I = 1
C      J = 1
C      KXX = 1
C 71     IND0 = IDK(I,J)
C      YMAT(IND0) = RMSW(DX3 + KXX )
C      IF ( I .LE. J ) GO TO 70
C      ?????????????????????????
C      IND1 = IDK(J,I)
C      YMAT(IND1) = RMSW(DX3 + KXX )
C 70     KXX = KXX + 1
C      J = J + 1
C      IF ( J .LE. K ) GO TO 71
C-->  C‹E„. CTPOKA..
C      I = I + 1
C      J = I
C      IF ( I .LE. K ) GO TO 71
C======================================================================
       KKON = 9 + K
       D3 = MD3(K1)
       CALL DEPOSA( MSW(9), MSW(KKON), K, K, YMAT,
     =              RMSW(D4), YALL, JALL )
       KOD = 0
       RETURN
C######################################################################
11     CONTINUE
C-->  H2 = 0.0
C      TTX = L * TOKN
       D2 = MD2(K1)
       GO TO ( 42, 43, 44, 45, 46), K1
       CALL SUBERR(K1)
C----------------------------------------------------------------------
42     CALL PMTS2( RMSW(D2),  TOKN(ITOK), TTX )
       GO TO 47
43     CALL PMTS3( RMSW(D2),  TOKN(ITOK), TTX )
       GO TO 47
44     CALL PMTS4( RMSW(D2),  TOKN(ITOK), TTX )
       GO TO 47
45     CALL PMTS5( RMSW(D2),  TOKN(ITOK), TTX )
       GO TO 47
46     CALL PMTS6( RMSW(D2),  TOKN(ITOK), TTX )
       GO TO 47
C----------------------------------------------------------------------
C######################################################################
C######################################################################
C######################################################################
C######################################################################
666    CONTINUE
C-->  ZAPROS = 2 ------------------------------------------------------
       ITOK = MSW(7)
       ITOK1 = ITOK - 1
C  ->  TOKN1 = GY * UL(N+1) + JY
       K = TIP - K20
       K1 = K - 1
       D0X = MD0(K1) - 1
       D0XK = D0X + K
       DO 60 I = 1, K
       KKK = MSW( D0X + I )
       MMM = MSW( D0XK + I )
       VK = 0.0
       VM = 0.0
       IF ( KKK .NE. 0 ) VK = VALLN1(KKK)
       IF ( MMM .NE. 0 ) VM = VALLN1(MMM)
       UL(I) = VK - VM
60     CONTINUE
C-->
       D3 = MD3(K1)
       D4X = MD4(K1) - 1
       GO TO ( 62, 63, 64, 65, 66 ), K1
       CALL SUBERR(K1)
C----------------------------------------------------------------------
62     CALL PMTS2( RMSW(D3), UL, TTX )
       GO TO 67
63     CALL PMTS3( RMSW(D3), UL, TTX )
       GO TO 67
64     CALL PMTS4( RMSW(D3), UL, TTX )
       GO TO 67
65     CALL PMTS5( RMSW(D3), UL, TTX )
       GO TO 67
66     CALL PMTS6( RMSW(D3), UL, TTX )
C----------------------------------------------------------------------
67     CONTINUE
C-->  ‡AˆCœ B TOKN1 = GY*UL + JY
       DO 68 I=1,K
68     TOKN1(ITOK1 + I) = TTX(I) + RMSW( D4X  +I )
       KOD = 0
       RETURN
C----------------------------------------------------------------------
9999     CONTINUE
         NWTRE = MASPRT(14)
         WRITE(NWTRE,9998) MSW(1), MSW(2)
9998     FORMAT(/' *** ‹Oƒ. CO‰ B DM0011 - ‡APOC HA OC‹“†ˆBAHˆE —“†Oƒ
     =O TˆA ***'/
     =    ' *** ‹EMEHT # ', I10, ' Tˆ MO„E‹ˆ # ', I10, ' ***'//)
         CALL SUBERR(0)
         END
