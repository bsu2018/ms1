         SUBROUTINE FM0006( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  FM0006( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹E‰ ˆH„“KTˆBHO_CBŸ‡AHH›X OMOTOK
C  ------------                           *****************************
C     KO‹-BO O-K  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C     K = 2        22      2-X OM. TPAHC.        L2
C     K = 3        23      3-X OM. TPAHC.        L3
C     K = 4        24      4-X OM. TPAHC.        L4
C     K = 5        25      5-X OM. TPAHC.        L5
C     K = 6        26      6-X OM. TPAHC.        L6
C
C                  BBO„ O OˆCAHˆž:
C
C            # T 2@ “ @ @ ..   R1 R2 .. L11 M12 M13..L22 M23 ...
C
C          “‡‹› O„K‹ž—EHˆŸ: <HA—A‹O OMOTOK> <KOH–› OMOTOK>
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹ˆ.
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹ˆ;
C   -----
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HE POˆ‡BO„ˆTCŸ;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
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
C I @ I       I  I   AME-I   AME-   I    Pˆ-  I    TOP   I
C I   I       I  I   TP› I   TP›    I    –A   I          I
C +---+-------+--+-------+----------+---------+----------+
C I ? I       I 1I K-C‹OBIK*(K+1)/2 IK*(K+1)/2I K C‹OB   I
C +---+-------+--+-------+----------+---------+----------+
C I 8 I       I  I       I          I         I          I
C +---+-------+--+-------+----------+---------+----------+
C             I          I          I         I
C            D1         D2         D3         D4
C            DR1        DR2        DR3        DR4
C
C    @ - ‹OKˆPOBAHO;
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
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
        EQUIVALENCE ( RK, K )
       INTEGER DR1, DR2
C-->  TA‹ˆ–A EPEBO„OB..
       DIMENSION KTIP(10)
       DATA KTIP/ 22, 2,   23, 3,   24, 4,  25, 5,  26, 6 /
       DATA K5 / 5 /
C-----------------------------------------------------------------------
       DATA MLM / 23, 33, 45, 59, 75 /
       DATA MD0 / 9,  9,  9,  9,  9  /
       DATA MD1 / 13,  15,  17,  19,  21 /
       DATA MD2 / 16,  19,  22,  25,  28 /
       DATA MD3 / 19,  25,  32,  40,  49 /
       DATA MD4 / 22,  31,  42,  55,  70 /
C-----------------------------------------------------------------------
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
CC-->  OˆCK "K"
        DO 2 I=1, K5
       IK = I
       IF ( KTIP( 2*I-1 )  .EQ.  TIP  )  GO TO 1
2      CONTINUE
C   HE HA˜‹ˆ.....
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0006: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C-->  K MO„E‹ˆ
       K = KTIP(  2*IK )
       K1 = K - 1
       IF ( K1  .LE. 0  .OR.  K1 .GT.  K5  )  CALL SUBERR(K1)
C-->  „‹ˆHA EPEMEHHO‰ —ACTˆ
       LM = MLM(K1)  -  ( 8+2*K)
       DO 100 I=1, LM
100    RMSW(I) = 0.0
C-->  ‡Aˆ˜EM  "K"
       RMSW(1) = RK
C----‹ž˜KA!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C      INDX = 1 - 2*K
C      RMSW(INDX) = RK
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C--- PˆEM APAMETPOB...
C  ˆX KO‹-BO...
       KPAR = K + ( K*(K+1) ) / 2
C----------------------------------------------------------------------
C*** KOHTPO‹Ÿ OKA HET !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C----------------------------------------------------------------------
       DO 101  I=1, KPAR
       RMSW(I+1) = PAR0T(I)
101    CONTINUE
       RETURN
         END
