         SUBROUTINE FM0034( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT ”EBPA‹Ÿ  1989 ƒ.)------------
C   O„POƒPAMMA:   FM0034( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹ˆ TPAHC”OPMATOPOB
C  ------------    *************************************
C     KO‹-BO O-K  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C     K = 6        36      2-X OM. 3-X ”A‡H.    TP6
C                          TPAHC”OPMATOP
C     K = 9        39      3-X OM. 3-X ”A‡H.    TP9
C                          TPAHC”OPMATOP
C
C
C                  BBO„ O OˆCAHˆž:
C
C            # T 3@ “ @ @ ..   SH UH PXX IXX PKZ1 PKZ2 ..UKZ1 UKZ2
C            ..R1 R2 ..W1 W2 ..K“
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
C
C
C   „‹Ÿ EPEMEHHO‰ —ACTˆ DR<*> = D<*> - D1 + 2
C                        $$$$$$$$$$$$$$$$$$$$$
C
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION MLM(2)
       INTEGER BA0T, TIP
       INTEGER DR1, DR2, DR3, DR4, DR5
       REAL IH, IXX, LCT,L1,L2
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE ( RN, N )
       DATA MLM /110,120 /
C-->  HA˜ ‹ˆ Tˆ ?
       NEL = BA0T(1)
       TIP = BA0T(2)
       NWTR = MASPRT(13)
       IF( TIP .EQ. 36  .OR.  TIP .EQ. 39 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0034: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C----------------------------------------------------------------------
C    TPEX”A‡H›‰ „B“XOMOTO—H›‰ TPAHC”OPMATOP (Tˆ # 36)
C    TPEX”A‡H›‰ TPEXOMOTO—H›‰ TPAHC”OPMATOP (Tˆ # 39)
C----------------------------------------------------------------------
1        CONTINUE
C-->  K MO„E‹ˆ
       K = TIP - 30
       K1 = K / 3 - 1
C-->  „‹ˆHA EPEMEHHO‰ —ACTˆ
       LM = MLM(K1)  -  ( 8+2*K)
       DO 200 I=1, LM
200    RMSW(I) = 0.0
C-->  ‡Aˆ˜EM  "K"
       N = K
       RMSW(1) = RN
C----------------------------------------------------------------------
C--> PˆEM APAMETPOB...
C  ˆX KO‹-BO...
       KPAR = 3 *( K + 2)
C----------------------------------------------------------------------
C*** KOHTPO‹œ HA O‹O†ˆTE‹œHOCTœ !!!
C----------------------------------------------------------------------
       DO 201  I=1, 10
       IF( PAR0T(I) .LE. 0.0 ) GO TO 9999
201    CONTINUE
C===>  PˆCBAˆBAEM „AHH›E...
       SH = PAR0T(1)/3.
       UH = PAR0T(2)
       IH = SH/UH
       PXX = PAR0T(3)/3.
       IXX = PAR0T(4)*IH/100.
       PKZ = PAR0T(5)/3.
       UKZ = PAR0T(6)*UH/100.
       R1 = PAR0T(7)
       R2 = PAR0T(8)
       W1 = PAR0T(9)
       W2 = PAR0T(10)
C===>  PAC—ˆT›BAEM ˆH„“KTˆBHOCTˆ...
       XXX = SQRT(UH**2*IXX**2-PXX**2)/IXX**2
       A = PXX/IXX**2 - R1
       RKZ = PKZ/IH**2
       XKZ = SQRT(UKZ**2*IH**2 - PKZ**2)
     * /IH**2
       AKT= RKZ - R1
       R2 = R2*W1**2/W2**2
C ******************************************
C==>   BHˆMAHˆE ‡„ECœ PˆBE„EHH›E ‡HA—EHˆŸ
C ******************************************
C==>  B›—ˆC‹ŸžTCŸ Pˆ‹ˆ†EHO
       XL2 = XKZ/2.
       XL1 = XL2
       B = XXX - XL1
       RCT = B**2/A + A
       XLCT = RCT*A/B
       PI = 3.1415926
       L1 = XL1/(100.*PI)
       LCT = XLCT/(100.*PI*W1**2)
       RCT = RCT/W1**2
       L2= XL2/(100.*PI)*W2**2/W1**2
       R2 = R2*W2**2/W1**2
C==> ‡AKOH—ˆ‹ˆ PAC—ET...
       DO 210   I =1,3
       RMSW(1 + I) = R1
       RMSW(4 + I) = R2
       RMSW(7 + I) = L1
       RMSW(10 + I) = L2
       RMSW(13 + I) =W1
       RMSW(16 + I) = W2
       RMSW(19 + I) = RCT
       RMSW(22 + I) = LCT
 210   CONTINUE
C----------------------------------------------------------------------
C--> PAC—ˆTAEM MATPˆ–“  X
       IDR3 = 3*K + 2
       A = RMSW(IDR3)
       B = RMSW(IDR3 + 1)
       C = RMSW(IDR3 + 2)
       T = A + B + C
C ---> ‡AO‹HŸEM X ...
       RMSW(IDR3 + 6) = A*(B+C)/T
       RMSW(IDR3 + 7) =- A*B/T
       RMSW(IDR3 + 8) =- A*C/T
       RMSW(IDR3 + 9) =- B*A/T
       RMSW(IDR3 +10) = B*(C+A)/T
       RMSW(IDR3 +11) =- C*B/T
       RMSW(IDR3 +12) =- C*A/T
       RMSW(IDR3 +13) =- C*B/T
       RMSW(IDR3 +14) = C*(B+A)/T
C----------------------------------------------------------------------
C      PACCTAB‹ŸEM HA—A‹œH›E ‡HA—EHˆŸ TOKJU
C----------------------------------------------------------------------
       RMSW(89) = IXX*SQRT(2.)
       N   = IFIX(PAR0T(11))
       RMSW(90) = RN
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 501) TIP, NEL
501      FORMAT(' *** ‹Oƒ. CO‰ B FM0034: „AHH›E HE O‹O†ˆTE‹œH›      **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
       RETURN
CCC    DEBUG INIT
       END
