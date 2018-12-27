      SUBROUTINE MA1N13 (PARD, PARM, H, OMEGA, GAMMA, PARGY, PARG1)
CXXXX+++KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)++++++++++
C     P ì ó H O â    B A P à A H T ******
C   èOÑèPOÉPAMMA: MA1N13( PARD, PARM, H, OMEGA, GAMMA, PARGY, PARG1 )
C  -------------
C   HAáHAóEHàE:  BõóàCãEHàE MATPàñ GY à G1 Ñãü ùã. MAòàHõ 3X3X0
C   -----------
C
C               MOTOR MODEL: #_13  T__3_3_0__G                       !
C   PARGY ===> GY[1,1]...GY[KALL,1],   GY[1,2],...,GY[KALL,2]......
C         ===> GY[1,3]...GY[KALL,3],...GY[1,KALL],...,GY[KALL,KALL]
C
C   PARG1 ===> G1[1,1]...G1[KALL,1],   G1[1,2],...,G1[KALL,2]......
C         ===> G1[1,3]...G1[KALL,3],...G1[1,KALL],...,G1[KALL,KALL]
C
C                     ***  KALL = 6 ***
C
C              MATPàñõ PACóàTõBAûTCü èO îOPMìãAM:
C
C            GY = H * G0,  G1 * G0 * L(GAMMA)
C
C                                     D L(GAMMA)
C    G0 = [ L(GAMMA) + H*R + H*OMEGA* -----------  ] **(-1)
C                                     D GAMMA
C
C
C   METOÑ:    AHAãàTàóECKAü áAèàCú îOPMìã, C ìóETOM
C   -----     # C T P ì K T ì P H O â # èOÑOÅHOCTà
C              OèPEÑEãüEMõX MATPàñ;
C              îOPMìãõ --->  P ì ó H O â   B A P à A H T
C
C   èAPAMETPõ:  PARD, PARM - "OTPEáKà" MSW_ALL MACCàAB,
C   ---------   èOÑPOÅHEE CM. FM0008, DM0007
C
C              PARGY, PARG1 - BõóàCãEHHõE MATPàñõ;
C
C   èOÑèPOÉPAMMõ: HET;
C   ------------
C   èPàMEóAHàE:  1.  PARGY  à PARG1 -  áAèàCAHõ èO "CTOãÅñAM";
C   ----------
CZZZZZ---------------------------------------------------------------
      REAL LS,  MS,  LR,  MR,  MSR, LMSP, LMSM, LMRM, LMRP, MSR2D3
      REAL LS1, MS1, LR1, MR1, MSR1, LM0, LMP, LMM, LBIG, L0, LM, LP
C--------------------------------------------------------------------
          DIMENSION PARM(*), LBIG(6,6), GY(6,6), GX(6,6), G1PAR(6,6)
          DIMENSION PARD(*)
          DIMENSION PARGY(*), PARG1(*)
          DATA SQRT3/1.732050808/
          DATA R120 / 2.09440  /
C*******
C
C**********************************************************************
C              . Ñãü A à LM èPà       OMEGA à GAMMA
C--->  àá         PARD....(DR2 - HAóAãO )
C----------------------------------------------------------------------
                      RS   = PARD(1)
                      RR   = PARD(2)
                      LS   = PARD(3)
                      MS   = PARD(4)
                      LR   = PARD(5)
                      MR   = PARD(6)
                      MSR  = PARD(7)
                      PTAU = PARD(8)
                      JMOM = PARD(9)
C----------------------------------------------------------------------
C--->  MACCàB PARM......
                     LS1   = PARM(1)
                     MS1   = PARM(2)
                     LR1   = PARM(3)
                     MR1   = PARM(4)
                     MSR1  = PARM(5)
                     LMSP  = PARM(6)
                     LMSM  = PARM(7)
                     MSR2D3  = PARM(8)
C                    PTAU / JMOM   = PARM(9)
       LMRM = LR - MR
       LMRP = LR + MR
C------- òAÉà METOÑA "MU"-------*******
       HRS =            RS*H
       HRR =            RR*H
       H1Q =                       H*H
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
       SAM0 = SIN(GAMMA)
       SAMP = SIN(GAMMA + R120)
       SAMM = SIN(GAMMA - R120)
C------
C      AM0 = MSR * SAM0
C      AMP = MSR * SAMP
C      AMM = MSR * SAMM
C------
       HM = H  * OMEGA
C------
C      A0 = - OMEGA * AM0
C      AP = - OMEGA * AMP
C      AM = - OMEGA * AMM
C------
       CLM0 = COS(GAMMA)
       CLMP = COS(GAMMA + R120)
       CLMM = COS(GAMMA - R120)
C------
       LM0 = MSR1 * CLM0
       LMP = MSR1 * CLMP
       LMM = MSR1 * CLMM
C------
       L0 = MSR * CLM0
       LP = MSR * CLMP
       LM = MSR * CLMM
C$$$$$$ BõóàCãEHàü: $$$$$$$!!!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         HD = MSR2D3 * (1+ H1Q*OMEGA*OMEGA)
         ZG = (4.0 * (LMSM+HRS) * (LMRM+HRR) - 3.0*HD )
         GH1 = (4.0*(LMSP+HRS)*(LMRM+HRR) - HD) / (ZG*(LS+2.*MS+HRS))
         GH2 = -(4.0*MS*(LMRM+HRR) + HD) / (ZG*(LS+2.*MS+HRS))
         GH3 = (4.0*(LMRP+HRR)*(LMSM+HRS) - HD) / (ZG*(LR+2.*MR+HRR))
         GH4 = -(4.0*MR*(LMSM+HRS) + HD) / (ZG*(LR+2.0*MR+HRR))
         GH5 = -4.0 * MSR / ZG
C--------
         G1 = H  * GH1
         G2 = H  * GH2
         G3 = H  * GH3
         G4 = H  * GH4
C********G5 = H  * GH5
C
       GH50 = GH5 * ( CLM0 - HM * SAM0 )
       GH5P = GH5 * ( CLMP - HM * SAMP )
       GH5M = GH5 * ( CLMM - HM * SAMM )
         G50 = H * GH50
         G5P = H * GH5P
         G5M = H * GH5M
C**********
C-->  MATPàñA GY èO CTOãÅñAM--->
       PARGY(01) = G1
       PARGY(02) = G2
       PARGY(03) = G2
       PARGY(04) = G50
       PARGY(05) = G5P
       PARGY(06) = G5M
C----------------------------------------------------------------------
       PARGY(07) = G2
       PARGY(08) = G1
       PARGY(09) = G2
       PARGY(10) = G5M
       PARGY(11) = G50
       PARGY(12) = G5P
C----------------------------------------------------------------------
       PARGY(13) = G2
       PARGY(14) = G2
       PARGY(15) = G1
       PARGY(16) = G5P
       PARGY(17) = G5M
       PARGY(18) = G50
C----------------------------------------------------------------------
       PARGY(19) = G50
       PARGY(20) = G5M
       PARGY(21) = G5P
       PARGY(22) = G3
       PARGY(23) = G4
       PARGY(24) = G4
C----------------------------------------------------------------------
       PARGY(25) = G5P
       PARGY(26) = G50
       PARGY(27) = G5M
       PARGY(28) = G4
       PARGY(29) = G3
       PARGY(30) = G4
C----------------------------------------------------------------------
       PARGY(31) = G5M
       PARGY(32) = G5P
       PARGY(33) = G50
       PARGY(34) = G4
       PARGY(35) = G4
       PARGY(36) = G3
C----------------------------------------------------------------------
C********
C---> G1  MATPàñì  HAXOÑàM....
C----------
          GX(1,1) = GH1
          GX(1,2) = GH2
          GX(1,3) = GH2
          GX(1,4) = GH50
          GX(1,5) = GH5P
          GX(1,6) = GH5M
C$$$$$$$$$
          GX(2,1) = GH2
          GX(2,2) = GH1
          GX(2,3) = GH2
          GX(2,4) = GH5M
          GX(2,5) = GH50
          GX(2,6) = GH5P
C$$$$$$$$$$
          GX(3,1) = GH2
          GX(3,2) = GH2
          GX(3,3) = GH1
          GX(3,4) = GH5P
          GX(3,5) = GH5M
          GX(3,6) = GH50
C$$$$$$$$$$$
          GX(4,4) = GH3
          GX(4,5) = GH4
          GX(4,6) = GH4
C$$$$$$$$$$
          GX(5,4) = GH4
          GX(5,5) = GH3
          GX(5,6) = GH4
C$$$$$$$$$$$$
          GX(6,4) = GH4
          GX(6,5) = GH4
          GX(6,6) = GH3
C$$$$$$$$$$$$$
          DO 110 KX = 4,6
          DO 110 KY = 1,3
          GX( KX , KY ) = GX( KY , KX )
110       CONTINUE
C********
C-->   LBIG MATPàñA
          LBIG(1,1) = LS
          LBIG(1,2) = MS
          LBIG(1,3) = MS
          LBIG(1,4) = L0
          LBIG(1,5) = LP
          LBIG(1,6) = LM
C$$$$$$$$$
          LBIG(2,1) = MS
          LBIG(2,2) = LS
          LBIG(2,3) = MS
          LBIG(2,4) = LM
          LBIG(2,5) = L0
          LBIG(2,6) = LP
C$$$$$$$$$$
          LBIG(3,1) = MS
          LBIG(3,2) = MS
          LBIG(3,3) = LS
          LBIG(3,4) = LP
          LBIG(3,5) = LM
          LBIG(3,6) = L0
C$$$$$$$$$$$
          LBIG(4,4) = LR
          LBIG(4,5) = MR
          LBIG(4,6) = MR
C$$$$$$$$$$
          LBIG(5,4) = MR
          LBIG(5,5) = LR
          LBIG(5,6) = MR
C$$$$$$$$$$$$
          LBIG(6,4) = MR
          LBIG(6,5) = MR
          LBIG(6,6) = LR
C$$$$$$$$$$$$$
          DO 1100 KX = 4,6
          DO 1100 KY = 1,3
          LBIG( KX , KY ) = LBIG( KY , KX )
1100      CONTINUE
C**********************************************************************
C
C   G1 -->  ùTO  G0 * LBIG   ( GX * LBIG)
C
       DO 555 I=1,6
       DO 556 J=1,6
       S1 = 0.0
       DO 557 K=1,6
557    S1 = S1 + GX(I,K) * LBIG(K,J)
       G1PAR(I,J) = S1
556    CONTINUE
555    CONTINUE
C----------------------------------------------------------------------
C-->  áAèàCATú B PARGY  à PARG1 èO CTOãÅñAM...
C
       JXX = 1
       DO 777 I=1,6
       DO 777 J=1,6
C
       PARG1(JXX) = G1PAR( J, I )
C                        *******
        JXX = JXX + 1
777    CONTINUE
         RETURN
         END
