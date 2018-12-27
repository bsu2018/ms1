       SUBROUTINE MOT0M8(G12, J12, V12, TN, H, MU, ZFZ,
     *                   MFN,MBN,OMEGAN,GAMAN,I1,I2,I3,I4,I5,I6)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)-------
C  MOÑàîàñàPOBAHHõâ PACóET BKãAÑOB MOÑEãà MAòàHõ 3*3
C G12, J12 - BKãAÑõ èPOB. à àCT. TOKA
C  V12 èOTEHñàAãõ ìáãOB B TN;
C  H - òAÉ, MU METOÑ, ZFZ=+1;
C  MFN, MBN - ùã. à BHEò. MOMEHTõ B TN;
C OMEGAN, GAMAN - CKOPOCTú à ìÉOã B (TN);
C I1-I6  TOKà B TN;
CZZZZ-------------------------------------------------------------------
         DIMENSION G12(12,12), J12(*), V12(*)
         DIMENSION LP6(6,6), RP6(6), JX(6), GX(6,6)
         REAL I1, I2, I3, I4, I5, I6
         REAL LP6, JI, KPPOL, MU, MF, MB, MFN, MBN, JIP
         REAL J1, J2, J3, J4, J5, J6, JX
         REAL LS,  MS,  LR,  MR,  MSR
         REAL LS1, MS1, LR1, MR1, MSR1
         REAL LMSP, LMSM, LMRP, LMRM, MSR2D3
         REAL LM0, LMP, LMM
C
         INTEGER FLAG, GFLAG
C
         DATA FLAG/0/, GFLAG/0/, KPPOL/4.0/
C U - HAèPüÜEHàü HA OÅMOTKAX
         U1=V12(1) - V12(7)
         U2=V12(2) - V12(8)
         U3=V12(3) - V12(9)
         U4=V12(4) - V12(10)
         U5=V12(5) - V12(11)
         U6=V12(6) - V12(12)
C  KAKOâ BXOÑ ?
         IF ( FLAG .EQ. 1 ) GO TO 1
C****** HACTPOâKA MOÑEãà************
       CALL MOT004 ( LP6, RP6, JI )
       LS = LP6(1,1)
       MS = LP6(1,2)
       LR = LP6(4,4)
       MR = LP6(4,5)
       MSR = LP6(1,4)
       RS = RP6(1)
       RR = RP6(4)
C****** ùãEMEHTõ OÅPATHOâ ******
       LMSP = LS + MS
       LMSM = LS - MS
       LMRP = LR + MR
       LMRM = LR - MR
       MSR2D3 = 3.0 * MSR * MSR
C-------
       ZLM = 4.0 * LMSM * LMRM  -  3.0 *MSR2D3
       LS1 = ( 4.0 * LMSP * LMRM  -  MSR2D3 ) / (ZLM*(LS+2.0*MS))
       MS1 =-( 4.0 *  MS  * LMRM  +  MSR2D3 ) / (ZLM*(LS+2.0*MS))
       LR1 = ( 4.0 * LMRP * LMSM  - MSR2D3  ) / (ZLM*(LR+2.0*MR))
       MR1 =-( 4.0 *  MR  * LMSM  + MSR2D3  ) / (ZLM*(LR+2.0*MR))
       MSR1 = -4.0 * MSR / ZLM
C------- òAÉà METOÑA "MU"-----------
       HRS = (1.0 - MU)*RS*H
       HRR = (1.0 - MU)*RR*H
       H1Q = (1.0 - MU)*(1.0 - MU)*H*H
C*******
       H1 = (1.0 - MU ) * H
       H2 = MU * H
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C********BBOÑ OMENA  à GAMA  (  KìCOK ìÅPATú )
          WRITE(6,2)
2         FORMAT(' BBEÑàTE HAó. áHAóEHàü OMEGA à GAMA (ùã.ÉPAÑ.)')
          READ(5, * ) OMEGA, GAMA
         WRITE(6, * ) OMEGA, GAMA
         GAMA = GAMA * ( 3.1415927 / 180.0 )
         R120 = 120.0* ( 3.1415927 / 180.0 )
       OMEGAN = OMEGA
       GAMAN = GAMA
       HN = 0.0
       CALL MOT009 ( MB, TN, OMEGAN)
       MBN = MB
       JIP = JI / KPPOL
       WRITE(6,3)
3      FORMAT(' BBEÑàTE HAó. áHAóEHàE TOKOB B OÅMOTKAX S à R')
       READ(5, *) I1, I2, I3, I4, I5, I6
       WRITE(6, *) I1, I2, I3, I4, I5, I6
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C****** OèPEÑEãEHàE KOùî. Ñãü A à LM **********************************
       AM0 = MSR * SIN ( GAMA )
       AMP = MSR * SIN ( GAMA + R120 )
       AMM = MSR * SIN ( GAMA - R120 )
C-------
       LM0 = MSR1 * COS ( GAMA )
       LMP = MSR1 * COS ( GAMA + R120 )
       LMM = MSR1 * COS ( GAMA - R120 )
C-------
       A0 = - OMEGA * AM0
       AP = - OMEGA * AMP
       AM = - OMEGA * AMM
C----------------------------------------------------------------------
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         WRITE(3, 306) ( I, I=1,6)
306      FORMAT(/20X,'ÑAHHõE èPOÉHOáA ( OÑHA àTEPPAñàü ): [G1-AHAã.]'/
     *           1X,'TN1',9X,6('I',I1,10X),'OMEÉA',7X,'ÉAMMA',7X,'MF')
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       FLAG = 1
C***** HAó. ìCTAHOBKà BõèOãHEHõ. **************************************
       GO TO 5
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C
C      HE èEPBõâ BXOÑ, PACóET TOKOB B MOMEHT TN.
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
1      CONTINUE
       GAMAN = GAMA
       OMEGAN = OMEGA
       MBN = YMB
C------
C  G1, G2, G3, G4, G50, G5P, G5M - OèPEÑEãEHõ;
C      J1, J2, J3, J4, J5, J6 -  OèPEÑEãEHõ;
C------
       I1 = G1*U1 + G2*(U2+U3)   +  G50*U4 + G5P*U5 + G5M*U6 + J1
       I2 = G2*(U1+U3) + G1*U2   +  G5M*U4 + G50*U5 + G5P*U6 + J2
       I3 = G2*(U1+U2) + G1*U3   +  G5P*U4 + G5M*U5 + G50*U6 + J3
       I4 = G50*U1 + G5M*U2 + G5P*U3   +  G3*U4 + G4*(U5+U6) + J4
       I5 = G5P*U1 + G50*U2 + G5M*U3   +  G4*(U4+U6) + G3*U5 + J5
       I6 = G5M*U1 + G5P*U2 + G50*U3   +  G4*(U4+U5) + G3*U6 + J6
C------
5      CONTINUE
C   PACóET X1 - X6  - óACTEâ àCTOóHàKOB TOKA
C   ( OHà ÅìÑìT àCèOãúáOBAHõ à Ñãü üBHOÉO òAÉA )
C
C------ [AI] = [U] - [A] * [I]
       AI1 = U1 - ( RS*I1 + A0*I4 + AP*I5 + AM*I6 )
       AI2 = U2 - ( RS*I2 + AM*I4 + A0*I5 + AP*I6 )
       AI3 = U3 - ( RS*I3 + AP*I4 + AM*I5 + A0*I6 )
       AI4 = U4 - ( A0*I1 + AM*I2 + AP*I3 + RR*I4 )
       AI5 = U5 - ( AP*I1 + A0*I2 + AM*I3 + RR*I5 )
       AI6 = U6 - ( AM*I1 + AP*I2 + A0*I3 + RR*I6 )
C------
C------  [X] = [LM] * ( [U] - [A] * [I] )
C------
       X1 = LS1*AI1 + MS1*(AI2+AI3) + LM0*AI4 + LMP*AI5 + LMM*AI6
       X2 = LS1*AI2 + MS1*(AI1+AI3) + LMM*AI4 + LM0*AI5 + LMP*AI6
       X3 = LS1*AI3 + MS1*(AI1+AI2) + LMP*AI4 + LMM*AI5 + LM0*AI6
       X4 = LM0*AI1 + LMM*AI2 + LMP*AI3 + LR1*AI4 + MR1*(AI5+AI6)
       X5 = LMP*AI1 + LM0*AI2 + LMM*AI3 + LR1*AI5 + MR1*(AI4+AI6)
       X6 = LMM*AI1 + LMP*AI2 + LM0*AI3 + LR1*AI6 + MR1*(AI4+AI5)
C------
C   PACóET MF B TN
C------
       MF = ( I1*I4 + I2*I5 + I3*I6 ) * AM0 +
     *      ( I1*I5 + I2*I6 + I3*I4 ) * AMP +
     *      ( I1*I6 + I2*I4 + I3*I5 ) * AMM
       MF = MF * KPPOL * ZFZ
       MFN = MF
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C   èPOÉHOá èO üBHOâ CXEME
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
       YI1 = I1 + H * X1
       YI2 = I2 + H * X2
       YI3 = I3 + H * X3
       YI4 = I4 + H * X4
       YI5 = I5 + H * X5
       YI6 = I6 + H * X6
       YOMEGA = OMEGAN + H * (MBN - MFN ) / JIP
       YGAMA = GAMAN + H * OMEGAN
C  PACóET MF èO HOBõM TOKAM à ìÉãAM èO ÑAHHõM èPOÉHOáA
       YMF = ( YI1*YI4 + YI2*YI5 + YI3*YI6 ) * SIN(YGAMA) +
     *       ( YI1*YI5 + YI2*YI6 + YI3*YI4 ) * SIN(YGAMA+R120) +
     *       ( YI1*YI6 + YI2*YI4 + YI3*YI5 ) * SIN(YGAMA-R120)
       YMF = MSR * KPPOL * YMF * ZFZ
C######################################################################
C----------------------------------------------------------------------
C   HOBõE áHAóEHàü èPà TN+H ...........................................
C----------------------------------------------------------------------
       T1 = TN + H
C-----BHEòHEÉO MOMEHTA:
       CALL MOT009 ( YMB, T1, YOMEGA )
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C   èPOMEÜìTOóHõâ BõBOÑ
       TTX = ( T1*1.8E+4 ) + 0.5
       LTTX = IFIX(TTX)
       L360X = LTTX - (LTTX/360) * 360
       GGAMA = YGAMA * 57.29577
       IF ( L360X .EQ. 0 )
     * WRITE(3,6) T1, YI1, YI2, YI3, YI4, YI5, YI6, YOMEGA, GGAMA, YMF
6      FORMAT(1X,10(G12.5))
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C----------------------------------------------------------------------
C  èO HEüBHOâ CXEME ìTOóHüEM
C
       OMEGA = OMEGAN + ( H1 * (YMB - YMF) + H2 * (MBN - MFN) ) / JIP
       GAMA = GAMAN + H1 * YOMEGA + H2 * OMEGAN
C**********************************************************************
C    HOBõE KOùî. Ñãü A à LM èPà HOBõX OMEGA à GAMA
       SAM0 = SIN(GAMA)
       SAMP = SIN(GAMA + R120)
       SAMM = SIN(GAMA - R120)
C------
       AM0 = MSR * SAM0
       AMP = MSR * SAMP
       AMM = MSR * SAMM
C------
       HM = H1 * OMEGA
C------
       A0 = - OMEGA * AM0
       AP = - OMEGA * AMP
       AM = - OMEGA * AMM
C------
       CLM0 = COS(GAMA)
       CLMP = COS(GAMA + R120)
       CLMM = COS(GAMA - R120)
C------
       LM0 = MSR1 * CLM0
       LMP = MSR1 * CLMP
       LMM = MSR1 * CLMM
C------
C**********************************************************************
C  PACóET G1 - G5, Z1 à Z2
C**********************************************************************
C
C------- IF ( H .EQ. HN ) GO TO 7
C     èPà G1 - G5 HE F(OMEGA), BõèOãHàTú TOãúKO èPà CMEHE òAÉA H
C------- HN = H
C        IF ( GFLAG . EQ. 1 ) GO TO 7
C--------   èPà èOCTOüHHOM òAÉE ( àãà èPà EÉO MAãOM àáMEHEHàà )
C             BõèOãHüTú TOãúKO OÑàH PAá
C        GFLAG = 1
C$$$$$$ BõóàCãEHàü: $$$$$$$!!!!!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         HD = MSR2D3 * (1+ H1Q*OMEGA*OMEGA)
         ZG = (4.0 * (LMSM+HRS) * (LMRM+HRR) - 3.0*HD )
         GH1 = (4.0*(LMSP+HRS)*(LMRM+HRR) - HD) / (ZG*(LS+2.*MS+HRS))
         GH2 = -(4.0*MS*(LMRM+HRR) + HD) / (ZG*(LS+2.*MS+HRS))
         GH3 = (4.0*(LMRP+HRR)*(LMSM+HRS) - HD) / (ZG*(LR+2.*MR+HRR))
         GH4 = -(4.0*MR*(LMSM+HRS) + HD) / (ZG*(LR+2.0*MR+HRR))
         GH5 = -4.0 * MSR / ZG
C--------
         G1 = H1 * GH1
         G2 = H1 * GH2
         G3 = H1 * GH3
         G4 = H1 * GH4
C********G5 = H1 * GH5
  7      CONTINUE
       GH50 = GH5 * ( CLM0 - HM * SAM0 )
       GH5P = GH5 * ( CLMP - HM * SAMP )
       GH5M = GH5 * ( CLMM - HM * SAMM )
         G50 = H1* GH50
         G5P = H1* GH5P
         G5M = H1* GH5M
C**********
C PACóET J6 - TOKOB àCTOóHàKOB
C*********
C
C  [XI] = [I] + H2 * [LM] * ( [U] - [A] * [I] )
C
         XI1 = I1 + H2 * X1
         XI2 = I2 + H2 * X2
         XI3 = I3 + H2 * X3
         XI4 = I4 + H2 * X4
         XI5 = I5 + H2 * X5
         XI6 = I6 + H2 * X6
C--------
C      [ZI] = [L(TN+H)] * [XI]
C------------>
         ZI1 = LS*XI1+MS*(XI2+XI3)+MSR*(CLM0*XI4+CLMP*XI5+CLMM*XI6)
C------------>
         ZI2 = LS*XI2+MS*(XI1+XI3)+MSR*(CLMM*XI4+CLM0*XI5+CLMP*XI6)
C------------>
         ZI3 = LS*XI3+MS*(XI1+XI2)+MSR*(CLMP*XI4+CLMM*XI5+CLM0*XI6)
C------------>
         ZI4 = MSR*(CLM0*XI1+CLMM*XI2+CLMP*XI3)+LR*XI4+MR*(XI5+XI6)
C------------>
         ZI5 = MSR*(CLMP*XI1+CLM0*XI2+CLMM*XI3)+LR*XI5+MR*(XI4+XI6)
C------------>
         ZI6 = MSR*(CLMM*XI1+CLMP*XI2+CLM0*XI3)+LR*XI6+MR*(XI4+XI5)
C------------>
C****************
C   àCTOóHàKà TOKA:   [J] = ( [G] / H1 ) * [ZI]
C
         J1 = GH1*ZI1 + GH2*(ZI2+ZI3) + GH50*ZI4 + GH5P*ZI5 + GH5M*ZI6
         J2 = GH1*ZI2 + GH2*(ZI1+ZI3) + GH5M*ZI4 + GH50*ZI5 + GH5P*ZI6
         J3 = GH1*ZI3 + GH2*(ZI1+ZI2) + GH5P*ZI4 + GH5M*ZI5 + GH50*ZI6
         J4 = GH50*ZI1 + GH5M*ZI2 + GH5P*ZI3 + GH3*ZI4 + GH4*(ZI5+ZI6)
         J5 = GH5P*ZI1 + GH50*ZI2 + GH5M*ZI3 + GH3*ZI5 + GH4*(ZI4+ZI6)
         J6 = GH5M*ZI1 + GH5P*ZI2 + GH50*ZI3 + GH3*ZI6 + GH4*(ZI4+ZI5)
C*********************************************************************
C    PACTAHOBKA B G12 à J12  èPOBOÑàMOCTEâ à àCTOóHàKOB:
C
          JX(1) = J1
          JX(2) = J2
          JX(3) = J3
          JX(4) = J4
          JX(5) = J5
          JX(6) = J6
C----------
          GX(1,1) = G1
          GX(1,2) = G2
          GX(1,3) = G2
          GX(1,4) = G50
          GX(1,5) = G5P
          GX(1,6) = G5M
C$$$$$$$$$
          GX(2,1) = G2
          GX(2,2) = G1
          GX(2,3) = G2
          GX(2,4) = G5M
          GX(2,5) = G50
          GX(2,6) = G5P
C$$$$$$$$$$
          GX(3,1) = G2
          GX(3,2) = G2
          GX(3,3) = G1
          GX(3,4) = G5P
          GX(3,5) = G5M
          GX(3,6) = G50
C$$$$$$$$$$$
          GX(4,4) = G3
          GX(4,5) = G4
          GX(4,6) = G4
C$$$$$$$$$$
          GX(5,4) = G4
          GX(5,5) = G3
          GX(5,6) = G4
C$$$$$$$$$$$$
          GX(6,4) = G4
          GX(6,5) = G4
          GX(6,6) = G3
C$$$$$$$$$$$$$
          DO 11 KX = 4,6
          DO 11 KY = 1,3
          GX( KX , KY ) = GX( KY , KX )
11        CONTINUE
C********
C    îOPMàPOBAHàE PAáBEPHìTOâ MATPàñõ BKãAÑOB ( 12 HA 12 )
          CALL MOT005 ( GX, G12, JX, J12 )
           RETURN
C----------DEBUG INIT
           END
