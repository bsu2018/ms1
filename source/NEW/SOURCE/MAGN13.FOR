      SUBROUTINE MAGN13 (PARD, PARM, H, OMEGA, GAMMA, PARGY       )
CXXXX+++KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)++++++++++
C     P ì ó H O â    B A P à A H T ******
C   èOÑèPOÉPAMMA: MAGN13( PARD, PARM, H, OMEGA, GAMMA, PARGY        )
C  -------------
C   HAáHAóEHàE:  BõóàCãEHàE MATPàñ GY      Ñãü ùã. MAòàHõ 3X3X0
C   -----------                KAK H  *  L(GAMMA)**(-1)
C
C               MOTOR MODEL: #_13  T__3_3_0__G                       !
C   PARGY ===> GY[1,1]...GY[KALL,1],   GY[1,2],...,GY[KALL,2]......
C         ===> GY[1,3]...GY[KALL,3],...GY[1,KALL],...,GY[KALL,KALL]
C
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
C              PARGY        - BõóàCãEHHõE MATPàñõ;
C
C   èOÑèPOÉPAMMõ: HET;
C   ------------
C   èPàMEóAHàE:  1.  PARGY          -  áAèàCAHõ èO "CTOãÅñAM";
C   ----------
CZZZZZ---------------------------------------------------------------
        REAL LS,  MS,  LR,  MR,  MSR, LMSP, LMSM, LMRM, LMRP, MSR2D3
        REAL LS1, MS1, LR1, MR1, MSR1, LM0, LMP, LMM, LBIG, L0, LM, LP
C--------------------------------------------------------------------
        DIMENSION PARM(*), LMBIG(6,6)
        DIMENSION PARD(*)
        DIMENSION PARGY(*)
        REAL LMBIG, L01, LM1, LP1
C
          DATA SQRT3/1.732050808/
          DATA R120 / 2.09440  /
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
C********************LMSP  = PARM(6)
C********************LMSM  = PARM(7)
C********************MSR2D3  = PARM(8)
C                    PTAU / JMOM   = PARM(9)
C******LMRM = LR - MR
C******LMRP = LR + MR
       CLM0 = COS(GAMMA)
       CLMP = COS(GAMMA + R120)
       CLMM = COS(GAMMA - R120)
C------
       L01 = MSR1 * CLM0
       LP1 = MSR1 * CLMP
       LM1 = MSR1 * CLMM
C------
C**********************************************************************
C-->   LMBIG MATPàñA
          LMBIG(1,1) = LS1*H
          LMBIG(1,2) = MS1*H
          LMBIG(1,3) = MS1*H
          LMBIG(1,4) = L01*H
          LMBIG(1,5) = LP1*H
          LMBIG(1,6) = LM1*H
C$$$$$$$$$                *H
          LMBIG(2,1) = MS1*H
          LMBIG(2,2) = LS1*H
          LMBIG(2,3) = MS1*H
          LMBIG(2,4) = LM1*H
          LMBIG(2,5) = L01*H
          LMBIG(2,6) = LP1*H
C$$$$$$$$$$               *H
          LMBIG(3,1) = MS1*H
          LMBIG(3,2) = MS1*H
          LMBIG(3,3) = LS1*H
          LMBIG(3,4) = LP1*H
          LMBIG(3,5) = LM1*H
          LMBIG(3,6) = L01*H
C$$$$$$$$$$$              *H
          LMBIG(4,4) = LR1*H
          LMBIG(4,5) = MR1*H
          LMBIG(4,6) = MR1*H
C$$$$$$$$$$               *H
          LMBIG(5,4) = MR1*H
          LMBIG(5,5) = LR1*H
          LMBIG(5,6) = MR1*H
C$$$$$$$$$$$$             *H
          LMBIG(6,4) = MR1*H
          LMBIG(6,5) = MR1*H
          LMBIG(6,6) = LR1*H
C$$$$$$$$$$$$$
          DO 1100 KX = 4,6
          DO 1100 KY = 1,3
          LMBIG( KX , KY ) = LMBIG( KY , KX )
1100      CONTINUE
C**********************************************************************
C
C----------------------------------------------------------------------
C-->  áAèàCATú B PARGY             CTOãÅñAM...
C
       JXX = 1
       DO 777 I=1,6
       DO 777 J=1,6
C
       PARGY(JXX) = LMBIG( J, I )
C                        *******
        JXX = JXX + 1
777    CONTINUE
         RETURN
         END
