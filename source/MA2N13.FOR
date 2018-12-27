      SUBROUTINE MA2N13 (PARD, PARM,    OMEGA, GAMMA, TOK, UL, PARJ0)
CXXXXX++KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)++++++++++
C
C   O„POƒPAMMA: MA2N13 (PARD, PARM,    OMEGA, GAMMA, TOK, UL, PARJ0)
C   ------------
C
C   DEFINE ARRAY PARJ0  FOR DATA MATRIX J0
C   #_13:  STATOR_3, ROTOR_3, DEMPF_0  G * 31.01.86 *
C  PARJ0==> J0[1], J0[2], ..... J0[KALL]
C                                            D(L)
C     PARJ0 =  L**(-1) * [ UL - (R + OMEGA * ----  ) * TOK ]
C                                            D(GAM)
C
C   METO„:  AHA‹ˆTˆ—ECKAŸ ‡AˆCœ ”OPM“‹›;  P “ — H A Ÿ +!!!!!!!!!!
C   ------
C   APAMETP›: CM. FM0008, DM0007
C  -----------
C  O„POƒPAMM›:  HET;
C  ------------
C  PˆME—AHˆE:
C  ----------
C
C
CZZZZZ---------------------------------------------------------------
       REAL    I1, I2,  I3, I4, I5, I6
       DIMENSION PARD(*), PARM(*)
C--------------------------------------------------------------------
       REAL LS,  MS,  LR,  MR,  MSR, LMSP, LMSM, LMRM, LMRP, MSR2D3
       REAL LS1, MS1, LR1, MR1, MSR1, LM0, LMP, LMM
       DIMENSION TOK(*), UL(*), PARJ0(*)
          DATA R120 /  2.09440    /
          DATA SQRT3/1.732050808/
                      RS   = PARD(1)
                      RR   = PARD(2)
                      LS   = PARD(3)
                      MS   = PARD(4)
                      LR   = PARD(5)
                      MR   = PARD(6)
                      MSR  = PARD(7)
C****                 PTAU = PARD(8)
C****                 JMOM = PARD(9)
C----------------------------------------------------------------------
C--->  MACCˆB PARM......
                     LS1   = PARM(1)
                     MS1   = PARM(2)
                     LR1   = PARM(3)
                     MR1   = PARM(4)
                     MSR1  = PARM(5)
C------------------  LMSP  = PARM(6)
C------------------  LMSM  = PARM(7)
C------------------  MSR2D3  = PARM(8)
C                    PTAU / JMOM   = PARM(9)
C----  LMRM = LR - MR
C----  LMRP = LR + MR
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C****** OPE„E‹EHˆE KO”. „‹Ÿ A ˆ LM **********************************
       AM0 = MSR * SIN ( GAMMA )
       AMP = MSR * SIN ( GAMMA + R120 )
       AMM = MSR * SIN ( GAMMA - R120 )
C-------
       LM0 = MSR1 * COS ( GAMMA )
       LMP = MSR1 * COS ( GAMMA + R120 )
       LMM = MSR1 * COS ( GAMMA - R120 )
C-------
       A0 = - OMEGA * AM0
       AP = - OMEGA * AMP
       AM = - OMEGA * AMM
C----------------------------------------------------------------------
C--------------------------------------------------------------------
      I1   = TOK(1 )
      I2   = TOK(2 )
      I3   = TOK(3 )
      I4   = TOK(4 )
      I5   = TOK(5 )
      I6   = TOK(6 )
C---------------------------------------------------------------------
      U1   = UL(1 )
      U2   = UL(2 )
      U3   = UL(3 )
      U4   = UL(4 )
      U5   = UL(5 )
      U6   = UL(6 )
C--------------------------------------------------------------------
C
C------ [AI] = [U] - [A] * [I]
       AI1 = U1 - ( RS*I1 + A0*I4 + AP*I5 + AM*I6 )
       AI2 = U2 - ( RS*I2 + AM*I4 + A0*I5 + AP*I6 )
       AI3 = U3 - ( RS*I3 + AP*I4 + AM*I5 + A0*I6 )
       AI4 = U4 - ( A0*I1 + AM*I2 + AP*I3 + RR*I4 )
       AI5 = U5 - ( AP*I1 + A0*I2 + AM*I3 + RR*I5 )
       AI6 = U6 - ( AM*I1 + AP*I2 + A0*I3 + RR*I6 )
C------
C  J0-->     = [LM] * ( [U] - [A] * [I] )
C------
       PARJ0(1) = LS1*AI1 + MS1*(AI2+AI3) + LM0*AI4 + LMP*AI5 + LMM*AI6
       PARJ0(2) = LS1*AI2 + MS1*(AI1+AI3) + LMM*AI4 + LM0*AI5 + LMP*AI6
       PARJ0(3) = LS1*AI3 + MS1*(AI1+AI2) + LMP*AI4 + LMM*AI5 + LM0*AI6
       PARJ0(4) = LM0*AI1 + LMM*AI2 + LMP*AI3 + LR1*AI4 + MR1*(AI5+AI6)
       PARJ0(5) = LMP*AI1 + LM0*AI2 + LMM*AI3 + LR1*AI5 + MR1*(AI4+AI6)
       PARJ0(6) = LMM*AI1 + LMP*AI2 + LM0*AI3 + LR1*AI6 + MR1*(AI4+AI5)
C------
       RETURN
      END
