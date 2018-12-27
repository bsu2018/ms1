      REAL FUNCTION  MA3N13 (PARD,       GAMMA, TOK )
CXXXXX++KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)++++++++++
C  èOÑèPOÉPAMMA îìHKñàü:   MA3N13( PARD,       GAMMA, TOK )
C  ---------------------
C   HAáHAóEHàE : BõóàCãEHàE ùã. MAÉ. MOMEHTA ùã. MAòàHõ
C   ----------     ***  MOÑEãú  3X3X0 *****
C   #_13:  STATOR_3, ROTOR_3, DEMPF_0  G * 31.01.86 *
C
C                                 T      D L(GAMMA)
C                 MFT = -1/2 * TOK    *  -----------  * TOK
C                                        D GAMMA
C
C   METOÑ:      AHAãàTàóECKAü îOPMìãA.-->  P ì ó H A ü  *****
C   ------
C   èAPAMETPõ:  CM. DM0007, FM0008
C   ---------
C   èOÑèPOÉPAMMõ:  HET
C   -------------
C   èPàMEóAHàE:    HET
C   -----------
CZZZZZ---------------------------------------------------------------
          DIMENSION PARD(*)
          DIMENSION TOK(*)
CVVVVVVVV REAL MA3N13
        REAL MF
        REAL                     MSR
        REAL    I1, I2,  I3, I4, I5, I6
        DATA R120 /  2.09440    /
          DATA SQRT3/1.732050808/
C--------------------------------------------------------------------
C
C**                   RS   = PARD(1)
C**                   RR   = PARD(2)
C**                   LS   = PARD(3)
C**                   MS   = PARD(4)
C**                   LR   = PARD(5)
C**                   MR   = PARD(6)
                      MSR  = PARD(7)
                      PTAU = PARD(8)
C****                 JMOM = PARD(9)
C----------------------------------------------------------------------
C****** OèPEÑEãEHàE KOùî. Ñãü A à LM **********************************
       AM0 = MSR * SIN ( GAMMA )
       AMP = MSR * SIN ( GAMMA + R120 )
       AMM = MSR * SIN ( GAMMA - R120 )
C-------
C--------------------------------------------------------------------
      I1   = TOK(1 )
      I2   = TOK(2 )
      I3   = TOK(3 )
      I4   = TOK(4 )
      I5   = TOK(5 )
      I6   = TOK(6 )
C---------------------------------------------------------------------
C------
C   PACóET MF B TN
C------
       MF = ( I1*I4 + I2*I5 + I3*I6 ) * AM0 +
     *      ( I1*I5 + I2*I6 + I3*I4 ) * AMP +
     *      ( I1*I6 + I2*I4 + I3*I5 ) * AMM
       MA3N13 = MF * PTAU
       RETURN
       END
