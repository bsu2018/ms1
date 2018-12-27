       SUBROUTINE MOTOR3( H0,GAMMA,PARM,A,B    )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA  MOTOR3( H0,GAMMA,PARM,A,B    )
C   ------------
C   HAáHAóEHàE:  1) HACTPOâKA MAKPOMOÑEãà ùãEKTPàóECKOâ  MAòàHõ 3X3X0:
C   ----------      ***************************************************
C                    çÄïéÜÑÖçàÖ A5
C
CZZZ-------------------------------------------------------------------
       DIMENSION PARM(*), A(*), B(*)
       REAL L0, LSD, LSQ, LF, LD, LQ, MF, MD, MQ, MR, R,
     =      RF, RD, RQ, OMEGA,LRD,LRQ
C----------------------------------------------------------------------
C
       DATA FK/1.2247448 / , SQRT3/1.7320508/
       DATA PI23/2.094395/
C
C--------------------------------------------------------------------
          L0     = PARM(1)
          LSD    = PARM(2)
          LSQ    = PARM(3)
          LF     = PARM(4)
          LD     = PARM(5)
          LQ     = PARM(6)
          MF     = PARM(7)
          MD     = PARM(8)
          MQ     = PARM(9)
          MR     = PARM(10)
          R      = PARM(11)
          RF     = PARM(12)
          RD     = PARM(13)
          RQ     = PARM(14)
          OMEGA  = PARM(15)
C--------------------------------------------------------------------
          LRD   = LD + H0 * RD
          LRQ   = LQ + H0 * RQ
C--------------------------------------------------------------------
C Çõóàëãàå åÄíêàñì Ä5
C--------------------------------------------------------------------
          C1  = L0
          C2  = LSD - (FK*MD)*(FK*MD)/ LRD
          C3  = - H0*OMEGA*(FK*MQ)*(FK*MQ)/ LRQ
          C4  = FK*MF - FK*MD*MR / LRD
          C5  = H0*OMEGA*(FK*MD)*(FK*MD)/ LRD
          C6  = LSQ - (FK*MQ)*(FK*MQ)/ LRQ
          C7  = H0*OMEGA*FK*MD*MR / LRD
          C8  = C4
          C9  = LF - MR*MR / LRD
c           write(*,*) 'A5'
c           write(*,*) c1
c           write(*,*) c2,c3,c4
c           write(*,*) c5,c6,c7
c           write(*,*) c8,c9
C--------------------------------------------------------------------
C Çõóàëãàå åÄíêàñì D
C--------------------------------------------------------------------
          D1  = C1 /SQRT3
          D2  = D1
          D3  = D1
          D4  = 0.
          D5  = (C2*COS(GAMMA) + C3*SIN(GAMMA)) / FK
          D6  = (C2*COS(GAMMA -PI23) + C3*SIN(GAMMA -PI23)) / FK
          D7  = (C2*COS(GAMMA +PI23) + C3*SIN(GAMMA +PI23)) / FK
          D8  = C4
          D9  = (C5*COS(GAMMA) + C6*SIN(GAMMA)) / FK
          D10 = (C5*COS(GAMMA -PI23) + C6*SIN(GAMMA -PI23)) / FK
          D11 = (C5*COS(GAMMA +PI23) + C6*SIN(GAMMA +PI23)) / FK
          D12 = C7
          D13 = C8*COS(GAMMA) / FK
          D14 = C8*COS(GAMMA -PI23) / FK
          D15 = C8*COS(GAMMA +PI23) / FK
          D16 = C9
C---> áÄèàòÖå êÖáìãúíÄí
          B(1) = A(1)*D1 +A(2)*D5 +A(3)*D9 +A(4)*D13
          B(2) = A(1)*D2 +A(2)*D6 +A(3)*D10 +A(4)*D14
          B(3) = A(1)*D3 +A(2)*D7 +A(3)*D11 +A(4)*D15
          B(4) = A(1)*D4 +A(2)*D8 +A(3)*D12 +A(4)*D16
C--------------------------------
          B(5) = A(5)*D1 +A(6)*D5 +A(7)*D9 +A(8)*D13
          B(6) = A(5)*D2 +A(6)*D6 +A(7)*D10 +A(8)*D14
          B(7) = A(5)*D3 +A(6)*D7 +A(7)*D11 +A(8)*D15
          B(8) = A(5)*D4 +A(6)*D8 +A(7)*D12 +A(8)*D16
C------------------------------
          B(9) = A(9)*D1 +A(10)*D5 +A(11)*D9 +A(12)*D13
          B(10) = A(9)*D2 +A(10)*D6 +A(11)*D10 +A(12)*D14
          B(11) = A(9)*D3 +A(10)*D7 +A(11)*D11 +A(12)*D15
          B(12) = A(9)*D4 +A(10)*D8 +A(11)*D12 +A(12)*D16
C---------------------------------
          B(13) = A(14)*D5 +A(15)*D9 +A(16)*D13
          B(14) = A(14)*D6 +A(15)*D10 +A(16)*D14
          B(15) = A(14)*D7 +A(15)*D11 +A(16)*D15
          B(16) = A(14)*D8 +A(15)*D12 +A(16)*D16
C----------------------------------------------------------------------
       RETURN
C......................................................................
       END
