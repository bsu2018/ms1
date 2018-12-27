       SUBROUTINE MOTOR1( H0,GAMMA,PARM,A1,A2,A3,A4,A5   )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA  MOTOR1( H0,GAMMA,PARM,A1,A2,A4,A4    )
C   ------------
C   HAáHAóEHàE:  1) HACTPOâKA MAKPOMOÑEãà ùãEKTPàóECKOâ  MAòàHõ 3X3X0:
C   ----------      ***************************************************
C                    çÄïéÜÑÖçàÖ A1, A2 ,A3 , ATA4
C
CZZZ-------------------------------------------------------------------
       DIMENSION PARM(*), A1(*), A2(*), A3(*), A4(*),A5(*)
       REAL*8  B(6),BOB(6),C(16)
       REAL*8  FK,SQRT3,PI23,GAMMAD
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
c          write(*,*) parm(1),parm(2),parm(3),parm(4)
c          write(*,*) parm(5),parm(6),parm(7),parm(8)
c          write(*,*) parm(9),parm(10),parm(11),parm(12)
c          write(*,*) parm(13),parm(14),parm(15)
          LRD   = LD + H0 * RD
          LRQ   = LQ + H0 * RQ
C--------------------------------------------------------------------
C Çõóàëãàå åÄíêàñì Ä1
C--------------------------------------------------------------------
           A1(1)  =  FK * MD / LRD
           A1(2)  =  - H0 * OMEGA * A1(1)
           A1(3)  =  MR / LRD
           A1(5)  =  FK * MQ / LRQ
           A1(4)  =  H0 * OMEGA * A1(5)
c           pause
C--------------------------------------------------------------------
C Çõóàëãàå åÄíêàñì Ä2
C--------------------------------------------------------------------
           A2(1)  =  A1(1)
           A2(2)  =  A1(3)
           A2(3)  =  A1(5)
C--------------------------------------------------------------------
C Çõóàëãàå åÄíêàñì Ä3
C--------------------------------------------------------------------
           A3(1)  =  LD / LRD
           A3(2)  =  LQ / LRQ
C--------------------------------------------------------------------
C Çõóàëãàå åÄíêàñì Ä4 Ç ëíèÖèÖçà (-1)
C--------------------------------------------------------------------
          CC   = 1. / (L0 + H0 * R )
          B(1)   = LSD + H0 * R - FK*MD*A1(1) - (H0*OMEGA*
     =  FK*MQ) * A1(4)
          B(2)   = H0*OMEGA*(LSQ -LSD) + (H0*OMEGA*FK*MD)*
     =  A1(1) - (FK*MQ*OMEGA*H0)*A1(5)
          B(3)   = FK*MF - MR*A1(1)
          B(4)   = LSQ + H0*R - (H0*OMEGA*FK*MD)*H0*OMEGA*A1(1) -
     =  FK*MQ*A1(5)
          B(5)   = - (FK*MF*OMEGA*H0)  + (H0*OMEGA*FK*MD)*A1(3)
          B(6)   = LF + H0*RF - MR*A1(3)
c           write(*,*) 'b(i)',b(1),b(2),b(3),b(4)
c           write(*,*) 'b(i)',b(5),b(6),'c=',c
C------------éÅêÄôÄÖå åÄíêàñì
         CALL MMMS3( B, BOB)
c           write(*,*) 'bob(i)',bob(1),bob(2),bob(3),bob(4)
c           write(*,*) 'bob(i)',bob(5),bob(6)
C---------- áÄèéåçàå
          GAMMAD = GAMMA
          C(1) =  CC / SQRT3
          C(2) = (BOB(1)*DCOS(GAMMAD) + BOB(2)*DSIN(GAMMAD) ) / FK
          C(3) = (BOB(2)*DCOS(GAMMAD) + BOB(4)*DSIN(GAMMAD) ) / FK
          C(4) = (BOB(3)*DCOS(GAMMAD) + BOB(5)*DSIN(GAMMAD) ) / FK
          C(5) = C(1)
          C(6) = (BOB(1)*DCOS(GAMMAD -PI23) + BOB(2)*
     X    DSIN(GAMMAD -PI23) ) / FK
          C(7) = (BOB(2)*DCOS(GAMMAD -PI23) + BOB(4)*
     X    DSIN(GAMMAD -PI23) ) / FK
          C(8) = (BOB(3)*DCOS(GAMMAD -PI23) + BOB(5)*
     X    DSIN(GAMMAD -PI23) ) / FK
          C(9) = C(1)
          C(10) = (BOB(1)*DCOS(GAMMAD +PI23) + BOB(2)*
     X    DSIN(GAMMAD +PI23) ) / FK
          C(11) = (BOB(2)*DCOS(GAMMAD +PI23) + BOB(4)*
     X    DSIN(GAMMAD +PI23) ) / FK
          C(12) = (BOB(3)*DCOS(GAMMAD +PI23) + BOB(5)*
     X    DSIN(GAMMAD +PI23) ) / FK
          C(13) = 0.
          C(14) = BOB(3)
          C(15) = BOB(5)
          C(16) = BOB(6)
c----ÇÎÁ®·´®¨ ¨†‚‡®Ê„ Ø‡Æ¢Æ§®¨Æ·‚® ·®≠Â‡Æ≠≠Æ© ¨†Ë®≠Î 
C--------------------------------------------------------------------
       A5(1) = H0*(C(1) / SQRT3 + (C(2)*DCOS(GAMMAD) +
     = C(3)*DSIN(GAMMAD)) /FK)
       A5(2) = H0*(C(1) / SQRT3 + (C(2)*DCOS(GAMMAD -PI23) +
     = C(3)*DSIN(GAMMAD - PI23)) /FK )
       A5(3) = H0*(C(1) / SQRT3 + (C(2)*DCOS(GAMMAD + PI23) +
     = C(3)*DSIN(GAMMAD + PI23)) /FK )
       A5(4) = H0*C(4)
C--------------------------------------------------------------------
       A5(5) = A5(2)
       A5(6) = H0*(C(5) / SQRT3 + (C(6)*DCOS(GAMMAD -PI23) +
     = C(7)*DSIN(GAMMAD - PI23)) /FK)
       A5(7) = H0*(C(5) / SQRT3 + (C(6)*DCOS(GAMMAD + PI23) +
     = C(7)*DSIN(GAMMAD + PI23)) /FK)
       A5(8) = H0*C(8)
C--------------------------------------------------------------------
       A5(9) = A5(3)
       A5(10) = A5(7)
       A5(10) = H0*(C(9) / SQRT3 + (C(10)*DCOS(GAMMAD - PI23) +
     = C(11)*DSIN(GAMMAD - PI23)) /FK )
       A5(11) = H0*(C(9) / SQRT3 + (C(10)*DCOS(GAMMAD + PI23) +
     = C(11)*DSIN(GAMMAD + PI23)) /FK )
       A5(12) = H0*C(12)
C--------------------------------------------------------------------
       A5(13) = A5(4)
       A5(14) = A5(8)
       A5(15) = A5(12)
       A5(16) =  H0*C(16)
C----------------------------------------------------------------------
       DO 111  I = 1,16
       A4(I)  = C(I)
111    CONTINUE       
c         write(*,*) a5(1),a5(2),a5(3),a5(4)
c         write(*,*) a5(5),a5(6),a5(7),a5(8)
c         write(*,*) a5(9),a5(10),a5(11),a5(12)
c         write(*,*) a5(13),a5(14),a5(15),a5(16)
C--------------------------------------------------------------------
C áÄèéåçàå åÄíêàñì Ä1
C--------------------------------------------------------------------
         A11   = ( A1(1)*COS(GAMMA) +A1(2)*SIN(GAMMA)) / FK
         A12   = ( A1(1)*COS(GAMMA -PI23) +
     =      A1(2)*SIN(GAMMA - PI23)) / FK
         A13   = ( A1(1)*COS(GAMMA + PI23) +
     =      A1(2)*SIN(GAMMA + PI23)) / FK
         A14   = A1(3)
         A15   = ( A1(4)*COS(GAMMA) +A1(5)*SIN(GAMMA)) / FK
         A16   = ( A1(4)*COS(GAMMA -PI23) +
     =      A1(5)*SIN(GAMMA - PI23)) / FK
         A17   = ( A1(4)*COS(GAMMA + PI23) +
     =      A1(5)*SIN(GAMMA + PI23)) / FK
c          write(*,*) 'a1=',a1(1),a1(2),a1(3),a1(4)
c          write(*,*) a1(5),a1(6),a1(7)
           A1(1) = A11
           A1(2) = A12
           A1(3) = A13
           A1(4) = A14
           A1(5) = A15
           A1(6) = A16
           A1(7) = A17
C--------------------------------------------------------------------
C áÄèéåçàå åÄíêàñì Ä2
C--------------------------------------------------------------------
         A21   =  A2(1)*COS(GAMMA) / FK
         A22   =  A2(1)*COS(GAMMA - PI23) / FK
         A23   =  A2(1)*COS(GAMMA + PI23) / FK
         A24   =  A2(2)
         A25   =  A2(3)*SIN(GAMMA) / FK
         A26   =  A2(3)*SIN(GAMMA - PI23 ) / FK
         A27   =  A2(3)*SIN(GAMMA + PI23 ) / FK
c          write(*,*) 'a2=',a2(1),a2(2),a2(3),a2(4)
c          write(*,*) a2(5),a2(6),a2(7)
c          pause
           A2(1) = A21
           A2(2) = A22
           A2(3) = A23
           A2(4) = A24
           A2(5) = A25
           A2(6) = A26
           A2(7) = A27
C--------------------------------------------------------------------
       RETURN
C......................................................................
       END
