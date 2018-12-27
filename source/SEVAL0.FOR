       FUNCTION SEVAL0(KOD,XT,XYMAS)
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA_îìHKñàü: SEVAL0(KOD,XT,XYMAS)
C   ---------------------
C   HAáHAóEHàE: èO ÑAHHõM, èOãìóEHHõM B PEáìãúTATE PAÅOTõ è/è SPLIN0
C  ------------ BõóàCãüET CãEÑ. áHAóEHàü Ñãü Y = F(X):
C              KOÑ       áHAóEHàE SEVAL0
C              1           F(XT)
C
C              2           èEPBAü èPOàáBOÑHAü B TOóKE X = XT
C
C              3           BTOPAü èPOàáBOÑHAü B TOóKE X = XT
C
C              4           TPETúü èPOàáBOÑHAü B TOóKE X = XT
C
C          BõÅOP àHTEPABãA:  èPà  XT .LT. X(1) --->  àHTEPBAã ÑO I=1;
C                            èPà  XT .GE. X(N) --->  àHTEPBAã èOCãE I=N;
C   METOÑ:   CM. è/è SPLIN0;
C   -----
C   èAPAMETPõ:  XT - TOóKA èO OCà X OèPEÑEãEHàü SEVAL0 COÉãACHO KOD;
C   ---------   XYMAS - MACCàB OèàCAHàü CèãAâHA -> CM. è/è SPLIN0;
C   èOÑèPOÉPAMMõ: SUBERR;
C   ------------
C   èPàMEóAHàE:  HET
C   ----------
C
CZZZ-------------------------------------------------------------------
       EQUIVALENCE( RN, N )
       DIMENSION XYMAS(1)
       INTEGER SB, SC,SD
       DATA I/1/
C      X(I) = XYMAS(2*I)
C      Y(I) = XYMAS(2*I+1)
C      B(I) = XYMAS(SB+I)
C      C(I) = XYMAS(SC+I)
C      D(I) = XYMAS(SD+I)
         RN =     XYMAS(1)
       SB = 2*N+1
       SC = 3*N+1
       SD = 4*N+1
          IF(I.GE.N) I = 1
               IP1 = I + 1
          IF(XT.LT.XYMAS(2*I) )  GO TO 10
          IF(XT.LE.XYMAS(2*IP1) )  GO TO 30
C**********************************************************************
   10  I = 1
       J = N+1
   20  K = (I+J)/2
          IF(XT.LT.XYMAS(2*K)) J = K
          IF(XT.GE.XYMAS(2*K)) I = K
          IF(J.GT.(I+1)) GO TO 20
   30  CONTINUE
C**********************************************************************
       GO TO (1,2,3,4), KOD
        CALL SUBERR(KOD)
    1  CONTINUE
C**********************************************************************
       DX = XT - XYMAS(2*I)
       SEVAL0 = XYMAS(1+2*I)+DX*(XYMAS(SB+I)
     =          +DX*(XYMAS(SC+I)+DX*XYMAS(SD+I)))
          RETURN
    2  CONTINUE
C**********************************************************************
       DX = XT - XYMAS(2*I)
       SEVAL0 = XYMAS(SB+I)+DX*(2*XYMAS(SC+I)+3*DX*XYMAS(SD+I))
       RETURN
    3  CONTINUE
C**********************************************************************
C                      BTOPAü èPOàáBOÑHAü
       DX = XT - XYMAS(2*I)
       SEVAL0 = 2*XYMAS(SC+I)+6*DX*XYMAS(SD+I)
       RETURN
    4  CONTINUE
C**********************************************************************
       SEVAL0 = 6*XYMAS(SD+I)
       RETURN
       END
