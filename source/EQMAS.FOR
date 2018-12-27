      SUBROUTINE EQMAS(M1,I1,M2,I2,N,KODS)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C    O„POƒPAMMA:  EQMAS(M1,I1,M2,I2,N,KODS)
C    ------------
C      HA‡HA—EHˆE: O„POƒPAMMA CPABHEHˆŸ N-‹EMEHTOB MACCˆBOB M1 ˆ M2,
C      ----------   HA—ˆHAŸ  C ‹EMEHTOB I1 ˆ I2 COOTBETCTBEHHO
C
C      METO„:             O—EBˆ„EH;
C      -----
C      APAMETP›:   M1 - MACCˆB
C      ---------    M2 - MACCˆB
C                   I1 - HOMEP EPBOƒO ‹EMEHTA MACCˆBA M1 C KOTOPOƒO
C                        HA—ˆHAETCŸ CPABHEHˆE
C                   I2 - -//- MACCˆBA M2 -//-
C                   N - KO‹ˆ—ECTBO ‹EMEHTOB CPABHEHˆŸ
C                   KOD - KO„ CPABHEHˆŸ:
C                         KOD=1 CPABHEHˆE “CE˜HO
C                         KOD=0 CPABHEHˆE HE “CE˜HO
C                                         **
C      O„POƒPAMM›:  HET
C      ------------
C
C      PˆME—AHˆE:
C      -----------
CZZZ-------------------------------------------------------------------
      DIMENSION M1(1),M2(1)
      KODS=1
      DO 1 K=1,N
      K1=K-1
      IF (M1(I1+K1).EQ.M2(I2+K1)) GO TO 1
      KODS=0
      GO TO 2
    1 CONTINUE
    2 RETURN
      END
