       SUBROUTINE SEQMAX( EYY, IK, IA, EYYV, EYYI )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: SEQMAX( EYY, IK, IA, EYYV, EYYI )
C   ------------
C   HA‡HA—EHˆE:        OƒPAHˆ—ˆBAET ‡HA—EHˆŸ EYY - AM‹ˆT“„›
C   ----------         “PAB‹Ÿž™EEƒO CˆƒHA‹A:
C
C                      "A" ƒP“A B›PŸM‹ŸET:
C                       =====================
C                        Pˆ IA > 0  ˆ EYY > 0 --> OƒPAHˆ—EHˆE "CBEPX“"
C                              EC‹ˆ EYY > EYYV, TO EYY=EYYV;
C
C
C                      "K" ƒP“A B›PŸM‹ŸET:
C                       =====================
C                        Pˆ IK > 0  ˆ EYY < 0 --> OƒPAHˆ—EHˆE "CBEPX“"
C                            EC‹ˆ  -EYY > EYYV, TO EYY=-EYYV;
C
C
C                      "A" ƒP“A ˆHBEPTˆP“ET:
C                       =====================
C                        Pˆ IA > 0  ˆ EYY < 0 --> OƒPAHˆ—EHˆE "CHˆ‡“"
C                              EC‹ˆ -EYY > EYYI, TO EYY=-EYYI;
C
C
C                      "K" ƒP“A ˆHBEPTˆP“ET:
C                       =====================
C                        Pˆ IK > 0  ˆ EYY > 0 --> OƒPAHˆ—EHˆE "CHˆ‡“"
C                              EC‹ˆ EYY > EYYI, TO EYY=EYYI;
C
C   METO„:             “CTAHOBˆTœ PE†ˆM ˆ ˆ‡MEHˆTœ EYY
C   ------             COƒ‹ACHO EYYV ˆ‹ˆ EYYI
C
C   APAMETP›:         EYY - ˆ‡MEHŸEMOE HAPŸ†EHˆE “PAB‹EHˆŸ,
C   ---------          IK - TOK "K" ƒP“›,
C                      IA - TOK "A" ƒP“›,
C                      EYYV - OƒPAHˆ—EHˆE "CBEPX“" (B›PŸM‹EHˆE),
C                      EYYI - OƒPAHˆ—EHˆE "CHˆ‡“"  (ˆHBEPTˆPOBAHˆE).
C
C   O„POƒPAMM›:   HET
C   -------------
C   PˆME—AHˆE:
C   -----------
C
CZZZ-------------------------------------------------------------------
       REAL IA, IK
C
       IF ( IA .GT. 0.0 .AND. EYY .GT. 0.0 .AND. EYY .GT. EYYV )
     *            EYY = EYYV
       IF ( IA .GT. 0.0 .AND. EYY .LT. 0.0 .AND. -EYY .GT. EYYI )
     *            EYY = -EYYI
       IF ( IK .GT. 0.0 .AND. EYY .LT. 0.0 .AND. -EYY .GT. EYYV )
     *            EYY = -EYYV
       IF ( IK .GT. 0.0 .AND. EYY .GT. 0.0 .AND. EYY .GT. EYYI )
     *            EYY = EYYI
       RETURN
       END
