          SUBROUTINE FM0014( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C
C   èOÑèPOÉPAMMA:  FM0014( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HAáHAóEHàE:    HACTPOâKA MAKPOMOÑEãà:
C  ------------
C                  Tàè     àMü MOÑEãà     OÅOáHAóEHàü B ÑAãúHEâòEM
C
C                  64      3-X îAáHAü ù.Ñ.C        E-PEÉ(1)
C                          PEÉìãàPìEMAü OT "PB"
C
C
C                  65      3-X îAáHAü ù.Ñ.C.,
C                          ìèPABãüEMAü BHEòHàM     E-PEÉ(2)
C                          CàÉHAãOM .
C
C
C              ùãEMEHT KOÑàPìETCü:
C              -------------------
C
C              ù # T 64 ì K0 KA KB KC è T-BKã  R-BHìT #-ùã-TA  K-ìCàã;
C
C              ù # T 65 ì K0 KA KB KC è R-BHìT L-BHìT K-ìCàã  W  FI
C                                       #-ùã-TA  #-CMEô.
C
C     K0 - OÅôàâ ìáEã, KA - îAáA A, KB - îAáA B, KC - îAáA C;
C
C---> BõBOÑHõE áHAóEHàü: TOKà: K ìáãAM KA, KB, KC, OT ìáãA K0,
C                              *                   **
C                      áHAóEHàü ù.Ñ.C. E1, E2, E3;
C
C
C !!!!   áAÑEPÜKA HA OÑàH òAÉ àHTEÉPàPOBAHàü !!!
C        -----------------------------------
C
C
C                  èPOàáBOÑàT îOPMàPOBAHàE "èEPEMEHHOâ" óACTà MSW
C                  MAKPOMOÑEãEâ ìKAáAHHõX TàèOB.
C
C   METOÑ:         BõóàCãüETCü à áAèàCõBAETCü B MSW BCü HEOÅXOÑàMAü
C   -----          àHîOPMAñàü, HEOÅXOÑàMAü Ñãü èOCãEÑìûôEÉO
C                  PACóETA MAKPOMOÑEãà. AÑAèTàPOBAHO K METOÑì
C                  ãàHàÑÜEPA - BàOãìÉÅà.
C
C   èAPAMETPõ:     BA0T, KNOTT, PAR0T - "OTPEáKà" àHîOPMAñàOHHõX
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 à GETMSW );
C                  RMSW - "èEPEMEHHAü" óACTú MSW ( "OTPEáOK" MSWALL );
C                  LM - ÑãàHA "èEPEMEHHOâ" óACTà RMSW - èOãü SPM à SRAB;
C
C   èOÑèPOÉPAMMõ:  1.COMMON PROF1
C   ------------
C
C
C   èPàMEóAHàE:    1. KOHTPOãú HE èPOàáBOÑàTCü;
C   -----------    2. BCE HE OèPEÑEãüEMõE èOãü RMSW áAHìãüûTCü;
C                  3. HàÜE èPàBOÑüTCü îOPMATõ RMSW:
C
C
C
C
C                  RMSW TàèA # 64 [ àCTOóHàK HAèPüÜEHàü OT "PEÉ.BOáÅ" ]
C                  -----------------------------------------
C
C                    TOK(J) = K_ìC. * E(J) / R_BHìT. + 1/R * U_N+1
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+---+---+-----+
C             II áHAó.  I áHAó.I # ùã-TAI AÑP. II      IJESIJESI K   I
C   ìáãõ:     II  1/R   I T_BKãI "PB"   I  B   II JESA I   I   I ìCàãI
C  K0         II        I      I        I TOKN II      I B I C I EHàüI
C    K1       ++--------+------+--------+------++------+---+---+-----+
C      K2     II   1    I  2   I   3    I  4   II  5   I 6 I 7 I  8  I
C         K3  ++--------+------+--------+------++------+---+---+-----+
C  9 10 11 12 II  13    I  14  I  15    I  16  II  17  I18 I19 I 20  I
C             ++--------+------+--------+------++------+---+---+-----+
C
C
C
C                         RMSW TàèA # 65
C                         --------------
C
C                             SPM                 SRAB
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C            II #   I #   IAÑP.I   I   I K I   I   I   I    I    I    I
C  ìáãõ:     IIùã-TAICMEô.I B  I R I L IìCàI W I FII G IJESAIJESBIJESCI
C K0         II     I     ITOKNI   I   IãEHI   I   I   I    I    I    I
C   K1       ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C     K2     II  1  I  2  I  3 I 4 I 5 I 6 I 7 I 8 I 9 I 10 I 11 I 12 I
C        K3  ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C 9 10 11 12 II 13  I 14  I 15 I16 I17 I18 I19 I20 I21 I 22 I 23 I 24 I
C            ++-----+-----+----+---+---+---+---+---+---+----+----+----+
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         EQUIVALENCE ( N, RN )
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 64 )   GO TO 63
         IF (TIP .EQ. 65 )   GO TO 65
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ãOÉ. CÅOâ B FM0014: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
63     CONTINUE
       LM = 8
C----------------------------------------------------------------------
C---> SIN àCT. HAèP.
         R = PAR0T(2)
         IF ( R .GT. 0.0 ) GO TO 61
         WRITE (NWTR,505) NEL
         R = 1.0E-32
C--> áAèOMàHAEM
61       RMSW(1) = 1.0 / R
         RMSW(2) = PAR0T(1)
C--> HOMEP  ùã-TA
         N = IFIX( PAR0T(3) )
         RMSW(3) = RN
         RMSW(4) = 0.0
         RMSW(5) = 0.0
         RMSW(6) = 0.0
         RMSW(7) = 0.0
         RMSW(8) = PAR0T(4)
C---> ÉOÑàTCü !
         RETURN
C######################################################################
C---> SIN àCT. HAèP.
65     CONTINUE
       LM = 12
C--> OÅHìãàM .
       DO 67 I=1,LM
67     RMSW(I) = 0.0
C----------------------------------------------------------------------
C--> áAèOMàHAEM.
       N = IFIX( PAR0T(5) )
       RMSW(1) = RN
       N = IFIX( PAR0T(6) )
       RMSW(2) = RN
       R = PAR0T(1)
CCC       L = PAR0T(2)
        L = 0.0
       IF ( R .GT. 0.0  ) GO TO 69
       WRITE (NWTR,505) NEL
       R = 1.0E-32
       L = 0.0
69     RMSW(4) = R
       RMSW(5) = L
       RMSW(6) = PAR0T(2)
       RMSW(7) = PAR0T(3)
       RMSW(8) = 3.14159 * PAR0T(4) / 180.0
       RETURN
C----------------------------------------------------------------------
505      FORMAT(' *** BHìTPEHHE COèPOTàBãEHàE àCTOóHàKA HAèPüÜEHàü ***'/
     =          ' *** ùãEMEHTA HOMEP ', I10,  ' PABHO Hìãû         ***'/
     =          ' *** èPàHàMAETCü PABHõM 1.0E-32  [OM]             ***')
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
