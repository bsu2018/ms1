         SUBROUTINE FM0001( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C
C
C   O„POƒPAMMA:  FM0001( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹E‰ C‹E„“ž™ˆX TˆOB:
C  ------------
C                  Tˆ     ˆMŸ MO„E‹ˆ     OO‡HA—EHˆŸ B „A‹œHE‰˜EM
C
C                   1      COPOTˆB‹EHˆE           R
C
C                   2      ˆH„“KTˆBHOCTœ           L
C
C                   3      EMKOCTœ                 C
C
C                          ˆCTO—HˆK
C                   4      OCTOŸHHOƒO             J
C                          TOKA
C
C                          ˆCTO—HˆK
C                   5      OCTOŸHHOƒO             E
C                          HAPŸ†EHˆŸ
C
C                          ˆCTO—HˆK
C                   6      SIN                     ES
C                          HAPŸ†EHˆŸ(1)
C
C                          ˆCTO—HˆK
C                   7      SIN                     ES
C                          HAPŸ†EHˆŸ(2)
C
C                  POˆ‡BO„ˆT ”OPMˆPOBAHˆE "EPEMEHHO‰" —ACTˆ MSW
C                  MAKPOMO„E‹E‰ “KA‡AHH›X TˆOB.
C
C   METO„:         B›—ˆC‹ŸETCŸ ˆ ‡AˆC›BAETCŸ B MSW BCŸ HEOXO„ˆMAŸ
C   -----          ˆH”OPMA–ˆŸ, HEOXO„ˆMAŸ „‹Ÿ OC‹E„“ž™EƒO
C                  PAC—ETA MAKPOMO„E‹ˆ. A„ATˆPOBAHO K METO„“
C                  ‹ˆHˆ„†EPA - BˆO‹“ƒˆ.
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW - O‹Ÿ SPM ˆ SRAB;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HE POˆ‡BO„ˆTCŸ;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C
C                  RMSW Tˆ # 1 [ COPOTˆB‹EHˆE ]
C                  ------------------------------
C
C                 SPM
C             ++--------++
C             II ‡HA—.  II
C             II  1/R   II
C             II        II            LM = 1
C             ++--------++            ******
C             II   1    II
C             ++--------++
C             II  11    II
C             ++--------++
C
C
C                  RMSW TˆA # 2 [ ˆH„“KTˆBHOCTœ ]
C                  -------------------------------
C
C                 SPM            SRAB
C              +--------++-----+-----+
C              I ‡HA—.  II     I     I
C              I   L    II  GL I  JL I
C              I        II     I     I
C              +--------++-----+-----+         LM = 3
C              I   1    II 2   I  3  I         ******
C              +--------++-----+-----+
C              I   11   II 12  I 13  I
C              +--------++-----+-----+
C
C
C
C
C                  RMSW TˆA # 3 [ EMKOCTœ ]
C                  -------------------------
C
C                 SPM            SRAB
C             ++--------++-----+-----+
C             II ‡HA—.  II     I     I
C             II   C    II  GC I  JC I
C             II        II     I     I           LM = 3
C             ++--------++-----+-----+           ******
C             II   1    II  2  I  3  I
C             ++--------++-----+-----+
C             II   11   II 12  I 13  I
C             ++--------++-----+-----+
C
C
C
C
C
C                  RMSW TˆA # 4 [ ˆCTO—HˆK OCTOŸHHOƒO TOKA ]
C                  -------------------------------------------
C
C                 SPM
C             ++--------+
C             II ‡HA—.  I
C             II   J    I
C             II        I
C             ++--------+           LM = 1
C             II   1    I           ******
C             ++--------+
C             II   11   I
C             ++--------+
C
C
C             RMSW TˆA # 5 [ OCTOŸHH›‰ ˆCTO—HˆK HAPŸ†EHˆŸ ]
C             ------------------------------------------------
C
C                   SPM
C             ++--------+------++
C             II ‡HA—.  I ‡HA—.II
C             II  1/R   I  E/R II
C             II        I      II
C             ++--------+------++
C             II   1    I  2   II        LM = 2
C             ++--------+------++        *******
C             II  11    I  12  II
C             ++--------+------++
C
C
C
C                  RMSW TˆA # 6 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ(1) ]
C                  --------------------------------------------
C                    ( U = EM * SIN( OMEGA * T  +  FAZA )     LM = 5
C                                                             ******
C
C                             SPM                 SRAB
C             ++--------+------+--------+------++------+
C             II ‡HA—.  I ‡HA—.I        I FAZA II      I
C             II  1/R   I EM/R I OMEGA  I  P   II JES  I
C             II        I      I        I PA„. II      I
C             ++--------+------+--------+------++------+
C             II   1    I  2   I   3    I  4   II  5   I
C             ++--------+------+--------+------++------+
C             II  11    I  12  I  13    I  14  II  15  I
C             ++--------+------+--------+------++------+
C
C
C                  RMSW TˆA # 7 [ ˆCTO—HˆK SIN HAPŸ†EHˆŸ(2) ]
C                  --------------------------------------------
C                                                             LM = 7
C                                                             ******
C
C                      SPM                        SRAB
C     ++------+------+------+--------+------++------+------+
C     II      I      I ‡HA—.I        I FAZA II      I      I
C     II  R   I  L   I  EM  I OMEGA  I  P   II GES  I JES  I
C     II      I      I      I        I PA„. II      I      I
C     ++------+------+------+--------+------++------+------+
C     II  1   I  2   I  3   I   4    I  5   II  6   I   7  I
C     ++------+------+------+--------+------++------+------+
C     II  11  I  12  I  13  I  14    I  15  II  16  I  17  I
C     ++------+------+------+--------+------++------+------+
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         GO TO (1, 2, 3, 4, 5, 6 , 7 ), TIP
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0001: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> COPOTˆB‹EHˆE
         R = PAR0T(1)
         IF ( R .EQ. 0.0 ) GO TO 10
C---> ‡AOMHˆM APAMETP›.
11       RMSW(1) = 1.0 / R
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 1
         RETURN
10       WRITE(NWTR,501) NEL
501      FORMAT(' *** COPOTˆB‹EHˆE ‹EMEHTA ', I5, ' PABHO H“‹ž ***'/
     =          ' *** PˆHˆMAETCŸ PABH›M 1.0E-32 [OM]            ***')
         R = 1.0E-32
         GO TO 11
C######################################################################
2        CONTINUE
C----> ˆH„“KTˆBHOCTœ
         L = PAR0T(1)
         IF ( L .EQ. 0.0 ) GO TO 20
C---> ‡AOMHˆM APAMETP›.
21       RMSW(1) = L
         RMSW(2) = 0.0
         RMSW(3) = 0.0
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 3
         RETURN
20       WRITE(NWTR, 521) NEL
521    FORMAT(' *** ˆH„“KTˆBHOCT ‹EMEHTA HOMEP ', I5, ' PABHA H“‹ž ***'
     =   /    ' *** PˆHˆMAETCŸ PABHO‰ 1.0E-32 [ƒH]                 ***'
     =  )
         L = 1.0E-32
         GO TO 21
C######################################################################
3        CONTINUE
C--->  EMKOCTœ
         C = PAR0T(1)
C---> ‡AOMHˆM APAMETP›.
         RMSW(1) = C
         RMSW(2) = 0.0
         RMSW(3) = 0.0
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 3
         RETURN
C######################################################################
4        CONTINUE
C----> ˆCTO—HˆK OCTOŸHHOƒO TOKA.
         AJ = PAR0T(1)
C---> ‡AOMHˆM APAMETP›.
         RMSW(1) = AJ
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 1
         RETURN
C######################################################################
5        CONTINUE
C---->  ˆCTO—HˆK OCT. HAP.
         R = PAR0T(1)
         E = PAR0T(2)
         IF ( R .EQ. 0.0 ) GO TO 50
C---> ‡AOMHˆM APAMETP›.
51       RMSW(1) = 1.0 / R
         RMSW(2) =   E / R
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 2
         RETURN
50       WRITE(NWTR,505) NEL
505      FORMAT(' *** BH“TPEHHE COPOTˆB‹EHˆE ˆCTO—HˆKA HAPŸ†EHˆŸ ***'/
     =          ' *** ‹EMEHTA HOMEP ', I10,  ' PABHO H“‹ž         ***'/
     =          ' *** PˆHˆMAETCŸ PABH›M 1.0E-32  [OM]             ***')
         R = 1.0E-32
         GO TO 51
C######################################################################
6        CONTINUE
C---> SIN ˆCT. HAP.
         R = PAR0T(1)
         EM = PAR0T(2)
         OMEGA = PAR0T(3)
         FAZA = PAR0T(4) * 3.141592654 / 180.0
         IF ( R .EQ. 0.0 ) GO TO 60
C---> ‡AOMHˆM APAMETP›.
61       RMSW(1) = 1.0 / R
         RMSW(2) = EM / R
         RMSW(3) = OMEGA
         RMSW(4) = FAZA
         RMSW(5) = 0.0
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 5
         RETURN
60       WRITE (NWTR,505) NEL
         R = 1.0E-32
         GO TO 61
C######################################################################
7        CONTINUE
C---> SIN ˆCT. HAPŸ†EHˆŸ (2)
         ER = PAR0T(1)
         EL = PAR0T(2)
         AEM = PAR0T(3)
         OMEGA = PAR0T(4)
         FAZA = PAR0T(5) * 3.141592654 / 180.0
         IF( ER .NE. 0.0  .OR.  EL .NE. 0.0 ) GO TO 70
         ER = 1.0E-32
         WRITE(NWTR,505) NEL
70       CONTINUE
C---> ‡AOMHˆM APAMETP›.
         RMSW(1) = ER
         RMSW(2) = EL
         RMSW(3) = AEM
         RMSW(4) = OMEGA
         RMSW(5) = FAZA
         RMSW(6) = 0.0
         RMSW(7) = 0.0
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 7
         RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
