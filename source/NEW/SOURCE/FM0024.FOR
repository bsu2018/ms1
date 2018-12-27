         SUBROUTINE FM0024( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT „EKAPŸ 1987 ƒ.)------------
C
C
C   O„POƒPAMMA:  FM0024( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹E‰ C‹E„“ž™ˆX TˆOB:
C  ------------
C                +-----+--------------------------------------------+
C                I Tˆ I              ˆMŸ MO„E‹ˆ                    I
C                I-----I--------------------------------------------I
C                I 15  I   “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK TOKA    I
C                I-----I--------------------------------------------I
C                I 16  I   “PAB‹ŸEM›‰ HAPŸ†EHˆEM ˆCTO—HˆK HAPŸ-  I
C                I     I   †EHˆŸ                                    I
C                I-----I--------------------------------------------I
C                I 200 I   ˆ„EA‹œH›‰ OEPA–ˆOHH›‰ “Cˆ‹ˆTE‹œ         I
C                I-----I--------------------------------------------I
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
C                  RMSW Tˆ # 15 [ ˆT“H ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   “1   I   “2    I    G    II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  11    I   12    I    13   II
C             ++--------+---------+---------++
C
C
C                  RMSW Tˆ # 16 [ ˆH“H ]
C                  ----------------------
C
C      ++--------+---------+---------+---------+----------+
C      II        I         I         I         I          I
C      II   “1   I   “2    I   GBH   I    K    I G=GBH*K  I
C      II        I         I         I         I          I
C      ++--------+---------+---------+---------+----------+    LM = 5
C      II   1    I    2    I     3   I    4    I    5     I    ******
C      ++--------+---------+---------+---------+----------+
C      II  11    I   12    I    13   I   14    I   15     I
C      ++--------+---------+---------+---------+----------+
C
C
C                  RMSW Tˆ # 200 [ ˆ„EA‹œH›‰ OEPA–ˆOHH›‰ “Cˆ‹ˆTE‹œ ]
C                  ----------------------
C
C             ++--------+---------+---------++
C             II        I         I         II
C             II   K    I   GBX   I  GB›X   II
C             II        I         I         II
C             ++--------+---------+---------++       LM = 3
C             II   1    I    2    I     3   II       ******
C             ++--------+---------+---------++
C             II  13    I   14    I    15   II
C             ++--------+---------+---------++
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         EQUIVALENCE ( RN , N )
         INTEGER BA0T, TIP
         REAL L
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF( TIP .EQ. 15 ) GO TO 10
         IF( TIP .EQ. 16 ) GO TO 20
         IF( TIP .EQ. 200) GO TO 30
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0024: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
10       CONTINUE
C---> ˆCTO—HˆK TOKA, “PAB‹ŸEM›‰ HAPŸ†EHˆEM
         N = IFIX( PAR0T(1) )
         RMSW(1) = RN
         N = IFIX( PAR0T(2) )
         RMSW(2) = RN
         G = PAR0T(3)
         RMSW(3) = G
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 3
         RETURN
C######################################################################
20       CONTINUE
C---> ˆCTO—HˆK HAPŸ†EHˆŸ, “PAB‹ŸEM›‰ HAPŸ†EHˆEM
         N = IFIX( PAR0T(1) )
         RMSW(1) = RN
         N = IFIX( PAR0T(2) )
         RMSW(2) = RN
         R = PAR0T(3)
         IF( R .LE. 0.0 ) GO TO 25
         GBH = 1./R
21       RMSW(3) = GBH
         AK= PAR0T(4)
         RMSW(4) = AK
         G = AK * GBH
         RMSW(5) = G
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 5
         RETURN
25       WRITE(NWTR, 521) NEL
521    FORMAT(//' *** BH“TPEHHE COPOTˆB‹EHˆE ˆCTO—HˆKA HAPŸ†EHˆŸ ***'/
     =          ' *** ‹EMEHTA HOMEP ', I10,  ' PABHO H“‹ž         ***'/
     =          ' *** PˆHˆMAETCŸ PABH›M 1.0E-32  [OM]             ***')
         GBH = 1.0E+32
         GO TO 21
C######################################################################
30       CONTINUE
C---> ˆ„EA‹œH›‰ OEPA–ˆOHH›‰ “Cˆ‹ˆTE‹œ
         AK= PAR0T(1)
         RMSW(1) = AK
         RBX = PAR0T(2)
         IF( RBX .LE. 0.0 ) GO TO 35
         GBX = 1./ RBX
31       RMSW(2) = GBX
         RBIX = PAR0T(3)
         IF( RBIX .LE. 0.0 ) GO TO 37
         GBIX = 1./ RBIX
32       RMSW(3) = GBIX
C---> „‹ˆHA RMSW MO„E‹ˆ
         LM = 3
         RETURN
35       WRITE(NWTR, 531) NEL
531    FORMAT(//' *** BXO„HOE COPOTˆB‹EHˆE OEPA–ˆOHHOƒO “Cˆ‹ˆTE‹Ÿ***'/
     =          ' *** ‹EMEHTA HOMEP ', I10,  ' OTPˆ–ATE‹œHO       ***'/
     =          ' *** PˆHˆMAETCŸ PABH›M ECKOHE—HOCTˆ             ***')
         GBX = 0.0
         GO TO 31
37       WRITE(NWTR, 532) NEL
532    FORMAT(//' ***B›XO„HOE COPOTˆB‹EHˆE OEPA–ˆOHHOƒO “Cˆ‹ˆTE‹Ÿ***'/
     =          ' *** ‹EMEHTA HOMEP ', I10,  ' PABHO H“‹ž         ***'/
     =          ' *** PˆHˆMAETCŸ PABH›M 1.E-32 [OM]               ***')
         GBIX = 1.0E+32
         GO TO 32
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
