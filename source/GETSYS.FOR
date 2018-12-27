         SUBROUTINE  GETSYS( MSWALL, YALL, JALL, LOGYTR,
     *                       TOKN, TOKN1, VALLN, VALLN1  )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:  GETSYS( MSWALL, YALL, JALL, LOGYTR,
C   ------------           TOKN, TOKN1, VALLN, VALLN1       )
C
C   HA‡HA—EHˆE:    ”OPMˆPOBAHˆE CˆCTEM› ‹ˆHE‰H›X A‹ƒEPAˆ—ECKˆX
C  -----------     “PABHEHˆ‰ OTHOCˆTE‹œHO HEˆ‡BECTHOƒO
C                  BEKTOPA VALL - “‡‹OB›X HAPŸ†EHˆ‰;
C                  CˆCTEMA: [YALL] * [VALL] = [JALL]
C                  OPE„E‹ŸET ‡HA—EHˆŸ VALLN1 „‹Ÿ MOMEHTA
C                  BPEMEHˆ, PABHOƒO TN + H0 ( T.E. „‹Ÿ T(N+1) ).
C                  (”OPMˆP“ETCŸ OCHOBHAŸ ˆ "TEHEBAŸ" CˆCTEMA );
C
C
C   METO„:         O‹EMEHTH›‰ BK‹A„ ‡HA—EHˆ‰ G-J APAMETPOB
C  ------          BETBE‰, KBˆBA‹EHTˆP“ž™ˆX BCE ‹EMEHT›
C                  AHA‹ˆ‡ˆP“EMO‰ –Eˆ ( ZAPROS = 1 );
C                  OPE„E‹EHˆE TˆX APAMETPOB ˆ ˆX „OABKA B
C                  YALL ˆ JALL  POˆ‡BO„ˆTCŸ / DM.....;
C                  ( "TEHEBAŸ" CˆCTEMA ”OPMˆP“ETCŸ AHA‹Oƒˆ—HO )
C
C   APAMETP›:     MSWALL - “PAB‹Ÿž™ˆE C‹OBA MAKPOMO„E‹E‰;
C   ---------      YALL   - MATPˆ–A POBO„ˆMOCTE‰;
C                  JALL   - BEKTOP ˆCTO—HˆKOB TOKA BETBE‰;
C                  LOGYTR - BEKTOP Pˆ‡HAKOB K.‡. “‡‹OB;
C                  VALLN1 - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ ( ˆCKOM›‰ );
C                  VALLN  - BEKTOP “‡‹OB›X HAPŸ†EHˆ‰ B T = TN;
C                  TOKN - ‡HA—EHˆE "TOKOB" ˆ „P. EPEMEHH›X COCTOŸHˆŸ
C                         O BCEM ‹EMEHTAM B T = TN;
C                  TOKN1 - TO †E, HO B TN + H0;
C
C   O„POƒPAMM›: 1. CALLDM - "B›‡›BA‹œ™ˆK" DM MOHˆTOPOB;
C   ------------  2. COMMON PROF1; PRINT1, SUBERR, ENDLOG, MODSYS,
C                     DEPOSQ, PRINT4;
C  PˆME—AHˆE:    1. HA‹ˆ—ˆE MOHˆTOPOB DM..... HE POBEPŸETCŸ;
C  ----------     2. YALL , JALL ˆ LOGYTR - ˆHˆ–ˆA‹ˆ‡ˆP“žTCŸ;
C                    ˆHˆ–-TCŸ TAK †E OPEN6 ˆ KNOT6M ( CM. MASPRT );
C                 3. PEA‹ˆ‡OBAHO OPA™EHˆE K 25 / DM..... —EPE‡
C                    CALLDM;
C                 4. Pˆ FTEST >=3 - E—ATœ "TEHEBO‰" ˆ "O™E‰" CˆCTEM;
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSWALL(1), YALL(1), JALL(1), LOGYTR(1)
         DIMENSION TOKN(1), TOKN1(1), VALLN(1), VALLN1(1)
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE ( MASPRT(37), FLOG )
         EQUIVALENCE ( MASPRT(28), ZAPROS )
         EQUIVALENCE ( MASPRT(29), TN )
         EQUIVALENCE ( MASPRT(38), FSYS )
         EQUIVALENCE ( MASPRT(23), NDALL )
         EQUIVALENCE ( MASPRT(57), OPEN6 )
         EQUIVALENCE ( MASPRT(58), KNOT6M )
         EQUIVALENCE ( MASPRT(49), T6MSW )
         EQUIVALENCE ( MASPRT(60), FTEST )
         EQUIVALENCE ( MASPRT(14), NWTRE )
         EQUIVALENCE ( MASPRT(26), NSYS  )
         INTEGER OPEN6, ZAPROS, FSYS, NDALL, FLOG, FTEST
         REAL JALL
         ID(I,J) = NDALL * ( J - 1 )  + I
C===>  “CHAHOBKA "ZAPROS = 1"
         ZAPROS = 1
C===> CPOC ”‹AƒA "CˆCTEMA C”OPMˆPOBAHA"
         FSYS = 0
C===> ˆHˆ–ˆA‹ˆ‡A–ˆŸ MACCˆBOB YALL, JALL, LOGYTR
         DO 500 I=1, NSYS
         LOGYTR(I) = -1
500      CONTINUE
C---->
         CALL DEPOSQ( 0.0, 0.0, YALL, JALL )
C---->
         OPEN6 = 0
         KNOT6M = NSYS
C===>   B›‡OB BCEX MOHˆTOPOB*******************************************
         KF = 1
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODD )
C===> CˆCTEMA C”OPMˆPOBAHA, LOGYTR - OPE„E‹EH....
         FSYS = 1
C---->  EC‹ˆ "TEHEBAŸ" CˆCTEMA HE H“†HA, TO BCE!
         IF ( FLOG .EQ. 0 ) GO TO 12345
C---> "AAPAT" FLOG BKž—EH - ”OPMˆP“EM  "TEHEB“ž" CˆCTEM“
C--> "OKOH—ATE‹œHOE" ”OPMˆPOBAEˆE LOGYTR
         CALL ENDLOG( LOGYTR )
C----> MO„ˆ”ˆ–ˆPOBATœ YALL „‹Ÿ ”OPMˆPOBAHˆŸ "TEHEBO‰" CˆCTEM›
         CALL MODSYS( YALL, JALL, LOGYTR )
C===>  ”OPMˆPOBATœ  "TEHEB“ž" CˆCTEM“....
         KF = 3
         ZAPROS = 10
         CALL CALLDM( KF, MSWALL, YALL, JALL, LOGYTR,
     =                TOKN, TOKN1, VALLN, VALLN1, KODD )
C--->  TEHEBAŸ CˆCTEMA C”OPMˆPOBAHA, HET ‹ˆ ‡APOCA HA KOHTP. E—ATœ
12345    IF ( FTEST .LT. 3 ) RETURN
C===> KOHTPO‹œH›E E—ATˆ......
         NNN = MASPRT(7)
         WRITE ( NWTRE, 449 ) TN
449      FORMAT(/' *** FTEST_3: "OCHOBHAŸ" ˆ "TEHEBAŸ" CˆCTEM›',
     =    ' TN=', G12.5, ' ***')
C--->
       CALL PRINT4( YALL, JALL, LOGYTR )
C-->
C         DO 44 I=1, NNN
C44        WRITE(NWTRE, 445) ( I, J,YALL( ID(I,J) ), J=1,NNN       )
C445      FORMAT( 5(1X,'Y(',I3,',', I3, ')=', G12.5) )
C        WRITE(NWTRE, 446) (I,JALL(I), I=1, NNN       )
C446      FORMAT( 5(1X,'J(', I3, ')=', G12.5 ) )
C------
C         WRITE(NWTRE,447) ( I, LOGYTR(I), I=1,NNN)
C447      FORMAT( 5(1X,'LOGYTR(', I3, ')=', I4 ) )
C--->
         WRITE(NWTRE, 448) OPEN6, KNOT6M
448      FORMAT( ' OPEN6 = ', I5, '  KNOT6M = ', I5 )
C------
         RETURN
         END
