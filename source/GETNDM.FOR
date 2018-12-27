         SUBROUTINE GETNDM( MSWALL     )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 2.0
C   O„POƒPAMMA:   GETNDM( MSWALL     )
C   ------------
C   HA‡HA—EHˆE:    POCMOTP MSWALL CˆCKA,
C   -----------    ¨ ¯®áâà®¥­¨¥ ä ©«  ­®¬¥à 96, á®¤¥à¦ é¥£®
C                  ­®¬¥à  DM ¬®­¨â®à®¢ - ¡¥áä®à¬ â­ë© «®£¨ç¥áª¨©
C                  MAS(i) = 0 = .FALSE. -  ­¥â ¬®­¨â®à  ¢ MSW
C                  MAS(i) = 1 = .TRUE.  -  ¥áâì ¬®­¨â®à ¢ MSW
C                  ” ©« § ¯¨á ­ ¢ ä®à¬¥ 'DIRECT' á ¤«¨­®© § ¯¨á¨
C                  100 ¡ ©â.
C   METO„:         OˆCK.
C   -----
C   APAMETP›:     MSWALL - C‹OBA COCTOŸHˆŸ ‹EMEHTOB;
C   ---------
C @@@@             IRC = 0 - ä ©« á®§¤ «¨;
C @@@@             IRC = 1 - ä ©« á®§¤ âì ­¥ ã¤ «®áì;
C   O„POƒPAMM›:  HET
C   ------------
C   PˆME—AHˆE:
C   ----------
C
C                  CTP“KT“PA MACCˆBA  MSWALL:
C                  --------------------------
C
C  +-------------...+------+------------+------------+...+----------+
C  I            I   I A„P. I"OCTOŸHHAŸ"I"EPEMEHHAŸ"I   IA„P. C‹E„.I
C  I „ˆCKPˆTOP I   I C‹E„.I —ACTœ MSW  I —ACTœ MSW  I   I   ˆ‹ˆ    I
C  I            I   I MSW  I (INTEGER*4)I            I   I  HO‹œ [0]I
C  +------------+...+------+------------+------------+...+----X-----+
C  I1 2 3 4 5   I   I  1   I   8 C‹OB   I    ????    I        I
C  +------------+   +------+------------+------------+        I
C                      I                                      I
C                      V                                      I
C                      +>------------------------------------>+
C
C
C               "OCTOŸHHAŸ" —ACTœ MSW:
C                ----------------------
C
C  +-------+--------+--------+--------+------+------+----------+------+
C  I HOMEP I HOMEP  I HOMEP  I Pˆ‡HAKI KO‹ˆ I KO‹. I A„P. B›B.I PE   I
C  I ‹EME I TˆA   I /    I TˆA   I —ECT I B›B. I ‡HA—. B  I ‡E   I
C  I HTA   I MO„E‹ˆ I PAC—ETAI        I BO   I ‡HA— I "TOKN" ˆ I PB   I
C  I       I        I DM.....I        I “‡‹OBI EHˆ‰.I B "TOKN1"I      I
C  +-------+--------+--------+--------+------+------+----------+------+
C  I  1    I   2    I   3    I    4   I   5  I  6   I    7     I  8   I
C  +-------+--------+--------+--------+------+------+----------+------+
C
C
C               "EPEMEHHAŸ" —ACTœ MSW:
C               -----------------------
C
C  +----------++-------------+-------------+
C  I CˆCOK   II CˆCOK      I PAO—AŸ     I
C  I “‡‹OB    II BH“TPEHHˆX  I O‹ACTœ     I
C  I O„K‹ž-  II APAMETPOB  I MAKPOMO„E‹ˆ I
C  I —EHˆŸ    II MAKPOMO„E‹ˆ I             I
C  I ‹EMEHTA II             I             I
C  I          II             I             I
C  +----------++-------------+-------------I
C  I 9+K"“‡‹OBII   SPM       I  SRAB       I
C  +----------++-------------+-------------+
C
CZZZ-------------------------------------------------------------------
         PARAMETER (LLMAS=100)
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE( MASPRT(13), NWTR )
         EQUIVALENCE( MASPRT(14), NWTRE )
         DIMENSION MSWALL(*)
         LOGICAL*1 MAS(LLMAS)
         INTEGER ADR
         DATA nf/96/
         DO 100 i=1,LLMAS
100      mas(i) = .FALSE.
         IRC = 1
         ADR = 6
900      I1 = MSWALL(ADR)
         IF ( I1 .EQ. 0 ) GO TO 555
         IMSW = ADR + 1
         ADR = I1
C---> KTO TAM?
         IF ( MSWALL(IMSW+2) .LE. 0    .OR.
     =        MSWALL(IMSW+2) .GT.LLMAS        ) GO TO 777
C--->
         MAS( MSWALL(IMSW+2) ) = .TRUE.
         GO TO 900
555      CONTINUE
CCCCC*         ACCESS='DIRECT',RECL=LLMAS,ACTION='WRITE',STATUS='NEW',
         OPEN (UNIT=nf,file='MS1_DATA.DMN',
     *         ACCESS='DIRECT',RECL=LLMAS,
     *         ERR=666,IOSTAT=irc)
        WRITE(UNIT=nf,REC=1,IOSTAT=irc,ERR=888) (MAS(iii),iii=1,LLMAS)
        CLOSE(UNIT=nf)
        IRC = 0
         RETURN
C************
666      WRITE(NWTRE,667) irc
         WRITE(NWTR ,667) irc
667    FORMAT(/' *** Žè¨¡ª¨ ¯à¨ ®âªàëâ¨¨ ä ©«  á¢ï§¨ MS1_DATA.DMN ***'/
     *        ' *** ª®¤ ®è¨¡ª¨ {IOSTAT} = ',I8, '                ***'/)
C        RETURN
        CALL STOPP(20)
C************
888      WRITE(NWTRE,887) irc
         WRITE(NWTR ,887) irc
887    FORMAT(/' *** Žè¨¡ª¨ ¯à¨ § ¯¨á¨   ä ©«  á¢ï§¨ MS1_DATA.DMN ***'/
     *        ' *** ª®¤ ®è¨¡ª¨ {IOSTAT} = ',I8, '                ***'/)
        CALL STOPP(24)
C        RETURN
C************
777      WRITE(NWTRE,778) MSWALL(IMSW+2),MSWALL(IMSW+0),MSWALL(IMSW+1)
         WRITE(NWTR ,778) MSWALL(IMSW+2),MSWALL(IMSW+0),MSWALL(IMSW+1)
778    FORMAT(/' *** Žè¨¡ª¨ ¯à¨ ¯à®á¬®âà¥ á¯¨áª   MSW '/
     *         ' *** ­¥ ¤®¯ãáâ¨¬ë© ­®¬¥à DM ®¡à ¡®âç¨ª :'/
     *         ' *** DM',I4.4, ' ¤«ï í«¥¬¥­â  ',I5,' â¨¯ ', I5/)
        CALL STOPP(28)
C        RETURN
         END
