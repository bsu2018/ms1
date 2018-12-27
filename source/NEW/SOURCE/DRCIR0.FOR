         SUBROUTINE DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRC )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DRCIR0( KAR, DAT, BA0, KNOT, PAR0, IRC )
C   ------------
C   HA‡HA—EHˆE:    *** OPAOT—ˆK #–Eœ:
C  ------------       #–Eœ  N
C
C                  N - HOMEP KAHA‹A BBO„A ˆH”OPMA–ˆˆ;
C
C                  Pˆ N "O“™EHO" ˆ‹ˆ "O˜ˆO—HO" “CTAHAB‹ˆBAETCŸ
C                  PABH›M NRDR KAHA‹“;
C
C                  ”OPMAP“ET MACCˆB› BA0, KNOT, PAR0
C                  ( 1-Ÿ —ACTœ „EPˆKTˆB› #–Eœ )
C   METO„:         CM. / INPUT0,INPUT2
C   -----
C   APAMETP›:     KAR - CˆMBO‹œHAŸ CTPOKA „EPˆKTˆB›,
C   ---------      DAT - MACCˆB —ˆCE‹ „EPˆKTˆB›;
C                  IRC = 1  MACCˆB› BA0, KNOT, PAR0 OPE„E‹EH›;
C                      = 2  ˆH”OPMA–ˆŸ HE PˆHŸTA;
C                  „P. AP. CM. INPUT0,INPUT2
C
C   O„POƒPAMM›:  1. COMMON PROF1
C   -------------  2. INPUT0  INPUT2   PPNCOM  READST
C   PˆME—AHˆE:    1. B C‹“—AE "”ATA‹œH›X" O˜ˆOK - STOP 666
C   ----------        ( KOHE– CˆCTEMHOƒO KAHK‹A NRDR )
C                  2. O„POHEE CM. INPUT0,INPUT2
C                  3. MACCˆBA BA0, KNOT, PAR0 - ˆHˆ–ˆˆPOBAHH›;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         DIMENSION BA0(*), PAR0(*), KNOT(*), KAR(*), DAT(*), DSNAME(11)
         EXTERNAL READST
         INTEGER BA0, FMSWAL, DIAL
         EQUIVALENCE ( MASPRT(13),  NWTR )
         EQUIVALENCE ( MASPRT(14),  NWTRE )
         EQUIVALENCE ( MASPRT(74),  FMSWAL )
         EQUIVALENCE ( MASPRT(12),  NRDR   )
         EQUIVALENCE ( MASPRT(15),  DIAL   )
         DATA DSNAME/ 11*'    '/
C-->  ”ˆKCA–ˆŸ “KA‡ATE‹E‰
         IBA01 = BA0(3)
         IKNOT1 = KNOT(3)
         IPAR01 = IFIX (PAR0(3))
C-->  C KAKOƒO KAHA‹A ‡APOC BBO„A „‹Ÿ #–Eœ:
         NRDR1 = NRDR
         IF ( DAT(1) .EQ. 0.0 ) GO TO 1
C--> —TO-TO ECTœ
         NRDR1 = IFIX( DAT(2) )
C--> KOHTPO‹œ
         IF ( NRDR1 .LE. 0  .OR. NRDR1. GT. 100 .OR.
     =  NRDR1 .EQ. NWTR  .OR. NRDR1 .EQ. NWTRE ) NRDR1 = NRDR
C--> COO™ˆM..
1        WRITE(NWTR, 50 ) NRDR1
50     FORMAT(/' *** #–Eœ ¯à¨­ïâ :==> ®¯¨á ­¨¥ æ¥¯¨ ¢¢®¤¨âáï ç¥à¥§ ',
     = 'ª ­ « # ',I2, ' ***'/)
         IRC = 1
C---> HAE—ATAEM.
       CALL PPNCOM(3,NWTR)
C######################################################################
C--->  HA—ˆHAEM BBO„ˆTœ „AHH›E
         ii_rr = 0
         IF ( NRDR1 .NE. NRDR ) CALL KSOPEN(NRDR1,0,ii_rr)
         IF ( ii_rr .NE. 0 )  STOP 666
C----------------------------------------------------------------------
         CALL INPUT0( BA0, KNOT, PAR0, NRDR1, DSNAME, READST,  III )
         IF ( NRDR1 .NE. NRDR ) CLOSE (NRDR1)
C--->EC‹ˆ #CPOC ˆ‹ˆ —TO-TO ˆƒHOPˆPOBA‹ˆ, TO Bˆ„ˆMO "PˆEXA‹ˆ"...
         IF ( III .EQ. 1  .OR. III .EQ. 2 ) IRC = 2
         IF ( IRC .EQ. 2 ) RETURN
C--> A MO†ET TO "“CE"
         IF ( III .EQ. 3 ) GO TO 9999
C--> A MO†ET O‹“—EHA „EPˆKTˆBA #MEX
         IF ( III .EQ. 4 ) GO TO 2
         RETURN
C######################################################################
C  OPAOT—ˆK
C  O„„EPˆKTˆB›        METKA 2
C  #MEX
C----------------------------------------------------------------------
2       CONTINUE
        IF( BA0(3) .NE. IBA01 .OR. KNOT(3) .NE. IKNOT1 .OR.
     *      IFIX(PAR0(3)) .NE. IPAR01  ) GO TO 5
        IF( DIAL .EQ. 0 ) WRITE(NWTR,9995)
9995   FORMAT(/20X,
     *         ' *** MO„E‹ˆP“EMAŸ CˆCTEMA HE CO„EP†ˆT ‹EKTPˆ—ECKO‰',
     *         ' –Eˆ ***'/)
        IF( DIAL .EQ. 1 ) WRITE(NWTR,9996)
9996   FORMAT(/10X,
     *         ' *** MO„E‹ˆP“EMAŸ CˆCTEMA HE CO„EP†ˆT ‹EKTPˆ—ECKO‰',
     *         ' –Eˆ ***'/)
5      CONTINUE
C----------------------------------------------------------------------
C-->  BBO„ˆM „A‹œ˜E
C!!!     IF ( NRDR1 .NE. NRDR ) CALL KSOPEN(NRDR1,0,ii_rr)
C!!!     CALL INPUT2( BA0, KNOT, PAR0, NRDR1,  READST,  III )
C!!!     IF ( NRDR1 .NE. NRDR ) CLOSE (NRDR1)
C--->EC‹ˆ #CPOC ˆ‹ˆ —TO-TO ˆƒHOPˆPOBA‹ˆ, TO Bˆ„ˆMO "PˆEXA‹ˆ"...
         IF ( III .EQ. 1  .OR. III .EQ. 2 ) IRC = 2
         IF ( IRC .EQ. 2 ) RETURN
C--> A MO†ET TO "“CE"
         IF ( III .EQ. 3 ) GO TO 9999
         RETURN
C######################################################################
C     KOHE– BXO„HOƒO HAOPA
C---> KOH—AEM....
9999    CONTINUE
        WRITE(NWTR,51)
51     FORMAT(//' *** PAOTA PEP›BAETCŸ - KAHA‹ CˆCTEMHOƒO BBO„A HE„OCT
     =“EH ***'//)
         STOP 666
         END
