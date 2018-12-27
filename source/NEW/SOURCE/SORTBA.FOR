      SUBROUTINE SORTBA( BA0)
CXXX-------------------------------------------------------------------
C   O„POƒPAMMA:  SORTBA ( BA0 )
C   ------------
C   HA‡HA—EHˆE:    COPTˆP“ET MACCˆB BA0:
C   ----------     A) O BO‡PACTAHˆž HOMEPA Pˆ‡HAKA TˆA.
C                  ) B KA†„O‰ ƒP“E ‡AˆCE‰ C O„ˆHAKOB›Mˆ ‡HA—EHˆŸMˆ
C                     Pˆ‡HAKA TˆA B›O‹HŸET COPTˆPOBK“ O BO‡PACTAHˆž
C                     HOMEPA ‹EMEHTA.
C   METO„:          "„PEBECHA" COPTˆPOBKA / TRIARB.
C   -----
C   O„POƒPAMM›:  1. TRIARB + BA0CLP + BA0CNG.
C   ------------   2. TRIARB + BA1CLP + BA1CNG.
C                  3. COMMON PROF1
C   APAMETP›:     BA0 - CˆCOK ‹EMEHTOB –Eˆ OC‹E INPUT0 ( CM. )
C   ---------
C   PˆME—AHˆE:    1. COPTˆPOBKA ) B›O‹HŸETCŸ Pˆ KEYSOR(I) = 1
C   ----------        ƒ„E I=1,2,3,4,5,6 - HOMEP Pˆ‡HAKA TˆA;
C                  2. Pˆ PAC˜ˆPEHˆˆ HOMEPOB .TˆA -> PACC˜ˆPˆTœCŸ !!!
C                  3. BA0 - CTATˆ—ECKˆ B›„E‹EH.
C                  4. BA0... ˆ BA1... TE†E ”“HK–ˆˆ, HO PA‡H›‰
C                     “—ET „ˆCKPˆTOPA MACCˆBA BA0 ( CM. / INPUT0 )
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0(*), KEYSOR(6), ABEG(6), NKOL(6)
       INTEGER BA0, BA0CPL, BA1CPL, PTIP, ABEG, PTIPBA
C--> ABEG - A„P. HA—A‹A B BA0, NKOL - KO‹-BO, ‹-OB C .TˆA = ˆH„EKC“
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       EQUIVALENCE ( NWTR,  MASPRT(13) )
       EQUIVALENCE ( NXBA0, MASPRT( 3) )
       EQUIVALENCE ( N1BA0, MASPRT( 4) )
C
       EXTERNAL BA0CPL, BA1CPL, BA0CNG, BA1CNG
       DATA KEYSOR / 1, 1, 1, 1, 1, 1 /, LKEYS /6/
C----------------------------------------------------------------------
C-->  KOHTPO‹œ ??
       IF (NXBA0 .GT. 0 ) GO TO 200
       WRITE( NWTRE, 201 ) NXBA0
       WRITE( NWTR,  201 ) NXBA0
201    FORMAT(//' *** B –Eˆ HET ‹EMEHTOB -> NXBA0 = ',I3,
     * ' ***'/' *** PAOTA BO‡MO†HO PEPBETCŸ =SORTBA= ***'//)
       RETURN
200    CONTINUE
       IF ( NXBA0 .EQ. 1 ) RETURN
C!!!-------------------------------------------------------------------
C@@@@@ CALL PRINT0( BA0, BA0, BA0 )
C!!!-------------------------------------------------------------------
C
C  EPBAŸ ”A‡A  (A)
C
       CALL TRIARB( NXBA0, BA0CNG, BA0CPL, BA0 )
C
C!!!-------------------------------------------------------------------
C@@@@ CALL PRINT0( BA0, BA0, BA0 )
C!!!-------------------------------------------------------------------
C
C   BTOPAŸ ”A‡A  (B)
C
C  ‡AO‹HEHˆE ABEG ˆ NKOL
C
       DO 1 I=1,LKEYS
       ABEG(I) = 0
1      NKOL(I) = 0
C  CKAHˆPOBATœ BA0
       I0 = 6
       IMAX = BA0(3)
C--> MACCˆB BA0 OTCOPTˆPOBAH O .TˆA
C    B›„E‹ŸEM ƒPAHˆ–› "OTPE‡KOB" ˆ ”OPMˆP“EM ABEG + NKOL
C
       PTIP = 0
       DO 100 I=1, NXBA0
C  -> A„P. HA—. TEK. ‡A. B BA0 - ‹E
       IZ = (I-1) * N1BA0 + I0
C-->  KOHTPO‹œ
       IF ( IZ + N1BA0  .GT. IMAX ) CALL SUBERR( IZ )
C-->  KAKO‰ TEK. .Tˆ?
       PTIPBA = BA0( IZ + 4 )
C-->  OCHOBHOE CPABHEHˆE....
       IF ( PTIPBA .EQ. PTIP ) GO TO 101
C
C-->  A K‹ž—, TO „P“ƒO‰ !
C      CTPAXOBKA...
       IF ( PTIPBA .GT. LKEYS .OR.
     *      PTIPBA .LE.   0         )  CALL SUBERR( PTIPBA )
C  SALTO-MORTALE.....
       PTIP = PTIPBA
       ABEG( PTIP ) = IZ
       NKOL( PTIP ) = 1
       GO TO 100
C----------------------------------------------------------------------
101    CONTINUE
C
C-->  K‹ž—ˆK CTAP›‰
C
       IF ( PTIP .GT. LKEYS  .OR.  PTIP .LE. 0 ) CALL SUBERR(PTIPBA)
C
       NKOL ( PTIP ) = NKOL( PTIP )  +  1
100    CONTINUE
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C
C  COPTˆP“EM O„ƒP“›
C
       DO 300 I=1,LKEYS
C-->  MO†ET HE HA„O ?
       IF ( KEYSOR(I) .NE. 1 ) GO TO 300
C-->  CKO‹œKO ˆ ƒ„E HA—A‹O ?
       N = NKOL(I)
       NBEG = ABEG(I)
C-->  MO†ET HE HA„O ?
       IF ( N .LE. 1 ) GO TO 300
C-->  COPTˆPOBATœ "OTPE‡OK" BA0...
       CALL TRIARB( N, BA1CNG, BA1CPL, BA0(NBEG) )
300    CONTINUE
C!!!-------------------------------------------------------------------
C@@@@@ CALL PRINT0( BA0, BA0, BA0 )
C!!!-------------------------------------------------------------------
C  “CE
       RETURN
       END
