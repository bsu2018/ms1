         SUBROUTINE DRPR01( KOD, TIME, TOK, VALL )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DRPR01( KOD, TIME, TOK, VALL )
C   -------------
C   HA‡HA—EHˆE:    ˆCOHˆTE‹œ „ˆPEKTˆB› #E—
C   ----------
C   METO„:         O ˆH”OPMA–ˆˆ ˆ‡ COMMON SERVZ6 - E—ATœ „AHH›X;
C   -----
C              EPEMEHHAŸ KXXX [MASPRT(99)] ˆCO‹œ‡“ETCŸ KAK C.—.K.
C              PO“CKAEM›X TO—EK ---> OPAOTKA APAMETPA KOUT;
C
C             Pˆ FREST = 1 ( ”‹Aƒ ‡APOCA #C—ET ) KXXX Pˆ KOD=1
C                            HE OH“‹ŸETCŸ;
C
C
C   APAMETP›:     KOD = 1 - OTKP›Tœ TA‹ˆ–“,  KXXX - OH“‹ˆTœ;
C   ---------          = 2 - E—ATœ O—EPE„HO‰;
C                      = 3 - ‡AKP›Tœ TA‹ˆ–“;
C                      = 4 E—ATœ O—EPE„HO‰ C Pˆ‡HAKOM KOMM“TA–ˆˆ;
C                      = 5 E—ATœ O—EPE„HO‰ (?) C Pˆ‡HAKOM #TECT_P“HƒE#
C                      = 6 E—ATœ „AHH›X B MOMEHT CO‡„AHˆŸ K.T. B KOH–E
C                             ( B›BO„ˆTCŸ BCEƒ„A )
C                      = 7 E—ATœ „AHH›X C Pˆ‡HAKOM ˆHTEPO‹. B›BO„A;
C                  TIME, TOK, VALL - MACCˆBA OPE„E‹Ÿž™ˆE TO—K“;
C
C   O„POƒPAMM›:  1. DRPR00
C   -------------  2. COMMON SERVZ6, PROF1
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( KXXX, MASPRT(99) )
       EQUIVALENCE( FREST,MASPRT(45) )
       INTEGER FREST
         COMMON/SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
         DIMENSION TOK(1), VALL(1)
         INTEGER SPISON
         GO TO (1,2,3,2,2,4,2 ), KOD
         CALL SUBERR(KOD)
1        CALL DRPR00(KOD, SPISON, TIME, TOK, VALL)
       IF ( FREST .EQ. 1 ) RETURN
         KXXX = 0
         RETURN
2        CONTINUE
         IF ( TIME .LT. TBEGON ) RETURN
         IF ( TIME .GT. TENDON ) RETURN
c         IF ( KOD .EQ. 4  .OR. KOD .EQ. 5 ) GO TO 4
c         IF ( KXXX .EQ. 0 ) CALL DRPR00( KOD, SPISON, TIME, TOK, VALL )
c         KXXX = KXXX + 1
c         IF ( KXXX .EQ. KOUTON ) KXXX = 0
         CALL DRPR00( KOD, SPISON, TIME, TOK, VALL )
         RETURN
C######################################################################
3        CONTINUE
C--->  ‡AKP›Tœ E—ATœ
         CALL DRPR00(KOD, SPISON, TIME, TOK, VALL )
         RETURN
C######################################################################
C--->  KOMM“TA–ˆž B›BO„ˆM BCEƒ„A, E—ATœ TECT_P“HƒE TO †E, K.T. TO †E...
4        CALL DRPR00(KOD, SPISON, TIME, TOK, VALL )
         RETURN
         END
