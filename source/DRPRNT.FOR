         SUBROUTINE DRPRNT(KAR, DATDIR, MSWALL, TOKN, VALN )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DRPRNT(KAR, DATDIR, MSWALL )
C   ------------
C   HA‡HA—EHˆE :   *** OPAOTKA „ˆPEKTˆB›  #E—:
C   -----------
C                    #E— T0 TM KOUT  PPVENT
C
C            ƒ„E:    T0 - BPEMŸ HA—A‹A B›BO„A;
C                    TM - BPEMŸ KOHE–A B›BO„A;
C                    KOUT - "KA†„“ž KOUT - “ž TO—K“ ˆ‡ PŸ„A COXPAHŸEM›X
C                            B›BO„H›X ‡HA—EHˆ‰ PE˜EHˆŸ (˜Aƒ COXPAHEHˆŸ
C                            HOUTT) B›BO„ˆTœ HA E—ATœ ";              0
C                      PPVENT - Pˆ = 0 HE E—ATAETCŸ ˆH”OPMA–ˆŸ B
C                                       MOMEHT KOMM“TA–ˆˆ;
C                               Pˆ = 1 - E—ATAETCŸ;
C                                 ( “MO‹—AHˆE = 0 );
C
C
C
C                  Pˆ “CE˜HOM PˆHŸTˆˆ APAMETPOB „ˆPEKTˆB› ˆ
C                   CˆCKA "‡AKA‡ B›BO„A" --> ‡AO‹HŸET O‹Ÿ
C                   COMMON/SERVZ6/ ˆ BO‡“†„AET ”‹Aƒ "PON",
C                    + BO‡“†„EHˆE MASPRT(106) <-> PPVENT;
C   METO„:         O—EBˆ„EH;
C   ------
C
C   APAMETP›:     KAR - CˆMBO‹œHAŸ CTPOKA TEKCTA „ˆPEKTˆB› #E—;
C   ---------            (OCTAB‹ŸETCŸ MOHˆTOPOM - CKAHEPOM );
C                  DATDIR - MACCˆB PˆHŸT›X —ˆCE‹ ˆ‡ KAR->(CTPOKA „ˆP.)
C                  MSWALL - CˆCOK COCTOŸHˆŸ ‹EMEHTOB –Eˆ;
C   O„POƒPAMM›:  1. GETSPS
C   ------------   2. COMMON PROF1, SERVZ6
C   PˆME—AHˆE:    1. B COMMON SERVZ6 ”ˆKCˆP“žTCŸ:
C   ----------        TBEGON, TENDON, KOUTON, SPISON
C                  2.  O “MO‹—AHˆž T0=0.0, TM=666.0, KOUT=1;
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/SERVZ6/ TBEGON, TENDON, KOUTON, SPISON(55)
         REAL TOKN(*),VALN(*)
         INTEGER SPISON
         DIMENSION KAR(*), DATDIR(*), MSWALL(*)
         NRDR = MASPRT(12)
         NWTR = MASPRT(13)
C--->  ECTœ ‹ˆ MSWALL
       IF ( MASPRT(74) .EQ. 1 ) GO TO 200
       WRITE(NWTR,201)
201    FORMAT(/' *** HET OˆCAHˆŸ –Eˆ - „ˆPEKTˆBA ˆƒHOPˆP“ETCŸ ***'/)
        RETURN
200    CONTINUE
C        ***************************
         KOUT = 1
         IPP = 0
C********READ(NRDR, PON)
       IF ( DATDIR(1) .EQ. 0.0 ) GO TO 100
C--> —TO-TO ECTœ
       T0 = DATDIR(2)
       TM = DATDIR(3)
       KOUT = IFIX( DATDIR(4) )
       IPP = IFIX( DATDIR(5) )
       IF ( IPP .EQ. 0 ) GO  TO   101
       IPP = 1
       GO TO 101
100    CONTINUE
C-->  “ M O ‹ — A H ˆ E
       T0 = 0.0
       TM = +666.0
       KOUT = 1
       IPP = 0
101    CONTINUE
C********WRITE(NWTR, PON)
         IF ( KOUT .LE. 0 ) KOUT = 1
         TBEGON = T0
         TENDON = TM
         KOUTON = KOUT
         WRITE(NWTR, 50) T0, TM, KOUT, IPP
50       FORMAT(/' *** #E— ¯à¨­ïâ :==> T0=',G12.5,' TM=',G12.5,
     =  ' KOUT=',I5, ' PPVENT=', I2, ' ***'/)
C--------
         CALL GETSPS(MSWALL, SPISON, IRC )
         GO TO (1, 2, 3 ), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR,10)
10       FORMAT(/' *** KOHE– ”A‰‹A "RDR" - PAOTA PEP›BAETCŸ ***'/
     = ' *** [ DRPRNT ] ***'/)
         call STOPP(16)
C######################################################################
2        CONTINUE
C--->  SPIS “CT....   CPOC PON....
         MASPRT(56) = 0
         MASPRT(106) = 0
         RETURN
C######################################################################
1        CONTINUE
C--> CˆCOK ECTœ.....  PON  "B‡„EPHEM"
         MASPRT(56) = 1
         MASPRT(106) = IPP
         RETURN
         END
