       SUBROUTINE FM0025(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT  „EKAPŸ  1987 ƒ.)-----------
C   O„POƒPAMMA: FM0025(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹E‰ –ˆ”POB›X ‹Oƒˆ—ECKˆX ‹EMEHTOB:
C   ----------
C               +-----+--------------------------------------+
C               I     I                                      I
C               I Tˆ I    HAˆMEHOBAHˆE ‹EMEHTA             I
C               I     I                                      I
C               +-----+--------------------------------------+
C               I 330 I –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT   "ˆ"    I
C               +-----+--------------------------------------+
C               I 331 I –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT   "ˆ‹ˆ"  I
C               +-----+--------------------------------------+
C               I 332 I –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT"ˆCK‹.ˆ‹ˆ"I
C               +-----+--------------------------------------+
C
C                Pˆ‡HAK TIP # 4 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0029, LM0007 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C
C                      „‹ˆHA:  „‹Ÿ BCEX TˆOB :==>+1<VAR>
C----------------------------------------------------------------------
C              ‹EMEHT› OˆC›BAžTCŸ:
C
C          # T 33X “ 0 
C              MAS < NBXOD  #‹1 #CM1  #‹2 #CM2   ... >;
C
C         ƒ„E
C          NBXOD  -   KO‹ˆ—ECTBO TOKOB ‹EMEHTOB CXEM› , KOTOP›E
C                     ˆCO‹œ‡“žTCŸ B KA—ECTBE BXO„H›X CˆƒHA‹OB;
C          #‹I,#CMI- HOMEP ‹EMEHTA ˆ HOMEP TOKA ‹EMEHTA ,
C                     ˆCO‹œ‡“EMOƒO B KA—ECTBE BXO„HOƒO CˆƒHA‹A;
C----------------------------------------------------------------------
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ
C   -----
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW Pˆ FGMSW = 0;
C                  LM - „‹ˆHA BCEƒO MSW OˆCAHˆŸ      Pˆ FGMSW = 1;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ B MˆHˆMA‹œHOM O"EME;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C
C              ”OPMAT RMSW Tˆ # 33X:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0025    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I         PE‡EPB
C            I              I      I
C    11      I     2        I  FM  I  NBXOD-KO‹ˆ—ECTBO TOKOB, ˆCO‹œ-
C            I              I      I        ‡“EM›X KAK BXO„H›E CˆƒHA‹›;
C    12      I     3        I  FM  I  #‹1 - HOMEP ‹EMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ‹EMEHTA #‹1;
C    14      I     5        I  LM  I  ATOKM1 - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ‹EMEHTA #‹1;
C .......    I   .......    I  ... I  ..........................
C  9+I*3     I     I*3      I  FM  I  #‹I -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 11 + 3*NBXOD
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 2 + 3*NBXOD
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       NEL = BA0T(1)
       TIP = BA0T(2)
       NTIP= TIP - 329
       IF ( NTIP .GE. 1 .AND. NTIP .LE. 3 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0025 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->           –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT   "ˆ"
C --->           –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT   "ˆ‹ˆ"
C --->           –ˆ”POBO‰ ‹Oƒˆ—ECKˆ‰ ‹EMEHT"ˆCK‹.ˆ‹ˆ"
C----------------------------------------------------------------------
C--> OPE„E‹EHˆE KOHCTAHT
       NBXOD = IFIX ( PAR0T(2) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  “TO—HˆTœ LM <O™“ž >
       LM = 11 + 3*NBXOD
       RETURN
C######################################################################
1000   CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 2 + 3*NBXOD
       DO 20 I=1,LM
20     RMSW(I) = 0.0
C---> POBEPˆM  PABˆ‹œHO ‹ˆ BBE„EH› „AHH›E?
       NBXOD1 = ( IFIX( PAR0T(1)) - 1 ) / 2
       IF ( NBXOD .EQ. NBXOD1 ) GO TO 22
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** O˜ˆKA B FM0025 -->HEPABˆ‹œHO ‡A„AHO KO‹ˆ—ECTBO TO
     =—EK ***'/' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
       CALL SUBERR(TIP)
C!!!-------------------------------------------------------------------
22     CONTINUE
C---> ‡AOMHˆM HOMEPA BXO„H›X TOKOB.
       N = NBXOD
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
          DO 25 I = 1 , NBXOD
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          IF( N .GE. NEL ) GO TO 9999
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IRMSW = IRMSW + 1
25     CONTINUE
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) NEL, TIP,N
555    FORMAT(/' *** O˜ˆKA B FM0025 -->HEPABˆ‹œHO ‡A„AH› BXO„H›E „AHH›
     =E   ***'/' *** HA BXO„ ‹EMEHTA ', I5, ' HOMEP TˆA ', I5,' ***',
     =         ' *** O„AETCŸ TOK ‹EMEHTA, ˆMEž™EƒO O‹œ˜ˆ‰ OPŸ„KOB›‰
     =HOMEP ( # ' ,I3,' )  *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
       END
