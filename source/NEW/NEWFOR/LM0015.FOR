       SUBROUTINE LM0015( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT €¢£ãáâ  1992 ƒ.)------------
C   O„POƒPAMMA: LM0015( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HA‡HA—EHˆE:   ‹Oƒˆ—ECKAŸ HACTPO‰KA MSW MAKPOMO„E‹E‰ TˆOB
C   ----------        351- ¥£ã«ïâ®à ¤«ï ¢¥­â¨«ì­®£® ¬®áâ  ‹ à¨®­®¢ 
C                à¥£ã«¨àã¥âáï ¯® ¯à®¨§¢®«ì­®¬ã § ¤ ­­®¬ã § ª®­ã
C
C   METO„:        OˆCK B MSW_ALL OˆCAHˆˆ ‹EMEHTOB , A„PECOB ‡A„AHH›X
C   -----         TOKOB ‹EMEHTOB B MACCˆAX TOKN/TOKN1.
C                 B C‹“—AE HAXO†„EHˆŸ -> „O”OPMˆPOBAHˆE MSW_OˆCAHˆŸ;
C                 B TOM C‹“—AE KOD = 0, ˆHA—E = 1;
C
C   APAMETP›:    MSW(RMSW) - "OTPE‡OK" MSW_MO„E‹ˆ Tˆ # 301;
C   ----------    ADR - A„PEC B MSW_ALL MSW_OˆCAHˆŸ TO‰ MO„E‹ˆ;
C                 MSWALL(RMSWAL) - BECœ CˆCOK MSW C‹OB;
C                 KOD - = 0 - C”OPMˆPOBA‹ˆ MSW,
C                       = 1 - ‹Oƒ. O˜ˆKˆ;
C                      ( HA NWTR COO™EHˆE B›BO„ˆTCŸ );
C   O„POƒPAMM›: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C                      ”OPMAT RMSW Tˆ # 351
C                 ------------------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0021    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - ”‹Aƒ HOB›E ‡HA—. B›—ˆC‹EH›
C            I              I      I   ( „‹Ÿ CˆHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  DM  I  NMIN - HOMEP ‹ˆ†A‰˜E‰ MEHœ˜E‰
C    12      I     3        I  DM  I  TIMEU - ‚à¥¬ï ¢ë¤ ç¨ á¨£­ «  ã¯à.
C    13      I     4        I  DM  I  SIGOLD 
C    14      I     5        I  DM  I  GAMMA  
C    15      I     6        I  DM  I  TOKOLD 
C    16      I     7        I      I         
C    17      I     8        I      I         
C    18      I     9        I      I         
C    19      I     10       I      I         …‡…‚
C    20      I     11       I      I         
C    21      I     12       I      I         
C    22      I     13       I      I         
C    23      I     14       I  FM  I  #‹1-        
C    24      I     15       I  FM  I  #‘Œ1       
C    25      I     16       I  FM  I  #€’ŽŠŒ1 - “¯à ¢«ï¥¬ë© â®ª      
C    26      I     17       I  FM  I  OMEG - frequenc;
C    27      I     18       I  FM  I  EDM  - €¬¯«¨âã¤  ¯¨â îé. ¨áâ®ç.;
C    28      I     19       I  LM  I  E    - à®â¨¢®…„‘
C    29      I     20       I  FM  I  XG  - ˆ­¤ãªâ¨¢­®¥ á®¯à®â¨¢.
C    30      I     21       I  FM  I  X   - ˆ­¤ãªâ¨¢­®¥ á®¯à®â¨¢.
C    31      I     22       I  FM  I  NT  - Š®«¨ç¥áâ¢® §­ ç¥­¨ï
C    32      I     23       I  FM  I  T1 
C    33      I     24       I  FM  I  T2 
C    31+2*nt I    22+2*NT   I  FM  I  I(NT/2)
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 31 + 2*NT
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 22+2*NT
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       COMMON /PROF2/ MACTAB(880)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM
C----------------------------------------------------------------------
C-->  HA—A‹ˆ...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
200   CONTINUE
          NELMOT = MSW(23)
          NSM = MSW(24)
C----------------------------------------------------------------------
C-->  ˆCKATœ B MSWALL ‹EMEHT C #‹-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HA˜‹ˆ ˆ‹ˆ HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 4
C----------------------------------------------------------------------
C-->  ‹EMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 500) NEL, TIP, NELMOT
500    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     =  ' ( Tˆ # ', I3, ' ) ***'/
     =  ' *** C HE C“™ECTB“ž™ˆM B –Eˆ ‹EMEHTOM # ', I5,' ***'/
     =  ' *** OPAOTKA –Eˆ PEKPA™AETCŸ *** '//)
       RETURN
C----------------------------------------------------------------------
4      CONTINUE
C-->  MSW - HA˜‹ˆ, ECTœ ‹ˆ “ ‹EMEHTA TPE“EM›‰ TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 6
C----------------------------------------------------------------------
C-->  CBŸ‡œ C TOKOM , KOTOPOƒO HE C“™ECTB“ET “ ‡A„AHHOƒO ‹EMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,501) NEL, TIP, NELMOT, NSM
501    FORMAT(//' *** ‡APOC HA ‹Oƒ. CBŸ‡œ ‹EMEHTA # ', I5,
     = ' ( Tˆ # ', I3, ' ) ***'//
     = ' *** C ‹EMEHTOM # ', I5, ' ( CME™EHˆE',I3, ' ) - O˜ˆO—EH',
     = ' ‹EMEHT HE ˆMEET TPE“EMOƒO TOKA ***'/
     = ' *** OPAOTKA –Eˆ PEKPA™AETCŸ ***'//)
       RETURN
6      CONTINUE
          MSW(25) = ATOKM + NSM - 1
C----------------------------------------------------------------------
C--> ‡AKOH—ˆ‹ˆ!
       KOD = 0
       RETURN
       END
