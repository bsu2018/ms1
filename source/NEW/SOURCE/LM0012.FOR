       SUBROUTINE LM0012( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT   MAü   1987 É.)------------
C   èOÑèPOÉPAMMA: LM0012( MSW, RMSW, ADR, MSWALL, RMSWAL, KOD )
C   -------------
C
C   HAáHAóEHàE:   ãOÉàóECKAü HACTPOâKA MSW MAKPOMOÑEãEâ Tàè 999:
C   ----------    ***  MEXAHàóECKAü CàCTEMA ***
C
C   METOÑ:        èOàCK B MSW_ALL OèàCAHàà ùãEMEHTOB , AÑPECOB áAÑAHHõX
C   -----         TOKOB ùãEMEHTOB B MACCàAX TOKN/TOKN1.
C                 B CãìóAE HAXOÜÑEHàü -> ÑOîOPMàPOBAHàE MSW_OèàCAHàü;
C                 B ùTOM CãìóAE KOD = 0, àHAóE = 1;
C
C   èAPAMETPõ:    MSW(RMSW) - "OTPEáOK" MSW_MOÑEãà Tàè # 999;
C   ----------    ADR - AÑPEC B MSW_ALL MSW_OèàCAHàü ùTOâ MOÑEãà;
C                 MSWALL(RMSWAL) - BECú CèàCOK MSW CãOB;
C                 KOD - = 0 - CîOPMàPOBAãà MSW,
C                       = 1 - ãOÉ. OòàÅKà;
C                      ( HA NWTR COOÅôEHàE BõBOÑàTCü );
C   èOÑèPOÉPAMMõ: 1. PROF1, PROF2
C   ------------  2. SEQMS0
C
C
C              îOPMAT RMSW Tàè # 999:
C              ----------------------
C
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II KOã-BO I KOã-BO I # áBEHAI # ùã-TAI # CME- I AÑPEC  I ...==>
C     II áBEHúEBI CBüáEâ I MEX.   I ùã.    I ôEHàü  I   B    I ...==>
C     II        I C ùã.ñ.I ñEèà   I ñEèà   I        I TOKN/N1I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   1    I    2   I   3    I   4    I   5    I   6    I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C     II   9    I   10   I   11   I   12   I   13   I   14   I ...==>
C     ++--------+--------+--------+--------+--------+--------+ ...==>
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       COMMON /PROF2/ MACTAB(800)
       DIMENSION MSW(*), RMSW(*), MSWALL(*), RMSWAL(*)
       INTEGER TIP, ADR, NELMOT, AMOT, ATOKM
C----------------------------------------------------------------------
C-->  HAóAãà...
C----------------------------------------------------------------------
       KOD = 0
       NEL = MSW(1)
       TIP = MSW(2)
       IF( TIP .NE. 999 ) GO TO 9999
C----------------------------------------------------------------------
C--> CKOãúKO CBüáEâ C ùãEKTPàóECKOâ ñEèúû ?
C----------------------------------------------------------------------
       KELC = MSW(10)
       IF( KELC .LE. 0 ) RETURN
C----------------------------------------------------------------------
C--> Ñãü KAÜÑOâ CBüáà àôEM...
C----------------------------------------------------------------------
          DO 15 I= 1 , KELC
          NELMOT = MSW(8 + 4*I)
          NSM    = MSW(9 + 4*I)
C----------------------------------------------------------------------
C-->  àCKATú B MSWALL ùãEMEHT C #ùã-TA = NELMOT:
C----------------------------------------------------------------------
          CALL SEQMS0(MSWALL, NELMOT, AMOT, IRC, NSMMAX, ATOKM )
C----------------------------------------------------------------------
C-->  HAòãà àãà HET ?
C----------------------------------------------------------------------
          IF   ( IRC .EQ. 1 ) GO TO 1
C----------------------------------------------------------------------
C-->  ùãEMEHTA HET...
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR, 500) NEL, TIP, NELMOT
500    FORMAT(//' *** áAèPOC HA ãOÉ. CBüáú ùãEMEHTA # ', I5,
     =  ' ( Tàè # ', I3, ' ) ***'/
     =  ' *** C HE CìôECTBìûôàM B ñEèà ùãEMEHTOM # ', I5,' ***'/
     =  ' *** OÅPAÅOTKA ñEèà èPEKPAôAETCü *** '//)
       RETURN
C----------------------------------------------------------------------
1      CONTINUE
C-->  MSW - HAòãà, ECTú ãà ì ùãEMEHTA TPEÅìEMõâ TOK?
       IF ( NSM .LE. NSMMAX ) GO TO 3
C----------------------------------------------------------------------
C-->  CBüáú C TOKOM , KOTOPOÉO HE CìôECTBìET ì áAÑAHHOÉO ùãEMEHTA.
       KOD = 1
       NWTR = MASPRT(13)
       WRITE(NWTR,501) NEL, TIP, NELMOT, NSM
501    FORMAT(//' *** áAèPOC HA ãOÉ. CBüáú ùãEMEHTA # ', I5,
     = ' ( Tàè # ', I3, ' ) ***'//
     = ' *** C ùãEMEHTOM # ', I5, ' ( CMEôEHàE',I3, ' ) - OòàÅOóEH',
     = ' ùãEMEHT HE àMEET TPEÅìEMOÉO TOKA ***'/
     = ' *** OÅPAÅOTKA ñEèà èPEKPAôAETCü ***'//)
       RETURN
3      CONTINUE
          MSW(10+4*I) = ATOKM + NSM - 1
15     CONTINUE
C######################################################################
C----------------------------------------------------------------------
C--> áAKOHóàãà!
       KOD = 0
       RETURN
C======================================================================
C!!!-------------------------------------------------------------------
9999   CONTINUE
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 555) TIP, NEL
555      FORMAT(' *** ãOÉ. CÅOâ B LM0012: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
       RETURN
       END
