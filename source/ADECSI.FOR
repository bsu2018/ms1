       SUBROUTINE ADECSI( KD, ADPROG, REG, IADRS, IDATA, TI, IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT    MAü    1988 É.)-----------
C   èOÑèPOÉPAMMA: ADECSI( KD, ADPROG, REG, IADRS, IDATA, TI, IER )
C   -------------
C   HAáHAóEHàE:  èOàCK èO KOÑì AÑPECAñàà  KD ( ÑBìXPAáPüÑHOE Q-óàCãO )
C   ----------   AÑPECA à ÑAHHOÉO B Oáì  àãà B  POH .
C                ( Ñãü KOMAHÑ : JMP,JSR   )
C                ******************************************************
C
C   METOÑ:         èEPEKOÑàPOBKA, OèPEÑEãEHàE METOÑA AÑPECAñàà à èOàCK.
C   -----
C
C   èAPAMETPõ:  BXOÑHõE :
C  ----------      KD     - KOÑ AÑPECAñàà ;
C                  REG(8) - MACCàB PEÉàCTPOB MàKPOèPOñECCOPA ;
C
C               BõXOÑHõE :
C                  IADRS  - AÑPEC  àCKOMOÉO ÑAHHOÉO :
C                           ECãà IADRS >= 0 - AÑPEC üóEâKà èAMüTà;
C                           ECãà IADRS <  0 - AÑPEC PEÉàCTPA .
C                                             ãàÅO AÑPEC èOPTA B/B .
C                  IDATA  - óàCãO, XPAHüôEECü èO AÑPECì 'IADRS';
C                  TI     - BPEMü BõèOãHEHàü KOMAHÑõ ;
C                  IER    - ìKAáATEãú OòàÅKà:
C                           IER=0  -  OòàÅOK HE OÅHAPìÜEHO ;
C                           IER=1  -  OòàÅOóHõâ KOÑ AÑPECAñàà ,
C                                     ãàÅO BõXOÑ áA èPEÑEãõ Oáì ;
C                           IER=2  -  èOãìóEHHõâ AÑPEC üóEâKà èAMüTà
C                                     'IADRS' HE óETHõâ .
C
C   èOÑèPOÉPAMMõ:  1. HET.
C   ------------
C
C   èPàMEóAHàE:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), ADPROG
       IER = 0
C--> KAKOâ METOÑ AÑPECAñàà ?
       METOD = KD/8 + 1
       NREG  = KD - 8*(METOD - 1) + 1
       GO TO (100, 200, 300, 400, 500, 600, 700, 800 ), METOD
       GO TO 9990
C######################################################################
C
C               èPüMAü  AÑPECAñàü
C
C######################################################################
100    CONTINUE
       GOTO 9990
C----------------------------------------------------------------------
300    CONTINUE
C--> ABTOàHKPEMEHTHõâ METOÑ AÑPECAñàà
C----------------------------------------------------------------------
       IADRS = REG(NREG)
       T = 1.6E-6
       REG(NREG) = REG(NREG) + 2
       GO TO 900
C----------------------------------------------------------------------
500    CONTINUE
C--> ABTOÑEKPEMEHTHõâ METOÑ AÑPECAñàà
C----------------------------------------------------------------------
       REG(NREG) = REG(NREG) - 2
       IADRS = REG(NREG)
       T = 1.6E-6
       GO TO 900
C----------------------------------------------------------------------
700    CONTINUE
C--> àHÑEKCHõâ METOÑ AÑPECAñàà
C----------------------------------------------------------------------
       INDEX = MEM( REG(8) + 1)
       REG(8) = REG(8) + 2
       IADRS = REG(NREG) + INDEX
       T = 2.8E-6
       GO TO 900
C######################################################################
C
C            KOCBEHHAü  AÑPECAñàü
C
C######################################################################
200    CONTINUE
C--> KOCBEHHO-PEÉàCTPOBõâ METOÑ AÑPECAñàà
C----------------------------------------------------------------------
       IADRS = REG(NREG)
       T = 0.8E-6
       GO TO 900
C----------------------------------------------------------------------
400    CONTINUE
C--> KOCBEHHO-ABTOàHKPEMEHTHõâ METOÑ AÑPECAñàà
C----------------------------------------------------------------------
       IADRS = MEM( REG(NREG)+1)
       T = 2.0E-6
       REG(NREG) = REG(NREG) + 2
       GO TO 900
C----------------------------------------------------------------------
600    CONTINUE
C--> KOCBEHHO-ABTOÑEKPEMEHTHõâ METOÑ AÑPECAñàà
C----------------------------------------------------------------------
       REG(NREG) = REG(NREG) - 2
       IADRS = MEM( REG(NREG)+1)
       T = 2.8E-6
       GO TO 900
C----------------------------------------------------------------------
800    CONTINUE
C--> KOCBEHHO-àHÑEKCHõâ METOÑ AÑPECAñàà
C----------------------------------------------------------------------
       INDEX = MEM( REG(8) + 1)
       REG(8) = REG(8) + 2
       IAD   =  REG(NREG) + INDEX
       IADRS = MEM(IAD+1)
       T = 4.8E-6
C----------------------------------------------------------------------
900    CONTINUE
C--> èPOBEPàM AÑPEC à áAèOMHàM ÑAHHOE.
       IF( IADRS .GE. 57344  .AND. IADRS .LT. 65536 ) GO TO 8888
       IF( IADRS .LT. 0      .OR.  IADRS .GT. ADPROG) GO TO 9990
       IF( MOD(IADRS,2) .GT. 0 ) GO TO 9999
       IDATA = MEM(IADRS+1)
       RETURN
C----------------------------------------------------------------------
8888   CONTINUE
C--> OÅPATàãàCú K èOPTì B/B.
       IADRS = - IADRS
       RETURN
C----------------------------------------------------------------------
9990   CONTINUE
       IER = 1
       RETURN
C----------------------------------------------------------------------
9999   CONTINUE
       IER = 2
       RETURN
C----------------------------------------------------------------------
       END
