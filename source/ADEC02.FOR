       SUBROUTINE ADEC02( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT CEHTüÅPü  1988 É.)-----------
C   èOÑèPOÉPAMMA: ADEC02( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   ------------          IPORT1,T,IER )
C
C   HAáHAóEHàE:  OÅPAÅOTóàK  KOMAHÑ ìèPABãEHàü,èEPECõãKà
C   ----------   MàKPOèPOñECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAHÑA  "HALT" ( 000000 ):                          *
C              * OCTAHOB                                              *
C              *------------------------------------------------------*
C              * KOMAHÑA  "WAIT"  (000001 ):                          *
C              * OÜàÑAHàE                                             *
C              *------------------------------------------------------*
C              * KOMAHÑA  "RTI"   (000002 ):                          *
C              * BOáBPAT àá èPEPõBAHàü                                *
C              *------------------------------------------------------*
C              * KOMAHÑA  "BPT"   (000003 ):                          *
C              * KOMAHÑHOE èPEPõBAHàE Ñãü OTãATKà                     *
C              *------------------------------------------------------*
C              * KOMAHÑA  "IOT"  (000004 ):                           *
C              * KOMAHÑHOE èPEPõBAHàü Ñãü BBOÑ-BõBOÑA                 *
C              *------------------------------------------------------*
C              * KOMAHÑA  "RESET"( 000005 ):                          *
C              * CBPOC BHEòHàX ìCTPOàCTB                              *
C              *------------------------------------------------------*
C              * KOMAHÑA  "RTT" ( 000006 ):                           *
C              * BOáBPAT àá èPEPõBAHàü                                *
C              *------------------------------------------------------*
C              * KOMAHÑA  "JMP" ( 0001DD ):                           *
C              * ÅEáìCãOBHõâ èEPEXOÑ HA AÑPEC <DD>                    *
C              *------------------------------------------------------*
C              * KOMAHÑA  "RTS"  ( 00020R):                           *
C              * BOáBPAT àá èOÑèPOÉPAMMõ                              *
C              *------------------------------------------------------*
C              * KOMAHÑA  "NOP" ( 000240):                            *
C              * èìCTAü KOMAHÑA                                       *
C              *------------------------------------------------------*
C              * KOMAHÑA "CLC"  ( 000241):                            *
C              * OóàCTKA îãAÉA èEPEHOC  C                             *
C              *------------------------------------------------------*
C              * KOMAHÑA "CLV" ( 000242 ):                            *
C              * OóàCTKA îãAÉA  APàîMETàóECKOE èEPEèOãHEHàE V         *
C              *------------------------------------------------------*
C              * KOMAHÑA "CLZ" ( 000244 ):                            *
C              * OóàCTKA îãAÉA HìãEBOâ PEáìãúTAT Z                    *
C              *------------------------------------------------------*
C              * KOMAHÑA "CLN"  (000250) :                            *
C              * OóàCTKA îãAÉA OTPàñATEãúHõâ PEáìãúTAT N              *
C              *------------------------------------------------------*
C              * KOMAHÑA "CCC"  (000257 ):                            *
C              * OóàCTKA BCEX îãAÉOB                                  *
C              *------------------------------------------------------*
C              * KOMAHÑA "SEC"  (000261) :                            *
C              * ìCTAHOBKA îãAÉA èEPEHOC C                            *
C              *------------------------------------------------------*
C              * KOMAHÑA "SEV"  ( 000262 ):                           *
C              * ìCTAHOBKA îãAÉA APàîMETàóECKOE èEPEèOãHEHàE V        *
C              *------------------------------------------------------*
C              * KOMAHÑA "SEZ"  ( 000264):                            *
C              * ìCTAHOBKA îãAÉA HìãEBOâ PEáìãúTAT Z                  *
C              *------------------------------------------------------*
C              * KOMAHÑA  "SEN" ( 000270 ):                           *
C              * ìCTAHOBKA îãAÉA OTPàñATEãúHõâ PEáìãúTAT  N           *
C              *------------------------------------------------------*
C              * KOMAHÑA  "SCC"  ( 000277 )                           *
C              * ìCTAHOBKA BCEX PAáPüÑOB                              *
C              *------------------------------------------------------*
C              * KOMAHÑA  "SWAB" ( 0003DD ):                          *
C              * èEPECTAHOBKA ÅAâTOB <DD>                             *
C              *------------------------------------------------------*
C              * KOMAHÑA  "BR" ( 000400):                             *
C              * BETBãEHàE ÅEáìCãOBHOE                                *
C              *------------------------------------------------------*
C
C   METOÑ:      ãOÉàóECKOE MOÑEãàPOBAHàE.
C   -----
C
C   èAPAMETPõ:  BXOÑHõE :
C  ----------      KD     - KOÑ KOMAHÑõ  ;
C                  REG(8) - MACCàB PEÉàCTPOB MàKPOèPOñECCOPA ;
C                           REG(1) --> R0
C                           REG(2) --> R1
C                           REG(3) --> R2
C                           REG(4) --> R3
C                           REG(5) --> R4
C                           REG(6) --> R5
C                           REG(7) --> R6 ( ìKAáATEãú CTEKA )
C                           REG(8) --> R7 ( CóETóàK KOMAHÑ  )
C                  FLAG(6)- MACCàB îãAÉOB CCè MàKPOèPOñECCOPA ;
C                           FLAG(1) --> îãAÉ  C (èEPEHOC)
C                           FLAG(2) --> îãAÉ  V (APàîM. èEPEèOãHEHàE)
C                           FLAG(3) --> îãAÉ  Z (HìãEBOâ PEáìãúTAT)
C                           FLAG(4) --> îãAÉ  N (OTPàñAT.PEáìãúTAT)
C                           FLAG(5) --> îãAÉ  T
C                           FLAG(6) --> îãAÉ I/O(èPàOPàTET)
C                  ADPRTO - AÑPEC èOPTA BõBOÑA ;
C                  ADPRTI - AÑPEC èOPTA BBOÑA ;
C                  ADPROG - MAKCàMAãúHõâ AÑPEC Oáì ,B èPEÑEãAX KOTOPOÉO
C                           PAÅOTAET ìèPABãüûôAü èPOÉPAMMA ;
C                  IPORT  - COÑEPÜàMOE èOPTA BõBOÑA ;
C                  IPORT1 - COÑEPÜàMOE èOPTA BBOÑA ;
C
C
C               BõXOÑHõE :
C                  T      - BPEMü BõèOãHEHàü KOMAHÑõ ;
C                  IER    - ìKAáATEãú OòàÅKà:
C                           IER=0  -  OòàÅOK HE OÅHAPìÜEHO ;
C                           IER=1  -  OòàÅOóHAü áAèàCú KOMAHÑõ ãàÅO
C                                     BõXOÑ áA èPEÑEãõ Oáì ;
C                           IER=2  -  èPOàáOòãO OÅPAôEHàE K üóEâKE
C                                     èAMüTà, àMEûôEâ HEóETHõâ AÑPEC.
C                           IER=3  -  HE HAòA KOMAHÑA ;
C                           IER=4  -  OÅPAôEHàE K èOPTì B/B
C                                     HE èO TOMì AÑPECì ;
C                           IER=5  -  KOMAHÑA èOKA HE PEAãàáOBAHA ;
C                           IER=6  -  KOMAHÑA HE CìôECTBìET ;
C
C   èOÑèPOÉPAMMõ:  1. ADECSI, ADECDB, ADECBD ;
C   ------------
C
C   èPàMEóAHàE:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG, KB(16), KB1(16)
       IER = 0
C--> PAáOâÑEMCü èO KOMAHÑAM :
       KOM = KD / 64 + 1
       GO TO ( 5,100,200,300,400,400,400,400 ), KOM
       GO TO 9903
5      CONTINUE
       KD0 = KD + 1
       GO TO ( 10,20,30,40,50,60,70,9906 ), KD0
C----------------------------------------------------------------------
C              KOMAHÑA  "HALT"
C              KOMAHÑA  "WAIT"
C----------------------------------------------------------------------
10     CONTINUE
20     CONTINUE
C --> HE PEAãàáOBAãúHO
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "RTI"
C----------------------------------------------------------------------
30     CONTINUE
C--> HE PEAãàáOBAH T-PAáPüÑ
C--->  BOCTAHABãàBAETCü COÑEPÜàMOE CK
       REG(8) = MEM(REG(7)+1)
       REG(7) = REG(7) + 2
C--> BOCTAHABãàBAETCü PEÉàCTP COCTOüHàü èPOñECCOPA
       ICCP = MEM( REG(7) + 1 )
       REG(7) = REG(7) + 2
C---> èPOBEPüEM ìKAáATEãú CTEKA ,HE BõòEã ãà áA èPEÑEãA ...
        IF ( REG(7) .GT.1024 ) GO TO 9901
C--> OèPEÑEãàM îãAÉà .
       CALL ADECDB( ICCP, KB, IER )
       IF( IER .NE. 0 ) RETURN
       FLAG(1) = KB(1)
       FLAG(2) = KB(2)
       FLAG(3) = KB(3)
       FLAG(4) = KB(4)
       FLAG(5) = KB(5)
       FLAG(6) = KB(8)
C--> BPEMü ?
       T = 10.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "BPT"
C              KOMAHÑA  "IOT"
C              KOMAHÑA  "RESET"
C----------------------------------------------------------------------
40     CONTINUE
50     CONTINUE
60     CONTINUE
C--> HE PEAãàáOBAãúHO
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "RTT"
C----------------------------------------------------------------------
70     CONTINUE
C--> HE PEAãàáOBAH T-PAáPüÑ
C--->  BOCTAHABãàBAETCü COÑEPÜàMOE CK
       REG(8) = MEM(REG(7)+1)
       REG(7) = REG(7) + 2
C--> BOCTAHABãàBAETCü PEÉàCTP COCTOüHàü èPOñECCOPA
       ICCP = MEM( REG(7) + 1 )
       REG(7) = REG(7) + 2
C---> èPOBEPüEM ìKAáATEãú CTEKA ,HE BõòEã ãà áA èPEÑEãA ...
        IF ( REG(7) .GT.1024 ) GO TO 9901
C--> OèPEÑEãàM îãAÉà .
       CALL ADECDB( ICCP, KB, IER )
       IF( IER .NE. 0 ) RETURN
       FLAG(1) = KB(1)
       FLAG(2) = KB(2)
       FLAG(3) = KB(3)
       FLAG(4) = KB(4)
       FLAG(5) = KB(5)
       FLAG(6) = KB(8)
C--> BPEMü ?
       T = 10.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "JMP"
C----------------------------------------------------------------------
100    CONTINUE
C-->èOKA HE áAèPEôAETCü PEÉàCTPOBAü AÑPECAñàü  ....
C--> àôEM OèEPAHÑ àCTOóHàK ( AÑPEC--> IADRS,  óàCãO--> IDATA )
       KD1 =  KD - 64
       CALL ADECSI( KD1, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 110
C--> áAèPOC HA BBOÑ ÑAHHõX àá èOPTA BBOÑA
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT1
110    CONTINUE
C---> èPOBEPüEM óETHOCTú AÑPECA èEPEXOÑA ...
       TMD = MOD(IDATA,2)
CCC    WRITE (6,*) TMD,IDATA
       IF ( TMD .NE. 0 ) GO TO 9902
       REG(8) = IDATA
C--> èOCóàTAEM BPEMü
       T  = TI + 4.0E-6
       RETURN
C######################################################################
200    CONTINUE
       KD1 = KD - 128
       KD2 = KD1/8 + 1
       GO TO (207, 9906, 9906, 9906, 239, 249, 260, 269 ),KD2
C----------------------------------------------------------------------
C              KOMAHÑA  "RTS"
C----------------------------------------------------------------------
207    CONTINUE
       KD1 = KD - 128
C--> èPOBEPüEM AÑPECAHT üBãüETCü PEÉàCTOPOM
       IF ( KD1 .GE. 8 ) GO TO 9901
       REG(8) = REG(KD1+1)
       REG(KD1+1) = MEM(REG(7)+1)
       REG(7) = REG(7) + 2
C---> èPOBEPüEM ìKAáATEãú CTEKA ,HE BõòEã ãà áA èPEÑEãA ...
        IF ( REG(7) .GT.1024 ) GO TO 9901
       T = 7.0E-6
       RETURN
C######################################################################
239    CONTINUE
       IF( KD .EQ. 160 ) GO TO 240
       IF( KD .EQ. 161 ) GO TO 241
       IF( KD .EQ. 162 ) GO TO 242
       IF( KD .EQ. 164 ) GO TO 244
       GO TO 9906
C----------------------------------------------------------------------
C              KOMAHÑA  "NOP"
C----------------------------------------------------------------------
240    CONTINUE
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "CLC"
C----------------------------------------------------------------------
241    CONTINUE
       FLAG(1) = 0
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "CLV"
C----------------------------------------------------------------------
242    CONTINUE
       FLAG(2) = 0
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "CLZ"
C----------------------------------------------------------------------
244    CONTINUE
       FLAG(3) = 0
       T = 4.0E-6
       RETURN
249    CONTINUE
       IF( KD .EQ. 168) GO TO 250
       IF( KD .EQ. 175) GO TO 257
       GO TO 9906
C----------------------------------------------------------------------
C              KOMAHÑA  "CLN"
C----------------------------------------------------------------------
250    CONTINUE
       FLAG(4) = 0
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "CCC"
C----------------------------------------------------------------------
257    CONTINUE
       DO 258 I=1,4
       FLAG(I) = 0
258    CONTINUE
       T = 4.0E-6
       RETURN
C######################################################################
260    CONTINUE
       IF( KD .EQ. 177) GO TO 261
       IF( KD .EQ. 178) GO TO 262
       IF( KD .EQ. 180) GO TO 264
       GO TO 9906
C----------------------------------------------------------------------
C           KOMAHÑA "SEC"
C----------------------------------------------------------------------
261    CONTINUE
       FLAG(1) = 1
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "SEV"
C----------------------------------------------------------------------
262    CONTINUE
       FLAG(2) = 1
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "SEZ"
C----------------------------------------------------------------------
264    CONTINUE
       FLAG(3) = 1
       T = 4.0E-6
       RETURN
C######################################################################
269    CONTINUE
       IF( KD .EQ. 184 ) GO TO 270
       IF( KD .EQ. 191 ) GO TO 277
       GO TO 9906
C----------------------------------------------------------------------
C              KOMAHÑA  "SEN"
C----------------------------------------------------------------------
270    CONTINUE
       FLAG(4) = 1
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA  "SCC"
C----------------------------------------------------------------------
277    CONTINUE
       DO 278 I=1,4
       FLAG(I) = 1
278    CONTINUE
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
C       KOMAHÑA  "SWAB"
C----------------------------------------------------------------------
300    CONTINUE
C--> àôEM OèEPAHÑ àCTOóHàK ( AÑPEC--> IADRS,  óàCãO--> IDATA )
       KD1 =  KD - 192
       CALL ADECIS( KD1, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 310
C--> áAèPOC HA BBOÑ ÑAHHõX àá èOPTA BBOÑA
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT
310    CONTINUE
       IDATA1 = IDATA/256
       IDATA2 = IDATA - IDATA1*256
C-->èOMEHüEM ÅAâTõ...
       IDATA = IDATA2 * 256 + IDATA1
C--> áAèàCõBAEM PEáìãúTAT
       IF( IADRS.LT.0)  GO TO 340
       MEM( IADRS+1) = IDATA
       MEM( IADRS+2) = IDATA/256
       GO TO 350
340    CONTINUE
       IF( IADRS.LT.-8) GO TO 345
       REG(-IADRS) = IDATA
       GO TO 350
345    CONTINUE
       IPORT = IDATA
350    CONTINUE
C-->> èOCóàTAEM BPEMü
       T = TI + 4.4E-6
       IF (TI.EQ.0) T= 4.8E-6
C--> áAâMEMCü  èPàáHAKAMà
      FLAG(1) = 0
      FLAG(2) = 0
      FLAG(3) = 0
      IF(IDATA1.EQ.0) FLAG(3) = 1
      FLAG(4) = 0
      IF( IDATA1.GT.128 ) FLAG(4) = 1
      RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "BR"
C----------------------------------------------------------------------
400    CONTINUE
C--> CMEôEHàE
       ISM = KD - 256
       IF( ISM .GE. 128 ) ISM = ISM - 256
C--> CMEôAEMCü
       REG(8) = REG(8) + 2*ISM
C--> BPEMü ?
       T = 4.0E-6
       RETURN
C----------------------------------------------------------------------
9901   CONTINUE
       IER = 1
       RETURN
C----------------------------------------------------------------------
9902   CONTINUE
       IER = 2
CCCC   WRITE (6,*) IER
       RETURN
C----------------------------------------------------------------------
9903   CONTINUE
       IER = 3
       RETURN
C----------------------------------------------------------------------
9904   CONTINUE
       IER = 4
       RETURN
C----------------------------------------------------------------------
9906   CONTINUE
       IER = 6
       RETURN
       END
