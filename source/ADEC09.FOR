       SUBROUTINE ADEC09( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (AT OCTOBER   1989 É.)-----------
C   èOÑèPOÉPAMMA: ADEC09( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )
C
C   HAáHAóEHàE:  OÅPAÅOTóàK  OÑHOAÑPECHõX KOMAHÑ
C   ----------   MàKPOèPOñECCOPA  K1801BM1(2,3) ( PAÅOTA C ÅAâTAMà )
C              *------------------------------------------------------*
C              * KOMAHÑA  "EMT"  ( 104000 ):                          *
C              *                                                      *
C              *------------------------------------------------------*
C              * KOMAHÑA  "TRAP"  ( 104400 ):                          *
C              *                                                      *
C              *------------------------------------------------------*
C              * KOMAHÑA  "CLRB"   (1050DD ):                          *
C              * OóàCTKA .                                            *
C              *------------------------------------------------------*
C              * KOMAHÑA  "COMB"   (1051DD ):                          *
C              * àHBEPTàPOBAHàE .                                     *
C              *------------------------------------------------------*
C              * KOMAHÑA  "INCB"   (1052DD ):                          *
C              * èPàÅABãEHàE EÑàHàñõ .                                *
C              *------------------------------------------------------*
C              * KOMAHÑA  "DECB"  (1053DD ):                           *
C              * BõóàTAHàE EÑàHàñõ .                                  *
C              *------------------------------------------------------*
C              * KOMAHÑA  "NEGB" ( 1054DD ):                           *
C              * àáMEHEHàE áHAKA .                                    *
C              *------------------------------------------------------*
C              * KOMAHÑA  "ADCB" ( 1055DD ):                           *
C              * èPàÅABãEHàE èEPEHOCA .                               *
C              *------------------------------------------------------*
C              * KOMAHÑA  "SBCB" ( 1056DD ):                           *
C              * BõóàTAHàE èEPEHOCA .                                 *
C              *------------------------------------------------------*
C              * KOMAHÑA  "TSTB"  ( 1057DD):                           *
C              * èPOBEPKA .                                           *
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
C                  IBAI2  - MãAÑòàâ ÅAâT   CãOBA
C                  IBAI1  - CTAPòàâ ÅAâT   CãOBA
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
C   èOÑèPOÉPAMMõ:  1. ADECIS
C   ------------
C
C   èPàMEóAHàE:    1. HET.
C   -----------
C
CZZZ-------------------------------------------------------------------
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG, KB(16), KB1(16)
       IER = 0
C--> óTO áA KOMAHÑõ ?
       IF (KD.LT.35328.AND.KD.GE.34816) GO TO 4000
       KD1 = KD - 35328
       KOM1= KD1/ 64  +1
C----------------------------------------------------------------------
C--> àôEM OèEPAHÑ  ( AÑPEC--> IADRS,  óàCãO--> IDATA )
       KD0 =  KD1 - 64 * ( KOM1 - 1 )
       CALL ADECIS( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .EQ. 1 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 10
C--> áAèPOC HA BBOÑ ÑAHHõX àá èOPTA BõBOÑA
       IF( (-IADRS) .NE. ADPRTO ) GO TO 9904
       IDATA = IPORT
10     CONTINUE
C -->  PAáOÅúEM CãOBO HA ÅAâTõ
       IBAI1 = IDATA / 256
       IBAI2 = IDATA - IBAI1*256
C--> KAKOâ ÅAâT OÅPABATõBAEM ?
       IDATA = IBAI2
       IF ( IER.EQ.2 ) IDATA = IBAI1
C-->  èAáOâÑEMCü èO KOMAHÑAM
       GO TO ( 5000, 5100, 5200, 5300, 5400, 5500, 5600, 5700 ), KOM1
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAHÑA "EMT"
C              KOMAHÑA "TRAP"
C----------------------------------------------------------------------
4000   CONTINUE
C--> KOMAHÑõ èOKA HE PEAãàáàPOBAHõ
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "CLRB"
C----------------------------------------------------------------------
5000   CONTINUE
       IDATA = 0
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 0
       FLAG(2) = 0
       FLAG(3) = 1
       FLAG(4) = 0
C--> BPEMü ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "COMB"
C----------------------------------------------------------------------
5100   CONTINUE
       IDATA = 255 - IDATA
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 1
       FLAG(2) = 0
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C--> BPEMü ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "INCB"
C----------------------------------------------------------------------
5200   CONTINUE
       IDATA = IDATA + 1
       IF(IDATA .GE. 256 ) IDATA = IDATA - 256
C--> áAâMEMCü îãAÉAMà.
       FLAG(2) = 0
       IF( IDATA .EQ. 255 ) FLAG(2) = 1
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C--> BPEMü ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "DECB"
C----------------------------------------------------------------------
5300   CONTINUE
       IDATA = IDATA - 1
       IF( IDATA .LT. 0 ) IDATA = 256 + IDATA
C--> áAâMEMCü îãAÉAMà.
       FLAG(2) = 0
       IF( IDATA .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C--> BPEMü ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "NEGB"
C----------------------------------------------------------------------
5400   CONTINUE
       IDATA = 256 - IDATA
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 1
       IF(IDATA .EQ. 0 ) FLAG(1) = 0
       FLAG(2) = 0
       IF(IDATA .EQ. 256 ) FLAG(2) = 1
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C--> BPEMü ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "ADCB"
C----------------------------------------------------------------------
5500   CONTINUE
       IDATA = IDATA + FLAG(1)
       IF(IDATA .GT. 255 ) IDATA = IDATA  - 256
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 0
C      IF( INEW .EQ. 0  .AND.  IDATA .EQ. 65535  ) FLAG(1) = 1
       FLAG(2) = 0
C      IF( INEW .EQ. 32768  .AND.  IDATA .EQ. 32767 ) FLAG(2) = 1
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "SBCB"
C----------------------------------------------------------------------
5600   CONTINUE
       IDATA = IDATA - FLAG(1)
       IF( INEW .LT. 0 ) INEW = 65536 + INEW
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 0
C      IF( IDATA .EQ. 0  .AND.  INEW .EQ. 65535  ) FLAG(1) = 1
       FLAG(2) = 0
C      IF( IDATA .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF(IDATA .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "TST"
C----------------------------------------------------------------------
5700   CONTINUE
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 0
       FLAG(2) = 0
       FLAG(3) = 0
       IF( IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( IDATA .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 4.0E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.4E-6
        RETURN
C######################################################################
9000   CONTINUE
C-->  áAèàCõBAEM PEáìãúTAT
       INEW = IBAI1*256 + IDATA
       IF(IER.EQ.2) INEW = IDATA*256 + IBAI2
       IER = 0
       IF(IADRS .LT. 0 ) GO TO 92
       MEM(IADRS + 1) = INEW
       MEM( IADRS + 2) = INEW/256
       RETURN
92     CONTINUE
       IF(IADRS . LT. -8 ) GO TO 93
       REG(-IADRS) = INEW
       RETURN
93     CONTINUE
       IPORT = INEW
       RETURN
C----------------------------------------------------------------------
9901   CONTINUE
       IER = 1
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
       END
