       SUBROUTINE ADEC04( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT CEHTüÅPü  1989 É.)-----------
C   èOÑèPOÉPAMMA: ADEC04( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )
C
C   HAáHAóEHàE:  OÅPAÅOTóàK  OÑHOAÑPECHõX KOMAHÑ
C   ----------   MàKPOèPOñECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAHÑA  "JSR"  ( 004RDD ):                          *
C              * OÅPAôEHàE K èOÑèPOÉPAMME .                           *
C              *------------------------------------------------------*
C              * KOMAHÑA  "CLR"   (0050DD ):                          *
C              * OóàCTKA .                                            *
C              *------------------------------------------------------*
C              * KOMAHÑA  "COM"   (0051DD ):                          *
C              * àHBEPTàPOBAHàE .                                     *
C              *------------------------------------------------------*
C              * KOMAHÑA  "INC"   (0052DD ):                          *
C              * èPàÅABãEHàE EÑàHàñõ .                                *
C              *------------------------------------------------------*
C              * KOMAHÑA  "DEC"  (0053DD ):                           *
C              * BõóàTAHàE EÑàHàñõ .                                  *
C              *------------------------------------------------------*
C              * KOMAHÑA  "NEG" ( 0054DD ):                           *
C              * àáMEHEHàE áHAKA .                                    *
C              *------------------------------------------------------*
C              * KOMAHÑA  "ADC" ( 0055DD ):                           *
C              * èPàÅABãEHàE èEPEHOCA .                               *
C              *------------------------------------------------------*
C              * KOMAHÑA  "SBC" ( 0056DD ):                           *
C              * BõóàTAHàE èEPEHOCA .                                 *
C              *------------------------------------------------------*
C              * KOMAHÑA  "TST"  ( 0057DD):                           *
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
       KOM = KD / 512 - 3
C--> PAáOâÑEMCü èO KOMAHÑAM .
       GO TO ( 4000, 5 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAHÑA "JSR"
C----------------------------------------------------------------------
4000   CONTINUE
C--> HOMEP PEÉàCTPA ?
       NR = ( KD - 2048 ) / 64 + 1
C---> èPOBEPüEM  èPABàãúHõâ ãà áAÑAH NR ?
        IF ( NR.LT.1 .OR .NR .GT .6 ) GOTO 9901
C--> àôEM OèEPAHÑ  ( AÑPEC--> IADRS,  óàCãO--> IDATA )
       KD0 =  KD - 2048  - 64 * ( NR - 1 )
       CALL ADECSI( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 4010
C--> áAèPOC HA BBOÑ ÑAHHõX àá èOPTA BBOÑA
       IF( (-IADRS) .NE. ADPRTI ) GO TO 9904
       IDATA = IPORT1
4010   CONTINUE
C
       MEM( REG(7) - 1 ) = REG(NR)
       MEM( REG(7) - 2 ) = REG(NR) / 256
       REG(7) = REG(7) - 2
C
       REG(NR) = REG(8)
C
       REG(8) = IDATA
C--> BPEMü ?
       T = 4.0E-6 + TI
       RETURN
C----------------------------------------------------------------------
5      CONTINUE
       KD1 = KD - 2560
       KOM1= KD1 / 64 + 1
C--> àôEM OèEPAHÑ  ( AÑPEC--> IADRS,  óàCãO--> IDATA )
       KD0 =  KD1 - 64 * ( KOM1 - 1 )
       CALL ADECIS( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 10
C--> áAèPOC HA BBOÑ ÑAHHõX àá èOPTA BõBOÑA
       IF( (-IADRS) .NE. ADPRTO ) GO TO 9904
       IDATA = IPORT
10     CONTINUE
       GO TO ( 5000, 5100, 5200, 5300, 5400, 5500, 5600, 5700 ), KOM1
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAHÑA "CLR"
C----------------------------------------------------------------------
5000   CONTINUE
       INEW = 0
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
C              KOMAHÑA "COM"
C----------------------------------------------------------------------
5100   CONTINUE
       INEW = 65535 - IDATA
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 1
       FLAG(2) = 0
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "INC"
C----------------------------------------------------------------------
5200   CONTINUE
       INEW = IDATA + 1
       IF( INEW .GT. 65535 ) INEW = INEW - 65536
C--> áAâMEMCü îãAÉAMà.
       FLAG(2) = 0
       IF( IDATA .EQ. 32767 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "DEC"
C----------------------------------------------------------------------
5300   CONTINUE
       INEW = IDATA - 1
       IF( INEW .LT. 0 ) INEW = 65536 + INEW
C--> áAâMEMCü îãAÉAMà.
       FLAG(2) = 0
       IF( IDATA .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "NEG"
C----------------------------------------------------------------------
5400   CONTINUE
       INEW = 65536 - IDATA
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 1
       IF( INEW .EQ. 0 ) FLAG(1) = 0
       FLAG(2) = 0
       IF( INEW .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.2E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "ADC"
C----------------------------------------------------------------------
5500   CONTINUE
       INEW = IDATA + FLAG(1)
       IF( INEW .GT. 65535 ) INEW = INEW - 65536
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 0
       IF( INEW .EQ. 0  .AND.  IDATA .EQ. 65535  ) FLAG(1) = 1
       FLAG(2) = 0
       IF( INEW .EQ. 32768  .AND.  IDATA .EQ. 32767 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "SBC"
C----------------------------------------------------------------------
5600   CONTINUE
       INEW = IDATA - FLAG(1)
       IF( INEW .LT. 0 ) INEW = 65536 + INEW
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = 0
       IF( IDATA .EQ. 0  .AND.  INEW .EQ. 65535  ) FLAG(1) = 1
       FLAG(2) = 0
       IF( IDATA .EQ. 32768 ) FLAG(2) = 1
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
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
       IF( IDATA .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 4.4E-6 + TI
       IF( TI .EQ. 0.0 ) T = 4.8E-6
        RETURN
C######################################################################
9000   CONTINUE
C-->  áAèàCõBAEM PEáìãúTAT
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