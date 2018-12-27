       SUBROUTINE ADEC06( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T, IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (AT SEPTEMBER 1989 É.)-----------
C   èOÑèPOÉPAMMA: ADEC06( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1 , T , IER )
C
C   HAáHAóEHàE:  OÅPAÅOTóàK  KOMAHÑ
C   ----------   MàKPOèPOñECCOPA  K1801BM1(2,3)
C              *------------------------------------------------------*
C              * KOMAHÑA  "MUL"  ( 070RSS ):                          *
C              * ìMHOÜEHàE .                                          *
C              *------------------------------------------------------*
C              * KOMAHÑA  "DIV"   (071RSS ):                          *
C              * ÑEãEHàE .                                            *
C              *------------------------------------------------------*
C              * KOMAHÑA  "ASH"   (072RSS ):                          *
C              * APàîMETàóECKàâ CÑBàÉ .                               *
C              *------------------------------------------------------*
C              * KOMAHÑA  "ASCH"  (073RSS ):                          *
C              * APàîMETàóECKàâ CÑBàÉ ÑBOâHOÉO CãOBA .                *
C              *------------------------------------------------------*
C              * KOMAHÑA  "XOR"   (074RSS ):                          *
C              * àCKãûóAûôEE àãà .                                    *
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
       INTEGER  REG(8), FLAG(6), ADPRTO,ADPRTI, ADPROG, KB(17), KB1(17)
       IER = 0
C--> óTO áA KOMAHÑõ ?
       KD1 = KD - 28672
       KOM = KD1 / 512 + 1
       NR  = ( KD1 - 512 * (KOM-1) ) / 64 + 1
C--> àôEM OèEPAHÑ  ( AÑPEC--> IADRS,  óàCãO--> IDATA )
       KD0 =   KD1 - 512 * (KOM-1) - (NR - 1) * 64
       CALL ADECIS( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .NE. 0 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 2
C--> áAèPOC HA BBOÑ ÑAHHõX àá èOPTA B/B
       IF( (-IADRS) .NE. ADPRTO.AND.(-IADRS).NE.ADPRTI ) GO TO 9904
       IDATA = IPORT
       IF( (-IADRS) .EQ. ADPRTI ) IDATA = IPORT1
2      CONTINUE
C--> PAáOâÑEMCü èO KOMAHÑAM .
       GO TO (70000,71000,72000,73000,74000 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAHÑA "MUL"
C----------------------------------------------------------------------
70000  CONTINUE
       INEW = REG(NR)*IDATA
C --> àôEM CTAPòàE PAáPüÑõ
       INEW1 = INEW/65536
C --> àôEM MãAÑòàE PAáPüÑõ
       INEW2 = INEW - INEW1*65536
C--> KAKOâ PEÉàCTOP ?
       MD = MOD(NR,2)
       IF (MD.EQ.0) GO TO 70010
       REG(NR) = INEW1
       REG(NR+1) = INEW2
       GO TO 70020
70010  CONTINUE
       REG(NR) = INEW2
70020 CONTINUE
C-->   îãAÉà ...
       IF(INEW.LT.(-32768).OR.INEW.GE.32768) FLAG(1) = 1
       FLAG(2) = 0
       FLAG(3) = 0
       IF ( INEW.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW.GE.32768) FLAG(4) = 1
C--> BPEMü  èOKA èPàMETàBHO ( = 55 MKC. )
       T = 55.E-6 + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "DIV"
C----------------------------------------------------------------------
71000  CONTINUE
C--> KAKOâ PEÉàCTOP ?
       MD = MOD(NR,2)
       IF (MD.EQ.0) GO TO 9901
       INEW = REG(NR)*65536 + REG(NR+1)
       IF (IDATA.EQ.0) GOTO 9901
C--> óACTHOE
       INEW1 = INEW /IDATA
C--> OCTATOK
       INEW2 = INEW -INEW1*IDATA
       INEW3 = INEW1 /65536
       INEW4 = INEW1 - INEW3*65536
C--> PHUS INTO REGICTOR
       REG(NR) = INEW4
       REG(NR+1) = INEW2
C-->   îãAÉà ...
       FLAG(1) = 0
       FLAG(3) = 0
       IF ( INEW4.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW4.GE.32768) FLAG(4) = 1
C--> BPEMü  èOKA èPàMETàBHO ( = 80 MKC. )
       T = 80.E-6 + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "ASH"
C----------------------------------------------------------------------
72000  CONTINUE
       IF ( IDATA.GE.64 ) GOTO 9901
       NR0 = REG(NR)
C--> B KAKìû CTOPOHì ?
       IF ( IDATA.GE.32 ) GOTO 72010
C-->  CÑBàÉ BãEBO
C-->  BPEMü
       TB = (2.8E-6)*IDATA
C-->  ìXOÑàM ECãà OÑHà Hìãà ...
       IF ( IDATA.GE.16 ) GOTO 72020
       INEW = REG(NR)*(2**IDATA)
       INEW1 = INEW /65536
       INEW  =  INEW -  INEW1*65536
C--> îãAÉ èEPEHOCA ?
       IC = MOD( INEW1,2)
       GO TO 72030
72010  CONTINUE
C--> CÑBàÉ BèPABO
       IDATA = 64 - IDATA
C-->  BPEMü
       TB = (2.E-6)*IDATA
C--> COXPAHüETCü CTAPòàâ PAáPüÑ
       I15 = 0
       IF ( NR0.GT.32767 ) I15 = 32768
       IF ( NR0.GT.32767 ) REG(NR) = REG(NR) - 32768
       INEW1 = REG(NR)/(2**(IDATA-1))
       INEW  = INEW1/2 + I15
C--> îãAÉ èEPEHOCA ?
       IC = MOD( INEW1,2)
       GO TO 72030
72020  INEW = 0
       IC = 0
72030  CONTINUE
C-->   áAèOMHàM
       REG(NR) = INEW
C-->   îãAÉà ...
       FLAG(1) = IC
       FLAG(2) = 0
       IF(NR0.GE.32768.AND.INEW.LT.32768) FLAG(2) =1
       IF(NR0.LT.32768.AND.INEW.GE.32768) FLAG(2) =1
       FLAG(3) = 0
       IF ( INEW.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW.GE.32768) FLAG(4) = 1
C--> BPEMü
       T = TB  + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "ASCH"
C----------------------------------------------------------------------
73000  CONTINUE
       NR1 = REG(NR)
C--> KAKOâ PEÉàCTOP àCèOãúáìETCü ?
       MD = MOD(NR,2)
       IF (MD.EQ.0) GO TO 73010
       NR0 = REG(NR)*65536 + REG(NR+1)
C--> B KAKìû CTOPOHì ?
       IF ( IDATA.GE.32 ) GOTO 73020
C-->  CÑBàÉ BãEBO
C-->  BPEMü
       TB = (3.6E-6)*IDATA
       I32 = 0
       INEW = NR0*(2**IDATA)
       INEW0 = 0
CCCC!  INEW  =  INEW -  INEW0*(2**32)
       GO TO 73030
73020  CONTINUE
C--> CÑBàÉ BèPABO
       IDATA = 64 - IDATA
C-->  BPEMü
       TB = (3.2E-6)*IDATA
C--> COXPAHüETCü CTAPòàâ PAáPüÑ
       I32 = 0
       IF ( REG(NR).GT.32767 ) I32 = 32768
       IF ( REG(NR).GT.32767 ) NR0 = NR0-65536*32768
       INEW1 = NR0/(2**(IDATA-1))
       INEW  = INEW1/2
73030  CONTINUE
       INEW1 = INEW /65536
       INEW2 =  INEW -  INEW1*65536
C--> îãAÉ èEPEHOCA ?
       IC = MOD( INEW0,2)
       REG(NR) = INEW1 + I32
       INEW = REG(NR)
       REG(NR+1) = INEW2
       GO TO 73050
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
73010 CONTINUE
C--> áHAóàT ñàKãàóECKàâ CÑBàÉ
C--> èPEOÅPAáìEM (REG) B ÑBOâóHõâ KOÑ
       CALL ADECDB(NR1,KB,IER )
       IF ( IER.NE.0) RETURN
       KB(17) = FLAG(1)
C--> B KAKìû CTOPOHì ?
       IF ( IDATA.GE.32 ) GOTO 73110
C-->  CÑBàÉ BãEBO
C-->  BPEMü
       TB = (3.6E-6)*IDATA
       DO 73120 I = 1,17
       I1 = I + IDATA
       I2 = MOD(I1,17)
       IF(I2.EQ.0) I2 = 17
       KB1(I2) = KB(I)
73120  CONTINUE
       GO TO 73130
73110  CONTINUE
C--> CÑBàÉ BèPABO
       IDATA = 64 - IDATA
C-->  BPEMü
       TB = (3.2E-6)*IDATA
       DO 73129 I = 1,17
       I1 =  18 -  I + IDATA
       I2 = MOD(I1,17)
       IF(I2.EQ.0) I2 = 17
       KB1(18 - I2) = KB(I)
73129  CONTINUE
73130  CONTINUE
C--> èPEOÅPAáìEM B ÑECüTàóHOM BàÑE
       CALL ADECBD(INEW,KB1,IER )
CCC    WRITE(6,*) KB1(17),INEW
       IF ( IER.NE.0) RETURN
       IC = KB1(17)
       REG(NR) = INEW
73050  CONTINUE
C-->   îãAÉà ...
       FLAG(1) = IC
       FLAG(2) = 0
       IF(NR1.GE.32768.AND.INEW.LT.32768) FLAG(2) =1
       IF(NR1.LT.32768.AND.INEW.GE.32768) FLAG(2) =1
       FLAG(3) = 0
       IF ( INEW.EQ.0) FLAG(3) = 1
       FLAG(4) = 0
       IF ( INEW.GE.32768) FLAG(4) = 1
C--> BPEMü
       T = TB  + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "XOR"
C----------------------------------------------------------------------
74000  CONTINUE
C--> PAáÅàBAEM COÑEPÜàMOE PEÉàCTPA.
       CALL ADECDB( REG(NR), KB, IER )
       IF( IER .NE. 0 ) RETURN
C--> PAáÅàBAEM COÑEPÜàMOE àCTOóHàKA.
       CALL ADECDB( IDATA, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> OèEPAñàü "àCKã. àãà"
       DO 74010 I=1,16
       KB(I) = KB(I) + KB1(I)
       IF( KB(I) .GT. 1 ) KB(I) = 0
74010  CONTINUE
       CALL ADECBD( INEW, KB, IER )
       IF( IER .NE. 0 ) RETURN
C--> áAâMEMCü îãAÉAMà.
       FLAG(2) = 0
       FLAG(3) = 0
       IF( INEW .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
       IF( INEW .GT. 32767 ) FLAG(4) = 1
C--> BPEMü ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0 ) T = 4.0E-6
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