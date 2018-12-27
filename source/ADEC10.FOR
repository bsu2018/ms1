       SUBROUTINE ADEC10( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
     *                    IPORT1,T,IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (AT OCTOBER   1989 É.)-----------
C   èOÑèPOÉPAMMA: ADEC10( KD, REG, FLAG, ADPRTO,ADPRTI, ADPROG, IPORT,
C   -------------         IPORT1,T,IER )
C
C   HAáHAóEHàE:  OÅPAÅOTóàK  OÑHOAÑPECHõX KOMAHÑ
C   ----------   MàKPOèPOñECCOPA  K1801BM1(2,3)    (PAÅOTA C ÅAâTAMà )
C              *------------------------------------------------------*
C              * KOMAHÑA  "RORB"  ( 1060DD ):                          *
C              * ñàKãàóECKàâ CÑBàÉ B èPABO.                           *
C              *------------------------------------------------------*
C              * KOMAHÑA  "ROLB"   (1061DD ):                          *
C              * ñàKãàóECKàâ CÑBàÉ B ãEBO .                           *
C              *------------------------------------------------------*
C              * KOMAHÑA  "ASRB"   (1062DD ):                          *
C              * APàîMETàóECKàâ CÑBàÉ B èPABO.                        *
C              *------------------------------------------------------*
C              * KOMAHÑA  "ASLB"   (1063DD ):                          *
C              * APàîMETàóECKàâ CÑBàÉ B ãEBO.                         *
C              *------------------------------------------------------*
C              * KOMAHÑA  "MTPS" (1064DD ):                           *
C              * áAèàCú CCè         .                                 *
C              *------------------------------------------------------*
C              * KOMAHÑA  "MFPI"( 1065DD ):                           *
C              *                                                      *
C              *------------------------------------------------------*
C              * KOMAHÑA  "MTPI"( 1066DD ):                           *
C              *                                                      *
C              *------------------------------------------------------*
C              * KOMAHÑA  "MFPS" ( 1067DD ):                           *
C              * óTEHàE CCè       .                                   *
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
C                  IBAI1  - CTAPòàâ PAáPüÑ
C                  IBAI2  - MãAÑòàâ PAáPüÑ
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
       KD1 = KD - 35840
       KOM = KD1 / 64 + 1
C--> àôEM OèEPAHÑ  ( AÑPEC--> IADRS,  óàCãO--> IDATA )
       KD0 =  KD1 - 64 * ( KOM - 1 )
       CALL ADECIS( KD0, ADPROG, REG, IADRS, IDATA, TI, IER )
       IF( IER   .EQ. 1 ) RETURN
       IF( IADRS .GE.-8 ) GO TO 2
C--> áAèPOC HA BBOÑ ÑAHHõX àá èOPTA BõBOÑA
       IF( (-IADRS) .NE. ADPRTO ) GO TO 9904
       IDATA = IPORT
2      CONTINUE
C -->  PAáOÅúEM CãOBO HA ÅAâTõ
       IBAI1 = IDATA / 256
       IBAI2 = IDATA - IBAI1*256
C--> KAKOâ ÅAâT OÅPABATõBAEM ?
       IDATA = IBAI2
       IF ( IER.EQ.2 ) IDATA = IBAI1
C--> PAáOâÑEMCü èO KOMAHÑAM .
       GO TO ( 6000, 6100, 6200, 6300, 6400, 6500, 6600, 6700 ), KOM
       GO TO 9903
C----------------------------------------------------------------------
C              KOMAHÑA "ROR"
C----------------------------------------------------------------------
6000   CONTINUE
C--> CÑBàÉAEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6010 I=2,8
       KB1(I-1) = KB(I)
6010   CONTINUE
       KB1(8) = FLAG(1)
       CALL ADECBD(IDATA, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = KB(1)
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEMü ?
       T = 5.6E-6 + TI
       IF( TI .EQ. 0 ) T = 4.8E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "ROL"
C----------------------------------------------------------------------
6100   CONTINUE
C--> CÑBàÉAEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6110 I=1,7
       KB1(I+1) = KB(I)
6110   CONTINUE
       KB1(1) = FLAG(1)
       CALL ADECBD(IDATA, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = KB(8 )
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEMü ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "ASRB"
C----------------------------------------------------------------------
6200   CONTINUE
C--> CÑBàÉAEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6210 I=2,7
       KB1(I-1) = KB(I)
6210   CONTINUE
       KB1(8) = KB(8)
       CALL ADECBD(IDATA, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = KB(1)
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEMü ?
       T = 6.8E-6 + TI
       IF( TI .EQ. 0 ) T = 5.2E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "ASLB"
C----------------------------------------------------------------------
6300   CONTINUE
C--> CÑBàÉAEM ...
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6310 I=1,7
       KB1(I+1) = KB(I)
6310   CONTINUE
       KB1(1) = 0
       CALL ADECBD(IDATA, KB1, IER )
       IF( IER .NE. 0 ) RETURN
C--> áAâMEMCü îãAÉAMà.
       FLAG(1) = KB(8)
       FLAG(3) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       FLAG(4) = 0
C      IF( INEW .GT. 32767 ) FLAG(4) = 1
       FLAG(2) = 0
       IF( FLAG(1) .NE. FLAG(4) ) FLAG(2) = 1
C--> BPEMü ?
       T = 4.8E-6 + TI
       IF( TI .EQ. 0 ) T = 4.4E-6
       GO TO 9000
C----------------------------------------------------------------------
C              KOMAHÑA "MTPS"
C----------------------------------------------------------------------
6400   CONTINUE
       CALL ADECDB( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
       DO 6410 I =1,5
       FLAG(I) = KB(I)
6410   CONTINUE
       FLAG(6) = KB(8)
C--> BPEMü ?
       T = 8.0E-6 + TI
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "MFPI"
C              KOMAHÑA "MTPI"
C----------------------------------------------------------------------
6500   CONTINUE
6600   CONTINUE
C--> KOMAHÑõ èOKA HE PEAãàáOBAHõ.
       IER = 5
       RETURN
C----------------------------------------------------------------------
C              KOMAHÑA "MFPS"
C----------------------------------------------------------------------
6700   CONTINUE
       DO 6710 I =1,5
       KB(I) = FLAG(I)
6710   CONTINUE
       KB(6) = 0
       KB(7) = 0
       KB(8) = FLAG(6)
       CALL ADECBD( IDATA, KB, IER )
       IF( IER .NE. 0 ) RETURN
C--> áAâMEMCü îãAÉAMà.
       FLAG(2) = 0
       FLAG(3) = 0
       FLAG(4) = 0
       IF(IDATA .EQ. 0 ) FLAG(3) = 1
       IF( KB(8) .EQ. 1 ) FLAG(4) = 1
C--> BPEMü ?
       T = 7.6E-6 + TI
       IF( TI .EQ. 0 ) T = 5.6E-6
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