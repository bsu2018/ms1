       SUBROUTINE ADEC00( TN, TN1, P, NOUT, KX, REG, FLAG, IPORT,
     *                    IPORT1,IADPTO,IADPTI, IADPGM, IER )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT  ABÉìCTA  1988 É.)-----------
C   èOÑèPOÉPAMMA: ADEC00( TN, TN1, P, NOUT, KX, REG, FLAG, IPORT,
C                         IPORT1,IADPTO,IADPTI, IADPGM, IER )
C   -------------
C   HAáHAóEHàE:  OÅPAÅOTóàK èPOÉPAMM, HAèàCAHHõX B MAòàHHõX KOÑAX
C   ----------   Ñãü MàKPOèPOñECCOPA  CEPàà  K1801BM1(2,3) ,
C                àMEûôEÉO CàCTEMì KOMAHÑ îàPMõ 'DEC' .
C
C   METOÑ:      ãOÉàóECKOE MOÑEãàPOBAHàE.
C   -----
C
C   èAPAMETPõ:  BXOÑHõE :
C  ----------      TN     - HAóAãúHOE BPEMü BõèOãHEHàü èPOÉPAMMõ ;
C                  TN1    - KOHEóHOE  BPEMü BõèOãHEHàü èPOÉPAMMõ ;
C                  P      - îãAÉ OTãAÑOóHOâ èEóATà :
C                           P=0 - OTãAÑOóHAü èEóATú OTKãûóEHA ,
C                           P=1 - OTãAÑOóHAü èEóATú BKãûóEHA ( èEóATAE-
C                                 Cü COÑEPÜàMOE BCEX PEÉàCTPOB à CCè
C                                 Mè èOCãE BõèOãHEHàü KAÜÑOâ àá KOMAHÑ )
C                  NOUT   - HOMEP KAHAãA BõBOÑA OTãAÑOóHOâ èEóATà ;
C                          (=10) !!!!
C               BõXOÑHõE :
C
C                  KX     - ìKAáATEãú àáMEHEHàü COÑEPÜàMOÉO èOPTA
C                           B/B HA àHTEPBAãE [ TN , TN1 ] ;
C                  REG    - PEÉàCTPõ Mè :
C                           REG(1) --> R0
C                           REG(2) --> R1
C                           REG(3) --> R2
C                           REG(4) --> R3
C                           REG(5) --> R4
C                           REG(6) --> R5
C                           REG(7) --> R6 ( ìKAáATEãú CTEKA )
C                           REG(8) --> R7 ( CóETóàK KOMAHÑ  )
C                  FLAG   - îãAÉà CCè MàKPOèPOñECCOPA ;
C                           FLAG(1)-> îãAÉ  C (èEPEHOC)
C                           FLAG(2)-> îãAÉ  V (APàîM. èEPEèOãHEHàE)
C                           FLAG(3)-> îãAÉ  Z (HìãEBOâ PEáìãúTAT)
C                           FLAG(4)-> îãAÉ  N (OTPàñAT.PEáìãúTAT)
C                           FLAG(5)-> îãAÉ  T
C                           FLAG(6)-> îãAÉ I/O(èPàOPàTET)
C                  IPORT  - COÑEPÜàMOE èOPTA BõBOÑA ;
C                  IPORT1 - COÑEPÜàMOE èOPTA BBOÑA ;
C                  IADPTO - AÑPEC èOPTA BõBOÑA ;
C                  IADPTI - AÑPEC èOPTA BBOÑA ;
C                  IADPGM - MAKCàMAãúHõâ AÑPEC Oáì ,B èPEÑEãAX
C                           KOTOPOÉO PAÅOTAET ìèPABãüûôAü èPOÉPAMMA
C                  IER    - ìKAáATEãú OòàÅKà:
C                           IER=0  -  OòàÅOK HE OÅHAPìÜEHO ;
C                           IER=1  -  OòàÅOóHAü áAèàCú KOMAHÑõ ãàÅO
C                                     BõXOÑ áA èPEÑEãõ Oáì ;
C                           IER=2  -  èPOàáOòãO OÅPAôEHàE K üóEâKE
C                                     èAMüTà, àMEûôEâ HEóETHõâ AÑPEC.
C                           IER=3  -  HE HAòA KOMAHÑA ;
C                           IER=4  -  áAèPOC HA BBOÑ ÑAHHõX àá èOPTA
C                                     B/B ( èOKA HE PEAãàáOBAH ) ;
C                           IER=5  -  KOMAHÑA èOKA HE PEAãàáOBAHA ;
C                           IER=6  -  KOMAHÑõ HE CìôECTBìET ;
C
C
C   èOÑèPOÉPAMMõ:
C   ------------
C---------------------------------------------------------------------+
C      CàCTEMA KOMAHÑ MàKPOèPOñECCOPOB CEPàà  K1801BM1(2,3)           I
C                                                                     I
C--------------+--------------+--------------+------------+-----------I
CI ÑECüTàóHõâ  I BOCúMàPàóHõâ I ACCEMÅãEPHõâ I   BPEMü    I   HOMEP   I
CI    KOÑ      I     KOÑ      I     KOÑ      I BõèOãHEHàü I    è/è    I
C+-------------+--------------+--------------+------------+-----------+
CI    00000    I    000000    I    HALT      I            I           I
CI    00001    I    000001    I    WAIT      I            I           I
CI    00002    I    000002    I    RTI       I            I           I
CI    00003    I    000003    I    BPT       I            I           I
CI    00004    I    000004    I    IOT       I            I           I
CI    00005    I    000005    I    RESET     I            I           I
CI    00006    I    000006    I    RTT       I            I           I
CI    00064    I    000100    I              I            I           I
CI    00127    I    000177    I    JMP       I            I           I
CI    00128    I    000200    I              I            I           I
CI    00135    I    000207    I    RTS       I            I           I
CI    00160    I    000240    I    NOP       I            I           I
CI    00161    I    000241    I    CLC       I            I           I
CI    00162    I    000242    I    CLV       I            I  ADEC02   I
CI    00164    I    000244    I    CLZ       I            I           I
CI    00168    I    000250    I    CLN       I            I           I
CI    00175    I    000257    I    CCC       I            I           I
CI    00177    I    000261    I    SEC       I            I           I
CI    00178    I    000262    I    SEV       I            I           I
CI    00180    I    000264    I    SEZ       I            I           I
CI    00184    I    000270    I    SEN       I            I           I
CI    00191    I    000277    I    SCC       I            I           I
CI    00192    I    000300    I              I            I           I
CI    00255    I    000377    I    SWAB      I            I           I
CI    00256    I    000400    I              I            I           I
CI    00511    I    000777    I    BR        I            I           I
C+-------------+--------------+--------------+------------+-----------+
CI    00512    I    001000    I              I            I           I
CI    00767    I    001377    I    BNE       I            I           I
CI    00768    I    001400    I              I            I           I
CI    01023    I    001777    I    BEQ       I            I           I
CI    01024    I    002000    I              I            I  ADEC03   I
CI    01279    I    002377    I    BGE       I            I           I
CI    01280    I    002400    I              I            I           I
CI    01535    I    002777    I    BLT       I            I           I
CI    01536    I    003000    I              I            I           I
CI    01791    I    003377    I    BGT       I            I           I
CI    01792    I    003400    I              I            I           I
CI    02047    I    003777    I    BLE       I            I           I
C+-------------+--------------+--------------+------------+-----------+
CI    02048    I    004000    I              I            I           I
CI    02559    I    004777    I    JSR       I            I           I
CI    02560    I    005000    I              I            I           I
CI    02623    I    005077    I    CLR       I            I           I
CI    02624    I    005100    I              I            I           I
CI    02687    I    005177    I    COM       I            I           I
CI    02688    I    005200    I              I            I           I
CI    02751    I    005277    I    INC       I            I  ADEC04   I
CI    02752    I    005300    I              I            I           I
CI    02815    I    005377    I    DEC       I            I           I
CI    02816    I    005400    I              I            I           I
CI    02879    I    005477    I    NEG       I            I           I
CI    02880    I    005500    I              I            I           I
CI    02943    I    005577    I    ADC       I            I           I
CI    02944    I    005600    I              I            I           I
CI    03007    I    005677    I    SBC       I            I           I
CI    03008    I    005700    I              I            I           I
CI    03071    I    005777    I    TST       I            I           I
C+-------------+--------------+--------------+------------+-----------+
CI    03072    I    006000    I              I            I           I
CI    03135    I    006077    I    ROR       I            I           I
CI    03136    I    006100    I              I            I           I
CI    03199    I    006177    I    ROL       I            I           I
CI    03200    I    006200    I              I            I           I
CI    03263    I    006277    I    ASR       I            I           I
CI    03264    I    006300    I              I            I           I
CI    03327    I    006377    I    ASL       I            I           I
CI    03328    I    006400    I              I            I  ADEC05   I
CI    03391    I    006477    I    MARK      I            I           I
CI    03392    I    006500    I              I            I           I
CI    03455    I    006577    I    MFPI(3)   I            I           I
CI    03456    I    006600    I              I            I           I
CI    03519    I    006677    I    MTPI(3)   I            I           I
CI    03520    I    006700    I              I            I           I
CI    03583    I    006777    I    SXT       I            I           I
CI#############I##############I##############I############I###########I
CI    04096    I    010000    I              I            I           I
CI    08191    I    017777    I    MOV       I            I  ADEC01   I
CI#############I##############I##############I############I###########I
CI    08192    I    020000    I              I            I           I
CI    12287    I    027777    I    CMP       I            I  ADEC01   I
CI#############I##############I##############I############I###########I
CI    12288    I    030000    I              I            I           I
CI    16383    I    037777    I    BIT       I            I  ADEC01   I
CI#############I##############I##############I############I###########I
CI    16384    I    040000    I              I            I           I
CI    20479    I    047777    I    BIC       I            I  ADEC01   I
CI#############I##############I##############I############I###########I
CI    20480    I    050000    I              I            I           I
CI    24575    I    057777    I    BIS       I            I  ADEC01   I
CI#############I##############I##############I############I###########I
CI    24576    I    060000    I              I            I           I
CI    28671    I    067777    I    ADD       I            I  ADEC01   I
CI#############I##############I##############I############I###########I
CI    28672    I    070000    I              I            I           I
CI    29183    I    070777    I    MUL(2)    I            I           I
CI    29184    I    071000    I              I            I           I
CI    29695    I    071777    I    DIV(2)    I            I           I
CI    29696    I    072000    I              I            I           I
CI    30207    I    072777    I    ASH(2)    I            I  ADEC06   I
CI    30208    I    073000    I              I            I           I
CI    30719    I    073777    I    ASCH(2)   I            I           I
CI    30720    I    074000    I              I            I           I
CI    31231    I    074777    I    XOR       I            I           I
C+-------------+--------------+--------------+------------+-----------+
CI    31232    I    075000    I              I            I           I
CI    31239    I    075007    I    FADD(2)   I            I           I
CI    31240    I    075010    I              I            I           I
CI    31247    I    075017    I    FSUB(2)   I            I           I
CI    31248    I    075020    I              I            I           I
CI    31255    I    075027    I    FMUL(2)   I            I  ADEC07   I
CI    31256    I    075030    I              I            I           I
CI    31263    I    075037    I    FDIV(2)   I            I           I
CI    32256    I    077000    I              I            I           I
CI    32767    I    077777    I    SOB       I            I           I
CI#############I##############I##############I############I###########I
CI    32768    I    100000    I              I            I           I
CI    33023    I    100377    I    BPL       I            I           I
CI    33024    I    100400    I              I            I           I
CI    33279    I    100777    I    BMI       I            I           I
CI    33280    I    101000    I              I            I           I
CI    33535    I    101377    I    BHI       I            I           I
CI    33536    I    101400    I              I            I           I
CI    33791    I    101777    I    BLOS      I            I           I
CI    33792    I    102000    I              I            I           I
CI    34047    I    102377    I    BVC       I            I           I
CI    34048    I    102400    I              I            I  ADEC08   I
CI    34303    I    102777    I    BVS       I            I           I
CI    34304    I    103000    I              I            I           I
CI    34559    I    103377    I    BCC       I            I           I
CI    34304    I    103000    I              I            I           I
CI    34559    I    103377    I    BHIS      I            I           I
CI    34560    I    103400    I              I            I           I
CI    34815    I    103777    I    BCS       I            I           I
CI    34560    I    103400    I              I            I           I
CI    34815    I    103777    I    BLO       I            I           I
C+-------------+--------------+--------------+------------+-----------+
CI    34816    I    104000    I              I            I           I
CI    35071    I    104377    I    EMT       I            I           I
CI    35072    I    104400    I              I            I           I
CI    35135    I    104477    I    TRAP      I            I           I
CI    35328    I    105000    I              I            I           I
CI    35391    I    105077    I    CLRB      I            I           I
CI    35392    I    105100    I              I            I           I
CI    35455    I    105177    I    COMB      I            I           I
CI    35456    I    105200    I              I            I           I
CI    35519    I    105277    I    INCB      I            I           I
CI    35520    I    105300    I              I            I  ADEC09   I
CI    35583    I    105377    I    DECB      I            I           I
CI    35584    I    105400    I              I            I           I
CI    35647    I    105477    I    NEGB      I            I           I
CI    35648    I    105500    I              I            I           I
CI    35711    I    105577    I    ADCB      I            I           I
CI    35712    I    105600    I              I            I           I
CI    35775    I    105677    I    SBCB      I            I           I
CI    35776    I    105700    I              I            I           I
CI    35839    I    105777    I    TSTB      I            I           I
C+-------------+--------------+--------------+------------+-----------+
CI    35840    I    106000    I              I            I           I
CI    35903    I    106077    I    RORB      I            I           I
CI    35904    I    106100    I              I            I           I
CI    35967    I    106177    I    ROLB      I            I           I
CI    35968    I    106200    I              I            I           I
CI    36031    I    106277    I    ASRB      I            I           I
CI    36032    I    106300    I              I            I           I
CI    36095    I    106377    I    ASLB      I            I           I
CI    36096    I    106400    I              I            I  ADEC10   I
CI    36159    I    106477    I    MTPS      I            I           I
CI    36160    I    106500    I              I            I           I
CI    36223    I    106577    I    MFPD(3)   I            I           I
CI    36224    I    106600    I              I            I           I
CI    36287    I    106677    I    MTPD(3)   I            I           I
CI    36288    I    106700    I              I            I           I
CI    36351    I    106777    I    MFPS      I            I           I
CI#############I##############I##############I############I###########I
CI    36864    I    110000    I              I            I           I
CI    40959    I    117777    I    MOVB      I            I  ADEC11   I
CI#############I##############I##############I############I###########I
CI    40960    I    120000    I              I            I           I
CI    45055    I    127777    I    CMPB      I            I  ADEC11   I
CI#############I##############I##############I############I###########I
CI    45056    I    130000    I              I            I           I
CI    49151    I    137777    I    BITB      I            I  ADEC11   I
CI#############I##############I##############I############I###########I
CI    49152    I    140000    I              I            I           I
CI    53247    I    147777    I    BICB      I            I  ADEC11   I
CI#############I##############I##############I############I###########I
CI    53248    I    150000    I              I            I           I
CI    57343    I    157777    I    BISB      I            I  ADEC11   I
CI#############I##############I##############I############I###########I
CI    57344    I    160000    I              I            I           I
CI    61439    I    167777    I    SUB       I            I  ADEC01   I
CI#############I##############I##############I############I###########I
C
C   èPàMEóAHàE:
C   -----------
C
C        OPÉAHàáAñàü èAMüTà B PEAãúHOâ MàKPOèPOñECCOPHOâ CàCTEME
C        à B MOÑEãàPìûôEâ EE èPOÉPAMME (MACCàBõ MEM à MEM1)
C        -------------------------------------------------------
C
C        OPÉAHàáAñàü èAMüTà B PEAãúHOâ MàKPOèPOñECCOPHOâ CàCTEME
C
C               CãOBO
C           ÅAâT      ÅAâT                      ÅAâT
C        *---------+---------*               *---------*
C        ! CTAPòàâ ! MãAÑòàâ ! 000000        ! MãAÑòàâ ! 000000
C        ! CTAPòàâ ! MãAÑòàâ ! 000002        ! CTAPòàâ ! 000001
C        ! CTAPòàâ ! MãAÑòàâ ! 000004        ! MãAÑòàâ ! 000002
C        ! CTAPòàâ ! MãAÑòàâ ! 000006        ! CTAPòàâ ! 000003
C        ! CTAPòàâ ! MãAÑòàâ ! 000010        ! MãAÑòàâ ! 000004
C        ! CTAPòàâ ! MãAÑòàâ ! 000012        ! CTAPòàâ ! 000005
C        ! CTAPòàâ ! MãAÑòàâ ! 000014        ! MãAÑòàâ ! 000006
C        ! ................. ! ......        ! CTAPòàâ ! 000007
C        ! ................. ! ......        ! ....... ! ......
C        ! ................. ! ......        ! ....... ! ......
C        ! CTAPòàâ ! MãAÑòàâ ! 003774        ! MãAÑòàâ ! 003776
C        ! CTAPòàâ ! MãAÑòàâ ! 003776        ! MãAÑòàâ ! 003777
C        *---------+---------*               *---------*
C
C
C        OPÉAHàáAñàü èAMüTà B MOÑEãàPìûôEâ èPOÉPAMME
C            (MACCàBõ   MEM  à  MEM1 )
C     *-------------------------------------*
C     ! CãOBO C AÑPECOM 000000              ! 0001
C     ! CTAPòàâ ÅAâT CãOBA C AÑPECOM 000000 ! 0002
C     ! CãOBO C AÑPECOM 000002              ! 0003
C     ! CTAPòàâ ÅAâT CãOBA C AÑPECOM 000002 ! 0004
C     ! CãOBO C AÑPECOM 000004              ! 0005
C     ! CTAPòàâ ÅAâT CãOBA C AÑPECOM 000004 ! 0006
C     ! CãOBO C AÑPECOM 000006              ! 0007
C     ! CTAPòàâ ÅAâT CãOBA C AÑPECOM 000006 ! 0008
C     ! CãOBO C AÑPECOM 000010              ! 0009
C     ! CTAPòàâ ÅAâT CãOBA C AÑPECOM 000010 ! 0010
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! CãOBO C AÑPECOM 003776              ! 2047
C     ! CTAPòàâ ÅAâT CãOBA C AÑPECOM 003776 ! 2048
C     *-------------------------------------*
C
CZZZ-------------------------------------------------------------------
         COMMON /MEMER/ MEM(2048), MEM1(2048)
         INTEGER  REG(8), FLAG(6), P, MEMQ(8)
         EQUIVALENCE ( N , RN )
         IER = 0
CCCC     WRITE (6,*) IPORT,IPORT1
C----------------------------------------------------------------------
C--> HAóAãà ?
         IF( TN  .GE. TN1 ) RETURN
         KX = 0
         IPORTO = IPORT
50       CONTINUE
C---> ÅEPEM OóEPEÑHìû KOMAHÑì .
         KOM = MEM( REG(8)+1 )
C---> ìBEãàóàBAEM CK HA '2'
         REG(8) = REG(8) + 2
C---> àÑEHTàîàñàPìEM KOMAHÑì à BõáõBAEM è/è, BõèOãHüûôìû ÑAHHìû KOMAHÑì
         TKOM = 0.0
         KOM1 = KOM/4096 + 1
         KOM2 = ( KOM - 4096*(KOM1-1) ) / 512 + 1
         GO TO ( 100, 1000, 1000, 1000, 1000, 1000, 1000,  200,
     *           300,11000,11000,11000,11000,11000, 1000,90006 ), KOM1
         GO TO 90001
C----------------------------------------------------------------------
100      CONTINUE
         GO TO (2000, 3000, 3000, 3000, 4000, 4000, 5000,90006 ), KOM2
         GO TO 90001
C----------------------------------------------------------------------
200      CONTINUE
         GO TO (6000, 6000, 6000, 6000, 6000, 6000,90006, 7000 ), KOM2
         GO TO 90001
C----------------------------------------------------------------------
300    CONTINUE
         GO TO (8000, 8000, 8000, 8000, 9000, 9000,10000,90006 ), KOM2
         GO TO 90001
C----------------------------------------------------------------------
C--> BõáõBAEM OÅPAÅATõBAûôàE èOÑèPOÉPAMMõ.
C----------------------------------------------------------------------
1000     CALL ADEC01( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
2000     CALL ADEC02( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
 3000    CALL ADEC03( KOM, REG, FLAG, IADPTO,IADRTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
4000     CALL ADEC04( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
5000     CALL ADEC05( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
6000     CALL ADEC06( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
7000     CALL ADEC07( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
8000     CALL ADEC08( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
9000     CALL ADEC09( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
10000    CALL ADEC10( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
         GO TO 15000
C----------------------------------------------------------------------
11000    CALL ADEC11( KOM, REG, FLAG, IADPTO,IADPTI, IADPGM, IPORT,
     *   IPORT1,TKOM,IER )
C----------------------------------------------------------------------
15000    CONTINUE
C---> OóEPEÑHAü KOMAHÑA BõèOãHEHA.
         IF( IER .NE. 0 ) P = 1
C---> ìBEãàóàM CóETóàK BPEMEHà.
         TN = TN + TKOM
C---> A óTO ì HAC C OTãAÑOóHOâ èEóATúû ?
         IF( P .NE. 1 ) GO TO 16000
C---> èEóATAEM !!!
C----------------------------------------------------------------------
C ÅEPEM KAHAã BõBOÑA OTãAÑOóHOâ èEóATà ...
C----------------------------------------------------------------------
         WRITE(NOUT,15010) TN
15010    FORMAT(5X,'TEKìôEE BPEMü C HAóAãA CóETA  T=',F10.8,' CEK.')
         DO 15015 J=1,8
         CALL ADECDQ( REG(J), MEMQ(J), IER1)
15015    CONTINUE
         WRITE(NOUT,15020) MEMQ
15020    FORMAT(5X,'COÑEPÜàMOE PEÉàCTPOB Mè(B "8" BàÑE)',
     *   ' R0=',I6,' R1=',I6,' R2=',I6,' R3=',I6,
     *   ' R4=',I6,' R5=',I6,' R6=',I6,' R7=',I6  )
         WRITE(NOUT,15030) FLAG, IPORT, IPORT1
15030    FORMAT(5X,'CãOBO COCTOüHàE  Mè(B "2" BàÑE)',
     *   '  C=',I1,'  V=',I1,'  Z=',I1,'  N=',I1,
     *   '  T=',I1,'  I/O=',I1,
     *   '  COÑEPÜàMõE èOPTOB (B "10" BàÑE)','  Bõ=',I5,'  BB=',I5 )
         WRITE(NOUT,1540)
1540     FORMAT(5X,'OÅãACTú Oáì Mè-CàCTEMõ, áAHàMAEMAü èPOÉPAMMOâ ')
         IEND = IADPGM + 1
         DO 1590 I=1,IEND,8
         I1 = I - 1
         I2 = I + 7
         DO 1545 J=1,8
         J1 = I + J - 1
         CALL ADECDQ( MEM(J1), MEMQ(J), IER1)
1545     CONTINUE
         CALL ADECDQ( I1 , IQ1 , IER1)
         WRITE(NOUT,1550)   IQ1, MEMQ
1550     FORMAT(2X,I6,10X,8(I6,4X))
1590     CONTINUE
         WRITE(NOUT,1560)
1560     FORMAT(1X,120('-'))
C
C----------------------------------------------------------------------
16000    CONTINUE
C-->  HE èOPA ãà áAKAHóàBATú ?
         IF( IER   .NE.   0    ) RETURN
         IF( IPORT .NE. IPORTO ) KX=1
         IF( TN .LT. TN1  .AND.  KX .EQ. 0 ) GO TO 50
         RETURN
C----------------------------------------------------------------------
C--> OòàÅKà
90001    CONTINUE
         IER = 1
         RETURN
C----------------------------------------------------------------------
90006    CONTINUE
         IER = 6
         RETURN
C----------------------------------------------------------------------
         END
