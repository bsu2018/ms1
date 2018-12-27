         SUBROUTINE FM0028( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT APE‹Ÿ   1989 ƒ.)------------
C   O„POƒPAMMA:   FM0028( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA‡HA—EHˆE:    HACTPO‰KA MAKPOMO„E‹ˆ MEXAHˆ—ECKO‰ CˆCTEM›
C  ------------    Tˆ 999
C
C
C              ‹EMEHT KO„ˆP“ETCŸ O OT„E‹œHO‰ O„„ˆPEKTˆBE (#MEX)
C              ---------------------------------------------------
C
C
C
C                    ”OPMAT ‹Oƒˆ—ECKO‰ CTPOKˆ OˆCAHˆŸ:
C                            ( BBO„ E‡ ”OPMATH›‰ )
C
C                  "‡" N‡  "C" NC K‡  "T" TIP  "R" RX RY RZ
C                  "P" PX PY PZ  "M" M  "J" J  "G" G  "Q" NEL NSM
C
C                  "%" .......";"  CTPOKA KOMEHTAPˆEB;
C
C                  (“KB› B " " KO„ˆP“žTCŸ KAK OKA‡AHO)
C
C
C                  N‡ - HOMEP ‡BEHA MEXAHˆ—ECKO‰ –Eˆ;
C                  NC - HOMEP CBŸ‡ˆ, COE„ˆHŸž™E‰ „AHHOE ‡BEHO
C                       C ‡BEHOM K‡;
C                  TIP- Tˆ CBŸ‡ˆ
C                        1 - BPA™EHˆE BOKP“ƒ  OCˆ  X ;
C                        2 - BPA™EHˆE BOKP“ƒ  OCˆ  “ ;
C                        3 - BPA™EHˆE BOKP“ƒ  OCˆ  Z ;
C                       -1 - CKO‹œ†EHˆE B„O‹œ OCˆ  X ;
C                       -2 - CKO‹œ†EHˆE B„O‹œ OCˆ  “ ;
C                       -3 - CKO‹œ†EHˆE B„O‹œ OCˆ  Z ;
C                *** EC‹ˆ ‡BEHO “P“ƒOE Tˆ CBŸ‡ˆ “MHO†AETCŸ HA 10. ***
C
C                   RX,RY,RZ - POEK–ˆˆ BEKTOPA R, OPE„E‹Ÿž™EƒO O‹O-
C                            †EHˆE –EHTPA MACC ‡BEHA OTHOCˆTE‹œHO
C                            HA—A‹A , CBŸ‡AHHO‰ C „AHH›M ‡BEHOM CˆCTEM›
C                            KOOP„ˆHAT;
C
C                   PX,PY,PZ - POEK–ˆˆ BEKTOPA P, OPE„E‹Ÿž™EƒO O‹O-
C                            †EHˆE HA—A‹A CBŸ‡AHHO‰ C „AHH›M ‡BEHOM
C                            CˆCTEM› KOOP„ˆHAT („‹Ÿ COE„ˆHEHˆ‰ BPA™E-
C                            HˆŸ) ˆ‹ˆ TO—Kˆ OTC—ETA OO™EHHO‰ KOOP-
C                            „ˆHAT› („‹Ÿ COE„ˆHEHˆ‰ CKO‹œ†EHˆŸ),
C                            OTHOCˆTE‹œHO –EHTPA MACC ‡BEHA  K‡;
C                   M  - MACCA ‡BEHA;
C                   J  - MOMEHT ˆHEP–ˆˆ ‡BEHA („‹Ÿ CBŸ‡E‰ TˆA 1,2,3)
C                   G  - KO””ˆ–ˆEHT †ECTKOCTˆ CBŸ‡ˆ („‹Ÿ “P“ƒˆX
C                        ‡BEHœEB )
C                   NEL,NSM - HOMEP ‹EMEHTA ‹EKTPˆ—ECKO‰ –Eˆ ˆ
C                             HOMEP CME™EHˆŸ TOKA TOƒO ‹EMEHTA,
C                             ŸB‹Ÿž™EƒOCŸ OO™EHHO‰ Cˆ‹O‰, „E‰CTB“ž-
C                             ™E‰ B COE„ˆHEHˆˆ ‡BEHA;
C
C
C   METO„:         B›—ˆC‹ŸETCŸ ˆ ‡AˆC›BAETCŸ B MSW BCŸ HEOXO„ˆMAŸ
C   -----          ˆH”OPMA–ˆŸ, HEOXO„ˆMAŸ „‹Ÿ OC‹E„“ž™EƒO
C                  PAC—ETA MAKPOMO„E‹ˆ.
C
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW - O‹Ÿ SPM ˆ SRAB;



C                            BA0:
C                            ---
C +------------+...+---------------------------------------------->
C !            !...!  HOMEP   ! Tˆ MAKPO- !  A„PEC   !  A„PEC  ! >
C ! „ˆCKPˆTOP !   ! ‹EMEHTA !  MO„E‹ˆ    !  CˆCKA  !  CˆCKA ! >
C !            !   !   ""    !   "T"      ! B "KNOT" ! B "PAR0"! >
C +------------+...+--------------------------------------------!->
C ! 1 2 3 4 5  !...!    1     !   2        !    3     !   4     ! >
C !------------!...!----------!------------!----------!---------! >
C ! “KA‡ATE‹œ---->J!   J + 0  !   J  + 1   !   J + 2  !   J + 3 ! >
C +------------+...+---------------------------------------------->
C
C -> +---------+---------------------------+----------+---------+
C  > ! Pˆ‡HAK !                           !  KO‹-BO  !         !
C  > !  TˆA   !        “CTO‰             ! ‡BEHœEB  ! “CTO‰  !
C  > ! MO„E‹ˆ  !                           !          !         !
C -> +---------+---------------------------+----------+---------!
C  > !    5    !             6             !    7     !   8     !
C  > +---------+---------------------------+----------+---------!
C  > !  J + 4  !      J + 5                !   J + 6  !   J + 7 !
C  > +---------+---------------------------+----------+---------+
C
C                            KNOT:
C                            -----
C +------------+...+---------------------------------------------......
C !            !...!  HOMEP   !   HOMEP    !  C KAKˆM !  Tˆ    !......
C ! „ˆCKPˆTOP !   !  ‡BEHA   !   CBŸ‡ˆ    !  ‡BEHOM  !  CBŸ‡ˆ  !......
C !            !   !    N‡    !    NC      !    K‡    !   TIP   !......
C +------------+...+--------------------------------------------!......
C ! 1 2 3 4 5  !...!    1     !   2        !    3     !   4     !......
C !------------!...!----------!------------!----------!---------!......
C ! “KA‡ATE‹œ---->J!   J + 0  !   J  + 1   !   J + 2  !   J + 3 !......
C +------------+...+---------------------------------------------......
C
C
C                            PAR0:
C                            ----
C +------------+...+---------+---------+---------+---------+---------+>
C !            !...!   RX    !   RY    !   RZ    !   PX    !   PY    !>
C ! „ˆCKPˆTOP !   !   1-ƒO  !   1-ƒO  !   1-ƒO  !   1-ƒO  !   1-ƒO  !>
C !            !   !  ‡BEHA  !  ‡BEHA  !  ‡BEHA  !  ‡BEHA  !  ‡BEHA  !>
C +------------+...+---------+---------+---------+---------+---------+>
C ! 1 2 3 4 5  !...!    1    !   2     !    3    !    4    !    5    !>
C !------------!...+---------+---------+---------+---------+---------+>
C ! “KA‡ATE‹œ---->J!   J + 0 !  J + 1  !   J + 2 !   J + 3 !  J + 4  !>
C +------------+...+---------+---------+---------+---------+---------+>
C
C -> +---------+---------+---------+---------+---------+---------+.....
C  > !   PZ    !    M    !    J    !    G    !  NEL    !   NSM   !.....
C  > !   1-ƒO  !   1-ƒO  !   1-ƒO  !   1-ƒO  !  1-ƒO   !   1-ƒO  !.....
C  > !  ‡BEHA  !  ‡BEHA  !  ‡BEHA  !  ‡BEHA  !  ‡BEHA  !  ‡BEHA  !.....
C -> +---------+---------+---------+---------+---------+---------!.....
C  > !    6    !    7    !    8    !    9    !    10   !    11   !.....
C  > +---------+---------+---------+---------+---------+---------!.....
C  > !  J + 5  !  J + 6  !  J + 7  !  J + 8  !   J + 9 !  J + 10 !.....
C  > +---------+---------+---------+---------+---------+---------+.....
C
C
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C
C   PˆME—AHˆE:    1. KOHTPO‹œ HE POˆ‡BO„ˆTCŸ;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C
C
C
C                  RMSW TˆA # 999
C                  ---------------
C
C ++--------+--------+--------+--------+--------+--------+..==>
C II KO‹-BO I KO‹-BO I # ‡BEHAI # ‹-TAI # CME- I A„PEC  I..==>
C II ‡BEHœEBI CBŸ‡E‰ I MEX.   I ‹.    I ™EHˆŸ  I   B    I..==>
C II        I C ‹.–.I –Eˆ   I –Eˆ   I        I TOKN/N1I..==>
C ++--------+--------+--------+--------+--------+--------+..==>
C II   1    I    2   I   3    I   4    I   5    I   6    I..==>
C ++--------+--------+--------+--------+--------+--------+..==>
C II   9    I   10   I   11   I   12   I   13   I   14   I..==>
C ++--------+--------+--------+--------+--------+--------+..==>
C
C      BCE APAMETP› EPE„AžTCŸ Pˆ OMO™ˆ "COMMON" ‹OKOB.
C
C
C
CZZZ-------------------------------------------------------------------
      COMMON /PROF1/ MASPRT(200)
      COMMON/BLINT/N,N3,N9,M(30),KK(30),IDLT(30),ISAKR(30,30),ISAIT(30)
      COMMON/BLKUST/RKST(90),ROKST(90),AIKST(270)
      COMMON/BLGRAF/INDEX(20),NGRAF,NSG
      COMMON/BLNEK/E(90),R(90),RO(540),RM(30)
      COMMON/BLQ/Q(30),QP(30),Q2P(100)
      COMMON/SYS1/KEY(20),LIM
      COMMON/BLSAIT/RKOEF(30),IP(30),ITIP(30),KOORD(6,30)
     *             ,INUM(2,30),IRKC(6,30)
      COMMON/RLI/QIR(30),DKOOR(3),IFLEX(30),NZ1,NZ2,KLU
      COMMON/CONSM/KS,NSUM,NS,NPS,NMS,NGS,NTR,NSV
      COMMON/OGR/OGR(2,30)
      COMMON/KOEFU/G(30)
      EQUIVALENCE ( N1    , RN1  )
      EQUIVALENCE ( FGMSW , MASPRT(87) )
      DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
      INTEGER BA0T, TIP
      REAL X0(30)
      DATA L,S/55555,55555.555555/
C----------------------------------------------------------------------
C--> HA—A‹ˆ .
C----------------------------------------------------------------------
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 999 )   GO TO 1000
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ‹Oƒ. CO‰ B FM0028: HOMEP TˆA HE OC‹“†ˆBAETCŸ **
     =*'/' *** HOMEP TˆA ', I10, ' ‹EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C--> CKO‹œKO ‡BEHœEB ?
C----------------------------------------------------------------------
       N1 = BA0T(7)
       RMSW(1) = RN1
C--> C—ˆTAEM KO‹-BO CBŸ‡E‰ C ‹. –Eœž ( KELC )
       KELC = 0
          DO 950  I=1,N1
          IPAR0T = 11*(I-1)+1
          NELC   = IFIX(PAR0T(IPAR0T+9))
          IF( NELC .EQ. 0 ) GO TO 950
          KELC = KELC + 1
950    CONTINUE
       IF( FGMSW .EQ. 0 ) GO TO 1010
C--> “TO—HˆTœ LM O™“ž
      LM = 10 + KELC*4
C     RETURN
1010  CONTINUE
      LM = 2  + KELC*4
         DO 1020 I=2,LM
         RMSW(I) = 0.0
1020  CONTINUE
C----------------------------------------------------------------------
C--> O„ƒOTOBˆM "COMMON" ‹OKˆ
C-->  AHA‹Oƒ / "BEGIN" KOM‹EKCA  "CADR"
C----------------------------------------------------------------------
      LIM=30
      NZ1=L
      NZ2=L
      NSV=0
      DO 1050 I=1,LIM
      ITIP(I)=0
      IP(I)=0
      INUM(1,I)=0
      INUM(2,I)=0
      RKOEF(I)=0.
      M(I)=L
      KK(I)=L
      IDLT(I)=L
      ISAIT(I)=0
      RM(I)=S
      Q(I)=S
      QP(I)=S
      Q2P(I)=S
1050  CONTINUE
      DO 1100 I=1,LIM
      DO 1100 J=1,6
      KOORD(J,I)=0
1100  IRKC(J,I)=0
      DO 1200 I=1,LIM
      DO 1200 J=1,LIM
      ISAKR(I,J)=L
1200  CONTINUE
      NN=9*LIM
      DO 1300 I=1,NN
      AIKST(I)=S
1300  CONTINUE
      NN=3*LIM
      DO 1400 I=1,NN
      ROKST(I)=S
      RKST(I)=S
1400  CONTINUE
      NN=15*LIM
      DO 1500 I=1,NN
      RO(I)=S
1500  CONTINUE
      DO 1600 I=1,20
      INDEX(I)=0
      KEY(I)=0
1600  CONTINUE
      DO 1700 I=1,LIM
      G(I)=S
      OGR(1,I)=S
      OGR(2,I)=S
1700  CONTINUE
      DO 1800 I=1,3
1800  DKOOR(I)=S
      NGRAF=L
      NSG=L
      KEY(20)=1
C----------------------------------------------------------------------
C    KOHE– / "BEGIN" KOM‹EKCA "CADR"
C----------------------------------------------------------------------
C--> BBE„EM „AHH›E.
C----------------------------------------------------------------------
C-->  AHA‹Oƒ / "INDATE" KOM‹EKCA "CADR"
C     „AHH›E BBO„ŸTCŸ „‹Ÿ PE†ˆMA    "DIN"
C----------------------------------------------------------------------
      NGS=0
      KLU=0
      N = BA0T(7)
      N3=3*N
      N9=9*N
      DO 2100 I=1,N9
      AIKST(I)=0.
2100  CONTINUE
      DO 2200 I=1,N
2200  IFLEX(I)=0
C----------------------------------------------------------------------
C--> BBE„EM MACCˆB B‡AˆMOCOE„ˆHEHˆ‰ M(I)
C--> ˆ MACCˆB KO„OB OCE‰ ˆ TˆOB COE„ˆHEHˆ‰ KK(I)
C----------------------------------------------------------------------
      DO 2250 I=1,N
      IKNOTT = (I-1)*4
      NZ  = KNOTT(IKNOTT + 1)
      NC  = KNOTT(IKNOTT + 2)
      NZ1 = KNOTT(IKNOTT + 3)
      NTC = KNOTT(IKNOTT + 4)
      M(NZ) = NZ1
      KK(NC)= NTC
2250  CONTINUE
      DO 2500 I=1,N
      J=KK(I)
      IF(J) 2300,2300,2400
2300  IDLT(I)=0
      GO TO 2500
2400  IDLT(I)=1
2500  CONTINUE
      DO 2600 I=1,N
      J=KK(I)
      LL=IABS(J)
      IF(LL.LE.3) GO TO 2550
      IFLEX(I)=1
      NGS=NGS+1
      KK(I)=KK(I)/10
2550  IF(IFLEX(I).EQ.0) KK(I)=LL
2600  CONTINUE
C----------------------------------------------------------------------
      KELC = 0
      IRMSW= 3
         DO 2700 I=1,N
         NZ = KNOTT( 4*(I-1) + 1 )
         NC = KNOTT( 4*(I-1) + 2 )
         IPAR0T = 11*(I-1) + 1
         IR =  3*(NZ-1) + 1
C--> BBO„ˆM MACCˆB POEK–ˆ‰ BEKTOPOB –EHTPOB MACC ‡BEHœEB  "RKST(3,I)
C--> ˆ MACCˆB POEK–ˆ‰ BEKTOPOB,OPE„E‹Ÿž™ˆX O‹O†EHˆE HA—A‹ I-X
C--> CˆCTEM CBŸ‡AHH›X C.K. ROKST(3,I)
         RKST(IR) = PAR0T(IPAR0T)
         RKST(IR+1) = PAR0T(IPAR0T+1)
         RKST(IR+2) = PAR0T(IPAR0T+2)
         ROKST(IR)  = PAR0T(IPAR0T+3)
         ROKST(IR+1)= PAR0T(IPAR0T+4)
         ROKST(IR+2)= PAR0T(IPAR0T+5)
C--> BBO„ˆM MACC› ‡BEHœEB
         RM(NZ) = PAR0T(IPAR0T+6)
C--> BBO„ˆM MOMEHT› ˆHEP–ˆˆ ‡BEHœEB
         X0(NZ) = PAR0T(IPAR0T+7)
C--> BBO„ˆM KOE””ˆ–ˆEHT †ECTKOCTˆ CBŸ‡ˆ
         G(NC)  = PAR0T(IPAR0T+8)
C--> BBO„ˆM CBŸ‡ˆ C ‹. –Eœž
         NELC = IFIX(PAR0T(IPAR0T+9))
         NSMC = IFIX(PAR0T(IPAR0T+10))
         IF( NELC .EQ. 0 ) GO TO 2700
         KELC = KELC + 1
         N1 = NZ
         RMSW(IRMSW) = RN1
         N1 = NELC
         RMSW(IRMSW+1) = RN1
         N1 = NSMC
         RMSW(IRMSW+2) = RN1
         IRMSW = IRMSW + 4
2700  CONTINUE
      N1 = KELC
      RMSW(2) = RN1
C---------------------------------------------------------------------
      DO 2800 I=1,N
      LL=KK(I)
      J=9*(I-1)+4*(LL-1)+1
      AIKST(J)=X0(I)
2800  CONTINUE
      DO 2900 I=1,N3
2900  RO(I)=ROKST(I)
      IF(NGS.EQ.0) GO TO 3000
C----------------------------------------------------------------------
C--> BBO„ˆM KO””ˆ–ˆEHT› †ECTKOCTˆ „‹Ÿ “P“ƒˆX ‡BEHœEB
C     OKA HE PEA‹ˆ‡OBAHO
C     *******************
C----------------------------------------------------------------------
3000  CONTINUE
C----------------------------------------------------------------------
C-->  KOHE– / "INDATE" KOM‹EKCA "CADR"
C----------------------------------------------------------------------
C-->  ‡A„AEM PE†ˆM PAOT›
C-->  AHA‹Oƒ / "MODE" KOM‹EKCA "CADR"
C-->  ‡A„AETCŸ PE†ˆM "NORMAL"
C----------------------------------------------------------------------
       DO 3100 I=1,19
3100   KEY(I) = 0
       KEY(4) = 1
       KEY(5) = 1
       KEY(10)= 1
       RETURN
C***** DEBUG INIT
       END
