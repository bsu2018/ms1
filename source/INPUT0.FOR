         SUBROUTINE INPUT0( BA0, KNOT, PAR0, NRDR1, DSNAME, RDRPGM,IRC)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C  Q - OT 20.03.86;   T - OT 05.04.86; P = OT 25.04.86; R-OT 29.05.86;
C      O„POƒPAMMA: INPUT0( BA0, KNOT, PAR0, NRDR1, DSNAME, RDRPGM,IRC)
C     --------------
C
C      HA‡HA—EHˆE:   BBO„ ˆCXO„HO‰ ˆH”OPMA–ˆˆ OˆCAHˆŸ AHA‹ˆ‡ˆP“EMO‰
C      ----------    ‹. –Eˆ. ‡AO‹HEHˆE ˆH”OPMA–ˆOHH›X MACCOBOB
C                              BA0, KNOT, PAR0.
C                    *** OPAOT—ˆK „AHH›X „ˆPEKTˆB› "#–E8" ***
C
C                    ”OPMAT ‹Oƒˆ—ECKO‰ CTPOKˆ OˆCAHˆŸ:
C                            ( BBO„ E‡ ”OPMATH›‰ )
C
C                  "" NN "T" MM "“" K1 K2 ... "" Z1 Z2.. ";"
C
C                  "%" .......";"  CTPOKA KOMEHTAPˆEB;
C
C                  (“KB› B " " KO„ˆP“žTCŸ KAK OKA‡AHO)
C
C
C                  NN - HOMEP ‹EMEHTA –Eˆ;
C                  MM - HOMEP TˆA MAKPOMO„E‹ˆ;
C                  K1 K2 ... - HOMEPA “‡‹OB O„K‹ž—EHˆŸ;
C                  Z1 Z2 ... - APAMETP› MAKPOMO„E‹ˆ;
C                     B O † M O † H O :
C                  A) ‡HA—EHˆE  ...  ‡HA—EHˆE ...
C                  ) --//-- "FT" HOMEP "<" ‡HA—EHˆŸ ... ">"
C                  B) --//--//-- "MAS" "<" ‡HA—EHˆŸ ...  ">"
C           @)-->  ƒ) --//--//--//-- "FN" "<" ˆH”. ”OPM“‹A  ">"
C                  „) --//--//--//--// "PSUB" XX "<" ‡HA—EHˆŸ ">"
C                     „ ˆ P E K T ˆ B › :
C
C                  "#BCE" - Pˆ‡HAK KOH–A OˆCAHˆŸ;
C                  "#CPOC" - PˆH“„ˆTE‹8HOE ‡ABEP˜EHˆE OˆCAHˆŸ;
C            **** CˆHOHˆM›: "#"     <=>   "#BCE"
C                           "#C"   <=>   "#CPOC"
C
C
C     METO„:       CˆHTAKCˆ—ECKˆ‰ ˆ CEMAHTˆ—ECKˆ‰  KOHTPO‹8 BBO„ˆM›X
C     -----        „AHH›X, POBEPKA  COOTBETCTBˆŸ ˆH”OPMA–OHH›X
C                  OˆCAHˆ‰ „O“CTˆM›M.
C                  ”OPMˆPOBAHˆE ˆ B›„A—A ˆH”OPMA–ˆOHH›X COO™EHˆ‰;
C                  Pˆ OHOAP“†EHˆˆ O˜ˆOK B OˆCAHˆˆ –Eˆ Pˆ DIAL=1,
C                  OC“™ECTB‹ŸETCŸ ‡APOC HA ˆCPAB‹EHˆŸ ˆ BBO„ HOBO‰
C                  ˆH”OPMA–ˆˆ; Pˆ TOM BBO„ ‡APA˜ˆBAETCŸ C
C                   CˆCTEMHOƒO KAHA‹A NRDR;
C
C    APAMETP›:   ˆH”OPMA–ˆOHH›E MACCˆB›:
C    ---------    BA0 - MACCˆB OˆCAHˆŸ ‹EMEHTOB;
C                 KNOT - MACCOB CˆCKOB “‡‹OB;
C                 PAR0 - MACCˆB CˆCKOB APAMETPOB MAKPOMO„E‹E‰;
C                 NRDR1 - KAHA‹ BBO„A ˆH”OPMA–ˆˆ OˆCAHˆŸ –Eˆ;
CPPPPRR             *** BBO„ OC“™ECTB‹ŸETCŸ —EPE‡
CPR                     BHE˜Hžž / RDRPGM:
CPR        CALL RDRPGM( KAR, IK, IKMAX, NRDRX, DSNAME, KFIZ, KODEOF )
CPR
CPR               DSNAME - MACCˆB ( DIM(11) ) ˆMEHˆ ”A‰‹A BBO„A;
CPR                         ( „‹Ÿ / CE–. OC‹“†ˆBAHˆŸ )
CPR               RDRPGM - ˆMŸ / BBO„A;
CPR               KAR - CˆMBO‹8H›‰ MACCˆB O—EPE„HO‰ CTPOKˆ,
CPR               IK, IKMAX - “KA‡ATE‹ˆ "H“†HO‰" ˆH”OPMA–ˆˆ;
CPR               KFIZ = 1 - Pˆ ”ˆ‡ˆ—ECKOM BBO„E1
CPR                    = 0 - Pˆ "MAKPO_PAC˜ˆPEHˆˆ";
CPR
CPR
CPR               KODEOF   = 0  -  CTPOK“ BBE‹ˆ, = 1  -  E.O.F.
CPR
CPPPP
C
C                 IRC = 0 --> O˜ˆOK HET;
C                     = 1 --> —ACT8 OˆCAHˆŸ ˆƒHOPˆPOBA‹OC8 ˆ‡-‡A
C                             O˜ˆOK;
C                     = 2 --> BBO„ PEKPA™EH O „ˆPEKTˆBE "#CPOC"
C                     = 3 --> - KOHE– ”A‰‹A HA KAHA‹E NRDR;
C                             ( ƒP“›E ‹Oƒ. O˜ˆKˆ ABTOMATˆ—ECKˆ
C                               B‹EK“T B›O‹HEHˆE „ˆPEKTˆB› #CPOC).
C                     = 4 --> O‹“—EHA „EPˆKTˆBA BBO„A APAMETPOB
C                             MEXAHˆ—ECKO‰ –Eˆ ( #MEX ) ;
C
C
C                  OCTA‹8H›E APAMETP› B›ˆPAžTCŸ ˆ‡ O™ˆX ‹OKOB
C                  PROF1, PROF2, PROF3;
C
C    O„POƒPAMM›: 1. HAOP O„POƒPAMM C PE”ˆKCOM APF...;
C    ------------  2. COMMON: PROF1, PROF2, PROF3;
C                  3. MSGOUT, DATER$, TIMER$ [ ....$ - BHE CˆCT. / ];
C                  4. COMMON OC‹“†ˆBAHˆŸ POƒ. APF... /PRINTN/;
C
C   PˆME—AHˆE:1.Pˆ O˜ˆKE B OˆCAHˆˆ - ˆƒHOPˆP“ETCŸ BCŸ ‹Oƒ. CTPOKA;
C   ---------- 2.Pˆ PAOTE B ˆTEPAKTˆBHOM PE†ˆME - POˆ‡BO„ˆTCŸ ‡APOC
C                HA OBTOPH›‰ BBO„ ˆH”OPM. C NRDR CˆCTEMHOƒO BBO„A;
C              2A. ˆCP. APFSQ. - BBO„ “CT. (MUDATSKAQ PROGAMMA INPUT0)
C              2. EPEKO„ˆPOBKA MA‹›X “KB B O‹8˜ˆE (OT 3.08.89)
C              3.PACO‹O†EHˆE ˆH”OPMA–ˆˆ B MACCˆBAX:
C                            BA0:
C                            ---
C +------------+...+---------------------------------------------->
C !            !...!  HOMEP   ! Tˆ MAKPO- !  A„PEC   !  A„PEC  ! >
C ! „ˆCKPˆTOP !   ! ‹EMEHTA !  MO„E‹ˆ    !  CˆCKA  !  CˆCKA ! >
C !            !   !   ""    !   "T"      ! B "KNOT" ! B "PAR0"! >
C +------------+...+--------------------------------------------!->
C ! 1 2 3 4 5  !...!    1     !   2        !    3     !   4     ! >
C !------------!...!----------!------------!----------!---------! >
C ! “KA‡ATE‹8---->J!   J + 0  !   J  + 1   !   J + 2  !   J + 3 ! >
C +------------+...+---------------------------------------------->
C
C -> +---------+---------------------------+----------+---------+...
C  > ! Pˆ‡HAK !  KO„ OˆCAHˆŸ             !  KO„›:   !         !...
C  > !  TˆA   !  APAMETPOB               ! 1-> PSUB ! KO‹-BO  !...
C  > ! MO„E‹ˆ  !    " KOD_PAR "            ! ‡APO˜EHA! “‡‹OB   !...
C -> +---------+---------------------------+----------+---------!...
C  > !    5    !             6             !    7     !   8     !...
C  > +---------+---------------------------+----------+---------!...
C  > !  J + 4  !      J + 5                !   J + 6  !   J + 7 !...
C  > +---------+---------------------------+----------+---------+...
C
C                         KNOT:
C                         -----
C       MACCˆB› KNOT  COCTOˆT ˆ‡ „ˆCKPˆTOPA ˆ CˆCKA
C        ‡HA—EHˆ‰ - HOMEPOB “‡‹OB O„K‹ž—EHˆŸ MAKPOMO„E‹ˆ -
C          E‡ CE–ˆA‹8H›X PA‡„E‹ˆTE‹E‰.
C
C                            PAR0:
C                            ----
C   HA—ˆHAETCŸ „ˆCKPˆTOPOM, O "A„PECAM" “KA‡AHH›M B BA0 - CO„EP†ˆTCŸ:
C    [ CTPOƒO B OPŸ„KE BO‡PACTAHˆŸ A„PECOB, KAK OKA‡AHO Hˆ†E ]
C
C
C       Pˆ “KA‡AHˆˆ
C    Pˆ‡HAKA KO„ˆPOBKˆ
C    APAMETPOB (KOD_PAR):
C   ( O„HO C‹OBO XXYYZZTT)        ( O C‹OBAM )
C
C
C       KOHCTAHT› (XX)          CˆCOK KOHCTAHT ( KO‹-BO ˆ‡ KOD_PAR);
C
C       ”“HK. ‡ABˆC OT
C       TEK“™EƒO BPEMEHˆ (YY)   HOMEP FUN   ‡ATEM EE APAMETP›;
C
C        ‡ABˆCˆMOCT8            KO‹ˆ—ECTBO
C      ‡A„AHHAŸ TA‹ˆ—HO          —ˆCE‹      ‡ATEM ˆX ‡HA—EHˆŸ;
C           "MAS" (ZZ)           TA‹ˆ–E
C
C
C  @)   Pˆ ‡A„AHˆˆ             A„PEC B
C       POˆ‡BO‹8HO‰            MACCˆBE
C       ”“HK. ‡ABˆCˆMOCTˆ       CˆCKOB
C       OT ‹ž›X EPEMEHH›X     POST-”OPM
C       KOOP„ˆHATHOƒO A‡ˆCA    „‹Ÿ POˆ‡-
C       EPEMEHH›X CXEM›        BO‹8H›X
C          (TT)                 ”“HK–ˆ‰
C
C        Pˆ ‡A„AHˆˆ APA-      OTPˆ–ATE‹8HOE     ‡ATEM, CˆCOK
C        METPOB —EPE‡ O„-      —ˆC‹O, O MOD(.)  APAMETPOB PSUBXX
C        POƒPAMM“ O‹8‡OBA-    PABHOE XX         #EPBOE C‹OBO# -
C        TE‹Ÿ ( ˆMŸ PSUBXX )           --         KO‹ˆ—ECTBO APA-OB
C                       --
C      @) - HE PEA‹ˆ‡OBAHO.
C      ********************
C
C
C                      „ ˆ C K P ˆ  T O P:
C                      --------------------
C  +-------------------------------------------------------------+
C  ! HOMEP MACCˆBA ! PA‡MEPHOCT8 MACCˆBA ! “KA‡ATE‹8 HA ! PE‡EPB !
C  ! B REGIOG=>PRT !    B C‹OBAX         ! CBOO„H›‰    !        !
C  +-------------------------------------------------------------+
C  !      1        !        2            !     3        ! 4 ! 5  !
C  +-------------------------------------------------------------+
C
CZZZ-------------------------------------------------------------------
         DIMENSION BA0(*),  KNOT(*), PAR0(*), MSG(25), FT(2), FN(2)
         COMMON/PRINTN/ NW6
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(800)
         COMMON /PROF3/ FUNTAB(50)
         INTEGER FL1, FL2, FL3, FL4, FL5, FLAGPP, FT, FN
         INTEGER FUNTAB, BA0, DIAL, SBROS, BCE, ER, BLANK, EOFRDR
         INTEGER FKAR, SUMER, SUMKAR, SUMEL, SKOBL, SKOBR, MEX
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
       EXTERNAL RDRPGM
       DIMENSION DSNAME(11)
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
         DIMENSION PSUB(4)
         DIMENSION FKAR(80), MAS(3), DATAX(2), TIMEX(3)
         DIMENSION KAR(80), SBROS(6), BCE(4), ER(80), MEX(4)
         INTEGER PSUB, FLAG6, FL6
         INTEGER FLY
         INTEGER FLAG1, FLAG2, FLAG3, FLAG4, FLAG5, PRT90, PPPAR0
         integer     HXX,MXX,SXX,MSXX
         integer     YYY,MYY,DYY
         DATA PSUB/ 'P', 'S', 'U', 'B' /
         DATA SUMER/0/, SUMKAR/0/, SUMEL/0/
         DATA SKOBL,  SKOBR / '<', '>' /
         DATA MAS / 'M', 'A', 'S' /, FT/ 'F', 'T' /, FN/'F', 'N' /
         DATA K72/ 72 /, K72OLD/72/
         DATA SBROS/ '#', 'C', '', 'P', 'O', 'C' /
         DATA BCE  / '#', 'B', 'C', 'E' /
         DATA MEX  / '#', 'M', 'E', 'X' /
         DATA BLANK / ' '/, EOFRDR / 0 /
         DATA LE/''/, LT/'T'/, LY/'“'/, LP/''/
         DATA LCOM/'%'/, LKON/';'/, LGOPA/'$'/
         DATA FLY/ 0 /
         DATA FLAG1, FLAG2, FLAG3, FLAG4, FLAG5/0,0,0,0,0/
         DATA IGNORE/0/, IVV1/0/, PRT90/0/
C====>  ”ˆKCA–ˆŸ „AT› ˆ BPEMEHˆ BBO„A ˆH”OPMA–ˆˆ
C        CALL TIMER$(TIMEX)
C        CALL DATER$(DATAX)
C====>  ”ˆKCA–ˆŸ „AT› ˆ BPEMEHˆ BBO„A ˆH”OPMA–ˆˆ
         CALL GETTIM_(HXX,MXX,SXX,MSXX)
         CALL GETDAT_(YYY,MYY,DYY)
         YYY = MOD ( YYY,100 )
C========> “CTAHOBKA “KA‡ATE‹E‰ ˆ PO”ˆ‹Ÿ PE†ˆMOB PAOT›;
C--> CBOO„H›E MECTA:
1111     IBA0 = BA0(3)
         IKNOT = KNOT(3)
         IPAR0 = IFIX( PAR0(3))
C====>   ”ˆKCA–ˆŸ “KA‡ATE‹E‰ „‹Ÿ B›O‹HEHˆŸ „ˆPEKTˆB› #CPOC
         IBA01 = IBA0
         IKNOT1 = IKNOT
         IPAR01 = IPAR0
C----> MAX PA‡MEP›
         MBA0 = BA0(2)
         MKNOT = KNOT(2)
         MPAR0 = IFIX( PAR0(2) )
C--> HOMEPA KAHA‹OB ˆ PE†ˆM›:
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
         NRDR00 = NRDR1
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
         NRDR = MASPRT(12)
         NWTR = MASPRT(13)
         NWTRE = MASPRT(14)
         DIAL  = MASPRT(15)
         NWTRD = MASPRT(16)
         NW6 = NWTRE
C---> “CTAHOBKA ”‹AƒOB ˆ HA—. ‡HA—. (PEEHTˆPA.)
         MSG(1) = 0
         IPRT = 0
         IRC = 0
         IER = 0
         SUMKAR = 0
         MBCE = 0
         SUMEL = 0
         SUMER = 0
         MBCE = 0
         FLAG1 = 0
         FLAG2 = 0
         FLAG3 = 0
         FLAG4 = 0
         FLAG5 = 0
         FLAG6 = 0
         FLAGPP = 0
         IGNORE = 0
         IVV1 = 0
         PRT90 = 0
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
         IK = K72OLD + 1
         IKMAX = K72OLD
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
         DO 10000 I=1,80
10000    ER(I) = BLANK
C--> ”ˆKCA–ˆŸ CTAP›X “KA‡ATE‹E‰
C-->
         IOBA0 = IBA0
         IOKNOT = IKNOT
         IOPAR0 = IPAR0
C---> ”ˆKCA–ˆŸ CTAP›X MAX HOMEPOB: “‡‹OB ˆ KO‹-BO ‹EMEHTOB
         NMKNOT = MASPRT(7)
         M3 = MASPRT(3)
         M7 = MASPRT(7)
C======================================================================
C    B›BO„ ‡AƒO‹OBKA.....
       IF ( DIAL .EQ. 1 )WRITE(NWTR, 9700) DYY,MYY,YYY,HXX,MXX,SXX,MSXX
9700     FORMAT(/ 1X,79('*')/
     = ' ***  B¢®¤ ¤ ­­ëå ¤¨à¥ªâ¨¢ë"#*–Eœ":    ¤ â  ', 2(I2,'/'),I2,
     = ' ¢à¥¬ï ',2(I2,':'),I2,'.',I3, ' ***'/ 1X,79('*')/)
       IF ( DIAL .EQ. 0 )WRITE(NWTR, 9701) DYY,MYY,YYY,HXX,MXX,SXX,MSXX
9701     FORMAT(/20X,    80('*')/ 20X,
     = '***  B¢®¤ ¤ ­­ëå ¤¨à¥ªâ¨¢ "#*–Eœ":   ¤ â  ', 2(I2,'/'),I2,
     = ' ¢à¥¬ï ',2(I2,':'),I2,'.',I3,' ***'/ 20X, 80('*')/)
C######################################################################
C######################################################################
C
C  B ‡ Ÿ T 8
C  O—EPE„HO‰            METKA  10
C  CˆMBO‹               ---------
C
C######################################################################
C######################################################################
10       CONTINUE
         IK = IK + 1
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
         IF ( IK .LE. IKMAX ) GO TO 1001
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
C----> EC‹ˆ ›‹ˆ O˜ˆKˆ  --> CKA†EM O TOM
         IF ( IER .EQ. 0 ) GO TO 1002
C=======> ƒ‹OA‹8H›‰ C.—.K. O˜ˆOK
         SUMER = SUMER + IER
C---> E—AT8 O˜ˆOK
         IF ( DIAL .EQ. 1 ) WRITE ( NWTR, 1003 ) (ER(I), I=1,80)
1003     FORMAT(80A1/' *** ‹Oƒˆ—ECKAŸ CTPOKA  -  ˆƒHOPˆP“ETCŸ ***')
         IF ( DIAL .EQ. 0 ) WRITE (NWTR, 1004 ) IER, (ER(I),I=1,80)
1004     FORMAT(1X,'*** O˜ˆOK ', I2, ' ***=>', 80A1/
     * ' *** ‹Oƒˆ—ECKAŸ CTPOKA -  ˆƒHOPˆP“ETCŸ ***')
         CALL MSGOUT( MSG )
         IF ( MBCE .EQ. 1 ) GO TO 9800
         IVV1 = 0
C$$$$$$->IGNORE = 1
         IRC = 1
         IF ( DIAL .EQ. 1 ) IRC = 0
C---> O—ˆCTKA  HE POˆ‡BO„ˆTCŸ
C$$$$$$$>PRT90 = 0
C$$$$$$$>IPRT = 0
C--->
         DO 1005 I=1,80
1005     ER(I) = BLANK
         IER = 0
1002     CONTINUE
C-------- BBO„ CTPOKˆ -------------------------------------------------
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
CRRRRR IF ( NRDR00 .NE. -1 ) GO TO  80010
C-->  BBO„ —EPE‡ RDRPGM
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
       IF ( EOFRDR  .EQ.  1  ) GO TO  80000
       CALL RDRPGM( KAR, IK, IKMAX, NRDR00, DSNAME, KFIZ, KOD )
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
       IF ( KOD .EQ. 1 ) GO TO 9999
CRRRRR GO TO 80011
80010  CONTINUE
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CRRRRRRR IF ( EOFRDR .EQ. 1 ) GO TO 80000
CRRRRRRR READ( NRDR00, 1006, END=9999) (KAR(I), I=1,80)
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
80011  CONTINUE
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
         SUMKAR = SUMKAR + KFIZ
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
C===>  "APA‹E‹8H›‰" MACCˆB FKAR B F-KO„:
1006     FORMAT(80A1)
         IF (DIAL .EQ. 0 ) WRITE( NWTR, 1007) (KAR(I), I=1,80)
         IF (DIAL .EQ. 1 ) WRITE( NWTR, 10070) (KAR(I), I=1,80)
CQQQQQQ  EPEKO„ˆPOBKA.....QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
            CALL  APFCAS ( 1, K72, KAR, KAR)
C        IF (DIAL .EQ. 0 ) WRITE( NWTR, 1007) (KAR(I), I=1,80)
C        IF (DIAL .EQ. 1 ) WRITE( NWTR, 10070) (KAR(I), I=1,80)
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         CALL APF TRT( 1, K72, KAR, FKAR )
1007     FORMAT(20X, 80A1)
10070    FORMAT(     80A1)
CRRRRRRR IK = 1
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
       IF ( IK .GT. IKMAX ) GO TO 1002
CRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
C----------------------------------------------------------------------
1001     CONTINUE
C==> BHE ‡ABˆCˆMOCTˆ OT "IGNORE" ˆ MECTA IK POBEPˆM HA‹ˆ—ˆE „ˆPEKTˆB
      IF ( KAR(IK) .EQ. SBROS(1) .AND. KAR(IK+1) .EQ. SBROS(2) .AND.
     =     KAR(IK+2) .EQ. SBROS(3) .AND. KAR(IK+3) .EQ. SBROS(4) .AND.
     =     KAR(IK+4) .EQ. SBROS(5) .AND. KAR(IK+5) .EQ. SBROS(6)  )
     =                       GO TO 9998
C======================================================================
      IF ( KAR(IK) .EQ. BCE(1)  .AND. KAR(IK+1) .EQ. BCE(2)  .AND.
     =     KAR(IK+2) .EQ. BCE(3)  .AND. KAR(IK+3) .EQ. BCE(4) )
     =                       GO TO 9997
C======================================================================
      IF ( KAR(IK) .EQ. MEX(1)  .AND. KAR(IK+1) .EQ. MEX(2)  .AND.
     =     KAR(IK+2) .EQ. MEX(3)  .AND. KAR(IK+3) .EQ. MEX(4) )
     =                       GO TO 9981
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C---> CˆHOHˆM›
         IF ( KAR(IK) .EQ. SBROS(1) .AND. KAR(IK+1) .EQ. SBROS(2)
     =        .AND. KAR(IK) .EQ. SBROS(3) ) GO TO 9998
C
         IF ( KAR(IK) .EQ. BCE(1) .AND. KAR(IK+1) .EQ. BLANK )
     =      GO TO 9997
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C===========================HA„O ‹ˆ ˆƒHOPˆPOBAT8=======================
         IF ( IGNORE .EQ. 0 ) GO TO 1010
C===> ˆƒHOPˆP“EM <=====================================================
C-----> HOBOCTˆ OT 8.2.86 =>  EC‹ˆ HA˜‹ˆ "", TO IGNORE ‡AKOH—ˆM.......
         IF (KAR(IK) .EQ. LE ) GO TO 1012
         IF (KAR(IK) .NE. LKON ) GO TO 10
1012     CONTINUE
         IF (KAR(IK) .EQ. LE ) IK = IK - 1
C---> ‹Oƒ. CTPOK“ PO ˆƒHOPˆPOBA‹ˆ
         IGNORE = 0
         IPRT = 0
         PRT90 = 0
C----> C—ˆTAEM, —TO OˆCAHˆE O—EPE„HOƒO ‹EMEHTA "BBE„EHO" O‹HOCT8ž
         FLAG5 = 1
C---> BOCTAHOBˆM "CTAP›E" “KA‡ATE‹ˆ
         IBA0 = IOBA0
         IKNOT = IOKNOT
         IPAR0 = IOPAR0
C===> O‡AOTˆMCŸ O ”‹AƒAX
         FLAG1 = 0
         FLAG2 = 0
         FLAG3 = 0
         FLAG4 = 0
         FLAG6 = 0
         PRT90 = 0
         GO TO 10
C######################################################################
C######################################################################
C   PA‡OP
C  ˆCXO„H›X      METKA 1010
C   „AHH›X       -----------
C######################################################################
C######################################################################
1010     CONTINUE
C@@@@@@@@@@@@@@@@ ƒAH„ˆKA [OKA ‹OKˆPOBAH ] @@@@@@@@@@@@@@@@@@@@@@@@@
C@@@@@@@@ IF ( KAR(IK) .EQ. LKON ) GO TO 6
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF (PRT90 .EQ. 1 ) GO TO 90
C---> BEPHEMCŸ B ‹OKˆ ‡APOCOB, EC‹ˆ BCE O"KEY
         GO TO (1,  2,  3,  4,  5,  6   ), IPRT
C---> “KB›:       T   “      %   ;       EPEXO„›....
C---> ˆHA—E "HA—A‹8H›‰" PA‡OP:
         IPRT = 0
C===== "CˆTO" =======>
         IF ( KAR(IK) .EQ. LE ) IPRT=1
         IF ( KAR(IK) .EQ. LT ) IPRT=2
         IF ( KAR(IK) .EQ. LY ) IPRT=3
         IF ( KAR(IK) .EQ. LP ) IPRT=4
         IF ( KAR(IK) .EQ. LCOM ) IPRT=5
         IF ( KAR(IK) .EQ. LKON ) IPRT=6
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( IPRT .NE. 0 ) GO TO 10
C-----> Bˆ„ˆMO O˜ˆKA....OHA MO†ET ›T8 O XO„“ PA‡OPA ‹Oƒ. ˆ‹ˆ B HA—.
         IF ( FLAG5 .EQ. 0 ) GO TO 1013
         IGNORE = 1
         ER(IK) = LGOPA
         IER = IER + 1
         IF ( IVV1 .EQ. 1 ) GO TO 10
         IVV1 = 1
         I = MSG(1)
         MSG(I+2) = 1
         MSG(1) = I + 1
         GO TO 10
1013     CONTINUE
C====>   O˜ˆKA B CPE„ˆHE PA‡OPA
         IGNORE = 1
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 15
         MSG(1) = I+1
         GO TO 10
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
1014     CONTINUE
C---> HEO†ˆ„AHH›‰ KOHE– ‹Oƒ. CTPOKˆ
         IGNORE = 1
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 20
         MSG(1) = I+1
         IK = IK - 1
         GO TO 10
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C######################################################################
C######################################################################
C   BBECTˆ
C   –E‹OE       M E T K A   90
C   —ˆC‹O       --------------
C######################################################################
C######################################################################
90       CONTINUE
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         PRT90 = 1
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO 1014
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         PRT90 = 0
         N2 = IKMAX - IK + 1
         N2OLD = N2
         CALL APF SQI( FKAR, N0, IK, N2, IFI )
         GO TO ( 9001, 9002, 9003, 9004, 9005, 9006 ), IFI
9004     CONTINUE
9005     CONTINUE
C!!!-------------------------------------------------------------------
         WRITE( NWTRE, 9007 ) IPRT, IFI
9007     FORMAT(' *** ‹Oƒ. CO‰ B INPUT0: IPRT=', I12,' IFI=',I10)
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
9006     CONTINUE
C===> TO‹8KO ‡HAK ˆ‹ˆ O„Hˆ POE‹›, OPOCˆM HOB“ž CTPOK“
         PRT90 = 1
         GO TO 10
9003     CONTINUE
C---> EPEO‹HEHˆE
         IK = IK - 1
         IGNORE = 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 2
         MSG(1) = I + 1
         IER = IER + 1
         GO TO 10
9002     CONTINUE
C===>  EC‹ˆ N2 = N2OLD ==> TO CPA‡“ BCTETˆ‹CŸ HE CˆMBO‹, —ˆCE‹ HE›‹O!!!
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( N2 .EQ. (N2OLD-1) ) GO TO 9009
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C----> BBE‹ˆ, BCTPETˆ‹ˆ HE –ˆ”P“ => IK HA„O CMECTˆT8 ‡A TOT CˆMBO‹
C----> „O“CTˆM OC‹E –ˆ”P› TO‹8KO POE‹ ˆ ";"
       IK  = IK - 1
       IF ( KAR(IK+1) .EQ. BLANK .OR. KAR(IK+1) .EQ. LKON ) GO TO 9001
C===> OKOH—AHˆE OC‹E –ˆ”P› O˜ˆO—HOE
9009       IF ( N2 .EQ. N2OLD ) IK = IK - 1
         IGNORE = 1
         ER(IK+1) = LGOPA
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I + 1
         IER = IER + 1
         GO TO 10
9001     CONTINUE
C=====> BCE O"KEY   BEPHEMCŸ B ‹OK ‡APOCA
         GO TO (1,  2,  3,  4,  5,  6 ),IPRT
C--->              T   “      %   ;
         GO TO 9005
C######################################################################
C######################################################################
C  PˆEM
C  HOMEPA         M E T K A  1
C  ‹EMEHTA       ------------
C######################################################################
C######################################################################
1        CONTINUE
C===> CPOC ”‹AƒA: "›‹O BBE„EHO O‹HOCT8ž OˆCAHˆE ‹EMEHTA" <========
         FLAG5 = 0
         IF ( FLAG1 .EQ. 1 ) GO TO 101
         FLAG1 = 1
C---> B‡ŸT8 —ˆC‹O ( B N0 )
         GO TO 90
C===> ‹AƒOO‹“—HO O‹“—ˆ‹ˆ, OPˆXO„“EM EƒO
101      CONTINUE
         IF ( IBA0 + MASPRT(4) .GT. MBA0 ) GO TO 777
         BA0( IBA0 ) = N0
         IPRT = 0
         GO TO 10
C######################################################################
C######################################################################
C  PˆEM
C  HOMEPA     M E T K A   2
C  TˆA       --------------
C######################################################################
C######################################################################
2        CONTINUE
C===> CPOC ”‹AƒA: "›‹O BBE„EHO O‹HOCT8ž OˆCAHˆE ‹EMEHTA" <========
         FLAG5 = 0
         IF ( FLAG2 .EQ. 1 ) GO TO 202
         FLAG2 = 1
C===> B‡ŸT8 –E‹OE —ˆC‹O B N0
         GO TO 90
202      CONTINUE
C===>  —ˆC‹O B‡Ÿ‹ˆ, POBEPˆM „O“CTˆMOCT8 # TˆA
         NTIP = N0
C IM = KO‹-BO PEA‹ˆ‡. MAKPOMO„.,  N1MACT = —ˆC‹O C‹OB B MACTAB HA 1 MO„.
         IM = MASPRT(10)
         N1MACT = MASPRT(17)
         DO 203 I=1,IM
C   OPE„E‹ˆM, EC‹ˆ Tˆ ECT8, “KA‡ATE‹8 HA CTPOK“ OˆC. MAKPOMO„E‹ˆ
         INTIP = (I-1) * N1MACT + 1
         IF ( MACTAB( INTIP )  .EQ.  NTIP  ) GO TO 204
203      CONTINUE
C===>  Tˆ O˜ˆO—EH ˆ‹ˆ HE PEA‹ˆ‡OBAH
         ER(IK-1) = LGOPA
         IER = IER + 1
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 4
         MSG(1) = I + 1
         GO TO 10
204      CONTINUE
C===> PˆHŸT8 HOMEP TˆA ˆ Pˆ‡HAK “‡‹OB MO„E‹ˆ
         BA0(IBA0 + 1) = NTIP
         BA0(IBA0 + 4) = MACTAB( INTIP + 4 )
C===> ‡A”ˆKCˆPOBAT8 KO„ KO„ˆPOBKˆ APAMETPOB ˆ KO‹-BO “‡‹OB
C                           KODPAR                KKKNOT
         KODPAR = MACTAB( INTIP + 1 )
         IF (KODPAR .EQ. 0 ) FLAGPP = 1
         KKKNOT = MACTAB( INTIP + 3 )
C====> Pˆ‡HAK HA‹ˆ—ˆŸ O„POƒPAMM› O‹8‡OBATE‹Ÿ
         ISUB = MACTAB(INTIP + 2)
C=====> ECT8 ‹ˆ APAMETP  "PSUB", EC‹ˆ „A, TO FLAGPP=0
         IF ( ISUB .NE. 0 ) FLAGPP = 0
C=====> ‡AˆCAT8 “KA‡ATE‹8 HA KNOT B BA0
         BA0( IBA0 + 2 ) = IKNOT
C===>  ‡AˆC8 “KA‡ATE‹Ÿ HA PAR0 B BA0
         BA0( IBA0 + 3 ) = 0
         BA0( IBA0 + 3 ) = IPAR0
C====>  ‡AˆC8 HA‹ˆ—ˆŸ PSUB_XX
         BA0( IBA0 + 6 ) = MACTAB( INTIP + 2 )
C======> ‡AˆC8 KO„A TˆA APAMETPOB
         BA0( IBA0+5 ) = KODPAR
C=====>  ‡AˆC8 KO‹ˆ—ECTBA “‡‹OB MO„E‹ˆ   [@]
         BA0( IBA0+7) = KKKNOT
C=====>  ‡AKOH—ˆM OPAOTK“  "T"
         IPRT = 0
         GO TO 10
C######################################################################
C######################################################################
C    BBO„
C    “‡‹OB            M E T K A  # 3
C  O„K‹ž—EHˆŸ       ---------------
C######################################################################
C######################################################################
3        CONTINUE
C===> CPOC ”‹AƒA: "›‹O BBE„EHO O‹HOCT8ž OˆCAHˆE ‹EMEHTA" <========
         FLAG5 = 0
         IF ( FLAG2 .NE. 0 ) GO TO 301
C=====>  PˆHŸT8 “‡‹› HE MO†EM, HEˆ‡BECTEH HOMEP TˆA MAKPOMO„E‹ˆ
         ER(IK) = LGOPA
         IER = IER + 1
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 5
         MSG(1) = I + 1
C=====>  BCTAT8 HA PE„. CˆMBO‹
         IK = IK - 1
         GO TO 10
301      CONTINUE
C====> Tˆ ˆ‡BECTEH
         IF ( FLAG3 .EQ. 1 ) GO TO 302
         FLAG3 = 1
         IX = 0
         FLY = 0
302      CONTINUE
C===== –ˆK‹ BBO„A HOMEPOB “‡‹OB ====HA 305 KOƒ„A ›‹ BBO„ O—EPE„HOƒO===
         IF ( FLY .EQ. 1 ) GO TO 305
C==>  B‡ŸT8 HOMEP O—EPE„HOƒO B N0
         FLY = 1
         GO TO 90
305      CONTINUE
C===> O—EPE„HO‰ HOMEP PˆHŸ‹ˆ, „O“CTˆM ‹ˆ OH
         IF (  N0 .GE. 0 ) GO TO 303
C===> HOMEP O˜ˆO—EH
         ER(IK) = LGOPA
         IER = IER + 1
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 6
         MSG(1) = I + 1
         GO TO 10
303      CONTINUE
C===> PˆMEM HOMEP “‡‹A
         IX = IX + 1
         IF ( N0 .GT. NMKNOT ) NMKNOT = N0
         KNOT(IKNOT) = N0
         IKNOT = IKNOT + 1
         IF ( IKNOT .GT. MKNOT ) GO TO 778
C===> E™E “‡‹› H“†H› ?
         IF ( IX .GE. KKKNOT ) GO TO 304
C====> PO„O‹†ˆM BBO„ HOMEPOB “‡‹OB
         FLY = 0
         IPRT = 3
         GO TO 10
304      CONTINUE
C====> BCE “‡‹› BBE‹ˆ, ‡AKAH—ˆBAEM
         IPRT = 0
         GO TO 10
C######################################################################
C######################################################################
C     BBO„
C  APAMETPOB         M E T K A  # 4
C  MAKPOMO„E‹ˆ        --------------
C######################################################################
C######################################################################
4        CONTINUE
C===> CPOC ”‹AƒA: "›‹O BBE„EHO O‹HOCT8ž OˆCAHˆE ‹EMEHTA" <========
         FLAG5 = 0
C====> PˆHˆMAEM APAMETP›
         IF ( FLAG2 .NE. 0 ) GO TO 401
C====> PˆHŸT8 HE MO†EM - HEˆ‡BECTEH Tˆ MAKPOMO„E‹ˆ
         IER = IER + 1
         ER(IK) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 7
         MSG(1) = I+1
         GO TO 10
401      CONTINUE
C====>  PA‡OP ˆ PˆEM APAMETPOB.
C===> ECT8 ‹8 B „AHHOM TˆE APAMETP› ?
         IF ( KODPAR .NE. 0 ) GO TO 403
C===> MO†ET ›T8 ECT8 "PSUB" ?
         FLAG6 = 0
         IF ( ISUB .EQ . 1 ) GO TO 20000
C===> „AHH›‰ Tˆ B APAMETPAX HE H“†„AETCŸ ( H“ OƒO„ˆ!!)
         IER = IER + 1
         ER(IK) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 8
         MSG(1) = I + 1
         GO TO 10
403      CONTINUE
         IF ( FLAG4 .EQ. 1 ) GO TO 402
         FL1 = 0
         FL2 = 0
         FL3 = 0
         FL4 = 0
         FL5 = 0
         FLAG6 = 0
         FLAGPP = 0
         FLAG4 = 1
C====> –ˆK‹ BBO„A APAMETPOB ==========================================
C       KODPAR - KO„ KO„ˆPOBKˆ APAMETPOB, B›„E‹ˆM KOMOHET›:          @
C  KCONST    NFUNT   NMAS    KFUNN
C    **       **      **      [HEPEA‹ˆ‡OBAHO]
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C===> PA‡EPEM  KOD_PAR:
         KCONST = KODPAR / 1 00 00 00
         I1 = KODPAR - KCONST * 1 00 00 00
         KFUNT = I1 / 1 00 00
         I2 = I1 - KFUNT * 1 00 00
         KMAS = I2 / 1 00
         KFUNN = I2 - KMAS * 1 00
C===> C.—.K.  IPX1  IPX2   IPX3  IPX4
C „‹Ÿ ==>    KCONST KFUNT   KMAS   KFUNN
C              *     *        *       @        HE PEA‹ˆ‡OBAH›
C  OC‹E OPAOTKˆ O—EPE„HO‰ PO–ˆˆ “KA‡ATE‹ˆ KCONST ˆ „P. OH“‹ŸžTCŸ
C
         IPX1 = 0
         IPX2 = 0
         IPX3 = 0
         IPX4 = 0
402      CONTINUE
C----> KTO O—EPE„HO‰ ?
         IF ( KCONST .EQ. 0 ) GO TO 450
C===> HEOXO„ˆMO PˆHŸT8 KCONST —ˆCE‹, C.—.K. IPX1...
C=====>  KCONST  ECT8, IPX1  -  KO‹ˆ—ECTBO, BBO„ˆM.....
C@@@@@@@@--->  METKA 45100  -  "ƒP“AŸ" KOH–OBKA @@@@@@@@@@@@@@@@@@@@@@
         IF ( KAR(IK) .EQ. LKON  .AND. IPX1 .EQ. KCONST ) GO TO 45100
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO 1014
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         N2 = IKMAX - IK + 1
         N2OLD = N2
         CALL APF SQE( FKAR, E0, IK, N2, IFE)
         GO TO ( 411, 412, 413, 414, 415, 416 ), IFE
414      CONTINUE
415      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE (NWTRE, 407 ) IFE
407      FORMAT(' *** ‹Oƒ. CO‰ B INPUT0: (APAMETP›) IFE= ', I12)
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
416      CONTINUE
C===> O‹E KOH—ˆ‹OC8, POE‹› ˆ‹ˆ ‡HAK, OPO“EM E™E...
         GO TO 10
413      CONTINUE
C====> EPEO‹HEHˆE
         IK = IK - 1
         IER = IER + 1
         ER(IK) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 2
         MSG(1) = I + 1
         GO TO 10
412      CONTINUE
C=====>  HE CˆMBO‹ --> KTO OH ?  TO‹8KO  „O“CTˆM POE‹  ˆ ";"
C====>  EC‹ˆ N2 = N2OLD TO CPA‡“ HE –ˆ”PA...
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( N2 .EQ.  (N2OLD-1) ) GO TO 491
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IK = IK - 1
         IF ( KAR(IK+1) .EQ. BLANK .OR. KAR (IK+1) .EQ. LKON )
     =                       GO TO 411
C====> ˆHA—E HE „O. CˆMBO‹....
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
491      IF ( N2 .EQ. (N2OLD-1) ) IK = IK - 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IER = IER + 1
         ER(IK+1) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I + 1
         GO TO 10
411      CONTINUE
C====> PˆHŸT8 APAMETP....
C=======                                    ========
C
         IPX1 = IPX1 + 1
         PAR0(IPAR0) = E0
         IPAR0 = IPAR0 + 1
         IF ( IPAR0 .GT. MPAR0 ) GO TO 779
         IF ( IPX1 .GE. KCONST ) GO TO 451
C===> ˆHA—E HA C‹E„“ž™“ž KOHCTAHT“
         IPRT = 4
         GO TO 10
C=====>
451      CONTINUE
C-----> BCE KOHCTAHT› BBE‹ˆ..
         KCONST = 0
         IF (KAR(IK) .EQ. LKON) IK = IK - 1
C======> O—ˆCTˆMCŸ
         FL1 = 0
         FL2 = 0
         FL3 = 0
         FL4 = 0
         FL5 = 0
         GO TO 10
450      CONTINUE
C   ===>  ECT8 ‹ˆ FUN(T)  =============================================
         IF (KFUNT .EQ. 0 ) GO TO 452
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF ( FL5 .EQ. 1 ) GO TO 513
         IF ( FL4 .EQ. 1 ) GO TO 509
         IF ( FL3 .EQ. 1 ) GO TO 507
         IF ( FL2 .EQ. 1 ) GO TO 503
         IF ( FL1 .EQ. 1 ) GO TO 502
C---->
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO 1014
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. FT(1) .AND.  KAR(IK+1) .EQ. FT(2)  )
     =                GO TO 501
C--->  PO“™EH "FT"
         IER = IER + 1
         ER(IK) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 12
         MSG(1) = I + 1
         IK = IK - 1
         GO TO 10
501      CONTINUE
C--->  "FT"  PˆHŸ‹ˆ, EPE‰„EM HA HOMEP
         FL1 = 1
         IK = IK + 1
         GO TO 10
502      CONTINUE
C====> HOMEP OTPE“EM
         FL2 = 1
         GO TO 90
503      CONTINUE
C---> B N0 HOMEP, „O“CTˆM ‹ˆ OH
         IF ( N0 .LE. 0 ) GO TO 505
C===> OˆCK O TA. „O. FUN(T)
C-----> CTPOKA TA‹ˆ–› FUNTAB = 4 C‹OBAM !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         I1 = 1
517      CONTINUE
         IF ( FUNTAB(I1)  .EQ.  0 ) GO TO 505
         IF ( FUNTAB(I1)  .EQ.  N0 ) GO TO 506
         I1 = I1 + 4
         GO TO 517
505      CONTINUE
C===>  O˜ˆKA B HOMEPE FUN(T)
         ER(IK) = LGOPA
         IGNORE = 1
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 13
         MSG(1) = I+1
         GO TO 10
506      CONTINUE
C---> PˆHŸ‹ˆ B N0 HOMEP FUN(T), OPˆXO„“EM EƒO,  O†ˆ„AEM "<"
         PAR0(IPAR0) = N0
         FL3 = 1
         IPAR0 = IPAR0 + 1
         IF ( IPAR0 .GT. MPAR0 ) GO TO 779
C==> KO‹-BO APAMETPOB FUN(T)
         KKKFUN = FUNTAB(I1 + 1)
C---->  BO‡MEM C‹. CˆMBO‹ ‡A HOMEPOM FT....
         GO TO 10
507      CONTINUE
         IF ( KAR(IK)  .EQ.  BLANK ) GO TO 10
         IF ( KAR(IK)  .EQ.  SKOBL ) GO TO 508
C--->  ˆHA—E O˜ˆKA - "<" HET      IXF - C.—.K ‹EMEHTOB <...>
         IER = IER + 1
         IGNORE = 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 17
         MSG(1) = I+1
         IK = IK - 1
         GO TO 10
508      CONTINUE
         FL4 = 1
         IXF = 0
         GO TO 10
509      CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. SKOBR  .AND.  IXF .LT. KKKFUN ) GO TO 516
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO 1014
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         N2 = IKMAX - IK + 1
         N2OLD = N2
         CALL APF SQE( FKAR, E0, IK, N2, IFE)
         GO TO (510, 511, 464, 414, 415, 416), IFE
         GO TO 414
511      CONTINUE
         IK1 = IK
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( N2 .EQ. (N2OLD-1) ) GO TO 591
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IK = IK - 1
         IF ( KAR(IK1)  .EQ.  BLANK ) GO TO 510
        IF( KAR(IK1) .EQ. SKOBR .AND. (IXF+1) .LT. KKKFUN ) GO TO 516
         IF ( KAR(IK1) .EQ. SKOBR ) GO TO 510
C===> ˆHA—E HE „O. CˆMBO‹
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
591      IF ( N2 .EQ. (N2OLD-1) ) IK = IK - 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IER = IER + 1
         ER(IK1) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I + 1
         GO TO 10
516      CONTINUE
C==> MA‹ CˆCOK <....>
         IGNORE = 1
         IER = IER + 1
         ER(IK1) = LGOPA
         I = MSG(1)
         MSG(I+2) = 16
         MSG(1) = I+1
         GO TO 10
510      CONTINUE
C===>  PˆHŸT8 APAMETP
         IXF = IXF + 1
         PAR0(IPAR0) = E0
         IPAR0 = IPAR0 + 1
         IF (IPAR0 .GT. MPAR0 ) GO TO 779
C===> H“†H› ‹ˆ E™E APAMETP› CˆCKA <....>
         IF ( IXF .LT. KKKFUN ) GO TO 10
C===> HE H“†H›,  ‹OBˆM ">"
         FL5 = 1
         GO TO 10
513      CONTINUE
C--->  †„EM ">"
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. SKOBR ) GO TO 514
C===>  O˜ˆKA HET ">"
         IER = IER + 1
         IGNORE = 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 14
         MSG(1) = I+1
         IK = IK - 1
         GO TO 10
514      CONTINUE
C===>  BCž O—EPE„H“ž " FT # <....> " BBE‹ˆ,  E™E H“†HO?
C$$$$$$$$IF (KAR(IK) .EQ. SKOBR ) IK = IK - 1
         IPX2 = IPX2 + 1
         IF ( IPX2 .GE. KFUNT ) GO TO 515
C===>  HA HOB“ž ....
         FL1 = 0
         FL2 = 0
         FL3 = 0
         FL4 = 0
         FL5 = 0
         IPRT = 4
         GO TO 10
515      CONTINUE
C===>  ‡AKOH—ˆM OPAOTK“  "FT"
         IF (KAR(IK) .EQ. LKON ) IK = IK -1
         KFUNT = 0
         FL1 = 0
         FL2 = 0
         FL3 = 0
         FL4 = 0
         FL5 = 0
         GO TO 10
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
452      CONTINUE
C====> PˆMEM MACCˆB›: ==EC‹ˆ OHˆ ECT8=================================
         IF (KMAS .EQ. 0 ) GO TO 454
C....
         IF ( FL2 .EQ. 1 ) GO TO 465
         IF ( FL1 .EQ. 1 ) GO TO 468
C====>  O†ˆ„AEM  MAS <
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. MAS(1) .AND .
     =        KAR(IK+1) .EQ. MAS(2) .AND .
     =        KAR(IK+2) .EQ. MAS(3)  ) GO TO  460
C====>  PO“™EH "MAS"
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 10
         MSG(1) = I+1
         IGNORE = 1
         IK = IK - 1
         GO TO 10
460      CONTINUE
C--->  O†ˆ„AEM HA—A‹O CˆCKA "MAS"
         FL1 = 1
         IPRT = 4
         IK = IK + 2
         GO TO 10
468      CONTINUE
C====>  †„EM "<"
         IPRT = 4
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. SKOBL ) GO TO 461
C====> O˜ˆKA  HET  "<" !!!
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 11
         MSG(1) = I+1
         IGNORE = 1
         IK = IK - 1
         GO TO 10
461      CONTINUE
C====> HA˜‹ˆ "<",  OTKPOEM MACCˆB
C     ==> IPX3 - C.—.K.  KO‹ˆ—ECTBA MAS <===> KMAS ...
         FL2 = 1
C----> “CTAHOBKA “KA‡ATE‹E‰  C.—.K. —ˆCE‹ B MACCˆBE MAS < ... >
         IPXM1 = 0
C-----> ‡AOMHˆM “KA‡ATE‹ˆ B PAR0 HA O‹E "KO‹-BO ‹. B CˆC. MAS"
         IPXM = IPAR0
C......................................................................
         IPAR0 = IPAR0 + 1
         IF ( IPAR0 .GT. MPAR0 ) GO TO 779
C====> BBO„ˆM CˆCOK —ˆCE‹.....
         GO TO 10
465      CONTINUE
C======> O—EPE„HOE —ˆC‹O BBE‹ˆ: HET ‹ˆ ">", A OE‹› ˆƒHOPˆP“EM B P“—H“ž
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. SKOBR ) GO TO 466
C===>  PˆHˆMAEM MACCˆB .....
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO 1014
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
C....
         N2 = IKMAX - IK + 1
         N2OLD = N2
         CALL APF SQE( FKAR, E0, IK, N2, IFE)
         GO TO ( 462, 463, 464, 414, 415, 416 ), IFE
         GO TO 414
464      CONTINUE
C====> EPEO‹HEHˆE
         IK = IK - 1
         IER = IER + 1
         ER(IK) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 2
         MSG(1) = I + 1
         GO TO 10
463      CONTINUE
         IK = IK - 1
C===>  MO†ET ›T8 -->  CPA‡“ HE CˆMBO‹?
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( N2 .EQ. (N2OLD-1) )  GO TO 20011
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C=> HE CˆMBO‹ --> KTO OH ?  TO‹8KO  „O“CTˆM POE‹, Pˆ ">" KOH—AEM...
         IF ( KAR(IK+1) .EQ. BLANK )
     =                       GO TO 462
         IF ( KAR(IK+1) .EQ. SKOBR ) GO TO 462
C====> ˆHA—E HE „O. CˆMBO‹....
20011    IER = IER + 1
         ER(IK+1) = LGOPA
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I + 1
         GO TO 10
462      CONTINUE
C====> PˆHŸT8 APAMETP....
         PAR0(IPAR0) = E0
         IPXM1 = IPXM1 + 1
         IPAR0 = IPAR0 + 1
         IF ( IPAR0 .GT. MPAR0 ) GO TO 779
         GO TO 10
C====>  „O†„A‹ˆC8 ">"  ...
466      CONTINUE
         IPX3 = IPX3 + 1
C---> KO‹-BO —ˆCE‹ B PAR0:
         PAR0( IPXM ) = IPXM1
C---> HE KOH—ˆT8 ‹ˆ .....$ !!!!!!
         IF ( IPX3 .GE . KMAS ) GO TO 467
C----> H“†EH HOB›‰ MACCˆB....
         FL1 = 0
         FL2 = 0
         IPRT = 4
         GO TO 10
467      CONTINUE
C**********************************************************************
C===> ‡AKAH—ˆBAEM OPAOTK“ "MAS"  -  BCEX OˆCAHˆ‰
         IF ( KAR(IK) .EQ. LKON ) IK = IK - 1
         KMAS = 0
         FL1 = 0
         FL2 = 0
         FL3 = 0
         FL4 = 0
         FL5 = 0
C$$$$$$$$IF ( KAR(IK) .EQ. SKOBR ) IK = IK - 1
         GO TO 10
454      CONTINUE
C====>  OCTA‹ˆC8 FUN( #N# )
         IF ( KFUNN .EQ. 0 ) GO TO 499
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C====> HA„O PˆHˆMAT8 FUN( #N# )
C!!!-------------------------------------------------------------------
         WRITE (NWTRE, 500)
500      FORMAT(' *** ”“HK. ‡ABˆCˆMOCTˆ OT EPEMEHH›X HE PEA‹ˆ‡OBAH› **
     =* ')
C!!!-------------------------------------------------------------------
         KFUNN = 0
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
499      CONTINUE
C  KOHE– PA‡OPA APAMETPOB
C===>  A HET ‹ˆ "PSUB" ?
         IF ( ISUB .EQ. 1 ) GO TO 20000
C====> ˆHA—E BCE BBE‹ˆ.....
20012    CONTINUE
         IF ( KAR(IK) .EQ. LKON ) IK = IK - 1
         IPRT = 0
         FLAGPP = 1
C======================================================================
         GO TO 10
C######################################################################
C  OPAOT—ˆK
C  "PSUB"        M E T K A  20000
C  V.M= 1.3      -----------------
C######################################################################
20000    CONTINUE
         IF ( FL4 .EQ. 1 ) GO TO 20007
         IF ( FL3. EQ. 1 ) GO TO 20005
         IF ( FL2. EQ. 1 ) GO TO 20004
         IF ( FL1. EQ. 1 ) GO TO 20003
C===>  †„EM   PSUB HOMEP  <......>
         IF ( FLAG6 .EQ. 1 ) GO TO 20001
C===> ƒOTOBˆMCŸ K PˆEM“
         FL1 = 0
         FL2 = 0
         FL3 = 0
         FL4 = 0
         FL5 = 0
         FL6 = 0
         FLAG6 = 1
20001    CONTINUE
C===>  O†ˆ„AEM "PSUB"
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. PSUB(1) .AND. KAR(IK+1) .EQ. PSUB(2)
     =                             .AND.
     =      KAR(IK+2) .EQ. PSUB(3) .AND. KAR(IK+3) .EQ. PSUB(4) )
     =                        GO TO  20002
C===>  HET "PSUB"
         IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 18
         MSG(1) = I + 1
         IER = IER + 1
         ER(IK) = LGOPA
         IK = IK - 1
         GO TO 10
20002    CONTINUE
C===> "PSUB"  PˆHŸ‹ˆ  †„EM EE HOMEP
         IK = IK + 3
         FL1 = 1
         GO TO 10
20003    CONTINUE
C===>  PˆHˆMAEM HOMEP PSUB
         FL2 = 1
         GO TO 90
20004    CONTINUE
C===>  N0  HOMEP PSUB,  B PAR0 EƒO...
         N0 = - IABS(N0)
         PAR0(IPAR0) = N0
         IPAR0 = IPAR0 + 1
         IF ( IPAR0 .GT. MPAR0 ) GO TO 779
C===>  †„EM "<"
         FL3 = 1
         GO TO 10
20005    CONTINUE
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. SKOBL ) GO TO 20006
C===> O˜ˆKA, HET "<"
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 19
         MSG(1) = I+1
         IK = IK - 1
         GO TO 10
20006    CONTINUE
C===>   "<" HA˜‹ˆ, PˆHˆMAEM —ˆC‹A, ISPAR0 - MECTO „‹Ÿ ˆX KO‹-BA
         ISPAR0 = IPAR0
         IPAR0 = IPAR0 + 1
         IF (IPAR0 .GT. MPAR0 ) GO TO 779
C===>  ISX  C.—.K.  KO‹-BA AP-B B CˆCKE <....>
         ISX = 0
         FL4 = 1
         GO TO 10
20007    CONTINUE
C====PˆEM AP-B CˆCKA: Pˆ ">" KOH—AEM, A PO. ˆƒHOP. B P“—H“ž
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. SKOBR ) GO TO 20008
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO 1014
CQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ
         N2 = IKMAX - IK + 1
         N2OLD = N2
         CALL APF SQE( FKAR, E0, IK, N2, IFE )
         GO TO ( 20009, 20010, 413, 414, 415, 416 ), IFE
         GO TO 414
20010    CONTINUE
C====>  HE –ˆ”PA: „O. POE‹ ˆ‹ˆ ">"
         IK = IK - 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( N2 .EQ. (N2OLD-1) ) GO TO 20011
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( KAR(IK+1) .EQ. BLANK  .OR.
     =        KAR(IK+1) .EQ. SKOBR  ) GO TO 20009
C===> O˜ˆKA HE —ˆC‹O...
         GO TO 20011
20009    CONTINUE
C====>   PˆHŸT8  —ˆC‹O:
         PAR0(IPAR0) = E0
         ISX = ISX + 1
         IPAR0 = IPAR0 + 1
         IF ( IPAR0 .GT. MPAR0 ) GO TO 779
C===>  ˆHA—E HA HOBOƒO
         GO TO 10
20008    CONTINUE
C@@@@@@@@IF ( KAR(IK) .EQ. SKOBR ) IK = IK - 1
C===>  „O†„A‹ˆC8  ">"
         PAR0(ISPAR0) = ISX
C====>  BCE!!!
         ISUB = 0
         GO TO 20012
C@#####################################################################
C######################################################################
C######################################################################
C    PˆHŸT8
C    CTPOK“               M E T K A   # 5
C  KOMEHTAPˆEB            ----------------
C######################################################################
C######################################################################
5        CONTINUE
         IF ( KAR(IK) .NE. LKON ) GO TO 10
C====> KOMEHTAPˆˆ KOH—ˆ‹ˆC8
         IPRT = 0
         FLAG5 = 1
         GO TO 10
C######################################################################
45100    CONTINUE
         FLAGPP = 1
         FLAG5 = 0
         IPRT = 0
         GO TO 6
C######################################################################
C   OPAOTAT8
C     KOHE–                 M E T K A  #  6
C  ‹Oƒ. OˆCAHˆŸ [;]        ---------------
C######################################################################
C######################################################################
6        CONTINUE
C===>Pˆ‹›‹ˆ..... “PA....
C====> EC‹ˆ CˆT“A–ˆŸ <...;  ;> TO  EE PO“CTˆT8
         IF ( FLAG5 .EQ. 1 ) IPRT = 0
         IF ( FLAG5 .EQ. 1 ) GO TO 10
         IF (FLAG1 .EQ. 1 .AND. FLAG2 .EQ. 1 .AND. FLAG3 .EQ. 1 .AND.
     =     FLAGPP  .EQ. 1 )  GO TO 60
C====>  ‡AˆC8 KOH—ˆ‹AC8, HO HE BCE PEKBˆ‡ˆT› BBE„EH›....
         IF ( MBCE .EQ. 1 ) WRITE(NWTR, 62)
62       FORMAT(' *** B ‹Oƒˆ—ECKO‰ CTPOKE, PE„˜ECTB“ž™E‰ CTPOKE  "#BCE"
     = ***'/ ' *** OTME—EH› O˜ˆKˆ, COO™EHˆŸ O KOTOP›X E—ATAETCŸ Hˆ†E
     =***' )
         IPRT = 0
C===> HO OˆC. BCE PABHO KOH—ˆ‹OC8
         FLAG5 = 1
         IER = IER + 1
         ER(IK) = LGOPA
C$$$$$$$>IGNORE = 1
         I = MSG(1)
         MSG(I+2) = 9
         MSG(1) = I + 1
         GO TO 10
60       CONTINUE
C====>  CMEHA “KA‡ATE‹E‰....
         IBA0 = IBA0 + MASPRT(4)
         IOKNOT = IKNOT
         IOPAR0 = IPAR0
         IOBA0 = IBA0
         MASPRT(3) = MASPRT(3) + 1
         SUMEL = SUMEL + 1
         IPRT = 0
         FLAG1 = 0
         FLAG2 = 0
         FLAG3 = 0
         FLAG4 = 0
         FLAG6 = 0
         FLAGPP = 0
C====> OTMETˆM, —TO O—EPE„HOE OˆCAHˆE KOH—ˆ‹OC8
         FLAG5 = 1
         PRT90 = 0
         IF ( MBCE .EQ. 1 ) GO TO 9997
         GO TO 10
C######################################################################
C######################################################################
C  HEO†ˆ„AHH›‰
C    KOHE–             M E T K A   # 9999
C  ”A‰‹A NRDR00        ------------------
C######################################################################
C######################################################################
9999     CONTINUE
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
         IF ( NRDR00 .EQ. NRDR ) GO TO 80000
C---> ˆH”OPM. HA NRDR1 KOH—ˆ‹AC8, #BCE HE›‹O.....
C-->  MAKETˆP“EM #BCE ˆ EPEK‹. HA NRDR
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
       IF ( NRDR00 .EQ. -1 ) GO TO 80002
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
         WRITE(NWTR,80001) NRDR1
80001    FORMAT(' *** KOHE– ”A‰‹A HA KAHA‹E ', I2, ' "#BCE MAKETˆP“ETCŸ
     =***')
         NRDR00 = NRDR
         GO TO 9997
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
80002  CONTINUE
       WRITE(NWTR, 80003)  (DSNAME(I88), I88=1,11)
80003  FORMAT(' *** KOHE– „AHH›X HA DSN=', 11A4, ' ***'/
     =        ' *** "#BCE"  MAKETˆP“ETCŸ, BBO„ EPEK‹ž—AETCŸ HA',
     = ' CˆCTEMH›‰ "NRDR" ***')
       NRDR00 = NRDR
       GO TO 9997
CPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP
80000    CONTINUE
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
         EOFRDR = 1
         MASPRT(3) = M3
         MASPRT(7) = M7
         BA0(3) = IBA01
         KNOT(3) = IKNOT1
         PAR0(3) = IPAR01
         IRC = 3
         WRITE(NWTR,776)  NRDR
776      FORMAT(' *** KOHE– BBO„ˆMO‰ ˆH”OPMA–ˆˆ HA KAHA‹E BBO„A # ',
     =           I3, ' ***'/' *** BBO„ˆMOE OˆCAHˆE CPAC›BAETCŸ ***'/
     =  ' *** PAOTA PEP›BAETCŸ [ INPUT0 ] ***'//)
         IF ( DIAL .EQ. 1 )  WRITE(NWTR,9095)
         IF ( DIAL .EQ. 0 )  WRITE(NWTR,9094)
         RETURN
C######################################################################
C######################################################################
C   OPAOTKA
C   „ˆPEKTˆB›     M E T K A  # 9981
C   #MEX          -----------------
C######################################################################
C######################################################################
9981     CONTINUE
         IRC = 4
         IF( SUMEL .EQ. 0 ) RETURN
         GO TO 9997
C######################################################################
C######################################################################
C   OPAOTKA
C   „ˆPEKTˆB›     M E T K A  # 9998
C   #CPOC        -----------------
C######################################################################
C######################################################################
9998     CONTINUE
         IRC = 2
         WRITE(NWTR,775)
775    FORMAT(' *** PˆHŸTA „ˆPEKTˆBA "#CPOC", OˆCAHˆE CPO˜EHO ***')
         MASPRT(3) = M3
         MASPRT(7) = M7
         BA0(3) = IBA01
         KNOT(3) = IKNOT1
         PAR0(3) = IPAR01
         IF ( DIAL .EQ. 1 )  WRITE(NWTR,9095)
         IF ( DIAL .EQ. 0 )  WRITE(NWTR,9094)
         RETURN
C######################################################################
C######################################################################
C   OPAOTKA
C   „ˆPEKTˆB›       M E T K A  # 9997
C    #BCE           -----------------
C######################################################################
C######################################################################
9997     CONTINUE
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
C-- EPEK‹. HA CˆCTEM. BBO„
         NRDR00 = NRDR
CTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
         MBCE = 1
C====> ‡AKOH—EHO ‹ˆ O—EPE„HOE OˆCAHˆE ?
         IF ( FLAG5 .EQ. 1 ) GO TO 9800
C===>  MO†E ›T8 M› ˆƒHOPˆP“EM ???
         IF ( IGNORE .EQ. 1 ) GO TO 6
C===> CKA†EM, ˆ CMO„E‹ˆP“EM
         WRITE( NWTR, 9801 )
9801     FORMAT(' *** HET ";" EPE„ OƒPAHˆ—ˆTE‹EM "#BCE" ***'/
     =          ' *** TEPMˆHATOP ";" - BCTAB‹ŸETCŸ ABTOMATˆ—ECKˆ ***')
         GO TO 6
9800     CONTINUE
         WRITE ( NWTR, 9802 ) SUMKAR, SUMEL, SUMER
9802  FORMAT(' *** ‡ ª®­ç¥­ ¢¢®¤ ®¯¨á ­¨ï í«¥ªâà¨ç¥áª®© æ¥¯¨   ***'/
     =      ' *** ¯à¨­ïâ®: ', I10, ' ä¨§¨ç¥áª¨å áâà®ª;  ***'/
     =      ' ***    ', I10, ' «®£¨ç¥áª¨å áâà®ª ®¯¨á ­¨ï; ***'/
     =      ' *** ®â¬¥ç¥­® ', I10, ' ®è¨¡®ª ;   ***'/)
C===>  ƒ‹OA‹8HAŸ —ˆCTKA
         IPRT = 0
         PRT90 = 0
         FLAG1 = 0
         FLAG2 = 0
         FLAG3 = 0
         FLAG4 = 0
         FLAG5 = 0
         FLAG6 = 0
         MBCE = 0
C********SUMER = 0
C********SUMEL = 0
C********SUMKAR = 0
C===> A M› —TO TO BBE‹ˆ ˆ‹ˆ HET?
         IF ( SUMEL .EQ. 0 ) GO TO 9803
C==>  —TO TO BBE‹ˆ...
C===****>  ˆTƒOB›E POBEPKˆ...
C====> POBEPKA HA‹ˆ—ˆŸ  0 - ƒO “‡‹A ( ˆH„ˆBˆ„“A‹8HO )
         IM = IKNOT - 1
C===> KM1  -  MˆHˆMA‹8H›‰,  KM2 - MAKCˆMA‹8H›‰
         KM1 = 1
         KM2 = 0
         DO 9996 I = 6, IM
         IF ( KNOT(I) .GT. KM2 ) KM2 = KNOT(I)
         IF ( KNOT(I) .LT. KM1 ) KM1 = KNOT(I)
9996     CONTINUE
         WRITE(NWTR,9090) KM1, KM2, MASPRT(3)
9090     FORMAT(/' *** M¨­¨¬ «ì­ë© ­®¬¥à ã§«   ', I5, ' ***'/
     =           ' *** M ªá¨¬ «ì­ë© ­®¬¥à ã§«  ', I5, ' ***'/
     =           ' *** K®«¨ç¥áâ¢® í«¥¬¥­â®¢    ', I5, ' ***'/)
         IF ( KM1 .EQ. 0 ) GO TO 9995
C====> HET H“‹EBOƒO “‡‹A.....
         WRITE ( NWTR, 9994 )
9994     FORMAT(/' *** B OˆCAHˆˆ CXEM› HET 0-ƒO “‡‹A ***'/)
9803     CONTINUE
         IF (DIAL .EQ. 0 ) WRITE(NWTR, 773)
         IF (DIAL. EQ. 0 ) GO TO 9998
773      FORMAT(' *** PE„O‹OƒAETCŸ B›„A—A „ˆPEKTˆB› "#CPOC" ***')
C====>   ‡APOCˆM BBO„ HOB›X ‹EMEHTOB ˆ‹ˆ #CPOCA.....
         WRITE(NWTR, 774)
774      FORMAT(' **** PO„O‹†A‰TE BBO„ ˆ‹ˆ BBE„ˆTE "#CPOC" ***')
         IPRT = 0
         GO TO 10
9995     CONTINUE
C====> POBEPKA "‹OTHOCTˆ" HOMEPOB “‡‹OB [ ˆ„“T ‹ˆ HOMEPA O„PŸ„ ]
         IXX0 = 0
         IXX = 1
9990     DO 9993 I=6,IM
         IF ( KNOT(I) .EQ. IXX ) GO TO 9992
9993     CONTINUE
C===> “‡‹A IXX HET ******
         IXX0 = 1
         WRITE(NWTR, 9991) IXX
9991     FORMAT(' *** B OˆCAHˆˆ HET “‡‹A HOMEP ', I10, ' ***')
9992     IXX = IXX + 1
         IF (IXX .LE. KM2 ) GO TO 9990
C===> ›‹A ‹ˆ ”ˆKCA–ˆŸ OTC“TCTBˆŸ HOMEPA
         IF ( IXX0 .EQ. 0 ) GO TO 9810
C===>   „A, EC‹ˆ MO†HO, OPO“EM ˆCPABˆT8...
         IF ( DIAL .EQ. 0 ) WRITE ( NWTR, 773 )
         IF ( DIAL .EQ. 0 ) GO TO 9998
C---> ‡APOCˆM...DIAL = 1 - „ˆA‹Oƒ ˆHˆ–ˆˆPOBAH
         WRITE(NWTR, 9989)
9989    FORMAT(' *** BBO„ˆTE HOB›E ‹EMEHT› ˆ‹ˆ „ˆPEKTˆB› #CPOC/#BCE **
     =*')
         GO TO 10
9810     CONTINUE
C====>  MO†HO ‡AKAH—ˆBAT8 POƒPAMM“
C---->  ‡AˆC8 B MASPRT:
         MASPRT(7) = KM2
C--->  KOPEKTˆPOBAT8 „ˆCKPˆTOP›:
         BA0(3) = IBA0
         KNOT(3) = IKNOT
         PAR0(3) = IPAR0
C--->
         SUMKAR = 0
         SUMEL = 0
         SUMER = 0
         IF ( DIAL .EQ. 1 )  WRITE(NWTR,9095)
         IF ( DIAL .EQ. 0 )  WRITE(NWTR,9094)
9095     FORMAT(/1X,79('*')/ ' *** ', 17X,
     =      'B¢®¤ ¤ ­­ëå ¤¨à¥ªâ¨¢ "#*–Eœ" ®ª®­ç¥­ ', 17X, '***'/
     =           1X, 79('*')/)
9094     FORMAT(/20X,    80('*')/  20X, '***   ', 17X,
     =      'B¢®¤ ¤ ­­ëå ¤¨à¥ªâ¨¢ "#*–Eœ" ®ª®­ç¥­ ',  17X, '***'/
     =   20X,        80('*')/)
         RETURN
C######################################################################
C######################################################################
C  OPAOTKA
C  HEXBATKˆ          M E T K ˆ #  777, 778, 779
C  PA‡MEPHOCTE‰      --------------------------
C  MACCˆBOB
C######################################################################
C######################################################################
777      CONTINUE
C!!!-------------------------------------------------------------------
         I1=IBA0
         I2=BA0(2)
         WRITE(NWTRE,707) I2, I1
707      FORMAT(' *** ‹Oƒ. CO‰ B INPUT0: MACCˆB BA0 - MA‹EHœKˆ‰ ***'/
     =          ' *** B›„E‹EHO ', I10, '  ‡APOC “†E HA ', I12, ' ***')
C!!!-------------------------------------------------------------------
         GO TO 706
C======================================================================
778      CONTINUE
C!!!-------------------------------------------------------------------
         I1=IKNOT
         I2=KNOT(2)
         WRITE(NWTRE,708) I2, I1
708      FORMAT(' *** ‹Oƒ. CO‰ B INPUT0: MACCˆB KNOT - MA‹EHœKˆ‰ ***'/
     =          ' *** B›„E‹EHO ', I10, '  ‡APOC “†E HA ', I12, ' ***')
C!!!-------------------------------------------------------------------
         GO TO 706
779      CONTINUE
C!!!-------------------------------------------------------------------
         I1=IPAR0
         I2=PAR0(2)
         WRITE(NWTRE,709) I2, I1
709      FORMAT(' *** ‹Oƒ. CO‰ B INPUT0: MACCˆB PAR0 - MA‹EHœKˆ‰ ***'/
     =          ' *** B›„E‹EHO ', I10, '  ‡APOC “†E HA ', I12, ' ***')
C!!!-------------------------------------------------------------------
         GO TO 706
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
706      CONTINUE
C!!--------------------------------------------------------------------
        WRITE(NWTRE, 705 ) MASPRT(3), SUMKAR, SUMEL
705    FORMAT(' *** BCEƒO B HA‹ˆ—ˆˆ ', I10, ' OˆCAHˆ‰ ‹EMEHTOB ***'/
     =        ' *** BBE„EHO ', I10, ' ”ˆ‡ˆ—ECKˆX CTPOK ***'/
     =        ' *** BBE„EHO ', I10, ' ‹Oƒˆ—ECKˆX CTPOK ***')
         I1 = IBA0
         I2 = IKNOT
         I3 = IPAR0
         II1 = BA0(2) - I1
         II2 = KNOT(2) - I2
         II3 = PAR0(2) - I3
         WRITE(NWTRE, 717) I1, II1, I2, II2, I3, II3
717   FORMAT(' *** B "BA0"  ==> ‡AHŸTO ', I10, '  CBOO„HO ', I10,';'/
     =       ' *** B "KNOT" ==> ‡AHŸTO ', I10, '  CBOO„HO ', I10,';'/
     =       ' *** B "PAR0" ==> ‡AHŸTO ', I10, '  CBOO„HO ', I10,';')
C!!!-------------------------------------------------------------------
1112       call STOPP(666)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
