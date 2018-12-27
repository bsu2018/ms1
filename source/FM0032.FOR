       SUBROUTINE FM0032(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.1  (OT    MAŸ    1988 ƒ.)-----------
C   O„POƒPAMMA: FM0032(BA0T, KNOTT, PAR0T, RMSW, LM )
C   -------------
C   HA‡HA—EHˆE:  HACTPO‰KA MAKPOMO„E‹ˆ MˆKPOPO–ECCOPHO‰ CˆCTEM›,
C   ----------   OCTPOEHHO‰ HA A‡E MˆKPOPO–ECCOPA CEPˆˆ
C                K1801BM1(2,3), ˆMEž™EƒO CˆCTEM“ KOMAH„ ”ˆPM› "DEC".
C                ******************************************************
C                Pˆ‡HAK TIP # 5 -> ‹Oƒ. MAKPOMO„E‹œ.
C                ( DM0036,LM0013 )
C                      ”OPMˆP“ET "EPEMEHH“ž" —ACTœ RMSW OˆCAHˆŸ
C
C                 „‹ˆHA  MSW  ==>+56
C----------------------------------------------------------------------
C
C        OPƒAHˆ‡A–ˆŸ AMŸTˆ B PEA‹œHO‰ MˆKPOPO–ECCOPHO‰ CˆCTEME
C        ˆ B MO„E‹ˆP“ž™E‰ EE POƒPAMME (MACCˆB› MEM ˆ MEM1)
C        -------------------------------------------------------
C
C        OPƒAHˆ‡A–ˆŸ AMŸTˆ B PEA‹œHO‰ MˆKPOPO–ECCOPHO‰ CˆCTEME
C
C               C‹OBO
C           A‰T      A‰T                      A‰T
C        *---------+---------*               *---------*
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000000        ! M‹A„˜ˆ‰ ! 000000
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000002        ! CTAP˜ˆ‰ ! 000001
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000004        ! M‹A„˜ˆ‰ ! 000002
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000006        ! CTAP˜ˆ‰ ! 000003
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000010        ! M‹A„˜ˆ‰ ! 000004
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000012        ! CTAP˜ˆ‰ ! 000005
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 000014        ! M‹A„˜ˆ‰ ! 000006
C        ! ................. ! ......        ! CTAP˜ˆ‰ ! 000007
C        ! ................. ! ......        ! ....... ! ......
C        ! ................. ! ......        ! ....... ! ......
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 003774        ! M‹A„˜ˆ‰ ! 003776
C        ! CTAP˜ˆ‰ ! M‹A„˜ˆ‰ ! 003776        ! M‹A„˜ˆ‰ ! 003777
C        *---------+---------*               *---------*
C
C
C        OPƒAHˆ‡A–ˆŸ AMŸTˆ B MO„E‹ˆP“ž™E‰ POƒPAMME
C            (MACCˆB›   MEM  ˆ  MEM1 )
C     *-------------------------------------*
C     ! C‹OBO C A„PECOM 000000              ! 0001
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000000 ! 0002
C     ! C‹OBO C A„PECOM 000002              ! 0003
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000002 ! 0004
C     ! C‹OBO C A„PECOM 000004              ! 0005
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000004 ! 0006
C     ! C‹OBO C A„PECOM 000006              ! 0007
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000006 ! 0008
C     ! C‹OBO C A„PECOM 000010              ! 0009
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 000010 ! 0010
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! ................................... ! ....
C     ! C‹OBO C A„PECOM 003776              ! 2047
C     ! CTAP˜ˆ‰ A‰T C‹OBA C A„PECOM 003776 ! 2048
C     *-------------------------------------*
C
C
C
C
C----------------------------------------------------------------------
C              ‹EMEHT OˆC›BAETCŸ:
C
C          # T 380 “ 0   #‹ #CM
C                          FTAKT  ADRPPTI  ADRPRTO ADRPROG  P  ;
C
C         ƒ„E
C          #‹ #CM- HOMEP ‹EMEHTA ˆ HOMEP TOKA ,KO-P›‰ O„K‹ž—AET
C                   K MˆKPOPO–ECCOP“ ,KAK BXO„HO‰ CˆƒHA‹  ;
C          FTAKT  - TAKTOBAŸ —ACTOTA MˆKPOPO–ECCOPHO‰ CˆCTEM› ;
C          ADRPRTI- A„PEC OPTA BBO„A  B BOCœMˆˆ—HOM Bˆ„E ;
C          ADRPRTO- A„PEC OPTA B›BO„A B BOCœMˆˆ—HOM Bˆ„E ;
C          ADRPROG- A„PEC Ÿ—E‰Kˆ O‡“ (B BOCœMˆˆ—HOM Bˆ„E), „A‹œ˜E
C                   KOTOPOƒO HE PA‡ME™AžTCŸ Hˆ “PAB‹Ÿž™AŸ POƒPAMMA
C                   Hˆ „AHH›E ;
C          P   -    ”‹Aƒ OT‹A„O—HO‰ E—ATˆ:
C                   P=0 - OT‹A„O—HAŸ E—ATœ OTMEHŸETCŸ ,
C                   P=1 - OT‹A„O—HAŸ E—ATœ A‡PE˜AETCŸ - OC‹E
C                         B›O‹HEHˆŸ KA†„OƒO ˜AƒA POƒPAMM› B B›XO„HO‰
C                         HAOP „AHH›X B›BO„ŸTCŸ CO„EP†ˆMOE  POH
C                         CC  ˆ  OPTA BBO„A-B›BO„A C A„PECOM ADRPORT;
C----------------------------------------------------------------------
C
C   METO„:         PA‡METKA O‹E‰ RMSW O„ MO„E‹œ
C   -----
C
C   APAMETP›:     BA0T, KNOTT, PAR0T - "OTPE‡Kˆ" ˆH”OPMA–ˆOHH›X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 ˆ GETMSW );
C                  RMSW - "EPEMEHHAŸ" —ACTœ MSW ( "OTPE‡OK" MSWALL );
C                  LM - „‹ˆHA "EPEMEHHO‰" —ACTˆ RMSW Pˆ FGMSW = 0;
C                  LM - „‹ˆHA BCEƒO MSW OˆCAHˆŸ      Pˆ FGMSW = 1;
C
C   O„POƒPAMM›:  1.COMMON PROF1
C   ------------
C
C   PˆME—AHˆE:    1. KOHTPO‹œ B MˆHˆMA‹œHOM O"EME;
C   -----------    2. BCE HE OPE„E‹ŸEM›E O‹Ÿ RMSW ‡AH“‹ŸžTCŸ;
C                  3. Hˆ†E PˆBO„ŸTCŸ ”OPMAT› RMSW:
C
C
C                   ”OPMAT RMSW Tˆ # 380:
C                  -----------------------
C+-----------I--------------I------I-----------------------------------
C  # C‹OBA   I    # C‹OBA B I KTO  I
C OT HA—A‹A  I    OTPE‡KE   IOPE„.I
C BCEƒO MSW  I    RMSW B    I      I       OO‡HA—EHˆE - ‡HA—EHˆE
C OˆCAHˆŸ   I    FM0032    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP “‡‹A ( BCEƒ„A 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I TX - BPEMŸ BO‡MO†HOƒO EPEK‹ž—EHˆŸ;
C    11      I     2        I  DM  I KX - “KA‡ATE‹œ ˆ‡MEHEHˆŸ COCTOŸHˆŸ
C            I              I      I      OPTA  B/B HA ˜AƒE ;
C    12      I     3        I  FM  I TTAKT - EPˆO„ TAKTOBO‰ —ACTOT› ;
C    13      I     4        I  FM  I P - ”‹Aƒ OT‹A„O—HO‰ E—ATˆ:
C            I              I      I     P=0 - OT‹A„O—HAŸ E—ATœ OTK‹.
C            I              I      I     P=1 - OT‹A„O—HAŸ E—ATœ BK‹.
C    14      I     5        I  FM  I           PE‡EPB
C    15      I     6        I  FM  I           PE‡EPB
C    16      I     7        I  DM  I KX - “KA‡ATE‹œ ˆ‡MEHEHˆŸ COCTOŸHˆŸ
C            I              I      I      OPTA B/B HA ˜AƒE ;
C    17      I     8        I  DM  I TT - TEK“™EE BPEMŸ ;
C    18      I     9        I  DM  I R0 - CO„EP†ˆMOE POH R0 Pˆ T=TT ;
C    19      I    10        I  DM  I R1 - CO„EP†ˆMOE POH R1 Pˆ T=TT ;
C    20      I    11        I  DM  I R2 - CO„EP†ˆMOE POH R2 Pˆ T=TT ;
C    21      I    12        I  DM  I R3 - CO„EP†ˆMOE POH R3 Pˆ T=TT ;
C    22      I    13        I  DM  I R4 - CO„EP†ˆMOE POH R4 Pˆ T=TT ;
C    23      I    14        I  DM  I R5 - CO„EP†ˆMOE POH R5 Pˆ T=TT ;
C    24      I    15        I  DM  I R6 - CO„EP†ˆMOE POH R6 Pˆ T=TT ;
C    25      I    16        I  DM  I R7 - CO„EP†ˆMOE POH R7 Pˆ T=TT ;
C    26      I    17        I  DM  I FLC-‡HA—EHˆE ”‹AƒA "C" CC Pˆ T=TT
C    27      I    18        I  DM  I FLV-‡HA—EHˆE ”‹AƒA "V" CC Pˆ T=TT
C    28      I    19        I  DM  I FLZ-‡HA—EHˆE ”‹AƒA "Z" CC Pˆ T=TT
C    29      I    20        I  DM  I FLN-‡HA—EHˆE ”‹AƒA "N" CC Pˆ T=TT
C    30      I    21        I  DM  I FLT-‡HA—EHˆE ”‹AƒA "T" CC Pˆ T=TT
C    31      I    22        I  DM  I FLIO-‡HA—EHˆE ”‹AƒA "I/O" CC
C            I              I      I             Pˆ T=TT ;
C    32      I    23        I  DM  I IPORT  -CO„EP†ˆMOE OPTA B›BO„A
C    33      I    24        I  DM  I IPORT1 -CO„EP†ˆMOE OPTA BBO„A
C            I              I      I             Pˆ T=TT ;
C    34      I    25        I  FM  I ADRPROG-MAKCˆMA‹œH›‰ A„PEC O‡“ ;
C    35      I    26        I  FM  I ADRPRTO-A„PEC OPTA B›BO„A;
C    36      I    27        I  FM  I ADRPRTI-A„PEC OPTA BBO„A ;
C    37      I    28        I  DM  I TO - CTAPOE BPEMŸ ;
C    38      I    29        I  DM  I R0 - CO„EP†ˆMOE POH R0 Pˆ T=TO ;
C    39      I    30        I  DM  I R1 - CO„EP†ˆMOE POH R1 Pˆ T=TO ;
C    40      I    31        I  DM  I R2 - CO„EP†ˆMOE POH R2 Pˆ T=TO ;
C    41      I    32        I  DM  I R3 - CO„EP†ˆMOE POH R3 Pˆ T=TO ;
C    42      I    33        I  DM  I R4 - CO„EP†ˆMOE POH R4 Pˆ T=TO ;
C    43      I    34        I  DM  I R5 - CO„EP†ˆMOE POH R5 Pˆ T=TO ;
C    44      I    35        I  DM  I R6 - CO„EP†ˆMOE POH R6 Pˆ T=TO ;
C    45      I    36        I  DM  I R7 - CO„EP†ˆMOE POH R7 Pˆ T=TO ;
C    46      I    37        I  DM  I FLC-‡HA—EHˆE ”‹AƒA "C" CC Pˆ T=TO
C    47      I    38        I  DM  I FLV-‡HA—EHˆE ”‹AƒA "V" CC Pˆ T=TO
C    48      I    39        I  DM  I FLZ-‡HA—EHˆE ”‹AƒA "Z" CC Pˆ T=TO
C    49      I    40        I  DM  I FLN-‡HA—EHˆE ”‹AƒA "N" CC Pˆ T=TO
C    50      I    41        I  DM  I FLT-‡HA—EHˆE ”‹AƒA "T" CC Pˆ T=TO
C    51      I    42        I  DM  I FLIO-‡HA—EHˆE ”‹AƒA "I/O" CC
C            I              I      I             Pˆ T=TO ;
C    52      I    43        I  DM  I IPORT  -CO„EP†ˆMOE OPTA B›BO„A
C    53      I    44        I  DM  I IPORT1 -CO„EP†ˆMOE OPTA BBO„A
C            I              I      I             Pˆ T=TO ;
C    54      I    45        I  FM  I  #‹ - HOMEP ‹EMEHTA;
C    55      I    46        I  FM  I  #CM - HOMEP TOKA ‹EMEHTA #‹ ;
C    56      I    47        I  LM  I  ATOKM - A„P B TOKN/N1 ‡HA—EHˆŸ
C            I              I      I           TOKA HOMEPA #CM ;
C----------------------------------------------------------------------
C  O™AŸ „‹ˆHA MSW --> LM<BCŸ> = 56
C
C  „‹ˆHA EPEMEH. —ACTˆ --> LM<RMSW> = 47
C
C   BHˆMAHˆE „OABKˆ*** :HA—A‹H›‰ A„PEC CTEKA B MEM-MACCˆBE 2046
C   ----------
C               „OBABˆ‹ LAN  11/06/89.
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(100)
       COMMON /MEMER/ MEM(2048), MEM1(2048)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE( H0   , MASPRT(30) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW, ADPRTI,ADPRTO, ADPROG
       NEL = BA0T(1)
       TIP = BA0T(2)
       IF (  TIP .EQ. 380 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ‹Oƒ. CO‰ B FM0032 --> HOMEP TˆA HE HA˜ ***'/
     =  ' *** HOMEP ‹EMEHTA ', I5, ' HOMEP TˆA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C----------------------------------------------------------------------
C --->       MˆKPOPO–ECCOPHAŸ CˆCTEMA HA A‡E MˆKPOPO–ECCOPA
C --->       CEPˆˆ K1801BM1(2,3)
C----------------------------------------------------------------------
         IF ( FGMSW.EQ. 0 ) GOTO 11
C--> “TO—HˆTœ LM <O™“ž>
         LM = 56
         RETURN
C----------------------------------------------------------------------
 11      CONTINUE
C-->  PˆHŸTœ ˆ —TO MO†HO C”OPMˆPOBATœ..
       LM = 47
       DO 10 I=1,LM
10     RMSW(I) = 0.0
C----------------------------------------------------------------------
C---> BBE„EM „AHH›E
           N = IFIX ( PAR0T(1) )
        NCM  =  IFIX ( PAR0T(2) )
       FTAKT = PAR0T(3)
       ADPRTI = IFIX ( PAR0T(4) )
       ADPRTO = IFIX ( PAR0T(5) )
       ADPROG = IFIX ( PAR0T(6) )
       P =  PAR0T(7)
C---> POBEPˆM ˆ ‡AOMHˆM „AHH›E.
C
C--> POBEPˆM HOMEP #‹ ..
       IF ( N .GE. NEL ) GO TO 9999
       RMSW( 45) = RN
       N = NCM
       RMSW(46) = RN
C---> POBEPˆM —ACTOT“.
       IF( FTAKT .GT. 0.0 ) GO TO 15
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 551 ) NEL, TIP
551    FORMAT(/' *** O˜ˆKA B FM0032 --> —ACTOTA HE O‹O†ˆTE‹œHA  ***'/
     =         ' *** ‹EMEHT  ',   I5,'      HOMEP TˆA ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
15     CONTINUE
       RMSW(3) = 1./FTAKT
C--> POBEPˆM A„PEC OPTA BBO„A
C--> POBEPˆM PABˆ‹œHOCTœ ‡A„AHˆŸ KA†„OƒO PA‡PŸ„A A„PECA
CCCC   WRITE (6,*) ADPRTI
       CALL ADECQD(ADPRTI, IADD, I0, I1, I2, I3, I4, I5, IER )
       IF( IER .NE. 0 ) GO TO 20
       IF( I0  .LT. 0 .OR. I0 .GT. 7 ) GO TO 20
       IF( I1  .LT. 0 .OR. I1 .GT. 7 ) GO TO 20
       IF( I2  .LT. 0 .OR. I2 .GT. 7 ) GO TO 20
       IF( I3  .LT. 0 .OR. I3 .GT. 7 ) GO TO 20
       IF( I4  .LT. 0 .OR. I4 .GT. 7 ) GO TO 20
       IF( I5  .LT. 0 .OR. I5 .GT. 7 ) GO TO 20
       IF( ADPRTI .LT. 160000 .OR. ADPRTI .GT. 177777 ) GO TO 20
C--> ‡AOMHˆM..
       N = IADD
       RMSW(27) = RN
       GO TO 22
20     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 552 ) NEL, TIP
552    FORMAT(/' *** O˜ˆKA B FM0032 --> HEPABˆ‹œHO ‡A„AH A„PEC  ***'/
     =         ' *** OPTA BBO„A                           ***'/
     =         ' *** ‹EMEHT  ',   I5,'      HOMEP TˆA ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
22     CONTINUE
C--> POBEPˆM A„PEC OPTA B›BO„A
C--> POBEPˆM PABˆ‹œHOCTœ ‡A„AHˆŸ KA†„OƒO PA‡PŸ„A A„PECA
       CALL ADECQD(ADPRTO, IADD, I0, I1, I2, I3, I4, I5, IER )
       IF( IER .NE. 0 ) GO TO 21
       IF( I0  .LT. 0 .OR. I0 .GT. 7 ) GO TO 21
       IF( I1  .LT. 0 .OR. I1 .GT. 7 ) GO TO 21
       IF( I2  .LT. 0 .OR. I2 .GT. 7 ) GO TO 21
       IF( I3  .LT. 0 .OR. I3 .GT. 7 ) GO TO 21
       IF( I4  .LT. 0 .OR. I4 .GT. 7 ) GO TO 21
       IF( I5  .LT. 0 .OR. I5 .GT. 7 ) GO TO 21
       IF( ADPRTO .LT. 160000 .OR. ADPRTO .GT. 177777 ) GO TO 21
C--> ‡AOMHˆM..
       N = IADD
       RMSW(26) = RN
       GO TO 25
21     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 553 ) NEL, TIP
553    FORMAT(/' *** O˜ˆKA B FM0032 --> HEPABˆ‹œHO ‡A„AH A„PEC  ***'/
     =         ' *** OPTA B›BO„A                           ***'/
     =         ' *** ‹EMEHT  ',   I5,'      HOMEP TˆA ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
 25    CONTINUE
C--> POBEPˆM MAKCˆMA‹œH›‰ A„PEC O‡“
C--> POBEPˆM PABˆ‹œHOCTœ ‡A„AHˆŸ KA†„OƒO PA‡PŸ„A A„PECA
       CALL ADECQD(ADPROG, IADPRG, I0, I1, I2, I3, I4, I5, IER )
       IF( IER .NE. 0 ) GO TO 30
       IF( I0  .LT. 0 .OR. I0 .GT. 7 ) GO TO 30
       IF( I1  .LT. 0 .OR. I1 .GT. 7 ) GO TO 30
       IF( I2  .LT. 0 .OR. I2 .GT. 7 ) GO TO 30
       IF( I3  .LT. 0 .OR. I3 .GT. 7 ) GO TO 30
       IF( I4  .LT. 0 .OR. I4 .GT. 7 ) GO TO 30
       IF( I5  .LT. 0 .OR. I5 .GT. 7 ) GO TO 30
       IF( ADPROG .LT. 0 .OR. ADPROG .GT. 3777 ) GO TO 30
C--> ‡AOMHˆM..
       N = IADPRG
       RMSW(25) = RN
C----------------------------------------------------------------------
C---> “CTAHAB‹ˆBAEM HA—A‹. ‡HA—EHˆE “KA‡ATE‹ˆ CTEKA
       RMSW(15) = RN
       GO TO 35
30     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 5522) NEL, TIP
5522   FORMAT(/' *** O˜ˆKA B FM0032 --> HEPABˆ‹œHO ‡A„AH        ***'/
     =         ' *** MAKCˆMA‹œH›‰ A„PEC O‡“                       ***'/
     =         ' *** ‹EMEHT  ',   I5,'      HOMEP TˆA ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
35     CONTINUE
C--> PO„O‹†ˆM..
       N = IFIX(P)
       RMSW(4) = RN
C======================================================================
C--> TEEPœ BBE„EM B O‡“ MˆKPOPO–ECCOPHO‰ CˆCTEM› “PAB‹Ÿž™“ž
C--> POƒPAMM“ B BOCœMˆPˆ—HOM KO„E, A ‡ATEM PEOPA‡“EM EE B
C--> „ECŸTˆ—H“ž ”OPM“.
C======================================================================
C--> OH“‹ˆM
       DO 80 I=1,2048
       MEM1(I) = 0
80     MEM(I)  = 0
C--> NPROG - KAHA‹ BBO„A POƒPAMM›.
       NPROG = 22
C--> BBO„ˆM
C***   READ(NPROG,600,END=100) (MEM1(I),I=1,2048)
       DO 400 I=1,2048
       READ(NPROG,600,END=100) MEM1(I)
600    FORMAT(8X,I6)
C      IF(MEM1(I).EQ.0)  I1=I1+1
C      IF(I1.GT.0) GOTO 100
400    CONTINUE
       I = 2049
100    CONTINUE
       WRITE (6,600) I
       WRITE (6,600) (MEM1(K),K=1,I)
       IK = I - 1
       IF( (2*IK-1) .GT. IADPRG ) GO TO 200
C--> EPEBO„ˆM B „ECŸTˆ—H“ž ”OPM“
       DO 150 I=1,IK
       KQ = MEM1(I)
       CALL ADECQD(KQ, KD, KQ0, KQ1, KQ2, KQ3, KQ4, KQ5, IER)
       IF( IER. NE. 0 ) GO TO 210
       IF( KQ0  .LT. 0 .OR. KQ0 .GT. 7 ) GO TO 210
       IF( KQ1  .LT. 0 .OR. KQ1 .GT. 7 ) GO TO 210
       IF( KQ2  .LT. 0 .OR. KQ2 .GT. 7 ) GO TO 210
       IF( KQ3  .LT. 0 .OR. KQ3 .GT. 7 ) GO TO 210
       IF( KQ4  .LT. 0 .OR. KQ4 .GT. 7 ) GO TO 210
       IF( KQ5  .LT. 0 .OR. KQ5 .GT. 7 ) GO TO 210
C--> ‡AOMHˆM BCE C‹OBO
       MEM(2*I-1) = KD
C--> ‡AOMHˆM CTAP˜ˆ‰ A‰T
       MEM(2*I) = KD/256
150    CONTINUE
CCCC   WRITE (6,*) RMSW(15),RMSW(3)
       RETURN
200    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 554 ) NEL, TIP
554    FORMAT(/' *** O˜ˆKA B FM0032 --> “PAB‹Ÿž™AŸ POƒPAMMA    ***'/
     =         ' *** ‡AHˆMAET O‹ACTœ AMŸTˆ O‹œ˜E ‡A„AHHO‰      ***'/
     =         ' *** ‹EMEHT  ',   I5,'      HOMEP TˆA ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
       RETURN
210    CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 555 ) I, NEL, TIP
555    FORMAT(/' *** O˜ˆKA B FM0032 --> O˜ˆO—HAŸ KOMAH„A B      ***'/
     =         ' *** POƒPAMME „‹Ÿ MˆKPOPO–ECCOPA B CTPOKE',I4,' ***'/
     =         ' *** ‹EMEHT  ',   I5,'      HOMEP TˆA ',   I5,' ***')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C----------------------------------------------------------------------
CCCC   WRITE (6,*) RMSW,MSW
       RETURN
C######################################################################
9999   CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 556 ) NEL, TIP
556    FORMAT(/' *** O˜ˆKA B FM0032 -->HEPABˆ‹œHO ‡A„AH› BXO„H›E „AHH›
     =E   ***'/' *** HA BXO„ ‹EMEHTA ', I5, ' HOMEP TˆA ', I5,' ***',
     =         ' *** O„AETCŸ TOK ‹EMEHTA, ˆMEž™EƒO O‹œ˜ˆ‰ OPŸ„KOB›‰
     =HOMEP   *** ')
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
       RETURN
CCCCC  DEBUG INIT
       END
