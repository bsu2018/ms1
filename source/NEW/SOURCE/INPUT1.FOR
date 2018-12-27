         SUBROUTINE INPUT1( MSWALL, VT, UT, IT, IRC )
CXXX----KOM‹EKC "MS1" -->
C
C      O„POƒPAMMA: INPUT1( MSWALL, VT, UT, IT, IRC )
C     --------------
C
C      HA‡HA—EHˆE:   BBO„ ˆCXO„HO‰ ˆH”OPMA–ˆˆ "‡APOC HA B›BO„",
C      ----------    ‡AO‹HEHˆE MACCˆBOB VT, UT, IT;
C
C                    *** OPAOT—ˆK „AHH›X „ˆPEKTˆB #TAB, #E—, #ƒPA”,
C                                                              #ƒPA”1;
C                    ”OPMAT ‹Oƒˆ—ECKO‰ CTPOKˆ OˆCAHˆŸ:
C                            ( BBO„ E‡ ”OPMATH›‰ )
C
C                  "V" K1 K2 ... "U" K1  K2  ...
C                   "I" N1 N2 ... N5"."S1 .... N6"."S2 ....
C                  "GAMMA" N1 N2 ... "OMEGA" N1 N2 .....
C                   "MM" N1 N2 ... "MBH" N1 N2 .......";"
C                   "Cˆ“"  N1  N2 .... ;
C
C                  (“KB› B " " KO„ˆP“žTCŸ KAK OKA‡AHO)
C
C
C                  N1 N2 ...- HOMEPA ‹EMEHTOB  B –Eˆ;
C
C                  K1 K2 ... - HOMEPA “‡‹OB O„K‹ž—EHˆŸ;
C
C                  S1 S2 ... - "CME™EHˆE" HOMEPA EPEMEHHO‰ B
C                               MACCˆBE TOKN ˆ TOKN1 „‹Ÿ “KA‡AHHOƒO
C                               ‹EMEHTA;
C
C                     B O † M O † H O :
C
C                  A) ‡A„ATœ HE O‹EE IRC-Tˆ BE‹E—ˆH „‹Ÿ B›BO„A O
C                     „ˆPEKTˆBAM #E—, "ƒPA”, #TA   ˆ  HE O‹EE 2-X -
C                     O „EPEKTˆBE #ƒPA”1;
C
C     METO„:       CˆHTAKCˆ—ECKˆ‰ ˆ CEMAHTˆ—ECKˆ‰  KOHTPO‹œ BBO„ˆM›X
C     -----        „AHH›X, POBEPKA  COOTBETCTBˆŸ ˆH”OPMA–OHH›X
C                  OˆCAHˆ‰ „O“CTˆM›M.
C                  ”OPMˆPOBAHˆE ˆ B›„A—A ˆH”OPMA–ˆOHH›X COO™EHˆ‰;
C
C    APAMETP›:   C‹“†EH›E ‹OKA‹œH›E MACCˆB›:
C    ---------    VT - HOMEPA “‡‹OB ( 1 + 1*9 = 10 );
C                 UT - HOMEPA AP “‡‹OB ( 1 + 2*9 = 19 );
C                 IT - HOMEPA "‹EMEHT"_"CME™EHˆE" ( 1 + 2*9 = 19 );
C                  *** EPB›‰ ‹EMEHT TˆX MACCˆBOB - KO‹-BO ‡AHŸT›X ***
C
C                  Pˆ BXO„E:
C                  IRC = MAX. KO‹ˆ—ECTBO „AHH›X B ‡APOCE ( OT 1 „O 9 );
C                  Pˆ B›XO„E:
C                 IRC = 1 --> O˜ˆOK HET;
C                     = 2 --> —ACTœ OˆCAHˆŸ ˆƒHOPˆPOBA‹OCœ ˆ‡-‡A
C                             O˜ˆOK;
C                     = 3 --> KOHE– „AHH›X HA NRDR;
C
C                  OCTA‹œH›E APAMETP› B›ˆPAžTCŸ ˆ‡ O™ˆX ‹OKOB
C                  PROF1, PROF2, PROF3;
C
C    O„POƒPAMM›: 1. HAOP O„POƒPAMM C PE”ˆKCOM APF...;
C    ------------  2. COMMON: PROF1, PROF2, PROF3;
C                  3. MSGOUT, SEQMS0, SEQNT0, APFTRT, APFSQI;
C
C   05.04.89 --> E—ATœ Pˆ DIAL=1 !!!
C   PˆME—AHˆE:1.Pˆ O˜ˆKE B OˆCAHˆˆ - ˆƒHOP. —ACTœ ‹Oƒ. O„ CTPOKˆ;
C   ---------- 2.Pˆ PAOTE B ˆTEPAKTˆBHOM PE†ˆME - POˆ‡BO„ˆTCŸ ‡APOC
C                HA OBTOP O˜ˆO—HO‰ ‹Oƒ. CTPOKˆ;
C              2A. ˆCP. APFSQ. - BBO„ “CT. (MUDATSKAQ PROGAMMA INPUT0)
C              ****¬ © 1990 £.¯à®¢¥àª  ­  ¤®¯. §­ ç¥­¨ï ¯® ¬ áá¨¢ ¬
C                  ¨§ /SERVZ8/ ¯à¨ ­¥ ­ã«¥¢®¬ §­ ç¥­¨¨ ä« £ 
C                  MASPRT(121) <==> FTRSPL
C
c
C              3.PACO‹O†EHˆE ˆH”OPMA–ˆˆ B MACCˆBAX:
C
C
C                            VT ˆ UT
C                            -------
C +------------+------------------------.....
C ! KO‹ˆ—ECTBO !  HOMEP   ! HOMEP      !.....
C !ˆCO‹œ‡“EM›X! “‡‹A     !  “‡‹A      !.....
C ! C‹OB       !          !            !.....
C +------------+------------------------.....
C !     1      !    1     !   2        !.....
C !------------!----------!------------!.....
C
C                             IT
C                             ---
C    +---------+---------------------------+--------------------+...
C    ! KO‹-BO  !  HOMEP ‹EMEHTA           !  ‡HA—EHˆE          !...
C    ! ˆCO‹.  !                           ! "CME™EHˆŸ"         !...
C    ! C‹OB    !                           !                    !...
C    +---------+---------------------------+--------------------!...
C    !    1    !             2             !          3         !...
C    +---------+---------------------------+----------+---------!...
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
         DIMENSION VT(1), UT(1), IT(1)
         INTEGER VT, UT, FLAGX, DIAL, EOFRDR
         EQUIVALENCE ( NWTR,  MASPRT(13) ),
     =               ( NWTRE, MASPRT(14) ),
     =               ( NRDR,  MASPRT(12) ),
     =               ( DIAL,  MASPRT(15) ),
     =               ( NMKNOT, MASPRT(7) ),
     =               ( NZRDR, MASPRT(127) ),
     =               ( FTRSPL, MASPRT(121))
         INTEGER FKAR, SUM, PRT90, ER, BLANK, POINT, CIU
         DIMENSION KAR(80), FKAR(80), ER(80), LG(5), LO(5),
     =             LMFT(3), LMBH(3), MSG(25), CIU(3)
         DIMENSION MSWALL(1)
         INTEGER SUMER, TIP
         INTEGER FTRSPL, SPISNN, SPISVL, SPISTK
C---->
         DATA K9/9/, K72/72/, K2/2/
         DATA LV/'V'/, LU/'U'/, LI/'I'/, CIU/'C','ˆ','“'/
         DATA  LG/'G', 'A', 'M', 'M', 'A'/
         DATA  LO/'O', 'M', 'E', 'G', 'A'/
         DATA  LMFT/ 'M', '', 'M' /
         DATA  LMBH/ 'M', 'B', 'H' /
         DATA LGOPA/'$'/, LKON/';'/, BLANK/' '/, POINT/'.'/
         DATA EOFRDR/0/
         SUMER = 0
         KK_19 = IRC*2+1
C----> HA—A‹œH›E “CTAHOBKˆ
         DO 10001 I = 1,KK_19
         UT(I) = 0
         IT(I) = 0
         IF ( I .GT. IRC+1 ) GO TO 10001
         VT(I) = 0
10001    CONTINUE
C---> C.—.K. *****
         SUM = 0
         KSUM = IRC
C        IF ( KSUM .GT. K9 ) KSUM = K9
         IF ( KSUM .LE. 0 ) KSUM = K2
         DO 10000 I = 1,80
10000    ER(I) = BLANK
         MSG(1) =0
         IPRT = 0
         IPRTX = 0
         IGNORE = 0
         IK = K72 + 1
         IER = 0
         PRT90 = 0
         IRC = 1
C--->
C######################################################################
C######################################################################
C
C  B ‡ Ÿ T œ
C  O—EPE„HO‰            METKA  10
C  CˆMBO‹               ---------
C
C######################################################################
C######################################################################
10       CONTINUE
         IF ( KAR(IK) .EQ. LKON ) GO TO 9998
         IF ( IGNORE .EQ. 1 ) IPRT = 0
         IK = IK + 1
         IF ( IK .LE. K72 ) GO TO 1001
C----> EC‹ˆ ›‹ˆ O˜ˆKˆ  --> CKA†EM O TOM
         IF ( IER .EQ. 0 ) GO TO 1002
C=======> ƒ‹OA‹œH›‰ C.—.K. O˜ˆOK
         SUMER = SUMER + IER
C---> E—ATœ O˜ˆOK
         IF ( DIAL .EQ. 1 ) WRITE ( NWTR, 1003 ) (ER(I), I=1,80)
1003     FORMAT(80A1/' *** O„CˆCOK  -  ˆƒHOPˆP“ETCŸ ***')
         IF ( DIAL .EQ. 0 ) WRITE (NWTR, 1004 ) IER, (ER(I),I=1,80)
1004     FORMAT(1X,'*** O˜ˆOK ', I2, ' ***=>', 80A1/
     * ' *** O„CˆCOK  -  ˆƒHOPˆP“ETCŸ ***')
         CALL MSGOUT( MSG )
         IRC = 2
         IF ( DIAL .EQ. 1 ) IRC = 1
         DO 1005 I=1,80
1005     ER(I) = BLANK
         IER = 0
1002     CONTINUE
C-------- BBO„ CTPOKˆ -------------------------------------------------
         IF ( EOFRDR .EQ. 1 ) GO TO 9999
         READ( NRDR, 1006, END=9999) (KAR(I), I=1,80)
         NZRDR = NZRDR + 1
C--->
1006     FORMAT(80A1)
         IF (DIAL .EQ. 0 ) WRITE( NWTR, 1007) (KAR(I), I=1,80)
         IF (DIAL .EQ. 1 ) WRITE( NWTR, 10070) (KAR(I), I=1,80)
1007     FORMAT(20X, 80A1)
10070    FORMAT(     80A1)
C???????????????????????????????????????????????????????????????????????
         CALL APFCAS ( 1, 80,KAR, KAR )
C???????????????????????????????????????????????????????????????????????
C===>  "APA‹E‹œH›‰" MACCˆB FKAR B F-KO„:
         CALL APF TRT( 1, K72, KAR, FKAR )
         IK = 1
C----------------------------------------------------------------------
1001     CONTINUE
C---> HE ‡AKOH—ˆTœ ‹ˆ?
         IF ( KAR(IK) .EQ. LKON ) GO TO 9998
C---> “PAœ PO.
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
C--->  HE MHOƒO ‹ˆ ?
         IF ( SUM .GT. KSUM ) GO TO 10
C---> HE POBEPKA ‹ˆ POCTO
         IF ( PRT90 .EQ. 1 ) GO TO 1008
C--->  B ‹OKˆ ‡APOCOB....
         GO TO (1, 2, 3,   4,     5,    6,   7,  8   ), IPRT
C--->           V  U  I  GAMMA  OMEGA  MFT  MBH  Cˆ“
C   # C ˆ T O #
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
1008     CONTINUE
         IPRTX = 0
         IF ( KAR(IK) .EQ. LV ) IPRTX = 1
         IF ( KAR(IK) .EQ. LU ) IPRTX = 2
         IF ( KAR(IK) .EQ. LI ) IPRTX = 3
         IF ( KAR(IK) .EQ. LG(1) .AND. KAR(IK+1) .EQ. LG(2)  .AND.
     =      KAR(IK+2) .EQ. LG(3) .AND. KAR(IK+3) .EQ. LG(4)  .AND.
     =      KAR(IK+4) .EQ. LG(5) ) IPRTX=4
         IF ( KAR(IK) .EQ. LO(1) .AND. KAR(IK+1) .EQ. LO(2)  .AND.
     =      KAR(IK+2) .EQ. LO(3) .AND. KAR(IK+3) .EQ. LO(4)  .AND.
     =      KAR(IK+4) .EQ. LO(5) ) IPRTX=5
         IF ( KAR(IK) .EQ. LMFT(1) .AND. KAR(IK+1) .EQ. LMFT(2)  .AND.
     =      KAR(IK+2) .EQ. LMFT(3) ) IPRTX = 6
         IF ( KAR(IK) .EQ. LMBH(1) .AND. KAR(IK+1) .EQ. LMBH(2)  .AND.
     =      KAR(IK+2) .EQ. LMBH(3) ) IPRTX = 7
         IF ( KAR(IK) .EQ. CIU (1) .AND. KAR(IK+1) .EQ. CIU (2)  .AND.
     =      KAR(IK+2) .EQ. CIU (3) ) IPRTX = 8
C
         IF ( PRT90 .EQ. 1  .AND.  IPRTX .EQ. 0 ) GO TO 9010
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF ( IPRTX .EQ. 0 ) GO TO 1010
C---> CˆTO HA˜‹O.., CMECTˆMCŸ ‡A <ˆMŸ>-1 CˆMBO‹....
         IGNORE = 0
         IPRT = IPRTX
         PRT90 = 0
         IF ( IPRT .EQ. 1 ) IK = IK + 0
         IF ( IPRT .EQ. 2 ) IK = IK + 0
         IF ( IPRT .EQ. 3 ) IK = IK + 0
         IF ( IPRT .EQ. 4 ) IK = IK + 4
         IF ( IPRT .EQ. 5 ) IK = IK + 4
         IF ( IPRT .EQ. 6 ) IK = IK + 2
         IF ( IPRT .EQ. 7 ) IK = IK + 2
         IF ( IPRT .EQ. 8 ) IK = IK + 2
         GO TO 10
1010     CONTINUE
C--->  O˜ˆKA, EC‹ˆ HE ˆƒHOPˆP“EM...
         IF ( IGNORE .EQ. 1 ) GO TO 10
C--->  #O˜ˆKA#
         IGNORE = 1
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 21
         MSG(1) = I+1
         GO TO 10
C**********************************************************************
C######################################################################
C######################################################################
C   BBECTˆ
C   –E‹OE       M E T K A   90
C   —ˆC‹O       --------------
C######################################################################
C######################################################################
90       CONTINUE
         PRT90 = 1
         IF ( KAR(IK) .EQ. BLANK ) GO TO 10
         IF ( KAR(IK) .EQ. LKON ) GO TO  9998
C--->  POBEPˆM HET ‹ˆ C‹E„. OˆCATE‹Ÿ...
         GO TO 1008
C--->  BEPHEMCŸ, EC‹ˆ HET....
9010     CONTINUE
         PRT90 = 0
         N2 = K72 - IK + 1
         N2OLD = N2
         CALL APF SQI( FKAR, N0, IK, N2, IFI )
         GO TO ( 9001, 9002, 9003, 9004, 9005, 9006 ), IFI
9004     CONTINUE
9005     CONTINUE
C!!!-------------------------------------------------------------------
         WRITE( NWTRE, 9007 ) IPRT, IFI
9007     FORMAT(' *** ‹Oƒ. CO‰ B INPUT1: IPRT=', I12,' IFI=',I10)
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
9006     CONTINUE
C===> TO‹œKO ‡HAK ˆ‹ˆ O„Hˆ POE‹›, OPOCˆM HOB“ž CTPOK“
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
C===>  EC‹ˆ N2 =(N2OLD-1)==> TO CPA‡“ BCTETˆ‹CŸ HE CˆMBO‹, —ˆCE‹ HE›‹O!
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
         IF ( N2 .EQ. (N2OLD-1) ) GO TO 9009
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C----> BBE‹ˆ, BCTPETˆ‹ˆ HE –ˆ”P“ => IK HA TOM CˆMBO‹E OCTABˆM
C----> „O“CTˆM OC‹E –ˆ”P› TO‹œKO POE‹ , ";"  ˆ‹ˆ "."
       IF ( KAR(IK) .EQ. BLANK .OR. KAR(IK) .EQ. LKON ) GO TO 9001
       IF ( KAR(IK) .EQ. POINT .AND. IPRT .EQ. 3 ) GO TO 9001
C===> OKOH—AHˆE OC‹E –ˆ”P› O˜ˆO—HOE, BCTAHEM HA HEƒO....
9009     IK = IK - 1
         IGNORE = 1
         ER(IK+1) = LGOPA
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I + 1
         IER = IER + 1
         GO TO 10
9001     CONTINUE
C=====> BCE O"KEY   BEPHEMCŸ B ‹OK ‡APOCA
         GO TO ( 100,  200,  300,  400,  500,  600,  700, 800 ), IPRT
C---????
         GO TO 9004
C######################################################################
C######################################################################
1        CONTINUE
C--->  "V" BBO„ˆM...
         IGNORE = 0
         GO TO 90
100      CONTINUE
C--> „O“CTˆM ‹ˆ HOMEP “‡‹A ?
         IF ( N0 .GE. 0  .AND.  N0 .LE. NMKNOT ) GO TO 101
C-->  HOMEP O˜ˆO—EH...
103      IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 23
         MSG(1) = I + 1
         GO TO 10
C--->
C-->  HOMEP O˜ˆO—EH...­¥â ¢  #SPOOL
66661    IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 34
         MSG(1) = I + 1
         GO TO 10
C--->
101      CONTINUE
         IF ( FTRSPL .EQ. 0 ) GO TO 66663
C    ¯à®¢¥à¨¬ ­  ¤®¯ãáâ¨¬®áâì ­®¬¥à®¢
         DO 66660 i66660=1,SPISVL(1)
            IF ( N0 .EQ. SPISVL(i66660+1) ) GO TO 66663
66660    CONTINUE
          GO TO 66661
C#############
66663  CONTINUE
C--> “‡E‹ HA˜, ECTœ K“„A PˆHŸTœ ?
         SUM = SUM + 1
         IF ( SUM .LE. KSUM ) GO TO 102
C--->  “†E MHOƒO —ˆCE‹ B ‡APOCE
104      IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 22
         MSG(1) = I + 1
         GO TO 10
102      CONTINUE
C---> PˆHŸTœ “‡E‹
         KVT = VT(1) + 1
         VT(KVT+1) = N0
         VT(1) = KVT
         GO TO 10
C######################################################################
2        CONTINUE
C--->  "U"
         IGNORE = 0
         IUU = 0
         GO TO 90
200      CONTINUE
         IF ( IUU .EQ. 1 ) GO TO 201
C--> B‡Ÿ‹ˆ EPB›‰ “‡E‹..
         N0U1 =  N0
         IF ( N0U1 .GE. 0  .AND.  N0U1 .LE. NMKNOT ) GO TO 202
C-->  “‡E‹ O˜ˆO—EH
         GO TO 103
202      CONTINUE
C#############
         IF ( FTRSPL .EQ. 0 ) GO TO 66664
C    ¯à®¢¥à¨¬ ­  ¤®¯ãáâ¨¬®áâì ­®¬¥à®¢
         DO 66662 i66660=1,SPISVL(1)
            IF ( N0U1 .EQ. SPISVL(i66660+1) ) GO TO 66664
66662    CONTINUE
          GO TO 66661
66664     CONTINUE
C-->  H“†EH „P. “‡E‹...
         IUU = 1
         GO TO 90
201      CONTINUE
C-->  C‹E„. “‡E‹ B‡Ÿ‹ˆ
         IUU = 0
         IF ( N0 .GE. 0  .AND.  N0 .LE. NMKNOT ) GO TO 203
         GO TO 103
203      CONTINUE
         IF ( FTRSPL .EQ. 0 ) GO TO 66665
C    ¯à®¢¥à¨¬ ­  ¤®¯ãáâ¨¬®áâì ­®¬¥à®¢
         DO 66666 i66660=1,SPISVL(1)
            IF ( N0 .EQ. SPISVL(i66660+1) ) GO TO 66665
66666    CONTINUE
          GO TO 66661
66665     CONTINUE
C-->  “‡‹› B HA‹ˆ—ˆˆ
         KUT = UT(1) + 1
         SUM = SUM + 1
         IF ( SUM .GT. KSUM ) GO TO 104
         UT(KUT+1) = N0U1
         UT(KUT+2) = N0
         UT(1) = KUT + 1
         GO TO 10
C######################################################################
3        CONTINUE
C--->  CˆCOK "I"
C---> NSM0 - CME™EHˆE
         NSM0 = 1
         IGNORE = 0
         II= 0
         GO TO 90
300      CONTINUE
         IF ( II .EQ. 1 ) GO TO 301
         NEL = N0
C--->  ECTœ ‹ˆ TAKO‰ ‹EMEHT ?
         CALL SEQMS0( MSWALL, NEL, IMSW, IR0, NSM, ITOK )
         GO TO( 302, 303 ), IR0
303      CONTINUE
C--->  HOMEPA HET...
         IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 24
         MSG(1) = I + 1
         GO TO 10
302      CONTINUE
C-->   HOMEP ‹-TA HA˜, BBO„ˆTCŸ ‹ˆ CME™EHˆE?
         IF ( KAR(IK) .NE. POINT ) GO TO 304
C--> BBO„ˆM "CME™EHˆE"
         IK = IK + 1
         II = 1
         GO TO 90
C--->
301      CONTINUE
         II = 0
         NSM0 = N0
304      CONTINUE
C-->  „O“CTˆMO ‹ˆ CME™EHˆE....
         IF ( NSM0 .GE. 1  .AND.  NSM0  .LE. NSM ) GO TO 305
C---> CME™EHˆE O˜ˆO—HOE
         IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 25
         MSG(1) = I + 1
         GO TO 10
C--->
305      CONTINUE
C--->  PˆHŸTœ NEL  ˆ  NSM0
         IF ( FTRSPL .EQ. 0 ) GO TO 66613
C    ¯à®¢¥à¨¬ ­  ¤®¯ãáâ¨¬®áâì ­®¬¥à®¢ #SPOOL
C   NEL + NSM0
C --> A„PECA...
C---> POCMOTP MSWALL...
         CALL SEQMS0( MSWALL, NEL, IMSW66, IR660, NSM66, ITOK66 )
         GO TO (66610,66611), IR660
         CALL SUBERR(IR660)
66611       CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE,66612) IR660, NEL
66612       FORMAT(' *** ‹Oƒ. CO‰ B INPUT1 "Žˆ‘Š ‚ MSWALL": IR660=',
     =   I5, ' NEL = ', I6, ' ***' )
C!!!--------------------------------- ---------------------------------
         CALL SUBERR(66623)
66610       CONTINUE
         TIP = MSWALL(IMSW+1)
         iii666 = ITOK66 + ( NSM0-1 )
         DO 66614 i66660=1,SPISTK(1)
            IF ( iii666 .EQ. SPISTK(i66660+1) ) GO TO 66613
66614    CONTINUE
C         GO TO 66604
C---> Ž˜ˆŠ€ …’ ‚ ‘ˆ‘Š… #SPOOL
66604    IK = IK - 1
         IGNORE = 1
         IER = IER + 1
         ER(IK) = LGOPA
         I = MSG(1)
         MSG(I+2) = 35
         MSG(1) = I + 1
         GO TO 10
66613     CONTINUE
         KIT = IT(1) + 1
         SUM = SUM + 1
         IF ( SUM .GT. KSUM ) GO TO 104
         IT(KIT+1) = NEL
         IT(KIT+2) = NSM0
         IT(1) = KIT + 1
         GO TO 10
C######################################################################
C######################################################################
4        CONTINUE
C-->  CˆCOK "GAMMA", ˆMŸ EPEMEHHO‰ C KO„OM #2#  HA„O HA‰Tˆ...
         KODSM = 2
410      IGNORE = 0
         GO TO 90
400      CONTINUE
411      NEL = N0
C--->  ECTœ ‹ˆ TOT ‹EMEHT?
         CALL SEQMS0(MSWALL, NEL, IMSW, IR0, NSM,  ITOK )
         GO TO ( 401, 303 ), IR0
         CALL SUBERR(IR0)
401      CONTINUE
         TIP = MSWALL(IMSW+1)
C--> O HOMEP“ TˆA HA‰Tˆ "CME™EHˆE" C KO„OM  ˆMEHˆ "KODSM"
         CALL SEQ NT0( TIP, KODSM, NSM0, IR0 )
         GO TO ( 402, 403 ), IR0
         CALL  SUBERR(IR0)
403      CONTINUE
C--->  ‹EMEHT HE CO„EP†ˆT TAKOƒO "CME™EHˆŸ"
         IGNORE = 1
         IK = IK - 1
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 26
         MSG(1) = I + 1
         GO TO 10
C--->
402      CONTINUE
C--> NSM0 - ˆCKOMOE "CME™EHˆE", NEL - HOMEP ‹EMEHTA
         GO TO 305
C######################################################################
C######################################################################
5        CONTINUE
C--->  CˆCOK "OMEGA"
         KODSM = 3
         GO TO 410
500      CONTINUE
         KODSM = 3
         GO TO 411
C######################################################################
6        CONTINUE
C--->  CˆCOK "MM",  KO„ ˆMEHˆ EPEMEHHO‰ = 4
         KODSM = 4
         GO TO 410
600      CONTINUE
         KODSM = 4
         GO TO 411
C######################################################################
7        CONTINUE
C-- CˆCOK "MBH",  KO„ ˆMEHˆ EPEMEHHO‰ 5
         KODSM = 5
         GO TO 410
700      CONTINUE
         KODSM = 5
         GO TO 411
C######################################################################
8        CONTINUE
C--->  CˆCOK "Cˆ“",  KO„ EPEMEHHO‰ 6
         KODSM = 6
         GO TO 410
800      CONTINUE
         KODSM = 6
         GO TO 411
C######################################################################
C######################################################################
C######################################################################
C  HEO†ˆ„AHH›‰
C    KOHE–             M E T K A   # 9999
C  ”A‰‹A NRDR          ------------------
C######################################################################
C######################################################################
9999     CONTINUE
         EOFRDR = 1
         IRC = 3
         WRITE(NWTR,776)  NRDR
776      FORMAT(' *** KOHE– BBO„ˆMO‰ ˆH”OPMA–ˆˆ HA KAHA‹E BBO„A # ',
     =           I3, ' ***'///)
         RETURN
C######################################################################
C######################################################################
9998     CONTINUE
C--->  ";"  OHAP“†EH...
         IF ( IER .EQ. 0 ) GO TO 803
         SUMER = SUMER + IER
C-->  CKA‡ATœ O O˜ˆKAX
C---> E—ATœ O˜ˆOK
         IF ( DIAL .EQ. 1 ) WRITE ( NWTR, 1003 ) (ER(I), I=1,80)
C
         IF ( DIAL .EQ. 0 ) WRITE (NWTR, 1004 ) IER, (ER(I),I=1,80)
         CALL MSGOUT( MSG )
         IRC = 2
803      CONTINUE
         IF ( SUMER .EQ. 0 ) RETURN
         IF ( DIAL .EQ. 0 ) RETURN
         I = KSUM - SUM
         IER = 0
         IF ( I .LE. 0 ) RETURN
         WRITE(NWTR, 801) I
801      FORMAT(' *** BBE„ˆTE ";" ˆ‹ˆ OˆCAHˆŸ „‹Ÿ ', I2, ' B›BO„ˆM›X BE
     =‹ˆ—ˆH ***' )
         SUMER = 0
         DO 802  I= 1,80
802      ER(I) = BLANK
         IRC = 1
         IGNORE = 0
         MSG(1) = 0
         IPRT = 0
         PRT90 = 0
         IER = 0
         IK = K72 + 1
         GO TO 10
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
