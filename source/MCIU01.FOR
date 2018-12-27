       INTEGER FUNCTION MCIU01( TN, H0, HX, TMAS )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA_”“HK–ˆŸ MCIU01( TN, H0, HX, TMAS )
C   ---------------------
C   HA‡HA—EHˆE:        "ƒEHEPATOP" ˆ "BEPˆ”ˆKATOP" ˆM“‹œCOB “PAB‹EHˆŸ
C  -----------         Cˆ“ „‹Ÿ MO„E‹ˆ “PAB. BEHTˆ‹œ ˆ ˆ„EA‹œH›‰ K‹ž—.
C                         BAPˆAHT # 01:
C                         -------------
C
C
C
C                         I<-HX->I        :<-HX->:
C                         $      $        :      $
C                       **********        :      ******
C                       **********        :      ******
C                       **********        :      ******
C  T0*--------I---------**********-----I--$------******----------------
C    I        I         I        I     I         I
C    I<-TZAD->I         I<-TIMP->I     I         I
C    I        I<-TALFA->I        I     I<-TALFA->I
C             I                        I
C             I<---------TCIU--------->I<-----------TCIU--------
C
C  B OˆCAHˆˆ –Eˆ ˆCO‹œ‡“ETCŸ C‹E„. ‡AˆCœ:
C
C   ... MAS < 1 TZAD  TCIU  TALFA  TIMP  TKCIU  DTALFA >
C
C  BO‡MO†HO KO„ˆPOBAHˆE:
C
C   ... MAS < 0 >, —TO COOTBETCTB“ET:
C
C       TZAD = 360 ‹. ƒPA„.
C       TCIU = 60  ‹. ƒPA„.
C       TAFLA= 30  ‹. ƒPA„.
C       TIMP = 10  ‹. ƒPA„.
C       TKCIU=  0  ‹. ƒPA„.
C       DTALFA= 0  ‹. ƒPA„.
C
C      ‡„ECœ ˆCO‹œ‡OBAH› C‹E„. OO‡HA—EHˆŸ:
C
C
C      TZAD  - BPEMŸ ‡A„EP†Kˆ BK‹ž—EHˆŸ "Cˆ“"    ( ‹. ƒPA„. )
C      TCIU  - EPˆO„ OBTOPEHˆŸ CˆƒHA‹OB "Cˆ“"  ( ‹. ƒPA„. )
C      TALFA - ‡A„EP†KA EPE„HEƒO ”POHTA CˆƒHA‹A
C              "Cˆ“" OT HA—A‹A O—EPE„HOƒO EPˆO„A
C              OBTOPEHˆŸ                          ( ‹. ƒPA„. )
C      TIMP - „‹ˆTE‹œHOCTœ ˆM“‹œCA "Cˆ“"          ( ‹. ƒPA„. )
C      TKCIU - —EPE‡ CKO‹œKO EPˆO„OB OBTOPEHˆŸ
C              CˆƒHA‹A "Cˆ“" ˆ‡MEHˆTœ TALFA HA
C      DTALFA - E„ˆHˆ– - ˜Aƒ ˆ‡MEHEHˆŸ TALFA       ( ‹. ƒPA„. )
C
C              - B C‹“—AE, KOƒ„A TEK“™EE ‡HA—EHˆE TALFA - "‡A„EP†KA
C                EPE„HEƒO ”POHTA Cˆ“" CTAHET O‹œ˜E TCIU - "EPˆO„A
C                OBTOPEHˆŸ Cˆ“" ( —TO BO‡MO†HO Pˆ TKCIU ˆ DTALFA
C                HE PABH›Mˆ H“‹œ ), POBO„ˆTCŸ #CTAˆ‹ˆ‡A–ˆŸ#
C                                               ------------
C                ˆM“‹œCA "Cˆ“":
C                ---------------
C                                            ******
C                                            ******
C                                            ******
C                                            ******
C   *----------------------*-----------------******--------------------
C   I(TGO)                 I(TGO+TCIU)       I
C   I                      I                 I
C   I                      I                 I
C   I<-------( TGO + TALFA<TEK“™EE> )------->I
C   I                      I                 I
C------------<HOBOE TZAD>->I                 I
C                          I<-<HOBOE TALFA>->I
C                          I     PABHOE      I
C                              <‡A„AHHOE>
C                              < TALFA  >
C
C
C              TGO - BPEMŸ HA—A‹A O—EPE„HOƒO EPˆO„A Cˆ“, „‹Ÿ KOTOPOƒO
C                    ‡A”ˆKCˆPOBAHA CˆT“A–ˆŸ TALFA<TEK“™EE> >= TCIU
C              TALFA = TALFA<‡A„AHHOE> + [ NP/KCIU ] * DTALFA -
C                   TEK“™EE ‡HA—EHˆE "‡A„AP†KA EPE„HEƒO ”POHTA";
C              NP = [ (TN-TZAD) / TCIU ] - HOMEP EPˆO„A "Cˆ“"
C                   ( Pˆ NP < 0, NP = 0 );
C
C              #CTAˆ‹ˆ‡A–ˆŸ# ‡AK‹ž—AET B C‹E„“ž™EM:
C
C               TZAD <HOBOE>    = TGO + TCIU
C
C               TALFA < HOBOE > = TALFA <‡A„AHHOE>
C
C               NP < HOBOE >    = 0 ( CPAC›BAETCŸ).
C
C               #ƒEHEPˆP“ETCŸ COO™EHˆE HA NWTRE KAHA‹....#
C
C
C
C
C   METO„:     "BOCTAHOB‹EHˆE" ”“HK–ˆˆ "Cˆ“"(T) ˆ KOHTPO‹œ
C   -----      HA‹ˆ—ˆŸ "Cˆ“" Pˆ T = TN;
C   APAMETP›: TN - BPEMŸ OPE„E‹EHˆŸ HA‹ˆ—ˆŸ ˆM“‹œCA "Cˆ“";
C   ---------  H0 - "OKPECHOCTœ" OPE„E‹EHˆŸ Cˆ“
C              HX - "˜Aƒ" OT TN „O ”POHTA ˆM“‹œCA "Cˆ“"(CM. PˆC.)
C
C
C              TMAS - "OTPE‡OK" MACCˆBA MSWALL - OˆCAHˆE "Cˆ“":
C
C  +------+------+-------+------+-------+--------+
C  I TZAD I TCIU I TALFA I TIMP I TKCIU I DTALFA I
C  +------+------+-------+------+-------+--------+
C  I  1   I  2   I   3   I  4   I   5   I    6   I
C  +------+------+-------+------+-------+--------+
C
C  ‡HA—EHˆE ”“HK–ˆˆ:  MCIU01 = 00  --> "Cˆ“" HET B [TN, TN+H0];
C  ****************                     HX = 0.0;
C                     MCIU01 = 01  --> B [TN, TN+H0] -> EPE„Hˆ‰ ”POHT
C                                      ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹ OT
C                                      TN „O EPE„HEƒO ”POHTA "Cˆ“";
C                     MCIU01 = 10 -->  B [TN, TN+H0] -> ‡A„Hˆ‰ ”POHT
C                                      ˆM“‹œCA "Cˆ“", HX = ˆHTEPBA‹
C                                      OT TN „O ‡A„HEƒO ”POHTA ˆM“‹œCA;
C                     MCIU01 = 11 -->  B [TN, TN+H0] "Cˆ“" BK‹ž—EH,
C                                      COCTOŸHˆE HE MEHŸ‹, HX=0.0;
C
C                !!!!!  Pˆ H0 > TIMP --->  BO†MO†HA OPEPŸ "Cˆ“" !!!!!
C                      PE„O‹OƒAETCŸ, —TO H0 < TIMP
C
C
C   O„POƒPAMM›:      PROF1
C   -------------
C
C   PˆME—AHˆE:        1. KOHTPO‹œ HE POBO„ˆTCŸ;
C  -----------         2. ˆM“‹œC ECTœ, EC‹ˆ:
C                         .... <= TN <= .....
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       DIMENSION TMAS(*)
100     CONTINUE
       MCIU01 = 0
         HX = 0.0
C-->  BOCTAHOBˆM ˆM“‹œC....
C  NP - HOMEP EPˆO„A "Cˆ“", TGO -  BPEMŸ HA—A‹A EPˆO„A "Cˆ“",
C        !!! BOCTAHOB‹EHˆE O TN !!!
C
         TZAD = TMAS(1)
         TCIU = TMAS(2)
         TALFA = TMAS(3)
         TIMP = TMAS(4)
         KCIU = IFIX( TMAS(5) )
         DTALFA = TMAS(6)
C---->
         RNP = 0.0
         NP = 0
         T1 = TN - TZAD
         IF ( T1 .LT. 0.0 ) GO TO 1
         RNP = T1/ TCIU
         NP = INT( RNP )
         RNP = FLOAT( NP )
1        CONTINUE
C-->  BPEMŸ HA—A‹A TEK. "Cˆ“"
         TGO = TZAD + RNP * TCIU
C-->  CME™EHˆE BH“TPˆ EPˆO„A "Cˆ“" ..
         IF ( KCIU .EQ. 0  .OR.  DTALFA .EQ. 0.0 ) GO TO 2
C-->  TALFA ˆ‡MEHˆ‹ACœ...
         IALFA = NP / KCIU
         TALFA = TALFA + FLOAT(IALFA) * DTALFA
C----------------------------------------------------------------------
C-->  HE H“†HO ‹ˆ CTAˆ‹ˆ‡ˆPOBATœ "Cˆ“" ?
        IF ( TALFA .LT. TCIU ) GO TO 2
C--->  CTAˆ‹ˆ‡A–ˆŸ...
        TMAS(1) = TGO + TCIU
C---->
        WRITE(NWTRE, 3 ) TN, TGO, TMAS(1)
3      FORMAT(' *** Pˆ TN=', G12.5, ' B›O‹HEHA "CTAˆ‹ˆ‡A–ˆŸ"',
     =        ' CˆƒHA‹A "Cˆ“" ***'/
     =        ' *** TGO = ', G12.5, ', HOBOE BPEMŸ TZAD = ', G12.5,
     =        ' ***' )
       GO TO 100
2        CONTINUE
C--->  TI1 - EPE„Hˆ‰ ”POHT, TI2 - ‡A„Hˆ‰ ”POHT...
         TI1 = TGO + TALFA
         TI2 = TI1 + TIMP
C--->  KAKO‰ ˆHTEPBA‹...
C  M1 - ‡HA—EHˆE "Cˆ“" „‹Ÿ TN,
C  M2 - ‡HA—EHˆE "Cˆ“" „‹Ÿ TN + H0;
         M1 = 0
         M2 = 0
         TN1 = TN + H0
         IF ( TI1 .LE. TN  .AND.  TN .LT. TI2  )  M1 = 1
         IF ( TI1 .LE. TN1  .AND.  TN1 .LE. TI2  )  M2 = 1
C--->   M1 = 0 ˆ M2 = 0  -->  ˆM“‹œCA HET...
         IF ( M1 .EQ. 0  .AND.  M2 .EQ. 0 ) RETURN
C--> EC‹M M1 ˆ M2 --> 1,  ˆM“‹œC HE "KOH—ˆ‹"....
         MCIU01 = 11
         IF ( M1 .EQ. 1  .AND.  M2 .EQ. 1 ) RETURN
C-->  „O“CTˆM, —TO M1 = 0,  -->  EPE„Hˆ‰ ”POHT...?
         MCIU01 = 01
         HX = TI1 - TN
C-->  TAK ‹ˆ TO?
         IF ( M1 .EQ. 0 ) RETURN
C-->  HE TAK....
         MCIU01 = 10
         HX = TI2 - TN
         RETURN
         END
