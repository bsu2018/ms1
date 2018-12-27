       SUBROUTINE READMK(KAR,IK,IKMAX,NRDRX,DSNAME,KFIZ,KODEOF)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 2.0
C   O„„POƒPAMMA: READMK(KAR,IK,IKMAX,NRDRX,DSNAME,KFIZ,KODEOF)
C   -------------
C   A‡A—EˆE:    BBO„ CTPOKˆ „A›X „ˆPEKTˆB› #X–Eœ —EPE‡
C   ---------      KAA‹ NRDRX B ”OPMATE 80A1, ˆCO‹œ‡“Ÿ Pˆ TOM
C                  "MAKPOƒEEPATOP" BBO„A *** BEPCˆŸ 0.0 ****
C                 TEKCT "MAKPOPAC˜ˆPEˆ‰" —ˆTAETCŸ —EPE‡ KAA‹
C                 NMAK [ MASPRT(89) ]
C
C                CTP“KT“PA ”A‰‹A NMAK:
C                --------------------
C
C              XXX_ZZZ_TT...TT
C->C 3-‰ O‡.  ___....TEKCT O„CTAOBKˆ ...(„O 70 O‡. )
C              ___    ----//----
C                ...........
C-> B KO–E:   000_000
C
C    ƒ„E:
C              XXX - KO‹-BO CTPOK C TEKCTOM O„CTAOBKˆ
C              ZZZ - „‹ˆA ˆMEˆ TTT..TT "MAKPOKOMA„›" --->
C                    B „A›X „‹Ÿ "#X–Eœ" TO ˆMŸ KO„ˆP“ETCŸ
C                    A—ˆAŸ C CˆMBO‹A "M" ( MAKCIM), O
C                    B ”A‰‹E NMAK CˆMBO‹ "M" E KO„ˆP“ETCŸ
C
C   === „O“CKAETCŸ ˆCO‹œ‡OBATœ TAK-†E CˆMBO‹ "‡"  ===
C
C    "M" --->  MO„E‹œ,            "‡" --->  ‡A—EˆŸ;
C
C
C                !! ”OPMAT 1-CTPOKˆ KA†„OƒO "MAKPO_TE‹A" ”ˆKCˆPOBA !!!!
C
C              -->  B KO–E ”A‰‹A -->  "000_000";
C              ‡„ECœ "_" -->  OO‡A—AET POE‹;
C
C              B CTPOKAX "MAKPO_O„CTAOBOK" -> ”OPMAT CBOO„›‰,
C                COƒ‹ACO CˆTAKCˆC“ „A›X „ˆPEKTˆB› "#–Eœ"
C                ( CM. /  INPUT0 );
C
C   METO„:  ( BEPCˆŸ 0.0 ) --> PŸMOE KOˆPOBAˆE;
C   -----
C@
C   APAMETP›:   KAR - MACCˆB BBE„EO‰ ˆ”OPMA–ˆˆ ( E O‹EE 80 );
C   ---------    IK, IKMAX - ƒPAˆ–› "O‹E‡O‰" ˆ”OPMA–ˆˆ;
C                NRDRX - KAA‹ BBO„A;
C                DSNAME - MACCˆB C DIM *(11) - ˆMŸ ”A‰‹A BBO„A
C                         „‹Ÿ POƒPAMM CE–. OC‹“†ˆBAˆŸ;
C                KODEOF = 0  -  CTPOKA BBE„EA;
C                       = 1  -  CˆT“A–ˆŸ END OF FILE A NRDRX;
C                KFIZ = 1  - "AKT" BBO„A ”ˆ‡-KO‰ CTPOKˆ COBEP˜E;
C                     = 0  - ”ˆ‡-K“ž CTPOK“ E BBO„ˆ‹ˆ;
C
C
C   O„POƒAMM›:  1. PROF1
C   -----------   2. SEQ0, MOVE0
C   PˆME—AˆE:   1. Pˆ O˜ˆKE CTP“KT“P› ”A‰‹A NMAK, ˆ‹ˆ Pˆ EƒO
C   ----------       COE --> COO™EˆE ˆ “CTAOBKA KODEOF = 1;
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION   KAR(*), DSNAME(1), KARX(80)
       EQUIVALENCE ( NWTR, MASPRT(13) )
       EQUIVALENCE ( NMAK, MASPRT(89) )
       EQUIVALENCE ( NZRDR, MASPRT(127) )
       EQUIVALENCE ( NRDR, MASPRT(12) )
C
       DIMENSION NAME(72)
       INTEGER BLANK, TERM
       DATA KOD/0/, IFLAG/1/, MAKCIM/'M'/,K72/72/, K3/3/, K70/70/
       DATA MAKCIZ / '‡' /, TERM / ';' /
       DATA BLANK/' '/, K80/80/, IPR/'%'/,IPL/'+'/, KODM/0/
C-->  IFLAG -->  OMEP BXO„A....
       KODEOF = 0
       KFIZ = 0
       GO TO ( 1,2), IFLAG
       CALL SUBERR(IFLAG)
C-->  1-‰ BXO„..
1      CONTINUE
       IKX = 1
C$$$$$$IKMAXX = K80
       IKMAXX = K72
       IF ( KOD .EQ. 1 ) GO TO 9999
       READ( NRDRX, 500, END=9999) (KARX(I), I=1,K80 )
       IF ( NRDRX .EQ. NRDR ) NZRDR = NZRDR + 1
500    FORMAT(80A1)
       KFIZ = 1
23     CONTINUE
C-->  ˆCKATœ CˆMBO‹ "MAKPO" -->  MAKCIM  ˆ  MAKCIZ ....
       DO 12 I=IKX, IKMAXX
       IKMAK = I
       IF ( KARX(I) .EQ. MAKCIM ) GO TO 13
       IF ( KARX(I) .EQ. MAKCIZ ) GO TO 13
12     CONTINUE
C-->  E A˜‹A, OT„ATœ CTPOK“...
       GO TO 50
13     CONTINUE
C-->  OXO†E A—A‹O..
C-->   LKMAK -> „‹ˆA ‡A "&", C„Bˆƒ ‡A "&"
       IKMAX  = IKMAK  - 1
       IKMAK = IKMAK + 1
       IF ( IKMAK .GT. IKMAXX ) GO TO 50
       LKMAK = 0
       DO 14 I=IKMAK, IKMAXX
       IF ( KARX(I) .EQ. BLANK ) GO TO 15
       IF ( KARX(I) .EQ. TERM  ) GO TO 15
       LKMAK = LKMAK + 1
14     CONTINUE
CC--
15     CONTINUE
C-->  IKMAK --> A—A‹O PAC˜ˆPEˆŸ B KARX, LKMAK -->  EƒO „‹ˆA...
       IF ( LKMAK .LE. 0 ) GO TO 50
C              ********
C--> —ˆTAEM NMAK...
10     CONTINUE
       REWIND NMAK
11     CONTINUE
       IF ( KODM .EQ. 1 ) GO TO 9998
       READ(NMAK,501, END=9998) NSTR, LNAME,(NAME(I), I=1,K72 )
501    FORMAT(I3, 1X, I3, 1X, 72A1 )
C--> E KO—ˆ‹, ‹ˆ O..
       IF ( NSTR .EQ. 0 ) GO TO 50
C-->  A˜ ‹ˆ POTOTˆ ?
       IF ( LNAME .NE. LKMAK ) GO TO 17
       CALL SEQ0( KARX(IKMAK), NAME, LNAME, ISEQ0 )
       GO TO (16, 17 ), ISEQ0
       CALL SUBERR(ISEQ0)
17     CONTINUE
C  -->  PAC˜ˆPEE E A˜‹ˆ, PO“CK  NSTR  CTPOK...
       IF ( KODM .EQ. 1 ) GO TO 9998
       DO 18 I=1,NSTR
       READ ( NMAK, 502, END=9998 )
502    FORMAT(80A1)
18     CONTINUE
C-->  A OB›‰ OˆCK..
       GO TO 11
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
16     CONTINUE
C-->  POTOTˆ OO‡A...
       IKBEG = IKMAK
       IKEND = (IKBEG + LNAME )
C-->  “KA‡ATE‹ˆ "OKA" POCMOTPA CTPOKˆ „‹Ÿ INPUT0..
       IK = IKX
C-->  IKMAX -->  A CˆMBO‹  EPE„ "&"
       DO 19 I=1, K80
19     KAR(I) = BLANK
C-->  EPEC‹ATœ OT IK „O IKEND
       III=(IKEND - IK) + 1
       CALL MOVE0(KAR(IK), KARX(IK), III)
       IFLAG = 2
       RETURN
C**********************************************************************
50     CONTINUE
C-->  ˆ —EƒO E A˜‹ˆ....OT IKX „O IKMAXX -> EPEC‹ATœ ˆ OT„ATœ..
       IK = IKX
       IKMAX = IKMAXX
C$$$$$$III=(IKMAX-IK) + 1
       III=(K80  -  IK) + 1
       DO 20 I= 1, K80
20     KAR(I) = BLANK
       CALL MOVE0( KAR(IK), KARX(IK), III )
       IFLAG = 1
       RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
2      CONTINUE
C-->  BTPO‰ BXO„..
       DO 21 I=1,K80
21     KAR(I) = BLANK
       IF ( NSTR . GT. 0 ) GO TO 22
C-->  BCE KO—ˆ‹OCœ, E„EM „A‹œ˜E...
       IFLAG = 1
       IKX = IKEND
C$$$$$$IKMAXX =  K80
       IKMAXX =  K72
       GO TO 23
C----------------------------------------------------------------------
22     CONTINUE
C-->  PAC˜ˆPŸEM..
       IF ( KODM .EQ. 1 ) GO TO 9998
       READ(NMAK, 5020, END=9998 ) ( KAR(I), I=K3, K70 )
5020   FORMAT(3X,70A1)
C----------------------------------------------------------------------
       KAR(1) = IPL
       KAR(2) = IPR
       KAR(71) = IPR
       KAR(72) = IPL
       IK = K3
       IKMAX = K70
       IFLAG = 2
       NSTR = NSTR - 1
C      $$$$$$$$$$$$$$$$
       RETURN
C######################################################################
C######################################################################
C######################################################################
9999   CONTINUE
       KODEOF = 1
       KOD = 1
       RETURN
C######################################################################
9998   CONTINUE
C--> C NMAK APŸ†EKA...
       WRITE(NWTR, 505 ) NMAK
505    FORMAT(/' *** Câàãªâãà  ä ©«  ¤«ï "#X–Eœ" (ª ­ « # ',I2,') - ',
     =  '®è¨¡®ç­  ***'/)
       GO TO 9999
       END
