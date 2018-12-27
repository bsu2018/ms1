         SUBROUTINE APF GET ( ICAR, RCAR, IER, NRDR, NWTR, DIALOG)
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C      O„POƒPAMMA:  APF GET ( ICAR, RCAR, IER, NRDR, NWTR, DIALOG)
C      ------------
C      HA‡HA—EHˆE:    BBO„ TEKCTOBO‰ ˆH”OPMA–ˆˆ C KAHA‹A NRDR,
C      ----------     PA‡ˆBKA BBE„EHHO‰ ˆH”OPMA–ˆˆ HA ‹Oƒˆ—ECKˆE
C                     TEPM›. Pˆ KA†„OM OPA™EHˆˆ B›„E‹ŸETCŸ O„ˆH
C                     TEPM. B C‹“—AE ƒP“›X O˜ˆOK - E—ATœ COO™EHˆ‰
C                     HE NWTR, B ”OPMATE ‡ABˆCŸ™EM OT ‡HA—EHˆŸ DIALOG.
C                     B TOM C‹“—AE IER = 1. Pˆ „OCTˆ†EHˆˆ KOH–A
C                     ”A‰‹A HA NRDR - IER = 2.
C      METO„:         O‹EMEHTHAŸ "O„KA—KA" TEPMOB.
C      -----
C      APAMETP›:    ICAR - O—EPE„HO‰ BBE„EHH›‰ C NRDR TEPM (INTEGER*4);
C      ---------     RCAR - O—EPE„HO‰ BBE„EHH›‰ C NRDR TEPM (REAL*4);
C                    IER  - = 0  -> “CE˜HOE B›„E‹EHˆE TEPMA;
C                           = 1  -> ›‹ˆ ƒP“›E O˜ˆKˆ, —ACTœ BBO„ˆM›X
C                                   CTPOK ˆƒHOPˆPOBA‹OCœ;
C                           = 2  -> HEO†ˆ„AHH›‰ KOHE– ”A‰‹A HA NRDR;
C                     NRDR, NWTR - KAHA‹› BBO„A/B›BO„A;
C                     DIALOG - = 0 -> AKETH›‰ PE†ˆM;
C                              = 1 -> ˆTEPAKTˆBH›‰ PE†ˆM;
C     O„POƒPAMM›: APFTEM, APFTRT, APFSQE, APFPAK;
C     ------------
C     PˆME—AHˆE:  1. PABˆ‹O PA‡ˆEHˆŸ HA TEPM› CM. B APFTEM;
C     ----------   2. Pˆ DIALOG = 1 ---> IER BCEƒ„A = 0;
C                  3. BO‡MO†HO OPA™EHˆE: CALL APFGET(ICAR, ICAR, ...
CZZZ-------------------------------------------------------------------
         EQUIVALENCE ( RSIM(1), SIM(1) )
         INTEGER DIALOG, ER, SIM, FLAG, TEKS, EORDR, BLANK
         DIMENSION KAR(80), SIM(80), RSIM(80), ER(80)
         DATA FLAG/0/, EORDR/0/, BLANK/' '/
         IER = 0
         IF ( EORDR .EQ. 1 ) GO TO 999
         IF ( FLAG  .EQ. 1 ) GO TO 100
C
C===> –ˆK‹ —TEHˆŸ ˆ B›„E‹EHˆŸ TEPMOB
C
10       DO 1 I=1,80
1        KAR(I)=BLANK
         READ(NRDR,600, END=999) (KAR(I),I=1,80)
600      FORMAT(80A1)
         IF ( DIALOG .EQ. 0 ) WRITE(NWTR,601) (KAR(I),I=1,80)
601      FORMAT( 20X,80A1 )
C===== EPE‰„EM B F-KO„:
         CALL APF TEM(KAR, MAXS, SIM, RSIM, ER, IX )
         IF ( IX .EQ. 0 ) GO TO 11
C==== ƒP“›E O˜ˆKˆ
         IF ( DIALOG .EQ. 0 ) WRITE(NWTR,602) IX, (ER(I),I=1,80)
         IF ( DIALOG .EQ. 1 ) WRITE(NWTR,603) IX, (ER(I),I=1,80)
602      FORMAT(1X,'*** O˜ˆOK ', I3, ' ***=>',80A1)
603      FORMAT(80A1/' <== ˆCPABœTE O˜ˆKˆ B CTPOKE ==> ')
         IF ( DIALOG .EQ. 1 ) GO TO 10
         IER = 1
         GO TO 10
C===> O˜ˆOK HET, “CTAHOBˆM „ˆAA‡OH CKAHˆPOBAHˆŸ
11       FLAG = 1
         TEKS = 1
         GO TO 100
C
C====> TEK“™ˆ‰ BXO„
C
100      CONTINUE
C====  TEPM „OCT“EH? ˆ‹ˆ HEOXO„ˆMO —ˆTATœ HOB“ž PO–ˆž...
         IF ( TEKS .GT. MAXS ) GO TO 10
         ICAR = SIM(TEKS)
         RCAR = RSIM(TEKS)
         TEKS = TEKS + 1
         RETURN
C
C===== HEO†ˆ„AHH›‰ KOHE– ”A‰‹A HA NRDR
C
999      EORDR = 1
         IER = 2
         RETURN
         END
