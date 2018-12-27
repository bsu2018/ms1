         SUBROUTINE FM0017( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:  FM0017( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HAáHAóEHàE:    HACTPOâKA MAKPOMOÑEãEâ:
C  ------------
C                  Tàè     àMü MOÑEãà     OÅOáHAóEHàü B ÑAãúHEâòEM
C
C                  93    "BEHTàãú,ìèP. Càîì"           TR3
C                 668    "Kãûó,ìèP. Càîì"              KL
C
C                  èPOàáBOÑàT îOPMàPOBAHàE "èEPEMEHHOâ" óACTà MSW
C                  MAKPOMOÑEãà.
C
C   METOÑ:         PAáMETKA èOãEâ RMSW èOÑ MOÑEãú " BEHTàãú(Kãûó) ".
C   -----
C   èAPAMETPõ:     BA0T, KNOTT, PAR0T - "OTPEáKà" àHîOPMAñàOHHõX
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 à GETMSW );
C                  RMSW - "èEPEMEHHAü" óACTú MSW ( "OTPEáOK" MSWALL );
C                  LM - ÑãàHA "èEPEMEHHOâ" óACTà RMSW;
C
C   èOÑèPOÉPAMMõ:  1.COMMON PROF1
C   ------------
C
C   èPàMEóAHàE:    1. KOHTPOãú B MàHàMAãúHOM OÅ"EME;
C   -----------    2. BCE HE OèPEÑEãüEMõE èOãü RMSW áAHìãüûTCü;
C
C
C  B OèàCAHàà ñEèà àCèOãúáìETCü CãEÑ. áAèàCú:
C
C   ... è NELT  NSM  TCIU  TZAD  TIMP
C      áÑECú àCèOãúáOBAHõ CãEÑ. OÅOáHAóEHàü:
C      NELT  -HOMEP ùãEMEHTA, ìèPABã.BEHTàãEM (KãûóEM);
C             ( Càîì, Tàè MOÑEãà #301 );
C      NSM   -èOPüÑKOBõâ HOMEP CàÉHAãA ìèPABãEHàü Càîì;
C
C
C
C
C                  4. HàÜE èPàBOÑüTCü îOPMATõ RMSW:
C
C      RMSW TàèA # 93 àãà # 668 [ BEHTàãú(Kãûó), ìèPABãüEMõâ Càîì]
C      -----------------------------------------------------------
C     SPM  I                SRAB
C +----+---+------+-----+---+------+----+--------+----+
C INELTINSMI ATOKMI KD  IHX I MCIU I HX I        I Q  I
C I    I   I      I     I   I      ICIU I PEáEPB I    I
C I    I   I      I     I   I      I    I        I    I
C +----+---+------+-----+---+------+----+--------+----+
C I  1 I 2 I   3  I  4  I 5 I   6  I  7 I   8,9  I 10 I
C +----+---+------+-----+---+------+----+--------+----+
C I 11 I12 I  13  I 14  I15 I  16  I 17 I  18,19 I 20 I
C +----+---+------+-----+---+------+----+--------+----+
C
C      BEHTàãú(Kãûó) OTKPõT -> KD=1; BEHTàãú (Kãûó) áAKPõT ->  KD=0;
C      HX - òAÉ èO BPEMEHà, BPEMü BOáMOÜHOÉO èEPEKãûóEHàü;
C
C        MCIU  à  HXCIU - èAPAMETPõ "BOCTAHOBãEHHOÉO" àMPìãúCA "Càì"
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         EQUIVALENCE ( RN , N )
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         NEL = BA0T(1)
         TIP = BA0T(2)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 93 .OR. TIP .EQ. 668 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ãOÉ. CÅOâ B FM0017: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1        CONTINUE
C---> OÅHìãEHàE BCEX èOãEâ RMSW Ñãü BEHTàãü (KãûóA)
         LM = 10
         DO 100 I=1,LM
100      RMSW(I) = 0.0
C===> èPàHüTú àCXOÑ. èAPAMETPõ:
         NELT = PAR0T(1)
         NSM  = PAR0T(2)
C ---> áAèOãHüEM MSW
         N = NELT
         RMSW(1) = RN
         N = NSM
         RMSW(2) = RN
         RETURN
         END
