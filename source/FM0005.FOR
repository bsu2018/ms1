         SUBROUTINE FM0005( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA:  FM0005( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HAáHAóEHàE:    HACTPOâKA MAKPOMOÑEãà:
C  ------------
C                  Tàè     àMü MOÑEãà     OÅOáHAóEHàü B ÑAãúHEâòEM
C
C                  666      àÑ. Kãûó ( PEãE )         KL
C
C                  èPOàáBOÑàT îOPMàPOBAHàE "èEPEMEHHOâ" óACTà MSW
C                  MAKPOMOÑEãà.
C
C   METOÑ:         PAáMETKA èOãEâ RMSW èOÑ MOÑEãú "Kãûó",
C   -----          èPà PMAS=1 - èEPEBOÑ àá ùã. ÉPAÑ. B CEK.
C
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
C                  3. PEAãàáAñàü Ñãü PMAS = 0 , PMAS = 1  à  PMAS = 2.
C                     èPà OòàÅKAX --> èPàHàMAETCü PMAS = 0;
C                     áHAóEHàü èAPAMETPOB ---->
C
C
C                           Kãûó
C                           BKãûó.
C                            I
C                       **********               ******
C                       **********               ******
C                       **********               ******
C  T0*--------I---------**********-----I---------******----------------
C    I        I         I        I     I         I
C    I<-TZAD->I         I<-TIMP->I     I         I
C    I        I<-TALFA->I        I     I<-TALFA->I
C             I                        I
C             I<---------TCIU--------->I<-----------TCIU--------
C
C  B OèàCAHàà ñEèà àCèOãúáìETCü CãEÑ. áAèàCú:
C
C   ... MAS < PMAS TZAD  TCIU  TALFA  TIMP  TKCIU  DTALFA >
C          èPà PMAS=1 - BCE "BPEMEHA" KOÑàPìûTCü B ùã. ÉPAÑ.;
C          èPà PMAS=2 - BCE "BPEMEHA" KOÑàPìûTCü B CEK.;
C  BOáMOÜHO KOÑàPOBAHàE:
C   ... MAS < 0 >, óTO COOTBETCTBìET:
C
C       TZAD = 360 ùã. ÉPAÑ.
C       TCIU = 60  ùã. ÉPAÑ.
C       TAFLA= 30  ùã. ÉPAÑ.
C       TIMP = 10  ùã. ÉPAÑ.
C       TKCIU=  0  ùã. ÉPAÑ.
C       DTALFA= 0  ùã. ÉPAÑ.
C
C      áÑECú àCèOãúáOBAHõ CãEÑ. OÅOáHAóEHàü:
C
C
C      TZAD  - BPEMA áAÑEPÜKà BKãûóEHàü "Càì"    ( CEK. )
C      TCIU  - èEPàOÑ èOBTOPEHàü CàÉHAãOB "Càì"  ( CEK. )
C      TALFA - áAÑEPÜKA èEPEÑHEÉO îPOHTA CàÉHAãA
C              "Càì" OT HAóAãA OóEPEÑHOÉO èEPàOÑA
C              èOBTOPEHàü                          ( CEK. )
C      TIMP - ÑãàTEãúHOCTú àMèYãúCA "Càì"          ( CEK. )
C      TKCIU - óEPEá CKOãúKO èEPàOÑOB èOBTOPEHàü
C              CàÉHAãA "Càì" àáMEHàTú TALFA HA
C      DTALFA - EÑàHàñ - òAÉ àáMEHEHàü TALFA       ( CEK. )
C
C                  4. HàÜE èPàBOÑüTCü îOPMATõ RMSW:
C
C                  RMSW TàèA # 666 [ àÑ. Kãûó     ]
C                  --------------------------------
C                                                             LM = 15
C                                                             *******
C                                                                  (25)
C                    SPM                      SRAB
C +--+--+----+----+----+------+------+---+----------+-----+----+------+
C IKDIHXIPMASITZADITCIUITALFA ITIMP  ITK I DTALFA   I M   I HX I PE   I
C I  I  I    I    I    I      I      ICIUI          I CIU I CIUI áE   I
C I  I  I    I    I    I      I      I   I          I     I    I PB   I
C +--+--+----+----+----+------+------+---+----------+-----+----+------+
C I1 I2 I 3  I 4  I 5  I  6   I  7   I 8 I   9      I 10  I 11 I 12-15I
C +--+--+----+----+----+------+------+---+----------+-----+----+------+
C I11I12I 13 I 14 I 15 I  16  I 17   I18 I   19     I 20  I 21 I 22-25I
C +--+--+----+----+----+------+------+---+----------+-----+-----------+
C
C      Kãûó OTKPõT -> KD=1;    Kãûó áAKPõT ->  KD=0;
C      HX - òAÉ èO BPEMEHà, BPEMü BOáMOÜHOÉO èEPEKãûóEHàü;
C
C        MCIU à HXCIU - èAPAMETPõ "BOCTAHOBãEHHOÉO" àMèìãúCA Càì
C                            ( CM. MCIU01 );
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 666 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** ãOÉ. CÅOâ B FM0005: HOMEP TàèA HE OÅCãìÜàBAETCü **
     =*'/' *** HOMEP TàèA ', I10, ' ùãEMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1        CONTINUE
C---> OÅHìãEHàE BCEX èOãEâ RMSW Ñãü KãûóA
         LM = 15
         DO 100 I=1,LM
100      RMSW(I) = 0.0
C===> èPàHüTú àCXOÑ. èAPAMETPõ:
C-->  KOTPOãú
         KKM = PAR0T(1)
C-->  BàÑ ìèPABãEHàü..
         PMAS = PAR0T(2)
         IF ( PMAS .EQ. 0.0 ) GO TO 2
         IF ( PMAS .EQ. 1.0 .OR. PMAS .EQ. 2.0 ) GO TO 3
C----------------------------------------------------------------------
         WRITE(NWTR,4) NEL, PMAS
4        FORMAT(' *** Tàè ìèPABãEHàü "Càì" Ñãü ùãEMEHTA #', I5, ' ***',
     =          ' *** PABHõâ ', F5.0,' OòàÅOóEH  --> èPàHàMAETCü',
     =    ' PABHõM Hìãû ***')
         GO TO 2
C----------------------------------------------------------------------
C-->  PMAS = 0    èO ìMOãóAHàû
2        CONTINUE
         RMSW(4) = 360.0 / 1.8E+4
         RMSW(5) =  60.0 / 1.8E+4
         RMSW(6) =  30.0 / 1.8E+4
         RMSW(7) =  10.0 / 1.8E+4
C-->  OCTAãúHõE à TAK PABHõ 0.0
         RETURN
C######################################################################
3        CONTINUE
C---> PMAS = 1 àãà 2
C-->  èPOBEPKA HA èOãHOTì
         IF ( KKM .GT. 7 ) WRITE(NWTR,40) KKM, NEL
40       FORMAT(' *** MHOÉO èAPAMETPOB OèàCAHàü "Càì" ->', I5, ' ***',
     =          ' *** ãàòHàE àÉHOPàPìûTCü (ùãEMEHT # ',I5,' ) ***')
         IF ( KKM .GT. 7 ) KKM = 7
C--->
         KX = 7 - KKM
         IF ( KKM .LT. 7 ) WRITE(NWTR,50) NEL, KX
50       FORMAT(' *** èAPAMETPõ ìèPABãEHàü "Càì" Ñãü ùãEMEHTA #', I5,
     =    ' ***'/' *** áAÑAHõ HE èOãHOCTúû --> HET ', I2, ' áHAóEHàâ',
     =    ' ***'/ ' *** èPOèìôEHHõE áHAóEHàü èPàHàMAûTCü PABHõMà 0.0',
     =    ' ***')
C----------------------------------------------------------------------
       CONST = 1.8E+4
       IF ( PMAS .EQ. 2 ) CONST = 1.0
         DO 6 I = 1, KKM
         IF ( I .EQ. 1 ) RMSW(3) = PMAS
         IF ( I .EQ. 2 ) RMSW(4) = PAR0T(3) / CONST
         IF ( I .EQ. 3 ) RMSW(5) = PAR0T(4) / CONST
         IF ( I .EQ. 4 ) RMSW(6) = PAR0T(5) / CONST
         IF ( I .EQ. 5 ) RMSW(7) = PAR0T(6) / CONST
         IF ( I .EQ. 6 ) RMSW(8) = PAR0T(7)
         IF ( I .EQ. 7 ) RMSW(9) = PAR0T(8) / CONST
6        CONTINUE
C--->  KOHTPOãú TCIU
         IF ( RMSW(5) .NE. 0.0 ) RETURN
         WRITE(NWTR,7) NEL, PAR0T(4)
7        FORMAT(' *** èEPàOÑ èOBTOPEHàü àMèìãúCA "Càì" Ñãü ùãEMEHTA',
     = ' # ', I5,' ***'/
     =  ' *** PABHõâ ', G12.5, ' ----> HEÑOèìCTàM ***'/
     =  ' *** èPàHàMAETCü PABHõM 360.0 ùã. ÉPAÑ. ***')
         RMSW(5) = 360.0 / 1.8E+4
         RETURN
         END
