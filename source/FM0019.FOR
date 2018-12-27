       SUBROUTINE FM0019(BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.1  (OT   MAü   1987 É.)------------
C   èOÑèPOÉPAMMA: FM0019(BA0T, KNOTT, PAR-T, RMSW, LM )
C   -------------
C   HAáHAóEHàE:   HACTPOâKA MAKPOMOÑEãà TàèA # 301
C   ----------   CàCTEMA àMèìãúCHO-îAáOBOÉO ìèPABãEHàü ( Càîì )
C                BEHTàãúHõM èPEOÅPAáOBATEãEM.
C                ***********************************************
C                è.TIP # 4 -> ãOÉ. MAKPOMOÑEãú.
C                ( DM0023, LM0006 )
C                      îOPMàPìET "èEPEMEHHìû" óACTú RMSW OèàCAHàü
C                      ( ÑãàHA :==> +1(VAR) )
C
C
C              ùãEMEHT OèàCõBAETCü:
C
C      ù # T 301 ì 0 è
C              MAS < NTOKOB  #ùã1 #CM1  #ùã2 #CM2 ... >
C              MAS < NHAèP   ìáEãN1 ìáEãK1  ìáEãN2 ìáEãK2 ... >
C              PSUB #P < .........>;
C
C              óEPEá AèèAPAT PSUB áAÑAûTCü áAKOHõ ìèPABãEHàü ;
C
C      ÉÑE
C          NTOKOB -   KOãàóECTBO TOKOB ùãEMEHTOB CXEMõ , KOTOPõE
C                     àCèOãúáìûTCü èPà PACóETE CàÉHAãOB ìèPABãEHàü(Càì);
C          #ùãI , #CMI     - HOMEP ùãEMEHTA à HOMEP TOKA ùãEMEHTA ,
C                            àCèOãúáìEMOÉO èPà PACóETE Càì;
C          NHAèP           - KOãàóECTBO HAèPüÜEHàâ MEÜÑì ìáãAMà CXEMõ ,
C                            àCèOãúáìEMõX èPà PACóETE Càì;
C          ìáEãNI , ìáEãKI - HOMEPA ìáãOB , HAèPüÜEHàü MEÜÑì KOTOPõMà
C                            àCèOãúáìûTCü èPà PACóETE Càì;
C          #P              - HOMEP è/è èOãúáOBATEãü ( PSUB#P ) ,
C                            B KOTOPOâ èPOàáBOÑàTCü PACóET Càì;
C
C
C   METOÑ:         PAáMETKA èOãEâ RMSW èOÑ MOÑEãú
C   -----
C
C   èAPAMETPõ:     BA0T, KNOTT, PAR0T - "OTPEáKà" àHîOPMAñàOHHõX
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 à GETMSW );
C                  RMSW - "èEPEMEHHAü" óACTú MSW ( "OTPEáOK" MSWALL );
C                  LM - ÑãàHA "èEPEMEHHOâ" óACTà RMSW èPà FGMSW = 0;
C                  LM - ÑãàHA BCEÉO MSW OèàCAHàü      èPà FGMSW = 1;
C
C   èOÑèPOÉPAMMõ:  1.COMMON PROF1
C   ------------
C
C   èPàMEóAHàE:    1. KOHTPOãú B MàHàMAãúHOM OÅ"EME;
C   -----------    2. BCE HE OèPEÑEãüEMõE èOãü RMSW áAHìãüûTCü;
C                  3. KOã-BO èAPAMETPOB B è/è PSUB HE KOHTPOãàP.
C                  4. HàÜE èPàBOÑüTCü îOPMATõ RMSW:
C
C              îOPMAT RMSW Tàè # 301:
C              ----------------------
C+-----------I--------------I------I-----------------------------------
C  # CãOBA   I    # CãOBA B I KTO  I
C OT HAóAãA  I    OTPEáKE   IOèPEÑ.I
C BCEÉO MSW  I    RMSW B    I      I       OÅOáHAóEHàE - áHAóEHàE
C OèàCAHàü   I    FM0019    I      I
C------------I--------------I------I-----------------------------------
C     9      I     0        I      I HOMEP ìáãA ( BCEÉÑA 0 )
C----------------------------------------------------------------------
C    10      I     1        I  DM  I  FQ - îãAÉ HOBõE áHAó. BõóàCãEHõ
C            I              I      I   ( Ñãü CàHXPO. MAKPO. P.TIP # 4 )
C    11      I     2        I  FM  I  NTOKOB -KOãàóECTBO TOKOB, àCèOãú-
C            I              I      I          áìEMõX èPà PACóETE Càì;
C    12      I     3        I  FM  I  #ùã1 - HOMEP ùãEMEHTA;
C    13      I     4        I  FM  I  #CM1 - HOMEP TOKA ùãEMEHTA #ùã1;
C    14      I     5        I  LM  I  ATOKM1 - AÑP B TOKN/N1 áHAóEHàü
C            I              I      I           TOKA HOMEP #CM1
C            I              I      I           ùãEMEHTA #ùã1;
C  9+I*3     I     I*3      I  FM  I  #ùãI -      -"-
C 10+I*3     I   1+I*3      I  FM  I  #CMI -      -"-
C 11+I*3     I   2+I*3      I  LM  I  ATOKMI -    -"-
C .......    I   .......    I  ... I  ..........................
C12+NTOKOB*3 I  3+NTOKOB*3  I  FM  I  NHAèP -KOãàóECTBO HAèPüÜEHàâ,
C            I              I      I         àCè. Ñãü PACóETA Càì;
C13+NTOKOB*3 I  4+NTOKOB*3  I  FM  I  ìáEãN1 -ìáEã HAóAãA HAèPüÜEHàü;
C14+NTOKOB*3 I  5+NTOKOB*3  I  FM  I  ìáEãK1 -ìáEã KOHñA  HAèPüÜEHàü;
C .........  I ............ I .... I  ...........................
C            I              I      I
C13+NTOKOB*3+I 3+NTOKOB*3+  I      I  NPSUB - HOMEP è/è èOãúáOBATEãü
C NHAèP*2    I  NHAèP*2     I  FM  I         ( PSUB ) B KOTOPOâ èPOàá-
C            I              I      I         BOÑàTCü PACóET Càì;
C (...)+1    I  (...)+1     I  FM  I  NPAR - KOãàóECTBO èAPAMETPOB PSUB;
C            I              I      I
C OT(...)+2  I OT(...)+2    I  FM  I  èAPAMETPõ PSUB , áAÑAHHõE
CÑO(...)+NPARI ÑO(...)+NPAR I      I  èOãúáOBATEãEM èãûC BOCEMú
C  + 8       I  + 8         I      I  ùãEMEHTOB (HA BCüKàâ CãìóAâ)
C----------------------------------------------------------------------
C
C  OÅôAü ÑãàHA MSW --> LM<BCü> = 22 + 3*NTOKOB + 2*NNAPR + NPAR
C
C  ÑãàHA èEPEMEH. óACTà --> LM<RMSW> = 13 + 3*NTOKOB + 2*NNAPR + NPAR
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
       EQUIVALENCE( NWTR, MASPRT(13) )
       EQUIVALENCE( NWTRE, MASPRT(14) )
       EQUIVALENCE( FGMSW, MASPRT(87) )
       EQUIVALENCE ( RN  ,   N  )
       INTEGER  TIP, BA0T, FGMSW
       TIP = BA0T(2)
       NEL = BA0T(1)
       IF ( TIP .EQ. 301 ) GO TO 1
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 501 ) NEL, TIP
501    FORMAT(/' *** ãOÉ. CÅOâ B FM0019 --> HOMEP TàèA HE HAò ***'/
     =  ' *** HOMEP ùãEMEHTA ', I5, ' HOMEP TàèA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
C######################################################################
1      CONTINUE
C--> OèPEÑEãEHàE KOHCTAHT
       NTOKOB = 0
       IF( PAR0T(2) .GT. 0.0 ) NTOKOB = IFIX ( PAR0T(2) )
       INNAPR = 4 + 2*NTOKOB
       NNAPR  = 0
       IF( PAR0T(INNAPR) .GT. 0.0 ) NNAPR = IFIX ( PAR0T(INNAPR) )
       INPSUB  = INNAPR + 2*NNAPR + 1
       NPSUB   = -IFIX ( PAR0T(INPSUB) )
       NPAR   = IFIX ( PAR0T(INPSUB + 1) )
       IF ( FGMSW .EQ. 0 ) GO TO 1000
C----------------------------------------------------------------------
C-->  ìTOóHàTú LM <OÅôìû >
       LM = 22 + 3*NTOKOB + 2*NNAPR + NPAR
       RETURN
C######################################################################
1000   CONTINUE
C-->  èPàHüTú à óTO MOÜHO CîOPMàPOBATú..
       LM = 13 + 3*NTOKOB + 2*NNAPR + NPAR
       DO 2 I=1,LM
2      RMSW(I) = 0.0
C---> èPOBEPàM  èPABàãúHO ãà BBEÑEHõ ÑAHHõE?
       NTKOB1 = (IFIX ( PAR0T(1) ) - 1 ) / 2
       IF ( NTOKOB .EQ. NTKOB1 ) GO TO 3
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 502 ) NEL, TIP
502    FORMAT(/' *** OòàÅKA B FM0019 -->HEèPABàãúHO áAÑAHO KOãàóECTBO TO
     =KOB ***'/' *** HOMEP ùãEMEHTA ', I5, ' HOMEP TàèA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
3      CONTINUE
       NNAPR1 = (IFIX( PAR0T(INNAPR-1) ) - 1 ) / 2
       IF ( NNAPR .EQ. NNAPR1 ) GO TO 4
C!!!-------------------------------------------------------------------
       WRITE(NWTRE, 503 ) NEL, TIP
503    FORMAT(/' *** OòàÅKA B FM0019 -->HEèPABàãúHO áAÑAHO KOã-BO HAèPüÜ
     =EHàâ***'/' *** HOMEP ùãEMEHTA ', I5, ' HOMEP TàèA ', I5, ' ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(TIP)
4      CONTINUE
C----------------------------------------------------------------------
C---->  áAèOMHàM HOMEPA TOKOB  ----------------------------------------
       N = NTOKOB
       RMSW(2) = RN
       IPAR0T = 2
       IRMSW = 2
       IF( NTOKOB .EQ. 0 ) GO TO 51
          DO 5 I = 1 , NTOKOB
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IRMSW = IRMSW + 1
5      CONTINUE
51     CONTINUE
C----------------------------------------------------------------------
C---->  áAèOMHàM HOMEPA HAèPüÜEHàâ  -----------------------------------
       IPAR0T = IPAR0T + 2
       IRMSW = IRMSW + 1
       N = NNAPR
       RMSW(IRMSW) = RN
       IF( NNAPR .EQ. 0 ) GO TO 61
          DO 6 I = 1 , NNAPR
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          N = IFIX ( PAR0T(IPAR0T) )
          RMSW(IRMSW) = RN
6      CONTINUE
61     CONTINUE
C----------------------------------------------------------------------
C---->  áAèOMHàM èAPAMETPõ, èEPEÑABAEMõE B è/è èOãúáOBATEãü  ----------
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       N = NPSUB
       RMSW(IRMSW) = RN
       IPAR0T = IPAR0T + 1
       IRMSW = IRMSW + 1
       N = NPAR
       RMSW(IRMSW) = RN
          DO 7 I = 1 , NPAR
          IPAR0T = IPAR0T + 1
          IRMSW = IRMSW + 1
          RMSW(IRMSW) = PAR0T(IPAR0T)
7      CONTINUE
C######################################################################
       RETURN
       END
