       SUBROUTINE PSUB4( NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA: PSUB4( NEL, URMAS, STMAS, DGMAS, INTMAS )
C   -------------
C   HAáHAóEHàE:  è/è èOãúáOBATEãü Ñãü MOÑEãàPOBAHàü
C   ----------   áAKOHOB ìèPABãEHàü OTÑEãúHõM BEHTàãEM àãà
C                ÉPìèèOâ BEHTàãEâ.
C                  ( MOÑEãú #301 - Càîì ).
C               èO BEãàóàHAM, èEPEÑABAEMõM èOãúáOBATEãEM à
C               áHAóEHàüM TOKOB à HAèPüÜEHàâ ùãEMEHTOB CXEMõ
C               OèPEÑEãüûTCü KOÑ Càì KAÜÑOÉO BEHTàãü à òAÉ ÑO
C               MOMEHTA àáMEHEHàü áHAóEHàü Càì.
C
C   METOÑ:     OóEBàÑEH.
C   -----
C  èAPAMETPõ:   NEL - HOMEP ùãEMEHTA, Ñãü KOTOPOÉO PSUB PAÅOTAET;
C  ---------    URMAS - MACCàB ÑAHHõX èOãúáOBATEãü:
C                       1-E CãOBO - KOãàóECTBO èAPAMETPOB B CèàCKE
C                       ( áAèàCAHO ñEãõM óàCãOM B BEôECTBEHHõâ MACCàB )
C                       2-E CãOBO à ÑAãEE - èAPAMETPõ PSUB,
C                           áAÑAHHõE èOãúáOBATEãEM;
C
C----------------------------------------------------------------------
C              STMAS - MACCàB ÑAHHõX, èEPEÑABAEMõX B PSUB:
C
C      CãOBO           OÅOáHAóEHE   -   áHAóEHàE
C      1               TN - BPEMü N - ÉO òAÉA PACóETA;
C      2               H0 - òAÉ àHTEÉPàPOBAHàü;
C      3               I1 - áHAóEHàE 1-ÉO TOKA, áATPEÅOBAHHOÉO
C                           èOãúáOBATEãEM, Ñãü MOMEHTA BPEMEHà TN;
C      4               I11- TOÜE, Ñãü MOMEHTA TN+H0;
C         ÑAãEE AHAãOÉàóHO áHAóEHàü OCTAãúHõX TOKOB;
C   N=3+2*NTOKOB       U1 - áHAóEHàE 1-ÉO HAèPüÜEHàü, áATPEÅOBAHHOÉO
C                           èOãúáOBATEãEM, Ñãü MOMEHTA BPEMEHà TN;
C    N+1               U11- TOÜE, Ñãü MOMEHTA TN+H0;
C         ÑAãEE AHAãOÉàóHO áHAóEHàü OCTAãúHõX HAèPüÜEHàâ;
C
C----------------------------------------------------------------------
C              DGMAS  -  MACCàB BõóECãEHHõX èAPAMETPOB Càì.
C
C   CãOBO        OÅOáHAóEHE - áHAóEHàE:
C  OT 1 ÑO 6     - áHAóEHàü MCIU ( KOÑ Càì )
C                  MCIU = 00  --> "Càì" HET B [TN, TN+H0];
C                                  HX = 0.0;
C                  MCIU = 01  --> B [TN, TN+H0] -> èEPEÑHàâ îPOHT
C                                 àMèìãúCA "Càì", HX = àHTEPBAã OT
C                                 TN ÑO èEPEÑHEÉO îPOHTA "Càì";
C                  MCIU = 10 -->  B [TN, TN+H0] -> áAÑHàâ îPOHT
C                                 àMèìãúCA "Càì", HX = àHTEPBAã
C                                 OT TN ÑO áAÑHEÉO îPOHTA àMèìãúCA  ;
C                  MCIU = 11 -->  B [TN, TN+H0] "Càì" BKãûóEH,
C                                 COCTOüHàE HE MEHüã, HX=0.0;
C
C  OT 7 ÑO 12    - áHAóEHàü COOTBETCTBìûôàX HX ( òAÉA èO BPEMEHà
C                    ÑO MOMEHTA àáMEHEHàü KOÑA Càì )
C----------------------------------------------------------------------
C              INTMAS - MACCàB àá OÑHOÉO CãOBA.
C                       ( B èOÑèPOÉPAMME HE àCèOãúáìETCü )
C----------------------------------------------------------------------
C
C   èOÑèPOÉPAMMõ: HET
C   -------------
C
C   èPàMEóAHàE: CìMMA KOããàóECTBA TPEÅìEMõX TOKOB à HAèPüÜEHàâ
C   ----------  HE ÑOãÜHA èPEBõòATú 49-Tà.
C               ( KOHTPOãú HE èPOàáBOÑàTCü )
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(*), STMAS(*), DGMAS(*), INTMAS(*)
       REAL INTMAS,IZ,IZ1,IN,IN1
C----------------------------------------------------------------------
       TN = STMAS(1)
       H0 = STMAS(2)
C----------------------------------------------------------------------
C èOCãEÑìûôAü óACTú è/è PAáPAÅATõBAETCü èOãúáOBATEãEM
C----------------------------------------------------------------------
       CIU  = STMAS(3)
       CIU1 = STMAS(4)
       IZ   = STMAS(5)
       IZ1  = STMAS(6)
       IN   = STMAS(7)
       IN1  = STMAS(8)
       DELI = URMAS(2)
C----------------------------------------------------------------------
       IF( TN .EQ. 0.0 ) GO TO 1000
       IF( CIU1 .GT. 0.5 ) GO TO 500
C----------------------------------------------------------------------
C PAÅOTAET èAPA K2 à K3
C----------------------------------------------------------------------
        IZ = IZ - DELI
        IZ1= IZ1- DELI
        DGMAS(1) = 00
        DGMAS(2) = 11
        DGMAS(7) = 0.0
        DGMAS(8) = 0.0
        IF( IN1 .GT. IZ1 ) RETURN
        DGMAS(1) = 01
        DGMAS(2) = 10
        GO TO 800
500     CONTINUE
C----------------------------------------------------------------------
C PAÅOTAET èAPA K1 à K4
C----------------------------------------------------------------------
        IZ = IZ + DELI
        IZ1= IZ1+ DELI
        DGMAS(1) = 11
        DGMAS(2) = 00
        DGMAS(7) = 0.0
        DGMAS(8) = 0.0
        IF( IN1 .LT. IZ1 ) RETURN
        DGMAS(1) = 10
        DGMAS(2) = 01
800     CONTINUE
        DGMAS(7) = ( (TN+H0)*(IZ-IN) - TN*(IZ1-IN1) ) / (IN1-IZ1+IZ-IN)
        DGMAS(8) = DGMAS(7)
        RETURN
1000    CONTINUE
        DGMAS(1) = 01
        DGMAS(2) = 00
        DGMAS(7) = 0.0
        DGMAS(8) = 0.0
        RETURN
        END
