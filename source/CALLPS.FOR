       SUBROUTINE CALL PS(NSUB, NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOMèãEKC "MS1" --> BEPCàü 1.0  (OT ÑEKAÅPü 1986 É.)------------
C   èOÑèPOÉPAMMA: CALL PS(NSUB, NEL, URMAS, STMAS, DGMAS, INTMAS )
C   ------------
C   HAáHAóEHàE:    èO HOMEPì NSUB, Ñãü áãEMEHTA ñEèà NEL BõáõBAET
C   ----------     è/è èOãúáOBATEãü PSUB#:
C
C                    CALL PSUB#( NEL, URMAS, STMAS, DGMAS, INTMAS )
C
C        ÉÑE       URMAS - MACCàB áHAóEHàâ èAPAMETPOB, èEPEÑABAEMõâ
C                          èOãúáOBATEãEM è/è PSUB# óEPEá CèàCOK:
C                            PSUB # < ... URMAS ....>;
C                          1-E CãOBO - KOã-BO èAPAMETPOB, áATEM àX
C                          áHAóEHàü;
C                  STMAS - MACCàB ÑAHHõX, èEPEÑABAEMõâ PSUB#,
C                  DGMAS - MACCàB "PACóàTAHHõX" áHAóEHàâ, HAâÑEHHõX B
C                          PSUB#;
C                  INTMAS - MACCàB PAÅOóàX CãOB, Ñãü OPÉAHàáAñàà,
C                           HAèPàMEP, àHTEPÉPàPOBAHàü èO TOóKAM;
C
C                        CTPìKTìPõ à PAáMEPõ ùTàX MACCàBOB
C                        OèPEÑEãüûTCü B KOHKPETHõX CãìóAüüX;
C
C   METOÑ:    OóEBàÑEH.
C   ------
C   èAPAMETPõ:  CM. HAáHAóEHàE;
C   ---------
C   èOÑèPOÉPAMMõ:  PSUB#, ÉÑE # OT 1 ÑO 20 ( KOHKPET. PEAãàáAñàü);
C   ------------
C   èPàMEóAHàE:  1.  KOHTPOãú HAãàóàü PSUB, à KOã-BA
C   ----------       èEPEÑABAEMõX B HEE èAPAMETPOB HE PEAãàáOBAH;
C
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(1), STMAS(1), DGMAS(1), INTMAS(1)
       REAL INTMAS
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( NWTRE, MASPRT(14) )
C----------------------------------------------------------------------
       GO TO ( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),NSUB
C!!!-------------------------------------------------------------------
       WRITE(NWTRE,100) NEL, NSUB
100    FORMAT(/' *** MAKPOMOÑEãú ùã-TA # ', I5, ' BõáBATú PSUB #', I3,
     = ' HE MOÜET ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(NSUB)
C######################################################################
1      CALL PSUB1( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
2      CALL PSUB2( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
3      CALL PSUB3( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
4      CALL PSUB4( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
5      CALL PSUB5( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
6      CALL PSUB6( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
7      CALL PSUB7( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
8      CALL PSUB8( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
9      CALL PSUB9( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
10     CALL PSUB10( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
11     CALL PSUB11( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
12     CALL PSUB12( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
13     CALL PSUB13( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
14     CALL PSUB14( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
15     CALL PSUB15( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
16     CALL PSUB16( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
17     CALL PSUB17( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
18     CALL PSUB18( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
19     CALL PSUB19( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
20     CALL PSUB20( NEL, URMAS, STMAS, DGMAS, INTMAS )
C######################################################################
       RETURN
       END
