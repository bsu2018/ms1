       SUBROUTINE SEQSOS(NEL, TN, KD, TOKN, UN )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: SEQSOS(NEL, TN, KD, TOKN, UN )
C   ------------
C   HA‡HA—EHˆE:        KOHTPO‹œ ( ˆ KOPPEK–ˆŸ !!! ) KO„A COCTOŸHˆŸ
C   ----------         BEHTˆ‹œHOƒO ‹EMEHTA;
C
C   METO„:             AHA‹ˆ‡ O‹O†EHˆŸ PA. TO—Kˆ (UN,TOKN) HA BAX
C   -----              BEHTˆ‹Ÿ; HEOPE„E‹EHHOCTœ -->(-0,+0)<--
C                      AHA‹ˆ‡ˆP“ETCŸ O TO—KE (UOPEN, ICLOSE);
C
C   APAMETP›:         NEL - HOMEP ‹EMEHTA;
C   ---------          TN - BPEMŸ OEP„E‹EHˆŸ TO—KA BAX:
C                      TOKN - ‡HA—EHˆE TOKA ˆ
C                      UN - A„EHˆE HAPŸ†EHˆŸ HA BEHTˆ‹Ÿ;
C
C   O„POƒPAMM›:      1.COMMON PROF1
C   -------------
C  PˆME—AHˆE:         1. OKA TO‹œKO COO™EHˆE...!!!!!!!!
C  ----------
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(80)
       EQUIVALENCE( MASPRT(14), NWTRE)
       EQUIVALENCE( MASPRT(77), ICLOSE )
       EQUIVALENCE( MASPRT(78), UOPEN  )
       REAL ICLOSE
C######################################################################
C  KOƒ„A "OTKP›T", TO...
C
C [ UN >= 0.0 ]   TOKN >= -ICLOSE
C
C KOƒ„A "‡AKP›T", TO...
C
C UN < = UOPEN   [ TOKN <= 0.0 ]
C
C######################################################################
       IF ( UN .GE. UOPEN  .AND. TOKN .GT. -ICLOSE ) GO TO 1
C--->  Bˆ„ˆMO ‡AKP›T?
       IF ( UN .LE. 0.0  .AND. TOKN .LE. -ICLOSE ) GO TO 2
666    CONTINUE
C-->  HEˆ‡BECHO, —TO ?
       WRITE(NWTRE,100) NEL, TN, KD, TOKN, UN
100    FORMAT(' *** —TO-TO HEŸCHO: NEL=',I5,' TN=',G12.5,' KD=',I1,
     = ' TOKN=',G12.5,' UN=',G12.5, ' ***')
       RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
1      CONTINUE
C--->  „O‹†EH ›Tœ OTKP›T›M....
       IF ( KD .EQ. 1 ) RETURN
C-->   ????
       GO TO 666
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
2      CONTINUE
C--->  „O‹†EH ›Tœ ‡AKP›T›M
       IF ( KD. EQ. 0 ) RETURN
C--->  ???
       GO TO 666
       END
