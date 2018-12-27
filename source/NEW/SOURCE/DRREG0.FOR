       SUBROUTINE DRREG0 ( KAR, DAT, MSWALL , TOKN , VALLN )
CXXX****KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)*************
C*                                                                     *
C*  O„POƒPAMMA :   DRREG0 ( KAR, DAT, MSWALL , TOKN , VALLN )        *
C*  ------------                                                       *
C*                                                                     *
C*  HA‡HA—EHˆE :  * OPAOT—ˆK „ˆPEKTˆB› #PE†ˆM *                      *
C*  ----------      PˆEM CˆCKA ˆ ‡A„AHˆE "HA—A‹œH›X" „AHH›X          *
C*                  TOKOB ˆ HAPŸ†EHˆ‰ COƒ‹ACHO CˆCKA:                *
C*                  #PE†ˆM                                             *
C*                  "V" Z "“" K1 K2....;                               *
C*                  "V" Z "“" "BCE";                                   *
C*                  "I" Z "" N1[.S1]...N2...N3[.BCE];                 *
C*                  "GAMMA" Z "" N1 N2...;                            *
C*                  "OMEGA" Z "" N1 N2...;                            *
C*                  "I" Z "" "BCE";                                   *
C*                   #BCE                                              *
C*           *** TO HE PEA‹ˆ‡OBAHO - TO‹œKO OH“‹EHˆE -               *
C*                                                                     *
C*  METO„ :         BBO„ ˆ PA‡OP „AHH›X , KOHTPO‹œ ˆ ‡AˆCœ           *
C*  -----           B TOKN ˆ VALLN COƒ‹ACHO HA‹ˆ—ˆŸ B MSWALL.          *
C*                                                                     *
C*  APAMETP› :    KAR,DAT - OˆCAHˆE CTPOKˆ „ˆPEKTˆB›;                *
C*  ---------       MSWALL - CˆCOK “PAB‹Ÿž™ˆX C‹OB ‹EMEHTOB;        *
C*                    TOKN - "TOKˆ" ‹EMEHTOB;                         *
C*                   VALLN - "HAPŸ†EHˆŸ" ‹EMEHTOB.                   *
C*                                                                     *
C*  OPOƒPAMM› :   1.PROF1 , SERVZ1                                   *
C*  -----------     2.INPUT2                                           *
C*                                                                     *
C*  PˆME—AHˆE:     —ACTˆ—HAŸ PEA‹ˆ‡A–ˆŸ                               *
C* ------------     ( MAKET POƒPAMM› ).                               *
C*                                                                     *
CZZZ********************************************************************
      COMMON / PROF1 / MASPRT(200)
      DIMENSION KAR(1),DAT(1),MSWALL(1),TOKN(1),VALLN(1)
      EQUIVALENCE ( NNTOK , MASPRT(46) )
      EQUIVALENCE ( FMSWAL , MASPRT(74) )
      EQUIVALENCE ( NMKNOT , MASPRT(7) )
      INTEGER FMSWAL
C-->
      EQUIVALENCE ( NWTR , MASPRT(13) )
      EQUIVALENCE ( NRDR , MASPRT(12) )
      IF( FMSWAL.EQ.0 ) WRITE (NWTR,1)
 1    FORMAT(/' *** HAP“˜EH OPŸ„OK C‹E„OBAHˆŸ „ˆPEKTˆB.',
     *' „ˆPEKTˆBA ˆƒHOPˆP“ETCŸ ***'/)
      IF( FMSWAL.EQ.0 ) RETURN
C-->   ‡AH“‹EHˆE E‡ BCŸKOƒO...
C----------------------------------------------------------------------
      DO 100 I=1,NNTOK
      TOKN(I)=0.0
 100  CONTINUE
C----------------------------------------------------------------------
      DO 200 I=1,NMKNOT
      VALLN(I)=0.0
 200  CONTINUE
C----------------------------------------------------------------------
C
       WRITE(NWTR,3000)
3000   FORMAT(/' *** „ˆPEKTˆBA #PE†ˆM - OKA  HE PEA‹ˆ‡OBAHA ***'  )
       WRITE(NWTR,3004)
3004  FORMAT(' *** HA—A‹œH›E ‡HA—EHˆŸ BCEX EPEMEHH›X “CTAHAB‹ˆBAžTCŸ',
     =' PABH›Mˆ 0.0 ***'/)
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      RETURN
      END
