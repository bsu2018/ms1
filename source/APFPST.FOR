      SUBROUTINE APFPST( SIM , RSIM , KSIM , POST , RPOST , K , KKK )
CXXX****KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)*************
C*                                                                     *
C*    O„POƒPAMMA :  APFPST ( SIM , RSIM , KSIM , POST , RPOST , K    *
C*    ------------             KKK )                                   *
C*                                                                     *
C*    HA‡HA—EHˆE :    EPEBO„ ˆH”ˆKCHOƒO B›PA†EHˆŸ S(1),S(2),...,S(N), *
C*    ----------      N>=1, B OCT”ˆKCH“ž ”OPM“. S(I) - ˆ„EHTˆ”ˆKATOP, *
C*                    —ˆC‹O, '(' ˆ‹ˆ ')', ‡HAKˆ OEPA–ˆ‰ ( + , - , / , *
C*                    *  ˆ‹ˆ  ** ).                                    *
C*                                                                     *
C*    METO„ :         OCHOBAH HA ˆCO‹œ‡OBAHˆˆ CTEKA C PˆOPˆTETAMˆ.   *
C*    -----           A‹ƒOPˆTM PAOTAET, —ˆTAŸ CˆMBO‹› ˆH”ˆKCHOƒO      *
C*                    B›PA†EHˆŸ C‹EBA HAPABO. BCE ˆ„EHTˆ”ˆKATOP›      *
C*                    ( ˆ‹ˆ —ˆC‹A ) OCT“AžT HA B›XO„ O MEPE —TEHˆŸ; *
C*                    ‡HAKˆ OEPA–ˆ‰ OME™AžTCŸ B CTEK OEPA–ˆ‰. EC‹ˆ  *
C*                    PˆOPˆTET BXO„HOƒO ‡HAKA PABEH 0 ˆ‹ˆ O‹œ˜E      *
C*                    PˆOPˆTETA ‡HAKA, HAXO„Ÿ™EƒOCŸ B BEP˜ˆHE CTEKA,  *
C*                    TO HOB›‰ ‡HAK „OAB‹ŸETCŸ B BEP˜ˆH“ CTEKA.       *
C*                    B POTˆBHOM C‹“—AE ˆ‡ CTEKA "B›TA‹KˆBAETCŸ" ˆ    *
C*                    EPEˆC›BAETCŸ B B›XO„H“ž CTPOK“ ‡HAK,           *
C*                    HAXO„Ÿ™ˆ‰CŸ B BEP˜ˆHE, A TAK†E C‹E„“ž™ˆE ‡A      *
C*                    HˆM ‡HAKˆ C PˆOPˆTETAMˆ, O‹œ˜ˆMˆ ˆ‹ˆ PABH›Mˆ   *
C*                    PˆOPˆTET“ BXO„HOƒO ‡HAKA. OC‹E TOƒO BXO„HO‰   *
C*                    ‡HAK „OAB‹ŸETCŸ K BEP˜ˆHE CTEKA. '(' "B›TA‹-    *
C*                    KˆBAETCŸ TO‹œKO ')', HO HA B›XO„ HE EPE„AETCŸ.  *
C*                                                                     *
C*        TA‹ˆ–A PˆOPˆTETOB ‡HAKOB OEPA–ˆ‰ ˆ OƒPAHˆ—ˆTE‹E‰          *
C*                 -------------------------------------               *
C*                   C ˆ M O ‹   !    P ˆ O P ˆ T E T                 *
C*                 -------------------------------------               *
C*                       (       !           0                         *
C*                       )       !           1                         *
C*                     +   -     !           2                         *
C*                     *   /     !           3                         *
C*                       **      !           4                         *
C*                    ”“HK–ˆŸ    !           5                         *
C*                 -------------------------------------               *
C*                                                                     *
C*    APAMETP› :     SIM  - MACCˆB TEPMOB B  F-KO„E (INTEGER*4) ,     *
C*    ---------              ‡AˆCAHH›X B ˆH”ˆKCHOM Bˆ„E.              *
C*                    RSIM - TO †E —TO ˆ SIM , HO REAL*4               *
C*                    KSIM - KO‹ˆ—ECTBO ‡AHŸT›X O‡ˆ–ˆ‰ B MACCˆBE      *
C*                           SIM ( <= 100 ).                           *
C*                    POST - MACCˆB TEPMOB, PE„CTAB‹Ÿ™ˆ‰ COO‰        *
C*                           OCT”ˆKCHOE B›PA†EHˆE.                    *
C*                    RPOST - TO †E —TO POST , HO REAL*4               *
C*                    K - KO‹ˆ—ECTBO ‹EMEHTOB B POST-‡AˆCˆ           *
C*                    KKK - KO„ BO‡BE„EHˆŸ B CTEEHœ '**'              *
C*                                                                     *
C*    O„POƒPAMM› :  APFTRT , ( SUBERR , COMMON : PRINTN )            *
C*    ------------                                                     *
C*                                                                     *
C*    PˆME—AHˆE :    BXO„HAŸ ˆH”ˆKCHAŸ ‡AˆCœ „O‹†HA ›Tœ B Bˆ„E      *
C*    ----------      OC‹E„OBATE‹œHOCTˆ TEPMOB B F-KO„E.              *
C*                                       ----------------              *
C*                                                                     *
CZZZ********************************************************************
C*   BEGIN DECLARE
       COMMON / PRINTN / NW6
       DIMENSION ZH(7) , PRTY(50) , STEK(50) , ZN(7)
       DIMENSION SIM(1) , POST(1) , RPOST(1) , RSIM(1)
       INTEGER SIM , POST , PRTY , STEK , DPRTY , ZH , ZN
       DATA ZN / '(' , ')' , '+' , '-' , '*' , '/' , ' ' /
C!!!-------------------------------------------------------------------
C!!!-------------------------------------------------------------------
C*   END DECLARE
C*   EPEKO„ˆPOBKA ZH ˆ‡ ”OPMATA 'A' B ”OPMAT 'F'
       CALL APFTRT( 1 , 7 , ZN , ZH )
       DO 1000 I=1,6
 1000  ZH(I) = -ZH(I)
C*   HA—A‹œHOE “C‹OBˆE
C*   I - ˆH„EKC „‹Ÿ SIM-‡AˆCˆ
       I = 1
C*   K - ˆH„EKC „‹Ÿ POST-‡AˆCˆ
       K = 1
C*   J - ˆH„EKC „‹Ÿ CTEKA
       J = 0
C*---------------------------------------------------------------------
       ZH(7) = KKK
C*---------------------------------------------------------------------
C*   OPAOTKA ‡AˆCˆ
 11    CONTINUE
C*   “ HAC ˆ„EHTˆ”ˆKATOP ?
       IF ( SIM(I) .GT.   0 ) GO TO 1
C*   “ HAC —ˆC‹O ?
       IF ( SIM(I) .EQ.   0 ) GO TO 3
C*   “ HAC OTKP›BAž™AŸ CKOKA '(' ?
       IF ( SIM(I) .EQ. ZH(1) ) GO TO 4
C*   “ HAC ‡AKP›BAž˜AŸ CKOKA ')' ?
       IF ( SIM(I) .EQ. ZH(2) ) GO TO 5
C*   ”“HK–ˆŸ ˆ‹ˆ OEPA–ˆŸ ( * , / , - , + )
C*
C*   ˆMEEM '-' ˆ‹ˆ '+'
       IF ( SIM(I) .EQ. ZH(3)  .OR.  SIM(I) .EQ. ZH(4) ) GO TO 21
C*   ˆMEEM '*' ˆ‹ˆ '/'
       IF ( SIM(I) .EQ. ZH(5)  .OR.  SIM(I) .EQ. ZH(6) ) GO TO 22
C*   ˆMEEM BO‡BE„EHˆE B CTEEHœ
       IF ( SIM(I) .EQ. ZH(7) ) GO TO 23
C*   “ HAC ”“HK–ˆŸ ?
       IF ( SIM(I) .LT. -100 ) GO TO 124
C!!!-------------------------------------------------------------------
       WRITE(NW6,100) I
 100   FORMAT(1X,'***** CO‰ B APFPST , HEOO‡HAHH›‰ TEPM N=',I5)
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
 124   J = J + 1
C*   ‡AƒOHŸEM ”“HK–ˆž B CTEK
       STEK(J) = SIM(I)
       PRTY(J) = 5
       GO TO 10
C*---------------------------------------------------------------------
C*   CTEKOB›‰ PˆOPˆTET OEPA–ˆˆ '+' ˆ '-' PABEH 2
 21    DPRTY = 2
       GO TO 25
C*   PˆOPˆTET '*' ˆ '/' PABEH 3
 22    DPRTY = 3
       GO TO 25
C*   PˆOPˆTET '**' PABEH 4
 23    DPRTY = 4
 25    CONTINUE
C*   CTEK “CT ? ( J=0 - „A ; J^=0 - HET )
       IF ( J .NE. 0 ) GO TO 30
C*   .... “CT !
 31    J = J + 1
C*   ˆ˜EM B CTEK OEPA–ˆž ( + , - , / , * , ** )
       STEK(J) = SIM(I)
       PRTY(J) = DPRTY
       GO TO 10
C*   .... HE “CT !
 30    CONTINUE
C*   EC‹ˆ PˆOPˆTET OEPA–ˆˆ  DPRTY  O‹œ˜E PˆOPˆTETA OEPA–ˆˆ
C*   B BEP˜ˆHE CTEKA  PRTY  , TO  GO TO 31
       IF ( PRTY(J) .LT. DPRTY ) GO TO 31
C*   ˆHA—E ”OPMˆP“EM OPATH“ž O‹œCK“ž ‡AˆCœ , T.E.
C*   B›TA‹KˆBAEM OEPA–ˆž ˆ‡ CTEKA
       POST(K) = STEK(J)
       K = K + 1
       J = J - 1
       GO TO 31
C*---------------------------------------------------------------------
C*   ˆMEEM ˆ„EHTˆ”ˆKATOP.EPEˆ˜EM EƒO B OCT”ˆKCH“ž ‡AˆCœ.
 1     POST(K) = SIM(I)
       K = K + 1
       GO TO 10
C*---------------------------------------------------------------------
C*   ˆMEEM —ˆC‹OB“ž KOHCTAHT“.‡AˆC›BAEM EE B  POSTFIX.
 3     POST(K) = SIM(I)
       I = I + 1
       RPOST(K+1) = RSIM(I)
       K = K + 2
       GO TO 10
C*---------------------------------------------------------------------
C*   ˆMEEM '(' . ˆ˜EM EE B CTEK C PˆOPˆTETOM "0".
 4     J = J + 1
       STEK(J) = ZH(1)
       PRTY(J) = 0
       GO TO 10
C*---------------------------------------------------------------------
C*   ˆMEEM ')'.
 5     CONTINUE
C*   B›TA‹KˆBAEM ˆ‡ CTEKA BCE ‡HAKˆ , —E‰ PˆOPˆTET O‹œ˜E ,
C*   —EM PˆOPˆTET ')' (T.E. >1) ˆ ˆ˜EM B  POSTFIX .
C*   '(' ˆ‡ CTEKA B›TA‹KˆBAETCŸ , HO B  POSTFIX HE ˆ˜ETCŸ .
       IF ( PRTY(J) .EQ. 0 ) GO TO 60
       POST(K) = STEK(J)
       K = K + 1
       J = J - 1
       GO TO 5
 60    J = J - 1
       GO TO 10
C*---------------------------------------------------------------------
 10    I = I + 1
C*   ‡AˆCœ ˆC—EPAHA ?
       IF ( I .LE. KSIM ) GO TO 11
C*---------------------------------------------------------------------
C*   CPOC OCTATKA CTEKA B  POSTFIX .
 72    IF ( J .EQ. 0 ) GO TO 71
       POST(K) = STEK(J)
       K = K + 1
       J = J - 1
       GO TO 72
 71    CONTINUE
       K = K - 1
       RETURN
       END
