       SUBROUTINE APFRUN ( POST , RPOST , KPOST , INDEX ,
     *                   DATAR , KINDEX , RESULT , KODFUN , KF , KKK )
CXXX**-1KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)*************
C*                                                                     *
C*  O„POƒPAMMA :  APFRUN ( POST , RPOST , KPOST , INDEX ,            *
C*  ------------          DATAR , KINDEX , RESULT , KODFUN , KF , KKK )*
C*                                                                     *
C*  HA‡HA—EHˆE :    B›—ˆC‹EHˆE APˆ”METˆ—ECKOƒO B›PA†EHˆŸ B OCT”ˆKCHO‰ *
C*  ----------      ”OPME. B›PA†EHˆE COCTOˆT ˆ‡ OC‹E„OBATE‹œHOCTˆ     *
C*                  S(1),S(2),...,S(N),N>=1, ƒ„E S(I) - ‹ˆO KOHCTOHTA,*
C*                  ‹ˆO ˆ„EHTˆ”ˆKATOP, ‹ˆO „B“XMECTHAŸ APˆ”METˆ—EC-  *
C*                  KAŸ OEPA–ˆŸ ( + , - , * , / , ** ), ‹ˆO ”“HK–ˆŸ. *
C*                                                                     *
C*  METO„ :         A‹ƒOPˆTM PAOTAET —ˆTAŸ OCT”ˆKCHOE B›PA†EHˆE      *
C*  -----           C‹EBA HAPABO. EC‹ˆ C—ˆTAHHOE B›PA†EHˆE S(I) -     *
C*                  ˆ„EHTˆ”ˆKATOP ˆ‹ˆ —ˆC‹O, TO OME™AEM EƒO B CTEK.   *
C*                  ˆHA—E : O–EHˆBAEM O„B›PA†EHˆE ˆ B›O‹HŸEM OEPA-  *
C*                  –ˆŸ, COOTBETCTB“ž™AŸ EM“. EC‹ˆ ˆMEEM „B“XMECTH›E   *
C*                  APˆ”METˆ—ECKˆE OEPA–ˆˆ ( + , - , * , / , ** ), TO *
C*                  B OEPA–ˆˆ “—ACTB“žT „BA BEPXHˆX ‹EMEHTA CTEKA.   *
C*                  Tˆ „BA ‹EMEHTA “„A‹ŸžTCŸ ˆ‡ CTEKA, A PE‡“‹œTAT   *
C*                  OME™AETCŸ HA BEPX CTEKA. EC‹ˆ ˆMEEM ”“HK–ˆž       *
C*                  (O„HOMECTH“ž), TO „E‰CTBˆE POˆ‡BO„ˆM HA„ OC‹E„-  *
C*                  HˆM ‹EMEHTOM CTEKA, T“„A †E ˆ˜EM PE‡“‹œTAT.      *
C*                  OC‹E OKOH—AHˆŸ B›—ˆC‹EHˆ‰ PE‡“‹œTAT “„ET HAXO-   *
C*                  „ˆTCŸ B EPBOM (CAMOM Hˆ†HEM) ‹EMEHTE CTEKA.      *
C*                                                                     *
C*  APAMETP› :     POST - MACCˆB OCT”ˆKCHO‰ ‡AˆCˆ                   *
C*  ---------       RPOST - TO †E —TO ˆ POST, HO REAL*4                *
C*                  KPOST - KO‹ˆ—ECTBO ‹EMEHTOB B OCT”ˆKCHO‰ ‡AˆCˆ  *
C*                  INDEX - MACCˆB ˆMEH ˆ„EHTˆ”ˆKATOPOB                *
C*                  DATAR - MACCˆB —ˆC‹EHH›X ‡HA—EHˆ‰ ˆ„EHTˆ”ˆKATOPOB  *
C*                  KINDEX - PA‡MEP MACCˆBOB INDEX ˆ DATAR             *
C*                  RESULT - PE‡“‹œTAT B›—ˆC‹EHˆŸ POST-‡AˆCˆ          *
C*                  KODFUN - MACCˆB ˆMEH ”“HK–ˆ‰                       *
C*                  KF - KO‹ˆ—ECTBO ”“HK–ˆ‰                            *
C*                  KKK - KO„ BO‡BE„EHˆŸ B CTEEHœ '**'                *
C*                                                                     *
C*  O„POƒPAMM› :  APFSEE , APFFUN , ( SUBERR(1) , COMMON : PRINTN )  *
C*  ------------                                                       *
C*                                                                     *
C*  PˆME—AHˆE :  1) A‹ƒOPˆTM PE„O‹AƒAET, —TO BXO„HAŸ OC‹E„OBATE‹œ- *
C*  ----------       HOCTœ ŸB‹ŸETCŸ PABˆ‹œH›M POST-B›PA†EHˆEM.        *
C*                2) A‹ƒOPˆTM HˆKOƒ„A HE ˆHˆ–ˆA‹ˆ‡ˆP“ET CTEK H“‹ŸMˆ    *
C*                   ˆ HE ‡AO‹HŸET CTEK H“‹ŸMˆ OC‹E TOƒO, KAK        *
C*                   ‡HA—EHˆŸ ›‹ˆ ˆCO‹œ‡OBAH›. TO CTOˆ‹O › „E‹ATœ  *
C*                   ˆ‡ CTETˆ—ECKˆX COOPA†EHˆ‰, HO HE OŸ‡ATE‹œHO.   *
C*                3) ‡HA—EHˆE ”“HK–ˆ‰ B›—ˆC‹ŸžTCŸ Pˆ OPA™EHˆˆ K      *
C*                   O„POƒPAMME  APFFUN.                             *
C*                4) —ˆC‹OB›E ‡HA—EHˆŸ ˆ„EHTˆ”ˆKATOPOB OPE„E‹ŸžTCŸ    *
C*                   Pˆ OPA™EHˆˆ K O„POƒPAMME  APFSEE. Pˆ TOM    *
C*                   ˆ„EHTˆ”ˆKATOP› „O‹†H› ›Tœ OTCOPTˆPOBAH› O       *
C*                   BO‡PACTAHˆž.                                      *
C*                                                                     *
CZZZ********************************************************************
C*
       COMMON / PRINTN / NW6
C!!--------------------------------------------------------------------
       DIMENSION STEK(51) , ZH(7) , ZN(7)
       DIMENSION POST(1) , DATAR(1) , INDEX(1) , RPOST(1) , KODFUN(1)
       INTEGER POST , ZH  , ZN
       DATA ZN / '*' , '/' , '+' , '-' , ' ',' ',' ' /
C!!!-------------------------------------------------------------------
C*  EPEKO„ˆPOBKA  ZH  ˆ‡ ”OPMATM "A" B ”OPMAT "F"
       CALL APFTRT ( 1 , 4 , ZN , ZH )
       DO 100 I=1,4
 100   ZH(I) = -ZH(I)
C*  ZH(5) = KKK  -  KO„ "BO‡BE„EHˆŸ B CTEEHœ"
       ZH(5) = KKK
C*----------------------------------------------------------------------
C*  HA—A‹œH›E “C‹OBˆŸ
C*  I - ˆH„EKC POST-‡AˆCˆ
       I = 1
C*  J - ˆH„EKC CTEKA
       J = 0
C*---------------------------------------------------------------------
C*  OPAOTKA  POST-‡AˆCˆ
 11    CONTINUE
C*  “ HAC ˆ„EHTˆ”ˆKATOP ?
       IF ( POST(I) .GT. 0 ) GO TO 1
C*  “ HAC —ˆC‹O ?
       IF ( POST(I) .EQ. 0   ) GO TO 2
C*  “ HAC ‡HAK OEPA–ˆˆ ( + , - , * , / , ** ) ?
       IF ( POST(I) .LT.   0  .AND.  POST(I) .GT. -100 ) GO TO 3
C*  “ HAC ”“HK–ˆŸ ?
       IPS = POST(I)
       CALL APFAAA ( KODFUN , KF , IPS , NOMER )
       RSTEK = STEK(J)
C*  B›‡›BAEM APFFUN - O„POƒPAMM“ B›—ˆC‹EHˆŸ ‡HA—EHˆŸ ”“HK–ˆˆ.
       CALL APFFUN ( RSTEK , RSTEK , NOMER )
       STEK(J) = RSTEK
       GO TO 10
C*---------------------------------------------------------------------
C*  ˆMEEM ˆ„EHTˆ”ˆKATOP
 1     IPOST = POST(I)
C*  B›‡OB O„POƒPAMM›, OPE„E‹Ÿž™E‰ —EC‹EHHOE ‡HA—EHˆE ˆ„HTˆ”ˆKATOPA.
       CALL APFSEE ( IPOST , RSTEK , INDEX , DATAR , KINDEX )
       J = J + 1
       STEK(J) = RSTEK
       GO TO 10
C*---------------------------------------------------------------------
C*  ˆMEEM —ˆC‹O. CPA‡“ †E ˆ˜EM EƒO B CTEK.
 2     J = J + 1
       I = I + 1
       STEK(J) = RPOST(I)
       GO TO 10
C*---------------------------------------------------------------------
C*  ˆMEEM ‡HAK OEPA–ˆˆ ( * , / , + , - , ** ). KAKO‰ ?
 3     CONTINUE
C*  “MHO†EHˆE ?
       IF ( POST(I) .EQ. ZH(1) ) GO TO 4
C*  „E‹EHˆE ?
       IF ( POST(I) .EQ. ZH(2) ) GO TO 5
C*  C‹O†EHˆE ?
       IF ( POST(I) .EQ. ZH(3) ) GO TO 6
C*  B›—ˆTAHˆE ?
       IF ( POST(I) .EQ. ZH(4) ) GO TO 7
C*.....................................................................
C*  BO‡BE„EHˆE B CTEEHœ ?
       IF ( POST(I) .EQ. ZH(5) ) GO TO 888
C!!!-------------------------------------------------------------------
       WRITE(NW6,1000)
 1000  FORMAT(1X,'***** CO‰ B PAOTE APFRUN,HEOO‡HAHH›‰ ‡HAK.')
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
 888   J = J - 1
       STEK(J) = STEK(J) ** STEK(J+1)
       GO TO 10
C*.....................................................................
C*  ˆMEEM “MHO†EHˆE.
 4     J = J - 1
       STEK(J) = STEK(J) * STEK(J+1)
       GO TO 10
C*.....................................................................
C*  ˆMEEM „E‹EHˆE.
 5     J = J - 1
       STEK(J) = STEK(J) / STEK(J+1)
       GO TO 10
C*.....................................................................
C*  ˆMEEM C‹O†EHˆE.
 6     J = J - 1
       STEK(J) = STEK(J) + STEK(J+1)
       GO TO 10
C*.....................................................................
C*  ˆMMEM B›—ˆTAHˆE.
 7     J = J - 1
       STEK(J) = STEK(J) - STEK(J+1)
       GO TO 10
C*---------------------------------------------------------------------
 10    CONTINUE
       I = I + 1
C*  POST-‡AˆCœ ˆC—EPAHA ?
       IF ( I .LE. KPOST ) GO TO 11
C*  COXPAHŸEM PE‡“‹œTAT ˆ BO‡BPA™AEMCŸ.
       RESULT = STEK(1)
       RETURN
       END
