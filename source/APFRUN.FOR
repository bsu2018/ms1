       SUBROUTINE APFRUN ( POST , RPOST , KPOST , INDEX ,
     *                   DATAR , KINDEX , RESULT , KODFUN , KF , KKK )
CXXX**-1KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*  �O��PO�PAMMA :  APFRUN ( POST , RPOST , KPOST , INDEX ,            *
C*  ------------          DATAR , KINDEX , RESULT , KODFUN , KF , KKK )*
C*                                                                     *
C*  HA�HA�EH�E :    B���C�EH�E AP��MET��ECKO�O B�PA�EH�� B �OCT��KCHO� *
C*  ----------      �OPME. B�PA�EH�E COCTO�T �� �OC�E�OBATE��HOCT�     *
C*                  S(1),S(2),...,S(N),N>=1, ��E S(I) - ���O KOHCTOHTA,*
C*                  ���O ��EHT���KATOP, ���O �B�XMECTHA� AP��MET��EC-  *
C*                  KA� O�EPA��� ( + , - , * , / , ** ), ���O ��HK���. *
C*                                                                     *
C*  METO� :         A��OP�TM PA�OTAET ��TA� �OCT��KCHOE B�PA�EH�E      *
C*  -----           C�EBA HA�PABO. EC�� C��TAHHOE B�PA�EH�E S(I) -     *
C*                  ��EHT���KATOP ��� ��C�O, TO �OME�AEM E�O B CTEK.   *
C*                  �HA�E : O�EH�BAEM �O�B�PA�EH�E � B��O�H�EM O�EPA-  *
C*                  ���, COOTBETCTB���A� EM�. EC�� �MEEM �B�XMECTH�E   *
C*                  AP��MET��ECK�E O�EPA��� ( + , - , * , / , ** ), TO *
C*                  B O�EPA��� ��ACTB��T �BA BEPXH�X ��EMEHTA CTEKA.   *
C*                  �T� �BA ��EMEHTA ��A���TC� �� CTEKA, A PE����TAT   *
C*                  �OME�AETC� HA BEPX CTEKA. EC�� �MEEM ��HK���       *
C*                  (O�HOMECTH��), TO �E�CTB�E �PO��BO��M HA� �OC�E�-  *
C*                  H�M ��EMEHTOM CTEKA, T��A �E ���EM PE����TAT.      *
C*                  �OC�E OKOH�AH�� B���C�EH�� PE����TAT ���ET HAXO-   *
C*                  ��TC� B �EPBOM (CAMOM H��HEM) ��EMEHTE CTEKA.      *
C*                                                                     *
C*  �APAMETP� :     POST - MACC�B �OCT��KCHO� �A��C�                   *
C*  ---------       RPOST - TO �E �TO � POST, HO REAL*4                *
C*                  KPOST - KO���ECTBO ��EMEHTOB B �OCT��KCHO� �A��C�  *
C*                  INDEX - MACC�B �MEH ��EHT���KATOPOB                *
C*                  DATAR - MACC�B ��C�EHH�X �HA�EH�� ��EHT���KATOPOB  *
C*                  KINDEX - PA�MEP MACC�BOB INDEX � DATAR             *
C*                  RESULT - PE����TAT B���C�EH�� POST-�A��C�          *
C*                  KODFUN - MACC�B �MEH ��HK���                       *
C*                  KF - KO���ECTBO ��HK���                            *
C*                  KKK - KO� BO�BE�EH�� B CTE�EH� '**'                *
C*                                                                     *
C*  �O��PO�PAMM� :  APFSEE , APFFUN , ( SUBERR(1) , COMMON : PRINTN )  *
C*  ------------                                                       *
C*                                                                     *
C*  �P�ME�AH�E :  1) A��OP�TM �PE��O�A�AET, �TO BXO�HA� �OC�E�OBATE��- *
C*  ----------       HOCT� �B��ETC� �PAB���H�M POST-B�PA�EH�EM.        *
C*                2) A��OP�TM H�KO��A HE �H���A����P�ET CTEK H���M�    *
C*                   � HE �A�O�H�ET CTEK H���M� �OC�E TO�O, KAK        *
C*                   �HA�EH�� ���� �C�O���OBAH�. �TO CTO��O �� �E�AT�  *
C*                   �� �CTET��ECK�X COO�PA�EH��, HO HE O���ATE��HO.   *
C*                3) �HA�EH�E ��HK��� B���C���TC� �P� O�PA�EH�� K      *
C*                   �O��PO�PAMME  APFFUN.                             *
C*                4) ��C�OB�E �HA�EH�� ��EHT���KATOPOB O�PE�E���TC�    *
C*                   �P� O�PA�EH�� K �O��PO�PAMME  APFSEE. �P� �TOM    *
C*                   ��EHT���KATOP� �O��H� ��T� OTCOPT�POBAH� �O       *
C*                   BO�PACTAH��.                                      *
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
C*  �EPEKO��POBKA  ZH  �� �OPMATM "A" B �OPMAT "F"
       CALL APFTRT ( 1 , 4 , ZN , ZH )
       DO 100 I=1,4
 100   ZH(I) = -ZH(I)
C*  ZH(5) = KKK  -  KO� "BO�BE�EH�� B CTE�EH�"
       ZH(5) = KKK
C*----------------------------------------------------------------------
C*  HA�A��H�E �C�OB��
C*  I - �H�EKC POST-�A��C�
       I = 1
C*  J - �H�EKC CTEKA
       J = 0
C*---------------------------------------------------------------------
C*  O�PA�OTKA  POST-�A��C�
 11    CONTINUE
C*  � HAC ��EHT���KATOP ?
       IF ( POST(I) .GT. 0 ) GO TO 1
C*  � HAC ��C�O ?
       IF ( POST(I) .EQ. 0   ) GO TO 2
C*  � HAC �HAK O�EPA��� ( + , - , * , / , ** ) ?
       IF ( POST(I) .LT.   0  .AND.  POST(I) .GT. -100 ) GO TO 3
C*  � HAC ��HK��� ?
       IPS = POST(I)
       CALL APFAAA ( KODFUN , KF , IPS , NOMER )
       RSTEK = STEK(J)
C*  B���BAEM APFFUN - �O��PO�PAMM� B���C�EH�� �HA�EH�� ��HK���.
       CALL APFFUN ( RSTEK , RSTEK , NOMER )
       STEK(J) = RSTEK
       GO TO 10
C*---------------------------------------------------------------------
C*  �MEEM ��EHT���KATOP
 1     IPOST = POST(I)
C*  B��OB �O��PO�PAMM�, O�PE�E����E� �EC�EHHOE �HA�EH�E ��HT���KATOPA.
       CALL APFSEE ( IPOST , RSTEK , INDEX , DATAR , KINDEX )
       J = J + 1
       STEK(J) = RSTEK
       GO TO 10
C*---------------------------------------------------------------------
C*  �MEEM ��C�O. CPA�� �E ���EM E�O B CTEK.
 2     J = J + 1
       I = I + 1
       STEK(J) = RPOST(I)
       GO TO 10
C*---------------------------------------------------------------------
C*  �MEEM �HAK O�EPA��� ( * , / , + , - , ** ). KAKO� ?
 3     CONTINUE
C*  �MHO�EH�E ?
       IF ( POST(I) .EQ. ZH(1) ) GO TO 4
C*  �E�EH�E ?
       IF ( POST(I) .EQ. ZH(2) ) GO TO 5
C*  C�O�EH�E ?
       IF ( POST(I) .EQ. ZH(3) ) GO TO 6
C*  B���TAH�E ?
       IF ( POST(I) .EQ. ZH(4) ) GO TO 7
C*.....................................................................
C*  BO�BE�EH�E B CTE�EH� ?
       IF ( POST(I) .EQ. ZH(5) ) GO TO 888
C!!!-------------------------------------------------------------------
       WRITE(NW6,1000)
 1000  FORMAT(1X,'***** C�O� B PA�OTE APFRUN,HEO�O�HAHH�� �HAK.')
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
 888   J = J - 1
       STEK(J) = STEK(J) ** STEK(J+1)
       GO TO 10
C*.....................................................................
C*  �MEEM �MHO�EH�E.
 4     J = J - 1
       STEK(J) = STEK(J) * STEK(J+1)
       GO TO 10
C*.....................................................................
C*  �MEEM �E�EH�E.
 5     J = J - 1
       STEK(J) = STEK(J) / STEK(J+1)
       GO TO 10
C*.....................................................................
C*  �MEEM C�O�EH�E.
 6     J = J - 1
       STEK(J) = STEK(J) + STEK(J+1)
       GO TO 10
C*.....................................................................
C*  �MMEM B���TAH�E.
 7     J = J - 1
       STEK(J) = STEK(J) - STEK(J+1)
       GO TO 10
C*---------------------------------------------------------------------
 10    CONTINUE
       I = I + 1
C*  POST-�A��C� �C�EP�AHA ?
       IF ( I .LE. KPOST ) GO TO 11
C*  COXPAH�EM PE����TAT � BO�BPA�AEMC�.
       RESULT = STEK(1)
       RETURN
       END
