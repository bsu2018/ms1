      SUBROUTINE APFPST( SIM , RSIM , KSIM , POST , RPOST , K , KKK )
CXXX****KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*    �O��PO�PAMMA :  APFPST ( SIM , RSIM , KSIM , POST , RPOST , K    *
C*    ------------             KKK )                                   *
C*                                                                     *
C*    HA�HA�EH�E :    �EPEBO� �H��KCHO�O B�PA�EH�� S(1),S(2),...,S(N), *
C*    ----------      N>=1, B �OCT��KCH�� �OPM�. S(I) - ��EHT���KATOP, *
C*                    ��C�O, '(' ��� ')', �HAK� O�EPA��� ( + , - , / , *
C*                    *  ���  ** ).                                    *
C*                                                                     *
C*    METO� :         OCHOBAH HA �C�O���OBAH�� CTEKA C �P�OP�TETAM�.   *
C*    -----           A��OP�TM PA�OTAET, ��TA� C�MBO�� �H��KCHO�O      *
C*                    B�PA�EH�� C�EBA HA�PABO. BCE ��EHT���KATOP�      *
C*                    ( ��� ��C�A ) �OCT��A�T HA B�XO� �O MEPE �TEH��; *
C*                    �HAK� O�EPA��� �OME�A�TC� B CTEK O�EPA���. EC��  *
C*                    �P�OP�TET BXO�HO�O �HAKA PABEH 0 ��� �O���E      *
C*                    �P�OP�TETA �HAKA, HAXO���E�OC� B BEP��HE CTEKA,  *
C*                    TO HOB�� �HAK �O�AB��ETC� B BEP��H� CTEKA.       *
C*                    B �POT�BHOM C���AE �� CTEKA "B�TA�K�BAETC�" �    *
C*                    �EPE��C�BAETC� B B�XO�H�� CTPOK� �HAK,           *
C*                    HAXO�����C� B BEP��HE, A TAK�E C�E�����E �A      *
C*                    H�M �HAK� C �P�OP�TETAM�, �O����M� ��� PABH�M�   *
C*                    �P�OP�TET� BXO�HO�O �HAKA. �OC�E �TO�O BXO�HO�   *
C*                    �HAK �O�AB��ETC� K BEP��HE CTEKA. '(' "B�TA�-    *
C*                    K�BAETC� TO��KO ')', HO HA B�XO� HE �EPE�AETC�.  *
C*                                                                     *
C*        TA����A �P�OP�TETOB �HAKOB O�EPA��� � O�PAH���TE�E�          *
C*                 -------------------------------------               *
C*                   C � M O �   !   � P � O P � T E T                 *
C*                 -------------------------------------               *
C*                       (       !           0                         *
C*                       )       !           1                         *
C*                     +   -     !           2                         *
C*                     *   /     !           3                         *
C*                       **      !           4                         *
C*                    ��HK���    !           5                         *
C*                 -------------------------------------               *
C*                                                                     *
C*    �APAMETP� :     SIM  - MACC�B TEPMOB B  F-KO�E (INTEGER*4) ,     *
C*    ---------              �A��CAHH�X B �H��KCHOM B��E.              *
C*                    RSIM - TO �E �TO � SIM , HO REAL*4               *
C*                    KSIM - KO���ECTBO �AH�T�X �O����� B MACC�BE      *
C*                           SIM ( <= 100 ).                           *
C*                    POST - MACC�B TEPMOB, �PE�CTAB����� CO�O�        *
C*                           �OCT��KCHOE B�PA�EH�E.                    *
C*                    RPOST - TO �E �TO POST , HO REAL*4               *
C*                    K - KO���ECTBO ��EMEHTOB B POST-�A��C�           *
C*                    KKK - KO� BO�BE�EH�� B CTE�EH� '**'              *
C*                                                                     *
C*    �O��PO�PAMM� :  APFTRT , ( SUBERR , COMMON : PRINTN )            *
C*    ------------                                                     *
C*                                                                     *
C*    �P�ME�AH�E :    BXO�HA� �H��KCHA� �A��C� �O��HA ��T� B B��E      *
C*    ----------      �OC�E�OBATE��HOCT� TEPMOB B F-KO�E.              *
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
C*   �EPEKO��POBKA ZH �� �OPMATA 'A' B �OPMAT 'F'
       CALL APFTRT( 1 , 7 , ZN , ZH )
       DO 1000 I=1,6
 1000  ZH(I) = -ZH(I)
C*   HA�A��HOE �C�OB�E
C*   I - �H�EKC ��� SIM-�A��C�
       I = 1
C*   K - �H�EKC ��� POST-�A��C�
       K = 1
C*   J - �H�EKC ��� CTEKA
       J = 0
C*---------------------------------------------------------------------
       ZH(7) = KKK
C*---------------------------------------------------------------------
C*   O�PA�OTKA �A��C�
 11    CONTINUE
C*   � HAC ��EHT���KATOP ?
       IF ( SIM(I) .GT.   0 ) GO TO 1
C*   � HAC ��C�O ?
       IF ( SIM(I) .EQ.   0 ) GO TO 3
C*   � HAC OTKP�BA��A� CKO�KA '(' ?
       IF ( SIM(I) .EQ. ZH(1) ) GO TO 4
C*   � HAC �AKP�BA��A� CKO�KA ')' ?
       IF ( SIM(I) .EQ. ZH(2) ) GO TO 5
C*   ��HK��� ��� O�EPA��� ( * , / , - , + )
C*
C*   �MEEM '-' ��� '+'
       IF ( SIM(I) .EQ. ZH(3)  .OR.  SIM(I) .EQ. ZH(4) ) GO TO 21
C*   �MEEM '*' ��� '/'
       IF ( SIM(I) .EQ. ZH(5)  .OR.  SIM(I) .EQ. ZH(6) ) GO TO 22
C*   �MEEM BO�BE�EH�E B CTE�EH�
       IF ( SIM(I) .EQ. ZH(7) ) GO TO 23
C*   � HAC ��HK��� ?
       IF ( SIM(I) .LT. -100 ) GO TO 124
C!!!-------------------------------------------------------------------
       WRITE(NW6,100) I
 100   FORMAT(1X,'***** C�O� B APFPST , HEO�O�HAHH�� TEPM N=',I5)
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
 124   J = J + 1
C*   �A�OH�EM ��HK��� B CTEK
       STEK(J) = SIM(I)
       PRTY(J) = 5
       GO TO 10
C*---------------------------------------------------------------------
C*   CTEKOB�� �P�OP�TET O�EPA��� '+' � '-' PABEH 2
 21    DPRTY = 2
       GO TO 25
C*   �P�OP�TET '*' � '/' PABEH 3
 22    DPRTY = 3
       GO TO 25
C*   �P�OP�TET '**' PABEH 4
 23    DPRTY = 4
 25    CONTINUE
C*   CTEK ��CT ? ( J=0 - �A ; J^=0 - HET )
       IF ( J .NE. 0 ) GO TO 30
C*   .... ��CT !
 31    J = J + 1
C*   ���EM B CTEK O�EPA��� ( + , - , / , * , ** )
       STEK(J) = SIM(I)
       PRTY(J) = DPRTY
       GO TO 10
C*   .... HE ��CT !
 30    CONTINUE
C*   EC�� �P�OP�TET O�EPA���  DPRTY  �O���E �P�OP�TETA O�EPA���
C*   B BEP��HE CTEKA  PRTY  , TO  GO TO 31
       IF ( PRTY(J) .LT. DPRTY ) GO TO 31
C*   �HA�E �OPM�P�EM O�PATH�� �O��CK�� �A��C� , T.E.
C*   B�TA�K�BAEM O�EPA��� �� CTEKA
       POST(K) = STEK(J)
       K = K + 1
       J = J - 1
       GO TO 31
C*---------------------------------------------------------------------
C*   �MEEM ��EHT���KATOP.�EPE���EM E�O B �OCT��KCH�� �A��C�.
 1     POST(K) = SIM(I)
       K = K + 1
       GO TO 10
C*---------------------------------------------------------------------
C*   �MEEM ��C�OB�� KOHCTAHT�.�A��C�BAEM EE B  POSTFIX.
 3     POST(K) = SIM(I)
       I = I + 1
       RPOST(K+1) = RSIM(I)
       K = K + 2
       GO TO 10
C*---------------------------------------------------------------------
C*   �MEEM '(' . ���EM EE B CTEK C �P�OP�TETOM "0".
 4     J = J + 1
       STEK(J) = ZH(1)
       PRTY(J) = 0
       GO TO 10
C*---------------------------------------------------------------------
C*   �MEEM ')'.
 5     CONTINUE
C*   B�TA�K�BAEM �� CTEKA BCE �HAK� , �E� �P�OP�TET �O���E ,
C*   �EM �P�OP�TET ')' (T.E. >1) � ���EM B  POSTFIX .
C*   '(' �� CTEKA B�TA�K�BAETC� , HO B  POSTFIX HE ���ETC� .
       IF ( PRTY(J) .EQ. 0 ) GO TO 60
       POST(K) = STEK(J)
       K = K + 1
       J = J - 1
       GO TO 5
 60    J = J - 1
       GO TO 10
C*---------------------------------------------------------------------
 10    I = I + 1
C*   �A��C� �C�EP�AHA ?
       IF ( I .LE. KSIM ) GO TO 11
C*---------------------------------------------------------------------
C*   C�POC OCTATKA CTEKA B  POSTFIX .
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