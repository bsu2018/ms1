       SUBROUTINE  APFPWR ( K , SIM , RSIM , KSIM , M , II )
CXXX****KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*   �O��PO�PAMMA :  APFPWR ( K , SIM , RSIM , KSIM , M , II )         *
C*   ------------                                                      *
C*                                                                     *
C*   HA�HA�EH�E :  B��E�EH�E TEPMA "BO�BE�EH�E B CTE�EH�" � ��AKOBKA   *
C*   ----------    ��� PAC�AKOBKA E�O. ��AKOBAHHA� O�EPA��� "BO�BE�E-  *
C*                 H�E B CTE�EH�" �AH�MAET O�HO C�OBO (INTEGER*4) �    *
C*                 �MEET KO� (-90). PAC�AKOBAHHOE "**" - �BO�HOE       *
C*                 C�OBO, KO�  - CMOTP� �O��PO�PAMM� APFTRT.           *
C*                                                                     *
C*   METO� :       �OC�E�OBATE��HOE CKAH�POBAH�E MACC�BA  SIM  �       *
C*   -----         �O�CK ��AKOBAHHO�O ��� PAC�AKOBAHHO�O �HA�EH�� "**".*
C*                                                                     *
C*   �APAMETP� :   K = 1  -  PAC�AKOBAHHOE -> ��AKOBAHHOE              *
C*   ---------     K = 2  -  ��AKOBAHHOE   -> PAC�AKOBAHHOE            *
C*                 SIM - MACC�B KOTOP�� HEO�XO��MO �POCMOTPET�         *
C*                 RSIM - TO �E �TO � SIM , HO REAL*4                  *
C*                 KSIM - KO���ECTBO ��EMEHTOB B SIM                   *
C*                 M - KO� ��AKOBAHHO�O "**"                           *
C*                 II - KO� C�MBO�A "*"                                *
C*                                                                     *
C*   �O��PO�PAMM� : HET , ( SUBERR , COMMON / PRINTN / NW6 )           *
C*   ------------                                                      *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION SIM(1) , RSIM(1)
       INTEGER SIM
C!!!-------------------------------------------------------------------
       COMMON / PRINT / NW6
C!!!-------------------------------------------------------------------
       I = 1
       GO TO ( 1 , 2 ) , K
C!!!-------------------------------------------------------------------
       WRITE ( NW6 , 10 ) K
 10    FORMAT ( ' ***** C�O� �P� O�PA�EH�� K  APFPWR ',I10 )
       CALL SUBERR ( 1 )
C!!!-------------------------------------------------------------------
C*   ��AKOBKA  **  B O�HO C�OBO
 1     CONTINUE
 4     IF ( SIM(I) .NE. II ) GO TO 3
       IF ( SIM(I+1) .NE. II ) GO TO 3
       SIM(I) = M
       KSIM = KSIM - 1
       I10 = I + 1
       DO 7 J=I10,KSIM
       SIM(J) = SIM(J+1)
 7     RSIM(J) = RSIM(J+1)
 3     I = I + 1
       IF ( I .LE. KSIM ) GO TO 4
       RETURN
C*   PAC�AKOBKA  **  �� C�OBA B �BO�HOE C�OBO
C*
 2     IF ( SIM(I) .NE. M ) GO TO 5
       SIM(I) = II
       KSIM = KSIM + 1
       J = KSIM
 6     SIM(J) = SIM(J-1)
       J = J - 1
       IF ( J .GT. I ) GO TO 6
 5     I = I + 1
       IF ( I .LE. KSIM ) GO TO 2
       RETURN
       END
