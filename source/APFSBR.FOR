       SUBROUTINE APFSBR ( L , SIM , KSIM , KODFUN , K )
CXXX**-1KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*  �O��PO�PAMMA :  APFSBR ( L , SIM , KSIM , KODFUN , K )             *
C*  ------------                                                       *
C*                                                                     *
C*  HA�HA�EH�E :    OT�CKAH�E ��EHT���KATOPA �MEH� ��HK��� B           *
C*  ----------      �H��KCHO� �A��C� � �CTAHOB�EH�E EM� B              *
C*                  COOTBETCTB�E �P��HAKA ��HK���.                     *
C*                                                                     *
C*  METO� :         �O�CK COOTBETCTB�� B TA����E �MEH ��HK���  KODFUN  *
C*  -----           EC�� TAKOE COOTBETCTB�E ECT�, TO C�E�OBATE��HO.    *
C*                  �MEEM ��HK���, A HE ��EHT���KATOP.                 *
C*                                                                     *
C*  �APAMETP� :     L , L = 1  -  KO�  ->  OTP��ATE��H�� K O � ,       *
C*  ---------           L = 2  -  OTP��ATE��H�� K O �  ->  KO� .       *
C*                  SIM - MACC�B TEPMOB B �H��KCHOM B��E .             *
C*                  KSIM - KO���ECTBO ��EMEHTOB B SIM .                *
C*                  KODFUN - MACC�B �MEH ��HK��� .                     *
C*                  K - ��C�O ��HK��� B KODFUN .                       *
C*                                                                     *
C*  �O��PO�PAMM� :  HET ; ( SUBERR(1) , COMMON : PRINTN )              *
C*  ------------                                                       *
C*                                                                     *
CZZZ********************************************************************
C*                                                                     *
       INTEGER SIM
       DIMENSION SIM(1) , KODFUN(1)
C-----------------------------------------------
       COMMON / PRINTN / NW6
C!!!-------------------------------------------------------------------
       GO TO ( 1 , 2 ) , L
C!!!-------------------------------------------------------------------
       WRITE(NW6,10) L
 10    FORMAT(1X,'***** C�O� �P� O�PA�EH�� K APFSBR , L=',I10)
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
C*  �EPEKO��POBKA �� �POCTO�O KO�A B OTP��ATE��H�� KO�
 1     I = 1
C*  EC�� �HAK, TO O��CKAEM E�O
 4     IF ( SIM(I) .LT. 0 ) GO TO 11
C*  EC�� ��EHT���KATOP ��� ��HK��� , TO ...
       IF ( SIM(I) .GT. 0 ) GO TO 12
C*  �MEEM ��C�O. O��CKAEM E�O.
       I = I + 1
       GO TO 11
 12    CONTINUE
       DO 13 J=1,K
       IF ( SIM(I) .EQ. KODFUN(J) ) GO TO 23
       GO TO 13
C*  �EPEKO��P�EM ��HK���
 23    SIM(I) = -SIM(I)
       GO TO 11
 13    CONTINUE
 11    I = I + 1
       IF ( I .LE. KSIM ) GO TO 4
       RETURN
C*.....................................................................
C*  ��  OTP��ATE��H�� K O � A  B �POCTO� KO�
 2     CONTINUE
       I = 1
 15    IF ( SIM(I) .GE. -100 ) GO TO 14
       SIM(I) = -SIM(I)
 14    I = I + 1
       IF ( I .LE. KSIM ) GO TO 15
       RETURN
       END
