       SUBROUTINE APFAAA ( KODFUN , KF , IFUN , NOMER )
CXXX****KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*  �O��PO�PAMMA :  APFAAA ( KODFUN , KF , IFUN , NOMER )              *
C*  ------------                                                       *
C*                                                                     *
C*  HA�HA�EH�E :    O�PE�E�EH�E �OP��KOBO�O HOMEPA �MEH� ��HK��� B     *
C*  ----------      TA����E �MEH ��HK���.                              *
C*                                                                     *
C*  METO� :         �OC�E�OBATE��H�� �POCMOTP TA����� �MEH ��HK���.    *
C*  -----                                                              *
C*                                                                     *
C*  �APAMETP� :     KODFUN - TA����A PA�PE�EHH�X ��HK���.              *
C*  ---------       KF - ��C�O ��HK���.                                *
C*                  IFUN - �M� O�PE�E��EMO� ��HK���.                   *
C*                  HOMEP - EE HOMEP B MACC�BE KODFUN.                 *
C*                                                                     *
C*  �O�PO�PAMM� :   HET ; ( SUBERR , COMMON : PRINTN )                 *
C*  -----------                                                        *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION KODFUN(1)
C!!!-------------------------------------------------------------------
       COMMON / PRINTN / NW6
C!!!-------------------------------------------------------------------
       DO 1 I=1,KF
       IKOD = -KODFUN(I)
       IF ( IFUN .NE. IKOD ) GO TO 1
       NOMER = I
       RETURN
 1     CONTINUE
C!!!-------------------------------------------------------------------
       WRITE(NW6,10)
 10    FORMAT(1X,'***** C�O� B APFFFF . TAKO� ��HK��� HET .')
       CALL SUBERR(1)
C!!!-------------------------------------------------------------------
       RETURN
       END
