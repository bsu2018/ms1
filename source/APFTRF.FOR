       SUBROUTINE APFTRF ( KODFUN , K )
CXXX****KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)*************
C*                                                                     *
C*  �O��PO�PAMMA :  APFTRF ( KODFUN , K )                              *
C*  ------------                                                       *
C*                                                                     *
C*  HA�HA�EH�E :    �EPEKO��POBKA �� C�MBO��HO�O �PE�CTAB�EH��         *
C*  ----------      (A-�OPMAT) BO BH�TPEHHEE �PE�CTAB�EH�E �PO�PAMM�   *
C*                  (F-�OPMAT,INTEGER*4) � �OC�E����A� ��AKOBKA        *
C*                  �MEH ��HK���. �MEHA ��HK��� �A��T� B               *
C*                  DATA - ��OKE �O��PO�PAMM�.                         *
C*                                                                     *
C*  METO� :         �OC�MBO��HA� �EPEKO��POBKA � ��AKOBKA �MEH�.       *
C*  -----                                                              *
C*                                                                     *
C*  �APAMETP� :     KODFUN - MACC�B �EPEKO��POBAHH�X � ��AKOBAHH�X     *
C*  ---------                �MEH ��HK���.                             *
C*                  K - KO���ECTBO ��HK���.                            *
C*                                                                     *
C*  �O��PO�PAMM� :  APFTRT , APFPAK                                    *
C*  ------------                                                       *
C*                                                                     *
CZZZ********************************************************************
C*
       DIMENSION KODFUN(1) , CHAR(4) , IFUN(4,12)
       INTEGER CHAR
       DATA      IFUN / 'L' , 'O' , 'G' , ' ' ,
     *                  'L' , 'G' , '1' , '0' ,
     *                  'E' , 'X' , 'P' , ' ' ,
     *                  'S' , 'Q' , 'R' , 'T' ,
     *                  'S' , 'I' , 'N' , ' ' ,
     *                  'C' , 'O' , 'S' , ' ' ,
     *                  'T' , 'A' , 'N' , ' ' ,
     *                  'C' , 'T' , 'A' , 'N' ,
     *                  'A' , 'S' , 'I' , 'N' ,
     *                  'A' , 'C' , 'O' , 'S' ,
     *                  'A' , 'T' , 'A' , 'N' ,
     *                  'A' , 'B' , 'S' , ' '    /
C*.....................................................................
       K = 12
       DO 1 I=1,K
       CHAR(1) = IFUN(1,I)
       CHAR(2) = IFUN(2,I)
       CHAR(3) = IFUN(3,I)
       CHAR(4) = IFUN(4,I)
       CALL APFTRT ( 1 , 4 , CHAR , CHAR )
       CALL APFPAK ( 1 , KODPAC , CHAR )
 1     KODFUN(I) = KODPAC
       RETURN
       END
