       SUBROUTINE DRRES0(KAR, DAT, MSWALL, TOKN, VALLN )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: DRRES0(KAR, DAT, MSWALL, TOKN, VALLN )
C   ------------
C
C   HA�HA�EH�E:       O�PA�OT��K  #BOCT  NFCHEK
C   ----------        **************************
C
C                      NFCHEK - KAHA� COXPAHEH��;
C
C   METO�:     �P�EM �AP-B ��PEKT�B� � B��OB �/� SVREST;
C   ------     �P� �C�E�HOM BOCT_AHOB�EH�� ��A� "FRESTE = 1"
C
C   �APAMETP�: KAR, DAT - MACC�B� CTPOK� � �AHH�X ��PEKT�B�;
C   ---------  MSWALL, TOKN, VALLN - "TO�KA" PE�EH��;
C
C   �O��PO�PAMM�: 1. PROF1, RSCHEK
C   -------------
C
C   �P�ME�AH�E:  1. "�MO��AH�E" HA HOMEP KAHA�A �� MASPRT(95)
C   ----------       NFCHEK = 10
C   @@@ OT 03.03.87 -> �OP�.
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( DTCHEK, MASPRT(90) )
       EQUIVALENCE( FRESTE, MASPRT(98) )
C                   ********************
       EQUIVALENCE( NFCHEK, MASPRT(95) )
       EQUIVALENCE( NZAPSP, MASPRT(92) )
       EQUIVALENCE(  TN,    MASPRT(29) )
       EQUIVALENCE( FCHEK,  MASPRT(91) )
       EQUIVALENCE( NFACHK, MASPRT(97) )
       EQUIVALENCE( NWTR,   MASPRT(13) )
       EQUIVALENCE( NWTRE,  MASPRT(14) )
       EQUIVALENCE( NRDR,   MASPRT(12) )
       EQUIVALENCE( NSPOOL, MASPRT(18) )
       EQUIVALENCE( NMAK,   MASPRT(89) )
       EQUIVALENCE( NWTRD,  MASPRT(16) )
       EQUIVALENCE( NFILER, MASPRT(83) )
       INTEGER FCHEK, FRESTE
       DIMENSION KAR(*), DAT(*), MSWALL(*), TOKN(*), VALLN(*)
C----------------------------------------------------------------------
       NF = NFCHEK
       KKK = IFIX( DAT(1) )
       IF ( KKK .EQ. 0 ) GO TO 100
C-->  HOB�� �A��..
       NF = IFIX( DAT(2) )
C-->  KOHTPO�� �A��A ....
       IF ( NF .EQ. NWTR  .OR.
     =      NF .EQ. NWTRE   .OR.
     =      NF .EQ. NRDR    .OR.
     =      NF .EQ. NSPOOL  .OR.
     =      NF .EQ. NMAK    .OR.
     =      NF .EQ. NWTRD   .OR.
     =      NF .EQ. NFILER  )
     =                                 NF = NFCHEK
C----------------------------------------------------------------------
C@@@@@ NFCHEK = NF
100    CONTINUE
       WRITE(NWTR, 500)         NF
500    FORMAT(/' *** #BOCT �ਭ��:==> ����� 䠩�� ���⠭������� ', I3,
     = ' ***'/)
C@@@@@@NFOLD = NFCHEK
       CALL RSCHEK( NF,     TN, MSWALL, TOKN, VALLN, IRC )
C @@@
       IF ( IRC .NE. 0 ) GO TO 666
CC-->  BOCTAHOB��� --->
       FRESTE = 1
       CALL PPNCOM( 3, NWTR )
       WRITE(NWTR,501) TN, NF,     NZAPSP
501    FORMAT(/' *** BOCTAHOB�EHO:=> TN=',G12.5,' CEK., KAHA� # ', I2,
     =    ', �A��C� B NSPOOL ', I6,' ***'/)
C----------------------------------------------------------------------
       RETURN
666    CONTINUE
C-->  HE CMO���...
       FRESTE = 0
       WRITE(NWTR, 505)  NF
       WRITE(NWTRE, 505)  NF
505    FORMAT(/' *** BOCTAHOB�EH�E C KAHA�A #',I3,' HE BO�MO�HO ***'/)
       RETURN
       END
