       SUBROUTINE DRSAV0(KAR, DAT, MSWALL, TOKN, VALLN )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: DRSAV0(KAR, DAT, MSWALL, TOKN, VALLN )
C   ------------
C
C   HA�HA�EH�E:       O�PA�OT��K  #COXP  NFCHEK
C   ----------        **************************
C
C                      NFCHEK - KAHA� COXPAHEH��;
C
C   METO�:     �P�EM �AP-B ��PEKT�B� � B��OB �/� SVCHEK;
C   ------
C
C   �APAMETP�: KAR, DAT - MACC�B� CTPOK� � �AHH�X ��PEKT�B�;
C   ---------  MSWALL, TOKN, VALLN - "TO�KA" PE�EH��;
C
C   �O��PO�PAMM�: 1. PROF1, SVCHEK
C   -------------
C
C   �P�ME�AH�E:  1. "�MO��AH�E" HA HOMEP KAHA�A �� MASPRT(95)
C   ----------       NFCHEK = 10
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( DTCHEK, MASPRT(90) )
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
       INTEGER FCHEK
       DIMENSION KAR(*), DAT(*), MSWALL(*), TOKN(*), VALLN(*)
C----------------------------------------------------------------------
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
       NFCHEK = NF
100    CONTINUE
       WRITE(NWTR, 500)         NFCHEK
500    FORMAT(/' *** #COXP �P�H�TA:==> KAHA� �A��A COXPAHEH�� ', I3,
     = ' ***'/)
C
       CALL SVCHEK( NFCHEK, TN, MSWALL, TOKN, VALLN )
C
       CALL PPNCOM( 3, NWTR )
       WRITE(NWTR,501) TN, NFCHEK, NZAPSP
501    FORMAT(/' *** COXPAHEHO:=> TN=',G12.5,' CEK., KAHA� # ', I2,
     =    ', �A��C� B NSPOOL ', I6,' ***'/)
C----------------------------------------------------------------------
       RETURN
       END
