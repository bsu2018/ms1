       SUBROUTINE DRSAVA(KAR, DAT )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: DRSAVA(KAR, DAT )
C   ------------
C
C   HA�HA�EH�E:      O�PA�OT��K  #ACOXP  DTCHEK   NFACHK
C   ----------       *************************************
C
C              DTCHEK - �TEPBA� �O BPEMEH� B��O�HEH�� ABTO_COXP.
C                       ( B CEK. KPATEH HOUTT )
C              NFACHK - KAHA� ��� ABTO_COXP_PAHEH��
C
C
C   METO�:     �P�EM �AP-B ��PEKT�B� � MO����KA��� �O�E� B MSWALL
C   ------    *** �P� �C�EXE --> BO�����EH�E ��A�A "FCHEK" ****
C                                 -------------------------
C
C   �APAMETP�: KAR, DAT - MACC�B� CTPOK� � �AHH�X ��PEKT�B�;
C   ---------
C
C   �O��PO�PAMM�: 1. PROF1
C   -------------
C
C   �P�ME�AH�E:  1. "�MO��AH�E" HA HOMEP KAHA�A �� MASPRT(97
C   ----------       NFACHK = 11, �HTEPBA� �O TN = DTCHEK ��
C                      MASPRT(90) -->  0.5  CEK;
C
C                2.  !! BH�MAH�E DTCHEK �O��HO ��T�
C                      KPATHO HOUTT !!!
C                3. B DIGITA -->  BO�MO�HO �C�PAB�EH�E;
C                   -----------------------------------
C
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( DTCHEK, MASPRT(90) )
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
       DIMENSION KAR(*), DAT(*)
C----------------------------------------------------------------------
       KKK = IFIX( DAT(1) )
       IF ( KKK .EQ. 0 ) GO TO 100
C-->  NEW  DTCHEK
       DTCHEK = DAT(2)
       IF ( KKK .EQ. 1 ) GO TO 100
C-->  HOB�� �A��..
       NF = IFIX( DAT(3) )
C-->  KOHTPO�� �A��A ....
       IF ( NF .EQ. NWTR  .OR.
     =      NF .EQ. NWTRE   .OR.
     =      NF .EQ. NRDR    .OR.
     =      NF .EQ. NSPOOL  .OR.
     =      NF .EQ. NMAK    .OR.
     =      NF .EQ. NWTRD   .OR.
     =      NF .EQ. NFILER  )
     =                                 NF = NFACHK
C----------------------------------------------------------------------
       NFACHK = NF
100    CONTINUE
       WRITE(NWTR, 500) DTCHEK, NFACHK
500    FORMAT(/' *** #ACOXP �P�H�TA:==> ABTO_COXPAHEH�E �EPE�',
     = ' ', G12.5, ' CEK. KAHA� # ', I2, ' ***'/)
C
       FCHEK = 1
       RETURN
       END
