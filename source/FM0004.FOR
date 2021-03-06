         SUBROUTINE FM0004( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  FM0004( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E��:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  9        "��P.BEHT���"             TR
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E�� "��P. BEHT���",
C   -----          EC�� HEO�XO��MO �EPEBO� �� ��. �PA�. B CEK.
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. PEA���A��� ��� PMAS = 0  , PMAS = 1  �  PMAS = 2.
C                     �P� O���KAX --> �P�H�MAETC� PMAS = 0;
C                     �HA�EH�� �APAMETPOB ---->
C
C
C
C                       **********               ******
C                       **********               ******
C                       **********               ******
C  T0*--------I---------**********-----I---------******----------------
C    I        I         I        I     I         I
C    I<-TZAD->I         I<-TIMP->I     I         I
C    I        I<-TALFA->I        I     I<-TALFA->I
C             I                        I
C             I<---------TCIU--------->I<-----------TCIU--------
C
C  B O��CAH�� �E�� �C�O����ETC� C�E�. �A��C�:
C
C   ... MAS < PMAS TZAD  TCIU  TALFA  TIMP  TKCIU  DTALFA >
C          �P� PMAS=1 BCE "BPEMEHA" KO��P��TC� B ��. �PA�. ,
C          �P� PMAS=2 BCE "BPEMEHA" KO��P��TC� B CEK. ,
C          BO�MO�HO KO��POBAH�E:
C   ... MAS < 0 >, �TO COOTBETCTB�ET:
C
C       TZAD = 360 ��. �PA�.
C       TCIU = 60  ��. �PA�.
C       TAFLA= 30  ��. �PA�.
C       TIMP = 10  ��. �PA�.
C       TKCIU=  0  ��. �PA�.
C       DTALFA= 0  ��. �PA�.
C
C      ��EC� �C�O���OBAH� C�E�. O�O�HA�EH��:
C
C
C      TZAD  - BPEMA �A�EP�K� BK���EH�� "C��"    ( CEK. )
C      TCIU  - �EP�O� �OBTOPEH�� C��HA�OB "C��"  ( CEK. )
C      TALFA - �A�EP�KA �EPE�HE�O �POHTA C��HA�A
C              "C��" OT HA�A�A O�EPE�HO�O �EP�O�A
C              �OBTOPEH��                          ( CEK. )
C      TIMP - ���TE��HOCT� �M�Y��CA "C��"          ( CEK. )
C      TKCIU - �EPE� CKO��KO �EP�O�OB �OBTOPEH��
C              C��HA�A "C��" ��MEH�T� TALFA HA
C      DTALFA - E��H�� - �A� ��MEHEH�� TALFA       ( CEK. )
C
C                  4. H��E �P�BO��TC� �OPMAT� RMSW:
C
C                  RMSW T��A # 9  [ ��P. BEHT��� ]
C                  --------------------------------
C                                                             LM = 15
C                                                             *******
C                                                                  (25)
C                    SPM                      SRAB
C +--+--+----+----+----+------+------+---+----------+----+----+-------+
C IKDIHXIPMASITZADITCIUITALFA ITIMP  ITK I DTALFA   I M  I HX I PE�E  I
C I  I  I    I    I    I      I      ICIUI          ICIU ICIU I PB    I
C I  I  I    I    I    I      I      I   I          I    I    I       I
C +--+--+----+----+----+------+------+---+----------+----+----+-------+
C I1 I2 I 3  I 4  I 5  I  6   I  7   I 8 I   9      I 10 I 11 I 12-15 I
C +--+--+----+----+----+------+------+---+----------+----+----+-------+
C I11I12I 13 I 14 I 15 I  16  I 17   I18 I   19     I 20 I 21 I 22-25 I
C +--+--+----+----+----+------+------+---+----------+----+----+-------+
C
C      BEHT��� OTKP�T -> KD=1;    BEHT��� �AKP�T ->  KD=0;
C      HX - �A� �O BPEMEH�, BPEM� BO�MO�HO�O �EPEK���EH��;
C
C        MCIU  �  HXCIU - �APAMETP� "BOCTAHOB�EHHO�O" �MP���CA "C��"
C                         ( CM. �/� MCIU01 );
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 9  ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0004: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1        CONTINUE
C---> O�H��EH�E BCEX �O�E� RMSW ��� BEHT���A
         LM = 15
         DO 100 I=1,LM
100      RMSW(I) = 0.0
C===> �P�H�T� �CXO�. �APAMETP�:
C-->  KOTPO��
         KKM = PAR0T(1)
C-->  B�� ��PAB�EH��..
         PMAS = PAR0T(2)
         IF ( PMAS .EQ. 0.0 ) GO TO 2
         IF ( PMAS .EQ. 1.0 ) GO TO 3
         IF ( PMAS .EQ. 2.0 ) GO TO 3
C----------------------------------------------------------------------
         WRITE(NWTR,4) NEL, PMAS
4        FORMAT(' *** T�� ��PAB�EH�� "C��" ��� ��EMEHTA #', I5, ' ***',
     =          ' *** PABH�� ', F5.0,' O���O�EH  --> �P�H�MAETC�',
     =    ' PABH�M H��� ***')
         GO TO 2
C----------------------------------------------------------------------
C-->  PMAS = 0    �O �MO��AH��
2        CONTINUE
         RMSW(4) = 360.0 / 1.8E+4
         RMSW(5) =  60.0 / 1.8E+4
         RMSW(6) =  30.0 / 1.8E+4
         RMSW(7) =  10.0 / 1.8E+4
C-->  OCTA��H�E � TAK PABH� 0.0
         RETURN
C######################################################################
3        CONTINUE
C---> PMAS = 1 ��� 2
C-->  �POBEPKA HA �O�HOT�
         IF ( KKM .GT. 7 ) WRITE(NWTR,40) KKM, NEL
40       FORMAT(' *** MHO�O �APAMETPOB O��CAH�� "C��" ->', I5, ' ***',
     =          ' *** ���H�E ��HOP�P��TC� (��EMEHT # ',I5,' ) ***')
         IF ( KKM .GT. 7 ) KKM = 7
C--->
         KX = 7 - KKM
         IF ( KKM .LT. 7 ) WRITE(NWTR,50) NEL, KX
50       FORMAT(' *** �APAMETP� ��PAB�EH�� "C��" ��� ��EMEHTA #', I5,
     =' ***'/' *** �A�AH� HE �O�HOCT�� --> HET ', I2, ' �HA�EH��',
     =' ***'/ ' *** �PO���EHH�E �HA�EH�� �P�H�MA�TC� PABH�M� 0.0 ***')
C----------------------------------------------------------------------
       CONST=1.8E+4
       IF( PMAS .EQ. 2.0 ) CONST=1.
         DO 6 I = 1, KKM
         IF ( I .EQ. 1 ) RMSW(3) = PMAS
         IF ( I .EQ. 2 ) RMSW(4) = PAR0T(3) / CONST
         IF ( I .EQ. 3 ) RMSW(5) = PAR0T(4) / CONST
         IF ( I .EQ. 4 ) RMSW(6) = PAR0T(5) / CONST
         IF ( I .EQ. 5 ) RMSW(7) = PAR0T(6) / CONST
         IF ( I .EQ. 6 ) RMSW(8) = PAR0T(7)
         IF ( I .EQ. 7 ) RMSW(9) = PAR0T(8) / CONST
6        CONTINUE
C--->  KOHTPO�� TCIU
         IF ( RMSW(5) .NE. 0.0 ) RETURN
         WRITE(NWTR,7) NEL, PAR0T(4)
7        FORMAT(' *** �EP�O� �OBTOPEH�� �M����CA "C��" ��� ��EMEHTA',
     = ' # ', I5,' ***'/
     =  ' *** PABH�� ', G12.5, '---> HE�O��CT�M ***'/
     =  ' *** �P�H�MAETC� PABH�M   0.020 CEK.   ***')
         RMSW(5) = 360.0 / 1.8E+4
         RETURN
         END
