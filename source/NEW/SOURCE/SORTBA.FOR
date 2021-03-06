      SUBROUTINE SORTBA( BA0)
CXXX-------------------------------------------------------------------
C   �O��PO�PAMMA:  SORTBA ( BA0 )
C   ------------
C   HA�HA�EH�E:    COPT�P�ET MACC�B BA0:
C   ----------     A) �O BO�PACTAH�� HOMEPA �P��HAKA T��A.
C                  �) B KA��O� �P���E �A��CE� C O��HAKOB�M� �HA�EH��M�
C                     �P��HAKA T��A B��O�H�ET COPT�POBK� �O BO�PACTAH��
C                     HOMEPA ��EMEHTA.
C   METO�:          "�PEBECHA" COPT�POBKA �/� TRIARB.
C   -----
C   �O��PO�PAMM�:  1. TRIARB + BA0CLP + BA0CNG.
C   ------------   2. TRIARB + BA1CLP + BA1CNG.
C                  3. COMMON PROF1
C   �APAMETP�:     BA0 - C��COK ��EMEHTOB �E�� �OC�E INPUT0 ( CM. )
C   ---------
C   �P�ME�AH�E:    1. COPT�POBKA �) B��O�H�ETC� �P� KEYSOR(I) = 1
C   ----------        ��E I=1,2,3,4,5,6 - HOMEP �P��HAKA T��A;
C                  2. �P� PAC��PEH�� HOMEPOB �.T��A -> PACC��P�T�C� !!!
C                  3. BA0 - CTAT��ECK� B��E�EH.
C                  4. BA0... � BA1... TE�E ��HK���, HO PA�H��
C                     ��ET ��CKP��TOPA MACC�BA BA0 ( CM. �/� INPUT0 )
C
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(200)
       DIMENSION BA0(*), KEYSOR(6), ABEG(6), NKOL(6)
       INTEGER BA0, BA0CPL, BA1CPL, PTIP, ABEG, PTIPBA
C--> ABEG - A�P. HA�A�A B BA0, NKOL - KO�-BO, ��-OB C �.T��A = �H�EKC�
       EQUIVALENCE ( NWTRE, MASPRT(14) )
       EQUIVALENCE ( NWTR,  MASPRT(13) )
       EQUIVALENCE ( NXBA0, MASPRT( 3) )
       EQUIVALENCE ( N1BA0, MASPRT( 4) )
C
       EXTERNAL BA0CPL, BA1CPL, BA0CNG, BA1CNG
       DATA KEYSOR / 1, 1, 1, 1, 1, 1 /, LKEYS /6/
C----------------------------------------------------------------------
C-->  KOHTPO�� ??
       IF (NXBA0 .GT. 0 ) GO TO 200
       WRITE( NWTRE, 201 ) NXBA0
       WRITE( NWTR,  201 ) NXBA0
201    FORMAT(//' *** B �E�� HET ��EMEHTOB -> NXBA0 = ',I3,
     * ' ***'/' *** PA�OTA BO�MO�HO �PEPBETC� =SORTBA= ***'//)
       RETURN
200    CONTINUE
       IF ( NXBA0 .EQ. 1 ) RETURN
C!!!-------------------------------------------------------------------
C@@@@@ CALL PRINT0( BA0, BA0, BA0 )
C!!!-------------------------------------------------------------------
C
C  �EPBA� �A�A  (A)
C
       CALL TRIARB( NXBA0, BA0CNG, BA0CPL, BA0 )
C
C!!!-------------------------------------------------------------------
C@@@@ CALL PRINT0( BA0, BA0, BA0 )
C!!!-------------------------------------------------------------------
C
C   BTOPA� �A�A  (B)
C
C  �A�O�HEH�E ABEG � NKOL
C
       DO 1 I=1,LKEYS
       ABEG(I) = 0
1      NKOL(I) = 0
C  CKAH�POBAT� BA0
       I0 = 6
       IMAX = BA0(3)
C--> MACC�B BA0 OTCOPT�POBAH �O �.T��A
C    B��E��EM �PAH��� "OTPE�KOB" � �OPM�P�EM ABEG + NKOL
C
       PTIP = 0
       DO 100 I=1, NXBA0
C  -> A�P. HA�. TEK. �A�. B BA0 - �E
       IZ = (I-1) * N1BA0 + I0
C-->  KOHTPO��
       IF ( IZ + N1BA0  .GT. IMAX ) CALL SUBERR( IZ )
C-->  KAKO� TEK. �.T��?
       PTIPBA = BA0( IZ + 4 )
C-->  OCHOBHOE CPABHEH�E....
       IF ( PTIPBA .EQ. PTIP ) GO TO 101
C
C-->  A K���, TO �P��O� !
C      CTPAXOBKA...
       IF ( PTIPBA .GT. LKEYS .OR.
     *      PTIPBA .LE.   0         )  CALL SUBERR( PTIPBA )
C  SALTO-MORTALE.....
       PTIP = PTIPBA
       ABEG( PTIP ) = IZ
       NKOL( PTIP ) = 1
       GO TO 100
C----------------------------------------------------------------------
101    CONTINUE
C
C-->  K����K CTAP��
C
       IF ( PTIP .GT. LKEYS  .OR.  PTIP .LE. 0 ) CALL SUBERR(PTIPBA)
C
       NKOL ( PTIP ) = NKOL( PTIP )  +  1
100    CONTINUE
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C**********************************************************************
C
C  COPT�P�EM �O��P����
C
       DO 300 I=1,LKEYS
C-->  MO�ET HE HA�O ?
       IF ( KEYSOR(I) .NE. 1 ) GO TO 300
C-->  CKO��KO � ��E HA�A�O ?
       N = NKOL(I)
       NBEG = ABEG(I)
C-->  MO�ET HE HA�O ?
       IF ( N .LE. 1 ) GO TO 300
C-->  COPT�POBAT� "OTPE�OK" BA0...
       CALL TRIARB( N, BA1CNG, BA1CPL, BA0(NBEG) )
300    CONTINUE
C!!!-------------------------------------------------------------------
C@@@@@ CALL PRINT0( BA0, BA0, BA0 )
C!!!-------------------------------------------------------------------
C  �CE
       RETURN
       END
