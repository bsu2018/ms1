         SUBROUTINE WRIT00( KOD, TIME, MSWALL, TOK, VALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0
C   �O��PO�PAMMA:   WRIT00( KOD, TIME, MSWALL, TOK, VALL )
C   ------------
C   HA�HA�EH�E:        B �AB�C�MOCT� OT KOD ==>
C   ----------     KOD=1
C                  *****
C                  �OPM�P�ET � B�BO��T �A�O�OBOK �A��A �HA�EH�� TOKOB
C                  � HA�P��EH��       HA KAHA� NSPOOL = MASPRT(18)
C                  B�BO��TC� H��EBA� �A��C�, �EPBOE C�OBO -> TIME;
C                                            **********************
C                   PA�MEPHOCT� B���PA�TC� �� NNTOK  �  NSYS;
C                  MACC�B� TOK  � VALL - PA�P��A�TC�;
C
C                  *** C.�.K. �A��CE� B NSPOOL NZAPSP -> NZAPSP + 1; ***
C
C                  KOD=2
C                  *****
C                  �OPM�P�ET � B�BO��T O�EPE�H�� CTPOK� B �A�� NSPOOL;
C                  CTPOKA -->  TIME, MACC�B TOK, MACC�B VALL;
C
C                  *** C.�.K. �A��CE� B NSPOOL NZAPSP -> NZAPSP + 1; ***
C
C              ==  �P� FCHEK = 1 �EPE� BPEM� DTCHEK CO��AETC�
C                  K.T. ( �/� SVCHEK ) HA KAHA�E NFACHK
C                      ( �PAKT��ECK� K.T. CO��AETC� �EPE� KA���E KKCHEK
C                        TO�EK B�BO�A B �A�� HAKO��EH�� NSPOOL );
C              ==
C                  KOD=3
C                  *****
C                  �OPM�P�ET � B�BO��T �OC�E�H�� �A��C� �A��A NSPOOL,
C                  �A�� NSPOOL �CTAHAB��BAETC� B HA�A�O;
C                  MACC�B� TOK  �  VALL -  PA�P��A�TC�;
C
C   METO�:         B�BO� MACC�BOB   TOK , VALL  � �HA�EH�� TIME;
C   ------
C
C
C   �APAMETP�:     KOD - �EPEK���ATE�� �A�POCOB;
C   ---------      MSWALL   -   C��COK ��PAB�����X C�OB MSW;
C                  TOK  - MACC�B �HA�EH�� TOKOB � �P. �EPEMEHH�X
C                         COCTO�H��, PAC��T�BAEMO� �E��;
C                     VALL - MACC�B ���OB�X HA�P��EH��;
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2, SVCHEK
C   ------------
C   �P�ME�AH�E:    1. CTP�KT�PA �A��A:

C   -----------       ****************
C                     1-� �A��C�:    HOUTT,FTRSPL      ( �� �/� DIGITA )
C
C                     C�E�. �A��C�:  TIME, TOK, VALL;
C
C                     �OC�E�H�� �A�:  "-1", 0-��, 0-��;
C
C                  2.  PEKOMEH��ETC� �OPMAT "RECFM=VBS";
C
C                  3. �O�E NZAPSP �P� KOD=1  O�H���ETC�!
C                     **********************************
C            ### ��� 1990 �� MASPRT(121) <==> FTRSPL = 1
C                � 䠩� ���������� �뢮����� ⮫쪮
C                㧫� � ⮪�, ����� ������ � /SERVZ8/ ....
C                FTRSPL �����뢠���� � ����� ������
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         COMMON /PROF2/ MACTAB(1)
         COMMON /SERVZ8/ SPISNN, SPISVL(16), SPISTK(16)
         DIMENSION MSWALL(*), VALL(*),  TOK(*)
         DIMENSION  V_A(15),  T_A(15)
         EQUIVALENCE ( MASPRT(18), NSPOOL ),
     =               ( MASPRT(46), NNTOK ),
     =               ( MASPRT(26), NSYS  ),
     =               ( MASPRT(90), DTCHEK),
     =               ( MASPRT(92), NZAPSP),
     =               ( MASPRT(97), NFACHK),
     =               ( MASPRT(96), KKCHEK),
     =               ( MASPRT(50), ZEROCP),
     =               ( MASPRT(14), WTRE  ),
     =               ( MASPRT(91), FCHEK   ),
     =               ( MASPRT(40), H0DATA  ),
     =               ( MASPRT(121), FTRSPL )
          INTEGER FCHEK, FTRSPL
          INTEGER        SPISNN, SPISVL, SPISTK
          DATA V_A, T_A / 30*0.0 /
         GO TO (100, 200, 300 ), KOD
C!!!-------------------------------------------------------------------
C
         WRITE(NWTRE,4) KOD
4        FORMAT(' *** �O�. O���KA B��OBA WRIT00, KOD=', I10, ' ***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
C######################################################################
C######################################################################
100       CONTINUE
C-->  OTKP�T�...
C       DO 101  I=1, NNTOK
C101    TOK(I) = 0.0
C
C       DO 102 I=1,NSYS
C102    VALL(I) = 0.0
C
       CALL KSOPEN(NSPOOL,1,ii_rr)
       IF (ii_rr .NE. 0 ) call STOPP(20)
C
       NZAPSP = 0
       REWIND NSPOOL
C
C@@@@@@@@@@@@@@@@@
C      WRITE(NSPOOL) TIME, ( TOK(I), I=1, NNTOK),
C    =                     ( VALL(I), I=1, NSYS )
C@@@@@@@@@@@@@@@@@
       WRITE(NSPOOL) TIME, FTRSPL
C       CALL writmb(nspool,NNTOK,TOK)
C       CALL writmb(nspool,NSYS,VALL)
C@@@@@@@@@@@@@@@@@
       NZAPSP = NZAPSP + 1
       RETURN
C######################################################################
200    CONTINUE
C-->  O�EPE�HA�..
C      WRITE(NSPOOL) TIME, (TOK(I), I=1,NNTOK),
C    =                     (VALL(I), I=1,NSYS )
C@@@@@@@@@@@@@@@@@
       WRITE(NSPOOL) TIME
       IF (FTRSPL .EQ. 0 ) THEN
          CALL writmb(nspool,NNTOK,TOK)
          CALL writmb(nspool,NSYS,VALL)
       ELSE
          DO 666 i666=1,SPISTK(1)
             nnii = SPISTK(i666+1)
666        T_A(i666) = TOK(nnii)
          DO 667 i666=1,SPISVL(1)
             nnii = SPISVL(i666+1)
667        V_A(i666) = VALL(nnii)
          CALL writmb(nspool,SPISTK(1),T_A)
          CALL writmb(nspool,SPISVL(1),V_A)
       ENDIF
C@@@@@@@@@@@@@@@@@
       NZAPSP = NZAPSP + 1
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       IF ( FCHEK .EQ. 0 ) RETURN
C-->  HE CO��AT� �� HAM K.T.  ?
C    NZX - HOMEP (OTHOC�TE��H��) �A��C� B NSPOOL--->
       NZX = NZAPSP - 2
       MNZX = MOD( NZX, KKCHEK )
       IF ( MNZX .NE. 0 ) RETURN
C-->>  CO��AT�
       CALL SVCHEK( NFACHK, TIME, MSWALL, TOK, VALL )
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       RETURN
C######################################################################
300    CONTINUE
C-->  �AKP�T�...
       DO 301 I=1, NNTOK
301    TOK(I) = 0.0
C
       DO 302 I=1, NSYS
302    VALL(I) = 0.0
C
       XXX = -1.0
C      WRITE(NSPOOL) XXX, (TOK(I), I=1, NNTOK ),
C    =                    (VALL(I), I=1, NSYS  )
C@@@@@@@@@@@@@@@@@
       WRITE(NSPOOL) XXX
       IF (FTRSPL .EQ. 0 ) THEN
C@@@@@@@@@@@@@@@@@
          CALL writmb(nspool,NNTOK,TOK)
          CALL writmb(nspool,NSYS,VALL)
C@@@@@@@@@@@@@@@@@
       ELSE
          DO 668 i666=1,SPISTK(1)
             nnii = SPISTK(i666+1)
668        T_A(i666) = TOK(nnii)
          DO 669 i666=1,SPISVL(1)
             nnii = SPISVL(i666+1)
669        V_A(i666) = VALL(nnii)
          CALL writmb(nspool,SPISTK(1),T_A)
          CALL writmb(nspool,SPISVL(1),V_A)
       ENDIF
C@@@@@@@@@@@@@@@@@
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       END FILE NSPOOL
       REWIND NSPOOL
       CLOSE (NSPOOL)
       RETURN
       END
