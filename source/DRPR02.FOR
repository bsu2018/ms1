          SUBROUTINE DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
C   ------------
C   HA�HA�EH�E:    *** O�PA�OT��K ��PEKT�B #����           ***
C   -----------      �����  PE����TATOB B� ������� ����
C                      ����� �� MASPRT(123)==13 - ���-�� �祪
C                      ����� �� MASPRT(124)==14 - ���祭��
C   METO�:         B��OPKA �� TIME, TOKN  �  VALLN CO��ACHO C��CKA SPIS;
C  -------
C
C   �APAMETP�:     SPIS - C��COK �A�POCOB ( C.M. GETSPS )
C   ---------      TIME, TOKN, VALLN - BEKTOPA PE�EH��;
C                  KOD = 1 - �E�AT� �A�K� TA�����;
C                      = 2 - �E�AT� O�EPE�HO� CTPOK�;
C                      = 3 - �AKP�T� TA�����;
C   �O��PO�PAMM�:  1. COMMON PROF1, SERVZ2, 3;
C   -------------  2. MOVE0
C   �P�ME�AH�E:    KOHTPO�� �A�O�HEH�� SPIS HE �PO��BO��TC�;
C   ----------
CZZZ-------------------------------------------------------------------
         COMMON/ PROF1 / MASPRT(200)
         COMMON/ SERVZ2/        TABTOK(135)
         COMMON/ SERVZ3/        TABTIP(300)
         CHARACTER*2 kanal, K_KK
         CHARACTER*8 JNAME
         INTEGER TABTOK, TABTIP, SPIS, DIAL
         DIMENSION SPIS(1), TOKN(1), VALLN(1), NAMEI(3), NAMEV(3),
     =             INTO(3)
         DIMENSION HELP(32), RHELP(1)
         INTEGER HELP, BLANK, TIRE, PX, ANM, ANT
         EQUIVALENCE( NWTR,  MASPRT(13)),
     =              ( NWTRE, MASPRT(14)),
     =              ( DIAL,  MASPRT(15)),
     =              ( RHELP(1), HELP(1) ),
     =              ( NEXTR, MASPRT(123)),
     =              ( NEXTRD, MASPRT(124)),
     =              ( JNAME, MASPRT(125))
C
         DATA NAMEI/ '��-�', '� & ', '����' /
         DATA NAMEV/ ' H��', '�殮', '��� ' /
         DATA INTO / '����', '� �', '����' /
         DATA BLANK/'    '/, TIRE/'----'/
         DATA K_KK/'.K'/
C**********************************************************************
         KP = SPIS(1)
         KP_P = KP
         GO TO ( 1,2,3,2,2,2,2 ), KOD
         CALL SUBERR(KOD)
1        CONTINUE
         K_SSS = 1
         IF ( KP .GT. 9 ) K_SSS = 2
         DO 66666 K_SSST = 1, K_SSS
            IF  ( K_SSST .EQ. 1 ) ISPIST = 2
            IF  ( K_SSST .EQ. 2 ) ISPIST = ISPIS
            IF  ( K_SSST .EQ. 1 .AND. KP_P .GT. 9 ) KP = 9
            IF  ( K_SSST .EQ. 1 .AND. KP_P .LE. 9 ) KP = KP_P
            IF  ( K_SSST .EQ. 2                   ) KP = KP_P - 9
            IF  ( K_SSST .EQ. 2 .AND. KP   .LE. 0 ) KP = 1
C--> ���HA B�B. CTPOK� ==> KSTR  � KSTR4
         KSTR = 11 + 13 * KP
C---->
         KSTR4 = KSTR/4
C===> B�BO� �A�O�OBKA....
         WRITE(NWTR, 360) NEXTR, NEXTRD
360      FORMAT(1H1,// 10X, '��ᯮ������� ���祭�� �᪮��� ����稭'/
     = 5X, ' �뢮����� �� ���譨� 䠩�� ������� N ',I3, ' � ',I3 /)
C--> C�PABKA ...
         ISPIS = ISPIST
         IHELP = 1
         KPP = KP
34       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 35
         PX = SPIS(ISPIS)
         IF ( PX .NE. 3 ) GO TO 36
         NEL = SPIS( ISPIS + 2 )
         ANM = SPIS( ISPIS + 3 )
         ANT = SPIS( ISPIS + 4 )
         NSM = SPIS( ISPIS + 5 )
         IHELP = 1
         HELP(IHELP) = NEL
         IHELP = IHELP + 1
         CALL MOVE0( HELP(IHELP), TABTIP(ANM), 5 )
         IHELP = IHELP + 5
         CALL MOVE0( HELP(IHELP), TABTOK(ANT), 3 )
         IHELP = IHELP + 3
         HELP(IHELP) = NSM
         WRITE(NWTR, 37) NEL, ( HELP(I),I=2,9 ), NSM
37       FORMAT(1X, '������� # ', I5, 1X, 5A4, 1X, '��६����� ', 3A4,
     =           1X, '(���祭�� # ', I3, ')'  )
36       KPP = KPP - 1
         ISPIS = ISPIS + 6
         GO TO 34
35       CONTINUE
         DO 11 I=1,32
11       HELP(I) = TIRE
         WRITE(NWTR,12) ( HELP(I), I=1,KSTR4 )
12       FORMAT( /1X, 32A4 )
         DO 13 I=1,32
13       HELP(I) = BLANK
C
C--->  TEKCT �A�O�OBKA...
         ISPIS = ISPIST
         IHELP = 1
         KPP = KP
15       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 16
         PX = SPIS(ISPIS)
         IF ( PX .EQ. 1  .OR.  PX .EQ. 2 )
     =      CALL MOVE0( HELP(IHELP), NAMEV, 3 )
         IF ( PX .NE. 3 ) GO TO 14
C-->  �O �H�.
         NEL = SPIS(ISPIS+2)
         ANM = SPIS(ISPIS+3)
         ANT = SPIS(ISPIS+4)
14       CONTINUE
         IF ( PX .EQ. 3 ) CALL MOVE0( HELP(IHELP), TABTOK(ANT), 3 )
         IHELP = IHELP + 3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 15
16       CONTINUE
         IH = IHELP - 1
         WRITE(NWTR, 17 ) ( HELP(I), I=1,IH )
17       FORMAT(1X, ' T���饥  ', 9('!': 3A4 ) )
C===> 2- CTPOKA
         DO 18 I = 1, 32
18       HELP(I) = BLANK
         ISPIS = ISPIST
         IHELP = 1
         KPP = KP
19       CONTINUE
         IF ( KPP .LE. 0 ) GO  TO  20
         PX = SPIS(ISPIS)
         IF ( PX. EQ. 1  .OR.   PX .EQ. 2 )
     =         CALL MOVE0( HELP(IHELP), INTO,  3  )
         IF ( PX .EQ. 3 )
     =         CALL MOVE0( HELP(IHELP), NAMEI,  3  )
         IHELP = IHELP + 3
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 19
20       CONTINUE
         IH = IHELP - 1
         WRITE(NWTR, 21 ) ( HELP(I), I=1, IH )
21       FORMAT(1X, '   �६�  ',  9('!': 3A4 ) )
C--->  3-� CTPOKA
         DO 22 I = 1, 32
22       HELP(I) = 0
C-->  KOL-BO ��CE�  KPX = 2*<KO�-BO �EPEMEHH�X>
         KPX = 2*KP
         ISPIS = ISPIST
         IHELP = 1
         KPP = KP
23       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 24
         PX = SPIS(ISPIS)
         IF ( PX .EQ. 1 ) HELP(IHELP) = SPIS(ISPIS+1)
         IF ( PX .EQ. 1 ) HELP(IHELP+1) = 0
         IF ( PX .EQ. 2 ) HELP(IHELP) = SPIS(ISPIS+1)
         IF ( PX .EQ. 2 ) HELP(IHELP+1) = SPIS(ISPIS+2)
         IF ( PX .EQ. 3 ) HELP(IHELP) = SPIS(ISPIS+2)
         IF ( PX .EQ. 3 ) HELP(IHELP+1) = SPIS(ISPIS+5)
         IHELP = IHELP + 2
         ISPIS = ISPIS + 6
         KPP = KPP - 1
         GO TO 23
24       CONTINUE
         IH = IHELP - 1
         WRITE(NWTR,25) (HELP(I), I=1,IH )
25       FORMAT(1X, '  [ᥪ.]  ', 9('!': I5, 1X, I5,1X ) )
C-->  4-� CTPOKA
         DO 26 I = 1, 32
26       HELP(I) = TIRE
         WRITE(NWTR, 1200) ( HELP(I), I=1,KSTR4)
66666    CONTINUE
1200     FORMAT(1X,32A4)
C*********@@@@@@@@@@
        write(unit=kanal,fmt=1666)  NEXTR
1666    format(i2.2)
         OPEN (unit=NEXTR,file=JNAME//K_KK//kanal,
     X         access='direct',recl=60)
C*********@@@@@@@@@@
        write(unit=kanal,fmt=1666)  NEXTRD
         OPEN (unit=NEXTRD,file=JNAME//K_KK//kanal)
C        OPEN (unit=NEXTRD,file='MS1-1989.K14')
C*********@@@@@@@@@@
C     ������� ���.
         nnrec = 0
C*********@@@@@@@@@@
         RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
C######################################################################
2        CONTINUE
C--> B�BO� O�EPE�HO�
C@@@@@@@ DO 27  I=1,9
         DO 27  I=1,15
27       RHELP(I) = 0.0
         IHELP = 1
         ISPIS = 2
         KP = SPIS(1)
         KPP = KP
28       CONTINUE
         IF ( KPP .LE. 0 ) GO TO 29
         PX = SPIS(ISPIS)
         I2 = SPIS(ISPIS+1)
C$$      I3 = SPIS(ISPIS+2)
C$$      IF ( PX .EQ. 1 ) RHELP(IHELP) = VALLN(I2)
C$$      IF ( PX .EQ. 2 ) RHELP(IHELP) = VALLN(I2) - VALLN(I3)
C$$      IF ( PX .EQ. 3 ) RHELP(IHELP) = TOKN(I2)
C--
       GO TO ( 51, 52, 53 ), PX
       CALL SUBERR(PX)
51     CONTINUE
C-->  HA�P��EH��
       VI2 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN(I2)
       RHELP(IHELP) = VI2
       GO TO 50
52     CONTINUE
C-->  PA�HOCT�
       I3 = SPIS( ISPIS+2 )
       VI2 = 0.0
       VI3 = 0.0
       IF ( I2 .NE. 0 ) VI2 = VALLN( I2 )
       IF ( I3 .NE. 0 ) VI3 = VALLN( I3 )
       RHELP( IHELP ) = VI2 - VI3
       GO TO 50
53     CONTINUE
C-->> "TOK"
       RHELP(IHELP) = TOKN(I2)
C$$$$$$GO TO 50
50     CONTINUE
         KPP = KPP - 1
         IHELP = IHELP + 1
         ISPIS = ISPIS + 6
         GO TO 28
29       CONTINUE
         nnrec = nnrec + 1
C        write(nwtre,*) NEXTR, nnrec, (RHELP(I), I=1,KP )
C        write(99   ,*) NEXTR,nnrec, (RHELP(I), I=1,KP )
         WRITE(NEXTR,rec=nnrec) (RHELP(I), I=1,KP )
         RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
C######################################################################
3        CONTINUE
         WRITE(NEXTRD,*) nnrec
C--> �AKP�T�
          close (unit=NEXTR)
          close (unit=NEXTRD)
          KP = SPIS(1)
          IF (KP .GT. 9 ) KP = 9
         DO 32 I=1,32
32       HELP(I) = TIRE
         KSTR = 11 + 13 * KP
C--------
         KSTR4 = KSTR/4
         WRITE(NWTR,33) ( HELP(I), I=1, KSTR4 )
33       FORMAT(1X, 32A4 // )
         RETURN
         END
