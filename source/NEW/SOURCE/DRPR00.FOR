         SUBROUTINE DRPR00(KOD, SPIS, TIME, TOKN, VALLN )
CXXX----KOM��EKC "MS1" -->
C   �O��PO�PAMMA:   DRPR00(KOD, SPIS, TIME, TOKN, VALLN )
C   ------------
C   HA�HA�EH�E:    *** O�PA�OT��K ��PEKT�B #�E�  �   #TA�  ***
C   -----------      �E�AT� PE����TATOB B TA����HO� �OPME;
C
C   METO�:         B��OPKA �� TIME, TOKN  �  VALLN CO��ACHO C��CKA SPIS;
C  -------
C
C   �APAMETP�:     SPIS - C��COK �A�POCOB ( C.M. GETSPS )
C   ---------      TIME, TOKN, VALLN - BEKTOPA PE�EH��;
C                  KOD = 1 - �E�AT� �A�K� TA�����;
C                      = 2 - �E�AT� O�EPE�HO� CTPOK�;
C                      = 3 - �AKP�T� TA�����;
C                      = 4 - �E�AT� O�EPE�HO� TO�K� C �P��HAKOM
C                            "B MOMEHT KOMM�TA���";
C                      = 5 - �E�AT� O�EPE�HO� TO�K� C �P��HAKOM
C                            "TECT _ P�H�E";
C
C                      = 6 - �E�AT� TO�K� �P� CO��AH�� K.T. C �P��HAKOM
C                            "@";
C                      = 7 - �E�AT� TO�K� C �P��HAKOM "�HTEP�O�����";
C
C
C
C
C   �O��PO�PAMM�:  1. COMMON PROF1, SERVZ2, 3;
C   -------------  2. MOVE0
C   �P�ME�AH�E:    KOHTPO�� �A�O�HEH�� SPIS HE �PO��BO��TC�;
C   ----------
CZZZ-------------------------------------------------------------------
         COMMON/ PROF1 / MASPRT(200)
         COMMON/ SERVZ2/        TABTOK(135)
         COMMON/ SERVZ3/        TABTIP(300)
         INTEGER TABTOK, TABTIP, SPIS, DIAL
         DIMENSION SPIS(1), TOKN(1), VALLN(1), NAMEI(3), NAMEV(3),
     =             INTO(3)
         DIMENSION HELP(32), RHELP(1)
         INTEGER HELP, BLANK, TIRE, PX, ANM, ANT
         EQUIVALENCE( NWTR,  MASPRT(13)),
     =              ( NWTRE, MASPRT(14)),
     =              ( DIAL,  MASPRT(15)),
     =              ( RHELP(1), HELP(1) )
C
         DATA NAMEI/ '��-�', '� & ', '����' /
         DATA NAMEV/ ' H��', '�殮', '��� ' /
         DATA INTO / '����', '� �', '����' /
         DATA BLANK/'    '/, TIRE/'����'/
C**********************************************************************
         KP = SPIS(1)
         GO TO ( 1,2,3,2,2,2,2 ), KOD
         CALL SUBERR(KOD)
1        CONTINUE
         IF ( DIAL .EQ. 1  .AND. KP .GT. 5 ) KP = 5
         IF ( KP .GT. 9 ) KP = 9
C--> ���HA B�B. CTPOK� ==> KSTR  � KSTR4
         KSTR = 11 + 13 * KP
C---->
         KSTR4 = KSTR/4
C===> B�BO� �A�O�OBKA....
         WRITE(NWTR, 360)
360      FORMAT(1H1,// 20X, 'T����� ���祭�� �᪮��� ����稭'/  )
C--> C�PABKA ...
         ISPIS = 2
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
c@@@@@@@@@@@
         WRITE(99,12000)
12000    FORMAT( '[12;0H',79('�'))
c@@@@@@@@@@@
         WRITE(NWTR,12) ( HELP(I), I=1,KSTR4 )
12       FORMAT( /1X, 32A4 )
         DO 13 I=1,32
13       HELP(I) = BLANK
C
C--->  TEKCT �A�O�OBKA...
         ISPIS = 2
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
         WRITE(99, 17000 ) ( HELP(I), I=1,IH )
17       FORMAT(1X, ' ����饥  ', 9('!': 3A4 ) )
17000    FORMAT('[13;0H',' T���饥  ', 9('�': 3A4 ) )
C===> 2- CTPOKA
         DO 18 I = 1, 32
18       HELP(I) = BLANK
         ISPIS = 2
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
         WRITE(99, 21000 ) ( HELP(I), I=1, IH )
21000    FORMAT( '[14;0H','   �६�  ',  9('�': 3A4 ) )
C--->  3-� CTPOKA
         DO 22 I = 1, 32
22       HELP(I) = 0
C-->  KOL-BO ��CE�  KPX = 2*<KO�-BO �EPEMEHH�X>
         KPX = 2*KP
         ISPIS = 2
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
25       FORMAT(1X, '  [CEK.]  ', 9('!': I5, 1X, I5,1X ) )
         WRITE(99,25000) (HELP(I), I=1,IH )
25000    FORMAT('[15;0H','  [CEK.]  ', 9('�': I5, 1X, I5,1X ) )
C-->  4-� CTPOKA
         DO 26 I = 1, 32
26       HELP(I) = TIRE
         WRITE(99  , 12001)
12001    FORMAT( '[16;0H',79('�'))
         WRITE(NWTR, 1200) ( HELP(I), I=1,KSTR4)
1200     FORMAT(1X,32A4)
         RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
C######################################################################
2        CONTINUE
C--> B�BO� O�EPE�HO�
         DO 27  I=1,9
27       RHELP(I) = 0.0
         IHELP = 1
         ISPIS = 2
         KP = SPIS(1)
         IF ( DIAL .EQ. 1  .AND. KP .GT. 5 ) KP = 5
         IF ( KP .GT. 9 ) KP = 9
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
         WRITE(99,30000) TIME,(RHELP(I), I=1,5  )
30000    FORMAT('[17;0H',G10.5, 9('�': G12.5 )  )
         IF ( KOD .EQ. 2 ) WRITE(NWTR,30) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 4 ) WRITE(NWTR,31) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 5 ) WRITE(NWTR,46) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 6 ) WRITE(NWTR,47) TIME,(RHELP(I), I=1,KP )
         IF ( KOD .EQ. 7 ) WRITE(NWTR,48) TIME,(RHELP(I), I=1,KP )
30       FORMAT(1X,G10.5, 9(':', G12.5 )  )
31       FORMAT(1X,G10.5, 9('*', G12.5 )  )
46       FORMAT(1X,G10.5, 9('T', G12.5 )  )
47       FORMAT(1X,G10.5, 9('@', G12.5 )  )
C31       FORMAT(1X,'*',G9.4, '*',  G12.5, 8( '!', G12.5 )  )
C46       FORMAT(1X,'T',G9.4, 'T',  G12.5, 8( '!', G12.5 )  )
C47       FORMAT(1X,'@',G9.4, '@',  G12.5, 8( '!', G12.5 )  )
48       FORMAT(1X,G10.5, 9('I', G12.5 )  )
         RETURN
C######################################################################
C######################################################################
C######################################################################
C######################################################################
C######################################################################
3        CONTINUE
C--> �AKP�T�
         DO 32 I=1,32
32       HELP(I) = TIRE
         KSTR = 11 + 13 * KP
C--------
         KSTR4 = KSTR/4
         WRITE(NWTR,33) ( HELP(I), I=1, KSTR4 )
         WRITE(99,33000)
33000    FORMAT('[18;0H', 79('�'))
33       FORMAT(1X, 32A4 // )
         RETURN
         END
