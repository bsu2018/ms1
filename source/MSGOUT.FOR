         SUBROUTINE MSGOUT( MSG )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C     �O��PO�PAMMA : MSGOUT ( MSG )
C     ------------
C     HA�HA�EH�E :  B�BO� PAC��PEHHO�O TEKCTOBO�O COO��EH�� HA NWTR
C    ------------
C     METO�:        B��OP HOMEPOB �� BXO�HO�O MACC�BA MSG � �E�AT�.
C     -----
C     �APAMETP�:   MACC�B MSG => MSG(1) - KO���ECTBO �AH�T�X �O�����
C     ---------                  (HOMEPOB COO��EH��, KOTOP�X HEO�XO��MO
C                                 B�BECT�) B MSG(2) ... MSG( MSG(1) );
C                                MSG(J) => HOMEP COO��EH��;
C    �O��PO�PAMM�:  COMMON / PROF1 /
C    ------------
C    �P�ME�AH�E: 1. �E�ATE��HO O�EC�E��T� ���H� CTPOK� COO��EH�� HE
C    ----------     �O�EE 80 C�MBO�OB;
C                2. B TEK��E� BEPC��  �P���CMOTPEHHO HE �O�EE MMSG
C                   COO��EH�� [ 100 ];
C                3. PEA���OBAHO MAXMSG COO��EH��  [ 33 ];
C                4. ��� �O �A��C� HOMEPA COO��EH�� B MSG(*):
C                   I = MSG(1); MSG(I+2) = HOMEP; MSG(1) = I + 1;
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSG(1)
         COMMON /PROF1/ MASPRT(200)
        DATA MAXMSG/35/, MMSG/100/
C             *******************
         NWTR = MASPRT(13)
         NWTRE = MASPRT(14)
         MAX = MSG(1)
         IF ( MAX .EQ. 0 ) RETURN
         IF (MAX .LT. 0  .OR.  MAX .GT. 100 ) GO TO 667
         IMAX = MAX + 1
         DO 666 I=2,IMAX
         IF ( MSG(I) .GT. MAXMSG ) WRITE(NWTR,668) MSG(I)
C----------------------------------------------------------------------
         IF ( MSG(I) .EQ. 1 ) WRITE(NWTR,1)
         IF ( MSG(I) .EQ. 2 ) WRITE(NWTR, 2)
         IF ( MSG(I) .EQ. 3 ) WRITE(NWTR, 3)
         IF ( MSG(I) .EQ. 4 ) WRITE(NWTR, 4)
         IF ( MSG(I) .EQ. 5 ) WRITE(NWTR, 5)
         IF ( MSG(I) .EQ. 6 ) WRITE(NWTR, 6)
         IF ( MSG(I) .EQ. 7 ) WRITE(NWTR, 7)
         IF ( MSG(I) .EQ. 8 ) WRITE(NWTR, 8)
         IF ( MSG(I) .EQ. 9 ) WRITE(NWTR, 9)
         IF ( MSG(I) .EQ.10 ) WRITE(NWTR, 10)
         IF ( MSG(I) .EQ.11 ) WRITE(NWTR, 11)
         IF ( MSG(I) .EQ.12 ) WRITE(NWTR, 12)
         IF ( MSG(I) .EQ.13 ) WRITE(NWTR, 13)
         IF ( MSG(I) .EQ.14 ) WRITE(NWTR, 14)
         IF ( MSG(I) .EQ.15 ) WRITE(NWTR, 15)
         IF ( MSG(I) .EQ.16 ) WRITE(NWTR, 16)
         IF ( MSG(I) .EQ.17 ) WRITE(NWTR, 17)
         IF ( MSG(I) .EQ.18 ) WRITE(NWTR, 18)
         IF ( MSG(I) .EQ.19 ) WRITE(NWTR, 19)
         IF ( MSG(I) .EQ.20 ) WRITE(NWTR, 20)
         IF ( MSG(I) .EQ.21 ) WRITE(NWTR, 21)
         IF ( MSG(I) .EQ.22 ) WRITE(NWTR, 22)
         IF ( MSG(I) .EQ.23 ) WRITE(NWTR, 23)
         IF ( MSG(I) .EQ.24 ) WRITE(NWTR, 24)
         IF ( MSG(I) .EQ.25 ) WRITE(NWTR, 25)
         IF ( MSG(I) .EQ.26 ) WRITE(NWTR, 26)
         IF ( MSG(I) .EQ.27 ) WRITE(NWTR, 27)
         IF ( MSG(I) .EQ.28 ) WRITE(NWTR, 28)
         IF ( MSG(I) .EQ.29 ) WRITE(NWTR, 29)
         IF ( MSG(I) .EQ.30 ) WRITE(NWTR, 30)
         IF ( MSG(I) .EQ.31 ) WRITE(NWTR, 31)
         IF ( MSG(I) .EQ.32 ) WRITE(NWTR, 32)
         IF ( MSG(I) .EQ.33 ) WRITE(NWTR, 33)
         IF ( MSG(I) .EQ.34 ) WRITE(NWTR, 34)
         IF ( MSG(I) .EQ.35 ) WRITE(NWTR, 35)
C
C----------------------------------------------------------------------
666      CONTINUE
         MSG(1) = 0
         RETURN
667      CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 500) MAX
500      FORMAT(' *** ���. ᡮ� �� �맮�� MSGOUT, MSG(1)=', I12)
         CALL SUBERR(1)
         RETURN
668      FORMAT (' *** C���饭�� # ', I4, ' �� ॠ�������� ***')
C######################################################################
1        FORMAT(' *** H� �������� ��砫� �����᪮� ��ப� ***')
C---> 2
2        FORMAT(' *** B������� �᫮ ᫨誮� ������ ***'/
     =          ' *** B������� - �ய�� ࠧ����⥫쭮�� �஡��� ***')
C--->3
3        FORMAT(' *** B������� �᫮ ᮤ�ন� �� �����⨬� ᨬ��� ***')
C----->4
4        FORMAT(' *** T�� ���஬����� �訡�祭 ��� �� ॠ������� ***')
C-----> 5
5        FORMAT(' *** H� ��।���� ����� ���஬����� ***'/
     =  ' *** ���ᠭ�� ᯨ᪠ "�" [ ���� ]  �ਭ��� ���������� ***')
C-----> 6
6        FORMAT(' *** H� �����⨬� ����� ��� ������祭�� 㧫� ***')
C---->  7
7        FORMAT(' *** H� ��।���� ����� ���஬����� ***'/
     = ' *** ���ᠭ�� ᯨ᪠ "�" [ �APAMETP� ] �ਭ��� ���������� ***')
C----->8
8        FORMAT(' *** ������ ���஬����� ��ࠬ���� �� �㦭� ***')
C---->9
9        FORMAT(' *** B �����᪮� ��ப� ���ᠭ�� 㪠���� �� ��� ����
     =��� ***')
C---->10
10       FORMAT(' *** H���襭� ������� ᯨ᪠ "�" - ��������� "MAS" **
     =*')
C---->11
11       FORMAT(' *** H�� "<" � ᯨ᪥ ��ࠬ��஢ [ "MAS" ] ***')
C----->12
12     FORMAT(' *** H���襭� ������� ᯨ᪠ "�" - ��������� "FT" ***')
C------>13
13       FORMAT(' *** B��஥���� �㭪�� "FT" ?? �� ॠ�������� ***')
C------>14
14    FORMAT(' *** B ᯨ᪥ "�" ���ᠭ�� "FT" ��� ��࠭��⥫� ">" ***')
C------>15
15       FORMAT(' *** B ⥪�饬 ᯨ᪥ ����� ��ࠬ��஢ ***')
C---->16
16       FORMAT(' *** C��᮪ FT # "<......>" - ��� ***')
C---->17
17       FORMAT(' *** H�� "<" � ᯨ᪥ ��ࠬ��஢ [ "FT" ] ***')
C---->18
18       FORMAT(' *** H���襭� ������� ᯨ᪠ "�" - ��������� "PSUB" *
     =**')
C---->19
19       FORMAT(' *** H�� "<" � ᯨ᪥ ��ࠬ��஢ "PSUB" ***')
C---->20
20       FORMAT(' *** H��������� ����� �����᪮� ��ப� - ���� ������
     =� ᯨ᪥ ***')
C---->21
21       FORMAT(' *** H��������� ��砫� ���ᯨ᪠ ***' / ' *** ��� "V",
     = "U", "I", "GAMMA", "OMEGA", "M�M", "MBH" ***')
C--->22
22       FORMAT(' *** C��᮪ "�A�POC B�BO�A" ����让 - �ᥪ����� ***')
C--->23
23       FORMAT(' *** O⬥祭���� 㧫� � 楯� ��� ***')
C--->24
24       FORMAT(' *** O⬥祭���� ������� 楯� �� ᮤ�ন� ***')
C---->25
25       FORMAT(' *** O⬥祭��� "�HA�EH�E" �� �����⨬� ***')
C----> 26
26       FORMAT(' *** O⬥祭�� �������, ����襭��� �� �뢮� ����稭,
     = �� ᮤ�ন� ***')
C--->27
27       FORMAT(' *** H� ��।���� ����� �����  ***')
C--->28
28       FORMAT(' *** H���� �裡 ����� ����� �����  ***')
C--->29
29       FORMAT(' *** C��� � ������, ����騬 ����訩 ����� ***')
C--->30
30       FORMAT(' *** T�� �裡 �訡�祭  ***')
C--->31
31       FORMAT(' *** C��� �� ��।����� ***')
C--->32
32       FORMAT(' *** B����� �ࠪ���⨪� �����, �� ����� ���ᠭ��',
     *                ' �痢� *** ' )
C--->33
33       FORMAT(' *** H���襭 ���冷� ᫥������� ��ࠬ��஢  ***')
C--->34
34       FORMAT(' *** O⬥祭�� 㧥� �� ��࠭���� � #SPOOL ***')
C---> 35
35    FORMAT(' *** O⬥祭��� "��������" �� ��࠭���� � #SPOOL ***')
C
C
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
