         SUBROUTINE READMB( spool, lmasi, mas, irc )
CXXX----KOM��EKC "MS1" --> FOR DOS                          -----------
C   �O��PO�PAMMA:   READMB( spool, lmasi, mas, irc)
C   -------------
C   HA�HA�EH�E:    �TEH�E  MACC�BA mas ������ lmasi c 䠩�� spool
C   ----------
C   METO�:
C   -----
C   �APAMETP�:     irc = 0  - ���⠫�
C   ---------      irc = 1  - ����⨫�� ����� ( ��孠��)
C                  irc = 2 -  �訡��
C                  O�HA �A��C� -> 1024 ��⮬� �� ���� ������
C                  �� ����� = 0 �⠥� ���� ᫮��
CZZZ-------------------------------------------------------------------
         DIMENSION mas(*)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         integer spool
         data lzap/256/
         lmas = lmasi
         IF ( lmasi .LE. 0 ) lmas = 1
         if (lmas .GT. lzap ) go to 100
         READ(SPOOL, END=20, ERR=21)  (mas(I), I=1,lmas)
         irc = 0
         RETURN
C@@@@@@@
100      continue
         k = lmas/lzap
         k0 = k*lzap
         k1 = lmas - k0
         do 1 i=1,k0,lzap
1           READ(SPOOL, END=20, ERR=21)  (mas(j), j=i,lzap+i-1)
         if (k1 .NE. 0 )
     +          READ(SPOOL, END=20, ERR=21)  (mas(j), j=k0+1,lmas)
         irc = 0
         RETURN
C@@@@
20       irc = 1
         RETURN
21       irc = 2
         RETURN
         END
