         SUBROUTINE DRMET0(KOD,IBIG)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.5                      ------------
C   �O��PO�PAMMA:   DRMET0(KOD,IBIG)
C   ------------
C   HA�HA�EH�E:    O�PE�E�EH�� �PO���� PA�OT� KOM��EKCA -
C   ----------     �A�O�EH�E "USER" �O�E� B MASPRT:
C ****  13.06.90  ===> �����஢��� ��⠭���� ���. ���祭�� �. makromod
C
C                �P� KOD = 1 --> �CTAHAB��BAET �HA�EH�E �O "�MO��AH��"
C                                  B�BO� �EPE� NWTRE-KAHA� "CTAH�APTH�E
C                                   �HA�EH��"
C
C                    KOD = 2 --> BBO� �EPE� NRDR KAHA� TEK���X �HA�EH��
C                                �O�E� MASPRT B B��E:
C                                <�M�>=<�HA�EH�E> [,<�M�>=<�HA�EH�E>...]
C                                B�BO� HA NWTR-KAHA� "��MEHEHH�E
C                                 �HA�EH��"
C
C                    KOD = 3 --> B�BO� HA NWTR TEK���X �HA�EH��
C                                �O�E� MASPRT "TEK���E �HA�EH��"
C
C                �P� KOD = 4 --> �CTAHAB��BAET �HA�EH�� �O�E� MASPRT
C                                �O "�MO��AH��"
C                                B�BO� HA NWTR-KAHA� "CTAH�APTH�E
C                                �HA�EH��"
C
C                �P� IBIG =1 --> "PAC��PEHH��" B�BO� �H�OPMA��� :
C                        "O�O�HA�EH�E"  "�HA�EH�E"  "HA�HA�EH�E"
C                         -----------    --------    ----------
C                            ...           ...          ...
C                            ...           ...          ...
C                            ...           ...          ...
C   METO�:      BBO�, �A�AH�E �CXO�HO� �H�OPMA���
C   -----
C   �APAMETP�:  CM. HA�HA�EH�E
C   ---------
C   �O��PO�PAMM�:  1. COMMON PROF1,SERVZ7, INTPRT
C   ------------   2. APFNML,SUBERR,PRINTD
C
C   �P�ME�AH�E:    1. �HA�EH�� �O "�MO��AH��" �CTAHAB��BA�TC�
C   ----------        B �/� INTPRT;
C
C   BH�MAH�E:      ��MEH��TE  �APAMETP�  TO��KO EC�� BAM OT�ET��BO
C  !!!!!!!!!!      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C                  �CHO �X B���H�E HA PA�OTOC�OCO�HOCT� C�CTEM�;
C                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON /SERVZ7/ MAKMOD(3),MAKSEQ,NAMEB
         DIMENSION R(200),KDIV(10)
         EQUIVALENCE( MASPRT(1),R(1))
C----------------------------------------------------------------------
         INTEGER  FTEST, FRUNGE, FRCPU, SEQTIM, SEQTRX,TAB,TAB1,TAB2
         INTEGER DIAL,WTRE,TBUF,TAB3,TAB4, TAB5, TAB6
         DIMENSION TAB(780),TAB1(130),TAB2(130),TAB3(130),RAB(780)
         DIMENSION TAB4(130),TAB5(130),TAB6(130)
         DIMENSION TBUF(32),RBUF(32)
         EQUIVALENCE( TAB(1),   RAB(1))
C
         EQUIVALENCE( TAB(1),   TAB1(1))
         EQUIVALENCE( TAB(131), TAB2(1))
         EQUIVALENCE( TAB(261), TAB3(1))
         EQUIVALENCE( TAB(391), TAB4(1))
         EQUIVALENCE( TAB(521), TAB5(1))
         EQUIVALENCE( TAB(651), TAB6(1))
C
         REAL MU, ICLOSE
C----------------------------------------------------------------------
C----------------------------------------------------------------------
         EQUIVALENCE( NRDR,   TAB(50))
         EQUIVALENCE( NWTR,   TAB(63))
         EQUIVALENCE( NWTRE,  TAB(76))
         EQUIVALENCE( NWTRD,  TAB(89))
         EQUIVALENCE( NSPOOL, TAB(102))
         INTEGER PONRUN
C+++++++++++++++++++++56++i - �� ������� ����� ���-�� ��ப+++++++++++
       DATA  KST/13/,KTAB/59/,IB32/32/
       DATA KKDIV/4/,KDIV/40,48,94,116,6*0/
       DATA TAB1/0,5,'N','M','B','A','0',' ',' ',' ',100,0,2,
     2           0,6,'N','D','K','N','O','T',' ',' ',650,0,6,
     3           0,6,'N','D','P','A','R','0',' ',' ',500,0,9,
     4           0,4,'N','R','D','R',' ',' ',' ',' ',  5,0,12,
     5           0,4,'N','W','T','R',' ',' ',' ',' ',  3,0,13,
     6           0,5,'N','W','T','R','E',' ',' ',' ',  6,0,14,
     7           0,5,'N','W','T','R','D',' ',' ',' ',  8,0,16,
     8           0,6,'N','S','P','O','O','L',' ',' ',  9,0,18,
     9           1,2,'M','U',' ',' ',' ',' ',' ',' ',000,0,31,
     +           1,6,'E','P','S','K','O','M',' ',' ',000,0,39/
       DATA TAB2/1,6,'H','0','D','A','T','A',' ',' ',000,0,40,
     2           1,5,'H','O','U','T','T',' ',' ',' ',000,0,48,
     3           1,6,'Z','E','R','O','C','P',' ',' ',000,0,50,
     4           1,6,'G','V','E','N','T','6',' ',' ',000,0,59,
     5           0,5,'F','T','E','S','T',' ',' ',' ',  0,0,60,
     6           1,5,'H','X','M','I','N',' ',' ',' ',000,0,62,
     7           0,6,'S','E','Q','T','R','X',' ',' ',  1,0,64,
     8           0,5,'M','I','T','E','R',' ',' ',' ', 10,0,66,
     9           1,5,'E','I','T','E','R',' ',' ',' ',000,0,67,
     +           0,6,'F','R','U','N','G','E',' ',' ',  0,0,68/
       DATA TAB3/1,6,'E','R','U','N','G','E',' ',' ',000,0,69,
     2           0,6,'N','R','U','N','G','E',' ',' ', 10,0,70,
     3           0,6,'S','E','Q','T','I','M',' ',' ', 10,0,71,
     4           1,5,'H','0','M','I','N',' ',' ',' ',000,0,72,
     5           0,6,'N','F','I','L','E','R',' ',' ',  4,0,83,
     6           1,6,'I','C','L','O','S','E',' ',' ',000,0,77,
     7           1,5,'U','O','P','E','N',' ',' ',' ',000,0,78,
     8           0,6,'P','O','N','R','U','N',' ',' ',  0,0,86,
     9           0,4,'N','M','A','K',' ',' ',' ',' ',  1,0,89,
     +           1,6,'D','T','M','C','I','U',' ',' ',000,0,94/
       DATA TAB4/1,6,'D','T','C','H','E','K',' ',' ',000,0,90,
     2           0,6,'N','F','C','H','E','K',' ',' ', 10,0,95,
     3           0,6,'N','F','A','C','H','K',' ',' ', 11,0,97,
     4           0,4,'D','I','A','L',' ',' ',' ',' ',  0,0,15,
     5           0,3,'P','O','N',' ',' ',' ',' ',' ',  0,0,56,
     6           1,5,'S','E','T','K','A',' ',' ',' ',000,0,63,
     7           1,4,'F','S','T','D',' ',' ',' ',' ',000,0,100,
     8           1,5,'D','F','F','U','R',' ',' ',' ',000,0,101,
     9           1,5,'F','0','F','U','R',' ',' ',' ',000,0,102,
     +           1,5,'F','M','F','U','R',' ',' ',' ',000,0,103/
       DATA TAB5/1,5,'P','P','F','U','R',' ',' ',' ',000,0,104,
     2           1,6,'I','Y','S','T','A','V',' ',' ',000,0,105,
     3           0,6,'P','P','V','E','N','T',' ',' ',  0,0,106,
     4           0,6,'M','E','T','V','E','T',' ',' ',  2,0,107,
     5           0,4,'I','P','O','L',' ',' ',' ',' ',  0,0,108,
     6           0,6,'K','O','D','S','Y','S',' ',' ',  2,0,109,
     7           1,6,'E','P','S','S','Y','S',' ',' ',000,0,110,
     8           0,5,'K','O','D','M','U',' ',' ',' ',  1,0,113,
     9           0,6,'K','O','D','M','U','M',' ',' ',  1,0,114,
     +           1,5,'T','0','O','U','T',' ',' ',' ',000,0,115/
       DATA TAB6/1,6,'H','0','O','U','T','T',' ',' ',000,0,116,
     2           0,5,'I','N','T','I','M',' ',' ',' ',000,0,119,
     3           0,5,'N','S','X','E','M',' ',' ',' ',000,0,120,
     4           0,5,'N','E','X','T','R',' ',' ',' ',013,0,123,
     5           0,6,'N','E','X','T','R','D',' ',' ',014,0,124,
     6           0,0,' ',' ',' ',' ',' ',' ',' ',' ',000,0,  0,
     7           0,0,' ',' ',' ',' ',' ',' ',' ',' ',000,0,  0,
     8           0,0,' ',' ',' ',' ',' ',' ',' ',' ',000,0,  0,
     9           0,0,' ',' ',' ',' ',' ',' ',' ',' ',000,0,  0,
     +           0,0,' ',' ',' ',' ',' ',' ',' ',' ',000,0,  0/
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C---------------------------------------------------------------------
      GO TO (1,2,3,4),KOD
      CALL SUBERR(KOD)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   1  CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C******CALL INTPRT
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      N=MASPRT(14)
      WRITE(N,21)
  21  FORMAT( /13X,'C � � � � � � � � � �   � � � � � � � �'/
     *15X,'------------------------------------'/)
C----------------------------------------------------------------------
C--> PAC�E�ATKA MACC�BOB
C!!! �PE�BAP�TE�8HO �EPEC��KA MASPRT=>TAB, R=>RAB !!!
      CALL PKROSS(1,TAB,RAB,MASPRT,R,KTAB,KST)
      CALL PRINTD(KOD,IBIG,0,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
       IF ( IBIG .EQ. 1 ) WRITE(N,700)
  700  FORMAT(1X,71('-'))
      IF (IBIG.EQ.1) GO TO 10
      CALL PRINTD(KOD,IBIG,1,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
      GO TO 10
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   2  CONTINUE
C--> �A�O�HEH�E �O�8�OBATE�EM �O�E� B MASPRT
C!!! �PE�BAP�TE�8HO �EPEC��KA MASPRT=>TAB, R=>RAB !!!
      CALL PKROSS(1,TAB,RAB,MASPRT,R,KTAB,KST)
      CALL APFNML(NRDR,NWTRE,TAB,RAB,KTAB,KST,KODEOJ)
C----------------------------------------------------------------------
C--> �EPEBO� �O KDIV-�KA�ATE�� �� ��. �PA�.
      K=KST*KTAB
      DO 800 ITAB=1,K,KST
C$$ BORU:
      IF ( TAB(ITAB+11) .EQ. 0 ) GO TO 800
      DO 801 IRR=1,KKDIV
C$$$$$IF (TAB(ITAB+11).EQ.0.OR.TAB(ITAB+12).NE.KDIV(IRR)) GO TO 800
C$$$ BORU:
      IF ( TAB(ITAB+12) .NE. KDIV(IRR) ) GO TO 801
C     WRITE(N,333) (TAB(ITAB+I-1),I=3,10),RAB(ITAB+10)
C333  FORMAT(5X,'*** ��MEH�EM ',8A1,2X,G12.5)
      RAB(ITAB+10)=RAB(ITAB+10)/1.8E+4
C     WRITE(N,334) (TAB(ITAB+I-1),I=3,10),RAB(ITAB+10)
C334  FORMAT(5X,'*** ��MEH��� ',8A1,2X,G12.5)
C$$$ BORU:
      GO TO 800
  801 CONTINUE
  800 CONTINUE
      N=NWTR
      WRITE(N,22)
  22  FORMAT( /15X,'� � � � � � � �  � � � � � � � � � � � � �'/
     *15X,'------------------------------------------'/)
C----------------------------------------------------------------------
C!!! �EPEC��KA TAB=>MASPRT, RAB=>R !!!
      CALL PKROSS(0,TAB,RAB,MASPRT,R,KTAB,KST)
C--> PAC�E�ATKA MACC�BOB
C!!! �PE�BAP�TE�8HO �EPEC��KA MASPRT=>TAB, R=>RAB !!!
      CALL PKROSS(1,TAB,RAB,MASPRT,R,KTAB,KST)
      CALL PRINTD(KOD,IBIG,0,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
      IF ( IBIG .EQ. 1 ) WRITE(N,700)
      IF (IBIG.EQ.1) GO TO 10
      CALL PRINTD(KOD,IBIG,1,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
      GO TO 10
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C--> PAC�E�ATKA TEK���X �HA�EH�� �O�E� MASPRT
  3   CONTINUE
      N=NWTR
      WRITE(N,23)
  23  FORMAT( /15X,'T � � � � � �  � � � � � � � �     '/
     *15X,'------------------------------'/)
C----------------------------------------------------------------------
C--> PAC�E�ATKA MACC�BOB
C!!! �PE�BAP�TE�8HO �EPEC��KA MASPRT=>TAB, R=>RAB !!!
      CALL PKROSS(1,TAB,RAB,MASPRT,R,KTAB,KST)
      CALL PRINTD(KOD,IBIG,0,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
      IF ( IBIG .EQ. 1 ) WRITE(N,700)
      IF (IBIG.EQ.1) GO TO 10
      CALL PRINTD(KOD,IBIG,1,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
      GO TO 10
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   4  CONTINUE
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C******CALL INTPRT
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      N=MASPRT(13)
      WRITE(N,21)
C--> PAC�E�ATKA MACC�BOB
C!!! �PE�BAP�TE�8HO �EPEC��KA MASPRT=>TAB, R=>RAB !!!
      CALL PKROSS(1,TAB,RAB,MASPRT,R,KTAB,KST)
      CALL PRINTD(KOD,IBIG,0,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
       IF ( IBIG .EQ. 1 ) WRITE(N,700)
      IF (IBIG.EQ.1) GO TO 10
      CALL PRINTD(KOD,IBIG,1,TAB,RAB,TBUF,RBUF,KTAB,KST,IB32,N)
C----------------------------------------------------------------------
       WRITE(N,700)
C----------------------------------------------------------------------
   10 RETURN
      END
