         SUBROUTINE SEQDIR(KD, KAR, DAT, IRC )
CXXX----KOM��EKC "MS1" --> BEPC�� 2.0
C   �O��PO�PAMMA:   SEQDIR(KD, KAR, DAT, IRC )
C   -------------
C   HA�HA�EH�E:    MOH�TOP B��E�EH�� � AHA���A ��PEKT�B
C   ----------     ************************************
C
C
C   METO�:         C�HT. PA��OP, CPABHEH�E C �A��OHOM;
C   -----           $$$ OT 2.08.89 ===> �EPEBO� �MEH� ��P. B CTP. ��KB�.
C
C   �APAMETP�:     KD - KO� B��E�EHHO� ��PEKT�B�:
C   ---------
C
C----------------------------------------------------------------------
C    KD     �M�         �/� O�PA�OTK�  ���HA  <ECT8 �� ��C�OB�E �AHH�E>
C----------------------------------------------------------------------
C    0       %           SEQDIR          1          HET
C    1      #�M�         SEQDIR          4          HET
C    2      #�P�M        SEQDIR          5          HET
C    3      #PE��M       DRREG0          6          HET
C    4      #�E��        DRCIRC          5          ECT8
C    5      #METO�       DRMET0          6          HET
C    6      #�E�         DRPRNT          4          ECT8
C    7      #TAB         DRTAB0          4          ECT8
C    8      #�PA�        DRGRF0          5          ECT8
C    9      #�PA�1       DRGRF1          6          ECT8
C   10      #C�ET        DRDIG0          5          ECT8
C   11      #CTO�        MAIN_PGM        5          HET
C   12      #COXP        DRSAV0          5          ECT8
C   13      #BOCT        DRRES0          5          ECT8
C   14      #��A�        SEQDIR          5          HET
C   15      #C�PAB       DRDAT0          6          HET
C   16      HELP         DRHELP          4          HET
C   17      #X�E��       DRCIRX          6          ECT8
C   18      #�C�ET       DRDIGP          6          ECT8
C   19      #ACOXP       DRSAVA          6          ECT8
C   20      #C�EKTP      DRFURE          7          ECT8
C   21      #MO�HOCT�    DRPPPP          9          ECT8
C   22      #MPE��M      DRRMMM          7          HET
C   23      #����        DROUT0          5          ����
C   24      #SPOOL       DRSPOL          6          ���
C   25      #������      DRREGS          7          ����
C   26      #�������     DRREGL          8          ����
C----------------------------------------------------------------------
C
C                  KAR - MACC�B ��PEKT�B�;
C                  DAT - MACC�B �AHH�X  ��PEKT�B�;
C
C                  IRC = 1 -  O�EPE�H�� ��PEKT�B� �P�H���;
C                      = 2 -  KOHE� �AHH�X HA C�CTEMHOM NRDR;
C
C   �O��PO�PAMM�:  1. COMMON PROF1, SERVZ4,  SERVZ5
C   -------------  2. GETDAT, SEQ1, MOVE1;
C                  3. APFCAS - �EPEBO� B �O�8��E ��KB�....
C                              �MEH ��PEKT�B...
C   �P�ME�AH�E:    1.  <# >  �  <#BCE>  -  C�HOH�M�;
C   ----------     2.  <%>  �O KOH�A CTPOK�  -  KOMEHTAP��;
C  OT 4.04.89 --> "+"  �EPBA� ��PEKT�BA KOHTPO��P�ETC� HA #��A�....
C  �� 13.06.90 --> SEQ1 �������� �� SEQ1M ---  ����� ����� ��४⨢�
C
CZZZ-------------------------------------------------------------------
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/SERVZ4/ NAMECR(80)
         COMMON/SERVZ5/ KCOM, COMENT(100)
         DIMENSION KOMENT(100)
         EQUIVALENCE( COMENT, KOMENT )
         DIMENSION KAR(*), DAT(*)
         DIMENSION YES(2), NO(3)
         INTEGER YES, NO, DIEZ, BCE, ABCE
         DIMENSION D1(4), D2(5), D3(6), D4(5), D5(6), D6(4),
     = D20(7),     D7(4), D8(5), D9(6), D10(5), D11(5), D12(5),
     = D21(9),     D13(5), D14(5), D15(6), D16(4), D17(6),D18(6),D19(6)
         INTEGER D1, D2, D3, D4, D5, D6, D7, D8, D9, D10,
     = D20, D21, D11, D12, D13, D14, D15, D16, D17, D18, D19
         DIMENSION D22(7),d23(5),D24(6),D25(7),D26(8)
         INTEGER D22,d23, D24 ,D25,D26
         EQUIVALENCE( MASPRT(12), NRDR)
         EQUIVALENCE( MASPRT(13), NWTR)
         EQUIVALENCE( MASPRT(14), NWTRE)
         EQUIVALENCE( MASPRT(15), DIAL)
         EQUIVALENCE( MASPRT(127), NZRDR)
         INTEGER DIAL, BLANK, FDIAL
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         DATA YES/'�','A'/, NO/'H', 'E', 'T' /
         DATA DIEZ/'#'/, BCE/'#BCE'/, ABCE/'#   '/
         DATA ICOM/'%'/, ICOM1/'%   '/ , K72/72/, K21/21/
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C----------------------------------------------------------------------
         DATA D1/ '#', '�', 'M', '�'            /
         DATA D2/ '#', '�', 'P', '�', 'M'       /
         DATA D3/ '#', 'P', 'E', '�', '�', 'M'  /
         DATA D4/ '#', '�', 'E', '�', '�'       /
         DATA D5/ '#', 'M', 'E', 'T', 'O', '�'  /
         DATA D6/ '#', '�', 'E', '�'            /
         DATA D7/ '#', 'T', 'A', '�'            /
         DATA D8/ '#', '�', 'P', 'A', '�'       /
         DATA D9/ '#', '�', 'P', 'A', '�', '1'  /
         DATA D10/ '#', 'C', '�', 'E', 'T'      /
         DATA D11/ '#', 'C', 'T', 'O', '�'      /
         DATA D12/ '#', 'C', 'O', 'X', '�'      /
         DATA D13/ '#', 'B', 'O', 'C', 'T'      /
         DATA D14/ '#', '�', '�', 'A', '�'      /
         DATA D15/ '#', 'C', '�', 'P', 'A', 'B' /
         DATA D16/ 'H', 'E', 'L', 'P'           /
         DATA D17/ '#', 'X', '�', 'E', '�', '�' /
         DATA D18/ '#', '�', 'C', '�', 'E', 'T' /
         DATA D19/ '#', 'A', 'C', 'O', 'X', 'P' /
         DATA D20/ '#', 'C', '�', 'E', 'K', 'T', 'P' /
         DATA D21/ '#', 'M', 'O', '�', 'H', 'O', 'C', 'T', '�' /
         DATA D22/ '#', 'M', 'P', 'E', '�', '�', 'M' /
         DATA D23/ '#', 'B', 'H', 'E', '�'           /
         DATA D24/ '#', 'S', 'P', 'O', 'O', 'L'      /
         DATA D25/ '#', 'P', 'E', '�', '�', 'A', '�' /
         DATA D26/ '#', 'P',  'E', '�', 'B', 'O', 'C', 'T'     /
C----------------------------------------------------------------------
         DATA BLANK /' '/,FDIAL/0/
C######################################################################
         IRC = 1
666      CONTINUE
         DO 667 I = 1, K21
667      DAT(I) = 0.0
         READ(NRDR, 100, END = 9999 ) (KAR(I), I=1,80 )
         NZRDR = NZRDR + 1
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
C --->   �EPEBO� �� MA�EH�K�X ��KB B �O�예E ......
         CALL APFCAS(1,80,KAR,KAR)
C@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
100      FORMAT(80A1)
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         CALL SEQ1M(KAR, D14, 5, I)
         GO TO (14),I
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         IF ( DIAL .EQ. 0 ) WRITE(NWTR,101) (KAR(I), I=1,80)
         IF ( DIAL .EQ. 1 ) WRITE(NWTR,1010) (KAR(I), I=1,80)
101      FORMAT(/20X, 80A1)
1010     FORMAT(/ 80A1)
         IK = 1
C--> C�TO ��� ��PEKT�B.....

!!         WRITE(*, 1022) (KAR(iiiii),iiiii=1,80)
!!         WRITE(NWTR, 1022) (KAR(iiiii),iiiii=1,80) 

         IF ( KAR(1) .EQ. ICOM ) GO TO 666
C  ->  �TO    C  �EM   CKO��KO  1-�A, 2=HET:::::::
         
         CALL SEQ1M(KAR, D1, 4, I)
         GO TO (1),I
         CALL SEQ1M(KAR, D2, 5, I)
         GO TO (2),I
         CALL SEQ1M(KAR, D3, 6, I)
         GO TO (3),I
         CALL SEQ1M(KAR, D4, 5, I)
         GO TO (4),I
         CALL SEQ1M(KAR, D5, 6, I)
         GO TO (5),I
         CALL SEQ1M(KAR, D6, 4, I)
         GO TO (6),I
         CALL SEQ1M(KAR, D7, 4, I)
         GO TO (7),I
         CALL SEQ1M(KAR, D9, 6, I)
         GO TO (9),I
         CALL SEQ1M(KAR, D8, 5, I)
         GO TO (8),I
         CALL SEQ1M(KAR, D10, 5, I)
         GO TO (10),I
         CALL SEQ1M(KAR, D11, 5, I)
         GO TO (11),I
         CALL SEQ1M(KAR, D12, 5, I)
         GO TO (12),I
         CALL SEQ1M(KAR, D13, 5, I)
         GO TO (13),I
C+       CALL SEQ1M(KAR, D14, 5, I)
C+       GO TO (14),I
         CALL SEQ1M(KAR, D15, 6, I)
         GO TO (15),I
         CALL SEQ1M(KAR, D16, 4, I)
         GO TO (16),I
         CALL SEQ1M( KAR, D17, 6, I )
         GO TO (17),I
         CALL SEQ1M(KAR, D18, 6, I)
         GO TO (18),I
         CALL SEQ1M( KAR, D19, 6, I )
         GO TO (19),I
         CALL SEQ1M( KAR, D20, 7, I )
         GO TO (20),I
         CALL SEQ1M( KAR, D21, 9, I )
         GO TO (21),I
         CALL SEQ1M( KAR, D22, 7, I )
         GO TO (22),I
         CALL SEQ1M( KAR, D23, 5, I )
         GO TO (23),I
         CALL SEQ1M( KAR, D24, 6, I )
         GO TO (24),I
         CALL SEQ1M( KAR, D25, 7, I )
         GO TO (25),I
         CALL SEQ1M( KAR, D26, 8, I )
         GO TO (26),I
C--->  MO�HO ��EC� AKT�B���POBAT8 C�HOH�M�:::::::
C######################################################################
         WRITE(NWTR, 102)
 102     FORMAT(/' *** ��४⨢� �� �������� - ��������: ��� -->',
     =    ' "#BCE" ��� ";" ***' )
         WRITE(NWTR, 1022) (KAR(iiiii),iiiii=1,80)
 1022    FORMAT('*** B�������� ��ப�:'/80A1/)
         GO TO 666
C######################################################################
1        CONTINUE
C---> #�M�
         KD = 1
         CALL MOVE1( NAMECR, KAR, 80 )
         GO TO 666
2        CONTINUE
C---> #�P�M
         KD = 2
         KCOM = 0
2002     CONTINUE
         KCOM = KCOM + 1
         IF ( KCOM .GT. 6 ) GO TO 2003
         I1 = (KCOM-1)*20 + 1
2000     CONTINUE
         READ(NRDR, 203, END = 9999) (KAR(I), I=1, 20 )
         NZRDR = NZRDR + 1
         IF ( DIAL .EQ. 0 ) WRITE(NWTR, 201 ) ( KAR(I), I=1, 20 )
         IF ( DIAL .EQ. 1 ) WRITE(NWTR, 2010 ) ( KAR(I), I=1, 20 )
203      FORMAT(20A4)
201      FORMAT(20X, 20A4)
2010     FORMAT(     20A4)
         IF ( KAR(1) .EQ. ICOM ) GO TO 2000
         IF ( KAR(1) .EQ. ICOM1 ) GO TO 2000
         KCOM = KCOM - 1
         IF ( KAR(1) .EQ. BCE ) GO TO 666
         IF ( KAR(1) .EQ. ABCE ) GO TO 666
         KCOM = KCOM + 1
         CALL MOVE1( KOMENT(I1), KAR, 20 )
         GO TO 2002
2003     CONTINUE
         WRITE(NWTR, 104)
104     FORMAT(/' *** ��� �ਭ﫨 6-� ��ப� #�P�M, � ��� "# " ��� "#BCE
     ="? ***'/ ' *** B������� - ���� ���ଠ樨 "�諠" � #�P�M ***'/
     =      ' *** ��� �����⥫�� ***'/)
         GO TO 666
C######################################################################
14       CONTINUE
C   #��A�
         KD = 14
         DIAL = 0
C-- ��E �O�E��
         DO 14000 I = 1, K72
         IK = I
         IF ( KAR(I) .EQ. BLANK )  GO TO 14001
14000    CONTINUE
         WRITE(NWTR,14005) DIAL
14005    FORMAT(/5X,'*** #��A�  ��⠭�����: "HET" (DIAL=',I1,') ***')
         GO TO 666
14001    CONTINUE
C--> ��EPEM �PO�E��
       I1= IK
       DO 14002 I=I1, K72
       IK = I
       IF( KAR(I) .NE. BLANK ) GO TO 14003
14002  CONTINUE
         WRITE(NWTR,14005)   DIAL
       GO TO 666
14003  CONTINUE
         IF (KAR(IK) .EQ. YES(1) .AND. KAR(IK+1) .EQ. YES(2) )
     =     DIAL = 1
         IF ( DIAL .EQ. 1 ) WRITE(NWTR,14006) DIAL
         IF ( DIAL .EQ. 0 ) WRITE(NWTR,14005) DIAL
14006    FORMAT(/5X,'*** #��A�  ��⠭�����: "�A" (DIAL=',I1,') ***')
         GO TO 666
C######################################################################
3        CONTINUE
C   #PE��M
         KD = 3
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
4        CONTINUE
C   #�E��
         KD = 4
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
5        CONTINUE
C   #METO�
         KD = 5
         RETURN
C######################################################################
6        CONTINUE
C  #�E�
         KD = 6
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
7        CONTINUE
C  #TA�
         KD = 7
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
8        CONTINUE
C   #�PA�
         KD = 8
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
9        CONTINUE
C  #�PA�1
         KD = 9
!!!         write(*,*)'### SEQDIR KD=', KD
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
10       CONTINUE
C  #C�ET
         KD = 10
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
11       CONTINUE
C  #CTO�
         KD = 11
         RETURN
C######################################################################
12       CONTINUE
C   #COXP
         KD = 12
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
13       CONTINUE
C  #BOCT
         KD = 13
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
15       CONTINUE
C  #C�PAB
         KD = 15
         RETURN
C######################################################################
16       CONTINUE
C  #HELP
         KD = 16
         RETURN
C######################################################################
17     CONTINUE
C   #X�E��  <�M�>   -->  �A�� XDATA....
       KD = 17
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
18       CONTINUE
C  #�C�ET
         KD = 18
       CALL GETDAD( KAR, DAT, III )
         RETURN
C######################################################################
19     CONTINUE
C   #ACOXP
       KD = 19
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
20       CONTINUE
C  #C�EKTP
         KD = 20
       CALL GETDAD( KAR, DAT, III )
         RETURN
C######################################################################
21     CONTINUE
C   #MO�HOCT�
       KD = 21
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
22     CONTINUE
C   #MPE��M
       KD = 22
       CALL GETDAD( KAR, DAT, III )
       RETURN
C######################################################################
C######################################################################
23       CONTINUE
C  #BHE�
         KD = 23
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
24       CONTINUE
C  #SPOOL
         KD = 24
C*****   CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
C######################################################################
25       CONTINUE
C  #������
         KD = 25
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
C######################################################################
26       CONTINUE
C  #�������
         KD = 26
         CALL GETDAD(KAR, DAT, III)
         RETURN
C######################################################################
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
9999     CONTINUE
C  - NRDR CKOH�A�C�
         WRITE(NWTR, 105 ) NRDR
105   FORMAT(///' *** KOHE� �H�OPMA��� HA KAHA�E C�CTEMHO�O BBO�A ', I2,
     =  ' ***'/ ' *** PA�OTA �PEP�BAETC�  [#SEQ_DIR#]  ***'//)
         IRC = 2
         RETURN
         END
