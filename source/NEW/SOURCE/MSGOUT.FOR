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
500      FORMAT(' *** �O�. C�O� �P� B��OBE MSGOUT, MSG(1)=', I12)
         CALL SUBERR(1)
         RETURN
668      FORMAT (' *** COO��EH�E # ', I4, ' HE PEA���OBAHO ***')
C######################################################################
1        FORMAT(' *** HE O�O�HAHO HA�A�O �O���ECKO� CTPOK� ***')
C---> 2
2        FORMAT(' *** BBO��MOE ��C�O C���KOM BE��KO ***'/
     =          ' *** BO�MO�HO - �PO��CK PA��E��TE��HO�O �PO�E�A ***')
C--->3
3        FORMAT(' *** BBO��MOE ��C�O CO�EP��T HE �O��CT�M� C�MBO� ***')
C----->4
4        FORMAT(' *** T�� MAKPOMO�E�� O���O�EH ��� HE PEA���OBAH ***')
C-----> 5
5        FORMAT(' *** HE O�PE�E�EH HOMEP MAKPOMO�E�� ***'/
     =  ' *** O��CAH�E C��CKA "�" [ ���� ]  �P�H�T� HEBO�MO�HO ***')
C-----> 6
6        FORMAT(' *** HE �O��CT�M�� HOMEP ��� O�O�HA�EH�� ���A ***')
C---->  7
7        FORMAT(' *** HE O�PE�E�EH HOMEP MAKPOMO�E�� ***'/
     = ' *** O��CAH�E C��CKA "�" [ �APAMETP� ] �P�H�T� HEBO�MO�HO ***')
C----->8
8        FORMAT(' *** �AHHO� MAKPOMO�E�� �APAMETP� HE H��H� ***')
C---->9
9        FORMAT(' *** B �O���ECKO� CTPOKE O��CAH�� �KA�AHA HE BC� �H�OPM
     =A��� ***')
C---->10
10       FORMAT(' *** HAP��EHA CTP�KT�PA C��CKA "�" - O���AETC� "MAS" **
     =*')
C---->11
11       FORMAT(' *** HET "<" B C��CKE �APAMETPOB [ "MAS" ] ***')
C----->12
12     FORMAT(' *** HAP��EHA CTP�KT�PA C��CKA "�" - O���AETC� "FT" ***')
C------>13
13       FORMAT(' *** BCTPOEHA� ��HK��� "FT" ?? HE PEA���OBAHA ***')
C------>14
14    FORMAT(' *** B C��CKE "�" O��CAH�� "FT" HET O�PAH���TE�� ">" ***')
C------>15
15       FORMAT(' *** B TEK��EM C��CKE MHO�O �APAMETPOB ***')
C---->16
16       FORMAT(' *** C��COK FT # "<......>" - MA� ***')
C---->17
17       FORMAT(' *** HET "<" B C��CKE �APAMETPOB [ "FT" ] ***')
C---->18
18       FORMAT(' *** HAP��EHA CTP�KT�PA C��CKA "�" - O���AETC� "PSUB" *
     =**')
C---->19
19       FORMAT(' *** HET "<" B C��CKE �APAMETPOB "PSUB" ***')
C---->20
20       FORMAT(' *** HEO���AHH�� KOHE� �O���ECKO� CTPOK� - MA�O �AHH�X
     =B C��CKE ***')
C---->21
21       FORMAT(' *** HEO�O�HAHO HA�A�O �O�C��CKA ***' / ' *** HET "V",
     = "U", "I", "GAMMA", "OMEGA", "M�M", "MBH" ***')
C--->22
22       FORMAT(' *** C��COK "�A�POC B�BO�A" �O���O� - �CEKAETC� ***')
C--->23
23       FORMAT(' *** OTME�EHHO�O ���A B �E�� HET ***')
C--->24
24       FORMAT(' *** OTME�EHHO�O ��EMEHTA �E�� HE CO�EP��T ***')
C---->25
25       FORMAT(' *** OTME�EHHOE "�HA�EH�E" HE �O��CT�MO ***')
C----> 26
26       FORMAT(' *** OTME�EHH�� ��EMEHT, �A�PO�EHH�X HA B�BO� BE����H,
     = HE CO�EP��T ***')
C--->27
27       FORMAT(' *** HE O�PE�E�EH HOMEP �BEHA  ***')
C--->28
28       FORMAT(' *** HOMEP CB��� �O���E HOMEPA �BEHA  ***')
C--->29
29       FORMAT(' *** CB��� C �BEHOM, �ME���M �O����� HOMEP ***')
C--->30
30       FORMAT(' *** T�� CB��� O���O�EH  ***')
C--->31
31       FORMAT(' *** CB��� HE O�PE�E�EHA ***')
C--->32
32       FORMAT(' *** BBO��M XAPAKTEP�CT�K� �BEHA, HE BBE�� O��CAH��',
     *                ' CB��E� *** ' )
C--->33
33       FORMAT(' *** HAP��EH �OP��OK C�E�OBAH�� �APAMETPOB  ***')
C--->34
34       FORMAT(' *** OTME�EHH�� ���� �� ����������� � #SPOOL ***')
C---> 35
35    FORMAT(' *** OTME�EHH�� "��������" �� ����������� � #SPOOL ***')
C
C
C
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
         END
