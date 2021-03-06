          SUBROUTINE GETSPO( MSWALL, SPIS, IRC )
CXXX----KOM��EKC "MS1" -->             
C   �O��PO�PAMMA:   GETSPO( MSWALL, SPIS, IRC )
C   ------------
C   HA�HA�EH�E:    �OPM�P�ET C��COK "�AKA� B�BO�A"
C   -----------     ��� ��������� #����   ( �� 15 ������� )           
C   METO�:         BBO� �AHH�X ( CM. INPUT1 ), �O�CK �O TA����AM SERVZ;
C   -----
C   �APAMETP�:     MSWALL - C��COK COCTO�H��;
C   ---------      SPIS - MACC�B "�AKA� B�BO�A";
C                  IRC = 1  SPIS C�OPM�POBAH;
C                  IRC = 2  SPIS ��CTO�;
C                  IRC = 3  KOHE� �A��A HA NRDR;
C   �O��PO�PAMM�:  1. COMMON PROF1, SERVZ1, 2, 3.
C   -------------  2. INPUT1, SEQMS0, SEQNT1;
C   �P�ME�AH�E:    1. B C��CKE HE �O�EE 15-T� �HA�EH��;
C   ----------     2. �OPMAT SPIS:
C
C                  �OPMAT  SPIS:
C                  -------------
C
C   +------------+----------+----------+
C   I KO���ECTBO I O��CAH�E I O��CAH�E I
C   I O��CAH��   I          I          I ........
C   +------------+----------+----------+
C   I 1 C�OBO    I 6-T� C�OBI 6-T� C�OBI .......
C   +------------+----------+----------+
C
C                O��CAH�E:
C                --------
C
C  1-C�OBO: T�� O��CAH�� PX = 1 - ���OBOE HA�P��EH�E;
C  *******                  = 2 - �A�EH�E HA�P��EH�� ME��� ���AM�;
C  (+0)                     = 3 - TOK ��� C�E�. �EPEMEHHA�    ��EMEHTA;
C
C 2-C�OBO: �P� PX = 1 - HOMEP ���A;
C *******      PX = 2 - HOMEP ���A #1;
C  (+1)        PX = 3 - A�PEC B MACC�BE TOKN (TOKN1) �HA�EH�� "TOK";
C
C 3-C�OBO: �P� PX = 1 - HE �C�����ETC�;
C *******      PX = 2 - HOMEP ���A #2;
C  (+2)        PX = 3 - HOMEP ��EMEHTA;
C
C 4-C�OBO: �P� PX = 1 - HE �C�O����ETC�;
C *******      PX = 2 - HE �C�O����ETC�;
C  (+3)        PX = 3 - A�PEC �MEH� ��EMEHTA B TABNM;
C
C 5-C�OBO: �P� PX = 1 - HE �C�O����ETC�;
C *******      PX = 2 - HE �C�O����ETC�;
C  (+4)        PX = 3 - A�PEC �MEH� �EPEMEHHO� B TABNT;
C
C 6-C�OBO: �P� PX = 1 - HE �C�O����ETC�;
C *******      PX = 2 - HE �C�O����ETC�;
C  (+5)        PX = 3 - �HA�EH�E "CME�EH��";
C
C
C     P A � M E P H O C T �    SPIS  ==>  1 + 6 * 15 = 91  C�OB;
C
C
CZZZ-------------------------------------------------------------------
         DIMENSION MSWALL(1), SPIS(1)
         COMMON /PROF1/ MASPRT(200)
C$$$$$$$$COMMON /SERVZ1/ TABSPC(1)
         EQUIVALENCE ( MASPRT(15),  DIAL )
         EQUIVALENCE ( MASPRT(14),  NWTRE )
         EQUIVALENCE ( MASPRT(13),  NWTR )
         DIMENSION VT(16), UT(31), IT(31)
         INTEGER VT, UT, SUM, PRT, SPIS, TABSPC, TIP, DIAL
         DO 600 I=1,91
600      SPIS(I) = 0
         IRC = 15
         CALL INPUT1( MSWALL, VT, UT, IT, IRC )
         IF (IRC .EQ. 3 ) RETURN
C!!!-------------------------------------------------------------------
C         WRITE(NWTRE,300) VT
C300      FORMAT(' ***SPIS_VT:', 10(2X,I5), ' ***')
C         WRITE(NWTRE,301) UT
C301      FORMAT(' ***SPIS_UT:', 10(2X,I5),' ***'/' ***', 7X,10(2X,I5))
C         WRITE(NWTRE,302) IT
C302      FORMAT(' ***SPIS_IT:', 10(2X,I5),' ***'/' ***', 7X,10(2X,I5))
C!!!-------------------------------------------------------------------
         IRC = 2
         SUM = VT(1) + UT(1) + IT(1)
         IF ( SUM .EQ. 0 ) WRITE(NWTR, 100)
100     FORMAT(/' *** C��COK "�A�POC B�BO�A" HE �P�H�T - ��PEKT�BA ��HOP
     =�P�ETC� ***'/)
         IF ( SUM .EQ. 0 ) RETURN
         IRC = 1
         PRT = 2
C---> PA��OP
         KVT = VT(1)
         IVT = 2
2        CONTINUE
         IF ( KVT .LE. 0 ) GO TO 1
         SPIS(PRT) = 1
         SPIS(PRT+1) = VT(IVT)
         SPIS(1) = SPIS(1) + 1
         PRT = PRT + 6
         IVT = IVT + 1
         KVT = KVT - 1
         GO TO 2
1        CONTINUE
C--->  "UT"
         KUT = UT(1)
         IUT = 2
3        CONTINUE
         IF ( KUT .LE. 0 ) GO TO 4
         SPIS( PRT) = 2
         SPIS(PRT+1) = UT(IUT)
         SPIS(PRT+2) = UT(IUT+1)
         PRT = PRT+6
         SPIS(1) = SPIS(1) + 1
         IUT = IUT + 2
         KUT = KUT - 2
         GO TO 3
4        CONTINUE
C--->"I"
         KIT = IT(1)
         IIT = 2
5        CONTINUE
         IF ( KIT .LE. 0 ) GO TO 6
         SPIS(PRT) = 3
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C --> A�PECA...
         NEL = IT(IIT)
         NSMX = IT(IIT+1)
         SPIS(PRT+2) = NEL
         SPIS(PRT+5) = NSMX
C---> �POCMOTP MSWALL...
         CALL SEQMS0( MSWALL, NEL, IMSW, IR0, NSM, ITOK )
         GO TO (10,11), IR0
         CALL SUBERR(IR0)
11       CONTINUE
C!!!-------------------------------------------------------------------
         WRITE(NWTRE,12) IR0, NEL
12       FORMAT(' *** �O�. C�O� B C��CKE "IT" GETSPO_MS0: IR0=',
     =   I5, ' NEL = ', I6, ' ***' ) 
C!!!--------------------------------- ---------------------------------
         CALL SUBERR(23)
10       CONTINUE
         TIP = MSWALL(IMSW+1)
         SPIS(PRT+1) = ITOK + ( NSMX-1 )
C--->  HA�T� A�PECA �MEH� ��-TA � �MEH� �EPEMEHHO�:
         CALL SEQNT1(TIP, KTIP, NSMX, KNSMX )
         SPIS(PRT+3) = KTIP
         SPIS(PRT+4) = KNSMX
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         SPIS(1) = SPIS(1) + 1
         PRT= PRT + 6
         IIT = IIT + 2
         KIT = KIT - 2
         GO TO 5
6        CONTINUE
C--->  BCE....
C!!!-------------------------------------------------------------------
C         K = SPIS(1)
C         DO 7 KK=1,K
C         I1 = (KK-1) * 6 + 2
C         I2 = I1 + 5
C         WRITE(NWTRE,8) (I, SPIS(I), I=I1,I2)
C8        FORMAT(6( ' SPIS(', I2, ')=', I5) )
C7        CONTINUE
C!!!-------------------------------------------------------------------
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
