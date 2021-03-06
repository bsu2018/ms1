         SUBROUTINE FM0028( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT A�PE��   1989 �.)------------
C   �O��PO�PAMMA:   FM0028( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�� MEXAH��ECKO� C�CTEM�
C  ------------    T�� 999
C
C
C              ��EMEHT KO��P�ETC� �O OT�E��HO� �O���PEKT�BE (#MEX)
C              ---------------------------------------------------
C
C
C
C                    �OPMAT �O���ECKO� CTPOK� O��CAH��:
C                            ( BBO� �E� �OPMATH�� )
C
C                  "�" N�  "C" NC K�  "T" TIP  "R" RX RY RZ
C                  "P" PX PY PZ  "M" M  "J" J  "G" G  "Q" NEL NSM
C
C                  "%" .......";"  CTPOKA KOMEHTAP�EB;
C
C                  (��KB� B " " KO��P��TC� KAK �OKA�AHO)
C
C
C                  N� - HOMEP �BEHA MEXAH��ECKO� �E��;
C                  NC - HOMEP CB���, COE��H���E� �AHHOE �BEHO
C                       C �BEHOM K�;
C                  TIP- T�� CB���
C                        1 - BPA�EH�E BOKP��  OC�  X ;
C                        2 - BPA�EH�E BOKP��  OC�  � ;
C                        3 - BPA�EH�E BOKP��  OC�  Z ;
C                       -1 - CKO���EH�E B�O�� OC�  X ;
C                       -2 - CKO���EH�E B�O�� OC�  � ;
C                       -3 - CKO���EH�E B�O�� OC�  Z ;
C                *** EC�� �BEHO ��P��OE T�� CB��� �MHO�AETC� HA 10. ***
C
C                   RX,RY,RZ - �POEK��� BEKTOPA R, O�PE�E����E�O �O�O-
C                            �EH�E �EHTPA MACC �BEHA OTHOC�TE��HO
C                            HA�A�A , CB��AHHO� C �AHH�M �BEHOM C�CTEM�
C                            KOOP��HAT;
C
C                   PX,PY,PZ - �POEK��� BEKTOPA P, O�PE�E����E�O �O�O-
C                            �EH�E HA�A�A CB��AHHO� C �AHH�M �BEHOM
C                            C�CTEM� KOOP��HAT (��� COE��HEH�� BPA�E-
C                            H��) ��� TO�K� OTC�ETA O�O��EHHO� KOOP-
C                            ��HAT� (��� COE��HEH�� CKO���EH��),
C                            OTHOC�TE��HO �EHTPA MACC �BEHA  K�;
C                   M  - MACCA �BEHA;
C                   J  - MOMEHT �HEP��� �BEHA (��� CB��E� T��A 1,2,3)
C                   G  - KO������EHT �ECTKOCT� CB��� (��� ��P���X
C                        �BEH�EB )
C                   NEL,NSM - HOMEP ��EMEHTA ��EKTP��ECKO� �E�� �
C                             HOMEP CME�EH�� TOKA �TO�O ��EMEHTA,
C                             �B����E�OC� O�O��EHHO� C��O�, �E�CTB��-
C                             �E� B COE��HEH�� �BEHA;
C
C
C   METO�:         B���C��ETC� � �A��C�BAETC� B MSW BC� HEO�XO��MA�
C   -----          �H�OPMA���, HEO�XO��MA� ��� �OC�E����E�O
C                  PAC�ETA MAKPOMO�E��.
C
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW - �O�� SPM � SRAB;



C                            BA0:
C                            ---
C +------------+...+---------------------------------------------->
C !            !...!  HOMEP   ! T�� MAKPO- !  A�PEC   !  A�PEC  ! >
C ! ��CKP��TOP !   ! ��EMEHTA !  MO�E��    !  C��CKA  !  C��CKA ! >
C !            !   !   "�"    !   "T"      ! B "KNOT" ! B "PAR0"! >
C +------------+...+--------------------------------------------!->
C ! 1 2 3 4 5  !...!    1     !   2        !    3     !   4     ! >
C !------------!...!----------!------------!----------!---------! >
C ! �KA�ATE��---->J!   J + 0  !   J  + 1   !   J + 2  !   J + 3 ! >
C +------------+...+---------------------------------------------->
C
C -> +---------+---------------------------+----------+---------+
C  > ! �P��HAK !                           !  KO�-BO  !         !
C  > !  T��A   !        ��CTO�             ! �BEH�EB  ! ��CTO�  !
C  > ! MO�E��  !                           !          !         !
C -> +---------+---------------------------+----------+---------!
C  > !    5    !             6             !    7     !   8     !
C  > +---------+---------------------------+----------+---------!
C  > !  J + 4  !      J + 5                !   J + 6  !   J + 7 !
C  > +---------+---------------------------+----------+---------+
C
C                            KNOT:
C                            -----
C +------------+...+---------------------------------------------......
C !            !...!  HOMEP   !   HOMEP    !  C KAK�M !  T��    !......
C ! ��CKP��TOP !   !  �BEHA   !   CB���    !  �BEHOM  !  CB���  !......
C !            !   !    N�    !    NC      !    K�    !   TIP   !......
C +------------+...+--------------------------------------------!......
C ! 1 2 3 4 5  !...!    1     !   2        !    3     !   4     !......
C !------------!...!----------!------------!----------!---------!......
C ! �KA�ATE��---->J!   J + 0  !   J  + 1   !   J + 2  !   J + 3 !......
C +------------+...+---------------------------------------------......
C
C
C                            PAR0:
C                            ----
C +------------+...+---------+---------+---------+---------+---------+>
C !            !...!   RX    !   RY    !   RZ    !   PX    !   PY    !>
C ! ��CKP��TOP !   !   1-�O  !   1-�O  !   1-�O  !   1-�O  !   1-�O  !>
C !            !   !  �BEHA  !  �BEHA  !  �BEHA  !  �BEHA  !  �BEHA  !>
C +------------+...+---------+---------+---------+---------+---------+>
C ! 1 2 3 4 5  !...!    1    !   2     !    3    !    4    !    5    !>
C !------------!...+---------+---------+---------+---------+---------+>
C ! �KA�ATE��---->J!   J + 0 !  J + 1  !   J + 2 !   J + 3 !  J + 4  !>
C +------------+...+---------+---------+---------+---------+---------+>
C
C -> +---------+---------+---------+---------+---------+---------+.....
C  > !   PZ    !    M    !    J    !    G    !  NEL    !   NSM   !.....
C  > !   1-�O  !   1-�O  !   1-�O  !   1-�O  !  1-�O   !   1-�O  !.....
C  > !  �BEHA  !  �BEHA  !  �BEHA  !  �BEHA  !  �BEHA  !  �BEHA  !.....
C -> +---------+---------+---------+---------+---------+---------!.....
C  > !    6    !    7    !    8    !    9    !    10   !    11   !.....
C  > +---------+---------+---------+---------+---------+---------!.....
C  > !  J + 5  !  J + 6  !  J + 7  !  J + 8  !   J + 9 !  J + 10 !.....
C  > +---------+---------+---------+---------+---------+---------+.....
C
C
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C
C   �P�ME�AH�E:    1. KOHTPO�� HE �PO��BO��TC�;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C
C
C
C                  RMSW T��A # 999
C                  ---------------
C
C ++--------+--------+--------+--------+--------+--------+..==>
C II KO�-BO I KO�-BO I # �BEHAI # ��-TAI # CME- I A�PEC  I..==>
C II �BEH�EBI CB��E� I MEX.   I ��.    I �EH��  I   B    I..==>
C II        I C ��.�.I �E��   I �E��   I        I TOKN/N1I..==>
C ++--------+--------+--------+--------+--------+--------+..==>
C II   1    I    2   I   3    I   4    I   5    I   6    I..==>
C ++--------+--------+--------+--------+--------+--------+..==>
C II   9    I   10   I   11   I   12   I   13   I   14   I..==>
C ++--------+--------+--------+--------+--------+--------+..==>
C
C      BCE �APAMETP� �EPE�A�TC� �P� �OMO�� "COMMON" ��OKOB.
C
C
C
CZZZ-------------------------------------------------------------------
      COMMON /PROF1/ MASPRT(200)
      COMMON/BLINT/N,N3,N9,M(30),KK(30),IDLT(30),ISAKR(30,30),ISAIT(30)
      COMMON/BLKUST/RKST(90),ROKST(90),AIKST(270)
      COMMON/BLGRAF/INDEX(20),NGRAF,NSG
      COMMON/BLNEK/E(90),R(90),RO(540),RM(30)
      COMMON/BLQ/Q(30),QP(30),Q2P(100)
      COMMON/SYS1/KEY(20),LIM
      COMMON/BLSAIT/RKOEF(30),IP(30),ITIP(30),KOORD(6,30)
     *             ,INUM(2,30),IRKC(6,30)
      COMMON/RLI/QIR(30),DKOOR(3),IFLEX(30),NZ1,NZ2,KLU
      COMMON/CONSM/KS,NSUM,NS,NPS,NMS,NGS,NTR,NSV
      COMMON/OGR/OGR(2,30)
      COMMON/KOEFU/G(30)
      EQUIVALENCE ( N1    , RN1  )
      EQUIVALENCE ( FGMSW , MASPRT(87) )
      DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
      INTEGER BA0T, TIP
      REAL X0(30)
      DATA L,S/55555,55555.555555/
C----------------------------------------------------------------------
C--> HA�A�� .
C----------------------------------------------------------------------
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(14)
         IF (TIP .EQ. 999 )   GO TO 1000
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(13)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0028: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1000   CONTINUE
C----------------------------------------------------------------------
C--> CKO��KO �BEH�EB ?
C----------------------------------------------------------------------
       N1 = BA0T(7)
       RMSW(1) = RN1
C--> C��TAEM KO�-BO CB��E� C ��. �E��� ( KELC )
       KELC = 0
          DO 950  I=1,N1
          IPAR0T = 11*(I-1)+1
          NELC   = IFIX(PAR0T(IPAR0T+9))
          IF( NELC .EQ. 0 ) GO TO 950
          KELC = KELC + 1
950    CONTINUE
       IF( FGMSW .EQ. 0 ) GO TO 1010
C--> �TO�H�T� LM O����
      LM = 10 + KELC*4
C     RETURN
1010  CONTINUE
      LM = 2  + KELC*4
         DO 1020 I=2,LM
         RMSW(I) = 0.0
1020  CONTINUE
C----------------------------------------------------------------------
C--> �O��OTOB�M "COMMON" ��OK�
C-->  AHA�O� �/� "BEGIN" KOM��EKCA  "CADR"
C----------------------------------------------------------------------
      LIM=30
      NZ1=L
      NZ2=L
      NSV=0
      DO 1050 I=1,LIM
      ITIP(I)=0
      IP(I)=0
      INUM(1,I)=0
      INUM(2,I)=0
      RKOEF(I)=0.
      M(I)=L
      KK(I)=L
      IDLT(I)=L
      ISAIT(I)=0
      RM(I)=S
      Q(I)=S
      QP(I)=S
      Q2P(I)=S
1050  CONTINUE
      DO 1100 I=1,LIM
      DO 1100 J=1,6
      KOORD(J,I)=0
1100  IRKC(J,I)=0
      DO 1200 I=1,LIM
      DO 1200 J=1,LIM
      ISAKR(I,J)=L
1200  CONTINUE
      NN=9*LIM
      DO 1300 I=1,NN
      AIKST(I)=S
1300  CONTINUE
      NN=3*LIM
      DO 1400 I=1,NN
      ROKST(I)=S
      RKST(I)=S
1400  CONTINUE
      NN=15*LIM
      DO 1500 I=1,NN
      RO(I)=S
1500  CONTINUE
      DO 1600 I=1,20
      INDEX(I)=0
      KEY(I)=0
1600  CONTINUE
      DO 1700 I=1,LIM
      G(I)=S
      OGR(1,I)=S
      OGR(2,I)=S
1700  CONTINUE
      DO 1800 I=1,3
1800  DKOOR(I)=S
      NGRAF=L
      NSG=L
      KEY(20)=1
C----------------------------------------------------------------------
C    KOHE� �/� "BEGIN" KOM��EKCA "CADR"
C----------------------------------------------------------------------
C--> BBE�EM �AHH�E.
C----------------------------------------------------------------------
C-->  AHA�O� �/� "INDATE" KOM��EKCA "CADR"
C     �AHH�E BBO��TC� ��� PE��MA    "DIN"
C----------------------------------------------------------------------
      NGS=0
      KLU=0
      N = BA0T(7)
      N3=3*N
      N9=9*N
      DO 2100 I=1,N9
      AIKST(I)=0.
2100  CONTINUE
      DO 2200 I=1,N
2200  IFLEX(I)=0
C----------------------------------------------------------------------
C--> BBE�EM MACC�B B�A�MOCOE��HEH�� M(I)
C--> � MACC�B KO�OB OCE� � T��OB COE��HEH�� KK(I)
C----------------------------------------------------------------------
      DO 2250 I=1,N
      IKNOTT = (I-1)*4
      NZ  = KNOTT(IKNOTT + 1)
      NC  = KNOTT(IKNOTT + 2)
      NZ1 = KNOTT(IKNOTT + 3)
      NTC = KNOTT(IKNOTT + 4)
      M(NZ) = NZ1
      KK(NC)= NTC
2250  CONTINUE
      DO 2500 I=1,N
      J=KK(I)
      IF(J) 2300,2300,2400
2300  IDLT(I)=0
      GO TO 2500
2400  IDLT(I)=1
2500  CONTINUE
      DO 2600 I=1,N
      J=KK(I)
      LL=IABS(J)
      IF(LL.LE.3) GO TO 2550
      IFLEX(I)=1
      NGS=NGS+1
      KK(I)=KK(I)/10
2550  IF(IFLEX(I).EQ.0) KK(I)=LL
2600  CONTINUE
C----------------------------------------------------------------------
      KELC = 0
      IRMSW= 3
         DO 2700 I=1,N
         NZ = KNOTT( 4*(I-1) + 1 )
         NC = KNOTT( 4*(I-1) + 2 )
         IPAR0T = 11*(I-1) + 1
         IR =  3*(NZ-1) + 1
C--> BBO��M MACC�B �POEK��� BEKTOPOB �EHTPOB MACC �BEH�EB  "RKST(3,I)
C--> � MACC�B �POEK��� BEKTOPOB,O�PE�E�����X �O�O�EH�E HA�A� I-X
C--> C�CTEM CB��AHH�X C.K. ROKST(3,I)
         RKST(IR) = PAR0T(IPAR0T)
         RKST(IR+1) = PAR0T(IPAR0T+1)
         RKST(IR+2) = PAR0T(IPAR0T+2)
         ROKST(IR)  = PAR0T(IPAR0T+3)
         ROKST(IR+1)= PAR0T(IPAR0T+4)
         ROKST(IR+2)= PAR0T(IPAR0T+5)
C--> BBO��M MACC� �BEH�EB
         RM(NZ) = PAR0T(IPAR0T+6)
C--> BBO��M MOMEHT� �HEP��� �BEH�EB
         X0(NZ) = PAR0T(IPAR0T+7)
C--> BBO��M KOE�����EHT �ECTKOCT� CB���
         G(NC)  = PAR0T(IPAR0T+8)
C--> BBO��M CB��� C ��. �E���
         NELC = IFIX(PAR0T(IPAR0T+9))
         NSMC = IFIX(PAR0T(IPAR0T+10))
         IF( NELC .EQ. 0 ) GO TO 2700
         KELC = KELC + 1
         N1 = NZ
         RMSW(IRMSW) = RN1
         N1 = NELC
         RMSW(IRMSW+1) = RN1
         N1 = NSMC
         RMSW(IRMSW+2) = RN1
         IRMSW = IRMSW + 4
2700  CONTINUE
      N1 = KELC
      RMSW(2) = RN1
C---------------------------------------------------------------------
      DO 2800 I=1,N
      LL=KK(I)
      J=9*(I-1)+4*(LL-1)+1
      AIKST(J)=X0(I)
2800  CONTINUE
      DO 2900 I=1,N3
2900  RO(I)=ROKST(I)
      IF(NGS.EQ.0) GO TO 3000
C----------------------------------------------------------------------
C--> BBO��M KO������EHT� �ECTKOCT� ��� ��P���X �BEH�EB
C     �OKA HE PEA���OBAHO
C     *******************
C----------------------------------------------------------------------
3000  CONTINUE
C----------------------------------------------------------------------
C-->  KOHE� �/� "INDATE" KOM��EKCA "CADR"
C----------------------------------------------------------------------
C-->  �A�AEM PE��M PA�OT�
C-->  AHA�O� �/� "MODE" KOM��EKCA "CADR"
C-->  �A�AETC� PE��M "NORMAL"
C----------------------------------------------------------------------
       DO 3100 I=1,19
3100   KEY(I) = 0
       KEY(4) = 1
       KEY(5) = 1
       KEY(10)= 1
       RETURN
C***** DEBUG INIT
       END
