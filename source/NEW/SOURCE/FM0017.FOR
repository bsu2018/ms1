         SUBROUTINE FM0017( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  FM0017( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E�E�:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                  93    "BEHT���,��P. C���"           TR3
C                 668    "K���,��P. C���"              KL
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E�� " BEHT���(K���) ".
C   -----
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� B M�H�MA��HOM O�"EME;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C
C
C  B O��CAH�� �E�� �C�O����ETC� C�E�. �A��C�:
C
C   ... � NELT  NSM  TCIU  TZAD  TIMP
C      ��EC� �C�O���OBAH� C�E�. O�O�HA�EH��:
C      NELT  -HOMEP ��EMEHTA, ��PAB�.BEHT��EM (K���EM);
C             ( C���, T�� MO�E�� #301 );
C      NSM   -�OP��KOB�� HOMEP C��HA�A ��PAB�EH�� C���;
C
C
C
C
C                  4. H��E �P�BO��TC� �OPMAT� RMSW:
C
C      RMSW T��A # 93 ��� # 668 [ BEHT���(K���), ��PAB��EM�� C���]
C      -----------------------------------------------------------
C     SPM  I                SRAB
C +----+---+------+-----+---+------+----+--------+----+
C INELTINSMI ATOKMI KD  IHX I MCIU I HX I        I Q  I
C I    I   I      I     I   I      ICIU I PE�EPB I    I
C I    I   I      I     I   I      I    I        I    I
C +----+---+------+-----+---+------+----+--------+----+
C I  1 I 2 I   3  I  4  I 5 I   6  I  7 I   8,9  I 10 I
C +----+---+------+-----+---+------+----+--------+----+
C I 11 I12 I  13  I 14  I15 I  16  I 17 I  18,19 I 20 I
C +----+---+------+-----+---+------+----+--------+----+
C
C      BEHT���(K���) OTKP�T -> KD=1; BEHT��� (K���) �AKP�T ->  KD=0;
C      HX - �A� �O BPEMEH�, BPEM� BO�MO�HO�O �EPEK���EH��;
C
C        MCIU  �  HXCIU - �APAMETP� "BOCTAHOB�EHHO�O" �MP���CA "C��"
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         EQUIVALENCE ( RN , N )
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         NEL = BA0T(1)
         TIP = BA0T(2)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 93 .OR. TIP .EQ. 668 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0017: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
1        CONTINUE
C---> O�H��EH�E BCEX �O�E� RMSW ��� BEHT��� (K���A)
         LM = 10
         DO 100 I=1,LM
100      RMSW(I) = 0.0
C===> �P�H�T� �CXO�. �APAMETP�:
         NELT = PAR0T(1)
         NSM  = PAR0T(2)
C ---> �A�O�H�EM MSW
         N = NELT
         RMSW(1) = RN
         N = NSM
         RMSW(2) = RN
         RETURN
         END
