         SUBROUTINE FM0002( BA0T, KNOTT, PAR0T, RMSW, LM )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA:  FM0002( BA0T, KNOTT, PAR0T, RMSW, LM )
C   ------------
C
C   HA�HA�EH�E:    HACTPO�KA MAKPOMO�E��:
C  ------------
C                  T��     �M� MO�E��     O�O�HA�EH�� B �A��HE��EM
C
C                   8        ��O� (HE ��P. BEHT���)    D
C
C                  �PO��BO��T �OPM�POBAH�E "�EPEMEHHO�" �ACT� MSW
C                  MAKPOMO�E��.
C
C   METO�:         PA�METKA �O�E� RMSW �O� MO�E�� "��EA��H�� K���";
C   -----
C
C   �APAMETP�:     BA0T, KNOTT, PAR0T - "OTPE�K�" �H�OPMA��OHH�X
C  ----------      MACCOBOB BA0, KNOT, PAR0 ( CM. INPUT0 � GETMSW );
C                  RMSW - "�EPEMEHHA�" �ACT� MSW ( "OTPE�OK" MSWALL );
C                  LM - ���HA "�EPEMEHHO�" �ACT� RMSW;
C
C   �O��PO�PAMM�:  1.COMMON PROF1
C   ------------
C
C   �P�ME�AH�E:    1. KOHTPO�� HE �PO��BO��TC�;
C   -----------    2. BCE HE O�PE�E��EM�E �O�� RMSW �AH����TC�;
C                  3. H��E �P�BO��TC� �OPMAT� RMSW:
C
C                  RMSW T��A # 8 [ ��EA��H�� ��O� ]
C                  --------------------------------
C                                                             LM = 4
C                                                             ******
C
C                    SRAB
C             +------+--------+-----+-------+
C             I COCT.I BPEM�  I PE  I PE    I         KD = 0 BEHT���
C             I TEK. I �EPEK�.I �E  I  �E   I                �AKP�T;
C             I "KD" I "TX"  .I PB  I   PB  I
C             +------+--------+-----+-------+         KD = 1 BEHT���
C             I  1   I   2    I  3  I   4   I                OTKP�T;
C             +------+--------+-----+-------+
C             I  11  I   12   I 13  I  14   I         TX - BO�MO�HOE
C             +------+--------+-----+-------+              BPEM� KOM-��;
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
         DIMENSION BA0T(*), KNOTT(*), PAR0T(*), RMSW(*)
         INTEGER BA0T, TIP
         TIP = BA0T(2)
         NEL = BA0T(1)
         NWTR = MASPRT(13)
         IF ( TIP .EQ. 8 ) GO TO 1
C!!!-------------------------------------------------------------------
         NWTRE = MASPRT(14)
         WRITE(NWTRE, 500) TIP, NEL
500      FORMAT(' *** �O�. C�O� B FM0002: HOMEP T��A HE O�C����BAETC� **
     =*'/' *** HOMEP T��A ', I10, ' ��EMEHT HOMEP: ', I10, '***')
C!!!-------------------------------------------------------------------
         CALL SUBERR(1)
C######################################################################
C######################################################################
1        CONTINUE
C---> O�H��EH�E BCEX �O�E� RMSW ��� ��O�A
         LM = 4
         DO 100 I=1,LM
100      RMSW(I) = 0.0
         RETURN
         END
