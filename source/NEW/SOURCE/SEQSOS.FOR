       SUBROUTINE SEQSOS(NEL, TN, KD, TOKN, UN )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: SEQSOS(NEL, TN, KD, TOKN, UN )
C   ------------
C   HA�HA�EH�E:        KOHTPO�� ( � KOPPEK��� !!! ) KO�A COCTO�H��
C   ----------         BEHT���HO�O ��EMEHTA;
C
C   METO�:             AHA��� �O�O�EH�� PA�. TO�K� (UN,TOKN) HA BAX
C   -----              BEHT���; HEO�PE�E�EHHOCT� -->(-0,+0)<--
C                      AHA����P�ETC� �O TO�KE (UOPEN, ICLOSE);
C
C   �APAMETP�:         NEL - HOMEP ��EMEHTA;
C   ---------          TN - BPEM� O�EP�E�EH�� TO�KA BAX:
C                      TOKN - �HA�EH�E TOKA �
C                      UN - �A�EH�E HA�P��EH�� HA BEHT���;
C
C   �O��PO�PAMM�:      1.COMMON PROF1
C   -------------
C  �P�ME�AH�E:         1. �OKA TO��KO COO��EH�E...!!!!!!!!
C  ----------
CZZZ-------------------------------------------------------------------
       COMMON /PROF1/ MASPRT(80)
       EQUIVALENCE( MASPRT(14), NWTRE)
       EQUIVALENCE( MASPRT(77), ICLOSE )
       EQUIVALENCE( MASPRT(78), UOPEN  )
       REAL ICLOSE
C######################################################################
C  KO��A "OTKP�T", TO...
C
C [ UN >= 0.0 ]   TOKN >= -ICLOSE
C
C KO��A "�AKP�T", TO...
C
C UN < = UOPEN   [ TOKN <= 0.0 ]
C
C######################################################################
       IF ( UN .GE. UOPEN  .AND. TOKN .GT. -ICLOSE ) GO TO 1
C--->  B���MO �AKP�T?
       IF ( UN .LE. 0.0  .AND. TOKN .LE. -ICLOSE ) GO TO 2
666    CONTINUE
C-->  HE��BECHO, �TO ?
       WRITE(NWTRE,100) NEL, TN, KD, TOKN, UN
100    FORMAT(' *** �TO-TO HE�CHO: NEL=',I5,' TN=',G12.5,' KD=',I1,
     = ' TOKN=',G12.5,' UN=',G12.5, ' ***')
       RETURN
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
1      CONTINUE
C--->  �O��EH ��T� OTKP�T�M....
       IF ( KD .EQ. 1 ) RETURN
C-->   ????
       GO TO 666
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
2      CONTINUE
C--->  �O��EH ��T� �AKP�T�M
       IF ( KD. EQ. 0 ) RETURN
C--->  ???
       GO TO 666
       END
