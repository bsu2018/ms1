         SUBROUTINE DRCIR1(BA0, KNOT, PAR0, IRC)
CXXX----KOM��EKC "MS1" --> BEPC�� 1.1  (OT AB��CTA 1987 �.)------------
C   �O��PO�PAMMA:   DRCIR1(BA0, KNOT, PAR0, IRC)
C   ------------
C   HA�HA�EH�E:    COPT�POBKA BA0 � O�PE�E�EH�E PA�MEPA MSW_ALL
C   ----------
C   METO�:         COPT�POBKA - CM. �/� TRIARB, �O �P��HAK� T��A MO�E��;
C   -----          PA�MEP MSWALL O�PE�E��ETC� �O �O��M MACTAB(8)
C                  �P� �TOM:   +5 C�OB HA ��CKP��TOP,
C                              +1*NXBA0 - HA �KA�ATE�� C�E�. MSW O��CAH�
C                               (NXBA0 - KO���ECTBO ��EMEHTOB �E��)
C                               +1 C�OBO HA �KA�ATE�� KOH�A C��CKA;
C                              �O�PO�HEE CM. CTP�KT�P� BA0 B �/� INPUT0;
C                 + LM = MACTAB(...+7) C�OB HA KA���� ��EMEHT �� BA0;
C                   ***  �P�  MACTAB(...+7) = +1(VAR) - �A�POC
C                        �EPE� CALLFM �P� BO�����EHHOM ��A�E FGMSW
C                        HA �TO�HEH�E BCE� ���H� MSW_ALL �AHHO�O
C                        ��EMEHTA.    ****
C
C
C   �APAMETP�:     BA0, KNOT, PAR0 - MACC�B� O��CAH�� ��EMEHTOB;
C   ---------      IRC = 1 - �A�AH�E B��O�H���;
C                      = 2 - BO�MO�HO TO��KO �P� FRCPU = 0
C                            "A��APAT" REGION OTK���EH (CM. �P�ME�AH�E);
C
C   �O��PO�PAMM�:  1. COMMON PROF1, PROF2
C   ------------   2. *** SORTBA ******  ( OT AB��CTA 1987 �. )
C                  3.  CALLFM.
C   �P�ME�AH�E:    1. �P� FRCPU = 1 [MASPRT(73)] O�PE�E�EHH��
C  -----------        PA�MEP MSWALL �A��C�BAETC� B DMSWAL [MASPRT(20)]
C                     �P� FRCPU = 0 (CTAT��ECKOE B��E�EH�E O.�.) -
C                     �POBO��TC� CPABHEH�E HEO�XO��MO�O ��� �A��HE��E�
C                     PA�OT� PA�MEPA MSWALL � �HA�EH�� DMSWAL �� MASPRT:
C                     B C���AE HE XBATK� O.�. �CTAHAB��BAE�C� IRC=2 �
C                     �EHEP�P�ETC� COO��EH�E;
C                   2.  !! CTPAXOBKA !! -->  �O�� NSYS �P�CBA�BAETC�
C                                            �HA�EH�E NMKNOT
C
C
C
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         COMMON/PROF2/ MACTAB(800)
         INTEGER BA0, BA0CPL, AKKK, DMSWAL, FRCPU, FGMSW, TIP
         DIMENSION BA0(*), MSWT(50), KNOT(*), PAR0(*)
         EXTERNAL BA0CPL, BA0CNG
         EQUIVALENCE ( DMSWAL, MASPRT(20) )
         EQUIVALENCE ( FGMSW,  MASPRT(87) )
         EQUIVALENCE ( NMKNOT, MASPRT(7) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NXBA0,   MASPRT(3)  )
         EQUIVALENCE ( N1BA0,    MASPRT(4)  )
         EQUIVALENCE ( FRCPU,    MASPRT(73)  )
         EQUIVALENCE ( N1MACT,   MASPRT(17)  )
         EQUIVALENCE ( NSYS,     MASPRT(26)  )
         NSYS = NMKNOT
         IRC = 1
C--> SORT....
C!!!-------------------------------------------------------------------
C      CALL PRINT0( BA0, KNOT, PAR0)
C!!!-------------------------------------------------------------------
         CALL SORTBA(  BA0  )
C!!!-------------------------------------------------------------------
C      CALL PRINT0( BA0, KNOT, PAR0)
C!!!-------------------------------------------------------------------
C--->  O�PE�E��EM PA�MEP MSW_ALL
         MMSW = 5 + 1 * NXBA0 + 1
C-->  �O�CK �O MACTAB ��� BCEX ��EMEHTOB:
         I0 = 6
         IMAX=BA0(3)
         DO 1 KKK = 1, NXBA0
C->  A�PEC ��EMEHTA KKK B BA0:
         AKKK = (KKK-1)*N1BA0 + I0
C-->  KOHTPO��
         IF( AKKK+N1BA0  .GT.  IMAX ) CALL SUBERR(AKKK)
C-->  CMOTP�M MACTAB
         TIP = BA0(AKKK+1)
         IX = 1
4        CONTINUE
         IF(MACTAB(IX) .EQ. 0 ) GO TO 2
         IF(MACTAB(IX) .EQ. TIP ) GO TO 3
         IX = IX + N1MACT
         GO TO 4
2        CONTINUE
C-->  �O�. O���KA, T��A HET...
         CALL SUBERR(TIP)
3        CONTINUE
C--> IX HA�A CTPOKA
C-->  KOHTPO��
         IF ( MACTAB(IX+7) .LT. 0 ) CALL SUBERR(IX)
         IF ( MACTAB(IX+7) .EQ. +1 ) GO TO 60
C--->    ��EMEHT HE C LM  -->  +1(VAR)
C ==> C��TAEM
         MMSW = MMSW + MACTAB(IX+7)
       GO TO 1
C######################################################################
60     CONTINUE
C-->  �TO�H�M LM.....
       FGMSW = 1
C      *********
C  AKKK - A�PEC O��CAH�� ��EMEHTA B BA0, IKNOT � IPAR0 -> B KNOT � PAR0:
       IKNOT = BA0( AKKK+2 )
       IPAR0 = BA0( AKKK+3 )
C--->  HOMEP FM MOH�TOPA:
       NFM = MACTAB( IX+6 ) / 1000
C-->  �TO�H�M
       LM = 10
       CALL CALLFM( NFM, BA0(AKKK), KNOT(IKNOT), PAR0(IPAR0), MSWT, LM )
C--> C�POC�M ��A�EK...
       FGMSW = 0
C-->  C��TAEM
       MMSW = MMSW + LM
C######################################################################
1        CONTINUE
C######################################################################
C!!!-------------------------------------------------------------------
         WRITE(NWTRE, 51) MMSW
51       FORMAT(' *** ��� C��CKA MSW_ALL HEO�XO��MO ', I5, ' C�OB O.�. *
     =**')
C!!!-------------------------------------------------------------------
C--->  �TO � HAC ?
         IF ( FRCPU .EQ. 0 ) GO TO 5
C--> "APPARAT" REGION BK��EH....
         DMSWAL = MMSW
         IRC = 1
         RETURN
C**********************************************************************
5        CONTINUE
         IRC = 1
C-->  CTAT��ECK� B��E���� �OCTATO�HO ??
         IF ( DMSWAL.GE. MMSW ) RETURN
         IRC = 2
C-->  HET O.�.
C!!!-------------------------------------------------------------------
         IZZ = MMSW - DMSWAL
         WRITE(NWTRE, 50) IZZ, DMSWAL
50      FORMAT(/' *** ��� MSW_ALL HEXBATAET ', I5, ' C�OB O.�. ***'/
     =  ' *** ��HA�A��HO �O� MSW_ALL B��E�EHO ', I5, ' C�OB O.�. ***'/
     = ' *** PA�OTA B���MO ���ET �PEPBAHA ***'///)
C!!!-------------------------------------------------------------------
         RETURN
         END
