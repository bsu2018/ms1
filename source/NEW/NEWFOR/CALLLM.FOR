       SUBROUTINE CALL LM( MSWALL, KOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: CALL LM( MSWALL, KOD )
C   ------------
C   HA�HA�EH�E: B��OB LM...  MOH�TOPOB ��� �O���ECKO� HACTPO�K�
C   ---------   MSW_ALL C��CKA O��CAH�� ��EMEHTOB.
C
C
C   METO�:      �POCMOTP MSW_ALL, B C���AE HA����� HOMEPA
C   -----       LM...  MOH�TOPA B O��CAH�� MACTAB, E�O B��OB;
C
C
C   �APAMETP�: MSW_ALL - MACC�B ��P. C�OB MAKPOMO�E�E� ��EMEHTOB;
C   ---------  KOD = MAX OT KO�OB LM... MOH�TOPOB,
C                    O���HO, EC�� KOD ^= 0 --> �O�. O���KA O��CAH��
C                                               �E��;
C   �O��PO�PAMM�:   1. PROF1, PROF2
C   ------------    2. BCE �/� LM.....;
C                      ( PEA���OBAHO O�PA�EH�E K 10 �/� T��A LM... );
C
C
C   �P�ME�AH�E:   1. B��OB LM MOH�TOPA:
C   ----------
C               CALL LM....(MSWT, RMSWT, ADR, MSWALL, RMSWAL, KOD )
C
C              ��E
C                      MSWT, RMSWT - "OTPE�K�" MSW_C��CKA ���
C                      ��EMEHTA, MAKPOMO�E�� KOTOPO�O TPE��ET
C                      O�C����BAH�E CO CTOPOH� DM... MOH�TOPA;
C                      ADR - MSW A�PEC C�OBA COCTO�H�� ��EMEHTA,
C                            ��� HACTPO�K� MAKPOMO�E�� KOTOPO�O
C                            B���BAETC� LM... MOH�TOP;
C                      MSWALL, RMSWAL - BEC� C��COK MSW_C�OB;
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       COMMON/PROF2/ MACTAB(800)
       DIMENSION MSWALL(*)
       INTEGER LM, TIP, PTIP, ADR
C-->  MAX A�PEC W MSWALL:
       MAX = MSWALL(3)
       KOD = 0
       KODX = 0
       ADR = 6
C######################################################################
900    CONTINUE
       KOD = MAX0( KOD, KODX )
       I1 = MSWALL( ADR )
       IF ( I1 .EQ. 0 ) GO TO 666
       IF ( I1 .GT. MAX ) CALL SUBERR(I1)
C-->  TEK. A�P. MSW ..
       IM = ADR+1
C-->  C�E�. A�P. MSW
       ADR = I1
C-->  T�� MAKPOMO�E��..
       TIP = MSWALL(IM+1)
C
C-->  �O�CK �O MACTAB...
       IX = 1
100    CONTINUE
       IF ( MACTAB(IX) .EQ. 0 ) GO TO 200
       IF ( MACTAB(IX) .EQ. TIP ) GO TO 300
       IX = IX + MASPRT(17)
       GO TO 100
200    CONTINUE
       NWTRE = MASPRT(14)
       WRITE(NWTRE, 1000) TIP
1000   FORMAT(/' *** �O�. C�O� B CALL_LM --> T��A # ', I5,
     = ' HET B MACTAB ***'/)
       CALL SUBERR(TIP)
C######################################################################
300    CONTINUE
C
C  IX  CTPOKA B MACTAB HA�E�O ��EMEHTA, ECT� �� LM - MOH�TOP ?
C
       KKK = MACTAB(IX+5)
       LM = KKK / 100
C----------------------------------------------------------------------
       IF ( LM .EQ. 0 ) GO TO 900
C-->  MOH�TOP B���BAEM..
C      IM - A�EC B MSW_ALL O��CAH��
C
       GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14)  LM
C----------------------------------------------------------------------
       NWTRE = MASPRT(14)
       WRITE(NWTRE,1001) LM
1001   FORMAT(/' *** �O�. C�O� B CALL_LM --> CTPAHH�� # "LM" ',
     = 'MOH�TOPA = ', I5, ' ***'/)
       GO TO 900
C----------------------------------------------------------------------
C
666    CONTINUE
       RETURN
C
C### B���BAEM LM MOH�TOP� #############################################
1      CALL LM0001( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
2      CALL LM0002( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
3      CALL LM0003( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
4      CALL LM0004( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
5      CALL LM0005( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
6      CALL LM0006( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
7      CALL LM0007( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
8      CALL LM0008( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
9      CALL LM0009( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
10     CALL LM0010( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
11     CALL LM0011( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
12     CALL LM0012( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
13     CALL LM0013( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
14     CALL LM0014( MSWALL(IM), MSWALL(IM), IM, MSWALL, MSWALL, KODX )
       GO TO 900
C----------------------------------------------------------------------
       END
