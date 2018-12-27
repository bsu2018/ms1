       SUBROUTINE CALL LM( MSWALL, KOD )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: CALL LM( MSWALL, KOD )
C   ------------
C   HA‡HA—EHˆE: B›‡OB LM...  MOHˆTOPOB „‹Ÿ ‹Oƒˆ—ECKO‰ HACTPO‰Kˆ
C   ---------   MSW_ALL CˆCKA OˆCAHˆŸ ‹EMEHTOB.
C
C
C   METO„:      POCMOTP MSW_ALL, B C‹“—AE HA‹ˆ—ˆŸ HOMEPA
C   -----       LM...  MOHˆTOPA B OˆCAHˆˆ MACTAB, EƒO B›‡OB;
C
C
C   APAMETP›: MSW_ALL - MACCˆB “P. C‹OB MAKPOMO„E‹E‰ ‹EMEHTOB;
C   ---------  KOD = MAX OT KO„OB LM... MOHˆTOPOB,
C                    O›—HO, EC‹ˆ KOD ^= 0 --> ‹Oƒ. O˜ˆKA OˆCAHˆŸ
C                                               –Eˆ;
C   O„POƒPAMM›:   1. PROF1, PROF2
C   ------------    2. BCE / LM.....;
C                      ( PEA‹ˆ‡OBAHO OPA™EHˆE K 10 / TˆA LM... );
C
C
C   PˆME—AHˆE:   1. B›‡OB LM MOHˆTOPA:
C   ----------
C               CALL LM....(MSWT, RMSWT, ADR, MSWALL, RMSWAL, KOD )
C
C              ƒ„E
C                      MSWT, RMSWT - "OTPE‡Kˆ" MSW_CˆCKA „‹Ÿ
C                      ‹EMEHTA, MAKPOMO„E‹œ KOTOPOƒO TPE“ET
C                      OC‹“†ˆBAHˆE CO CTOPOH› DM... MOHˆTOPA;
C                      ADR - MSW A„PEC C‹OBA COCTOŸHˆŸ ‹EMEHTA,
C                            „‹Ÿ HACTPO‰Kˆ MAKPOMO„E‹ˆ KOTOPOƒO
C                            B›‡›BAETCŸ LM... MOHˆTOP;
C                      MSWALL, RMSWAL - BECœ CˆCOK MSW_C‹OB;
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       COMMON/PROF2/ MACTAB(800)
       DIMENSION MSWALL(*)
       INTEGER LM, TIP, PTIP, ADR
C-->  MAX A„PEC W MSWALL:
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
C-->  TEK. A„P. MSW ..
       IM = ADR+1
C-->  C‹E„. A„P. MSW
       ADR = I1
C-->  Tˆ MAKPOMO„E‹ˆ..
       TIP = MSWALL(IM+1)
C
C-->  OˆCK O MACTAB...
       IX = 1
100    CONTINUE
       IF ( MACTAB(IX) .EQ. 0 ) GO TO 200
       IF ( MACTAB(IX) .EQ. TIP ) GO TO 300
       IX = IX + MASPRT(17)
       GO TO 100
200    CONTINUE
       NWTRE = MASPRT(14)
       WRITE(NWTRE, 1000) TIP
1000   FORMAT(/' *** ‹Oƒ. CO‰ B CALL_LM --> TˆA # ', I5,
     = ' HET B MACTAB ***'/)
       CALL SUBERR(TIP)
C######################################################################
300    CONTINUE
C
C  IX  CTPOKA B MACTAB HA˜EƒO ‹EMEHTA, ECTœ ‹ˆ LM - MOHˆTOP ?
C
       KKK = MACTAB(IX+5)
       LM = KKK / 100
C----------------------------------------------------------------------
       IF ( LM .EQ. 0 ) GO TO 900
C-->  MOHˆTOP B›‡›BAEM..
C      IM - A„EC B MSW_ALL OˆCAHˆˆ
C
       GO TO (1,2,3,4,5,6,7,8,9,10,11,12), LM
C----------------------------------------------------------------------
       NWTRE = MASPRT(14)
       WRITE(NWTRE,1001) LM
1001   FORMAT(/' *** ‹Oƒ. CO‰ B CALL_LM --> CTPAHH›‰ # "LM" ',
     = 'MOHˆTOPA = ', I5, ' ***'/)
       GO TO 900
C----------------------------------------------------------------------
C
666    CONTINUE
       RETURN
C
C### B›‡›BAEM LM MOHˆTOP› #############################################
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
C----------------------------------------------------------------------
       END
