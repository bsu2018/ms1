       SUBROUTINE CALL PS(NSUB, NEL, URMAS, STMAS, DGMAS, INTMAS )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: CALL PS(NSUB, NEL, URMAS, STMAS, DGMAS, INTMAS )
C   ------------
C   HA�HA�EH�E:    �O HOMEP� NSUB, ��� ��EMEHTA �E�� NEL B���BAET
C   ----------     �/� �O���OBATE�� PSUB#:
C
C                    CALL PSUB#( NEL, URMAS, STMAS, DGMAS, INTMAS )
C
C        ��E       URMAS - MACC�B �HA�EH�� �APAMETPOB, �EPE�ABAEM��
C                          �O���OBATE�EM �/� PSUB# �EPE� C��COK:
C                            PSUB # < ... URMAS ....>;
C                          1-E C�OBO - KO�-BO �APAMETPOB, �ATEM �X
C                          �HA�EH��;
C                  STMAS - MACC�B �AHH�X, �EPE�ABAEM�� PSUB#,
C                  DGMAS - MACC�B "PAC��TAHH�X" �HA�EH��, HA��EHH�X B
C                          PSUB#;
C                  INTMAS - MACC�B PA�O��X C�OB, ��� OP�AH��A���,
C                           HA�P�MEP, �HTEP�P�POBAH�� �O TO�KAM;
C
C                        CTP�KT�P� � PA�MEP� �T�X MACC�BOB
C                        O�PE�E���TC� B KOHKPETH�X C���A��X;
C
C   METO�:    O�EB��EH.
C   ------
C   �APAMETP�:  CM. HA�HA�EH�E;
C   ---------
C   �O��PO�PAMM�:  PSUB#, ��E # OT 1 �O 20 ( KOHKPET. PEA���A���);
C   ------------
C   �P�ME�AH�E:  1.  KOHTPO�� HA����� PSUB, � KO�-BA
C   ----------       �EPE�ABAEM�X B HEE �APAMETPOB HE PEA���OBAH;
C
CZZZ-------------------------------------------------------------------
       DIMENSION URMAS(1), STMAS(1), DGMAS(1), INTMAS(1)
       REAL INTMAS
       COMMON /PROF1/ MASPRT(200)
       EQUIVALENCE ( NWTRE, MASPRT(14) )
C----------------------------------------------------------------------
       GO TO ( 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),NSUB
C!!!-------------------------------------------------------------------
       WRITE(NWTRE,100) NEL, NSUB
100    FORMAT(/' *** MAKPOMO�E�� ��-TA # ', I5, ' B��BAT� PSUB #', I3,
     = ' HE MO�ET ***'/)
C!!!-------------------------------------------------------------------
       CALL SUBERR(NSUB)
C######################################################################
1      CALL PSUB1( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
2      CALL PSUB2( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
3      CALL PSUB3( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
4      CALL PSUB4( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
5      CALL PSUB5( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
6      CALL PSUB6( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
7      CALL PSUB7( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
8      CALL PSUB8( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
9      CALL PSUB9( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
10     CALL PSUB10( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
11     CALL PSUB11( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
12     CALL PSUB12( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
13     CALL PSUB13( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
14     CALL PSUB14( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
15     CALL PSUB15( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
16     CALL PSUB16( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
17     CALL PSUB17( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
18     CALL PSUB18( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
19     CALL PSUB19( NEL, URMAS, STMAS, DGMAS, INTMAS )
       RETURN
20     CALL PSUB20( NEL, URMAS, STMAS, DGMAS, INTMAS )
C######################################################################
       RETURN
       END
