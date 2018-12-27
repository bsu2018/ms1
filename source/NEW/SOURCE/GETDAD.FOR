         SUBROUTINE GETDAD(KAR, DAT, IRC )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   GETDAD(KAR, DAT, IRC )
C   -------------
C
C   HA‡HA—EHˆE:    EPEBO„ „AHH›X CˆMBO‹œHO‰ CTPOKˆ „ˆPEKTˆB› KAR BO
C   ----------     BH“TPEHHEE PE„CTAB‹EHˆE (REAL*4) --> MACCˆB DAT;
C                  ** ABTOMATˆ—ECKˆ‰ B›XO„ HA HA—A‹O PA‡OPA ‡AˆCˆ
C                     O —ˆC‹AX „ˆPEKTˆB: HA—A‹O PA‡OPA - OT EPBOƒO
C                     POE‹A OC‹E TEKCTA „ˆPEKTˆB› ***
C
C   METO„:         CKAHˆPOBAHˆE KAR ˆ PˆEM —ˆCE‹.
C   -----
C
C   APAMETP›:     KAR - MACCˆB CTPOKˆ „ˆPEKTˆB›;
C   ---------      DAT - MACCˆB ˆHŸT›X —ˆCE‹ (REAL*4):
C                   PA‡MEPHOCTœ = 21 C‹OBO, B DAT(1) - KO‹ˆ—ECTBO,
C                   ‡ATEM  -  PˆHŸT›E —ˆC‹A;
C                  IRC = 1  -  „AHH›E PˆHŸT›,
C                      = 2  -  ›‹ˆ O˜ˆKˆ Pˆ PˆEME —ˆCE‹;
C
C   O„POƒPAMM›:  1. COMMON PROF1
C   -------------  2. MSGOUT, APFTRT, APFSQE, SUBEROR
C
C   PˆME—AHˆE:    1. CTPOKA POE‹OB - „O“CTˆMA;
C   ----------     2. MACCOB DAT - —ˆCTˆTCŸ H“‹ŸMˆ;
C                  3. Pˆ O˜ˆKAX - E—ATœ COO™EHˆ‰;
C                  4. PA‡„E‹ˆTE‹œ "," ŸB‹ŸETCŸ „O“CTˆM›M;
C                  5. Pˆ O˜ˆKE - B DAT ‡AˆCœ 0.0;
C                  6. ‡AˆCˆ: <NNN,,NNN> <NNN,0,NNN> KBˆBA‹EHTH›;
C                  7. KOHE– MO†HO OTME—ATœ CˆMBO‹OM ";" ;
CZZZ-------------------------------------------------------------------
         COMMON/PROF1/ MASPRT(200)
         EQUIVALENCE ( NWTR, MASPRT(13) )
         EQUIVALENCE ( NWTRE, MASPRT(14) )
         EQUIVALENCE ( DIAL,  MASPRT(15) )
         EQUIVALENCE ( NRDR,  MASPRT(12) )
         DIMENSION DAT(1), KAR(1), ER(80), FKAR(80), MSG(20)
         INTEGER DIAL, PRT90, ER, BLANK, COMA, FKAR, DIEZ, TERM
         DATA K72/72/, K21/21/
         DATA BLANK/' '/, COMA/','/, LGOPA/'$'/, DIEZ/'#'/, TERM/';'/
C--> B›XO„ HA HA—A‹O PA‡OPA
         DO 67 I = 1, K72
         IK = I
         IF ( KAR(I) .EQ. BLANK ) GO TO 68
67       CONTINUE
C--> POE‹OB B CTPOKE HET.....
         IK = K72 + 1
68       CONTINUE
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK, (KAR(IZ),IZ=1,K72)
C660   FORMAT(' *** IK =', I5, ' ***'/1X, 80A1 )
C!!--------------------------------------------------------------------
66       CONTINUE
C--->  O—ˆCTKA...
         DO 1 I = 1, 80
1        ER(I) = BLANK
         MSG(1) = 0
         IER = 0
         PRT90 = 0
         DO 2 I=1,K21
2        DAT(I) = 0.0
C--->  APA‹E‹œH›‰ MACCˆB...
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
         CALL APF TRT( 1, K72, KAR, FKAR )
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
C--->  OˆCK...
         IRC = 1
3        CONTINUE
         IF ( IK .GT. K72 ) GO TO 666
         IF ( KAR(IK) .NE. BLANK ) GO TO 10
30       CONTINUE
C--> HE KOH—ˆTœ ‹ˆ HAM..
         IF ( KAR(IK) .EQ. TERM ) GO TO 666
         IK = IK + 1
         GO TO 3
C---
10       CONTINUE
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
C--> —TO-TO  HA˜‹ˆ:
         N2 = K72 - IK + 1
         CALL APF SQE( FKAR, E0, IK, N2,  IFE )
         GO TO ( 11, 12, 13, 4, 4, 3 ), IFE
4        CALL SUBERR(IFE)
11       CONTINUE
C--> OC‹E„HEE —ˆC‹O
         ID = DAT(1) + 1
         IF ( (ID+1) .GT. K21 ) CALL SUBERR( ID   )
         DAT(ID+1) = E0
         DAT(1) = ID
         GO TO 666
C######################################################################
12       CONTINUE
C!!!-------------------------------------------------------------------
C      WRITE(6,6660) IK , (KAR(IZ), IZ=1,K72)
C!!!-------------------------------------------------------------------
C-->  HE –ˆ”PA
         IF ( KAR(IK) .EQ. BLANK .OR. KAR(IK) .EQ. COMA ) GO TO 120
         IF ( KAR(IK) .EQ. TERM ) GO TO 120
C--> KOHE– ‹OXO‰
         IRC = 2
         DAT(1) = DAT(1) + 1.0
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 3
         MSG(1) = I+1
121      CONTINUE
C--- –ˆK‹ ˆƒHOPˆPOBAHˆŸ::::
         IK = IK + 1
         IF ( KAR(IK) .EQ. BLANK .OR. KAR(IK) .EQ. COMA ) GO TO 30
         IF ( KAR(IK) .EQ. TERM ) GO TO 666
         IF ( IK .GT. K72 ) GO TO 666
         GO TO 121
120      CONTINUE
C--> PˆHŸTœ —ˆC‹O...
         ID = DAT(1) + 1
         IF ( (ID+1) .GT. K21 ) CALL SUBERR( ID )
         DAT(ID+1) = E0
         DAT(1) = ID
C-->  HA HOBOE
         GO TO 30
C######################################################################
13       CONTINUE
C--> EPEO‹HEHˆE
         IRC = 2
         DAT(1) = DAT(1) + 1.0
         ER(IK) = LGOPA
         IER = IER + 1
         I = MSG(1)
         MSG(I+2) = 2
         MSG(1) = I+1
         GO TO 121
C######################################################################
C  BCE
C######################################################################
666      CONTINUE
C!!!-------------------------------------------------------------------
C        WRITE(NWTRE, 50 ) ( I, DAT(I), I=1,K21)
C0       FORMAT( 5(2X,'DAT(', I2,')=', G12.5 ) )
C!!!-------------------------------------------------------------------
         IF ( IER .EQ. 0 ) RETURN
C--->E—ATœ O˜ˆOK
         IF ( DIAL .EQ. 1 ) WRITE(NWTR, 1003) ( ER(I), I=1,80)
1003     FORMAT(80A1/' *** „AHH›E ˆƒHOPˆP“žTCŸ - PE„O‹OƒAETCŸ',
     = ' OTC“TCTBˆE —ˆC‹A ***')
         IF ( DIAL .EQ. 0 ) WRITE(NWTR,1004) IER,(ER(I), I=1,80 )
1004     FORMAT(1X,'*** O˜ˆOK ',I2,' ***=>',80A1/' *** „AHH›E ˆƒHOPˆP“ž
     =TCŸ - PE„O‹OƒAETCŸ OTC“TCTBˆE —ˆC‹A ***')
         CALL MSGOUT(MSG)
         IF ( DIAL .EQ. 0 ) RETURN
         WRITE(NWTR, 51)
51       FORMAT(' *** BBO„ˆTE TO‹œKO „AHH›E „ˆPEKTˆB› ˆ‹ˆ # ***')
         READ(NRDR,52, END=53  )  ( KAR(I), I=1,80)
52       FORMAT(80A1)
         IRC = 1
         IK = 1
         IF ( KAR(1) .EQ. DIEZ ) RETURN
C--->  OPO“EM C HA—A‹A....
         GO TO 66
C----------------------------------------------------------------------
53       CONTINUE
         WRITE( NWTR, 54 ) NRDR
54       FORMAT(/' *** KOHE– ”A‰‹A HA KAHA‹E NRDR = ', I2,
     = ' ***'/ ' *** PAOTA PEP›BAETCŸ [ SEQDAT ] ***'/)
         CALL SUBERR(0)
         RETURN
         END
