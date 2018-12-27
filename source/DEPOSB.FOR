       SUBROUTINE DEPOSB(NALL, KK, GK, JK, YALL, JALL )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: DEPOSB(NALL, KK, GK, JK, YALL, JALL )
C   ------------
C   HA‡HA—EHˆE:   OC“™ECTB‹EHˆE "AKTA" O‹EMEHTHOƒO BK‹A„A G-J
C   ----------    APAMETPOB ˆ‡ "O‹H›X" MATPˆ– POˆ‡BO‹œHOƒO MHOƒOO‹.
C                 B  MATPˆ–›  YALL ˆ JALL O™E‰ CˆCTEM› “PABHEHˆ‰;
C
C                      GK - "O‹HAŸ" MATPˆ–A “‡‹OB›X POBO„ˆMOCTE‰,
C                      JK - ---> ˆCTO—HˆKOA TOKA;
C                 NALL - MACCˆB CˆCKA HOMEPOB “‡‹OB O„K‹ž—EHˆŸ,
C                        ( KK - ˜T“K );
C
C                 [I] = [GK] * [V]  + [JK]  <---- "MO„E‹œ" MHOƒOO‹.
C
C              ƒ„E [I] - BEKTOP-CTO‹E– (PA‡MEPHOCTœž KK) - TOKˆ
C                        "BTEKAž™ˆE"  BO BCE BETBˆ  B MHOƒOO‹žCHˆKA;
C                  [V] - BEKTOP CTO‹E– ( KK ) - “‡‹OB›X HAPŸ†EHˆ‰
C                        HA “‡‹AX O„K‹œ—EHˆŸ  MHOƒOO‹žCHˆKA;
C                      - HOMEPA “‡‹OB O„K‹ž—EHˆŸ CO„EP†ATCŸ B MACCˆBE
C
C                        NALL  -  HOMEPA “‡‹OB O„K‹ž—EHˆŸ;
C
C         CXEMA "BK‹A„A":           #NALL#
C         *************         ƒ               T
C                               I               I
C                               I .[ + GK ]     I #NALL#
C       [ YALL ] = [ YALL ] +   I               I
C                               I               I
C                               L               J
C                                             .
C                                ƒ           T
C                                I           I
C       [ JALL ] = [ JALL ]  -   I [ + JK ]  I
C                            #   I           I
C                            $   L           J
C  !!! BHˆMAHˆE $$$$$$$$$$$$$$   !!!!!#MˆH“C#!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C   METO„:     CM. B›˜E;
C   -----
C   APAMETP›: CM. B›˜E;
C   ---------
C   O„POƒPAMM›:  PROF1;
C   ------------
C
C   PˆME—AHˆE:  1. B V.M= 2.* - HE “AKOBAHHOE XPAHEHˆE;
C   ----------   2. Pˆ NALL(I) = 0 ->
C                   "BK‹A„" HE „E‹AETCŸ;
C                3. YALL(I,J)   --> —EPE‡ IDY(I,J)=NDALL*(J-1)+I
C                   GK    (I,J) --> —EPE‡ IDG(I,J)=KK  *(J-1)+I
C
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(100)
       EQUIVALENCE( NDALL, MASPRT(23) )
       DIMENSION NALL(1), GK(1), JK(1)
       DIMENSION YALL(1), JALL(1)
       REAL JK,     JALL
C--
       IDY(I,J) = NDALL * (J-1) + I
       IDG(I,J) = KK    * (J-1) + I
C--->  BK‹A„› „‹Ÿ ˆCTO—HˆKOB...
       DO 1 I=1, KK
C--> PEA‹œH›E HOMEPA B CˆCTEME *_ALL
       K = NALL(I)
C          ********
       IF ( K .NE. 0 ) JALL(K) = JALL(K) - JK(I)
1      CONTINUE
C######################################################################
C--->  BK‹A„ B YALL
       DO 2 I = 1, KK
         K = NALL(I)
C            ********
         IF ( K .EQ. 0 ) GO TO 2
           DO 3 J = 1, KK
             M = NALL(J)
C                ********
C            IF ( M .EQ. 0 ) GO TO 3
C--->        BK‹A„›BAEM...
             IJ = IDG(I,J)
             KM = IDY(K,M)
             YALL( KM ) = YALL( KM ) + GK( IJ )
3          CONTINUE
2      CONTINUE
       RETURN
       END
