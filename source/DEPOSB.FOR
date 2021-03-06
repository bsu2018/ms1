       SUBROUTINE DEPOSB(NALL, KK, GK, JK, YALL, JALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: DEPOSB(NALL, KK, GK, JK, YALL, JALL )
C   ------------
C   HA�HA�EH�E:   OC��ECTB�EH�E "AKTA" �O��EMEHTHO�O BK�A�A G-J
C   ----------    �APAMETPOB �� "�O�H�X" MATP�� �PO��BO��HO�O MHO�O�O�.
C                 B  MATP���  YALL � JALL O��E� C�CTEM� �PABHEH��;
C
C                      GK - "�O�HA�" MATP��A ���OB�X �POBO��MOCTE�,
C                      JK - ---> �CTO�H�KOA TOKA;
C                 NALL - MACC�B C��CKA HOMEPOB ���OB �O�K���EH��,
C                        ( KK - �T�K );
C
C                 [I] = [GK] * [V]  + [JK]  <---- "MO�E��" MHO�O�O�.
C
C              ��E [I] - BEKTOP-CTO��E� (PA�MEPHOCT�� KK) - TOK�
C                        "BTEKA���E"  BO BCE BETB�  B MHO�O�O��CH�KA;
C                  [V] - BEKTOP CTO��E� ( KK ) - ���OB�X HA�P��EH��
C                        HA ���AX �O�K���EH��  MHO�O�O��CH�KA;
C                      - HOMEPA ���OB �O�K���EH�� CO�EP�ATC� B MACC�BE
C
C                        NALL  -  HOMEPA ���OB �O�K���EH��;
C
C         CXEMA "BK�A�A":           #NALL#
C         *************         �               T
C                               I               I
C                               I .[ + GK ]     I #NALL#
C       [ YALL ] = [ YALL ] +   I               I
C                               I               I
C                               L               J
C                                             .
C                                �           T
C                                I           I
C       [ JALL ] = [ JALL ]  -   I [ + JK ]  I
C                            #   I           I
C                            $   L           J
C  !!! BH�MAH�E $$$$$$$$$$$$$$   !!!!!#M�H�C#!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C   METO�:     CM. B��E;
C   -----
C   �APAMETP�: CM. B��E;
C   ---------
C   �O��PO�PAMM�:  PROF1;
C   ------------
C
C   �P�ME�AH�E:  1. B V.M= 2.* - HE ��AKOBAHHOE XPAHEH�E;
C   ----------   2. �P� NALL(I) = 0 ->
C                   "BK�A�" HE �E�AETC�;
C                3. YALL(I,J)   --> �EPE� IDY(I,J)=NDALL*(J-1)+I
C                   GK    (I,J) --> �EPE� IDG(I,J)=KK  *(J-1)+I
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
C--->  BK�A�� ��� �CTO�H�KOB...
       DO 1 I=1, KK
C--> PEA��H�E HOMEPA B C�CTEME *_ALL
       K = NALL(I)
C          ********
       IF ( K .NE. 0 ) JALL(K) = JALL(K) - JK(I)
1      CONTINUE
C######################################################################
C--->  BK�A� B YALL
       DO 2 I = 1, KK
         K = NALL(I)
C            ********
         IF ( K .EQ. 0 ) GO TO 2
           DO 3 J = 1, KK
             M = NALL(J)
C                ********
C            IF ( M .EQ. 0 ) GO TO 3
C--->        BK�A��BAEM...
             IJ = IDG(I,J)
             KM = IDY(K,M)
             YALL( KM ) = YALL( KM ) + GK( IJ )
3          CONTINUE
2      CONTINUE
       RETURN
       END
