       SUBROUTINE DEPOSA(KLONG, MLONG, LONG, DLONG, G0LONG, J0LONG,
     *                   YALL, JALL )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: DEPOSA(KLONG, MLONG, LONG, DLONG, G0LONG, J0LONG,
C   ------------ *       YALL, JALL )
C   HA�HA�EH�E:   OC��ECTB�EH�E "AKTA" �O��EMEHTHO�O BK�A�A G-J
C   ----------    �APAMETPOB "�ETHO�O" MHO�O�O��CH�KA B MATP���
C                 YALL � JALL O��E� C�CTEM� �PABHEH��;
C
C                 ��EC� "�ETH��" MHO�O�O��CH�K - MHO�O�O��CH�K
C                 �KB�BA�EHT�POBAHH�� B B��E:
C
C                 [I] = [GOLONG] * [U]  + [J0LONG]
C
C              ��E [I] - BEKTOP-CTO��E� (PA�MEPHOCT�� LONG) - TOK�
C                        "BTEKA���E" B MHO�O�O��CH�K;
C                  [U] - BEKTOP CTO��E� ( LONG ) - �A�EH�� HA�P��EH��
C                        HA "TOKOB�X" BETB�X MHO�O�O��CH�KA;
C                      - HOMEPA ���OB �O�K���EH�� CO�EP�ATC� B MACC�BE
C                        KLONG - HOMEPA "HA�A�" BETBE�,
C                        MLONG - HOMEPA "KOH�OB" BETBE�;
C
C         CXEMA "BK�A�A":         #KLONG#     .   #MLONG#
C         *************         �             .             T #KLONG#
C                               I  [+G0LONG]  .  [-G0LONG]  I
C                               I ..........................I
C       [ YALL ] = [ YALL ] +   I             .             I
C                               I  [-G0LONG]  .  [+G0LONG]  I #MLONG#
C                               L             .             J
C                                             .
C                                �           T
C                                I [-J0LONG] I  #KLONG#
C       [ JALL ] = [ JALL ]  +   I...........I
C                                I [+J0LONG] I  #MLONG#
C                                L           J
C
C
C   METO�:     CM. B��E;
C   -----
C   �APAMETP�:  1.  DLONG - "�A�BO�HA�" PA�MEPHOCT� MACC�AB G0LONG,
C   ---------      OCTA��H�E CM. B��E;
C   �O��PO�PAMM�:  PROF1;
C   ------------
C
C   �P�ME�AH�E:  1. B V.M= 2.* - HE ��AKOBAHHOE XPAHEH�E;
C   ----------   2. �P� KLONG(I) ��� MLONG(I) = 0 ->
C                   "BK�A�" HE �E�AETC�;
C                3. YALL(I,J)   --> �EPE� IDY(I,J)=NDALL*(J-1)+I
C                   G0LONG(I,J) --> �EPE� IDG(I,J)=DLONG*(J-1)+I
C                                                  -----
C
CZZZ-------------------------------------------------------------------
       COMMON/PROF1/ MASPRT(200)
       EQUIVALENCE( NDALL, MASPRT(23) )
       DIMENSION KLONG(1), MLONG(1), G0LONG(1), J0LONG(1)
       DIMENSION YALL(1), JALL(1)
       REAL J0LONG, JALL
       INTEGER DLONG
       IDY(I,J) = NDALL * (J-1) + I
       IDG(I,J) = DLONG  * (J-1) + I
C--->  BK�A�� ��� �CTO�H�KOB...
       DO 1 I=1, LONG
C--> PEA��H�E HOMEPA B C�CTEME *_ALL
       K = KLONG(I)
C          ********
       M = MLONG(I)
C          ********
       IF ( K .NE. 0 ) JALL(K) = JALL(K) - J0LONG(I)
       IF ( M .NE. 0 ) JALL(M) = JALL(M) + J0LONG(I)
1      CONTINUE
C######################################################################
C--->  BK�A� B YALL �ACT� [ KLONG, KLONG ]  <---- [+G0LONG]
       DO 2 I = 1, LONG
         K = KLONG(I)
C            ********
         IF ( K .EQ. 0 ) GO TO 2
           DO 3 J = 1, LONG
             M = KLONG(J)
C                ********
C            IF ( M .EQ. 0 ) GO TO 3
C--->        BK�A��BAEM...
             IJ = IDG(I,J)
             KM = IDY(K,M)
             YALL( KM ) = YALL( KM ) + G0LONG( IJ )
3          CONTINUE
2      CONTINUE
C######################################################################
C######################################################################
C--->  BK�A� B YALL �ACT� [ KLONG, MLONG ]  <---- [-G0LONG]
C
       DO 4 I = 1, LONG
         K = KLONG(I)
C            ********
         IF ( K .EQ. 0 ) GO TO 4
           DO 5 J = 1, LONG
             M = MLONG(J)
C                ********
C            IF ( M .EQ. 0 ) GO TO 5
C--->        BK�A��BAEM...
             IJ = IDG(I,J)
             KM = IDY(K,M)
C*********** MK = IDY(M,K)
             YALL( KM ) = YALL( KM ) - G0LONG( IJ )
C************YALL( MK ) = YALL( MK ) - G0LONG( IJ )
5          CONTINUE
4      CONTINUE
C######################################################################
C######################################################################
C--->  BK�A� B YALL �ACT� [ MLONG, KLONG ]  <---- [-G0LONG]
C
       DO 8 I = 1, LONG
         K = MLONG(I)
C            ********
         IF ( K .EQ. 0 ) GO TO 8
           DO 9 J = 1, LONG
             M = KLONG(J)
C                ********
C            IF ( M .EQ. 0 ) GO TO 9
C--->        BK�A��BAEM...
             IJ = IDG(I,J)
             KM = IDY(K,M)
C
             YALL( KM ) = YALL( KM ) - G0LONG( IJ )
C
9          CONTINUE
8      CONTINUE
C######################################################################
C######################################################################
C--->  BK�A� B YALL �ACT� [ MLONG, MLONG ]  <---- [+G0LONG]
       DO 6 I = 1, LONG
         K = MLONG(I)
C            ********
         IF ( K .EQ. 0 ) GO TO 6
           DO 7 J = 1, LONG
             M = MLONG(J)
C                ********
C            IF ( M .EQ. 0 ) GO TO 7
C--->        BK�A��BAEM...
             IJ = IDG(I,J)
             KM = IDY(K,M)
             YALL( KM ) = YALL( KM ) + G0LONG( IJ )
7          CONTINUE
6      CONTINUE
C######################################################################
       RETURN
       END
