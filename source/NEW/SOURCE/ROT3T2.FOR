       SUBROUTINE ROT3T2(X1, X2, X3, BX1, BX2, BX3, BQ,
     *                   XQ, XD, XMOD, BXMOD, GQXMOD )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: ROT3T2(X1, X2, X3, BX1, BX2, BX3, BQ,
C   ------------         XQ, XD, XMOD, BXMOD, GQXMOD     )
C
C   HA�HA�EH�E:   �EPO�PA�OBAH�E "TPO�K�" BE����H
C   ----------    X1, X2, X3, �A�AHH�X ���AM�
C                 BX1, BX2, BX3 OTHOC�TE��HO OC� OTC�ETA "A"
C                 B "�BO�K�" BE����H "XQ"  �  "XD", PAC�O�O�EHH�X
C                 �O OC�M HOBO� C�CTEM� OTC�ETA "Q"-"D" -->
C                 OC� "D" O�EPE�AET OC� "Q" ( �POT�B �AC. CTPE�K� ), A
C                 OC� "Q" COCTAB��ET ��O� "BQ" C O�OPHO� OC�� "A";
C                   ( XMOD - MO����, A "BXMOD" ��O� OTHOC. OC� "A" );
C                 �P� �TOM TAK �E O�PE�E���TC�:
C                - XMOD - MO���� BEKTOPA, C�MAPHO�O K X1, X2, X3;
C                - BXMOD - ��O� ME��� O�OPHO� OC�� "A" � BEKTOPOM "XMOD"
C                - GQXMOD - ��O� ME��� OC�� "Q" � BEKTOPOM "XMOD";
C   METO�:    "�P�MOE" BEKTPOHOE C�MM�POBAH�E B �POEK���X � PA��O�EH�E;
C   -----
C   �APAMETP�:  CM. HA�HA�EH�E
C   ---------
C   �O��PO�PAMM�: HET
C   ------------
C   �P�ME�AH�E:  1. BCE ���� B PA��AHAX
C   ----------   2. ��O� BCE��A �E��T B �PE�E�AX OT 0 �O 360 �PA�;
C
CZZZ-------------------------------------------------------------------
        DATA PI2 / 6.283185306 /
       D1 =      BX1  -  BQ
       D2 =      BX2  -  BQ
       D3 =      BX3  -  BQ
C--> "Q"  �POEK���-->
       XQ = X1 * COS(D1) + X2 * COS(D2) + X3 * COS(D3)
C--> "D"  �POEK��� -->
       XD = X1 * SIN(D1) + X2 * SIN(D2) + X3 * SIN(D3)
C-->  MO����-->
       XMOD = SQRT( XD**2 + XQ**2 )
C-->  ��O� B C�CTEME "Q" - "D"
       GQXMOD =  ATAN2( XD, XQ )
       IF ( XD .LT. 0.0 ) GQXMOD = GQXMOD + PI2
       IF ( GQXMOD .GE. PI2 ) GQXMOD = GQXMOD - INT(GQXMOD/PI2)*PI2
C      -------------------------------------------------------------
C-->  ��O� OTHOC. OC� OT�ETA "A"
       BXMOD = GQXMOD + BQ
       IF (  BXMOD .GE. PI2 )  BXMOD =  BXMOD - INT( BXMOD/PI2)*PI2
C      -------------------------------------------------------------
       RETURN
       END
