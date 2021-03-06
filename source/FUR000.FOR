       SUBROUTINE FUR000(KTF, FTAB,F0,HOUTT,TN,ZADN,ZADN1,ZMID,ZTEP,
     *                   BF0, CF0, BCKOF )
CXXX----KOM��EKC "MS1" --> BEPC�� 1.0  (OT �EKA�P� 1986 �.)------------
C   �O��PO�PAMMA: FUR000(KTF, FTAB,F0,HOUTT,TN,ZADN,ZADN1,ZMID,ZTEP,
C   ------------         BF0, CF0, BCKOF )
C   HA�HA�EH�E:   B���C�EH�E �HTE�PA��H�X C�MM C�EKTP. XAP-K C��HA�OB;
C   ----------     ( �/� ��P�E AHA���A )
C
C   METO�:        �HTE�P�POBAH�E METO�OM TPA�E���, ��� [TN, TN+H0];
C   -----          �P� �EPBOM BXO�E BCE �HTE�PA��H�E "C�MM�" -->
C                  �O��H� ��T� "O�H��EH�";
C
C   �APAMETP�:    KTF - KO�-BO TO�EK ��CKPET �ACTOT�;
C   ---------     FTAB - MACC�B �HA�EH�� �T�X �ACTOT(KTF �T�K);
C                 F0 - "OCHOBHA�" �ACTOTA;
C                 HOUTT - �A� O�PE�E�EH�� �HA�EH�� ��HK��� �O BPEMEH�;
C                 TN - BPEM� O�PE�E�EH�� N-� TO�K�,
C                 ZADN - �HA�EH�E ��HK��� B TN;
C                 ZADN1 - �HA�EH�E ��HK��� B TN+HOUTT;
C                 ***** ��� HAKO��EH�� �HTE�PA��H�X C�MM *****
C                       (!!! HA �HTEPBA� O�PE�E�EH�� HE �E�EHO !!!)
C                 ZMID - CPE�HEE �HA�EH�E;
C                 ZTEP - �E�CTB���EE �HA�EH�E -> !!! KBA�PAT !!!;
C                 BF0 - B - KO��-T� (COS) HA F0;
C                 CF0 - C - KO��-T� (SIN) HA F0;
C                 BCKOF - MACC�B KO�-OB HA �ACTOTAX �� FTAB:
C                         B1,B2,......C1,C2....( KTF+KTF �T�K );
C
C   �O��PO�PAMM�:  HET
C   ------------
C   �P�ME�AH�E:    1.B FTAB - HE KP��OBA� �ACTOTA;
C   ----------     2. B���BAT� CTO��KO PA�, CKO��KO TO�EK B
C                     AHA����P�EMOM BPEMEHHOM �HTEPBA�E;
C
CZZZ-------------------------------------------------------------------
       DIMENSION FTAB(1), BCKOF(1)
       REAL INT, IOLD
       DATA PI2/6.283185306/
C-->  "�HTE�PATOP"
       INT( IOLD, XN, XN1 ) = IOLD + ( (XN1+XN)/2.0 ) * HOUTT
C-->  CPE�. & �E�CTB.
       ZMID = INT( ZMID, ZADN, ZADN1)
       ZTEP = INT( ZTEP, ZADN**2, ZADN1**2)
C---> �O BCEM �ACTOTAM...
       TN1 = TN + HOUTT
       DO 1 I=1,KTF
       ARGN  = PI2 * FTAB(I) * TN
       ARGN1 = PI2 * FTAB(I) * TN1
C-->  B, C - KO��.
       BCKOF(I) = INT(BCKOF(I),ZADN*COS(ARGN),ZADN1*COS(ARGN1))
       BCKOF(KTF  +I) = INT(BCKOF(KTF  +I),ZADN*SIN(ARGN),
     *                                     ZADN1*SIN(ARGN1))
1      CONTINUE
C-->  ��� "OCHOBHO�"
       ARGN  = PI2*F0*TN
       ARGN1 = PI2*F0*TN1
       BF0 = INT( BF0, ZADN*COS(ARGN), ZADN1*COS(ARGN1) )
       CF0 = INT( CF0, ZADN*SIN(ARGN), ZADN1*SIN(ARGN1) )
C----------------------------------------------------------------------
       RETURN
       END
