       SUBROUTINE FUR000(KTF, FTAB,F0,HOUTT,TN,ZADN,ZADN1,ZMID,ZTEP,
     *                   BF0, CF0, BCKOF )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA: FUR000(KTF, FTAB,F0,HOUTT,TN,ZADN,ZADN1,ZMID,ZTEP,
C   ------------         BF0, CF0, BCKOF )
C   HA‡HA—EHˆE:   B›—ˆC‹EHˆE ˆHTEƒPA‹œH›X C“MM CEKTP. XAP-K CˆƒHA‹OB;
C   ----------     ( / ”“PœE AHA‹ˆ‡A )
C
C   METO„:        ˆHTEƒPˆPOBAHˆE METO„OM TPAE–ˆ‰, „‹Ÿ [TN, TN+H0];
C   -----          Pˆ EPBOM BXO„E BCE ˆHTEƒPA‹œH›E "C“MM›" -->
C                  „O‹†H› ›Tœ "OH“‹EH›";
C
C   APAMETP›:    KTF - KO‹-BO TO—EK „ˆCKPET —ACTOT›;
C   ---------     FTAB - MACCˆB ‡HA—EHˆ‰ TˆX —ACTOT(KTF ˜T“K);
C                 F0 - "OCHOBHAŸ" —ACTOTA;
C                 HOUTT - ˜Aƒ OPE„E‹EHˆŸ ‡HA—EHˆ‰ ”“HK–ˆ‰ O BPEMEHˆ;
C                 TN - BPEMŸ OPE„E‹EHˆŸ N-‰ TO—Kˆ,
C                 ZADN - ‡HA—EHˆE ”“HK–ˆˆ B TN;
C                 ZADN1 - ‡HA—EHˆE ”“HK–ˆˆ B TN+HOUTT;
C                 ***** „‹Ÿ HAKO‹EHˆŸ ˆHTEƒPA‹œH›X C“MM *****
C                       (!!! HA ˆHTEPBA‹ OPE„E‹EHˆŸ HE „E‹EHO !!!)
C                 ZMID - CPE„HEE ‡HA—EHˆE;
C                 ZTEP - „E‰CTB“ž™EE ‡HA—EHˆE -> !!! KBA„PAT !!!;
C                 BF0 - B - KO”-T› (COS) HA F0;
C                 CF0 - C - KO”-T› (SIN) HA F0;
C                 BCKOF - MACCˆB KO-OB HA —ACTOTAX ˆ‡ FTAB:
C                         B1,B2,......C1,C2....( KTF+KTF ˜T“K );
C
C   O„POƒPAMM›:  HET
C   ------------
C   PˆME—AHˆE:    1.B FTAB - HE KP“ƒOBAŸ —ACTOTA;
C   ----------     2. B›‡›BATœ CTO‹œKO PA‡, CKO‹œKO TO—EK B
C                     AHA‹ˆ‡ˆP“EMOM BPEMEHHOM ˆHTEPBA‹E;
C
CZZZ-------------------------------------------------------------------
       DIMENSION FTAB(1), BCKOF(1)
       REAL INT, IOLD
       DATA PI2/6.283185306/
C-->  "ˆHTEƒPATOP"
       INT( IOLD, XN, XN1 ) = IOLD + ( (XN1+XN)/2.0 ) * HOUTT
C-->  CPE„. & „E‰CTB.
       ZMID = INT( ZMID, ZADN, ZADN1)
       ZTEP = INT( ZTEP, ZADN**2, ZADN1**2)
C---> O BCEM —ACTOTAM...
       TN1 = TN + HOUTT
       DO 1 I=1,KTF
       ARGN  = PI2 * FTAB(I) * TN
       ARGN1 = PI2 * FTAB(I) * TN1
C-->  B, C - KO”.
       BCKOF(I) = INT(BCKOF(I),ZADN*COS(ARGN),ZADN1*COS(ARGN1))
       BCKOF(KTF  +I) = INT(BCKOF(KTF  +I),ZADN*SIN(ARGN),
     *                                     ZADN1*SIN(ARGN1))
1      CONTINUE
C-->  „‹Ÿ "OCHOBHO‰"
       ARGN  = PI2*F0*TN
       ARGN1 = PI2*F0*TN1
       BF0 = INT( BF0, ZADN*COS(ARGN), ZADN1*COS(ARGN1) )
       CF0 = INT( CF0, ZADN*SIN(ARGN), ZADN1*SIN(ARGN1) )
C----------------------------------------------------------------------
       RETURN
       END
