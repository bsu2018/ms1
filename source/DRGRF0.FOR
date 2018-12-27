         SUBROUTINE DRGRF0(KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
CXXX----KOM‹EKC "MS1" --> BEPCˆŸ 1.0  (OT „EKAPŸ 1986 ƒ.)------------
C   O„POƒPAMMA:   DRGRF0(KAR, DAT, MSWALL, TOKN, VALLN, NDX, XGRAF )
C   ------------
C   HA‡HA—EHˆE:    *** ˆCO‹HˆTE‹œ ˆ OPAOT—ˆK „ˆPEKTˆB› #ƒPA” ***
C   -----------      #ƒPA” T0 TM KOUT M S
C
C                  T0 - HA—A‹œHOE BPEMŸ E—ATˆ ƒPA”ˆKOB;
C                  TM - KOHE—HOE  BPEMŸ E—ATˆ ƒPA”ˆKOB;
C                  KOUT - —EPE‡ CKO‹œKO TO—EK B›BO„ˆTœ;
C                  M - MAC˜TA ==> = 0 BCE B PA‡H›X;
C                                  = 1 BCE B O„HOM;
C                                  = 2 KBˆBA‹EHTHO ‡APOC“ Pˆ M = 0
C                                                         ˆ Pˆ M = 1;
C
C                  S - CETKA ƒPA”ˆKA O OCˆ BPEMEHˆ => B ‹. ƒPA„.
C
C
C   METO„:         O—EBˆ„EH;
C   -----
C   APAMETP›:     KAR - CˆMBO‹œHAŸ CTPOKA TEKCTA „ˆPEKTˆB›(A-KOD,80);
C   ---------      DAT - MACCˆB PˆHŸT›X „AHH›X „ˆPEKTˆB›;
C                  MSWALL - C‹OBA COCTOŸHˆŸ –Eˆ;
C                  TOKN, VALLN - MACCˆB› XPAHEHˆŸ "TO—EK";
C                  NDX - PA‡MEPHOCTœ XGRAF;
C                  XGRAF - "C“‹ˆHƒ" „‹Ÿ B›BO„A ƒPA”ˆKOB;
C
C   O„POƒPAMM›:  1. COMMON PROF1;
C   -------------  2. GETSPS, DRGR00, READ00 , PPNCOM
C
C   PˆME—AHˆE:    1. "“MO‹—AHˆE" --> TO‹œKO „‹Ÿ KOUT M S  ---->
C   ----------         1   0   90.0
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
C-->  B›ˆPAETCŸ ˆ‡ NSPOOL 1-Ÿ ‡AˆCœ
         EQUIVALENCE ( MASPRT(48), HOUTT )
         EQUIVALENCE ( SETKA,  MASPRT(63) )
         EQUIVALENCE ( NTGRAF, MASPRT(47) )
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
         EQUIVALENCE ( NRDR,   MASPRT(12) )
         INTEGER SPIS, OPT
         DIMENSION SPIS(55), MSWALL(*), TOKN(*), VALLN(*) , XGRAF(*)
         DIMENSION KAR(*), DAT(*)
C-->  ECTœ ‹ˆ MSWALL ?
       IF ( MASPRT(74) .EQ. 1 ) GO TO 59
       WRITE(NWTR, 60)
60     FORMAT(/' *** HET OˆCAHˆŸ –Eˆ - „ˆPEKTˆBA ˆƒHOPˆP“ETCŸ ***'/)
       RETURN
59     CONTINUE
C===>
         SETKA = 90.0
         KOUT = 1
       OPT = 0
C===>
C=======>READ(NRDR, GRAF)
       KKK = IFIX( DAT(1) )
       IF ( KKK .GE. 2 ) GO TO 5600
       WRITE(NWTR, 57 )
57     FORMAT(/' *** HET "T0" ˆ/ˆ‹ˆ "TM" - „ˆPEKTˆBA ˆƒHOPˆP“ETCŸ ***'/)
       RETURN
5600   CONTINUE
       T0 = DAT(2)
       TM = DAT(3)
       IF ( KKK. EQ. 2 ) GO TO 58
       KOUT = IFIX( DAT(4) )
       IF ( KKK .EQ. 3 ) GO TO 58
       OPT = IFIX( DAT(5) )
       IF ( KKK .EQ. 4 ) GO TO 58
       SETKA = DAT(6)
58     CONTINUE
C=======>WRITE(NWTR, GRAF)
C====>
         IF ( OPT .LT. 0 .OR. OPT .GT. 2 ) OPT = 0
         IF ( SETKA .LT. 0.0 ) SETKA = 90.0
         IF ( KOUT .LE. 0 ) KOUT = 1
C===>
         WRITE(NWTR, 50) T0, TM, KOUT, OPT, SETKA
50     FORMAT(/' *** #ƒPA” PˆHŸTA:==> T0=', G12.5,' TM=',G12.5,' ***'/
     =          '                   ==> KOUT=',I3,' M=',I2, ' S=',
     =   G12.5, '    ***'/)
C----------------------------------------------------------------------
         CALL GETSPS( MSWALL, SPIS, IRC )
         GO TO ( 1,2,3), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR, 51)
51      FORMAT(//' *** KOHE– ”A‰‹A HA "RDR" - PAOTA PEP›BAETCŸ ***'//)
         call STOPP(16)
C----------------------------------------------------------------------
2        CONTINUE
C-->  SPIS “CT
         RETURN
C**********************************************************************
1        CONTINUE
C--> O„ƒOTOBKA K B›BO„“...
       TIME = 0.0
       RK = KOUT
       KTXX = 0
C      NTGRAF = INT( (TM-T0)/(HOUTT*RK) +0.5 )  + 0
       NTGRAF = 0
C-->  B SETKE “—ECTœ KOUT:
       SETKA = SETKA/RK
C
C--> OTKPOEMCŸ
         KOD = 1
         CALL READ00( KOD, HOUTT,  TOKN, VALLN)
         GO TO ( 100, 200, 300 ), KOD
         CALL SUBERR(KOD)
100      CONTINUE
C**** ‹OKˆP“EM *******************************************************
C---> KOHTPO‹ˆP“EM TM, B TIME --> MAX BPEMŸ...
C        IF ( TIME .LT. TM ) WRITE(NWTR,56) TIME, TM, TIME
C56       FORMAT(/' *** „AHH›E ”A‰‹OB HAKO‹EHˆŸ OPE„E‹EH› TO‹œKO „O ',
C    = G12.5, ' CEK. ***'/
C    = ' *** ‡APO˜EHAŸ BEPXHŸŸ ƒPAHˆ–A BPEMEHHOƒO ˆHTEPBA‹A TM=',
C    = G12.5, ' CEK. ***'/
C    = ' *** EPEOPE„E‹ŸETCŸ HA ‡HA—EHˆE TM=', G12.5, ' CEK. ***')
C        IF ( TIME .LT. TM ) TM = TIME
C**********************************************************************
C----------------------------------------------------------------------
       HOUTTX = HOUTT * 1.8E+4
       WRITE(NWTR, 5667 ) HOUTT, HOUTTX
5667   FORMAT(/' *** ‡HA—EHˆŸ EPEMEHH›X –Eˆ OPE„E‹EH› C ˜AƒOM',
     = 1X,G12.5, ' (', G12.5, ' ) ***'/)
C----------------------------------------------------------------------
C****CTPAXOBKA*********************************************************
       IF ( TM .LE. T0 ) WRITE(NWTR,5666) T0, TM
5666  FORMAT(//' *** ˆHTEPBA‹ ‡HA—EHˆ‰ [T0,TM]:=> ','[',G12.5,',',G12.5,
     =   '] ***'/ ' *** OPE„E‹EH O˜ˆO—HO --> "#ƒPA”" - ',
     =        ' ˆƒHOPˆP“ETCŸ ***'//)
       IF ( TM .LE. T0 ) RETURN
C**********************************************************************
         RK = KOUT
C--> CKO‹œKO POCŸT B›BECTˆ
         NTGRAF = INT( (TM-T0)/(HOUTT*RK) + 0.5 ) + 0
C                                                *******
         WRITE(NWTR ,52) NTGRAF
52       FORMAT(/' *** B›BO„ˆTCŸ ƒPA”ˆK ˆ‡ ', I5, ' TO—EK ***' )
C-->  OTKP›Tœ E—AATœ
       CALL PPNCOM(3,NWTR)
         KTXX = 0
         KOD = 1
         CALL DRGR00(KOD, OPT, SPIS, TIME, TOKN, VALLN, NDX, XGRAF )
         KXX = 0
C==> —ˆCTKA XGRAF
         DO 53 I = 1, NDX
53       XGRAF(I) = 0.0
C-->  –ˆK‹ HAKO‹EHˆŸ
102      CONTINUE
         KOD = 2
         CALL READ00(KOD, TIME, TOKN, VALLN )
         GO TO (101, 200, 300 ), KOD
         CALL SUBERR(KOD)
101      CONTINUE
C-->  HA˜A ‹ˆ TO—KA
         IF ( TIME .LT. T0 ) GO TO 102
         IF ( TIME .GT. TM ) GO TO 105
103      CONTINUE
         KOD = 2
         IF ( KXX .EQ. 0 )
     =     CALL DRGR00(KOD, OPT, SPIS, TIME, TOKN, VALLN, NDX, XGRAF)
         IF ( KXX .EQ. 0    ) KTXX = KTXX + 1
         KXX = KXX + 1
         IF ( KXX .EQ. KOUT ) KXX = 0
         GO TO 102
105      CONTINUE
C!!!===================================================================
         IF ( KTXX .NE. NTGRAF ) WRITE(NWTRE, 110) KTXX,NTGRAF
110      FORMAT(' *** PEA‹œHO PˆHŸ‹ˆ ', I5, ' TO—EK, A ‡APOCˆ‹ˆ ',
     = I5, ' ***')
C!!!===================================================================
C---> B›BECTˆ ƒPA”ˆK...
         KOD = 3
         CALL DRGR00(KOD,OPT, SPIS, TIME, TOKN, VALLN, NDX, XGRAF)
C===>          „ˆPEKTˆBA B›O‹HEHA
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
200      CONTINUE
C--->   ‹Oƒ. KOHE–, BCEƒO B ”A‰‹E KTXX TO—EK
         WRITE(NWTR, 54) KTXX, NTGRAF
54       FORMAT(/' *** „OCTˆƒH“T KOHE– ”A‰‹OB HAKO‹EHˆŸ    ***'/
     = ' *** B ‡APO˜EHHOM „ˆAO‡OHE OT T0 „O TM ***'/
     =          ' *** OHAP“†EHO BCEƒO ', I5, ' TO—EK PE˜EHˆŸ ***'/
     =' *** B› ‡APA˜ˆBAETE ', I5, ' TO—EK „‹Ÿ OCTPOEHˆŸ ƒPA”ˆKA ***'/)
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
300      CONTINUE
C-->  ”ˆ‡. KOHE–
         WRITE(NWTR,55) KTXX, NTGRAF, TIME
55       FORMAT(/' *** ”A‰‹› HAKO‹EHˆŸ PE˜EHˆŸ HE ›‹ˆ ‡AKP›T› ***'/
     = ' *** B ‡APO˜EHHOM „ˆAO‡OHE OT T0 „O TM ***'/
     = ' *** OHAP“†EHO BCEƒO ', I5, ' TO—EK PE˜EHˆŸ ***'/
     = ' *** B› ‡APA˜ˆBAETE ', I5, ' TO—EK „‹Ÿ OCTPOEHˆŸ ƒPA”ˆKA ***'/
     = ' *** OC‹E„HŸŸ TO—KA OPE„E‹EHA „‹Ÿ T =',G12.5, ' CEK. ***'/
     = ' *** PAOTA PEP›BAETCŸ ***********************************'///)
         call STOPP(16)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
