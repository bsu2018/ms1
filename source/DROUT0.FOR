         SUBROUTINE DRout0(KAR, DAT, MSWALL, TOKN, VALLN )
CXXX----KOM‹EKC "MS1" -->
C   O„POƒPAMMA:   DRout0(KAR, DAT, MSWALL, TOKN, VALLN )
C   ------------
C   HA‡HA—EHˆE:    *** ˆCO‹HˆTE‹œ ˆ OPAOT—ˆK „ˆPEKTˆB› #‚…˜ ***
C   -----------      #‚…˜  T0 TM KOUT
C
C                  T0 - HA—A‹œHOE BPEMŸ
C                  TM - KOHE—HOE  BPEMŸ
C                  KOUT - —EPE‡ CKO‹œKO TO—EK B›BO„ˆTœ;
C
C
C   METO„:         O—EBˆ„EH;
C   -----
C   APAMETP›:     KAR - CˆMBO‹œHAŸ CTPOKA TEKCTA „ˆPEKTˆB›(A-KOD,80);
C   ---------      DAT - MACCˆB PˆHŸT›X „AHH›X „ˆPEKTˆB›;
C                  MSWALL - C‹OBA COCTOŸHˆŸ –Eˆ;
C                  TOKN, VALLN - MACCˆB› XPAHEHˆŸ "TO—EK";
C
C   O„POƒPAMM›:  1. COMMON PROF1;
C   -------------  2. GETSPS, DRPR00, READ00 , PPNCOM
C
C   PˆME—AHˆE:    1. "“MO‹—AHˆE" --> TO‹œKO „‹Ÿ KOUT      ---->
C   ----------         1
C
CZZZ-------------------------------------------------------------------
         COMMON /PROF1/ MASPRT(200)
C-->  ˆ‡  1-‰ ‡AˆCˆ NSPOOL
         EQUIVALENCE ( MASPRT(48), HOUTT )
C********EQUIVALENCE ( SETKA,  MASPRT(63) )
C
         EQUIVALENCE ( NWTR,   MASPRT(13) )
         EQUIVALENCE ( NWTRE,  MASPRT(14) )
C********EQUIVALENCE ( NRDR,   MASPRT(12) )
         INTEGER SPIS
         DIMENSION SPIS(91), MSWALL(*), TOKN(*), VALLN(*)
         DIMENSION KAR(*), DAT(*)
C-->  ECTœ ‹ˆ MSWALL ?
       IF ( MASPRT(74) .EQ. 1 ) GO TO 59
       WRITE(NWTR, 60)
60     FORMAT(/' *** HET OˆCAHˆŸ –Eˆ - „ˆPEKTˆBA ˆƒHOPˆP“ETCŸ ***'/)
       RETURN
59     CONTINUE
         T0 = 0.0
         TM = 666.0
         KOUT = 1
       KKK = IFIX( DAT(1) )
       IF ( KKK .LT. 2 ) GO TO 202
       T0 = DAT(2)
       TM = DAT(3)
       KOUT = IFIX( DAT(4) )
         IF ( KOUT .LE. 0 ) KOUT = 1
202     CONTINUE
         WRITE(NWTR, 203) T0, TM, KOUT
203      FORMAT(/' *** #‚…˜ ¯à¨­ïâ :=> T0=', G12.5,
     = ' TM=',G12.5, ' KOUT=',I5, ' ***'/)
C----------------------------------------------------------------------
         CALL GETSPO( MSWALL, SPIS, IRC )
         GO TO ( 1,2,3), IRC
         CALL SUBERR(IRC)
3        CONTINUE
         WRITE(NWTR, 204)
204      FORMAT(/' *** KOHE– ”A‰‹A HA "RDR" - PAOTA PEP›BAETCŸ ***'/)
         call STOPP(16)
C----------------------------------------------------------------------
2        CONTINUE
C-->  SPIS “CT
         RETURN
C**********************************************************************
1        CONTINUE
C--> O„ƒOTOBKA K B›BO„“...
       TIME = 0.0
C
       KTXX = 0
C
C                                             ???????????
C--> OTKPOEMCŸ
C
       TIME = 0.0
       KTXX = 0
         KOD = 1
         CALL READ00( KOD, HOUTT, TOKN, VALLN)
         GO TO ( 1000, 2000, 3000 ), KOD
         CALL SUBERR(KOD)
1000     CONTINUE
C*** ‹OKˆP“EM ********************************************************
C---> KOHTPO‹ˆP“EM TM, B TIME --> MAX BPEMŸ...
C        IF ( TIME .LT. TM ) WRITE(NWTR,205) TIME, TM, TIME
C205      FORMAT(' *** „AHH›E ”A‰‹OB HAKO‹EHˆŸ OPE„E‹EH› TO‹œKO „O ',
C    = G12.5, ' CEK. ***'/
C    = ' *** ‡APO˜EHAŸ BEPXHŸŸ ƒPAHˆ–A BPEMEHHOƒO ˆHTEPBA‹A TM=',
C    = G12.5, ' CEK. ***'/
C    = ' *** EPEOPE„E‹ŸETCŸ HA ‡HA—EHˆE TM=', G12.5, ' CEK. ***')
C        IF ( TIME .LT. TM ) TM = TIME
C=*********************************************************************
C----------------------------------------------------------------------
       HOUTTX = HOUTT * 1.8E+4
       WRITE(NWTR, 5667 ) HOUTT, HOUTTX
5667   FORMAT(/' *** ‡HA—EHˆŸ EPEMEHH›X –Eˆ OPE„E‹EH› C ˜AƒOM',
     = 1X,G12.5, ' (', G12.5, ') ***'/)
C----------------------------------------------------------------------
C****CTPAXOBKA*********************************************************
       IF ( TM .LE. T0 ) WRITE(NWTR,5666) T0, TM
5666  FORMAT(//' *** ˆHTEPBA‹ ‡HA—EHˆ‰ [T0,TM]:=> ','[',G12.5,',',G12.5,
     =   '] ***'/ ' *** OPE„E‹EH O˜ˆO—HO --> "#‚…˜ - ',
     =        ' ˆƒHOPˆP“ETCŸ ***'//)
       IF ( TM .LE. T0 ) RETURN
C**********************************************************************
C-->  OTKP›Tœ E—AATœ
       CALL PPNCOM(3,NWTR)
C
         KOD = 1
         CALL DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
         KXX = 0
C-->  –ˆK‹ E—ATˆ
206      CONTINUE
         KOD = 2
         CALL READ00(KOD, TIME, TOKN, VALLN )
         GO TO (207, 2000, 3000 ), KOD
         CALL SUBERR(KOD)
207      CONTINUE
C-->  HA˜A ‹ˆ TO—KA
         IF ( TIME .LT. T0 ) GO TO 206
         IF ( TIME .GT. TM ) GO TO 208
C   TO—KA HA˜A
         KOD = 2
         IF ( KXX .EQ. 0 )
     =     CALL DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
         KTXX = KTXX + 1
         KXX = KXX + 1
         IF ( KXX .EQ. KOUT ) KXX = 0
         GO TO 206
208      CONTINUE
         KOD = 3
         CALL DRPR02(KOD, SPIS, TIME, TOKN, VALLN )
C===>          „ˆPEKTˆBA B›O‹HEHA
         RETURN
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
2000     CONTINUE
C--->   ‹Oƒ. KOHE–, BCEƒO B ”A‰‹E KTXX TO—EK
         WRITE(NWTR, 54) KTXX
54       FORMAT(/' *** „OCTˆƒH“T KOHE– ”A‰‹OB HAKO‹EHˆŸ    ***'/
     = ' *** B ‡APO˜EHHOM „ˆAO‡OHE OT T0 „O TM ***'/
     =          ' *** OHAP“†EHO BCEƒO ', I5, ' TO—EK PE˜EHˆŸ ***'/)
C
       GO TO 208
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
3000     CONTINUE
C-->  ”ˆ‡. KOHE–
         WRITE(NWTR,55) KTXX,         TIME
55       FORMAT(/' *** ”A‰‹› HAKO‹EHˆŸ PE˜EHˆŸ HE ›‹ˆ ‡AKP›T› ***'/
     = ' *** B ‡APO˜EHHOM „ˆAO‡OHE OT T0 „O TM ***'/
     = ' *** OHAP“†EHO BCEƒO ', I5, ' TO—EK PE˜EHˆŸ ***'/
     = ' *** OC‹E„HŸŸ TO—KA OPE„E‹EHA „‹Ÿ T =',G12.5, ' CEK. ***'/
     = ' *** PAOTA PEP›BAETCŸ ***********************************'//)
         call STOPP(16)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         END
